{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module OracleValidator where

import Plutus.V2.Ledger.Api
    ( ScriptContext(scriptContextTxInfo),
      PubKeyHash,
      Datum(Datum),
      TxInfo (txInfoOutputs, TxInfo, txInfoReferenceInputs, txInfoMint),
      OutputDatum(OutputDatumHash, NoOutputDatum, OutputDatum),
      TxOut(txOutDatum, txOutValue, txOutAddress), BuiltinData, Validator, mkValidatorScript, UnsafeFromData (unsafeFromBuiltinData), TxInInfo (txInInfoResolved, TxInInfo), CurrencySymbol, ValidatorHash, adaSymbol, adaToken, Value )
import Plutus.V2.Ledger.Contexts
    ( findDatum, txSignedBy, getContinuingOutputs, findOwnInput, valueSpent, valueProduced, valuePaidTo, scriptOutputsAt, valueLockedBy )
import PlutusTx
    ( unstableMakeIsData,
      FromData(fromBuiltinData),
      makeLift, compile, applyCode, liftCode, CompiledCode)
import PlutusTx.Prelude
    ( Bool (False, True),
      Integer ,
      Maybe(..), traceIfFalse, ($), (&&), head, Eq ((==)), (.), traceError, filter, not, isJust, (/=), (*), (-), Ord (..), negate, (+)
      )
import           Prelude                    (Show (show), undefined, IO)
import Plutus.V1.Ledger.Value               ( AssetClass(AssetClass), assetClassValueOf, valueOf )
import Utilities                            (wrapValidator, writeCodeToFile)
import Plutus.V1.Ledger.Address             (scriptHashAddress)

-- Define the Oracle's parameter type
data OracleParams = OracleParams {
    oNFT :: AssetClass ,
    developerPKH :: PubKeyHash
 }
makeLift ''OracleParams

-- Define the Oracle's datum type
data OracleDatum = OracleDatum {
    rate :: Integer ,
    mintorburn :: (Bool, Bool)
} deriving Show
unstableMakeIsData ''OracleDatum

-- data OracleRedeemer = Update
-- unstableMakeIsData ''OracleRedeemer

{-# INLINABLE mkOracleValidator #-}
mkOracleValidator :: OracleParams -> OracleDatum -> () -> ScriptContext -> Bool
mkOracleValidator oParams _ _ ctx =     traceIfFalse "Update: Developer hasn't signed!" developerSigned &&
                                        traceIfFalse "Update: NFT missing on input!" nftOnInput &&
                                        traceIfFalse "Update: NFT missing on output!" nftOnOutput &&
                                        traceIfFalse "Update: Invalid oracle output datum!" checkOracleOpDatum
                                                                
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        developerSigned :: Bool
        developerSigned = txSignedBy info $ developerPKH oParams

        nftOnInput :: Bool
        nftOnInput =  1 == assetClassValueOf (valueSpent info) (oNFT oParams)

        nftOnOutput :: Bool
        nftOnOutput = 1 == assetClassValueOf (valueProduced info) (oNFT oParams)

        checkOracleOpDatum :: Bool  -- Check the type of the Oracle output datum
        checkOracleOpDatum =  isJust $ case opDatum of
                                        OutputDatum (Datum d) -> fromBuiltinData d :: Maybe OracleDatum
                                        _                     -> traceError "Expected inline datum at the Oracle address!"
            where 
                opDatum :: OutputDatum
                opDatum = case getContinuingOutputs ctx of
                                    [tOut] -> txOutDatum tOut
                                    _      -> traceError "Expected only one output at the Oracle address!"


-- ======================================================== Boilerplate: Wrap, compile and serialize =============================================================
                -- CurrencySymbol,  TokenName,  DeveloperPKH,  ReserveValidator, TokenMintingPolicy, OracleDatum, (),  ScriptContext
{-# INLINABLE wrappedOracleCode #-}
wrappedOracleCode :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedOracleCode curSym tn devPKH = wrapValidator $ mkOracleValidator oParams
    where
        oParams = OracleParams {
            oNFT = AssetClass (unsafeFromBuiltinData curSym, unsafeFromBuiltinData tn) ,
            developerPKH = unsafeFromBuiltinData devPKH 
        }

compiledOracleCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledOracleCode = $$( compile [|| wrappedOracleCode ||] )

saveOracleCode :: IO()
saveOracleCode = writeCodeToFile "./assets/oracle.plutus" compiledOracleCode



-- =============================================================== Helper functions for other scripts =====================================================

-- ======== Code to extract the oracle datum from the reference input UTxOs ==========
{-# INLINABLE getOracleDatumFromRef #-}
getOracleDatumFromRef :: TxInfo -> ValidatorHash -> Maybe OracleDatum
getOracleDatumFromRef _info _oracleValHash = case oracleDatum of
                    OutputDatum (Datum d) -> fromBuiltinData d
                    _                     -> traceError "Invalid Oracle Datum!"
    where
        lookupOracleAddress :: TxInInfo -> Bool
        lookupOracleAddress tinfo  = addr == txOutAddress (txInInfoResolved tinfo)   -- Check for the oracle UTxO by its address from reference TxInInfo
            where
                addr = scriptHashAddress _oracleValHash

        oracleDatum :: OutputDatum
        oracleDatum = txOutDatum $ txInInfoResolved oracleTxInInfo
            where
                oracleTxInInfos = filter lookupOracleAddress $ txInfoReferenceInputs _info     -- Filter the oracle UTxO by its address (there might be another reference input UTxO
                oracleTxInInfo = case oracleTxInInfos of                                            -- because we also execute the minting policy in the same txn
                                    [o] -> o
                                    _   -> traceError "Expected exactly one Oracle UTxO!"

-- ======= Code to parse the Oracle Datum into its appropriate type ===========
{-# INLINABLE parseOracleDatum #-}
parseOracleDatum :: OutputDatum -> OracleDatum
parseOracleDatum datum = case oracleDatum of
                            Just d -> d
                            Nothing -> traceError "Unable to parse Oracle Datum!"
    where
        oracleDatum = case datum of
                        OutputDatum (Datum d) -> fromBuiltinData d
                        _                     -> traceError "Expected inline datum!"

-- ====== Code to extract the ADA value from a Value type ========
{-# INLINABLE lovelaceValueOf #-}
lovelaceValueOf :: Value -> Integer
lovelaceValueOf v = valueOf v adaSymbol adaToken