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
      Integer,
      Maybe(..), traceIfFalse, ($), (&&), head, Eq ((==)), (.), traceError, filter, not, isJust, (/=), (*), (-), Ord (..), negate, (+)
      )
import           Prelude                    (Show (show), undefined, IO)
import Plutus.V1.Ledger.Value               ( AssetClass(AssetClass), assetClassValueOf, valueOf )
import Utilities                            (wrapValidator, writeCodeToFile)
import Plutus.V1.Ledger.Address             (scriptHashAddress)

-- Define the Oracle's parameter type
data OracleParams = OracleParams {
    oNFT :: AssetClass ,
    developerPKH :: PubKeyHash ,
    reserveValidator :: ValidatorHash ,
    tokenMintingPolicy :: AssetClass
 }
makeLift ''OracleParams

-- Define the Oracle's datum type
data OracleDatum = OracleDatum {
    rate :: Integer ,
    canMint :: Bool ,
    mintedAmount :: Integer
} deriving Show
unstableMakeIsData ''OracleDatum

data OracleRedeemer = Update | Toggle | ChangeAmount
unstableMakeIsData ''OracleRedeemer

{-# INLINABLE mkOracleValidator #-}
mkOracleValidator :: OracleParams -> OracleDatum -> OracleRedeemer -> ScriptContext -> Bool
mkOracleValidator oParams _ oRedeemer ctx = case oRedeemer of
                                                Update       -> traceIfFalse "Update: Developer hasn't signed!" developerSigned &&
                                                                traceIfFalse "Update: NFT missing on input!" nftOnInput &&
                                                                traceIfFalse "Update: NFT missing on output!" nftOnOutput &&
                                                                traceIfFalse "Update: Invalid oracle output datum!" checkOracleOpDatum &&
                                                                traceIfFalse "Update: You should only change the 'rate' value!" checkValidDeveloperUpdate &&
                                                                traceIfFalse "Update: Incorrect amount in reserve based on rate change!" checkEnoughFundsInReserve

                                                Toggle       -> traceIfFalse "Toggle: Developer hasn't signed!" developerSigned &&
                                                                traceIfFalse "Toggle: NFT missing on input!" nftOnInput &&
                                                                traceIfFalse "Toggle: NFT missing on output!" nftOnOutput &&
                                                                traceIfFalse "Toggle: Invalid oracle output datum!" checkOracleOpDatum &&
                                                                traceIfFalse "Toggle: You should only change the 'canMint' value!" checkValidDeveloperToggle

                                                ChangeAmount -> traceIfFalse "ChangeAmount: NFT missing on input!" nftOnInput &&
                                                                traceIfFalse "ChangeAmount: NFT missing on output!" nftOnOutput &&
                                                                traceIfFalse "ChangeAmount: Invalid oracle output datum!" checkOracleOpDatum &&
                                                                traceIfFalse "ChangeAmount: You should only change the 'mintedAmount' value!" checkValidMintUpdate &&
                                                                traceIfFalse "ChangeAmount: Invalid 'mintedAmount' value based on amount minted/burnt!" correctAmountChanged
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        oracleInputDatum :: OracleDatum
        oracleInputDatum = case findOwnInput ctx of
                            Just ip -> parseOracleDatum $ txOutDatum $ txInInfoResolved ip
                            Nothing -> traceError "Expected 1 UTxO to be consumed from the Oracle!"

        oracleOutputDatum :: OracleDatum
        oracleOutputDatum = case getContinuingOutputs ctx of
                                [tOut] -> parseOracleDatum $ txOutDatum tOut
                                _      -> traceError "Expected exactly 1 output UTxO to at the OracleValidator!"

        -- ======== Old and New states ========

        oldRate = rate oracleInputDatum :: Integer
        newRate = rate oracleOutputDatum :: Integer

        oldCanMint = canMint oracleInputDatum :: Bool
        newCanMint = canMint oracleOutputDatum :: Bool

        oldMintedAmount = mintedAmount oracleInputDatum :: Integer
        newMintedAmount = mintedAmount oracleOutputDatum :: Integer

        -- =========== Common Conditions ============

        developerSigned :: Bool
        developerSigned = txSignedBy info $ developerPKH oParams

        nftOnInput :: Bool
        nftOnInput =  1 == assetClassValueOf (valueSpent info) (oNFT oParams)

        nftOnOutput :: Bool
        nftOnOutput = 1 == assetClassValueOf (valueProduced info) (oNFT oParams)

        checkOracleOpDatum :: Bool  -- Check the type of the Oracle output datum
        checkOracleOpDatum =  True -- isJust oracleOutputDatum


        -- ================ Update Conditions ============

        -- Check that the developer only changes the rate and nothing else
        checkValidDeveloperUpdate :: Bool
        checkValidDeveloperUpdate = (oldRate /= newRate)  &&  (oldCanMint == newCanMint)  &&  (oldMintedAmount == newMintedAmount)

        -- Check that the developer claims the appropriate rewards (this is only relevant when there will extra funds in the reserve due to rate change)
        developerClaimedRewards :: Integer -> Bool
        developerClaimedRewards rewards = rewards == lovelaceValueOf (valuePaidTo info $ developerPKH oParams)

        -- Check that the developer gives the appropirate funds to the reserve (this is only relevant when there will be less funds in the reserve due to rate change)
        developerFundedReserve :: Integer -> Bool
        developerFundedReserve funds = funds == lovelaceValueOf (valueLockedBy info $ reserveValidator oParams)

        -- Check that the developer either funds the reserve or receives rewards based on changes in rate
        checkEnoughFundsInReserve :: Bool
        checkEnoughFundsInReserve = if valueDifference > 0
                                        then developerFundedReserve valueDifference
                                        else developerClaimedRewards $ negate valueDifference
            where
                    -- The no. of ADA available in the reserve must always match the no. of tokens in circulation as per the USD/ADA rate
                valueDifference = (oldRate * oldMintedAmount) - (newRate * newMintedAmount)     -- The valueDifference is the old ADA/Token reserved minus the new ADA/Token reserved


        -- ================ Toggle Conditions ============

        -- Check that the developer only changes the canMint value and nothing else (The new canMint value must be different from the old canMint value)
        checkValidDeveloperToggle :: Bool
        checkValidDeveloperToggle = (oldRate == newRate)  &&  (oldCanMint /= newCanMint)  &&  (oldMintedAmount == newMintedAmount)


        -- ================ ChangeAmount Conditions ============

        -- Check that the Stablecoin owner only changes the mintedAmount value and nothing else
        checkValidMintUpdate :: Bool
        checkValidMintUpdate = (oldRate == newRate)  &&  (oldCanMint == newCanMint)  &&  (oldMintedAmount /= newMintedAmount)

        -- Check that the Stablecoin owner updates the mintedAmount to the correct value
        correctAmountChanged :: Bool
        correctAmountChanged = (oldMintedAmount + amountMinted) == newMintedAmount

        -- The amount of tokens minted/burned in the same txn (amountMinted will be negative if the tokens are actually being burned)
        amountMinted :: Integer
        amountMinted = assetClassValueOf (txInfoMint info) $ tokenMintingPolicy oParams


-- ======================================================== Boilerplate: Wrap, compile and serialize =============================================================
                -- CurrencySymbol,  TokenName,  DeveloperPKH,  ReserveValidator, TokenMintingPolicy, OracleDatum, (),  ScriptContext
{-# INLINABLE wrappedOracleCode #-}
wrappedOracleCode :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedOracleCode curSym tn devPKH reserveVal tokenMintPol = wrapValidator $ mkOracleValidator oParams
    where
        oParams = OracleParams {
            oNFT = AssetClass (unsafeFromBuiltinData curSym, unsafeFromBuiltinData tn) ,
            developerPKH = unsafeFromBuiltinData devPKH ,
            reserveValidator = unsafeFromBuiltinData reserveVal ,
            tokenMintingPolicy = unsafeFromBuiltinData tokenMintPol
        }

compiledOracleCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledOracleCode = $$( compile [|| wrappedOracleCode ||] )

saveOracleCode :: IO()
saveOracleCode = writeCodeToFile "./assets/oracle.plutus" compiledOracleCode

-- =============================================================== Helper functions for other scripts =====================================================

-- ======== Code to extract the oracle datum from the reference input UTxOs ==========
{-# INLINABLE getOracleDatum #-}
getOracleDatum :: TxInfo -> ValidatorHash -> Maybe OracleDatum
getOracleDatum _info _oracleValHash = case oracleDatum of
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