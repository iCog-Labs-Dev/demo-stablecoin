{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE NumericUnderscores #-}

module ReserveValidator where

import Plutus.V2.Ledger.Api
    ( ScriptContext(scriptContextTxInfo),
      PubKeyHash,
      Datum(Datum),
      TxInfo (txInfoOutputs, TxInfo, txInfoMint, txInfoReferenceInputs, txInfoInputs, txInfoFee),
      OutputDatum(OutputDatumHash, NoOutputDatum, OutputDatum),
      TxOut(txOutDatum, txOutValue, txOutAddress), BuiltinData, Validator, mkValidatorScript, UnsafeFromData (unsafeFromBuiltinData), ValidatorHash, adaToken, TxInInfo (txInInfoResolved, TxInInfo), Address )
import Plutus.V2.Ledger.Contexts
    ( findDatum, txSignedBy, getContinuingOutputs, valueProduced, scriptOutputsAt )
import PlutusTx
    ( unstableMakeIsData,
      FromData(fromBuiltinData),
      makeLift, compile, applyCode, liftCode, CompiledCode )
import PlutusTx.Prelude
    ( Bool (..),
      Integer,
      Maybe(..), traceIfFalse, ($), (&&), head, Eq ((==)), (.), not, negate, traceError, (*), filter, divide, foldl, (+), (-), (||)
      )
import           Prelude                    (Show (show), undefined, IO, Ord ((>)), lookup)
import Plutus.V1.Ledger.Value
    ( AssetClass(AssetClass), assetClassValueOf, adaSymbol, valueOf )
import Data.Aeson (Value(Bool))
import Utilities (wrapValidator, writeCodeToFile)
import OracleValidator (OracleDatum (rate), getOracleDatum, lovelaceValueOf)
import Plutus.V1.Ledger.Address (scriptHashAddress)


data ReserveParams = ReserveParams {
    tokenMintingPolicy :: AssetClass ,
    oracleValidator :: ValidatorHash
}
makeLift ''ReserveParams

{-# INLINABLE  mkReserveValidator #-}
mkReserveValidator :: ReserveParams -> () -> () -> ScriptContext -> Bool
mkReserveValidator rParams _ _ ctx =    traceIfFalse "The net value of ADA consumed doesn't match the required amount!" checkRightAmountConsumed
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        oracleDatum :: Maybe OracleDatum
        oracleDatum = getOracleDatum info (oracleValidator rParams)

        netAdaConsumed :: Integer       -- Net ADA = (the total ada values of UTxOs coumed from the reserve) - (the change we give back to the reserve) - (the txn fee paid by the user)
        netAdaConsumed = totalOutputAda - totalInputAda - valueOf (txInfoFee info) adaSymbol adaToken
            where
                totalInputAda :: Integer            -- This should be the total amount of Ada UTxOs we consume from the ReserveValidator while burning
                totalInputAda = foldl (\acc x -> acc + lovelaceValueOf (txOutValue $ txInInfoResolved x)) 0 allInputs
                    where allInputs = txInfoInputs info             -- This is the list of all the input UTxOs of the txn (the ones we consume from the Reserve)
            
                totalOutputAda :: Integer           -- This should be the change we are giving back to the ReserveValidator while burning
                totalOutputAda = foldl (\acc x -> acc + lovelaceValueOf (txOutValue x)) 0 allOutputs
                    where allOutputs = getContinuingOutputs ctx     -- This is the list of all the output UTxOs we pay to the Reserve (the change we give back) 
        
        -- ========= Check if the developer has signed ===========
        developerSigned :: Bool
        developerSigned = txSignedBy info $ developerPKH rParams
            
        -- ========= Check if there are sufficient tokens burnt for the amount of ADA unlocked ===========
        requiredAdaForTokens :: Integer    -- Bool
        requiredAdaForTokens = case oracleDatum of
                                    Just d -> (totalTokensBurnt * 1_000_000) `divide` rate d    -- < totalAdaProduced
                                    Nothing  -> traceError "ReserveValidator: Invalid 'rate' on Oracle Datum!"
            where 
                -- totalAdaProduced :: Integer
                -- totalAdaProduced = assetClassValueOf (valueProduced info) (AssetClass (adaSymbol, adaToken))

                totalTokensBurnt :: Integer
                totalTokensBurnt = negate $ assetClassValueOf (txInfoMint info) (tokenMintingPolicy rParams)
    
        -- ========= Check if the right amount of funds are consumed from the reserve when burning Tokens =========
        checkRightAmountConsumed :: Bool
        checkRightAmountConsumed = netAdaConsumed == requiredAdaForTokens

-- ======================================================== Boilerplate: Wrap, compile and serialize =============================================================
{-# INLINABLE wrappedReserveCode #-}
wrappedReserveCode :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedReserveCode tkn_mint_pol oracle_val = wrapValidator $ mkReserveValidator params
    where
        params = ReserveParams {
            tokenMintingPolicy = unsafeFromBuiltinData tkn_mint_pol ,
            oracleValidator = unsafeFromBuiltinData oracle_val
        }

compiledReserveCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledReserveCode = $$( compile [|| wrappedReserveCode ||] )

saveReserveCode :: IO()
saveReserveCode = writeCodeToFile "./assets/reserve.plutus" compiledReserveCode