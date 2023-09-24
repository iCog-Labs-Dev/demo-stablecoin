{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores #-}

module TokenMintingPolicy where

import Plutus.V2.Ledger.Api
    ( ScriptContext(scriptContextTxInfo),
      PubKeyHash,
      Datum(Datum),
      TxInfo (txInfoOutputs, TxInfo, txInfoReferenceInputs, txInfoMint, txInfoInputs),
      OutputDatum(OutputDatumHash, NoOutputDatum, OutputDatum),
      TxOut(txOutDatum, txOutValue, txOutAddress), BuiltinData, Validator, mkValidatorScript, UnsafeFromData (unsafeFromBuiltinData), ValidatorHash, TxInInfo (txInInfoResolved), adaSymbol, adaToken, TokenName )
import Plutus.V2.Ledger.Contexts
    ( findDatum, txSignedBy, getContinuingOutputs, scriptOutputsAt, ownCurrencySymbol, valueLockedBy, valuePaidTo, valueSpent )
import PlutusTx
    ( makeLift, compile, CompiledCode )
import PlutusTx.Prelude
    ( Bool (..),
      Integer,
      Maybe(..), traceIfFalse, ($), (&&), not, traceError, fst, snd, Ord ((>), (<)), (||), (*), divide, (-)
      )
import           Prelude                    (Show, undefined, IO)
import Plutus.V1.Ledger.Value
    ( AssetClass(AssetClass), assetClassValueOf, valueOf )
import Utilities (wrapPolicy, writeCodeToFile)
import OracleValidator (OracleDatum (canMint, rate, mintedAmount), getOracleDatum, parseOracleDatum)

data TokenMintParams = TokenMintParams {
    tokenName :: TokenName ,
    oracleValidator :: ValidatorHash ,
    reserveValidator :: ValidatorHash ,
    developerPKH :: PubKeyHash
} deriving Show
makeLift ''TokenMintParams

data TokenRedeemer = Mint | Burn
unstableMakeIsData ''TokenRedeemer

{-# INLINABLE mkTokenMintingpolicy #-}
mkTokenMintingpolicy :: TokenMintParams -> TokenRedeemer -> ScriptContext -> Bool
mkTokenMintingpolicy tParams tRedeemer ctx = case tRedeemer of
                                                Mint ->  traceIfFalse "Minting is not allowed!" oldCanMint && -- This is false when minting is'nt allowed, but we are minting
                                                         traceIfFalse "Insufficient amount paid to reserve while minting!" checkEnoughPaidToReserve &&
                                                         traceIfFalse "You should add the amount you minted to the Oracle!" checkOracleUpdated &&
                                                         traceIfFalse "You can't mint a negative value!" (mintedAmount > 0)
                                                         -- traceIfFalse "You must pay 0.01% to the Developer!" paidToDeveloper
                                                Burn ->  traceIfFalse "You can't take that much ADA for those amount of tokens!" checkReceivedAmountOnBurn  &&
                                                         traceIfFalse "You should subtract the amount you minted to the Oracle!" checkOracleUpdated &&
                                                         traceIfFalse "You can't burn a positive value!" (mintedAmount < 0)
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

        oldCanMint = canMint oracleInputDatum :: Bool

        oldMintedAmount = mintedAmount oracleInputDatum :: Integer
        newMintedAmount = mintedAmount oracleOutputDatum :: Integer

        -- oracleDatum :: Maybe OracleDatum
        -- oracleDatum = getOracleDatum info (oracleValidator tParams)

        -- ======= Some Helper functions ========
        mintedAmount :: Integer
        mintedAmount = assetClassValueOf (txInfoMint info) $ AssetClass (ownCurrencySymbol ctx, tokenName tParams)
        
        currentAdaRequiredForToken :: Integer  -- This is the number of ADA required for the amount of Tokens we Mint/Burn.
        currentAdaRequiredForToken = case oracleDatum of
                            Just d -> (mintedAmount * 1_000_000) `divide` rate d
                            Nothing -> traceError "TokenMintingPolicy: Invalid 'rate' on Oracle Datum!"
        
        -- ========= Check if sufficient funds are sent to the reserve when minting Tokens =========

        checkEnoughPaidToReserve :: Bool
        checkEnoughPaidToReserve = if minting 
                                    then amountPaidToReserve > currentAdaRequiredForToken                   -- We don't mind if you send more ADA ;)
                                    else True        -- This check is irrelevant while burning
            where
                amountPaidToReserve :: Integer
                amountPaidToReserve = case scriptOutputsAt (reserveValidator tParams) info of
                                        [(_ , v)] -> valueOf v adaSymbol adaToken
                                        _         -> traceError "Expected exactly one UTxO to be sent to the Reserve!"

        -- ========= Check if the right amount of funds are consumed from the reserve when burning Tokens =========

        checkReceivedAmountOnBurn :: Bool
        checkReceivedAmountOnBurn = if not minting
                                    then (paidToUser - changeToReserve) < currentAdaRequiredForToken    -- The ADA you get might be lower than expected due to txn fees 
                                    else True       -- This check is irrelevant while minting
            where
                paidToUser = valueOf (valuePaidTo info (developerPKH tParams)) adaSymbol adaToken
                changeToReserve = valueOf (valueLockedBy info (reserveValidator tParams)) adaSymbol adaToken
                -- paidToUserPKH = assetClassValueOf (valuePaidTo info (... userPKH ...)) $ AssetClass (adaSymbol, adaToken)

        -- totalInputAda = foldl foldOnInputs 0 (txInfoInputs info)
        --     where 
        --         foldOnInputs acc x = acc + valueOf (txOutValue $ txInInfoResolved x) adaSymbol adaToken

        -- ======== Check if the Oracle is updated with the appropriate mintedAmount when minting and burning ===========
        checkOracleUpdated :: Bool
        checkOracleUpdated = (oldMintedAmount + mintedAmount) == newMintedAmount

        -- ======== Check if 0.01% is paid to the developer while minting ==========
        paidToDeveloper :: Bool
        paidToDeveloper = undefined


-- ======================================================== Boilerplate: Wrap, compile and serialize =============================================================
{-# INLINABLE wrappedTokenMintingCode #-}
wrappedTokenMintingCode :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedTokenMintingCode tknName developer oracle reserve = wrapPolicy $ mkTokenMintingpolicy params
    where
        params = TokenMintParams {
            tokenName = unsafeFromBuiltinData tknName,
            developerPKH = unsafeFromBuiltinData developer,
            oracleValidator = unsafeFromBuiltinData oracle,
            reserveValidator = unsafeFromBuiltinData reserve
        }

compiledTokenMintingCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledTokenMintingCode = $$( compile [|| wrappedTokenMintingCode ||] )

saveTokenMintingCode :: IO()
saveTokenMintingCode = writeCodeToFile "./assets/token_minting.plutus" compiledTokenMintingCode