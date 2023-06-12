{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module OracleValidator where

import Data.String (IsString (fromString), String)
import Plutus.V1.Ledger.Value
  ( AssetClass (AssetClass),
    assetClassValueOf,
  )
import Plutus.V2.Ledger.Api
  ( BuiltinData,
    Datum (Datum),
    OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash),
    PubKeyHash,
    ScriptContext (scriptContextTxInfo),
    TxInInfo (txInInfoResolved),
    TxInfo,
    TxOut (txOutDatum, txOutValue),
    UnsafeFromData (unsafeFromBuiltinData),
    Validator,
    mkValidatorScript,
  )
import Plutus.V2.Ledger.Contexts
  ( findDatum,
    findOwnInput,
    getContinuingOutputs,
    txSignedBy,
  )
import PlutusTx
  ( CompiledCode,
    FromData (fromBuiltinData),
    applyCode,
    compile,
    liftCode,
    makeLift,
    unstableMakeIsData,
  )
import PlutusTx.Prelude (Bool, Integer, Maybe (..), traceError, traceIfFalse, ($), (&&), (=<<), (==))
import Text.Printf (printf)
import Utilities (wrapValidator, writeCodeToFile, writeValidatorToFile)
import Prelude (IO, Show (show), span)
import qualified Prelude ((/=))

-- import qualified Prelude

-----------------------------------------------------------------------------
----------------------- ON-CHAIN: HELPER FUNCTIONS/TYPES --------------------

{-# INLINEABLE parserOracleDatum #-}

parseOracleDatum :: TxOut -> TxInfo -> Maybe OracleDatum
parseOracleDatum output info = case txOutDatum output of
  NoOutputDatum -> Nothing
  OutputDatum (Datum dtm) -> PlutusTx.fromBuiltinData dtm
  OutputDatumHash dtmHash -> PlutusTx.fromBuiltinData =<< findDatum dtmHash info

-----------------------------------------------------------------------------
----------------------------- ON-CHAIN: VALIDATOR ---------------------------

data OracleParams = OracleParams
  { oNFT :: AssetClass, -- NFT of the oracle
    oOperator :: PubKeyHash -- operator of the oracle
  }
  deriving (Prelude.Show)

PlutusTx.makeLift ''OracleParams

data OracleDatum = OracleDatum
  { oRate :: Integer, -- USD per ADA
    oMintBurnTruthValue :: (Bool, Bool) -- (canMint, canBurn)
  }
  deriving (Prelude.Show)

{-# INLINEABLE mkOracleValidator #-}
mkOracleValidator :: OracleParams -> OracleDatum -> () -> ScriptContext -> Bool
mkOracleValidator param dtm _ ctx =
  traceIfFalse "operator signature missing" signedByOperator
    && traceIfFalse "NFT missing from input" inputHasNFT
    && traceIfFalse "NFT missing from output" outputHasNFT
    && traceIfFalse "invalid output datum" outputHasOracleDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByOperator :: Bool
    signedByOperator = txSignedBy info $ oOperator param

    inputHasNFT :: Bool
    inputHasNFT = assetClassValueOf ownInput (oNFT param) == 1

    ownInput :: Value
    ownInput = case findOwnInput info of
      Nothing -> traceError "oracle input missing"
      Just i -> txOutValue $ txInInfoResolved i

    outputHasNFT :: Bool
    outputHasNFT = assetClassValueOf (txOutValue ownOutput) (oNFT param) == 1

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "expected exactly one oracle output"

    outputHasOracleDatum :: Bool
    outputHasOracleDatum = parseOracleDatum ownOutput info == Just dtm

-----------------------------------------------------------------------------
------------------------------ COMPILE VALIDATOR ----------------------------

-- ======== Apply the parameters in lucid ========
{-# INLINEABLE wrappedOracleValidator #-}
--                            CS              TN           operator        datum          redeemer       context
wrappedOracleValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedOracleValidator cs tn pkh = wrapValidator $ mkOracleValidator op
  where
    op =
      OracleParams
        { oNFT = AssetClass (unsafeFromBuiltinData cs, unsafeFromBuiltinData tn),
          oOperator = unsafeFromBuiltinData pkh
        }

compileOracleCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
compileOracleCode = $$(compile [||wrappedOracleValidator||])

saveOracleCode :: IO ()
saveOracleCode = writeCodeToFile "assets/oracle.plutus" compileOracleCode

-- ========== Apply the parameters while serializing the script =============
{-
{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: OracleParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator . mkOracleValidator

validator :: OracleParams -> Validator
validator oracleParams = mkValidatorScript $
    $$(PlutusTx.compile [|| mkWrappedValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oracleParams

saveOracleScript :: String -> PubKeyHash -> IO ()
saveOracleScript symbol pkh = do
    let
    writeValidatorToFile fp $ validator op
    where
        op = OracleParams
            { oNFT= parseToken symbol
            , oOperator   = pkh
            }
        fp = printf "assets/oracle-%s-%s.plutus" (take 3 (show pkh)) $ take 3 (show pkh)

parseToken :: String -> AssetClass
parseToken s =
  let
    (x, y) = span (Prelude./= '.') s
  in
    AssetClass (fromString x, fromString $ tail y)
-}
