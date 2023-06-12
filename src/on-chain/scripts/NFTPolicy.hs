{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# HLINT ignore "Use fewer imports" #-}

module NFTPolicy where

import Plutus.V1.Ledger.Value (flattenValue)
import Plutus.V2.Ledger.Api
  ( BuiltinData,
    CurrencySymbol,
    MintingPolicy,
    PubKeyHash,
    ScriptContext (scriptContextTxInfo),
    TokenName (unTokenName),
    TxId (TxId, getTxId),
    TxInInfo (txInInfoOutRef),
    TxInfo (txInfoInputs, txInfoMint),
    TxOutRef (TxOutRef, txOutRefId, txOutRefIdx),
    mkMintingPolicyScript,
  )
import Plutus.V2.Ledger.Contexts (findTxInByTxOutRef)
import qualified PlutusTx
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import PlutusTx.Prelude (Bool (False, True), Eq ((==)), Maybe (Just, Nothing), any, traceIfFalse, ($), (&&), (.))
import Utilities
  ( bytesToHex,
    currencySymbol,
    wrapPolicy,
    writeCodeToFile,
    writePolicyToFile,
  )
import Prelude (IO, undefined)

-- import PlutusTx (liftCode, applyCode)

{-# INLINEABLE mkNFTPolicy #-}
mkNFTPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkNFTPolicy oRef _ ctx =
  traceIfFalse "The UTxO must be consumed" utxoConsumed
    && traceIfFalse "Only 1 NFT can be minted" rightAmountMinted
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    utxoConsumed :: Bool
    utxoConsumed = case findTxInByTxOutRef oRef info of
      Just _ -> True
      Nothing -> False

    rightAmountMinted :: Bool
    rightAmountMinted = case flattenValue $ txInfoMint info of
      [(_, _, amt)] -> amt == 1
      _ -> False

{-# INLINEABLE wrappedNFTPolicy #-}
-- ======== Apply the parameters in lucid ========
wrappedNFTPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedNFTPolicy tid idx tn = wrapPolicy $ mkNFTPolicy oRef tn'
  where
    oRef :: TxOutRef
    oRef = TxOutRef (TxId $ PlutusTx.unsafeFromBuiltinData tid) (PlutusTx.unsafeFromBuiltinData idx)

    tn :: TokenName
    tn' = PlutusTx.unsafeFromBuiltinData tn

compiledNFTCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledNFTCode = $$(PlutusTx.compile [||wrappedNFTPolicy||])

saveNFTCode :: IO ()
saveNFTCode = writeCodeToFile "../assets/nft.plutus" compiledNFTCode

-- ========== Apply the parameters while serializing the script =============
{-
wrappedNFTPolicy :: TxOutRef -> BuiltinData -> BuiltinData -> ()
wrappedNFTPolicy = wrapPolicy . mkNFTPolicy

compiledNFTPolicy :: TxOutRef -> MintingPolicy
compiledNFTPolicy oRef = mkMintingPolicyScript ($$(PlutusTx.compile [|| wrappedNFTPolicy ||]) `applyCode` liftCode oRef)

saveNFTPolicy :: TxOutRef -> IO()
saveNFTPolicy = writePolicyToFile "./assets/nft.plutus" . compiledNFTPolicy
-}