{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}

module NFTPolicy where

import           Plutus.V1.Ledger.Value     (flattenValue)
import           Plutus.V2.Ledger.Api       (BuiltinData, CurrencySymbol,
                                             MintingPolicy,
                                             ScriptContext (scriptContextTxInfo),
                                             TokenName (unTokenName),
                                             TxId (TxId, getTxId),
                                             TxInInfo (txInInfoOutRef),
                                             TxInfo (txInfoInputs, txInfoMint),
                                             TxOutRef (TxOutRef, txOutRefId, txOutRefIdx),
                                             mkMintingPolicyScript, PubKeyHash)
import qualified PlutusTx
import           PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import           PlutusTx.Prelude           (Bool (False, True), Eq ((==)), any,
                                             traceIfFalse, ($), (&&), (.))
import           Prelude                    (IO, undefined)
import           Utilities                  (bytesToHex, currencySymbol,
                                             wrapPolicy, writeCodeToFile,
                                             writePolicyToFile)
import Plutus.V2.Ledger.Contexts (findTxInByTxOutRef)
import PlutusTx.Prelude (Maybe(Just, Nothing))
-- import PlutusTx (liftCode, applyCode)

{-# INLINABLE mkNFTPolicy #-}

mkNFTPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkNFTPolicy oRef _ ctx =    traceIfFalse "The UTxO must be consumed" utxoConsumed &&
                            traceIfFalse "Only 1 NFT can be minted" rightAmountMinted
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        utxoConsumed :: Bool
        utxoConsumed = case findTxInByTxOutRef oRef info of
                        Just _ -> True
                        Nothing -> False

        rightAmountMinted :: Bool
        rightAmountMinted = case flattenValue $ txInfoMint info of
                            [( _ , _ , amt )] -> amt == 1
                            _                 -> False

{-# INLINABLE wrappedNFTPolicy #-}

-- ======== Apply the parameters in lucid ========
wrappedNFTPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedNFTPolicy tid idx = wrapPolicy $ mkNFTPolicy oRef
    where
            oRef :: TxOutRef
            oRef = TxOutRef (TxId $ PlutusTx.unsafeFromBuiltinData tid) (PlutusTx.unsafeFromBuiltinData idx)

compiledNFTCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledNFTCode = $$(PlutusTx.compile [|| wrappedNFTPolicy ||])

saveNFTCode :: IO()
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