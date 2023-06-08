# Demo Stablecoin

## Overview


This is a demo stablecoin dApp created using haskell framework called Plutus built on the Cardano blockchain.

### Why build this project?

I started this project building a demo stablecoin dApp with Plutus because I wanted to apply my new skills and knowledge in a practical way. I had recently learned about Plutus, a functional programming language for smart contracts on Cardano, and I was excited to see what I could create with it. I also wanted to gain experience in building dApps.

### What did you learn?

Building a dApp that mints stable coins using Plutus on the Cardano blockchain give me a deep understanding of how these technologies work. It will also taught me how to develop decentralized applications on a blockchain platform. This is a valuable skill that can be used to build a variety of applications, such as financial applications, games, and social networks.

### How does the dApp Work?

Now that we have answered the "What" and "Why" of the project it's time to understand the "How". Which in my opinion is the most important question of the three must know questions about a project.

In this section we will see how the stablecoin works and see the taught process that lead to the dApp being the way it is. We are going to use the divide and conquer principle and split the problem in to multiple sections and take it one step at a time to see how this dApp functions.

But first let us take a look at the four main components that will be used though out he dApp and explain briefly what they are used for.

#### 1. NFT Minting Policy

This policy is used to generate an NFT (non-fungible tokens) that will be used later on in the oracle validator as an identifier

#### 2. Oracle Validator

This validator is used to ensure the stablecoin and the pegged currency is in sync by storing the rate of the pegged currency and the collateral currency in it's state.

#### 3. Stablecoin Minting Policy

This policy is used for minting and burning of the stable coins.

#### 4. Reserve Validator

This validator is used to store the collateral amount in ada that has been exchanged by when minting the stable coins and will be exchanged back when burning them using the rate stored at the oracle validator

Now that we have a basic understanding of what the script are used for we can take a deeper look at how we achieve using this scripts to create stablecoin dApp.

#### Deploy Oracle Validator

This is the first step where we deploy the oracle script with it's initial datums (state) and parameters which include:

- The developer's PubKeyHash which is used to prevent other users from tampering with the USD/ADA rate
- The NFT which will be used to identify the UTxO that contains the datum (state)

So since the NFT is required as a parameter, the first thing to do is for the developer to create a transaction to mint NFT using the _`NFT minting policy`_ and store it in his/her address then use that NFT to store the `USD/ADA rate` as well as the `minted amount` which will have a value of `0` and `can mint` which will have a value of `true`. This is illustrated in the diagram below.

![Deploy Oracle](/doc-assets/exports/deploy_oracle.svg)

#### Deploy Reserve Validation & Minting Policy

This is the second step where we deploy the _`Reserve validator`_ and _`Minting policy`_ in the same transaction at a reference script in different UTxOs at a burn address (where the validator always is false). This makes it easier to make the access the scripts as we don't have to include them in every transaction when we use them letter on we can simply reference the UTxO they sit at.

![Deploy Reserve Validation & Minting Policy](/doc-assets//exports/deploy_reserve_validator_%26_minting_policy.svg)

#### Update Oracle

This is the step where developer will change the datum `USD/ADA rate`. In this step we have two possible cases where:

1. previous `USD/ADA rate` is greater than the new `USD/ADA rate`
2. previous `USD/ADA rate` is less than the new `USD/ADA rate`

In the first case, where the old rate is higher than the new rate, the developer has to fill in the difference that will result from the rate change in ada. Consequently if the stable coins in circulation where to all be burnt there would be enough funds to return.

In the second case, where the old rate is lower than the new rate, the developer will take the difference that will result from the rate change in ada and have enough funds in the reserve to exchange for the circulating stable coins.

> __Note__: Currently the rate change is done manually by the developer through the front-end but we plan to change it by setting up a back-end that will handle the rate change automatically using a trustworthy API in the future.  

![Update Oracle](/doc-assets/exports/update_oracle.svg)

#### Mint Stablecoin

There are a few things that is happening in this transaction, so lets take it step by step:

1. The transaction takes:

    - A UTxO containing the amount of collateral to exchange for a stablecoin and the developer fee from the user
    - A UTxO containing the minting policy from a burn address, locked by always false validator, as a reference script
    - A UTxO containing the NFT from an address locked by the oracle validator

2. The transaction returns:

    - A UTxO to the reserve validator containing the collateral used to mint the stable coin
    - A UTxO to the user containing the stablecoin minted using the collateral provided
    - A UTxO to the developer containing the developer fee provided by the user
    - A UTxO to an address locked by the oracle validator with NFT and new value of minted amount in the datum that is the sum of the old value and the minted amount

The mint stablecoin transaction runs the minting policy, provided as a reference script, to validate wether the collateral is enough to mint the specified stable coins using the rate from the datum in the UTxO containing the NFT locked at the oracle validator. It also updates the datum found in the UTxO the has the NFT.
The transaction will also run the oracle validator to validate the datum update.

![Mint Stablecoin](/doc-assets/exports/mint_stablecoin.svg)

#### Burn Stablecoin

There are a few things that is happening in this transaction as well, so we'll follow the above example and take it step by step:

1. The transaction takes:

    - A UTxO containing the amount of stable coins to exchange for ada which is stored at an address locked by the reserve validator from the user
    - A UTxO containing the collateral in ada from an address locked but the reserve validator
    - A UTxO containing the minting policy from a burn address, locked by always false validator, as a reference script
    - A UTxO containing the reserve validator from a burn address, locked by always false validator, as a reference script
    - A UTxO containing the NFT from an address locked by the oracle validator

2. The transaction returns:

    - A UTxO to the user containing the collateral exchanged for burning the stable coins
    - A UTxO to an address, locked by the oracle validator, with NFT and new value of minted amount in the datum that is the sum of the old value and the minted amount

The transaction will run the minting policy to check if indeed the stable coins are worth the collateral given to the user. It also run the oracle validator to validate the datum update.

> __Note__: The UTxOs containing the reserve validator and the minting policy as a reference script are reference but not consumed by the transaction. On the other hand, the UTxOs at an address locked by the reserve validator and the oracle validator are consumed.

![Burn Stablecoin](/doc-assets//exports/burn_stablecoin.svg)

#### Toggle Oracle

This transaction is relatively simple. It is used to change the _`can mint`_ datum.

The _`can mint`_ datum has two possible states which will determine the minting is allowed or not. When the value is `true` minting is allowed and When the value is `false` minting is not allowed, effectively deleting the oracle. This makes it so that even if the developer wants to stop minting, burning will always be allowed.

This transaction consumes a UTxO, that has the right NFT, found at an address locked by the oracle validator.


![Toggle Oracle](/doc-assets//exports/toggle_oracle.svg)