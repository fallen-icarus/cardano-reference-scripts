# Cardano-Reference-Scripts

:warning: Knowledge of basic Haskell syntax and cardano-cli usage is assumed.

The Getting Started instructions can be found [here](GettingStarted.md).

---
## Table of Contents
- [Motivation](#motivation)
- [The Reference Beacon](#the-reference-beacon)
  - [The helper spending plutus script](#the-helper-spending-plutus-script)
  - [Reference Beacon minting requirements](#reference-beacon-minting-requirements)
  - [Reference Beacon burning requirements](#reference-beacon-burning-requirements)
  - [Querying the Reference Beacons](#querying-the-reference-beacons)

---
## Motivation

Sometimes there is a need to be able to share plutus scripts with other users. Without being able to share, every user would need his/her own copy of the plutus script. This application allows sharing reference scripts in a trustless and p2p manner.

---
## The Reference Beacon
Just like all native tokens on Cardano, this beacon token consists of a policy id and a token name. For this application, the token name is the script hash of the reference script being shared. Then, whenever a user wants to check if there is a reference utxo available for that plutus script, he/she can query the beacon tokens and find the utxos, if any.

### The helper spending plutus script
Since these beacon tokens are meant to represent the existence of a reference script at the utxo where the beacon token is located, it would be a problem if the reference script was consumed without burning the beacon token. For this reason, a separate spending script was created. Users who want to share a reference script must store the utxo in an address associated with this spending script. **Users do not lose custody of their assets while using this spending script.** Spending from an address associated with this spending script is only allowed if:

1. All beacons among the tx inputs are burned.
2. The address' staking credential must signal approval:
    - pubkey must sign
    - script must be executed in the same tx

In other words, the spending of utxos from the helper spending script is protected by the staking credential. If an helper address without a staking credential is used:

1. You will not be able to mint a beacon to this address. In other words, the reference script would not be broadcasted to other users.
2. **Anyone can spend from this address.**

This second feature was added to prevent permanent locking of funds. **Make sure you use the helper spending script with a staking credential.**

#### Datum and Redeemer for helper spending script
The datum for the helper script is just the policy id of whatever beacon is stored in that utxo. This means the helper beacon can be used with any beacon, not just the reference script beacons.

The helper script is redeemer agnostic. This means it can be used with any redeemer; it doesn't use the redeemer anyway. This means the helper script can use the same redeemer as whatever beacon minting policy is being used.

### Reference Beacon minting requirements
Minting is heavily controlled. In order to mint the beacons, all of the following must be true:

1) Only one beacon is minted in the tx.
2) The minted beacon uses the reference script's hash as the token name.
3) The beacon is minted to an address protected by the helper spending script.
4) The beacon is minted to an address with a staking credential (either a pubkey or a script).
5) The beacon is stored in a utxo containing the reference script that matches the beacon's token name.
6) The datum of the output containing the beacon must contain the proper policy id.

Once the beacon is at one of the helper script's addresses, the script does not allow consuming the beacon's utxo unless the beacon is also burned. This makes sure the beacon can never be in a utxo without the associated reference script.

### Reference Beacon burning requirements
Since minting and spending beacons are so heavily controlled, there is no need to regulate burning. Burning is allways allowed as long as the burn redeemer is only ussed to burn beacons.

### Querying the Reference Beacons
These are the api endpoints that can be used to query the Reference Beacons:

| Task | Koios Api | Blockfrost Api |
|--|--|--|
| Addresses with a beacon | [api](https://api.koios.rest/#get-/asset_address_list) | [api](https://docs.blockfrost.io/#tag/Cardano-Assets/paths/~1assets~1%7Basset%7D~1addresses/get)|
| UTxOs at the address | [api](https://api.koios.rest/#post-/address_info) | [api](https://docs.blockfrost.io/#tag/Cardano-Addresses/paths/~1addresses~1%7Baddress%7D~1utxos/get)|

Since only one reference utxo is needed, the `cardano-reference-script query` command will only return the first utxo it finds. Here is an example output when piped into jq:
``` JSON
{
  "reference_script_tx_ix": "0060cbba981e2785ed5892a7e35b883bcd853d66900c221ebfda3114b460c25a#0"
}
```

If there are no reference scripts currently available for that script hash, then the command will return
`No reference scripts available for that script hash.`