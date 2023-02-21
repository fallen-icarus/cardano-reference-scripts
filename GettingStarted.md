# Getting Started

:warning: Assumes a local PreProduction Testnet node running locally and `cardano-cli` installed since it is used to actually build and sign transactions.

Template bash scripts that follow these steps are available [here](scripts/).

When testing, it is highly recommended that you change the string passed to the mkBeaconPolicy function [here](src/CardanoSwaps.hs#L274). When developers make mistakes (myself included), it can create bad/locked utxos that will appear when you query the beacons. This can complicate your own testing. To avoid this, this extra parameter was added. Change the string to something unique to you. **Do this before building the executable in the installations section.** You should remember to change it to the desired string for mainnet.

---
## Table of Contents
- [Installing](#installing)

---
## Installing
Instructions are adapted from the [plutus-pioneers-program](https://github.com/input-output-hk/plutus-pioneer-program) week 1 exercise.

1. Install NixOS cross-referencing the following resources.
     - https://nixos.org/download.html
     - https://docs.plutus-community.com
     - A few resources to understand the what and why regarding NixOS
       - https://nixos.org/manual/nix/stable
       - https://serokell.io/blog/what-is-nix
2. Set-up IOHK binary caches [How to set up the IOHK binary caches](https://github.com/input-output-hk/plutus-apps#iohk-binary-cache). "If you do not do this, you will end up building GHC, which takes several hours. If you find yourself building GHC, *stop* and fix the cache."

3. After adding the cache, you will need to restart the nix service. This can be done by executing `sudo systemctl restart nix` or by restarting your machine. If the cache was configured properly, you should see a lot of `copying path ... from 'https://cache.iog.io'` when you execute `nix-shell` in the next step.

4. Execute the following:
```
git clone https://github.com/fallen-icarus/cardano-reference-scripts
git clone https://github.com/input-output-hk/plutus-apps
cd plutus-apps
git checkout v1.0.0
nix-shell           # this may take a while the first time

# Your terminal should now have a nix-shell prompt

cd ../cardano-reference-scripts
cabal clean
cabal update
cabal build all
```
The `cardano-reference-scripts` CLI program should now be at `dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-reference-scripts-0.1.0.0/x/cardano-reference-scripts/build/cardano-reference-scripts/cardano-reference-scripts`. Move the program to somewhere in your $PATH.

You can now exit the nix-shell with `exit`.

All `cardano-reference-scripts` subcommands have an associated `--help` option. The functionality is meant to feel like `cardano-cli`.

## Create a shared reference script
### Calculate the hash of the reference script to be added - needed for token name
``` Bash
scriptHash=$(cardano-cli transaction policyid \
  --script-file referenceScript.plutus)
```

### Export the beacon policy
``` Bash
cardano-reference-scripts export-policy \
  --out-file beacon.policy
```

### Create the beacon redeemer
``` Bash
cardano-reference-scripts redeemer \
  --mint-beacon $scriptHash \
  --out-file mint.json
```

### Get the beacon policy id
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacon.plutus)
```

### Helper beacon variable
``` Bash
beacon="${beaconPolicyId}.${scriptHash}"
```

### Export the helper script
``` Bash
cardano-reference-scripts export-helper-script \
  --out-file helper.plutus
```

### Create the helper script address using your staking credential
``` Bash
cardano-cli address build \
  --payment-script-file helper.plutus \
  --stake-verification-key-file ownerStake.vkey \
  --testnet-magic 1 \
  --out-file helper.addr
```

### Create the datum for the helper script
``` Bash
cardano-reference-scripts datum \
  --out-file beaconSymbol.json
```

### Create transaction
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <owner_utxo> \
  --tx-out "$(cat helper.addr) + 20000000 lovelace + 1 ${beacon}" \
  --tx-out-inline-datum-file beaconSymbol.json \
  --tx-out-reference-script-file referenceScript.plutus \
  --mint "1 ${beacon}" \
  --mint-script-file beacon.plutus \
  --mint-redeemer-file mint.json \
  --change-address $(cat owner.addr) \
  --tx-in-collateral <owner_collateral_utxo> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file ownerPayment.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

## Close a shared reference script
### Calculate the hash of the reference script to be removed - needed for token name
If you know the hash of the reference script, you can set this variable to that hash. It would be the token name of the beacon stored with the reference script you want to remove.

``` Bash
scriptHash=$(cardano-cli transaction policyid \
  --script-file referenceScript.plutus)
```

### Export the beacon policy
``` Bash
cardano-reference-scripts export-policy \
  --out-file beacon.plutus
```

### Create the beacon redeemer
``` Bash
cardano-reference-scripts redeemer \
  --burn-beacon \
  --out-file burn.json
```

### Get the beacon policy id
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacon.plutus)
```

### Helper beacon variable
``` Bash
beacon="${beaconPolicyId}.${scriptHash}"
```

### Export the helper script
``` Bash
cardano-reference-scripts export-helper-script \
  --out-file helper.plutus
```

The helper script can work with any redeemer. This example just uses the same redeemer as for the beacon policy.

### Create transaction
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <helper_script_utxo_with_beacon_and_ref_script> \
  --tx-in-script-file helper.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file burn.json \
  --mint "-1 ${beacon}" \
  --mint-script-file beacon.plutus \
  --mint-redeemer-file burn.json \
  --change-address $(cat owner.addr) \
  --required-signer-hash $(cat ownerStake.pkh) \
  --tx-in-collateral <owner_utxo_for_collateral> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file ownerPayment.skey \
  --signing-key-file ownerStake.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

---
## Staking PubKey Hash
To get the staking pubkey hash from a staking verification key, you can use this command:

``` Bash
cardano-cli stake-address key-hash \
  --stake-verification-key-file ownerStake.vkey \
  --out-file ownerStake.pkh
```

---
## Querying the chain for a usable reference script
### Query command
``` Bash
cardano-reference-scripts query \
  --script-hash <target_script_hash> \
  --preprod-testnet $(cat apiKey.txt) \
  --stdout
```

You can see the other available options with `cardano-reference-scripts query --help`.

### Query response
This is what an example query would look like when piped into jq:
``` JSON
{
  "reference_script_tx_ix": "0060cbba981e2785ed5892a7e35b883bcd853d66900c221ebfda3114b460c25a#0"
}
```

:important: When using blockfrost, the query command will return an error of `"The requested component has not been found."` when that beacon has never been minted before. This is due to the beacon name being part of the Blockfrost api url like:

``` Url
https://cardano-preprod.blockfrost.io/api/v0/assets/{beacon_name}/addresses
```

A future version of this program may address this issue.
