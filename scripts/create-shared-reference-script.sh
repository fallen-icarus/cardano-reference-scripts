# Variables
dir="../assets/reference-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacon.plutus"
beaconRedeemerFile="${dir}mint.json"

helperScriptFile="${dir}helper.plutus"
helperAddrFile="${dir}helper.addr"

helperDatumFile="${dir}symbol.json"

referenceScriptFile="../always.plutus"  # This is your reference script file

# Calculate the hash of the reference script to be added - needed for token name
scriptHash=$(cardano-cli transaction policyid \
  --script-file $referenceScriptFile)

# Export the beacon policy
cardano-reference-scripts export-policy \
  --out-file $beaconPolicyFile

# Create the beacon redeemer
cardano-reference-scripts redeemer \
  --mint-beacon $scriptHash \
  --out-file $beaconRedeemerFile

# Get the beacon policy id
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile)

# Helper beacon variable
beacon="${beaconPolicyId}.${scriptHash}"

# Export the helper script
cardano-reference-scripts export-helper-script \
  --out-file $helperScriptFile

# Create the helper script address using your staking credential
cardano-cli address build \
  --payment-script-file $helperScriptFile \
  --stake-verification-key-file "../assets/wallets/01Stake.vkey" \
  --testnet-magic 1 \
  --out-file $helperAddrFile

# Create the datum for the helper script
cardano-reference-scripts datum \
  --out-file $helperDatumFile

# Create transaction
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in b1ee1a7a24dc9fb0257ceeb0e7071234b84daa7fbecc78d52a9d0af27a1480d2#1 \
  --tx-out "$(cat ${helperAddrFile}) + 20000000 lovelace + 1 ${beacon}" \
  --tx-out-inline-datum-file $helperDatumFile \
  --tx-out-reference-script-file $referenceScriptFile \
  --mint "1 ${beacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --change-address $(cat ../assets/wallets/01.addr) \
  --tx-in-collateral bc54229f0755611ba14a2679774a7c7d394b0a476e59c609035e06244e1572bb#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"