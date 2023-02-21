# Variables
dir="../assets/reference-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacon.plutus"
beaconRedeemerFile="${dir}burn.json"

helperScriptFile="${dir}helper.plutus"
helperAddrFile="${dir}helper.addr"

referenceScriptFile="../always.plutus"  # This is your reference script file - this is needed for the token name

# Calculate the hash of the reference script to be removed - needed for token name
scriptHash=$(cardano-cli transaction policyid \
  --script-file $referenceScriptFile)

# Export the beacon policy
cardano-reference-scripts export-policy \
  --out-file $beaconPolicyFile

# Create the beacon redeemer
cardano-reference-scripts redeemer \
  --burn-beacon \
  --out-file $beaconRedeemerFile

# Get the beacon policy id
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile)

# Helper beacon variable
beacon="${beaconPolicyId}.${scriptHash}"

# Export the helper script
cardano-reference-scripts export-helper-script \
  --out-file $helperScriptFile

# Create transaction
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 0060cbba981e2785ed5892a7e35b883bcd853d66900c221ebfda3114b460c25a#1 \
  --tx-in 0060cbba981e2785ed5892a7e35b883bcd853d66900c221ebfda3114b460c25a#0 \
  --tx-in-script-file $helperScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $beaconRedeemerFile \
  --mint "-1 ${beacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --change-address $(cat ../assets/wallets/01.addr) \
  --required-signer-hash $(cat ../assets/wallets/01Stake.pkh) \
  --tx-in-collateral bc54229f0755611ba14a2679774a7c7d394b0a476e59c609035e06244e1572bb#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/01.skey \
  --signing-key-file ../assets/wallets/01Stake.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"