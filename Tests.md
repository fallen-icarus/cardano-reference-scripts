# Tests

## Creating Shared Reference Scripts
- [x] Reference script and beacon must be stored in a reference vault address.
  - [x] Fail if stored in another script address.
  - [x] Fail if stored in a pubkey address.
- [x] Beacon can only be minted to a reference vault address with a staking credential.
  - [x] Fail if minted to a reference vault address without a staking credential.
- [x] Beacon must be stored with the reference script for that token name.
  - [x] Fail if beacon stored in different vault utxo than reference script.
  - [x] Fail if beacon stored with a different reference script than specified in token name.
  - [x] Fail if beacon not stored with a reference script.
- [x] Only the beacon for that reference script can be minted.
  - [x] Fail if minting more than just that reference script's beacon.
- [x] Only one beacon can be minted for that reference script.
  - [x] Fail if more than one beacon minted for that reference script.
- [x] Only the mint redeemer can be used to mint beacons.
  - [x] Fail if burn redeemer used to mint.
- [x] Beacon must be stored with inline datum of its policy id.
  - [x] Fail if beacon stored with datum of different policy id.
  - [x] Fail if beacon not stored with inline datum.

## Close Shared Reference Scripts
- [x] All beacons in inputs must be burned.
  - [x] Fail if beacon withdrawn.
  - [x] Fail if beacon left behind in vault address.
- [x] Staking credential must approve.
  - [x] Stake pubkey must sign tx.
  - [x] Staking script must be executed in tx.\
- [x] Always allow burning.
  - [x] Allows burning many beacons.
  - [x] Allows burning single beacons.
- [x] Must use burn redeemer.
  - [x] Fail if mint redeemer used to burn.