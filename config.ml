open Command
open CommandDB
open ArgType

let dft_cdb db =
    let db = TypeDb.add "chain_getBlockHash" (TList [TInt]) db in
    let db = TypeDb.add "state_getRuntimeVersion" (TList []) db in
    let db = TypeDb.add "state_getStorage" (TList [TString]) db in
    let db = TypeDb.add "author_submitAndWatchExtrinsic" TString db in
    db
