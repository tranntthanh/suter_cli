(*
 * Copyright 2019 Suterusu project <contact@suterusu.io>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)


open Command
open CommandDB
open ArgType

let dft_cdb db =
    let db = TypeDb.add "state_getMetadata" (TList []) db in
    let db = TypeDb.add "chain_getBlockHash" (TList []) db in
    let db = TypeDb.add "chain_getBlock" (TList [TString]) db in
    let db = TypeDb.add "chain_getFinalizedHead" (TList []) db in
    let db = TypeDb.add "state_getRuntimeVersion" (TList []) db in
    let db = TypeDb.add "state_getStorage" (TList [TString]) db in
    let db = TypeDb.add "state_subscribeStorage" (TList [TList [TString]]) db in
    let db = TypeDb.add "author_submitAndWatchExtrinsic" (TList [TString]) db in
    let db = TypeDb.add "author_submitExtrinsic" (TList [TString]) db in
    let db = TypeDb.add "author_pendingExtrinsics" (TList []) db in
    db
