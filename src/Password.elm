module Password exposing (compareHashes, generateHash, getHash)

import Bytes exposing (Bytes)
import Bytes.Encode as E
import Env
import Hex.Convert as Hex
import HmacSha1
import HmacSha1.Key as Key
import PBKDF2 exposing (pbkdf2)


type HashedPassword
    = HashedPassword String


getHash : HashedPassword -> String
getHash (HashedPassword hash) =
    hash


compareHashes : HashedPassword -> HashedPassword -> Bool
compareHashes (HashedPassword left) (HashedPassword right) =
    left == right


generateHash : String -> Result PBKDF2.Error HashedPassword
generateHash password =
    computeHash password Env.pbkdf2Salt
        |> Result.map HashedPassword


hmacSha1 : Bytes -> Bytes -> Bytes
hmacSha1 key message =
    HmacSha1.fromBytes (Key.fromBytes key) message
        |> HmacSha1.toBytes


computeHash : String -> String -> Result PBKDF2.Error String
computeHash password salt =
    let
        encodedPassword =
            password |> E.string |> E.encode

        encodedSalt =
            salt |> E.string |> E.encode
    in
    pbkdf2 ( hmacSha1, 20 ) encodedPassword encodedSalt 4096 20
        |> Result.map Hex.toString
