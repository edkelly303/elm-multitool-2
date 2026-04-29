module Example exposing (..)

import Adapters.Diff
import Adapters.Fuzz
import Expect
import Fuzz exposing (Fuzzer)
import IR
import Test exposing (..)


type alias Record =
    { bool : Bool
    , int : Int
    , float : Float
    , string : String
    , char : Char
    , custom : Custom
    }


recordCodec : IR.Codec Record Record
recordCodec =
    IR.succeed Record
        |> IR.andMap .bool IR.bool
        |> IR.andMap .int IR.int
        |> IR.andMap .float IR.float
        |> IR.andMap .string IR.string
        |> IR.andMap .char IR.char
        |> IR.andMap .custom customCodec


type Custom
    = Var0
    | Var1 (List Bool)
    | Var2 Int ( Bool, Char )


customCodec : IR.Codec Custom Custom
customCodec =
    IR.custom
        (\v0 v1 v2 v ->
            case v of
                Var0 ->
                    v0

                Var1 l ->
                    v1 l

                Var2 i r ->
                    v2 i r
        )
        |> IR.variant0 Var0
        |> IR.variant1 Var1 (IR.list IR.bool)
        |> IR.variant2 Var2 IR.int (IR.tuple IR.bool IR.char)
        |> IR.endCustom


recordFuzzer : Fuzzer Record
recordFuzzer =
    Adapters.Fuzz.fuzzer recordCodec


irTests : Test
irTests =
    Test.describe "IR"
        [ fuzz recordFuzzer "record `IR.fromInput` -> `IR.toOutput` roundtrip" <|
            \rec ->
                rec
                    |> IR.fromInput recordCodec
                    |> IR.toOutput recordCodec
                    |> Expect.equal (Ok rec)
        ]


diffTests : Test
diffTests =
    Test.describe "Diff"
        [ roundTrip recordCodec "record"
        , roundTrip IR.int "int"
        , roundTrip (IR.list IR.bool) "list bool"
        ]


roundTrip : IR.Codec b b -> String -> Test
roundTrip codec name =
    fuzz2
        (Adapters.Fuzz.fuzzer codec)
        (Adapters.Fuzz.fuzzer codec)
        (name ++ " diff -> patch roundtrip")
    <|
        \old new ->
            let
                diff =
                    Adapters.Diff.diff codec old new
                    |> Debug.log "diff"
            in
            Adapters.Diff.patch codec diff old
                |> Expect.equal (Ok new)
