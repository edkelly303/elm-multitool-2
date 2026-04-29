module Main exposing (..)

import Adapters.Diff
import Adapters.Fuzz
import Adapters.Json
import Adapters.Random
import Fuzz
import Html
import IR
import Json.Decode as JD
import Json.Encode as JE
import Random


type Example
    = Yellow
    | Green String Record
    | Red Char (List Bool)


type alias Record =
    { field1 : String
    , field2 : Int
    }


recordCodec : IR.Codec Record Record
recordCodec =
    IR.succeed Record
        |> IR.andMap .field1 IR.string
        |> IR.andMap .field2 IR.int


exampleCodec : IR.Codec Example Example
exampleCodec =
    IR.custom
        (\red yellow green value ->
            case value of
                Red b s ->
                    red b s

                Yellow ->
                    yellow

                Green s r ->
                    green s r
        )
        |> IR.variant2 Red IR.char (IR.list IR.bool)
        |> IR.variant0 Yellow
        |> IR.variant2 Green IR.string recordCodec
        |> IR.endCustom



main : Html.Html msg
main =
    let
        codec = 
            IR.list IR.bool
        ( old, _ ) =
            ([False], ())
            --Random.step (Adapters.Random.generator codec) (Random.initialSeed 0)

        ( new, _ ) =
            ([], ())
            --Random.step (Adapters.Random.generator codec) (Random.initialSeed 1)

        diff =
            Adapters.Diff.diff codec old new

        patched =
            Adapters.Diff.patch codec diff old
            
        fuzzed =
            Fuzz.examples 2 (Adapters.Fuzz.fuzzer codec)

        encoded =
            JE.encode 2 (Adapters.Json.encode codec old)

        decoded =
            JD.decodeString (Adapters.Json.decoder codec) encoded


    in
    Html.pre []
        [ head "Generator ('old' value)"
        , show old
        , show (IR.fromInput codec old)
        , show (IR.irType codec)
        , head "Generator ('new' value)"
        , show new
        , show (IR.fromInput codec new)
        , head "Diff between 'old' & 'new'"
        , show diff
        , head "Patch 'old' with diff"
        , show patched
        , head "Did patch work?"
        , show (patched == Ok new)
        , head "JSON encoder"
        , Html.text encoded
        , head "JSON decoder"
        , show decoded
        , head "Fuzzer"
        , show fuzzed
        ]


head : String -> Html.Html msg
head txt =
    Html.h3 [] [ Html.text txt ]


show : a -> Html.Html msg
show a =
    Html.div [] [Html.text (Debug.toString a)]
