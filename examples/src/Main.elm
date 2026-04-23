module Main exposing (..)

import Adapters
import Exhaustive
import Fuzz
import Html
import IR exposing (Codec)
import Json.Decode as JD
import Json.Encode as JE


type Example
    = Yellow
    | Green String RecordField
    | Red Bool String


type alias RecordField =
    { field1 : Int, field2 : Char }


fieldCodec : Codec RecordField RecordField
fieldCodec =
    IR.succeed RecordField
        |> IR.andMap .field1 IR.int
        |> IR.andMap .field2 IR.char


exampleMultitool : Codec Example Example
exampleMultitool =
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
        |> IR.variant2 Red IR.bool IR.string
        |> IR.variant0 Yellow
        |> IR.variant2 Green IR.string fieldCodec
        |> IR.endCustom


fuzzer : Fuzz.Fuzzer Example
fuzzer =
    Adapters.fuzzer exampleMultitool


decoder : JD.Decoder Example
decoder =
    Adapters.decoder exampleMultitool


encoder : Example -> JE.Value
encoder =
    Adapters.encode exampleMultitool


exhaustive : Exhaustive.Generator Example
exhaustive =
    Adapters.exhaustive exampleMultitool



-- main


main : Html.Html msg
main =
    let
        fuzzed =
            Fuzz.examples 7 fuzzer

        encoded =
            JE.encode 2 (JE.list encoder fuzzed)

        decoded =
            JD.decodeString (JD.list decoder) encoded

        exhaustives =
            exhaustive.nth 70
    in
    Html.pre []
        [ head "Fuzzer"
        , show fuzzed
        , head "JSON encoder"
        , Html.text encoded
        , head "JSON decoder"
        , show decoded
        , head "Exhaustive generator"
        , show exhaustives
        ]


head : String -> Html.Html msg
head txt =
    Html.h3 [] [ Html.text txt ]


show : a -> Html.Html msg
show a =
    Html.text (Debug.toString a)
