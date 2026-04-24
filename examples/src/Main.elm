module Main exposing (..)

import Adapters.Diff
import Adapters.Fuzz
import Adapters.Json
import Fuzz
import Html
import IR exposing (Codec)
import Json.Decode as JD
import Json.Encode as JE


type Example
    = Yellow
    | Green String Record
    | Red Bool String


type alias Record =
    { field1 : Bool
    , field2 : Bool
    }


recordCodec : Codec Record Record
recordCodec =
    IR.succeed Record
        |> IR.andMap .field1 IR.bool
        |> IR.andMap .field2 IR.bool


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
        |> IR.variant2 Green IR.string recordCodec
        |> IR.endCustom


fuzzer : Fuzz.Fuzzer Example
fuzzer =
    Adapters.Fuzz.fuzzer exampleMultitool


decoder : JD.Decoder Example
decoder =
    Adapters.Json.decoder exampleMultitool


encode : Example -> JE.Value
encode =
    Adapters.Json.encode exampleMultitool



-- there's something really slow about the exhaustive generator adapter, let's
-- switch it off for now...
--
-- exhaustive : Exhaustive.Generator Example
-- exhaustive =
--     Adapters.exhaustive exampleMultitool


main : Html.Html msg
main =
    let
        fuzzed =
            Fuzz.examples 2 fuzzer

        encoded =
            JE.encode 2 (JE.list encode fuzzed)

        decoded =
            JD.decodeString (JD.list decoder) encoded
    in
    Html.pre []
        [ head "Fuzzer"
        , show fuzzed
        , head "JSON encoder"
        , Html.text encoded
        , head "JSON decoder"
        , show decoded
        , head "Differ"
        , show (Adapters.Diff.diff recordCodec { field1 = False, field2 = False } { field1 = True, field2 = False })

        -- there's something really slow about the exhaustive generator adapter,
        -- let's switch it off for now...
        --
        -- , head "Exhaustive generator"
        -- , show (exhaustive.nth 0)
        ]


head : String -> Html.Html msg
head txt =
    Html.h3 [] [ Html.text txt ]


show : a -> Html.Html msg
show a =
    Html.text (Debug.toString a)
