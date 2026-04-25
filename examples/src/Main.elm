module Main exposing (..)

import Adapters.Diff
import Adapters.Fuzz
import Adapters.Json
import Adapters.Random
import Fuzz
import Html
import IR exposing (Codec)
import Json.Decode as JD
import Json.Encode as JE
import Random


type Example
    = Yellow
    | Green String Record
    | Red Char (List Int)


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
        |> IR.variant2 Red IR.char (IR.list IR.int)
        |> IR.variant0 Yellow
        |> IR.variant2 Green IR.string recordCodec
        |> IR.endCustom


exampleFuzzer : Fuzz.Fuzzer Example
exampleFuzzer =
    Adapters.Fuzz.fuzzer exampleMultitool


exampleDecoder : JD.Decoder Example
exampleDecoder =
    Adapters.Json.decoder exampleMultitool


encodeExample : Example -> JE.Value
encodeExample =
    Adapters.Json.encode exampleMultitool


diffExample : Example -> Example -> Adapters.Diff.Diff
diffExample =
    Adapters.Diff.diff exampleMultitool


patchExample : Adapters.Diff.Diff -> Example -> Result IR.Error Example
patchExample =
    Adapters.Diff.patch exampleMultitool


exampleGenerator : Random.Generator Example
exampleGenerator =
    Adapters.Random.generator exampleMultitool



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
            Fuzz.examples 2 exampleFuzzer

        encoded =
            JE.encode 2 (JE.list encodeExample fuzzed)

        decoded =
            JD.decodeString (JD.list exampleDecoder) encoded

        ( old, seed ) =
            Random.step exampleGenerator (Random.initialSeed 1000)

        ( new, _ ) =
            Random.step exampleGenerator seed

        diff =
            diffExample old new

        patched =
            patchExample diff old
    in
    Html.pre []
        [ head "Fuzzer"
        , show fuzzed
        , head "Generator"
        , show old
        , show new
        , head "Diff"
        , show diff
        , head "Patch"
        , show patched
        , head "JSON encoder"
        , Html.text encoded
        , head "JSON decoder"
        , show decoded

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
