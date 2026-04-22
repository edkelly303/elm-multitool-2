module Research.AdapterCombinators exposing
    ( bool
    , char
    , custom
    , dict
    , endCustom
    , endRecord
    , field
    , fix
    , fixP
    , float
    , int
    , list
    , map
    , maybe
    , record
    , result
    , set
    , string
    , triple
    , tuple
    , variant0
    , variant1
    , variant2
    , variant3
    , variant4
    , variant5
    )

import Dict
import Exhaustive
import Fuzz
import Json.Decode as JD
import Json.Encode as JE


type Example
    = Yellow
    | Green { int : Int } (Maybe Int)
    | Red Bool String


exampleMultitool adapter1 adapter2 adapter3 =
    custom
        (\red yellow green value ->
            case value of
                Red b s ->
                    red b s

                Yellow ->
                    yellow

                Green intRecord_ h ->
                    green intRecord_ h
        )
        |> fixP adapter1 (variant2 "Red" Red bool string)
        |> variant0 "Yellow" Yellow
        |> fixP adapter2 (variant2 "Green" Green intRecord (fix adapter3 (maybe int)))
        |> endCustom


intRecord =
    record (\int_ -> { int = int_ })
        |> field "int" .int int
        |> endRecord


fuzzer : Fuzz.Fuzzer Example
fuzzer =
    exampleMultitool
        fuzzAdapter
        fuzzAdapter
        fuzzAdapter
        fuzzAdapter


decoder : JD.Decoder Example
decoder =
    exampleMultitool
        decoderAdapter
        decoderAdapter
        decoderAdapter
        decoderAdapter


encoder : Example -> JE.Value
encoder =
    exampleMultitool
        encoderAdapter
        encoderAdapter
        encoderAdapter
        encoderAdapter


exhaustive : Exhaustive.Generator Example
exhaustive =
    exampleMultitool
        exhaustiveAdapter
        exhaustiveAdapter
        exhaustiveAdapter
        exhaustiveAdapter


decoderAdapter =
    { bool = JD.bool
    , string = JD.string
    , int = JD.int
    , custom = \_ -> Dict.empty
    , variant0 = \name ctor prev -> Dict.insert name (JD.succeed ctor) prev
    , variant1 =
        \name ctor dec1 prev ->
            Dict.insert name
                (JD.map ctor
                    (JD.index 0 dec1)
                )
                prev
    , variant2 =
        \name ctor dec1 dec2 prev ->
            Dict.insert name
                (JD.map2 ctor
                    (JD.index 0 dec1)
                    (JD.index 1 dec2)
                )
                prev
    , endCustom =
        \prev ->
            JD.field "tag" JD.string
                |> JD.andThen
                    (\tag ->
                        case Dict.get tag prev of
                            Nothing ->
                                JD.fail <| "tag " ++ tag ++ " did not match"

                            Just dec ->
                                JD.field "args" dec
                    )
    , record = JD.succeed
    , field = \name _ dec prev -> JD.map2 (|>) (JD.field name dec) prev
    , endRecord = Basics.identity
    , map = JD.map
    }


encoderAdapter =
    { bool = JE.bool
    , string = JE.string
    , int = JE.int
    , custom = \match -> match
    , variant0 =
        \name _ prev ->
            prev
                (JE.object
                    [ ( "tag", JE.string name )
                    , ( "args", JE.null )
                    ]
                )
    , variant1 =
        \name _ enc1 prev ->
            prev <|
                \arg1 ->
                    JE.object
                        [ ( "tag", JE.string name )
                        , ( "args", JE.list identity [ enc1 arg1 ] )
                        ]
    , variant2 =
        \name _ enc1 enc2 prev ->
            prev <|
                \arg1 arg2 ->
                    JE.object
                        [ ( "tag", JE.string name )
                        , ( "args", JE.list identity [ enc1 arg1, enc2 arg2 ] )
                        ]
    , endCustom = \prev -> prev
    , record = \_ -> []
    , field = \name getter enc prev -> \rec -> ( name, enc (getter rec) ) :: prev
    , endRecord = \prev -> \rec -> JE.object (List.reverse (prev rec))
    , map = \_ x -> x
    }


fuzzAdapter =
    { bool = Fuzz.bool
    , string = Fuzz.string
    , int = Fuzz.int
    , custom = \_ -> []
    , variant0 = \_ ctor prev -> Fuzz.constant ctor :: prev
    , variant1 = \_ ctor fuzzer1 prev -> Fuzz.map ctor fuzzer1 :: prev
    , variant2 = \_ ctor fuzzer1 fuzzer2 prev -> Fuzz.map2 ctor fuzzer1 fuzzer2 :: prev
    , endCustom = \prev -> Fuzz.oneOf prev
    , record = Fuzz.constant
    , field = \_ _ f -> Fuzz.andMap f
    , endRecord = Basics.identity
    , map = Fuzz.map
    }


exhaustiveAdapter =
    { string = Exhaustive.string
    , bool = Exhaustive.bool
    , int = Exhaustive.int
    , custom = \_ -> Exhaustive.customType
    , variant0 = \_ -> Exhaustive.variant0
    , variant1 = \_ -> Exhaustive.variant1
    , variant2 = \_ -> Exhaustive.variant2
    , endCustom = Basics.identity
    , record = Exhaustive.record
    , field = \_ _ e -> Exhaustive.field e
    , endRecord = Basics.identity
    , map = Exhaustive.map
    }


int =
    .int


bool =
    .bool


string =
    .string


float =
    .float


char =
    .char


list =
    .list


dict =
    .dict


set =
    .set


maybe a =
    custom
        (\nothing_ just_ variant ->
            case variant of
                Nothing ->
                    nothing_

                Just a_ ->
                    just_ a_
        )
        |> variant0 "Nothing" Nothing
        |> variant1 "Just" Just a
        |> endCustom


result x a =
    custom
        (\err_ ok_ variant ->
            case variant of
                Err x_ ->
                    err_ x_

                Ok a_ ->
                    ok_ a_
        )
        |> variant1 "Err" Err x
        |> variant1 "Ok" Ok a
        |> endCustom


tuple fst snd =
    record Tuple.pair
        |> field "First" Tuple.first fst
        |> field "Second" Tuple.second snd
        |> endRecord


triple fst snd thd =
    record (\fst_ snd_ thd_ -> ( fst_, snd_, thd_ ))
        |> field "First" (\( fst_, _, _ ) -> fst_) fst
        |> field "Second" (\( _, snd_, _ ) -> snd_) snd
        |> field "Third" (\( _, _, thd_ ) -> thd_) thd
        |> endRecord


custom match =
    \adapter -> adapter.custom match


variant0 name ctor prev =
    \adapter ->
        prev adapter
            |> adapter.variant0 name ctor


variant1 name ctor arg1 prev =
    \adapter ->
        prev adapter
            |> adapter.variant1 name ctor (arg1 adapter)


variant2 name ctor arg1 arg2 prev =
    \adapter ->
        prev adapter
            |> adapter.variant2 name ctor (arg1 adapter) (arg2 adapter)


variant3 name ctor arg1 arg2 arg3 prev =
    \adapter ->
        prev adapter
            |> adapter.variant3 name ctor (arg1 adapter) (arg2 adapter) (arg3 adapter)


variant4 name ctor arg1 arg2 arg3 arg4 prev =
    \adapter ->
        prev adapter
            |> adapter.variant4 name ctor (arg1 adapter) (arg2 adapter) (arg3 adapter) (arg4 adapter)


variant5 name ctor arg1 arg2 arg3 arg4 arg5 prev =
    \adapter ->
        prev adapter
            |> adapter.variant5 name ctor (arg1 adapter) (arg2 adapter) (arg3 adapter) (arg4 adapter) (arg5 adapter)


endCustom prev =
    \adapter ->
        prev adapter
            |> adapter.endCustom


record ctor =
    \adapter -> adapter.record ctor


field name getter arg prev =
    \adapter ->
        prev adapter
            |> adapter.field name getter (arg adapter)


endRecord prev =
    \adapter ->
        prev adapter
            |> adapter.endRecord


map f prev =
    \adapter ->
        prev adapter
            |> adapter.map f


fixP adapter multiTool prev =
    fix adapter (multiTool prev)


fix adapter multiTool _ =
    multiTool adapter
