module Adapters exposing (decoder, encode, exhaustive, fuzzer)

import Exhaustive
import Fuzz
import IR exposing (..)
import Json.Decode as JD
import Json.Encode as JE


encode : IRCodec a b -> a -> JE.Value
encode codec value =
    IR.toIR codec value
        |> encodeAdapter


decoder : IRCodec a b -> JD.Decoder b
decoder codec =
    decodeAdapter
        |> JD.andThen
            (\ir ->
                case IR.fromIR codec ir of
                    Ok s ->
                        JD.succeed s

                    Err Error ->
                        JD.fail ""
            )


fuzzer : IRCodec a b -> Fuzz.Fuzzer b
fuzzer codec =
    IR.toIRType codec
        |> fuzzAdapter
        |> Fuzz.andThen
            (\x ->
                case IR.fromIR codec x of
                    Ok y ->
                        Fuzz.constant y

                    Err _ ->
                        Fuzz.invalid ""
            )


exhaustive : IRCodec a b -> Exhaustive.Generator b
exhaustive codec =
    IR.toIRType codec
        |> exhaustiveAdapter
        |> Exhaustive.andThen
            (\x ->
                case IR.fromIR codec x of
                    Ok y ->
                        Exhaustive.constant y

                    Err _ ->
                        Exhaustive.empty
            )



-- adapters


exhaustiveAdapter : IRType -> Exhaustive.Generator IR
exhaustiveAdapter irType =
    case irType of
        BoolT ->
            Exhaustive.bool |> Exhaustive.map Bool

        StringT ->
            Exhaustive.string |> Exhaustive.map String

        CustomT variants ->
            Exhaustive.values
                (List.indexedMap
                    (\idx variant ->
                        case variant of
                            Variant0T ->
                                Exhaustive.constant
                                    (Custom idx Variant0)

                            Variant1T arg ->
                                Exhaustive.map
                                    (\a -> Custom idx (Variant1 a))
                                    (exhaustiveAdapter arg)

                            Variant2T arg1 arg2 ->
                                exhaustiveAdapter arg1
                                    |> Exhaustive.andThen
                                        (\a1 ->
                                            Exhaustive.map
                                                (\a2 -> Custom idx (Variant2 a1 a2))
                                                (exhaustiveAdapter arg2)
                                        )
                    )
                    (List.reverse variants)
                )
                |> Exhaustive.andThen identity


fuzzAdapter : IRType -> Fuzz.Fuzzer IR
fuzzAdapter irType =
    case irType of
        BoolT ->
            Fuzz.bool |> Fuzz.map Bool

        StringT ->
            Fuzz.string |> Fuzz.map String

        CustomT variants ->
            Fuzz.oneOf
                (List.indexedMap
                    (\idx variant ->
                        case variant of
                            Variant0T ->
                                Fuzz.constant
                                    (Custom idx Variant0)

                            Variant1T arg ->
                                Fuzz.map
                                    (\a -> Custom idx (Variant1 a))
                                    (fuzzAdapter arg)

                            Variant2T arg1 arg2 ->
                                Fuzz.map2
                                    (\a1 a2 -> Custom idx (Variant2 a1 a2))
                                    (fuzzAdapter arg1)
                                    (fuzzAdapter arg2)
                    )
                    (List.reverse variants)
                )


encodeAdapter : IR -> JE.Value
encodeAdapter irType =
    case irType of
        Bool b ->
            JE.object
                [ ( "bool", JE.bool b ) ]

        String s ->
            JE.object
                [ ( "string", JE.string s ) ]

        Custom selected variant ->
            JE.object
                [ ( "custom"
                  , JE.object
                        [ ( "tag", JE.int selected )
                        , ( "args"
                          , JE.list encodeAdapter
                                (case variant of
                                    Variant0 ->
                                        []

                                    Variant1 arg ->
                                        [ arg ]

                                    Variant2 arg1 arg2 ->
                                        [ arg1
                                        , arg2
                                        ]
                                )
                          )
                        ]
                  )
                ]


decodeAdapter : JD.Decoder IR
decodeAdapter =
    JD.oneOf
        [ JD.field "bool" JD.bool |> JD.map Bool
        , JD.field "string" JD.string |> JD.map String
        , JD.field "custom"
            (JD.map2
                (\selected args ->
                    case args of
                        [] ->
                            Just (Custom selected Variant0)

                        [ arg ] ->
                            Just (Custom selected (Variant1 arg))

                        [ arg1, arg2 ] ->
                            Just (Custom selected (Variant2 arg1 arg2))

                        _ ->
                            Nothing
                )
                (JD.field "tag" JD.int)
                (JD.field "args" (JD.list (JD.lazy (\_ -> decodeAdapter))))
                |> JD.andThen
                    (\mc ->
                        case mc of
                            Nothing ->
                                JD.fail ""

                            Just c ->
                                JD.succeed c
                    )
            )
        ]
