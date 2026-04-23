module Adapters exposing (decoder, encode, exhaustive, fuzzer)

import Exhaustive
import Fuzz
import IR exposing (IR(..), IRType)
import Json.Decode as JD
import Json.Encode as JE


encode : IR.Codec a b -> a -> JE.Value
encode codec value =
    IR.toIR codec value
        |> encodeAdapter


decoder : IR.Codec a b -> JD.Decoder b
decoder codec =
    decodeAdapter
        |> JD.andThen
            (\ir ->
                case IR.fromIR codec ir of
                    Ok s ->
                        JD.succeed s

                    Err IR.Error ->
                        JD.fail ""
            )


fuzzer : IR.Codec a b -> Fuzz.Fuzzer b
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


exhaustive : IR.Codec a b -> Exhaustive.Generator b
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
        IR.BoolType ->
            Exhaustive.bool |> Exhaustive.map IR.Bool

        IR.CharType ->
            Exhaustive.char |> Exhaustive.map IR.Char

        IR.StringType ->
            Exhaustive.string |> Exhaustive.map IR.String

        IR.IntType ->
            Exhaustive.int |> Exhaustive.map IR.Int

        IR.FloatType ->
            Exhaustive.float |> Exhaustive.map IR.Float

        IR.CustomType variants ->
            Exhaustive.values
                (List.indexedMap
                    (\idx variant ->
                        case variant of
                            IR.Variant0Type ->
                                Exhaustive.constant
                                    (IR.Custom idx IR.Variant0)

                            IR.Variant1Type arg ->
                                Exhaustive.map
                                    (\a -> IR.Custom idx (IR.Variant1 a))
                                    (exhaustiveAdapter arg)

                            IR.Variant2Type arg1 arg2 ->
                                exhaustiveAdapter arg1
                                    |> Exhaustive.andThen
                                        (\a1 ->
                                            Exhaustive.map
                                                (\a2 -> IR.Custom idx (IR.Variant2 a1 a2))
                                                (exhaustiveAdapter arg2)
                                        )
                    )
                    (List.reverse variants)
                )
                |> Exhaustive.andThen identity

        IR.ProductType fields ->
            -- this isn't a good exhaustive generator, it should rotate through
            -- the values of the fields... but it'll do for now.
            Exhaustive.new
                (\nth ->
                    List.foldl
                        (\field acc ->
                            acc
                                |> Maybe.andThen
                                    (\list ->
                                        let
                                            gen =
                                                exhaustiveAdapter field
                                        in
                                        gen.nth nth
                                            |> Maybe.map (\value -> value :: list)
                                    )
                        )
                        (Just [])
                        (List.reverse fields)
                )
                |> Exhaustive.map IR.Product


fuzzAdapter : IRType -> Fuzz.Fuzzer IR
fuzzAdapter irType =
    case irType of
        IR.BoolType ->
            Fuzz.bool |> Fuzz.map IR.Bool

        IR.CharType ->
            Fuzz.char |> Fuzz.map IR.Char

        IR.StringType ->
            Fuzz.string |> Fuzz.map IR.String

        IR.IntType ->
            Fuzz.int |> Fuzz.map IR.Int

        IR.FloatType ->
            Fuzz.float |> Fuzz.map IR.Float

        IR.CustomType variants ->
            Fuzz.oneOf
                (List.indexedMap
                    (\idx variant ->
                        case variant of
                            IR.Variant0Type ->
                                Fuzz.constant
                                    (IR.Custom idx IR.Variant0)

                            IR.Variant1Type arg ->
                                Fuzz.map
                                    (\a -> IR.Custom idx (IR.Variant1 a))
                                    (fuzzAdapter arg)

                            IR.Variant2Type arg1 arg2 ->
                                Fuzz.map2
                                    (\a1 a2 -> IR.Custom idx (IR.Variant2 a1 a2))
                                    (fuzzAdapter arg1)
                                    (fuzzAdapter arg2)
                    )
                    (List.reverse variants)
                )

        IR.ProductType fields ->
            fields
                |> Fuzz.traverse fuzzAdapter
                |> Fuzz.map IR.Product


encodeAdapter : IR -> JE.Value
encodeAdapter irType =
    case irType of
        IR.Bool b ->
            JE.object
                [ ( "bool", JE.bool b ) ]

        IR.Char c ->
            JE.object
                [ ( "char", JE.string (String.fromChar c) ) ]

        IR.String s ->
            JE.object
                [ ( "string", JE.string s ) ]

        IR.Int i ->
            JE.object
                [ ( "int", JE.int i ) ]

        IR.Float f ->
            JE.object
                [ ( "float", JE.float f ) ]

        IR.Custom selected variant ->
            JE.object
                [ ( "custom"
                  , JE.object
                        [ ( "tag", JE.int selected )
                        , ( "args"
                          , JE.list encodeAdapter
                                (case variant of
                                    IR.Variant0 ->
                                        []

                                    IR.Variant1 arg ->
                                        [ arg ]

                                    IR.Variant2 arg1 arg2 ->
                                        [ arg1
                                        , arg2
                                        ]
                                )
                          )
                        ]
                  )
                ]

        IR.Product fields ->
            JE.object
                [ ( "product"
                  , JE.list encodeAdapter fields
                  )
                ]


decodeAdapter : JD.Decoder IR
decodeAdapter =
    JD.oneOf
        [ JD.field "bool" JD.bool |> JD.map IR.Bool
        , JD.field "char" JD.string
            |> JD.andThen
                (\s ->
                    case String.uncons s of
                        Nothing ->
                            JD.fail "not a char"

                        Just ( c, _ ) ->
                            JD.succeed (IR.Char c)
                )
        , JD.field "string" JD.string |> JD.map IR.String
        , JD.field "int" JD.int |> JD.map IR.Int
        , JD.field "float" JD.float |> JD.map IR.Float
        , JD.field "custom"
            (JD.map2
                (\selected args ->
                    case args of
                        [] ->
                            Just (IR.Custom selected IR.Variant0)

                        [ arg ] ->
                            Just (IR.Custom selected (IR.Variant1 arg))

                        [ arg1, arg2 ] ->
                            Just (IR.Custom selected (IR.Variant2 arg1 arg2))

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
        , JD.field "product"
            (JD.list (JD.lazy (\_ -> decodeAdapter))
                |> JD.map IR.Product
            )
        ]
