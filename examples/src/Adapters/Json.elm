module Adapters.Json exposing (..)

-- import Exhaustive

import IR exposing (IR)
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
                (JD.field "args" (JD.list (JD.lazy (\() -> decodeAdapter))))
                |> JD.andThen
                    (\maybeIR ->
                        case maybeIR of
                            Nothing ->
                                JD.fail ""

                            Just ir ->
                                JD.succeed ir
                    )
            )
        , JD.field "product"
            (JD.list (JD.lazy (\() -> decodeAdapter))
                |> JD.map IR.Product
            )
        ]
