module Adapters.Fuzz exposing (..)

import Fuzz
import IR exposing (IR, IRType)


fuzzer : IR.Codec a b -> Fuzz.Fuzzer b
fuzzer codec =
    IR.toIRType codec
        |> fuzzAdapter
        |> Fuzz.andThen
            (\x ->
                case IR.fromIR codec x of
                    Ok y ->
                        Fuzz.constant y

                    Err IR.Error ->
                        Fuzz.invalid ""
            )


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

        IR.ListType itemType ->
            Fuzz.list (fuzzAdapter itemType)
                |> Fuzz.map IR.List
