module Adapters.Random exposing (..)

import IR exposing (IR, IRType)
import Random
import Random.Char
import Random.Extra
import Random.Float
import Random.Int
import Random.String


generator : IR.Codec input output -> Random.Generator output
generator codec =
    IR.irType codec
        |> randomAdapter
        |> Random.andThen
            (\ir ->
                case IR.toOutput codec ir of
                    Ok b ->
                        Random.constant b

                    Err IR.Error ->
                        -- let's hope this never happens...
                        generator codec
            )


randomAdapter : IRType -> Random.Generator IR
randomAdapter irType =
    case irType of
        IR.BoolType ->
            Random.uniform False [ True ] |> Random.map IR.Bool

        IR.CharType ->
            Random.Char.basicLatin |> Random.map IR.Char

        IR.StringType ->
            Random.String.rangeLengthString 0 10 Random.Char.basicLatin |> Random.map IR.String

        IR.IntType ->
            Random.Int.anyInt |> Random.map IR.Int

        IR.FloatType ->
            Random.Float.anyFloat |> Random.map IR.Float

        IR.CustomType firstVariant restVariants ->
            let
                variantTypeToGenerator idx variant =
                    case variant of
                        IR.Variant0Type ->
                            Random.constant
                                (IR.Custom idx IR.Variant0)

                        IR.Variant1Type arg ->
                            Random.map
                                (\a -> IR.Custom idx (IR.Variant1 a))
                                (randomAdapter arg)

                        IR.Variant2Type arg1 arg2 ->
                            Random.map2
                                (\a1 a2 -> IR.Custom idx (IR.Variant2 a1 a2))
                                (randomAdapter arg1)
                                (randomAdapter arg2)
            in
            Random.Extra.choices
                (variantTypeToGenerator (List.length restVariants) firstVariant)
                (List.indexedMap variantTypeToGenerator (List.reverse restVariants))

        IR.ProductType fields ->
            fields
                |> Random.Extra.traverse randomAdapter
                |> Random.map IR.Product

        IR.ListType itemType ->
            Random.int 0 10
                |> Random.andThen (\int -> Random.list int (randomAdapter itemType))
                |> Random.map IR.List
