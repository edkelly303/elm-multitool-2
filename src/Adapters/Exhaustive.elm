module Adapters.Exhaustive exposing (..)

import Exhaustive
import IR exposing (IR, IRType)



-- there's something really slow about the exhaustive generator adapter, let's
-- switch it off for now...


exhaustive : IR.Codec input output -> Exhaustive.Generator output
exhaustive codec =
    IR.irType codec
        |> exhaustiveAdapter
        |> Exhaustive.andThen
            (\x ->
                case IR.toOutput codec x of
                    Ok y ->
                        Exhaustive.constant y

                    Err _ ->
                        Exhaustive.empty
            )


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

        IR.CustomType firstVariant restVariants ->
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
                    (List.reverse (firstVariant :: restVariants))
                )
                |> Exhaustive.andThen identity

        IR.ProductType fields ->
            -- this isn't a good exhaustive generator, it should rotate through
            -- the values of the fields... but it'll do for now.
            Exhaustive.new
                (\nth ->
                    List.foldr
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
                        fields
                )
                |> Exhaustive.map IR.Product

        IR.ListType itemType ->
            Exhaustive.list 5 (exhaustiveAdapter itemType)
                |> Exhaustive.map IR.List
