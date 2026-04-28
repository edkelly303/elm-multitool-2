module IR exposing
    ( Codec
    , IR(..), Variant(..), fromInput, toOutput, Error(..)
    , IRType(..), VariantType(..), irType
    , bool, char, string, int, float
    , list, maybe, result, tuple, triple
    , succeed, andMap
    , CustomCodec, custom, variant0, variant1, variant2, endCustom
    , map, contramap, andThen
    )

{-|


# elm-ir

Convert between Elm data types and an intermediate representation (IR)


## IR codecs

@docs Codec


## IR values

@docs IR, Variant, fromInput, toOutput, Error


## IR types

@docs IRType, VariantType, irType


## IR primitives

@docs bool, char, string, int, float


## IR combinators


### Common data types

@docs list, maybe, result, tuple, triple


### Record types

@docs succeed, andMap


### Custom types

@docs CustomCodec, custom, variant0, variant1, variant2, endCustom


### Transforming codec input and output

@docs map, contramap, andThen

-}

import Result.Extra


{-| TODO
-}
type Error
    = Error


{-| TODO
-}
type Codec input output
    = Codec
        { fromInput : input -> IR
        , toOutput : IR -> Result Error output
        , irType : IRType
        }


{-| TODO
-}
type IR
    = Bool Bool
    | Char Char
    | String String
    | Int Int
    | Float Float
    | Custom Int Variant
    | Product (List IR)
    | List (List IR)


{-| TODO
-}
type Variant
    = Variant0
    | Variant1 IR
    | Variant2 IR IR


{-| TODO
-}
type IRType
    = BoolType
    | CharType
    | StringType
    | IntType
    | FloatType
    | CustomType VariantType (List VariantType)
    | ProductType (List IRType)
    | ListType IRType


{-| TODO
-}
type VariantType
    = Variant0Type
    | Variant1Type IRType
    | Variant2Type IRType IRType


{-| TODO
-}
fromInput : Codec input output -> input -> IR
fromInput (Codec c) =
    c.fromInput


{-| TODO
-}
irType : Codec input output -> IRType
irType (Codec c) =
    c.irType


{-| TODO
-}
toOutput : Codec input output -> IR -> Result Error output
toOutput (Codec c) =
    c.toOutput


{-| TODO
-}
bool : Codec Bool Bool
bool =
    Codec
        { fromInput = Bool
        , toOutput =
            \ir ->
                case ir of
                    Bool b ->
                        Ok b

                    _ ->
                        Err Error
        , irType = BoolType
        }


{-| TODO
-}
char : Codec Char Char
char =
    Codec
        { fromInput = Char
        , toOutput =
            \ir ->
                case ir of
                    Char c ->
                        Ok c

                    _ ->
                        Err Error
        , irType = CharType
        }


{-| TODO
-}
string : Codec String String
string =
    Codec
        { fromInput = String
        , toOutput =
            \ir ->
                case ir of
                    String s ->
                        Ok s

                    _ ->
                        Err Error
        , irType = StringType
        }


{-| TODO
-}
int : Codec Int Int
int =
    Codec
        { fromInput = Int
        , toOutput =
            \ir ->
                case ir of
                    Int i ->
                        Ok i

                    _ ->
                        Err Error
        , irType = IntType
        }


{-| TODO
-}
float : Codec Float Float
float =
    Codec
        { fromInput = Float
        , toOutput =
            \ir ->
                case ir of
                    Float s ->
                        Ok s

                    _ ->
                        Err Error
        , irType = FloatType
        }


{-| TODO
-}
list : Codec a a -> Codec (List a) (List a)
list (Codec item) =
    Codec
        { fromInput = \items -> List (List.map item.fromInput items)
        , toOutput =
            \ir ->
                case ir of
                    List items ->
                        List.map item.toOutput items
                            |> Result.Extra.combine

                    _ ->
                        Err Error
        , irType = ListType item.irType
        }


{-| TODO
-}
maybe : Codec a a -> Codec (Maybe a) (Maybe a)
maybe item =
    custom
        (\just nothing variant ->
            case variant of
                Just a ->
                    just a

                Nothing ->
                    nothing
        )
        |> variant1 Just item
        |> variant0 Nothing
        |> endCustom


{-| TODO
-}
result : Codec x x -> Codec a a -> Codec (Result x a) (Result x a)
result x a =
    custom
        (\err ok variant ->
            case variant of
                Err x_ ->
                    err x_

                Ok a_ ->
                    ok a_
        )
        |> variant1 Err x
        |> variant1 Ok a
        |> endCustom


{-| TODO
-}
tuple : Codec a a -> Codec b b -> Codec ( a, b ) ( a, b )
tuple a b =
    succeed Tuple.pair
        |> andMap Tuple.first a
        |> andMap Tuple.second b


{-| TODO
-}
triple : Codec a a -> Codec b b -> Codec c c -> Codec ( a, b, c ) ( a, b, c )
triple a b c =
    succeed (\a_ b_ c_ -> ( a_, b_, c_ ))
        |> andMap (\( a_, _, _ ) -> a_) a
        |> andMap (\( _, b_, _ ) -> b_) b
        |> andMap (\( _, _, c_ ) -> c_) c


{-| TODO
-}
type CustomCodec input hasAtLeastOneVariant output
    = CustomCodec
        { match : input
        , fromIR : IR -> Result Error output
        , variantTypes : List VariantType
        , index : Int
        }


{-| TODO
-}
custom : input -> CustomCodec input Never output
custom match =
    CustomCodec
        { match = match
        , index = 0
        , fromIR = \_ -> Err Error
        , variantTypes = []
        }


{-| TODO
-}
variant0 :
    output
    -> CustomCodec (IR -> input) variantType output
    -> CustomCodec input () output
variant0 ctor (CustomCodec prev) =
    CustomCodec
        { match = prev.match <| Custom prev.index Variant0
        , index = prev.index + 1
        , fromIR =
            \ir ->
                case ir of
                    Custom selected Variant0 ->
                        if selected == prev.index then
                            Ok ctor

                        else
                            prev.fromIR ir

                    _ ->
                        prev.fromIR ir
        , variantTypes = Variant0Type :: prev.variantTypes
        }


{-| TODO
-}
variant1 :
    (arg1 -> output)
    -> Codec arg1 arg1
    -> CustomCodec ((arg1 -> IR) -> input) variantType output
    -> CustomCodec input () output
variant1 ctor (Codec argfns) (CustomCodec prev) =
    let
        thisVariant =
            Variant1Type argfns.irType
    in
    CustomCodec
        { match = prev.match <| \arg -> Custom prev.index (Variant1 (argfns.fromInput arg))
        , index = prev.index + 1
        , fromIR =
            \ir ->
                case ir of
                    Custom selected (Variant1 arg) ->
                        if selected == prev.index then
                            Result.map ctor (argfns.toOutput arg)

                        else
                            prev.fromIR ir

                    _ ->
                        prev.fromIR ir
        , variantTypes = thisVariant :: prev.variantTypes
        }


{-| TODO
-}
variant2 :
    (arg1 -> arg2 -> output)
    -> Codec arg1 arg1
    -> Codec arg2 arg2
    -> CustomCodec ((arg1 -> arg2 -> IR) -> input) variantType output
    -> CustomCodec input () output
variant2 ctor (Codec arg1fns) (Codec arg2fns) (CustomCodec prev) =
    let
        thisVariant =
            Variant2Type arg1fns.irType arg2fns.irType
    in
    CustomCodec
        { match = prev.match <| \arg1 arg2 -> Custom prev.index (Variant2 (arg1fns.fromInput arg1) (arg2fns.fromInput arg2))
        , index = prev.index + 1
        , fromIR =
            \ir ->
                case ir of
                    Custom selected (Variant2 arg1 arg2) ->
                        if selected == prev.index then
                            Result.map2 ctor (arg1fns.toOutput arg1) (arg2fns.toOutput arg2)

                        else
                            prev.fromIR ir

                    _ ->
                        prev.fromIR ir
        , variantTypes = thisVariant :: prev.variantTypes
        }


{-| TODO
-}
endCustom : CustomCodec (input -> IR) () output -> Codec input output
endCustom (CustomCodec prev) =
    Codec
        { fromInput = prev.match
        , toOutput = prev.fromIR
        , irType =
            case List.reverse prev.variantTypes of
                [] ->
                    -- we know this can't happen, because if the second type
                    -- variable of CustomCodec is `()`, then we know that we've
                    -- used at least one `variantX` function, so the list of
                    -- variants can't be empty. So it's ok to use a spurious
                    -- Variant0Type here, because this will never get produced.
                    CustomType Variant0Type []

                firstVariantType :: restVariantTypes ->
                    CustomType firstVariantType restVariantTypes
        }


{-| TODO
-}
succeed : output -> Codec input output
succeed ctor =
    Codec
        { fromInput = \_ -> Product []
        , toOutput =
            \ir ->
                case ir of
                    Product [] ->
                        Ok ctor

                    _ ->
                        Err Error
        , irType = ProductType []
        }


{-| TODO
-}
andMap :
    (input -> field)
    -> Codec field field
    -> Codec input (field -> output)
    -> Codec input output
andMap getter (Codec this) (Codec prev) =
    Codec
        { fromInput =
            \a ->
                case prev.fromInput a of
                    Product prevFields ->
                        Product (this.fromInput (getter a) :: prevFields)

                    _ ->
                        Product [ this.fromInput (getter a) ]
        , toOutput =
            \ir ->
                case ir of
                    Product (thisField :: prevFields) ->
                        Result.map2 (\ctor val -> ctor val)
                            (prev.toOutput (Product prevFields))
                            (this.toOutput thisField)

                    _ ->
                        Err Error
        , irType =
            case prev.irType of
                ProductType prevFieldTypes ->
                    ProductType (this.irType :: prevFieldTypes)

                _ ->
                    ProductType [ this.irType ]
        }


{-| TODO
-}
map :
    (output1 -> output2)
    -> Codec input output1
    -> Codec input output2
map f (Codec prev) =
    Codec
        { fromInput = prev.fromInput
        , toOutput = prev.toOutput >> Result.map f
        , irType = prev.irType
        }


{-| TODO
-}
contramap :
    (input2 -> input1)
    -> Codec input1 output
    -> Codec input2 output
contramap f (Codec prev) =
    Codec
        { fromInput = f >> prev.fromInput
        , toOutput = prev.toOutput
        , irType = prev.irType
        }


{-| TODO
-}
andThen :
    (output1 -> Result Error output2)
    -> Codec input output1
    -> Codec input output2
andThen f (Codec prev) =
    Codec
        { fromInput = prev.fromInput
        , toOutput = prev.toOutput >> Result.andThen f
        , irType = prev.irType
        }
