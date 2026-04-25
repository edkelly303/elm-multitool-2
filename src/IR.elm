module IR exposing
    ( Codec
    , CustomCodec
    , Error(..)
    , IR(..)
    , IRType(..)
    , Variant(..)
    , VariantType(..)
    , andMap
    , andThen
    , bool
    , char
    , contramap
    , custom
    , endCustom
    , float
    , fromInput
    , int
    , irType
    , list
    , map
    , string
    , succeed
    , toOutput
    , variant0
    , variant1
    , variant2
    )

import Result.Extra


type Error
    = Error


type Codec input output
    = Codec
        { fromInput : input -> IR
        , toOutput : IR -> Result Error output
        , irType : IRType
        }


type IR
    = Bool Bool
    | Char Char
    | String String
    | Int Int
    | Float Float
    | Custom Int Variant
    | Product (List IR)
    | List (List IR)


type Variant
    = Variant0
    | Variant1 IR
    | Variant2 IR IR


type IRType
    = BoolType
    | CharType
    | StringType
    | IntType
    | FloatType
    | CustomType VariantType (List VariantType)
    | ProductType (List IRType)
    | ListType IRType


type VariantType
    = Variant0Type
    | Variant1Type IRType
    | Variant2Type IRType IRType


fromInput : Codec input output -> input -> IR
fromInput (Codec c) =
    c.fromInput


irType : Codec input output -> IRType
irType (Codec c) =
    c.irType


toOutput : Codec input output -> IR -> Result Error output
toOutput (Codec c) =
    c.toOutput


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


type CustomCodec input hasAtLeastOneVariant output
    = CustomCodec
        { match : input
        , fromIR : IR -> Result Error output
        , variantTypes : List VariantType
        , index : Int
        }


custom : input -> CustomCodec input Never output
custom match =
    CustomCodec
        { match = match
        , index = 0
        , fromIR = \_ -> Err Error
        , variantTypes = []
        }


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


endCustom : CustomCodec (input -> IR) () output -> Codec input output
endCustom (CustomCodec prev) =
    Codec
        { fromInput = prev.match
        , toOutput = prev.fromIR
        , irType =
            case prev.variantTypes of
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
