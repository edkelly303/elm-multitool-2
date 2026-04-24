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
    , fromIR
    , int
    , list
    , map
    , string
    , succeed
    , toIR
    , toIRType
    , variant0
    , variant1
    , variant2
    )

import Result.Extra


type Error
    = Error


type Codec input output
    = Codec
        { toIR : input -> IR
        , fromIR : IR -> Result Error output
        , toIRType : IRType
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
    | CustomType (List VariantType)
    | ProductType (List IRType)
    | ListType IRType


type VariantType
    = Variant0Type
    | Variant1Type IRType
    | Variant2Type IRType IRType


toIR : Codec input output -> input -> IR
toIR (Codec c) =
    c.toIR


toIRType : Codec input output -> IRType
toIRType (Codec c) =
    c.toIRType


fromIR : Codec input output -> IR -> Result Error output
fromIR (Codec c) =
    c.fromIR


bool : Codec Bool Bool
bool =
    Codec
        { toIR = Bool
        , fromIR =
            \ir ->
                case ir of
                    Bool b ->
                        Ok b

                    _ ->
                        Err Error
        , toIRType = BoolType
        }


char : Codec Char Char
char =
    Codec
        { toIR = Char
        , fromIR =
            \ir ->
                case ir of
                    Char c ->
                        Ok c

                    _ ->
                        Err Error
        , toIRType = CharType
        }


string : Codec String String
string =
    Codec
        { toIR = String
        , fromIR =
            \ir ->
                case ir of
                    String s ->
                        Ok s

                    _ ->
                        Err Error
        , toIRType = StringType
        }


int : Codec Int Int
int =
    Codec
        { toIR = Int
        , fromIR =
            \ir ->
                case ir of
                    Int i ->
                        Ok i

                    _ ->
                        Err Error
        , toIRType = IntType
        }


float : Codec Float Float
float =
    Codec
        { toIR = Float
        , fromIR =
            \ir ->
                case ir of
                    Float s ->
                        Ok s

                    _ ->
                        Err Error
        , toIRType = FloatType
        }


list : Codec a a -> Codec (List a) (List a)
list (Codec item) =
    Codec
        { toIR = \items -> List (List.map item.toIR items)
        , fromIR =
            \ir ->
                case ir of
                    List items ->
                        List.map item.fromIR items
                            |> Result.Extra.combine

                    _ ->
                        Err Error
        , toIRType = ListType item.toIRType
        }


type alias CustomCodec input output =
    { match : input
    , fromIR : IR -> Result Error output
    , toIRType : List VariantType
    , index : Int
    }


custom : input -> CustomCodec input output
custom match =
    { match = match
    , index = 0
    , fromIR = \_ -> Err Error
    , toIRType = []
    }


variant0 :
    output
    -> CustomCodec (IR -> input) output
    -> CustomCodec input output
variant0 ctor prev =
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
    , toIRType = Variant0Type :: prev.toIRType
    }


variant1 :
    (arg1 -> output)
    -> Codec arg1 arg1
    -> CustomCodec ((arg1 -> IR) -> input) output
    -> CustomCodec input output
variant1 ctor (Codec argfns) prev =
    { match = prev.match <| \arg -> Custom prev.index (Variant1 (argfns.toIR arg))
    , index = prev.index + 1
    , fromIR =
        \ir ->
            case ir of
                Custom selected (Variant1 arg) ->
                    if selected == prev.index then
                        Result.map ctor (argfns.fromIR arg)

                    else
                        prev.fromIR ir

                _ ->
                    prev.fromIR ir
    , toIRType = Variant1Type argfns.toIRType :: prev.toIRType
    }


variant2 :
    (arg1 -> arg2 -> output)
    -> Codec arg1 arg1
    -> Codec arg2 arg2
    -> CustomCodec ((arg1 -> arg2 -> IR) -> input) output
    -> CustomCodec input output
variant2 ctor (Codec arg1fns) (Codec arg2fns) prev =
    { match = prev.match <| \arg1 arg2 -> Custom prev.index (Variant2 (arg1fns.toIR arg1) (arg2fns.toIR arg2))
    , index = prev.index + 1
    , fromIR =
        \ir ->
            case ir of
                Custom selected (Variant2 arg1 arg2) ->
                    if selected == prev.index then
                        Result.map2 ctor (arg1fns.fromIR arg1) (arg2fns.fromIR arg2)

                    else
                        prev.fromIR ir

                _ ->
                    prev.fromIR ir
    , toIRType = Variant2Type arg1fns.toIRType arg2fns.toIRType :: prev.toIRType
    }


endCustom : CustomCodec (input -> IR) output -> Codec input output
endCustom prev =
    Codec
        { toIR = prev.match
        , fromIR = prev.fromIR
        , toIRType = CustomType prev.toIRType
        }


succeed : output -> Codec input output
succeed ctor =
    Codec
        { toIR = \_ -> Product []
        , fromIR =
            \ir ->
                case ir of
                    Product [] ->
                        Ok ctor

                    _ ->
                        Err Error
        , toIRType = ProductType []
        }


andMap :
    (input -> field)
    -> Codec field field
    -> Codec input (field -> output)
    -> Codec input output
andMap getter (Codec this) (Codec prev) =
    Codec
        { toIR =
            \a ->
                case prev.toIR a of
                    Product prevFields ->
                        Product (this.toIR (getter a) :: prevFields)

                    _ ->
                        Product [ this.toIR (getter a) ]
        , fromIR =
            \ir ->
                case ir of
                    Product (thisField :: prevFields) ->
                        Result.map2 (\ctor val -> ctor val)
                            (prev.fromIR (Product prevFields))
                            (this.fromIR thisField)

                    _ ->
                        Err Error
        , toIRType =
            case prev.toIRType of
                ProductType prevFieldTypes ->
                    ProductType (this.toIRType :: prevFieldTypes)

                _ ->
                    ProductType [ this.toIRType ]
        }


map :
    (output1 -> output2)
    -> Codec input output1
    -> Codec input output2
map f (Codec prev) =
    Codec
        { toIR = prev.toIR
        , fromIR = prev.fromIR >> Result.map f
        , toIRType = prev.toIRType
        }


contramap :
    (input2 -> input1)
    -> Codec input1 output
    -> Codec input2 output
contramap f (Codec prev) =
    Codec
        { toIR = f >> prev.toIR
        , fromIR = prev.fromIR
        , toIRType = prev.toIRType
        }


andThen :
    (output1 -> Result Error output2)
    -> Codec input output1
    -> Codec input output2
andThen f (Codec prev) =
    Codec
        { toIR = prev.toIR
        , fromIR = prev.fromIR >> Result.andThen f
        , toIRType = prev.toIRType
        }
