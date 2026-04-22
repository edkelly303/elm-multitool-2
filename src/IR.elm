module IR exposing (..)


type Error
    = Error


type IRCodec a b
    = IRCodec
        { toIR : a -> IR
        , fromIR : IR -> Result Error b
        , toIRType : IRType
        }


type IR
    = Bool Bool
    | String String
    | Custom Int Variant


type Variant
    = Variant0
    | Variant1 IR
    | Variant2 IR IR


type IRType
    = BoolT
    | StringT
    | CustomT (List VariantT)


type VariantT
    = Variant0T
    | Variant1T IRType
    | Variant2T IRType IRType


toIR : IRCodec a b -> a -> IR
toIR (IRCodec c) =
    c.toIR


toIRType : IRCodec a b -> IRType
toIRType (IRCodec c) =
    c.toIRType


fromIR : IRCodec a b -> IR -> Result Error b
fromIR (IRCodec c) =
    c.fromIR


bool : IRCodec Bool Bool
bool =
    IRCodec
        { toIR = Bool
        , fromIR =
            \ir ->
                case ir of
                    Bool b ->
                        Ok b

                    _ ->
                        Err Error
        , toIRType = BoolT
        }


string : IRCodec String String
string =
    IRCodec
        { toIR = String
        , fromIR =
            \ir ->
                case ir of
                    String s ->
                        Ok s

                    _ ->
                        Err Error
        , toIRType = StringT
        }


custom match =
    { match = match
    , index = 0
    , fromIR = \_ -> Err Error
    , toIRType = []
    }


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
    , toIRType = Variant0T :: prev.toIRType
    }


variant1 ctor (IRCodec argfns) prev =
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
    , toIRType = Variant1T argfns.toIRType :: prev.toIRType
    }


variant2 ctor (IRCodec arg1fns) (IRCodec arg2fns) prev =
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
    , toIRType = Variant2T arg1fns.toIRType arg2fns.toIRType :: prev.toIRType
    }


endCustom :
    { match : a -> IR
    , fromIR : IR -> Result Error b
    , toIRType : List VariantT
    , index : Int
    }
    -> IRCodec a b
endCustom prev =
    IRCodec
        { toIR = prev.match
        , fromIR = prev.fromIR
        , toIRType = CustomT prev.toIRType
        }


map :
    (b -> c)
    -> IRCodec a b
    -> IRCodec a c
map f (IRCodec prev) =
    IRCodec
        { toIR = prev.toIR
        , fromIR = prev.fromIR >> Result.map f
        , toIRType = prev.toIRType
        }


contramap :
    (b -> a)
    -> IRCodec a c
    -> IRCodec b c
contramap f (IRCodec prev) =
    IRCodec
        { toIR = f >> prev.toIR
        , fromIR = prev.fromIR
        , toIRType = prev.toIRType
        }


andThen :
    (b -> Result Error c)
    -> IRCodec a b
    -> IRCodec a c
andThen f (IRCodec prev) =
    IRCodec
        { toIR = prev.toIR
        , fromIR = prev.fromIR >> Result.andThen f
        , toIRType = prev.toIRType
        }
