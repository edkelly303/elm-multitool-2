module Research.IR exposing (..)

import Json.Decode as JD
import Json.Encode as JE



-- example


type Semaphore
    = Yellow
    | Green Bool String
    | Red Bool Bool


semaphoreCodec : IRCodec Semaphore Semaphore
semaphoreCodec =
    custom
        (\red yellow green value ->
            case value of
                Red i s ->
                    red i s

                Yellow ->
                    yellow

                Green f x ->
                    green f x
        )
        |> variant2 Red bool bool
        |> variant0 Yellow
        |> variant2 Green bool string
        |> endCustom


unitCodec : IRCodec () ()
unitCodec =
    semaphoreCodec
        |> contramap (\() -> Yellow)
        |> map (always ())


encodeSemaphore : Semaphore -> JE.Value
encodeSemaphore =
    encode semaphoreCodec


semaphoreDecoder : JD.Decoder Semaphore
semaphoreDecoder =
    decoder semaphoreCodec


unitEncoder : () -> JE.Value
unitEncoder =
    encode unitCodec


unitDecoder : JD.Decoder ()
unitDecoder =
    decoder unitCodec


encode : IRCodec a b -> a -> JE.Value
encode codec value =
    codec.toIRType value
        |> encodeAdapter


decoder : IRCodec a b -> JD.Decoder b
decoder codec =
    decodeAdapter
        |> JD.andThen
            (\ir ->
                case codec.fromIRType ir of
                    Ok s ->
                        JD.succeed s

                    Err _ ->
                        JD.fail ""
            )



-- innards


type Error
    = Error


type alias IRCodec a b =
    { toIRType : a -> IR
    , fromIRType : IR -> Result Error b
    }


type IR
    = Bool Bool
    | String String
      -- | Product (List IRType)
    | Custom Int Variant


type Variant
    = Variant0
    | Variant1 IR
    | Variant2 IR IR


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


bool : IRCodec Bool Bool
bool =
    { toIRType = Bool
    , fromIRType =
        \ir ->
            case ir of
                Bool b ->
                    Ok b

                _ ->
                    Err Error
    }


string : IRCodec String String
string =
    { toIRType = String
    , fromIRType =
        \ir ->
            case ir of
                String s ->
                    Ok s

                _ ->
                    Err Error
    }


custom match =
    { match = match
    , index = 0
    , fromIRType = \_ -> Err Error
    }


variant0 ctor prev =
    { match = prev.match <| Custom prev.index Variant0
    , index = prev.index + 1
    , fromIRType =
        \ir ->
            case ir of
                Custom selected Variant0 ->
                    if selected == prev.index then
                        Ok ctor

                    else
                        prev.fromIRType ir

                _ ->
                    prev.fromIRType ir
    }


variant1 ctor argfns prev =
    { match = prev.match <| \arg -> Custom prev.index (Variant1 (argfns.toIRType arg))
    , index = prev.index + 1
    , fromIRType =
        \ir ->
            case ir of
                Custom selected (Variant1 arg) ->
                    if selected == prev.index then
                        Result.map ctor (argfns.fromIRType arg)

                    else
                        prev.fromIRType ir

                _ ->
                    prev.fromIRType ir
    }


variant2 ctor arg1fns arg2fns prev =
    { match = prev.match <| \arg1 arg2 -> Custom prev.index (Variant2 (arg1fns.toIRType arg1) (arg2fns.toIRType arg2))
    , index = prev.index + 1
    , fromIRType =
        \ir ->
            case ir of
                Custom selected (Variant2 arg1 arg2) ->
                    if selected == prev.index then
                        Result.map2 ctor (arg1fns.fromIRType arg1) (arg2fns.fromIRType arg2)

                    else
                        prev.fromIRType ir

                _ ->
                    prev.fromIRType ir
    }


endCustom prev =
    { toIRType = prev.match
    , fromIRType = prev.fromIRType
    }


map :
    (b -> c)
    -> IRCodec a b
    -> IRCodec a c
map f prev =
    { toIRType = prev.toIRType
    , fromIRType = prev.fromIRType >> Result.map f
    }


contramap :
    (b -> a)
    -> IRCodec a c
    -> IRCodec b c
contramap f prev =
    { toIRType = f >> prev.toIRType
    , fromIRType = prev.fromIRType
    }
