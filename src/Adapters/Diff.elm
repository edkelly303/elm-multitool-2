module Adapters.Diff exposing (..)

import Dict
import IR
import List.Extra
import Result.Extra


type Diff
    = Changes (List ( Int, Diff ))
    | CustomChanges Int (List ( Int, Diff ))
    | BoolChange Bool
    | IntChange Int
    | FloatChange Float
    | CharChange Char
    | StringChange String
      -- | DictChange DictChanges
      -- | SetChange SetChanges
    | ListChange (List ListChange)


type ListChange
    = Added Diff
    | Moved Int
    | Updated Int Diff
    | Existing Int Int


diff : IR.Codec output output -> output -> output -> Diff
diff codec old new =
    let
        oldIR =
            IR.fromInput codec old

        newIR =
            IR.fromInput codec new

        irType =
            IR.irType codec

        help oldIR_ newIR_ irType_ =
            case ( oldIR_, newIR_, irType_ ) of
                ( IR.Bool b1, IR.Bool b2, _ ) ->
                    if b1 == b2 then
                        Changes []

                    else
                        BoolChange b2

                ( IR.String b1, IR.String b2, _ ) ->
                    if b1 == b2 then
                        Changes []

                    else
                        StringChange b2

                ( IR.Char b1, IR.Char b2, _ ) ->
                    if b1 == b2 then
                        Changes []

                    else
                        CharChange b2

                ( IR.Float b1, IR.Float b2, _ ) ->
                    if b1 == b2 then
                        Changes []

                    else
                        FloatChange b2

                ( IR.Int b1, IR.Int b2, _ ) ->
                    if b1 == b2 then
                        Changes []

                    else
                        IntChange b2

                ( IR.Product fields1, IR.Product fields2, IR.ProductType fieldTypes ) ->
                    if List.length fields1 == List.length fields2 then
                        List.map3 help fields1 fields2 fieldTypes
                            |> List.indexedMap Tuple.pair
                            |> List.filter (\( _, arg ) -> arg /= Changes [])
                            |> Changes

                    else
                        Changes []

                ( IR.Custom oldSelected oldVariant, IR.Custom newSelected newVariant, IR.CustomType firstVariantType restVariantTypes ) ->
                    let
                        argsToList variant =
                            case variant of
                                IR.Variant0 ->
                                    []

                                IR.Variant1 a ->
                                    [ a ]

                                IR.Variant2 a1 a2 ->
                                    [ a1, a2 ]

                        argTypesToList variantType =
                            case variantType of
                                IR.Variant0Type ->
                                    []

                                IR.Variant1Type a ->
                                    [ a ]

                                IR.Variant2Type a1 a2 ->
                                    [ a1, a2 ]

                        newArgs =
                            argsToList newVariant

                        newArgTypes =
                            List.Extra.getAt newSelected (firstVariantType :: restVariantTypes)
                                |> Maybe.withDefault firstVariantType
                                |> argTypesToList
                    in
                    if oldSelected == newSelected then
                        let
                            oldArgs =
                                argsToList oldVariant

                            diffedArgs =
                                List.Extra.zip3 oldArgs newArgs newArgTypes
                                    |> List.indexedMap
                                        (\idx ( oldArg, newArg, argType ) ->
                                            ( idx, help oldArg newArg argType )
                                        )
                                    |> List.filter (\( _, arg ) -> arg /= Changes [])
                        in
                        CustomChanges newSelected diffedArgs

                    else
                        let
                            diffedArgs =
                                List.Extra.zip newArgs newArgTypes
                                    |> List.indexedMap (\idx ( newArg, argType ) -> ( idx, help (default argType) newArg argType ))
                                    |> List.filter (\( _, arg ) -> arg /= Changes [])
                        in
                        CustomChanges newSelected diffedArgs

                _ ->
                    Changes []
    in
    help oldIR newIR irType


default : IR.IRType -> IR.IR
default irType =
    case irType of
        IR.BoolType ->
            IR.Bool True

        IR.CharType ->
            IR.Char ' '

        IR.StringType ->
            IR.String ""

        IR.IntType ->
            IR.Int 0

        IR.FloatType ->
            IR.Float 0.0

        IR.CustomType firstVariantType _ ->
            IR.Custom 0
                (case firstVariantType of
                    IR.Variant0Type ->
                        IR.Variant0

                    IR.Variant1Type arg ->
                        IR.Variant1 (default arg)

                    IR.Variant2Type arg1 arg2 ->
                        IR.Variant2 (default arg1) (default arg2)
                )

        IR.ProductType fieldTypes ->
            IR.Product (List.map default fieldTypes)

        IR.ListType _ ->
            IR.List []


patch : IR.Codec a a -> Diff -> a -> Result String a
patch codec delta old =
    let
        help changes_ old_ irType_ =
            case ( changes_, old_, irType_ ) of
                ( Changes [], any, _ ) ->
                    Ok any

                ( BoolChange b, IR.Bool _, _ ) ->
                    Ok (IR.Bool b)

                ( CharChange b, IR.Char _, _ ) ->
                    Ok (IR.Char b)

                ( StringChange b, IR.String _, _ ) ->
                    Ok (IR.String b)

                ( IntChange b, IR.Int _, _ ) ->
                    Ok (IR.Int b)

                ( FloatChange b, IR.Float _, _ ) ->
                    Ok (IR.Float b)

                ( Changes fieldChanges, IR.Product oldFields, IR.ProductType fieldTypes ) ->
                    let
                        fieldChangesDict =
                            Dict.fromList fieldChanges
                    in
                    List.Extra.zip oldFields fieldTypes
                        |> List.indexedMap
                            (\idx ( oldField, fieldType ) ->
                                case Dict.get idx fieldChangesDict of
                                    Nothing ->
                                        Ok oldField

                                    Just change ->
                                        help change oldField fieldType
                            )
                        |> Result.Extra.combine
                        |> Result.map IR.Product

                ( CustomChanges diffSelected diffVariant, IR.Custom oldSelected oldVariant, IR.CustomType firstVariantType restVariantTypes ) ->
                    let
                        argsDict =
                            Dict.fromList diffVariant
                    in
                    List.Extra.getAt diffSelected (firstVariantType :: restVariantTypes)
                        |> Result.fromMaybe ""
                        |> Result.andThen
                            (\variantType ->
                                case variantType of
                                    IR.Variant0Type ->
                                        Ok IR.Variant0

                                    IR.Variant1Type argType ->
                                        if diffSelected == oldSelected then
                                            case oldVariant of
                                                IR.Variant1 arg ->
                                                    case Dict.get 0 argsDict of
                                                        Nothing ->
                                                            Ok (IR.Variant1 arg)

                                                        Just changes ->
                                                            Result.map IR.Variant1 (help changes arg argType)

                                                _ ->
                                                    Err ""

                                        else
                                            case Dict.get 0 argsDict of
                                                Nothing ->
                                                    Ok (IR.Variant1 (default argType))

                                                Just changes ->
                                                    Result.map IR.Variant1 (help changes (default argType) argType)

                                    IR.Variant2Type arg1Type arg2Type ->
                                        if diffSelected == oldSelected then
                                            case oldVariant of
                                                IR.Variant2 arg1 arg2 ->
                                                    let
                                                        arg1Diff =
                                                            case Dict.get 0 argsDict of
                                                                Nothing ->
                                                                    Ok arg1

                                                                Just changes ->
                                                                    help changes arg1 arg1Type

                                                        arg2Diff =
                                                            case Dict.get 1 argsDict of
                                                                Nothing ->
                                                                    Ok arg2

                                                                Just changes ->
                                                                    help changes arg2 arg2Type
                                                    in
                                                    Result.map2 IR.Variant2 arg1Diff arg2Diff

                                                _ ->
                                                    Err ""

                                        else
                                            let
                                                arg1Diff =
                                                    case Dict.get 0 argsDict of
                                                        Nothing ->
                                                            Ok (default arg1Type)

                                                        Just changes ->
                                                            help changes (default arg1Type) arg1Type

                                                arg2Diff =
                                                    case Dict.get 1 argsDict of
                                                        Nothing ->
                                                            Ok (default arg2Type)

                                                        Just changes ->
                                                            help changes (default arg2Type) arg2Type
                                            in
                                            Result.map2 IR.Variant2 arg1Diff arg2Diff
                            )
                        |> Result.map (IR.Custom diffSelected)

                ( diff_, old__, type_ ) ->
                    Err ("mismatch between \n" ++ Debug.toString diff_ ++ "\n" ++ Debug.toString old__ ++ "\n" ++ Debug.toString type_)

        oldIR =
            IR.fromInput codec old

        irType =
            IR.irType codec

        maybeNewIR =
            help delta oldIR irType
    in
    case maybeNewIR of
        Ok ir ->
            IR.toOutput codec ir |> Result.mapError (\_ -> "IR.toOutput failed")

        Err e ->
            Err e
