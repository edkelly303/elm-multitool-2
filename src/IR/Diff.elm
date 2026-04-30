module IR.Diff exposing (..)

import Dict
import Diff as ListDiffer
import IR
import List.Extra
import Maybe.Extra
import Result.Extra


type Diff
    = Identical
    | ProductChanges ( Int, Diff ) (List ( Int, Diff ))
    | CustomChanges Int (List ( Int, Diff ))
    | BoolChange Bool
    | IntChange Int
    | FloatChange Float
    | CharChange Char
    | StringChange String
      -- | DictChange DictChanges
      -- | SetChange SetChanges
    | ListChanges (List ListChange)


type ListChange
    = Added Diff
    | Moved Int
    | Updated Int Diff
    | RangeForward Int Int
    | RangeBackward Int Int
    | Repeat Int ListChange


diff : IR.Codec output output -> output -> output -> Diff
diff codec old new =
    let
        (IR.IR oldIR) =
            IR.fromInput codec old

        (IR.IR newIR) =
            IR.fromInput codec new

        irType =
            IR.irType codec
    in
    diffHelp oldIR newIR irType


diffHelp : IR.IRValue -> IR.IRValue -> IR.IRType -> Diff
diffHelp oldIR_ newIR_ irType_ =
    if oldIR_ == newIR_ then
        Identical

    else
        case ( oldIR_, newIR_, irType_ ) of
            ( IR.Bool _, IR.Bool b2, _ ) ->
                BoolChange b2

            ( IR.String _, IR.String b2, _ ) ->
                StringChange b2

            ( IR.Char _, IR.Char b2, _ ) ->
                CharChange b2

            ( IR.Float _, IR.Float b2, _ ) ->
                FloatChange b2

            ( IR.Int _, IR.Int b2, _ ) ->
                IntChange b2

            ( IR.List oldList, IR.List newList, IR.ListType itemType ) ->
                ListDiffer.diffWith (areSimilar itemType) oldList newList
                    |> List.foldl
                        (\change { idx, out } ->
                            case change of
                                ListDiffer.Added newItem ->
                                    { idx = idx
                                    , out =
                                        case List.Extra.elemIndex newItem oldList of
                                            Just oldIdx ->
                                                Just (Moved oldIdx) :: out

                                            Nothing ->
                                                Just (Added (diffHelp (default itemType) newItem itemType)) :: out
                                    }

                                ListDiffer.Removed _ ->
                                    { idx = idx + 1
                                    , out = Nothing :: out
                                    }

                                ListDiffer.Similar _ _ changes_ ->
                                    { idx = idx + 1
                                    , out = Just (Updated idx changes_) :: out
                                    }

                                ListDiffer.NoChange _ ->
                                    { idx = idx + 1
                                    , out = Just (Moved idx) :: out
                                    }
                        )
                        { idx = 0, out = [] }
                    |> .out
                    |> List.filterMap identity
                    |> coalesceForwardMoveSequences
                    |> coalesceBackwardMoveSequences
                    |> doRunLengthEncoding
                    |> ListChanges

            ( IR.Product fields1, IR.Product fields2, IR.ProductType fieldTypes ) ->
                let
                    changes =
                        List.map3 diffHelp fields1 fields2 fieldTypes
                            |> List.indexedMap Tuple.pair
                            |> List.filter (\( _, arg ) -> arg /= Identical)
                in
                case changes of
                    change :: restChanges ->
                        ProductChanges change restChanges

                    [] ->
                        Identical

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

                    diffedArgs =
                        if oldSelected == newSelected then
                            let
                                oldArgs =
                                    argsToList oldVariant
                            in
                            List.Extra.zip3 oldArgs newArgs newArgTypes
                                |> List.indexedMap
                                    (\idx ( oldArg, newArg, argType ) ->
                                        ( idx, diffHelp oldArg newArg argType )
                                    )

                        else
                            List.Extra.zip newArgs newArgTypes
                                |> List.indexedMap
                                    (\idx ( newArg, argType ) ->
                                        ( idx, diffHelp (default argType) newArg argType )
                                    )
                in
                diffedArgs
                    |> List.filter (\( _, arg ) -> arg /= Identical)
                    |> CustomChanges newSelected

            _ ->
                Identical


coalesceForwardMoveSequences : List ListChange -> List ListChange
coalesceForwardMoveSequences list =
    List.foldr
        (\item prev ->
            case ( prev, item ) of
                ( [], _ ) ->
                    [ item ]

                ( (Moved prevMove) :: restPrevItems, Moved move ) ->
                    if move == prevMove + 1 then
                        RangeForward prevMove move :: restPrevItems

                    else
                        item :: prev

                ( (RangeForward start end) :: restPrevItems, Moved move ) ->
                    if move == end + 1 then
                        RangeForward start move :: restPrevItems

                    else
                        item :: prev

                _ ->
                    item :: prev
        )
        []
        list


coalesceBackwardMoveSequences : List ListChange -> List ListChange
coalesceBackwardMoveSequences list =
    List.foldl
        (\item prev ->
            case ( prev, item ) of
                ( [], _ ) ->
                    [ item ]

                ( (Moved prevMove) :: restPrevItems, Moved move ) ->
                    if move == prevMove + 1 then
                        RangeBackward move prevMove :: restPrevItems

                    else
                        item :: prev

                ( (RangeBackward end start) :: restPrevItems, Moved move ) ->
                    if move == end + 1 then
                        RangeBackward move start :: restPrevItems

                    else
                        item :: prev

                _ ->
                    item :: prev
        )
        []
        list
        |> List.reverse


doRunLengthEncoding : List ListChange -> List ListChange
doRunLengthEncoding list =
    List.foldr
        (\item prev ->
            case prev of
                [] ->
                    [ item ]

                prevItem :: restPrevItems ->
                    case prevItem of
                        Repeat length runItem ->
                            if item == runItem then
                                Repeat (length + 1) runItem :: restPrevItems

                            else
                                item :: prev

                        _ ->
                            if item == prevItem then
                                Repeat 2 item :: restPrevItems

                            else
                                item :: prev
        )
        []
        list


areSimilar : IR.IRType -> IR.IRValue -> IR.IRValue -> Maybe Diff
areSimilar irType old new =
    let
        oldNewDiff =
            diffHelp old new irType

        defaultNewDiff =
            diffHelp (default irType) new irType
    in
    if size oldNewDiff < size defaultNewDiff then
        Just oldNewDiff

    else
        Nothing


size : Diff -> Int
size changes =
    case changes of
        Identical ->
            0

        ProductChanges c cs ->
            List.map (\( _, x ) -> size x) (c :: cs)
                |> List.sum

        CustomChanges _ cs ->
            List.map (\( _, x ) -> size x) cs
                |> List.sum

        -- DictChange { insertions, deletions } ->
        --     Set.size deletions
        --         + (Dict.values insertions
        --             |> List.map size
        --             |> List.sum
        --           )
        -- SetChange { insertions, deletions } ->
        --     List.length deletions
        --         + (List.map size insertions
        --             |> List.sum
        --           )
        ListChanges cs ->
            List.map
                (\change ->
                    case change of
                        Added addedC ->
                            size addedC

                        Moved _ ->
                            1

                        Updated _ updatedC ->
                            size updatedC

                        RangeForward _ _ ->
                            1

                        RangeBackward _ _ ->
                            1

                        Repeat _ _ ->
                            1
                )
                cs
                |> List.sum

        BoolChange _ ->
            1

        IntChange _ ->
            1

        FloatChange _ ->
            1

        CharChange _ ->
            1

        StringChange _ ->
            1


default : IR.IRType -> IR.IRValue
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

        IR.ListType _ ->
            IR.List []

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


patch : IR.Codec a a -> Diff -> a -> Result String a
patch codec delta old =
    let
        (IR.IR oldIR) =
            IR.fromInput codec old

        irType =
            IR.irType codec
    in
    case patchHelp delta oldIR irType of
        Ok ir ->
            IR.toOutput codec (IR.IR ir)
                |> Result.mapError (\_ -> "IR.toOutput failed")

        Err e ->
            Err e


patchHelp : Diff -> IR.IRValue -> IR.IRType -> Result String IR.IRValue
patchHelp changes_ old_ irType_ =
    case ( changes_, old_, irType_ ) of
        ( Identical, any, _ ) ->
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

        ( ListChanges cs, IR.List oldList, IR.ListType itemType ) ->
            Ok
                (List.foldl
                    (\change out ->
                        listPatchHelp change oldList itemType :: out
                    )
                    []
                    cs
                    |> List.filterMap identity
                    |> List.concat
                    |> IR.List
                )

        ( ProductChanges fieldChange restFieldChanges, IR.Product oldFields, IR.ProductType fieldTypes ) ->
            let
                fieldChangesDict =
                    Dict.fromList (fieldChange :: restFieldChanges)
            in
            List.Extra.zip oldFields fieldTypes
                |> List.indexedMap
                    (\idx ( oldField, fieldType ) ->
                        case Dict.get idx fieldChangesDict of
                            Nothing ->
                                Ok oldField

                            Just change ->
                                patchHelp change oldField fieldType
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
                                                    Result.map IR.Variant1 (patchHelp changes arg argType)

                                        _ ->
                                            Err ""

                                else
                                    case Dict.get 0 argsDict of
                                        Nothing ->
                                            Ok (IR.Variant1 (default argType))

                                        Just changes ->
                                            Result.map IR.Variant1 (patchHelp changes (default argType) argType)

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
                                                            patchHelp changes arg1 arg1Type

                                                arg2Diff =
                                                    case Dict.get 1 argsDict of
                                                        Nothing ->
                                                            Ok arg2

                                                        Just changes ->
                                                            patchHelp changes arg2 arg2Type
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
                                                    patchHelp changes (default arg1Type) arg1Type

                                        arg2Diff =
                                            case Dict.get 1 argsDict of
                                                Nothing ->
                                                    Ok (default arg2Type)

                                                Just changes ->
                                                    patchHelp changes (default arg2Type) arg2Type
                                    in
                                    Result.map2 IR.Variant2 arg1Diff arg2Diff
                    )
                |> Result.map (IR.Custom diffSelected)

        ( diff_, old__, type_ ) ->
            Err ("mismatch between \n" ++ Debug.toString diff_ ++ "\n" ++ Debug.toString old__ ++ "\n" ++ Debug.toString type_)


listPatchHelp : ListChange -> List IR.IRValue -> IR.IRType -> Maybe (List IR.IRValue)
listPatchHelp change oldList itemType =
    case change of
        Added itemDiff ->
            patchHelp itemDiff (default itemType) itemType
                |> Result.toMaybe
                |> Maybe.map List.singleton

        Moved idx ->
            List.Extra.getAt idx oldList
                |> Maybe.map List.singleton

        Updated idx itemDiff ->
            let
                oldItem =
                    List.Extra.getAt idx oldList
                        |> Maybe.withDefault (default itemType)
            in
            patchHelp itemDiff oldItem itemType
                |> Result.toMaybe
                |> Maybe.map List.singleton

        RangeForward start end ->
            oldList
                |> List.drop start
                |> List.take (1 + end - start)
                |> Just

        RangeBackward start end ->
            oldList
                |> List.drop end
                |> List.take (1 + start - end)
                |> List.reverse
                |> Just

        Repeat length change_ ->
            listPatchHelp change_ oldList itemType
                |> List.repeat length
                |> Maybe.Extra.combine
                |> Maybe.map List.concat
