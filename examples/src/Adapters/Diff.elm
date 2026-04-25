module Adapters.Diff exposing (..)

import IR exposing (IR)
import Maybe.Extra


type alias Diff =
    Maybe IR


diff : IR.Codec output output -> output -> output -> Diff
diff codec old new =
    let
        oldIR =
            IR.fromInput codec old

        newIR =
            IR.fromInput codec new

        help oldIR_ newIR_ =
            case ( oldIR_, newIR_ ) of
                ( IR.Bool b1, IR.Bool b2 ) ->
                    if b1 == b2 then
                        Just (IR.Product [])

                    else
                        Just (IR.Bool b2)

                ( IR.String b1, IR.String b2 ) ->
                    if b1 == b2 then
                        Just (IR.Product [])

                    else
                        Just (IR.String b2)

                ( IR.Char b1, IR.Char b2 ) ->
                    if b1 == b2 then
                        Just (IR.Product [])

                    else
                        Just (IR.Char b2)

                ( IR.Float b1, IR.Float b2 ) ->
                    if b1 == b2 then
                        Just (IR.Product [])

                    else
                        Just (IR.Float b2)

                ( IR.Int b1, IR.Int b2 ) ->
                    if b1 == b2 then
                        Just (IR.Product [])

                    else
                        Just (IR.Int b2)

                ( IR.Product fields1, IR.Product fields2 ) ->
                    if List.length fields1 == List.length fields2 then
                        List.map2 help fields1 fields2
                            |> Maybe.Extra.combine
                            |> Maybe.map IR.Product

                    else
                        Nothing

                ( IR.Custom oldSelected oldVariant, IR.Custom newSelected newVariant ) ->
                    if oldSelected == newSelected then
                        case ( oldVariant, newVariant ) of
                            ( IR.Variant0, IR.Variant0 ) ->
                                Just (IR.Product [])

                            ( IR.Variant1 oldArg, IR.Variant1 newArg ) ->
                                help oldArg newArg
                                    |> Maybe.map IR.Variant1
                                    |> Maybe.map (IR.Custom newSelected)

                            ( IR.Variant2 oldArg1 oldArg2, IR.Variant2 newArg1 newArg2 ) ->
                                Maybe.map2 IR.Variant2
                                    (help oldArg1 newArg1)
                                    (help oldArg2 newArg2)
                                    |> Maybe.map (IR.Custom newSelected)

                            _ ->
                                Nothing

                    else
                        Just newIR_

                _ ->
                    Nothing
    in
    help oldIR newIR


patch : IR.Codec a a -> Diff -> a -> Result IR.Error a
patch codec delta old =
    let
        help changes_ old_ =
            case ( changes_, old_ ) of
                ( IR.Product [], any ) ->
                    Just any

                ( IR.Bool b, IR.Bool _ ) ->
                    Just (IR.Bool b)

                ( IR.Char b, IR.Char _ ) ->
                    Just (IR.Char b)

                ( IR.String b, IR.String _ ) ->
                    Just (IR.String b)

                ( IR.Int b, IR.Int _ ) ->
                    Just (IR.Int b)

                ( IR.Float b, IR.Int _ ) ->
                    Just (IR.Float b)

                ( IR.Product fieldChanges, IR.Product oldFields ) ->
                    if List.length fieldChanges == List.length oldFields then
                        List.map2 help fieldChanges oldFields
                            |> Maybe.Extra.combine
                            |> Maybe.map IR.Product

                    else
                        Nothing

                ( IR.Custom diffSelected diffVariant, IR.Custom oldSelected oldVariant ) ->
                    if diffSelected == oldSelected then
                        case ( diffVariant, oldVariant ) of
                            ( IR.Variant0, IR.Variant0 ) ->
                                Just changes_

                            ( IR.Variant1 diffArg, IR.Variant1 oldArg ) ->
                                help diffArg oldArg
                                    |> Maybe.map (\arg -> IR.Custom diffSelected (IR.Variant1 arg))

                            ( IR.Variant2 diffArg1 diffArg2, IR.Variant2 oldArg1 oldArg2 ) ->
                                Maybe.map2 (\arg1 arg2 -> IR.Custom diffSelected (IR.Variant2 arg1 arg2))
                                    (help diffArg1 oldArg1)
                                    (help diffArg2 oldArg2)

                            _ ->
                                Nothing

                    else
                        Just changes_

                _ ->
                    Nothing

        maybeNewIR =
            Maybe.andThen
                (\changes ->
                    help changes (IR.fromInput codec old)
                )
                delta
    in
    case maybeNewIR of
        Just ir ->
            IR.toOutput codec ir

        Nothing ->
            Err IR.Error
