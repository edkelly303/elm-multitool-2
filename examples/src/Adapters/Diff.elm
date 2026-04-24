module Adapters.Diff exposing (..)

import IR exposing (IR)
import Maybe.Extra


type alias Diff =
    Maybe IR


diff : IR.Codec a a -> a -> a -> Diff
diff codec old new =
    let
        oldIR =
            IR.toIR codec old

        newIR =
            IR.toIR codec new

        help oldIR_ newIR_ =
            case ( oldIR_, newIR_ ) of
                ( IR.Bool b1, IR.Bool b2 ) ->
                    if b1 == b2 then
                        Just (IR.Product [])

                    else
                        Just (IR.Bool b2)

                ( IR.Product fields1, IR.Product fields2 ) ->
                    if List.length fields1 == List.length fields2 then
                        List.map2 help fields1 fields2
                            |> Maybe.Extra.combine
                            |> Maybe.map IR.Product

                    else
                        Nothing

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

                ( IR.Product fieldChanges, IR.Product oldFields ) ->
                    if List.length fieldChanges == List.length oldFields then
                        List.map2 help fieldChanges oldFields
                            |> Maybe.Extra.combine
                            |> Maybe.map IR.Product

                    else
                        Nothing

                _ ->
                    Nothing

        maybeNewIR =
            Maybe.andThen
                (\changes ->
                    help changes (IR.toIR codec old)
                )
                delta
    in
    case maybeNewIR of
        Just ir ->
            IR.fromIR codec ir

        Nothing ->
            Err IR.Error
