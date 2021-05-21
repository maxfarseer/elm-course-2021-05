module Columns exposing (main)

import Columns.Field exposing (Cell(..), Field, Player(..), empty, getXY, updateXY)
import Columns.Render
import Columns.Vec4 exposing (D4(..), Vec4)
import Html exposing (Html)


putEmpty : ( D4, D4 ) -> Vec4 (Vec4 Cell) -> Vec4 (Vec4 Cell)
putEmpty ( x, y ) =
    updateXY (always Empty) x y


put p v ( x, y ) =
    updateXY (always (Occupied p v)) x y


main : Html msg
main =
    let

        startField =
            empty
                |> put Red V1 ( V1, V4 )
                |> put Blue V1 ( V3, V3 )

        afterMove =
            startField
                |> move ( V1, V4 ) ( V2, V4 )
                |> Maybe.andThen (move ( V2, V4 ) ( V3, V4 ))
                |> Maybe.andThen (move ( V3, V4 ) ( V3, V3 )) -- этот ход не выполнится, т.к синяя стоит, дальше не пойдет
                |> Maybe.andThen (move ( V3, V3 ) ( V3, V2 ))
                |> Maybe.andThen (move ( V3, V2 ) ( V3, V1 ))

    in
    Columns.Render.svg (Maybe.withDefault startField afterMove)



move : ( D4, D4 ) -> ( D4, D4 ) -> Field -> Maybe Field
move (x1, y1) (x2, y2) field =
    let
        isReachable : Bool
        isReachable = (near x1 x2 && y1 == y2) || (x1 == x2 && near y1 y2)

        near : D4 -> D4 -> Bool
        near a b =
          add a V1 == Just b || add b V1 == Just a


        cell : Cell
        cell =
            getXY x1 y1 field


        nextField =
            if not isReachable then
                Nothing
            else
                case cell of
                    Empty ->
                        Nothing

                    Occupied player d4 ->
                        updateXY (always (Occupied player d4)) x2 y2 field
                        |> updateXY (always Empty) x1 y1
                        |> Just

    in
    nextField


add : D4 -> D4 -> Maybe D4
add a b =
    case a of
        V1 ->
            case b of
                V1 ->
                    Just V2

                V2 ->
                    Just V3

                V3 ->
                    Just V4

                V4 ->
                    Nothing

        V2 ->
            case b of
                V1 ->
                    Just V3

                V2 ->
                    Just V4

                V3 ->
                    Nothing

                V4 ->
                    Nothing

        V3 ->
            case b of
                V1 ->
                    Just V4

                V2 ->
                    Nothing

                V3 ->
                    Nothing

                V4 ->
                    Nothing

        V4 ->
            Nothing
