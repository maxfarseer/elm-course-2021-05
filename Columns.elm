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
        example =
            empty
                |> put Red V1 ( V1, V4 )
                |> put Blue V1 ( V1, V3 )
                |> move ( V1, V3 ) ( V2, V3 )
                |> move ( V1, V4 ) ( V2, V4 )
    in
    Columns.Render.svg example



-- move : ( D4, D4 ) -> ( D4, D4 ) -> Field -> Maybe Field пока что без maybe


move : ( D4, D4 ) -> ( D4, D4 ) -> Field -> Field
move from to field =
    let
        ( fromX, fromY ) =
            from

        ( toX, toY ) =
            to

        cell : Cell
        cell =
            getXY fromX fromY field

        putFishkaOrEmpty =
            case cell of
                Empty ->
                    -- сходило empty, в будущем не валидно или не важно
                    updateXY (always Empty) fromX fromY

                Occupied player d4 ->
                    updateXY (always (Occupied player d4)) toX toY
    in
    if validateXY from to field then
        let
            _ =
                Debug.log "Ход сделан: " ( from, to )
        in
        field
            |> putEmpty from
            |> putFishkaOrEmpty

    else
        let
            _ =
                Debug.log "Ход НЕ сделан: " ( from, to )
        in
        field


validateXY : ( D4, D4 ) -> ( D4, D4 ) -> Field -> Bool
validateXY from to field =
    let
        ( fromX, fromY ) =
            from

        ( toX, toY ) =
            to
    in
    (validate fromX toX && validate fromY toY)
        && not (isSameCell from to)
        && not (isOccupiedByEnemy from to field)
        && not (isDiagonalMove from to)


isSameCell : ( D4, D4 ) -> ( D4, D4 ) -> Bool
isSameCell from to =
    from == to


isDiagonalMove : (D4, D4) -> (D4, D4) -> Bool
isDiagonalMove (fromX, fromY) (toX, toY) =
    fromX /= toX && fromY /= toY

isOccupiedByEnemy : ( D4, D4 ) -> ( D4, D4 ) -> Field -> Bool
isOccupiedByEnemy ( fromX, fromY ) ( toX, toY ) field =
    let
        fromColorOrFromEmpty =
            getXY fromX fromY field

        toColorOrToEmpty =
            getXY toX toY field
    in
    case (fromColorOrFromEmpty, toColorOrToEmpty) of
        ( Empty, Empty ) ->
            False
        ( Empty, Occupied _ _) ->
            False
        ( Occupied _ _, Empty ) ->
            False
        (Occupied playerFrom _, Occupied playerTo _) ->
            if (playerFrom == playerTo) then
                False
            else
                True



{-| можно ли и нужно ли записать короче?

  - проверяем что мы сделали шаг на 0 или 1 клетку

-}
validate : D4 -> D4 -> Bool
validate from to =
    case from of
        V1 ->
            case to of
                V1 ->
                    True

                V2 ->
                    True

                V3 ->
                    False

                V4 ->
                    False

        V2 ->
            case to of
                V1 ->
                    True

                V2 ->
                    True

                V3 ->
                    True

                V4 ->
                    False

        V3 ->
            case to of
                V1 ->
                    False

                V2 ->
                    True

                V3 ->
                    True

                V4 ->
                    True

        V4 ->
            case to of
                V1 ->
                    False

                V2 ->
                    False

                V3 ->
                    True

                V4 ->
                    True
