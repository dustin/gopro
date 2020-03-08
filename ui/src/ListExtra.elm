module ListExtra exposing (..)

groupWhile : (a -> a -> Bool) -> List a -> List (List a)
groupWhile eq xs_ =
    case xs_ of
        [] ->
            []

        x :: xs ->
            let
                ( ys, zs ) =
                    span (eq x) xs
            in
                (x :: ys) :: groupWhile eq zs

span : (a -> Bool) -> List a -> ( List a, List a )
span p xs =
    ( takeWhile p xs, dropWhile p xs )

takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate =
    let
        takeWhileMemo memo list =
            case list of
                [] ->
                    List.reverse memo

                x :: xs ->
                    if (predicate x) then
                        takeWhileMemo (x :: memo) xs
                    else
                        List.reverse memo
    in
        takeWhileMemo []

dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if (predicate x) then
                dropWhile predicate xs
            else
                list
