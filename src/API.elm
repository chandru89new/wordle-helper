module API exposing (..)

import List as List
import Set
import String


filterByLetters : String -> List String -> List String
filterByLetters lettersInString dict =
    let
        hasLetters : String -> String -> Bool
        hasLetters ltrs wrd =
            String.split "" ltrs
                |> Set.fromList
                |> Set.toList
                |> List.map
                    (\l -> String.contains l wrd)
                |> List.all ((==) True)
    in
    dict
        |> List.filter (hasLetters <| String.toLower lettersInString)


filterByPosition : String -> List String -> List String
filterByPosition str wrds =
    let
        -- ['x','_','n','_','_'] ['x','a','n','a','x']
        mapsCorrectly : List Char -> List Char -> Bool
        mapsCorrectly charsFromStr charsFromWrd =
            let
                boolFn a b =
                    if a /= '_' && a /= '-' then
                        a == b

                    else
                        True
            in
            List.map2 boolFn charsFromStr charsFromWrd
                |> List.all ((==) True)
    in
    wrds
        |> List.map String.toList
        |> List.filter (mapsCorrectly <| String.toList (String.toLower str))
        |> List.map String.fromList


filterByPositionMultiple : List String -> List String -> List String
filterByPositionMultiple strList wrds =
    List.concatMap (\str -> filterByPosition str wrds) strList


exclude : String -> List String -> List String
exclude str wrds =
    wrds
        |> List.filter (\w -> List.map (\s -> String.contains s w) (String.split "" str) |> List.all ((==) False))


filterByPositionNot : String -> List String -> List String
filterByPositionNot str wrds =
    let
        mapsCorrectly : List Char -> List Char -> Bool
        mapsCorrectly charsFromStr charsFromWrd =
            let
                boolFn a b =
                    if a /= '_' && a /= '-' then
                        a /= b

                    else
                        True
            in
            List.map2 boolFn charsFromStr charsFromWrd
                |> List.all ((==) True)
    in
    wrds
        |> List.map String.toList
        |> List.filter (mapsCorrectly <| String.toList (String.toLower str))
        |> List.filter
            (\w ->
                String.toLower str
                    |> String.toList
                    |> List.filter (\s -> s /= '_' && s /= '-')
                    |> List.map (\s -> List.member s w)
                    |> List.all ((==) True)
            )
        |> List.map String.fromList


filterByPositionNotMultiple : List String -> List String -> List String
filterByPositionNotMultiple strList wrds =
    List.map (\str -> filterByPositionNot str wrds) strList
        |> List.foldl pickUniq []


pickUniq : List String -> List String -> List String
pickUniq l1 l2 =
    if List.isEmpty l2 then
        l1

    else
        List.filter (\str -> List.member str l2) l1
