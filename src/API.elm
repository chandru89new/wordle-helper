module API exposing (..)

import List as List
import Set
import String
import Tests exposing (test)
import Words


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



{-
   - words
   - take a word from words (w)
   - strsList
   - take a str from strList (s)
   - split both w and s (wchars, schars)
   - map2 (\wchar schar -> if ['-','_'].includes(schar) then True else if (schar is lower) then (schar /= wchar && w includes schar) else (schar == wchar))
   - list.all (== true)
   -
-}


filterByPosition_ : List String -> List String -> List String
filterByPosition_ strList wrdList =
    wrdList
        |> List.filter
            (\w ->
                strList
                    |> List.map
                        (\str ->
                            let
                                wchars =
                                    String.toList w

                                schars =
                                    String.toList str
                            in
                            List.map2
                                (\s w_ ->
                                    if s == '-' || s == '_' then
                                        True

                                    else if Char.isLower s then
                                        s /= w_ && List.member s wchars

                                    else
                                        s == Char.toUpper w_
                                )
                                schars
                                wchars
                                |> List.all ((==) True)
                        )
                    |> List.all ((==) True)
            )



{-
   compareGreenPosition "--a-e" "flame" == True

   compareGreenPosition "--a-e" "truce" == False
-}


runTests =
    Tests.runTests
        [ test "compareGreenPosition 1" <| compareGreenPosition "--a-e" "flame" == True
        , test "compareGreenPosition 2" <|
            compareGreenPosition "--a-e" "truce"
                == False
        , test "compareYellowPosition 1" <|
            compareYellowPosition "aflem" "flame"
                == True
        , test "compareYellowPosition 2" <|
            compareYellowPosition "flair" "flame"
                == False
        , test "compareYellowPosition 3" <| compareYellowPosition "----a" "spray" == True
        , test "compareYellowPosition 4" <| compareYellowPosition "a--" "ass" == False
        , test "compareYellowPosition 5" <| compareYellowPosition "-a-" "ass" == True
        , test "compareYellowPosition 6" <| compareYellowPosition "--a" "ass" == True
        , test "filterByPosSingle 1" <| filterByPosSingle "Fa_M_" [ "fasmf", "flame", "frome" ] == [ "flame" ]
        , test "filterByPos 1" <| filterByPos [ "S-RAp", "y-RAs" ] Words.words == [ "spray" ]
        , test "filterByPos 2" <| filterByPos [ "RA-sn", "i--NS" ] Words.words == [ "rains" ]
        ]


compareGreenPosition : String -> String -> Bool
compareGreenPosition str wrd =
    let
        schars =
            String.toList str

        wchars =
            String.toList wrd

        recur l1 l2 =
            case ( l1, l2 ) of
                ( schar :: scharTail, wchar :: wcharTail ) ->
                    if schar == wchar || (schar == '-' || schar == '_') then
                        recur scharTail wcharTail

                    else
                        False

                ( [], [] ) ->
                    True

                _ ->
                    False
    in
    recur schars wchars



{-
   compareYellowPosition "aflem" "flame" == True

   compareYellowPosition "flair" "flame" == False
-}


compareYellowPosition : String -> String -> Bool
compareYellowPosition str wrd =
    let
        schars =
            String.toList str

        wchars =
            String.toList wrd

        recur l1 l2 =
            case ( l1, l2 ) of
                ( schar :: scharsTail, wchar :: wcharsTail ) ->
                    if schar == '-' || schar == '_' || (schar /= wchar && List.member schar wchars) then
                        recur scharsTail wcharsTail

                    else
                        False

                ( [], [] ) ->
                    True

                _ ->
                    False
    in
    recur schars wchars



{-
   filterByPosSingle "Fa_M_" ["fasmf", "flame", "frome"] == ["flame"]
-}


filterByPosSingle : String -> List String -> List String
filterByPosSingle str wrds =
    let
        yellow =
            str
                |> String.toList
                |> List.map
                    (\s ->
                        if Char.isLower s then
                            s

                        else
                            '-'
                    )
                |> String.fromList

        green =
            str
                |> String.toList
                |> List.map
                    (\s ->
                        if Char.isUpper s then
                            Char.toLower s

                        else
                            '-'
                    )
                |> String.fromList
    in
    wrds
        |> List.filter (\w -> compareGreenPosition green w && compareYellowPosition yellow w)


filterByPos : List String -> List String -> List String
filterByPos strs wrds =
    List.map (\str -> filterByPosSingle str wrds) strs |> List.foldl pickUniq []
