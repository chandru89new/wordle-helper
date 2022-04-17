module Tests exposing (..)


test : String -> Bool -> Result String ()
test testName testResult =
    if testResult then
        Result.Ok ()

    else
        Result.Err <| testName ++ ": Failed"


runTests : List (Result String ()) -> Result String ()
runTests tests =
    case tests of
        h :: tail ->
            Result.andThen (\_ -> runTests tail) h

        [] ->
            Ok ()
