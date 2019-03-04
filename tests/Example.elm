module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html exposing (Html, div, h1, text)
import Random
import Test exposing (..)
import Test.Runner.Html exposing (defaultConfig, showPassedTests, viewResults, withFuzzCount)


config =
    Random.initialSeed 10000
        |> defaultConfig
        |> withFuzzCount 50
        |> showPassedTests


main : Html msg
main =
    div []
        [ h1 [] [ text "My Test Suite" ]
        , div [] [ viewResults config suite ]
        ]


suite : Test
suite =
    describe "The String module"
        [ describe "String.reverse"
            -- Nest as many descriptions as you like.
            [ test "has no effect on a palindrome" <|
                \_ ->
                    let
                        palindrome =
                            "hannah"
                    in
                    Expect.equal palindrome (String.reverse palindrome)

            -- Expect.equal is designed to be used in pipeline style, like this.
            , test "reverses a known string" <|
                \_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"

            -- fuzz runs the test 100 times with randomly-generated inputs!
            , fuzz string "restores the original string if you run it again" <|
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]
