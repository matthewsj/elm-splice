module List.SpliceTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import List.Splice exposing (..)


suite : Test
suite =
    describe "The List.Splice module"
        [ test "Empty list" <|
            \_ ->
                splice []
                    |> Expect.equal []
        , test "Literal items" <|
            \_ ->
                splice [I 1, I 2, I 3]
                    |> Expect.equal [1, 2, 3]
        , test "List of items" <|
            \_ ->
                splice [L [1,2,3]]
                    |> Expect.equal [1, 2, 3]
        , test "Empty list of items" <|
            \_ ->
                splice [L []]
                    |> Expect.equal []
        , test "Maybe item" <|
            \_ ->
                splice [M Nothing, M (Just 2)]
                    |> Expect.equal [2]
        , test "Mixed" <|
            \_ ->
                splice
                    [ I 1
                    , M (Just 2)
                    , L [3, 4, 5]
                    , M Nothing
                    , I 6
                    ]
                    |> Expect.equal [1, 2, 3, 4, 5, 6]
        ]


