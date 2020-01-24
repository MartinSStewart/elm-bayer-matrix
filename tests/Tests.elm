module Tests exposing (..)

import Array2D
import BayerMatrix
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Bayer matrix tests"
        [ test "Handle invalid input" <|
            \_ -> BayerMatrix.matrix -1 |> Expect.equal (Array2D.fromList [ [ 0 ] ])
        , test "Handle invalid input for normalized" <|
            \_ -> BayerMatrix.matrixNormalized -1 |> Expect.equal (Array2D.fromList [ [ 0 ] ])
        , test "1x1 matrix" <|
            \_ -> BayerMatrix.matrix 0 |> Expect.equal (Array2D.fromList [ [ 0 ] ])
        , test "2x2 matrix" <|
            let
                expected =
                    Array2D.fromList
                        [ [ 0, 2 ]
                        , [ 3, 1 ]
                        ]
            in
            \_ -> BayerMatrix.matrix 1 |> Expect.equal expected
        , test "4x4 matrix" <|
            let
                expected =
                    Array2D.fromList
                        [ [ 0, 8, 2, 10 ]
                        , [ 12, 4, 14, 6 ]
                        , [ 3, 11, 1, 9 ]
                        , [ 15, 7, 13, 5 ]
                        ]
            in
            \_ -> BayerMatrix.matrix 2 |> Expect.equal expected
        , test "2x2 normalized matrix" <|
            let
                expected =
                    Array2D.fromList
                        [ [ 0, 2 / 4 ]
                        , [ 3 / 4, 1 / 4 ]
                        ]
            in
            \_ -> BayerMatrix.matrixNormalized 1 |> Expect.equal expected
        , test "4x4 normalized matrix" <|
            let
                expected =
                    Array2D.fromList
                        [ [ 0, 8 / 16, 2 / 16, 10 / 16 ]
                        , [ 12 / 16, 4 / 16, 14 / 16, 6 / 16 ]
                        , [ 3 / 16, 11 / 16, 1 / 16, 9 / 16 ]
                        , [ 15 / 16, 7 / 16, 13 / 16, 5 / 16 ]
                        ]
            in
            \_ -> BayerMatrix.matrixNormalized 2 |> Expect.equal expected
        ]
