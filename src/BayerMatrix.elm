module BayerMatrix exposing (matrix, matrixNormalized)

import Array2D exposing (Array2D)


{-| Get a Bayer matrix with a width and height of 2^size.

    import Array2D

    matrix 1
    --> Array2D.fromList
    --> [ [ 0, 2 ]
    --> , [ 3, 1 ]
    --> ]

    matrix 2
    --> Array2D.fromList
    --> [ [ 0, 8, 2, 10 ]
    --> , [ 12, 4, 14, 6 ]
    --> , [ 3, 11, 1, 9 ]
    --> , [ 15, 7, 13, 5 ]
    --> ]

-}
matrix : Int -> Array2D Int
matrix size =
    -- Original implementation found here: https://en.wikipedia.org/wiki/Ordered_dithering
    if size <= 0 then
        Array2D.initialize 1 1 (\_ _ -> 0)

    else
        let
            fullSize =
                2 ^ size

            halfSize =
                fullSize // 2

            subMatrix =
                matrix (size - 1)

            get x y =
                Array2D.get (modBy halfSize x) (modBy halfSize y) subMatrix |> Maybe.withDefault 0 |> (*) 4
        in
        Array2D.initialize
            fullSize
            fullSize
            (\x y ->
                if x < halfSize && y < halfSize then
                    get x y

                else if x >= halfSize && y < halfSize then
                    get x y + 3

                else if x < halfSize && y >= halfSize then
                    get x y + 2

                else
                    get x y + 1
            )


{-| Same as matrix but the values are normalized between 0 and 1.

    import Array2D

    matrixNormalized 1
    --> Array2D.fromList
    --> [ [ 0, 2 / 4 ]
    --> , [ 3 / 4, 1 / 4 ]
    --> ]

    matrixNormalized 2
    --> Array2D.fromList
    --> [ [ 0, 8 / 16, 2 / 16, 10 / 16 ]
    --> , [ 12 / 16, 4 / 16, 14 / 16, 6 / 16 ]
    --> , [ 3 / 16, 11 / 16, 1 / 16, 9 / 16 ]
    --> , [ 15 / 16, 7 / 16, 13 / 16, 5 / 16 ]
    --> ]

-}
matrixNormalized : Int -> Array2D Float
matrixNormalized size =
    let
        factor =
            max 1 (2 ^ (size * 2)) |> toFloat |> (/) 1
    in
    matrix size |> Array2D.map (toFloat >> (*) factor)
