module Easings exposing (easeInBack, easeInExpo, easeInOutBack, easeInOutCirc, easeInOutExpo, easeInQuad, easeInQuart, easeOutBack, easeOutCirc, easeOutCubic, pow)

{-| adapted from <https://easings.net/#easeOutBack>
x is the percentage elapsed, from 0.0 to 1.0
-}


easeOutBack : Float -> Float
easeOutBack x =
    let
        c1 =
            1.70158

        c3 =
            c1 + 1
    in
    1 + c3 * ((x - 1) ^ 3) + c1 * ((x - 1) ^ 2)


easeInOutExpo : Float -> Float
easeInOutExpo x =
    if x == 0 then
        0

    else if x == 1 then
        1

    else if x < 0.5 then
        (2 ^ (20 * x - 10)) / 2

    else
        (2 - (2 ^ (-20 * x + 10))) / 2


easeInQuad : Float -> Float
easeInQuad x =
    x * x


easeInQuart : Float -> Float
easeInQuart x =
    x * x * x * x


easeInExpo : Float -> Float
easeInExpo x =
    if x == 0 then
        0

    else
        2 ^ (10 * x - 10)


easeOutCirc : Float -> Float
easeOutCirc x =
    -- sqrt(1 - pow(x - 1, 2));
    sqrt <| (1 - ((x - 1) ^ 2))


easeInOutCirc : Float -> Float
easeInOutCirc x =
    if x < 0.5 then
        let
            pow2 =
                (2 * x) ^ 2

            s =
                sqrt (1 - pow2)
        in
        (1 - s) / 2

    else
        let
            pow2 =
                (-2 * x + 2) ^ 2

            s =
                sqrt (1 - pow2)
        in
        (s + 1) / 2


{-| adapted from <https://easings.net/#easeInBack>
x is the percentage elapsed, from 0.0 to 1.0
-}
easeInBack : Float -> Float
easeInBack x =
    let
        c1 =
            1.70158

        c3 =
            c1 + 1
    in
    c3 * x * x * x - c1 * x * x


easeOutCubic : Float -> Float
easeOutCubic x =
    1 - (1 - x ^ 3)


{-| adapted from <https://easings.net/#easeInOutBack>
x is the percentage elapsed, from 0.0 to 1.0
-}
easeInOutBack : Float -> Float
easeInOutBack x =
    let
        c1 =
            1.70158

        c2 =
            c1 * 1.525

        c2p1 =
            c2 + 1
    in
    if x < 0.5 then
        -- (((2 * x) ^ 2) * (c2p1 * 2 * x - c2)) / 2
        (pow (2 * x) 2 * ((c2 + 1) * 2 * x - c2)) / 2

    else
        (pow (2 * x - 2) 2 * ((c2 + 1) * (x * 2 - 2) + c2) + 2) / 2


pow : Float -> Float -> Float
pow base exp =
    base ^ exp
