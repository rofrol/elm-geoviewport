module Main exposing (main)

import Html


bbox =
    { w = 21.0187
    , s = 52.06511
    , e = 21.03063
    , n = 52.07282
    }



-- The size of the desired map.


dims =
    { x = 400, y = 400 }


main =
    Html.div []
        [ Html.div [] [ Html.text (String.fromFloat (.zoom (viewport bbox dims))) ]
        , Html.div [] [ Html.text (String.fromFloat (viewport bbox dims).center.lon ++ ", " ++ String.fromFloat (viewport bbox dims).center.lat) ]
        ]


epsln =
    1.0e-10


d2r =
    pi / 180


r2d =
    180 / pi



-- 900913 properties.


a =
    6378137.0


maxextent =
    20037508.342789244


size =
    512



-- fixed base at maxzoom 20


s =
    size * 2 ^ 20


cache =
    { bc = toFloat s / 360
    , cc = toFloat s / (2 * pi)
    , zc = toFloat s / 2
    , ac = s
    }


px lon lat =
    let
        d =
            cache.zc

        f =
            min (max (sin (d2r * lat)) -0.9999) 0.9999

        x =
            round (d + lon * cache.bc)

        y =
            round (d + 0.5 * logBase e ((1 + f) / (1 - f)) * -cache.cc)

        x_ =
            if x > cache.ac then
                cache.ac

            else
                x

        y_ =
            if y > cache.ac then
                cache.ac

            else
                y
    in
    ( x_, y_ )


getAdjusted base ( r0, r1 ) =
    min (base - logBase e r0 / logBase e 2)
        (base - logBase e r1 / logBase e 2)


viewport bounds dimensions =
    let
        minzoom =
            0

        maxzoom =
            20

        base =
            maxzoom

        ( bl0, bl1 ) =
            px bounds.w bounds.s

        ( tr0, tr1 ) =
            px bounds.e bounds.n

        width =
            tr0 - bl0

        height =
            bl1 - tr1

        ratios =
            ( toFloat width / dimensions.x, toFloat height / dimensions.y )

        center =
            { lon = (bounds.w + bounds.e) / 2, lat = (bounds.s + bounds.n) / 2 }

        adjusted =
            getAdjusted base ratios

        zoom =
            max minzoom (min maxzoom adjusted)
    in
    { center = center, zoom = zoom }
