module Digits exposing (encodeBaseSixtyTwo)

import RadixInt exposing (Base(..), fromInt, toList)


type alias NumberSystem =
    { nullity : Char
    , unity : Char
    , digits : List Char
    }


type alias DigitModel =
    { nullity : Result String Char
    , unity : Result String Char
    , digits : List Char
    , value : Maybe Int
    }


baseSixtyTwo : NumberSystem
baseSixtyTwo =
    { nullity = '0'
    , unity = '1'
    , digits = String.toList "23456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    }


viewOneValue : NumberSystem -> Int -> String
viewOneValue system value =
    let
        base =
            Base <| List.length system.digits + 2

        placeValues =
            toList <| fromInt base value

        viewOnePlace v =
            String.fromChar <|
                case v of
                    0 ->
                        system.nullity

                    1 ->
                        system.unity

                    n ->
                        Maybe.withDefault '?' <| List.head <| List.drop (n - 2) system.digits
    in
    String.concat <| List.map viewOnePlace <| List.reverse placeValues


encodeBaseSixtyTwo : Int -> String
encodeBaseSixtyTwo =
    viewOneValue baseSixtyTwo
