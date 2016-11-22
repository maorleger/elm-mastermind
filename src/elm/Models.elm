module Models exposing (..)

import Http


type GameOver
    = Win
    | Lose
    | None
    | Error Http.Error


type Peg
    = Orange
    | Yellow
    | Green
    | Red
    | Blue
    | Pink


type alias Score =
    ( Int, Int )



-- MODEL


type alias Round =
    { guess : List Peg
    , score : Maybe Score
    }
