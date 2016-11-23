module Src.Models exposing (..)


type alias Ingredient =
    { name : String
    , unit : String
    , volume : Float
    , category : String
    , checked : Bool
    }


type alias Dish =
    { name : String
    , ingredients : List Ingredient
    , checked : Bool
    }
