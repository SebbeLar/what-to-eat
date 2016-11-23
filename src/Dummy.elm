module Src.Dummy exposing (milk, cheese, carrot, sallad, soup)

import Src.Models exposing (Ingredient, Dish)


milk : Ingredient
milk =
    Ingredient "Mjölk" "dl" 0 "Mejeri" False


cheese : Ingredient
cheese =
    Ingredient "Ost" "dl" 0 "Mejeri" False


carrot : Ingredient
carrot =
    Ingredient "Morrot" "dl" 0 "Grönsaker" False


milk_ : Ingredient
milk_ =
    Ingredient "Mjölk" "dl" 3 "Mejeri" False


cheese_ : Ingredient
cheese_ =
    Ingredient "Ost" "dl" 6 "Mejeri" False


carrot_ : Ingredient
carrot_ =
    Ingredient "Morrot" "dl" 4 "Grönsaker" False


sallad : Dish
sallad =
    Dish "Sallad" [ milk_, cheese_ ] False


soup : Dish
soup =
    Dish "Soppa" [ milk_, cheese_, carrot_ ] False
