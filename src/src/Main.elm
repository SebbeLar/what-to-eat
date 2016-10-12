module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program Never
main =
    App.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { ingredients : List Ingredient
    , ingredientInput : String
    , unitInput : String
    , dishName : String
    , dishes : List String
    }


type alias Ingredient =
    { name : String
    , unit : String
    }


model : Model
model =
    { ingredients = []
    , ingredientInput = ""
    , unitInput = ""
    , dishName = ""
    , dishes = []
    }



-- UPDATE


type Msg
    = AddDish String
    | AddIngredient String String
    | InputIngredient String
    | InputUnit String
    | InputDish String


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddDish dish ->
            { model | dishes = dish :: model.dishes }

        AddIngredient ingredient unit ->
            addIngredient model ingredient unit

        InputIngredient ingredient ->
            { model | ingredientInput = ingredient }

        InputUnit unit ->
            { model | unitInput = unit }

        InputDish dish ->
            { model | dishName = dish }


addIngredient : Model -> String -> String -> Model
addIngredient model ingredient unit =
    let
        newIngredient =
            Ingredient ingredient unit
    in
        { model
            | ingredients = newIngredient :: model.ingredients
            , ingredientInput = ""
            , unitInput = ""
        }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ onSubmit (AddIngredient model.ingredientInput model.unitInput) ]
            [ input
                [ type' "text"
                , onInput InputIngredient
                , placeholder "Add Ingredient"
                , value model.ingredientInput
                ]
                []
            , input
                [ type' "text"
                , onInput InputUnit
                , placeholder "Add Unit"
                , value model.unitInput
                ]
                []
            , input [ type' "submit" ] [ text "Save" ]
            ]
        , Html.form [ onSubmit (AddDish model.dishName) ]
            [ input
                [ type' "text"
                , onInput InputDish
                , placeholder "Add Dish"
                , value model.dishName
                ]
                []
            , ingredientsSelect model
            , input [ type' "submit" ] [ text "Save" ]
            ]
        , div [] [ text (toString model) ]
        ]


ingredientsSelect : Model -> Html Msg
ingredientsSelect model =
    model.ingredients
        |> List.map selectOptions
        |> select []


selectOptions : Ingredient -> Html Msg
selectOptions ingredient =
    option [ value ingredient.name ] [ text ingredient.name ]
