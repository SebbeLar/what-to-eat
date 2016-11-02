module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json


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
    , categoryInput : String
    , dishName : String
    , dishes : List String
    , ingredientsCategory : List String
    , categorySelected : String
    }


type alias Ingredient =
    { name : String
    , unit : String
    , category : String
    }


type alias Dish =
    { name : String
    , ingredients : List Ingredient
    }


model : Model
model =
    { ingredients = []
    , ingredientInput = ""
    , unitInput = ""
    , categoryInput = "Välj.."
    , dishName = ""
    , dishes = []
    , ingredientsCategory = [ "Välj..", "Mejeri", "Grönsaker", "Frukt", "Kryddor" ]
    , categorySelected = "Välj.."
    }



-- UPDATE


type Msg
    = AddDish String
    | AddIngredient Ingredient
    | InputIngredient String
    | InputUnit String
    | InputDish String
    | CategorySelect String
    | IngredientCategory String


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddDish dish ->
            { model | dishes = dish :: model.dishes }

        AddIngredient ingredient ->
            addIngredient model ingredient

        InputIngredient ingredient ->
            { model | ingredientInput = ingredient }

        InputUnit unit ->
            { model | unitInput = unit }

        InputDish dish ->
            { model | dishName = dish }

        CategorySelect ingredient ->
            { model | categorySelected = ingredient }

        IngredientCategory ingredient ->
            { model | categoryInput = ingredient }


addIngredient : Model -> Ingredient -> Model
addIngredient model ingredient =
    { model
        | ingredients = ingredient :: model.ingredients
        , ingredientInput = ""
        , unitInput = ""
    }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ ingredientInputSection model
        , dishInputSection model
        , div [] [ categoryCheckboxSection model ]
        , div [] [ text (toString model) ]
        ]


dishInputSection : Model -> Html Msg
dishInputSection model =
    Html.form [ onSubmit (AddDish model.dishName) ]
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


ingredientInputSection : Model -> Html Msg
ingredientInputSection model =
    Html.form
        [ onSubmit
            (AddIngredient
                (Ingredient
                    model.ingredientInput
                    model.unitInput
                    model.categoryInput
                )
            )
        ]
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
        , ingredientCategorySelect model
        , input [ type' "submit" ] [ text "Save" ]
        ]


categoryCheckboxSection : Model -> Html Msg
categoryCheckboxSection model =
    model.ingredients
        |> List.filter (\val -> val.category == model.categorySelected)
        |> List.map ingredientCheckbox
        |> ul []


ingredientCheckbox : Ingredient -> Html Msg
ingredientCheckbox ingredient =
    li []
        [ input
            [ type' "checkbox", id ingredient.name ]
            []
        , label
            [ for ingredient.name, class "checkbox-label" ]
            [ text ingredient.name ]
        ]


ingredientCategorySelect : Model -> Html Msg
ingredientCategorySelect model =
    model.ingredientsCategory
        |> List.map selectOptions
        |> select [ on "change" (Json.map IngredientCategory targetValue) ]


ingredientsSelect : Model -> Html Msg
ingredientsSelect model =
    model.ingredientsCategory
        |> List.map selectOptions
        |> select [ on "change" (Json.map CategorySelect targetValue) ]


selectOptions : String -> Html Msg
selectOptions ingredient =
    option [ value ingredient ] [ text ingredient ]
