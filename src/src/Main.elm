module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Src.Models exposing (Ingredient, Dish)
import Src.Dummy exposing (milk, cheese, carrot, sallad, soup)


main : Program Never
main =
    App.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { ingredientInput : String
    , unitInput : String
    , categoryInput : String
    , dishName : String
    , dishes : List Dish
    , ingredientsCategory : List String
    , categorySelected : String
    , ingredients : List Ingredient
    }


model : Model
model =
    { ingredientInput = ""
    , unitInput = ""
    , categoryInput = "Välj.."
    , dishName = ""
    , dishes = [ sallad, soup ]
    , ingredientsCategory = [ "Välj..", "Mejeri", "Grönsaker", "Frukt", "Kryddor" ]
    , categorySelected = "Välj.."
    , ingredients = [ milk, cheese, carrot ]
    }



-- UPDATE


type Msg
    = AddDish Dish
    | AddIngredient Ingredient
    | InputIngredient String
    | InputUnit String
    | InputDish String
    | CategorySelect String
    | IngredientCategory String
    | CheckedIngredient Ingredient
    | UpdateIngredientVolume Ingredient String


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddDish dish ->
            addDish model dish

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

        CheckedIngredient ingredient ->
            toggleCheckedIngredients model ingredient

        UpdateIngredientVolume ingredient volume ->
            updateIngredientVolume model ingredient volume


updateIngredientVolume : Model -> Ingredient -> String -> Model
updateIngredientVolume model ingredient volume =
    let
        newList =
            List.map
                (\x ->
                    if x == ingredient then
                        { x | volume = volume }
                    else
                        x
                )
                model.ingredients
    in
        { model | ingredients = newList }


addDish : Model -> Dish -> Model
addDish model dish =
    let
        newList =
            List.map
                (\x -> { x | checked = False })
                model.ingredients
    in
        { model
            | dishes = dish :: model.dishes
            , ingredients = newList
        }


toggleCheckedIngredients : Model -> Ingredient -> Model
toggleCheckedIngredients model ingredient =
    let
        newList =
            List.map
                (\x ->
                    if x == ingredient then
                        { x | checked = not x.checked }
                    else
                        x
                )
                model.ingredients
    in
        { model | ingredients = newList }


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
        , dinnerSelectionSection model
        , div [] [ text (toString model.dishes) ]
        ]


dinnerSelectionSection : Model -> Html Msg
dinnerSelectionSection model =
    div []
        [ h3 [] [ text "Choose dinners for the week" ] ]


dishInputSection : Model -> Html Msg
dishInputSection model =
    Html.form
        [ onSubmit
            (AddDish
                (Dish model.dishName (getCheckedIngredients model))
            )
        ]
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


getCheckedIngredients : Model -> List Ingredient
getCheckedIngredients model =
    List.filter
        (\x -> x.checked == True)
        model.ingredients


ingredientInputSection : Model -> Html Msg
ingredientInputSection model =
    Html.form
        [ onSubmit
            (AddIngredient
                (Ingredient
                    model.ingredientInput
                    model.unitInput
                    "0"
                    model.categoryInput
                    False
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
            [ type' "checkbox"
            , id ingredient.name
            , onClick (CheckedIngredient ingredient)
            , checked ingredient.checked
            ]
            []
        , label
            [ for ingredient.name, class "checkbox-label" ]
            [ text ingredient.name ]
        , input
            [ type' "text"
            , onInput (UpdateIngredientVolume ingredient)
            , value ingredient.volume
            ]
            []
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
