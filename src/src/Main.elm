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
    , dishes : List Dish
    , ingredientsCategory : List String
    , categorySelected : String
    , checkboxIngredient : List CheckboxIngredients
    }


type alias CheckboxIngredients =
    { name : String
    , unit : String
    , volume : Float
    , category : String
    , checked : Bool
    }


type alias Ingredient =
    { name : String
    , unit : String
    , category : String
    , checked : Bool
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
    , checkboxIngredient = []
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
    | CheckedIngredient CheckboxIngredients


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


toggleCheckedIngredients : Model -> CheckboxIngredients -> Model
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
                model.checkboxIngredient
    in
        { model | checkboxIngredient = newList }


addIngredient : Model -> Ingredient -> Model
addIngredient model ingredient =
    let
        newIngredient =
            CheckboxIngredients ingredient.name ingredient.unit 0.0 ingredient.category False
    in
        { model
            | ingredients = ingredient :: model.ingredients
            , checkboxIngredient = newIngredient :: model.checkboxIngredient
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
        , div [] [ text (toString model.checkboxIngredient) ]
        ]


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
    model.checkboxIngredient
        |> List.filter (\val -> val.category == model.categorySelected)
        |> List.map ingredientCheckbox
        |> ul []


ingredientCheckbox : CheckboxIngredients -> Html Msg
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
