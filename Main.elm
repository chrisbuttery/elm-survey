module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Html.Attributes exposing (..)
import Html.App as Html
import String
import Json.Decode as Json exposing (..)

import Model exposing (Model, Option, newOption)
import Ports exposing (..)


-- UPDATE

{--
  Action: Union Type
  An action could be one of the following listed.
--}

type Msg =
  NoOp
  | Increment Int
  | Decrement Int
  | UpdateTitleInput String
  | Add (Maybe String)
  | Remove Int


{--
  update:
  Take an action type and a model.
  If the action is 'Increment' or 'Decrement' update the model by 1
  'UpdateTitleInput' sets the title on the model
  'Add' prepends a new 'Model' to our existing options
  'Remove' deletes an option from our options
--}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)

    Increment id ->
      let
        updateEntry e =
          if e.id == id then { e | count = e.count + 1 } else { e | count = e.count }

        model' =
          { model | options = List.map updateEntry model.options }
      in
        ( model', Ports.modelChange model' )

    Decrement id ->
      let
        count num =
          if num > 0 then num - 1 else 0

        updateEntry e =
          if e.id == id then { e | count = (count e.count) } else { e | count = e.count }

        model' =
          { model | options = List.map updateEntry model.options }
      in
        ( model', Ports.modelChange model' )

    UpdateTitleInput title ->
      let
        model' =
          { model | newOptionTitle = title }
      in
        (model', Ports.modelChange model' )

    Add title ->
      let
        newTitle = Maybe.withDefault model.newOptionTitle title

        newlyAddedOption =
          newOption newTitle 0 model.nextID

        isInvalid model =
          String.isEmpty model.newOptionTitle

        model' =
          if isInvalid model
          then model
          else
            { model |
              options = newlyAddedOption :: model.options
              , newOptionTitle = ""
              , nextID = model.nextID + 1
            }
      in
        ( model', Ports.modelChange model' )

    Remove id ->
      let
        remainingOptions = List.filter (\e -> e.id /= id) model.options

        model' =
          { model | options = remainingOptions }
      in
        (model', Ports.modelChange model' )


-- VIEW


{--
  onInput:
  Create a Signal that map to an Action (Add)
--}

onInput : Attribute Msg
onInput =
  on "input" (Json.map UpdateTitleInput targetValue)


{--
  view:
  Takes an address and a model
  Call title, optionList and optionForm passing in
  require params
--}


view : Model -> Html Msg
view model =
  div [] [
    div [] [
      title
      , optionList model.options
      , optionForm model.newOptionTitle
    ]
  ]


{--
  title returns static Html
--}

title : Html Msg
title =
  h1 [ class "title" ] [ text "What languages do you use primarily, besides JavaScript?"]


{--
  optionList:
  Take an address and array of options.
  Iterate all options and assign to 'optionItems'.
  Render to a UL
--}

optionList : List Option -> Html Msg
optionList options =
  let
    optionItems = List.map (optionItem) options
  in
    ul [ class "options" ] optionItems


{--
  optionItem
  Take an address and an option.
  Render a LI populating the element from the option.
  Apply Signals for click events
--}

optionItem : Option -> Html Msg
optionItem option =
  li [ class "option" ]
    [ div [ class "actions"] [
        button [ class "button vote-down", onClick (Decrement option.id) ] [ text "Down" ]
        , button [ class "button vote-up", onClick (Increment option.id) ] [ text "Up" ]
        , button [ class "button remove", onClick (Remove option.id) ] [ text "x" ]
      ]
      , div [ class "option__title" ] [
        span [] [ text option.title ]
        , span [ class "count"] [ text ("Votes: " ++ toString option.count) ]
       ]
    ]


{--
  optionForm:
  Take an address and an title.
  Render a text input and button
  Apply a Signals to 'Add'
--}

optionForm : String -> Html Msg
optionForm title
 =
  div [ class "form" ] [
    input
      [ class "form__input"
      , type' "text"
      , placeholder "Other..."
      , Html.Attributes.value title
      , name "title"
      , autofocus True
      , onInput
      ] []
      , button [ class "button form__button add", onClick (Add Nothing) ] [ text "Add" ]
    ]


-- Main


initialModel : Model
initialModel =
  { options =
    [ newOption "Ruby" 1 1
    , newOption "Rust" 3 2
    , newOption "Python" 2 3
    , newOption "Elm" 5 4
    ]
    , newOptionTitle = ""
    , nextID = 5
  }


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
  ( Maybe.withDefault initialModel savedModel, Cmd.none )


main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
  Ports.newOptionTitle UpdateTitleInput
