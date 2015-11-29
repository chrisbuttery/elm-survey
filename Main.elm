module Main where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Signal exposing (Address)
import String


-- MODEL

{-- define a model type --}

type alias Model =
  { options : List Option
  , newOptionTitle : String
  , nextID : Int
  }


{-- define an option type --}

type alias Option =
  { title: String
  , count: Int
  , id: Int
  }


{--
  newOption:
  Take a string, int and int.
  Return a structure based on the 'Option' type
--}

newOption : String -> Int -> Int -> Option
newOption title count id =
  { title = title
  , count = count
  , id = id
  }


{--
  initialModel:
  Define a default model
  Use 'emptyModel' if 'getStoredModel' isn't returning a model
  from localStorage
--}

initialModel : Model
initialModel =
  let
    emptyModel =
      { options =
        [ newOption "Ruby" 1 1
        , newOption "Rust" 3 2
        , newOption "Python" 2 3
        , newOption "Elm" 5 4
        ]
        , newOptionTitle = ""
        , nextID = 5
      }
  in
    Maybe.withDefault emptyModel getStoredModel


-- UPDATE

{--
  Action: Union Type
  An action could be one of the following listed.
--}

type Action =
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

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    Increment id ->
      let
        updateEntry e =
          if e.id == id then { e | count = e.count + 1 } else { e | count = e.count }
      in
        { model | options = List.map updateEntry model.options }

    Decrement id ->
      let
        count num =
          if num > 0 then num - 1 else 0
        updateEntry e =
          if e.id == id then { e | count = (count e.count) } else { e | count = e.count }
      in
        { model | options = List.map updateEntry model.options }

    UpdateTitleInput title ->
      { model | newOptionTitle = title }

    Add title ->
      let
        newTitle = Maybe.withDefault model.newOptionTitle title
        newlyAddedOption =
          newOption newTitle 0 model.nextID

        isInvalid model =
          String.isEmpty model.newOptionTitle
      in
        if isInvalid model
        then model
        else
          { model |
            options = newlyAddedOption :: model.options
            , newOptionTitle = ""
            , nextID = model.nextID + 1
          }

    Remove id ->
      let
        remainingOptions = List.filter (\e -> e.id /= id) model.options
      in
        { model | options = remainingOptions }

-- VIEW

{--
  onInput:
  Create a Signal that map to an Action (Add)
--}

onInput : Address a -> (String -> a) -> Attribute
onInput address f =
  on "input" targetValue (\v -> Signal.message address (f v))


{--
  view:
  Takes an address and a model
  Call title, optionList and optionForm passing in
  require params
--}

view : Address Action -> Model -> Html
view address model =
  div [] [
    div [] [
      title
      , optionList address model.options
      , optionForm address model.newOptionTitle
    ]
  ]

{--
  title returns static Html
--}

title : Html
title =
  h1 [ class "title" ] [ text "What languages do you use primarily, besides JavaScript?"]


{--
  optionList:
  Take an address and array of options.
  Iterate all options and assign to 'optionItems'.
  Render to a UL
--}

optionList : Address Action -> List Option -> Html
optionList address options =
  let
    optionItems = List.map (optionItem address) options
  in
    ul [ class "options" ] optionItems


{--
  optionItem
  Take an address and an option.
  Render a LI populating the element from the option.
  Apply Signals for click events
--}

optionItem : Address Action -> Option -> Html
optionItem address option =
  li [ class "option" ]
    [ div [ class "actions"] [
        button [ class "button vote-down", onClick address (Decrement option.id) ] [ text "Down" ]
        , button [ class "button vote-up", onClick address (Increment option.id) ] [ text "Up" ]
        , button [ class "button remove", onClick address (Remove option.id) ] [ text "x" ]
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

optionForm : Address Action -> String -> Html
optionForm address title
 =
  div [ class "form" ] [
    input
      [ class "form__input"
      , type' "text"
      , placeholder "Other..."
      , value title
      , name "title"
      , autofocus True
      , onInput address UpdateTitleInput
      ] []
      , button [ class "button form__button add", onClick address (Add Nothing) ] [ text "Add" ]
    ]


-- SIGNALS

inbox : Signal.Mailbox Action
inbox =
  Signal.mailbox NoOp


actions : Signal Action
actions =
  Signal.merge inbox.signal (Signal.map Add newOptionTitle)


model : Signal Model
model =
  Signal.foldp update initialModel actions


main : Signal Html
main =
  Signal.map (view inbox.address) model


-- PORTS

port modelChanges : Signal Model
port modelChanges =
  model

port getStoredModel : Maybe Model

port newOptionTitle : Signal (Maybe String)
