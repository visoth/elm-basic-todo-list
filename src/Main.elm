module Main exposing (AppModel, AppMsg(..), init, main, update, view)

import Browser
import Bulma.CDN exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Form exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Bulma.Modifiers.Typography exposing (textCentered)
import Html exposing (Html, div, h1, h2, img, main_, p, span, text)
import Html.Attributes exposing (placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Todo



---- MODEL ----


type alias AppModel =
    { todo : Todo.Model
    }


init : ( AppModel, Cmd AppMsg )
init =
    ( { todo = Todo.init
      }
    , Cmd.none
    )



---- UPDATE ----


type AppMsg
    = TodoMsg Todo.Msg


update : AppMsg -> AppModel -> ( AppModel, Cmd AppMsg )
update msg model =
    case msg of
        TodoMsg subMsg ->
            let
                ( subModel, subCmd ) =
                    Todo.update subMsg model.todo
            in
            ( { model | todo = subModel }, Cmd.map TodoMsg subCmd )



---- VIEW ----


view : AppModel -> Html AppMsg
view model =
    main_ []
        [ stylesheet
        , Html.map TodoMsg (Todo.view model.todo)
        ]



---- PROGRAM ----


main : Program () AppModel AppMsg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
