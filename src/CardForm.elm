module CardForm exposing (InternalMsg, Model, Msg(..), OutputMsg(..), update, view)

import Card exposing (Card, CardPhrase)
import Css exposing (column, displayFlex, flexDirection)
import Html.Styled exposing (Html, button, div, input, label, text)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onClick, onInput)


type alias Model =
    { title : String
    , phrases : List CardPhrase
    }


type OutputMsg
    = Cancel
    | Save


type InternalMsg
    = AddCardFormPair
    | DeleteCardFormPair Int
    | EditTitle String
    | EditPhrase Int String
    | EditTranslation Int String


type Msg
    = Internal InternalMsg
    | Out OutputMsg


update : InternalMsg -> Model -> Model
update msg model =
    case msg of
        AddCardFormPair ->
            { model | phrases = { phrase = "", translation = "" } :: model.phrases }

        DeleteCardFormPair index ->
            { model | phrases = List.take index model.phrases ++ List.drop (index + 1) model.phrases }

        EditTitle title ->
            { model | title = title }

        EditPhrase index phrase ->
            { model
                | phrases =
                    List.indexedMap
                        (\i prevPhrase ->
                            if i == index then
                                { prevPhrase | phrase = phrase }

                            else
                                prevPhrase
                        )
                        model.phrases
            }

        EditTranslation index translation ->
            { model
                | phrases =
                    List.indexedMap
                        (\i prevPhrase ->
                            if i == index then
                                { prevPhrase | translation = translation }

                            else
                                prevPhrase
                        )
                        model.phrases
            }


view : Model -> Html Msg
view model =
    div [ css [ displayFlex, flexDirection column ] ]
        [ label []
            [ text "title: "
            , input [ value model.title, onInput <| Internal << EditTitle ] []
            ]
        , div [] <|
            div [] [ button [ onClick <| Internal AddCardFormPair ] [ text "Add new pair" ] ]
                :: List.indexedMap
                    (\index { phrase, translation } ->
                        div []
                            [ label []
                                [ text "phrase: "
                                , input
                                    [ value phrase
                                    , onInput <| Internal << EditPhrase index
                                    ]
                                    []
                                ]
                            , label []
                                [ text "translation: "
                                , input
                                    [ value translation
                                    , onInput <| Internal << EditTranslation index
                                    ]
                                    []
                                ]
                            , button [ onClick <| Internal <| DeleteCardFormPair index ]
                                [ text "Delete this pair"
                                ]
                            ]
                    )
                    model.phrases
        , div []
            [ button [ onClick <| Out Cancel ] [ text "cancel" ]
            , button [ onClick <| Out <| Save ] [ text "save" ]
            ]
        ]
