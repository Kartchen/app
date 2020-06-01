port module Main exposing (ModelVariant, Msg(..), init, main, update, view)

import Browser
import Card exposing (Card, CardPhrase)
import CardForm
import Css exposing (alignItems, auto, border2, borderBottom, borderBottom2, borderRadius, center, displayFlex, em, fontSize, justifyContent, lastChild, listStyle, margin, margin2, marginBottom, maxWidth, none, padding, px, rem, solid, spaceBetween)
import Html.Styled as Html exposing (Html, b, button, div, h1, i, li, p, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed exposing (ul)
import Html.Styled.Lazy exposing (lazy)
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Maybe exposing (withDefault)


port signIn : () -> Cmd msg


port signInInfo : (Json.Encode.Value -> msg) -> Sub msg


port loggedOutUser : (() -> msg) -> Sub msg


port signInError : (() -> msg) -> Sub msg


port signOut : () -> Cmd msg


port addNewCard : Json.Encode.Value -> Cmd msg


port addNewCardError : (() -> msg) -> Sub msg


port deleteCard : Json.Encode.Value -> Cmd msg


port deleteCardError : (() -> msg) -> Sub msg


port saveEditedCard : Json.Encode.Value -> Cmd msg


port saveEditedCardError : (() -> msg) -> Sub msg


port receiveCards : (Json.Encode.Value -> msg) -> Sub msg


port receiveCardsError : (() -> msg) -> Sub msg



---- MODEL ----


type alias UserData =
    { email : String
    , uid : String
    , name : String
    }


type Cards
    = GetCardsFailed
    | SaveEditedCardFailed
    | DeleteCardFailed
    | AddNewCardFailed
    | CardsList (List Card)


type alias CardFormOld =
    { title : String
    , phrases : List CardPhrase
    }


type CardFormState
    = Hidden
    | AddCard CardForm.Model
    | EditCard Card CardForm.Model


type alias LoggedInModel =
    { user : UserData
    , cardForm : CardFormState
    , cards : Cards
    }


type ModelVariant
    = UnknownUser
    | LoggedOut
    | LoginError
    | LoggedIn LoggedInModel


init : () -> ( ModelVariant, Cmd Msg )
init _ =
    ( UnknownUser, Cmd.none )



---- UPDATE ----


type Msg
    = LogIn
    | LogOut
    | LoggedInData (Result Json.Decode.Error UserData)
    | LoggedInError
    | AddNewCardError
    | DeleteCardError
    | SaveEditedCardError
    | CardsReceived (Result Json.Decode.Error (List Card))
    | CardsReceivedError
    | OpenAddCardForm
    | OpenEditCardForm Card
    | DeleteCard Card
    | CloseCardForm
    | CardFormMsg CardForm.Msg


update : Msg -> ModelVariant -> ( ModelVariant, Cmd Msg )
update msg variant =
    case variant of
        UnknownUser ->
            case msg of
                LogOut ->
                    ( LoggedOut, signOut () )

                LoggedInData (Ok userData) ->
                    ( LoggedIn
                        { user = userData
                        , cardForm = Hidden
                        , cards = CardsList []
                        }
                    , Cmd.none
                    )

                LoggedInData (Err _) ->
                    ( LoginError, Cmd.none )

                LoggedInError ->
                    ( LoginError, Cmd.none )

                _ ->
                    ( UnknownUser, Cmd.none )

        LoggedOut ->
            case msg of
                LogIn ->
                    ( UnknownUser, signIn () )

                LoggedInData (Ok userData) ->
                    ( LoggedIn
                        { user = userData
                        , cardForm = Hidden
                        , cards = CardsList []
                        }
                    , Cmd.none
                    )

                LoggedInData (Err _) ->
                    ( LoginError, Cmd.none )

                LoggedInError ->
                    ( LoginError, Cmd.none )

                _ ->
                    ( variant, Cmd.none )

        LoginError ->
            case msg of
                LogIn ->
                    ( UnknownUser, signIn () )

                LoggedInData (Ok userData) ->
                    ( LoggedIn
                        { user = userData
                        , cardForm = Hidden
                        , cards = CardsList []
                        }
                    , Cmd.none
                    )

                _ ->
                    ( variant, Cmd.none )

        LoggedIn model ->
            case msg of
                LogIn ->
                    ( variant, Cmd.none )

                LogOut ->
                    ( LoggedOut, signOut () )

                LoggedInData (Ok userData) ->
                    ( LoggedIn { model | user = userData }, Cmd.none )

                LoggedInData (Err _) ->
                    ( LoginError, Cmd.none )

                LoggedInError ->
                    ( LoginError, Cmd.none )

                SaveEditedCardError ->
                    ( LoggedIn { model | cards = SaveEditedCardFailed }, Cmd.none )

                AddNewCardError ->
                    ( LoggedIn { model | cards = AddNewCardFailed }, Cmd.none )

                CardsReceived (Ok cards) ->
                    ( LoggedIn { model | cards = CardsList cards }, Cmd.none )

                CardsReceived (Err _) ->
                    ( LoggedIn { model | cards = GetCardsFailed }, Cmd.none )

                CardsReceivedError ->
                    ( LoggedIn { model | cards = GetCardsFailed }, Cmd.none )

                DeleteCard card ->
                    ( LoggedIn model, deleteCard <| existingCardEncoder card model.user.uid )

                DeleteCardError ->
                    ( LoggedIn { model | cards = DeleteCardFailed }, Cmd.none )

                OpenAddCardForm ->
                    ( LoggedIn
                        { model
                            | cardForm =
                                AddCard <|
                                    { title = ""
                                    , phrases = [ { phrase = "", translation = "" } ]
                                    }
                        }
                    , Cmd.none
                    )

                OpenEditCardForm card ->
                    ( LoggedIn
                        { model
                            | cardForm =
                                EditCard card
                                    { title = card.title
                                    , phrases = card.phrases
                                    }
                        }
                    , Cmd.none
                    )

                CloseCardForm ->
                    ( LoggedIn { model | cardForm = Hidden }, Cmd.none )

                CardFormMsg cardFormMsg ->
                    case cardFormMsg of
                        CardForm.Internal cardFormInternalMsg ->
                            case model.cardForm of
                                Hidden ->
                                    ( LoggedIn model, Cmd.none )

                                AddCard cardFormModel ->
                                    ( LoggedIn
                                        { model
                                            | cardForm = AddCard <| CardForm.update cardFormInternalMsg cardFormModel
                                        }
                                    , Cmd.none
                                    )

                                EditCard card cardFormModel ->
                                    ( LoggedIn
                                        { model
                                            | cardForm = EditCard card <| CardForm.update cardFormInternalMsg cardFormModel
                                        }
                                    , Cmd.none
                                    )

                        CardForm.Out cardFormOutMsg ->
                            case cardFormOutMsg of
                                CardForm.Cancel ->
                                    ( LoggedIn { model | cardForm = Hidden }, Cmd.none )

                                CardForm.Save ->
                                    ( LoggedIn { model | cardForm = Hidden }
                                    , case model.cardForm of
                                        Hidden ->
                                            Cmd.none

                                        AddCard newCardForm ->
                                            addNewCard <| newCardEncoder newCardForm model.user.uid

                                        EditCard editedCard newCardForm ->
                                            saveEditedCard <|
                                                existingCardEncoder
                                                    { id = editedCard.id
                                                    , title = newCardForm.title
                                                    , phrases = newCardForm.phrases
                                                    }
                                                    model.user.uid
                                    )


phraseEncode : CardPhrase -> Json.Encode.Value
phraseEncode { phrase, translation } =
    Json.Encode.object
        [ ( "phrase", Json.Encode.string phrase )
        , ( "translation", Json.Encode.string translation )
        ]


newCardEncoder : CardFormOld -> String -> Json.Encode.Value
newCardEncoder cardForm uid =
    Json.Encode.object
        [ ( "phrases", Json.Encode.list phraseEncode cardForm.phrases )
        , ( "title", Json.Encode.string cardForm.title )
        , ( "uid", Json.Encode.string uid )
        ]


existingCardEncoder : Card -> String -> Json.Encode.Value
existingCardEncoder card uid =
    Json.Encode.object
        [ ( "phrases", Json.Encode.list phraseEncode card.phrases )
        , ( "title", Json.Encode.string card.title )
        , ( "id", Json.Encode.string card.id )
        , ( "uid", Json.Encode.string uid )
        ]


userDataDecoder : Json.Decode.Decoder UserData
userDataDecoder =
    Json.Decode.succeed UserData
        |> Json.Decode.Pipeline.required "email" Json.Decode.string
        |> Json.Decode.Pipeline.required "uid" Json.Decode.string
        |> Json.Decode.Pipeline.optional "name" Json.Decode.string "User"


phraseDecoder : Json.Decode.Decoder CardPhrase
phraseDecoder =
    Json.Decode.succeed CardPhrase
        |> Json.Decode.Pipeline.required "phrase" Json.Decode.string
        |> Json.Decode.Pipeline.required "translation" Json.Decode.string


cardDecoder : Json.Decode.Decoder Card
cardDecoder =
    Json.Decode.succeed Card
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "phrases" (Json.Decode.list phraseDecoder)


cardsListDecoder : Json.Decode.Decoder (List Card)
cardsListDecoder =
    Json.Decode.succeed identity
        |> Json.Decode.Pipeline.required "cards" (Json.Decode.list cardDecoder)


formatUserName : UserData -> String
formatUserName useData =
    useData.name |> String.split " " |> List.head |> withDefault "User"


viewCard : Card -> Html Msg
viewCard card =
    li
        [ css
            [ listStyle none
            , border2 (px 1) solid
            , marginBottom <| em 1
            , borderRadius <| em 0.25
            ]
        ]
        [ div [ css [ padding (em 1) ] ]
            [ p [ css [ margin (px 0) ] ] [ b [] [ text card.title ] ]
            , div [] <|
                List.map
                    (\{ phrase, translation } ->
                        div [ css [ borderBottom2 (px 1) solid, lastChild [ borderBottom (px 0) ] ] ]
                            [ p [] [ text <| phrase ++ " - ", i [ css [ fontSize (em 0.75) ] ] [ text translation ] ]
                            ]
                    )
                    card.phrases
            , button [ onClick (OpenEditCardForm card) ] [ text "edit" ]
            , button [ onClick (DeleteCard card) ] [ text "delete" ]
            ]
        ]


viewKeyedCard : Card -> ( String, Html Msg )
viewKeyedCard card =
    ( card.id, lazy viewCard card )



---- VIEW ----


view : ModelVariant -> Html Msg
view variant =
    div [ css [ padding (em 1), maxWidth <| rem 40, margin2 (px 0) auto ] ]
        [ h1 [] [ text "Kärtchen" ]
        , case variant of
            UnknownUser ->
                text "Loading user info..."

            LoggedOut ->
                button [ onClick LogIn ] [ text "Login with Google" ]

            LoginError ->
                text "error obtaining user info"

            LoggedIn model ->
                div []
                    [ div
                        [ css
                            [ displayFlex
                            , justifyContent spaceBetween
                            , alignItems center
                            ]
                        ]
                        [ p [] [ text <| "Hello, " ++ formatUserName model.user ++ "!" ]
                        , button
                            [ onClick LogOut
                            ]
                            [ text "Logout" ]
                        ]
                    , case model.cardForm of
                        AddCard cardForm ->
                            Html.map CardFormMsg <| CardForm.view cardForm

                        EditCard _ cardForm ->
                            Html.map CardFormMsg <| CardForm.view cardForm

                        Hidden ->
                            div []
                                [ button [ onClick OpenAddCardForm ] [ text "Add new card" ]
                                , case model.cards of
                                    CardsList cards ->
                                        ul [ css [ padding (px 0) ] ] <| List.map viewKeyedCard cards

                                    SaveEditedCardFailed ->
                                        b [] [ text "Could not save edited card" ]

                                    GetCardsFailed ->
                                        b [] [ text "Could not get cards" ]

                                    AddNewCardFailed ->
                                        b [] [ text "Could not add new card" ]

                                    DeleteCardFailed ->
                                        b [] [ text "Could not delete card" ]
                                ]
                    ]
        ]



---- PROGRAM ----


subscriptions : ModelVariant -> Sub Msg
subscriptions _ =
    Sub.batch
        [ signInInfo <| Json.Decode.decodeValue userDataDecoder >> LoggedInData
        , signInError <| always LoggedInError
        , loggedOutUser <| always LogOut
        , saveEditedCardError <| always SaveEditedCardError
        , addNewCardError <| always AddNewCardError
        , deleteCardError <| always DeleteCardError
        , receiveCards <| Json.Decode.decodeValue cardsListDecoder >> CardsReceived
        , receiveCardsError <| always CardsReceivedError
        ]


main : Program () ModelVariant Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
