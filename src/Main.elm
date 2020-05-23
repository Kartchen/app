port module Main exposing (ModelVariant, Msg(..), init, main, update, view)

import Browser
import Css exposing (column, displayFlex, em, flexDirection, listStyle, none, padding, px)
import Html.Styled exposing (Html, b, button, div, h1, i, input, label, li, p, text, toUnstyled)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onClick, onInput)
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


type alias Card =
    { id : String
    , title : String
    , phrase : String
    , translation : String
    }


type Cards
    = GetCardsFailed
    | SaveEditedCardFailed
    | AddNewCardFailed
    | CardsList (List Card)


type CardFormVariant
    = AddCard
    | EditCard Card


type alias CardForm =
    { variant : CardFormVariant
    , title : String
    , phrase : String
    , translation : String
    }


type alias LoggedInModel =
    { user : UserData
    , cardForm : Maybe CardForm
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
    | SaveEditedCardError
    | CardsReceived (Result Json.Decode.Error (List Card))
    | CardsReceivedError
    | OpenAddCardForm
    | OpenEditCardForm Card
    | CloseCardForm
    | SubmitNewCard CardForm
    | SubmitEditedCard Card
    | EditTitle String
    | EditPhrase String
    | EditTranslation String


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
                        , cardForm = Nothing
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
                        , cardForm = Nothing
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
                        , cardForm = Nothing
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

                OpenAddCardForm ->
                    ( LoggedIn
                        { model
                            | cardForm =
                                Just <|
                                    { variant = AddCard
                                    , title = ""
                                    , phrase = ""
                                    , translation = ""
                                    }
                        }
                    , Cmd.none
                    )

                OpenEditCardForm card ->
                    ( LoggedIn
                        { model
                            | cardForm =
                                Just <|
                                    { variant = EditCard card
                                    , title = card.title
                                    , phrase = card.phrase
                                    , translation = card.translation
                                    }
                        }
                    , Cmd.none
                    )

                CloseCardForm ->
                    ( LoggedIn { model | cardForm = Nothing }, Cmd.none )

                SubmitEditedCard card ->
                    ( LoggedIn { model | cardForm = Nothing }
                    , saveEditedCard <| editedCardEncoder card model.user.uid
                    )

                SubmitNewCard cardForm ->
                    ( LoggedIn { model | cardForm = Nothing }
                    , addNewCard <| newCardEncoder cardForm model.user.uid
                    )

                EditTitle title ->
                    case model.cardForm of
                        Just prevEditedCard ->
                            ( LoggedIn { model | cardForm = Just { prevEditedCard | title = title } }
                            , Cmd.none
                            )

                        Nothing ->
                            ( LoggedIn model, Cmd.none )

                EditPhrase phrase ->
                    case model.cardForm of
                        Just prevEditedCard ->
                            ( LoggedIn { model | cardForm = Just { prevEditedCard | phrase = phrase } }
                            , Cmd.none
                            )

                        Nothing ->
                            ( LoggedIn model, Cmd.none )

                EditTranslation translation ->
                    case model.cardForm of
                        Just prevEditedCard ->
                            ( LoggedIn { model | cardForm = Just { prevEditedCard | translation = translation } }
                            , Cmd.none
                            )

                        Nothing ->
                            ( LoggedIn model, Cmd.none )


newCardEncoder : CardForm -> String -> Json.Encode.Value
newCardEncoder cardForm uid =
    Json.Encode.object
        [ ( "phrase", Json.Encode.string cardForm.phrase )
        , ( "title", Json.Encode.string cardForm.title )
        , ( "translation", Json.Encode.string cardForm.translation )
        , ( "uid", Json.Encode.string uid )
        ]


editedCardEncoder : Card -> String -> Json.Encode.Value
editedCardEncoder card uid =
    Json.Encode.object
        [ ( "phrase", Json.Encode.string card.phrase )
        , ( "title", Json.Encode.string card.title )
        , ( "translation", Json.Encode.string card.translation )
        , ( "id", Json.Encode.string card.id )
        , ( "uid", Json.Encode.string uid )
        ]


userDataDecoder : Json.Decode.Decoder UserData
userDataDecoder =
    Json.Decode.succeed UserData
        |> Json.Decode.Pipeline.required "email" Json.Decode.string
        |> Json.Decode.Pipeline.required "uid" Json.Decode.string
        |> Json.Decode.Pipeline.optional "name" Json.Decode.string "User"


cardDecoder : Json.Decode.Decoder Card
cardDecoder =
    Json.Decode.succeed Card
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "phrase" Json.Decode.string
        |> Json.Decode.Pipeline.required "translation" Json.Decode.string


cardsListDecoder : Json.Decode.Decoder (List Card)
cardsListDecoder =
    Json.Decode.succeed identity
        |> Json.Decode.Pipeline.required "cards" (Json.Decode.list cardDecoder)


formatUserName : UserData -> String
formatUserName useData =
    useData.name |> String.split " " |> List.head |> withDefault "User"


viewCard : Card -> Html Msg
viewCard card =
    li [ css [ listStyle none ] ]
        [ div [ css [ padding (em 1) ] ]
            [ p [] [ b [] [ text card.title ] ]
            , p [] [ text card.phrase ]
            , p [] [ i [] [ text card.translation ] ]
            , button [ onClick (OpenEditCardForm card) ] [ text "edit" ]
            ]
        ]


viewKeyedCard : Card -> ( String, Html Msg )
viewKeyedCard card =
    ( card.id, lazy viewCard card )



---- VIEW ----


view : ModelVariant -> Html Msg
view variant =
    div [ css [ padding (em 1) ] ]
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
                    [ p [] [ text <| "Hello, " ++ formatUserName model.user ++ "!" ]
                    , button [ onClick LogOut ] [ text "Logout" ]
                    , case model.cardForm of
                        Just cardForm ->
                            div [ css [ displayFlex, flexDirection column ] ]
                                [ p []
                                    [ text <|
                                        case cardForm.variant of
                                            AddCard ->
                                                "Add new card:"

                                            EditCard _ ->
                                                "Edit card:"
                                    ]
                                , label []
                                    [ text "title: "
                                    , input [ value cardForm.title, onInput EditTitle ] []
                                    ]
                                , label []
                                    [ text "phrase: "
                                    , input [ value cardForm.phrase, onInput EditPhrase ] []
                                    ]
                                , label []
                                    [ text "translation: "
                                    , input [ value cardForm.translation, onInput EditTranslation ] []
                                    ]
                                , div []
                                    [ button [ onClick CloseCardForm ] [ text "cancel" ]
                                    , case cardForm.variant of
                                        AddCard ->
                                            button [ onClick <| SubmitNewCard cardForm ] [ text "add" ]

                                        EditCard card ->
                                            button
                                                [ onClick <|
                                                    SubmitEditedCard
                                                        { id = card.id
                                                        , title = cardForm.title
                                                        , phrase = cardForm.phrase
                                                        , translation = cardForm.translation
                                                        }
                                                ]
                                                [ text "save" ]
                                    ]
                                ]

                        Nothing ->
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
                                        b [] [ text "Could add new card" ]
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
