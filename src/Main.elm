port module Main exposing (Model, Msg(..), init, main, update, view)

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


type User
    = Unknown
    | LoggedOut
    | LoginError
    | LoggedIn UserData


type alias Card =
    { id : String
    , title : String
    , phrase : String
    , translation : String
    }


type Cards
    = GetCardsFailed
    | SaveEditedCardFailed
    | CardsList (List Card)


type alias Model =
    { user : User
    , editedCard : Maybe Card
    , cards : Cards
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { user = Unknown
      , cards = CardsList []
      , editedCard = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = LogIn
    | LogOut
    | LoggedInData (Result Json.Decode.Error UserData)
    | LoggedInError
    | SaveEditedCardError
    | CardsReceived (Result Json.Decode.Error (List Card))
    | CardsReceivedError
    | StartEditingCard Card
    | StopEditingCard
    | SubmitEditedCard Card
    | EditTitle String
    | EditPhrase String
    | EditTranslation String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LogIn ->
            ( model, signIn () )

        LogOut ->
            ( { model | user = LoggedOut }, signOut () )

        LoggedInData (Ok userData) ->
            ( { model | user = LoggedIn userData }, Cmd.none )

        LoggedInData (Err _) ->
            ( { model | user = LoginError }, Cmd.none )

        LoggedInError ->
            ( { model | user = LoginError }, Cmd.none )

        SaveEditedCardError ->
            ( { model | cards = SaveEditedCardFailed }, Cmd.none )

        CardsReceived (Ok cards) ->
            ( { model | cards = CardsList cards }, Cmd.none )

        CardsReceived (Err _) ->
            ( { model | cards = GetCardsFailed }, Cmd.none )

        CardsReceivedError ->
            ( { model | cards = GetCardsFailed }, Cmd.none )

        StartEditingCard card ->
            ( { model | editedCard = Just card }, Cmd.none )

        StopEditingCard ->
            ( { model | editedCard = Nothing }, Cmd.none )

        SubmitEditedCard card ->
            case model.user of
                LoggedIn user ->
                    ( { model | editedCard = Nothing }, saveEditedCard <| editedCardEncoder card user.uid )

                _ ->
                    ( model, Cmd.none )

        EditTitle title ->
            case model.editedCard of
                Just prevEditedCard ->
                    let
                        newEditedCard =
                            { prevEditedCard | title = title }
                    in
                    ( { model | editedCard = Just newEditedCard }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditPhrase phrase ->
            case model.editedCard of
                Just prevEditedCard ->
                    let
                        newEditedCard =
                            { prevEditedCard | phrase = phrase }
                    in
                    ( { model | editedCard = Just newEditedCard }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditTranslation translation ->
            case model.editedCard of
                Just prevEditedCard ->
                    let
                        newEditedCard =
                            { prevEditedCard | translation = translation }
                    in
                    ( { model | editedCard = Just newEditedCard }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


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
            , button [ onClick (StartEditingCard card) ] [ text "edit" ]
            ]
        ]


viewKeyedCard : Card -> ( String, Html Msg )
viewKeyedCard card =
    ( card.id, lazy viewCard card )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ css [ padding (em 1) ] ]
        [ h1 [] [ text "Kärtchen" ]
        , case model.user of
            Unknown ->
                text "Loading user info..."

            LoggedOut ->
                button [ onClick LogIn ] [ text "Login with Google" ]

            LoginError ->
                text "error obtaining user info"

            LoggedIn userData ->
                div []
                    [ p [] [ text <| "Hello, " ++ formatUserName userData ++ "!" ]
                    , button [ onClick LogOut ] [ text "Logout" ]
                    , case model.editedCard of
                        Just card ->
                            div [ css [ displayFlex, flexDirection column ] ]
                                [ p [] [ text "Edit card:" ]
                                , label []
                                    [ text "title: "
                                    , input [ value card.title, onInput EditTitle ] []
                                    ]
                                , label []
                                    [ text "phrase: "
                                    , input [ value card.phrase, onInput EditPhrase ] []
                                    ]
                                , label []
                                    [ text "translation: "
                                    , input [ value card.translation, onInput EditTranslation ] []
                                    ]
                                , div []
                                    [ button [ onClick StopEditingCard ] [ text "cancel" ]
                                    , button [ onClick <| SubmitEditedCard card ] [ text "save" ]
                                    ]
                                ]

                        Nothing ->
                            case model.cards of
                                CardsList cards ->
                                    ul [ css [ padding (px 0) ] ] <| List.map viewKeyedCard cards

                                SaveEditedCardFailed ->
                                    b [] [ text "Could not save edited card" ]

                                GetCardsFailed ->
                                    b [] [ text "Could not get cards" ]
                    ]
        ]



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ signInInfo <| Json.Decode.decodeValue userDataDecoder >> LoggedInData
        , signInError <| always LoggedInError
        , loggedOutUser <| always LogOut
        , saveEditedCardError <| always SaveEditedCardError
        , receiveCards <| Json.Decode.decodeValue cardsListDecoder >> CardsReceived
        , receiveCardsError <| always CardsReceivedError
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
