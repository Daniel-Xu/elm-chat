module App exposing (..)

import Html exposing (Html, text, div, img, input, button, form, p, span)
import Html.Attributes exposing (src, value, class, disabled)
import Html.Events exposing (onInput, onClick, onSubmit)
import Phoenix
import Phoenix.Socket as Socket
import Phoenix.Channel as Channel
import Phoenix.Push as Push
import Json.Decode as JD
import Json.Encode as JE
import Debug


type alias Model =
    { messages : List Message
    , newMessage : String
    , state : State
    , userName : String
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( { newMessage = "", messages = [], userName = "", state = LeftLobby }, Cmd.none )


type Msg
    = NoOp
    | SetNewMessage String
    | SetUserName String
    | UpdateState State
    | SendMsg
    | NewMsg JD.Value


type State
    = LeftLobby
    | LeavingLobby
    | JoinedLobby
    | JoiningLobby


type alias Message =
    { user : String
    , body : String
    }


socketUrl userName =
    "ws://localhost:4000/socket/websocket?username=" ++ userName


socketInit userName =
    (Socket.init << socketUrl) userName


lobby userName =
    Channel.init "room:lobby"
        |> Channel.on "new:msg" NewMsg
        |> Channel.onJoin (\_ -> UpdateState JoinedLobby)
        |> Channel.onLeave (\_ -> UpdateState LeftLobby)
        |> Channel.withDebug


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetNewMessage msg ->
            ( { model | newMessage = msg }, Cmd.none )

        SetUserName name ->
            ( { model | userName = name }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        UpdateState newState ->
            ( { model | state = newState }, Cmd.none )

        SendMsg ->
            let
                payload =
                    JE.object
                        [ ( "body", JE.string model.newMessage )
                        ]

                push =
                    Push.init "room:lobby" "new:msg"
                        |> Push.withPayload payload
            in
                ( { model | newMessage = "" }, Phoenix.push (socketUrl model.userName) push )

        NewMsg message ->
            case JD.decodeValue messageDecoder message of
                Ok msg ->
                    ( { model | messages = (List.append model.messages [ msg ]) }, Cmd.none )

                Err err ->
                    ( model, Cmd.none )



-- Decoder


messageDecoder =
    JD.map2 Message
        (JD.oneOf
            [ JD.field "user" JD.string
            , JD.succeed "anonyous"
            ]
        )
        (JD.field "body" JD.string)



-- View


messagesView model =
    div [] (List.map messageView model.messages)


messageView message =
    div [ class "message" ]
        [ span [ class "user" ] [ text (message.user ++ ": ") ]
        , span [ class "body" ] [ text message.body ]
        ]


messageInputView model =
    form [ onSubmit SendMsg ]
        [ input [ onInput SetNewMessage, value model.newMessage ] []
        ]


lobbyEntry model =
    case model.state of
        JoiningLobby ->
            button [ disabled True ] [ text "joining channel" ]

        JoinedLobby ->
            button [ onClick (UpdateState LeavingLobby) ] [ text "Leave Lobby" ]

        LeavingLobby ->
            button [ disabled True ] [ text "leaving lobby" ]

        LeftLobby ->
            button [ onClick (UpdateState JoiningLobby) ] [ text "join channel" ]


chatInterfaceView model =
    div []
        [ messagesView model
        , lobbyEntry model
        , messageInputView model
        ]


userNameView model =
    form [ onSubmit (UpdateState JoiningLobby) ]
        [ input [ onInput SetUserName, value model.userName ] []
        ]


view : Model -> Html Msg
view model =
    case model.state of
        JoiningLobby ->
            userNameView model

        JoinedLobby ->
            chatInterfaceView model

        LeavingLobby ->
            chatInterfaceView model

        LeftLobby ->
            userNameView model


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        socket =
            socketInit model.userName

        connect =
            Phoenix.connect socket
    in
        case model.state of
            JoinedLobby ->
                connect [ lobby model.userName ]

            JoiningLobby ->
                connect [ lobby model.userName ]

            _ ->
                connect []
