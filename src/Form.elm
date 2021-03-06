module Form exposing (ContentType(..), FormField, Model, Msg(..), Validity(..), buttonStyleReady, empty, encodeField, encodeFields, errorStyle, formToInput, init, inputStyle, isValid, main, makeField, makeLabelName, onlyWordFields, submitAll, submitAsRequest, subscriptions, update, updateFieldList, validateField, validateModel, view, viewForm, viewInput, viewTextarea, viewValidity)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode as Encode



-- MAIN
-- VIEW
--The abstracts must be in English and must contain the
--following information:
--1.
--Your name;
--2.
--Main subject;
--3.
--Name of research supervisor(s);
--4.
--Title of research;
--5.
--Research question;
--6.
--Summary of the results of the research and an explanation of
--the chosen format of presentation (max. 250 words);
--7.
--Short biography (max. 100 words)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type ContentType
    = Chars
    | Words


type alias FormField =
    { name : String
    , min : Int
    , max : Int
    , content : String
    , contentType : ContentType
    , validity : Validity
    }



-- createMinValidator : Int -> (String -> Validity)
-- createMinValidator min =
--     \string ->
--         let
--             n =
--                 String.length string
--         in
--         if n < min then
--             TooShort n min
--         else
--             Valid min


makeField : String -> Int -> Int -> ContentType -> FormField
makeField name min max contentType =
    { name = name
    , min = min
    , max = max
    , content = ""
    , contentType = contentType
    , validity = Pristine
    }


type Validity
    = Empty Int
    | Valid Int Int
    | TooLong Int Int
    | TooShort Int Int
    | Pristine


validateField : FormField -> FormField
validateField field =
    let
        counter =
            case field.contentType of
                Chars ->
                    String.length

                Words ->
                    \s -> String.words s |> List.length |> (+) -1
    in
    let
        n =
            counter field.content
    in
    if n == 0 then
        case field.validity of
            Pristine ->
                field

            _ ->
                { field | validity = Empty field.max }

    else if n < field.min then
        { field | validity = TooShort n field.min }

    else if n > field.max then
        { field | validity = TooLong n field.max }

    else
        { field | validity = Valid n field.max }


type alias Model =
    { fields : List FormField
    , showErrors : Bool
    , ready : Bool
    , submitted : Bool
    , result : String
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


viewValidity : FormField -> Html Msg
viewValidity field =
    let
        contentType =
            case field.contentType of
                Chars ->
                    "character"

                Words ->
                    "word"
    in
    case field.validity of
        Pristine ->
            Alert.simpleWarning [] [ text "this field is required" ]

        Empty _ ->
            Alert.simpleWarning [] [ text "this field is required" ]

        TooShort n min ->
            Alert.simpleWarning [] [ text (field.name ++ " too short, current " ++ contentType ++ " count " ++ String.fromInt n ++ ", minimum is " ++ String.fromInt min) ]

        TooLong n max ->
            Alert.simpleWarning [] [ text (field.name ++ " too long, current " ++ contentType ++ " count " ++ String.fromInt n ++ ", maximum is " ++ String.fromInt max) ]

        Valid _ _ ->
            span [] []


init : () -> ( Model, Cmd Msg )
init _ =
    ( { fields =
            --[ FormField "Student Name" 4 80 "Casper Schipper" Chars
            --, FormField "Student Number" 6 8 "C123456" Chars
            --, FormField "Main Subject" 4 80 "Sonology" Chars
            --, FormField "Supervisors" 6 120 "Supervisor1 Supervisor2" Chars
            --, FormField "Title" 4 200 "My fantastic title yay" Chars
            --, FormField "Research Question" 6 200 "Research question" Chars
            --, FormField "Summary" 150 250 "A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish." Words
            --, FormField "Short Bio" 50 100 "A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is n" Words
            [ makeField "Student Name" 4 80 Chars
            , makeField "Student Number" 6 8 Chars
            , makeField "Main Subject" 4 80 Chars
            , makeField "Supervisors" 6 120 Chars
            , makeField "Title" 5 1000 Chars
            , makeField "Research Question" 5 1000 Chars
            , makeField "Summary" 125 250 Words
            , makeField "Short Bio" 50 100 Words
            ]
      , showErrors = False
      , ready = False
      , submitted = False
      , result = "Please fill in all fields before submitting. The \"show problems\" button will highlight the fields that are missing or incorrect."
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateField String String
    | Submit
    | SubmitResult (Result Http.Error String)
    | Clean
    | ShowErrors


submitAll : Model -> Cmd Msg
submitAll model =
    let
        json =
            encodeFields model.fields
    in
    Http.post
        { url = "submit_kc_form.php"
        , body = Http.jsonBody json
        , expect = Http.expectString SubmitResult
        }


submitAsRequest : Model -> Cmd Msg
submitAsRequest model =
    let
        json =
            encodeFields model.fields
    in
    Http.request
        { method = "POST"
        , headers = []
        , url = "submit_kc_form.php"
        , body = Http.jsonBody json
        , expect = Http.expectString SubmitResult
        , timeout = Nothing
        , tracker = Nothing
        }


encodeFields : List FormField -> Encode.Value
encodeFields fields =
    Encode.object <| List.map encodeField fields


encodeField : FormField -> ( String, Encode.Value )
encodeField field =
    ( field.name, Encode.string field.content )


updateFieldList : List FormField -> String -> String -> List FormField
updateFieldList fieldList fieldName text =
    case fieldList of
        field :: rest ->
            if field.name == fieldName then
                validateField { field | content = text } :: rest

            else
                field :: updateFieldList rest fieldName text

        [] ->
            []


onlyWordFields : List FormField -> List FormField
onlyWordFields fields =
    List.filter
        (\field ->
            case field.contentType of
                Words ->
                    True

                Chars ->
                    False
        )
        fields


isValid : FormField -> Bool
isValid field =
    case field.validity of
        Valid _ _ ->
            True

        _ ->
            False


validateModel : Model -> Model
validateModel model =
    { model
        | ready = model.fields |> List.all isValid
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateField fieldName text ->
            ( validateModel { model | fields = updateFieldList model.fields fieldName text }, Cmd.none )

        Submit ->
            let
                command =
                    if model.ready then
                        submitAsRequest model

                    else
                        Cmd.none
            in
            ( validateModel { model | showErrors = True }, command )

        ShowErrors ->
            ( validateModel { model | showErrors = True }, Cmd.none )

        SubmitResult result ->
            case result of
                Ok message ->
                    ( { model | submitted = True, result = "Thank you!" }, Cmd.none )

                Err httpError ->
                    case httpError of
                        Http.BadUrl url ->
                            ( { model | result = "badurl -> contact Casper" ++ url }, Cmd.none )

                        Http.Timeout ->
                            ( { model | result = "timeout -> contact Casper" }, Cmd.none )

                        Http.NetworkError ->
                            ( { model | result = "connection -> contact Casper" }, Cmd.none )

                        Http.BadStatus badStatus ->
                            ( { model | result = "bad status -> contact Casper" ++ String.fromInt badStatus }, Cmd.none )

                        Http.BadBody body ->
                            ( { model | result = "json body is not ok -> contact Casper" ++ body }, Cmd.none )

        Clean ->
            ( model, Cmd.none )


localStyles : Html Msg
localStyles =
    node "link"
        [ rel "stylesheet"
        , href "styles.css"
        ]
        []


view : Model -> Html Msg
view model =
    let
        statusText =
            if not model.ready then
                model.result

            else if model.submitted then
                "Your abstract has been received, thank you!"

            else
                "All good, please submit"
    in
    Grid.container [ style "margin-top" "100px" ]
        [ localStyles
        , CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Grid.row []
            [ Grid.col [] [ viewForm model ]
            ]
        , Grid.row []
            [ Grid.col []
                [ p [] [ text statusText ]
                ]
            ]
        ]


formToInput : Bool -> FormField -> Html Msg
formToInput showErrors field =
    let
        validity =
            if showErrors then
                viewValidity field

            else
                empty

        -- only show errors once tried to submit
    in
    case field.contentType of
        Chars ->
            viewInput (not showErrors) field.validity field.name field.content (UpdateField field.name) validity

        Words ->
            viewTextarea (not showErrors) field.validity field.name field.content (UpdateField field.name) validity


viewForm : Model -> Html Msg
viewForm model =
    Form.form []
        (List.append
            [ h1 [] [ text "KonCon Research Abstract form" ] ]
            (List.append
                (List.map (formToInput model.showErrors) model.fields)
                [ br [] []
                , if not model.ready then
                    Button.button [ Button.attrs [ Spacing.ml1 ], Button.warning, Button.onClick ShowErrors ] [ text "show problems !" ]

                  else
                    Button.button
                        [ Button.primary
                        , Button.onClick Submit
                        ]
                        [ text "submit" ]
                ]
            )
        )


makeLabelName : String -> String
makeLabelName placeholderText =
    String.replace " " "-" placeholderText


viewInput : Bool -> Validity -> String -> String -> (String -> msg) -> Html msg -> Html msg
viewInput showHelper validity placeholder value toMsg errorInfo =
    let
        counter =
            case validity of
                Empty max ->
                    "max " ++ String.fromInt max ++ " characters"

                Valid current max ->
                    String.fromInt (max - current) ++ " remaining"

                TooLong current desired ->
                    String.fromInt (desired - current) ++ " remaining"

                _ ->
                    ""

        counterText =
            span [ class "char-counter" ] [ text counter ]

        id =
            makeLabelName placeholder
    in
    Form.group []
        [ Form.label [ for id ] [ text placeholder ]
        , Input.text [ Input.id id, Input.placeholder "type here", Input.value value, Input.onInput toMsg ]
        , if showHelper then
            counterText

          else
            span [] []
        , errorInfo
        ]


viewTextarea : Bool -> Validity -> String -> String -> (String -> msg) -> Html msg -> Html msg
viewTextarea showHelper validity placeholder value toMsg errorInfo =
    let
        counter =
            case validity of
                Empty max ->
                    "max " ++ String.fromInt max ++ " words"

                Valid current max ->
                    String.fromInt (max - current) ++ " remaining"

                TooLong current desired ->
                    String.fromInt (current - desired) ++ " too many"

                TooShort current desired ->
                    String.fromInt current ++ " words, need more (" ++ String.fromInt desired ++ ")"

                _ ->
                    ""

        counterText =
            span [] [ text counter ]

        id =
            makeLabelName placeholder
    in
    Form.group []
        [ Form.label [ for id ] [ text placeholder ]
        , Textarea.textarea [ Textarea.id placeholder, Textarea.rows 10, Textarea.value value, Textarea.onInput toMsg ]
        , counterText
        , errorInfo
        ]


inputStyle : List ( String, String )
inputStyle =
    [ ( "display", "block" )
    , ( "color", "#111" )
    , ( "padding", "10px 10px" )
    ]


errorStyle : List ( String, String )
errorStyle =
    [ ( "color", "red" ) ]


buttonStyleReady : List ( String, String )
buttonStyleReady =
    [ ( "border-width", "0" )
    , ( "border-radius", "2px" )
    , ( "background-color", "#379CFF" )
    , ( "width", "100%" )
    , ( "color", "white" )
    , ( "padding", "8px 10px" )
    ]


empty : Html msg
empty =
    text ""
