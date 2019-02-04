import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode as Encode
import Debug

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
    }


makeField : String -> Int -> Int -> ContentType -> FormField
makeField name min max contentType =
    { name = name
    , min = min
    , max = max
    , content = ""
    , contentType = contentType
    }

type Validity
    = Empty
    | Valid
    | TooLong
    | TooShort


validateField : FormField -> Validity
validateField field =
    let
        counter =
            case field.contentType of
                Chars ->
                    String.length

                Words ->
                    \s -> String.words s |> List.length
    in
    let
        n =
            counter field.content
    in
    if n == 0 then
        Empty

    else if n < field.min then
        TooShort

    else if n > field.max then
        TooLong

    else
        Valid


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
    case validateField field of
        Empty ->
            Alert.simpleWarning [] [ text "this field is required" ]

        TooShort ->
            Alert.simpleWarning [] [ text (field.name ++ " is too short") ]

        TooLong ->
            Alert.simpleWarning [] [ text (field.name ++ " is too long") ]

        Valid ->
            Alert.simpleSuccess [] [ text "ok" ]


init : () -> ( Model, Cmd Msg )
init _ =
    ({ fields =
        [ FormField "Student Name" 4 80 "Casper Schipper" Chars
        , FormField "Student number" 6 8 "C123456" Chars
        , FormField "Main Subject" 4 80 "Sonology" Chars
        , FormField "Supervisors" 6 120 "Supervisor1 Supervisor2" Chars
        , FormField "Title" 4 200 "My fantastic title yay" Chars 
        , FormField "Research Question" 6 200 "Research question" Chars 
        , FormField "Summary" 150 250 "A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish." Words
        , FormField "Short Bio" 50 100 "A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is not a fish.A fish is n" Words
        --  makeField "Student Name" 4 80 Chars
        --, makeField "Student Number" 6 8 Chars
        --, makeField "Main Subject" 4 80 Chars
        --, makeField "Supervisors" 6 80 Chars
        --, makeField "Title" 4 70 Chars
        --, makeField "Research Question" 6 150 Chars
        --, makeField "Summary" 150 250 Words
        --, makeField "Short Bio" 50 100 Words
        ]
    , showErrors = False   
    , ready = False
    , submitted = False
    , result = "nothing yet"
    }, Cmd.none)



-- UPDATE


type Msg
    = UpdateField String String
    | Submit
    | SubmitResult (Result Http.Error String)
    | Clean

submitAll : Model -> Cmd Msg
submitAll model =
    let 
        json = encodeFields model.fields
    in 
        Http.post {
            url = "submit_kc_form.php"
            , body = Http.jsonBody json
            , expect = Http.expectString SubmitResult
        }

submitAsRequest : Model -> Cmd Msg
submitAsRequest model =
    let 
        json = encodeFields model.fields
    in
        Http.request
                { method = "POST"
                , headers = []
                , url = "submit_kc_form.php"
                , body = Http.jsonBody json
                , expect = Http.expectString SubmitResult
                , timeout = Nothing
                , tracker = Nothing }
            
encodeFields : List FormField -> Encode.Value
encodeFields fields =
    Encode.list encodeField fields

encodeField : FormField -> Encode.Value 
encodeField field =
    Encode.object [
        (field.name, Encode.string field.content)
    ]

updateFieldList : List FormField -> String -> String -> List FormField
updateFieldList fieldList fieldName text =
    case fieldList of
        field :: rest ->
            if field.name == fieldName then
                { field | content = text } :: rest

            else
                field :: updateFieldList rest fieldName text

        [] ->
            []


validateModel : Model -> Model
validateModel model =
    { model
        | ready =
            List.map validateField model.fields
                |> List.all
                    (\val ->
                        case val of
                            Valid ->
                                True

                            _ ->
                                False
                    )
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UpdateField fieldName text ->
            (validateModel { model | fields = updateFieldList model.fields fieldName text }, Cmd.none)

        Submit ->
            (validateModel { model | showErrors = True, result = "something is submitted?" }, submitAsRequest model)

        SubmitResult result -> 
            case result of
                Ok message -> 
                    ({ model | submitted = True, result = message }, Cmd.none)
                
                Err httpError ->
                    case httpError of
                        Http.BadUrl url -> 
                            ({ model | result = "badurl" ++ url }, Cmd.none)
                        
                        Http.Timeout ->
                            ({ model | result = "Timeout" }, Cmd.none)

                        Http.NetworkError ->
                            ({ model | result = "Connection is dead" }, Cmd.none )

                        Http.BadStatus badStatus ->
                            ({ model | result = "bad status :" ++ (String.fromInt badStatus) }, Cmd.none)

                        Http.BadBody body ->
                            ({ model | result = "json body is not ok" ++ body }, Cmd.none)

        Clean ->
            (model,Cmd.none)



view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Grid.row []
            [ Grid.col [] [ viewForm model ]
            ]
        , Grid.row []
            [ Grid.col [] [
                p [] [text model.result]
            ]
            ]
        ]

formToInput : Bool -> FormField -> Html Msg
formToInput showErrors field =
    let validity = if showErrors then (viewValidity field) else empty -- only show errors once tried to submit
    in 
        case field.contentType of
            Chars ->
                viewInput field.name field.content (UpdateField field.name) validity

            Words ->
                viewTextarea field.name field.content (UpdateField field.name) validity


viewForm : Model -> Html Msg
viewForm model =
    Form.form []
        (List.append
            [ h1 [] [ text "KonCon Research Abstract form" ] ]
            (List.append
                (List.map (formToInput model.showErrors) model.fields)
                [ br [] []
                , Button.button
                    [ if model.ready then
                        Button.primary

                      else
                        Button.danger
                    , Button.onClick Submit
                    , Button.disabled False
                    ]
                    [ text
                        (if model.ready then
                            "Submit"

                         else
                            "Show missing fields"
                        )
                    ]
                ]
            )
        )


makeLabelName : String -> String
makeLabelName placeholderText =
    String.replace " " "-" placeholderText

viewInput : String -> String -> (String -> msg) -> Html msg -> Html msg
viewInput placeholder value toMsg errorInfo =
    let
        id =
            makeLabelName placeholder
    in
    Form.group []
        [ Form.label [ for id ] [ text placeholder ]
        , Input.text [ Input.id id, Input.placeholder "type here", Input.value value, Input.onInput toMsg ]
        , errorInfo
        ]


viewTextarea : String -> String -> (String -> msg) -> Html msg -> Html msg
viewTextarea placeholder value toMsg errorInfo =
    let 
        id = 
            makeLabelName placeholder 
    in
    Form.group []
    [ Form.label [for id] [text placeholder]
    , Textarea.textarea [ Textarea.id placeholder, Textarea.rows 10, Textarea.value value, Textarea.onInput toMsg ]
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





