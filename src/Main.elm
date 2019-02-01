import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button



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
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL
type alias Model =
  { name : String
  , nameValidation : RequiredStatus
  , studentNumber : String
  , studentValidation : RequiredStatus
  , mainSubject : String
  , subjectValidation : RequiredStatus
  , supervisors : String
  , supervisorsValidation : RequiredStatus
  , title : String
  , titleValidation : RequiredStatus
  , researchQuestion : String
  , researchQuestionValidation : RequiredStatus
  , summary : String -- 200 words,
  , summaryValidation : SummaryStatus
  , shortBio : String
  , bioValidation : BioStatus
  , showErrors : Bool
  , ready : Bool
  }

type RequiredStatus
 = RequiredEmpty
 | RequiredValid

type SummaryStatus
 = EmptySummary 
 | ValidSummary
 | TooLongSummary
 | TooShortSummary

type BioStatus
 = EmptyBio
 | ValidBio
 | TooLongBio
 | TooShortBio


requiredError : RequiredStatus -> Html msg
requiredError status =
    case status of
        RequiredValid ->
            Alert.simpleSuccess [] [ text "ok" ]

        RequiredEmpty ->
            Alert.simpleWarning [] [ text "this field is required" ]

summaryError : SummaryStatus -> Html msg
summaryError status =
    case status of
        ValidSummary ->
          Alert.simpleSuccess [] [ text "ok" ]

        EmptySummary ->
          Alert.simpleWarning [] [ text "this field is required" ]

        TooLongSummary ->
          Alert.simpleWarning [] [ text "too long, needs to be less than 500 words" ]

        TooShortSummary ->
          Alert.simpleWarning [] [ text "summary is too short" ]

bioError : BioStatus -> Html msg
bioError status =
    case status of
        ValidBio ->
          Alert.simpleSuccess [] [ text "ok" ]

        EmptyBio ->
          Alert.simpleWarning [] [ text "this field is required" ]    

        TooLongBio ->
          Alert.simpleWarning [] [ text "biography should be only 200 words" ]

        TooShortBio ->
          Alert.simpleWarning [] [ text "biography is too short" ]



init : Model
init =
  {
    name = ""
    ,nameValidation = RequiredEmpty
    ,studentNumber = ""
    ,studentValidation = RequiredEmpty
    ,mainSubject = ""
    ,subjectValidation = RequiredEmpty
    ,supervisors = ""
    ,supervisorsValidation = RequiredEmpty
    ,title = ""
    ,titleValidation = RequiredEmpty
    ,researchQuestion = ""
    ,researchQuestionValidation = RequiredEmpty
    ,summary = "" -- 200 words,
    ,summaryValidation = EmptySummary
    ,shortBio = ""
    ,bioValidation = EmptyBio
    ,showErrors = True
    ,ready = False
  }



-- UPDATE

type Msg 
 = Name String
 | StudentNumber String
 | MainSubject String
 | Supervisors String
 | Title String
 | ResearchQuestion String
 | Summary String -- 200 words
 | ShortBio String
 | Submit


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      validate { model | name = name }

    StudentNumber num -> 
      validate { model  | studentNumber = num }

    MainSubject subj ->
      validate { model | mainSubject = subj }

    Supervisors supr ->
      validate { model | supervisors = supr }

    Title tit ->
      validate { model | title = tit }

    ResearchQuestion qst ->
      validate { model | researchQuestion = qst }

    Summary sum -> 
      validate { model | summary = sum }

    ShortBio bio ->
      validate { model | shortBio = bio }

    Submit ->
      validate { model | showErrors = True }


wordCount : String -> Int
wordCount text =
  String.words text |> List.length


view : Model -> Html Msg
view model =
  Grid.container []
  [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Grid.row []
            [ Grid.col [] [(viewForm model)]
            ]
        ]

showError : Model -> Html Msg -> Html Msg
showError model errorMessage = 
  if model.showErrors then
    errorMessage
  else 
    span [] []

viewForm : Model -> Html Msg
viewForm model =
  Form.form [] 
    [
     h1 [] [text "KonCon Research Abstract form"]
     , viewInput "text" "Name" model.name Name (requiredError model.nameValidation |> showError model) 
     , viewInput "text" "Student Number (for example: C123456)" model.studentNumber StudentNumber (requiredError model.studentValidation |> showError model) 
     , viewInput "text" "Main Subject" model.mainSubject MainSubject (requiredError model.subjectValidation |> showError model) 
     , viewInput "text" "Supervisors" model.supervisors Supervisors (requiredError model.supervisorsValidation |> showError model) 
     , viewInput "text" "Title" model.title Title (requiredError model.titleValidation |> showError model) 
     , viewInput "text" "Research Question" model.researchQuestion ResearchQuestion (requiredError model.researchQuestionValidation |> showError model) 
     , viewTextarea "Summary (max 250 words)" model.summary Summary (summaryError model.summaryValidation |> showError model)
     , p [] [text ("word count: " ++ (model.summary |> wordCount |> String.fromInt))]
     , viewTextarea "Short Bio (max 100 words)" model.shortBio ShortBio (bioError model.bioValidation |> showError model) 
     , p [] [text ("word count: " ++ (model.shortBio |> wordCount |> String.fromInt))]
     , br [] []
     , button [ style "color" "green" , onClick Submit , disabled (not model.ready) ] [text ((\mdl -> if mdl.ready then "yes" else "no") model)]
     , Button.button [ 
         if model.ready then Button.primary else Button.danger 
         , Button.onClick Submit
         , Button.disabled (not model.ready) ] 
         [text (if model.ready then "Submit" else "Please fill in all fields")]
     ]

makeLabelName : String -> String
makeLabelName placeholderText = 
  String.replace " " "-" placeholderText

viewInput : String -> String -> String -> (String -> msg) -> Html msg -> Html msg
viewInput t p v toMsg errorInfo =
  let id = makeLabelName p in
    Form.group [ ] [
      Form.label [ for id ] [ text p ] 
      ,Input.text [ Input.id id , Input.placeholder "type here", Input.value v, Input.onInput toMsg ] 
      ,errorInfo 
    ]

viewTextarea : String -> String -> (String -> msg) -> Html msg -> Html msg
viewTextarea p v toMsg errorInfo =
  Form.group [ ] [
    Form.label [ ] [
      text p
      ,Textarea.textarea [ Textarea.rows 10, Textarea.value v, Textarea.onInput toMsg ],
      errorInfo
    ]
  ]

validate model =
  { model | 
    nameValidation = checkRequired model.name,
    studentValidation = checkRequired model.studentNumber,
    subjectValidation = checkRequired model.mainSubject,
    supervisorsValidation = checkRequired model.supervisors,
    titleValidation = checkRequired model.title,
    summaryValidation = validateSummary model,
    bioValidation = validateBio model,
    ready = isReady model
  }

isReady : Model -> Bool
isReady model =
  model.nameValidation == RequiredValid &&
  model.studentValidation == RequiredValid &&
  model.subjectValidation == RequiredValid &&
  model.supervisorsValidation == RequiredValid &&
  model.titleValidation == RequiredValid &&
  model.summaryValidation == ValidSummary &&
  model.bioValidation == ValidBio


validateSummary : Model -> SummaryStatus 
validateSummary model =
  let 
    summary = model.summary 
    length = wordCount model.summary
  in
    if summary == "" then
      EmptySummary
    else if length > 250 then
      TooLongSummary
    else if length < 150 then
      TooShortSummary
    else 
      ValidSummary

validateBio : Model -> BioStatus 
validateBio model =
  let
    bio = model.shortBio
    length = wordCount model.shortBio
  in
    if bio == "" then
      EmptyBio
    else if length > 100 then
      TooLongBio
    else if length < 50 then
      TooShortBio
    else
      ValidBio


checkRequired : String -> RequiredStatus 
checkRequired input =
  if input == "" then
    RequiredEmpty
  else 
    RequiredValid


inputStyle =
    [ ( "display", "block" )
    , ( "color", "#111" )
    , ( "padding", "10px 10px" )
    ]


errorStyle =
    [ ( "color", "red" ) ]


buttonStyleReady =
    [ ( "border-width", "0" )
    , ( "border-radius", "2px" )
    , ( "background-color", "#379CFF" )
    , ( "width", "100%" )
    , ( "color", "white" )
    , ( "padding", "8px 10px" )
    ]


empty =
    text ""