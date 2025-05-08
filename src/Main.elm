port module Main exposing (..)

import Browser
import DataAliasDict exposing (aliasDict)
import DataNotInExomeList exposing (notInExomeList)
import DataValidSymbols exposing (validSymbols)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Set



---- MODEL ----


type SymbolResult
    = ValidSymbol
    | MatchedWith String
    | NotInExomeList
    | Unmatched


isValidSymbol : SymbolResult -> Bool
isValidSymbol symbolResult =
    case symbolResult of
        ValidSymbol ->
            True

        _ ->
            False


type alias ProcessedSymbol =
    { pastedString : String
    , symbolResult : SymbolResult
    }


type alias Model =
    { pasteBox : String
    , processedSymbols : List ProcessedSymbol
    , numSymbols : Int
    , numValidSymbols : Int
    , numMatchedSymbols : Int
    , matchedSymbols : List ( String, String )
    , numNotInExomeList : Int
    , notInExomeList : List String
    , numUnmatchedSymbols : Int
    , unmatchedSymbols : List String
    , showCopiedMessage : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { pasteBox = ""
      , processedSymbols = []
      , numSymbols = 0
      , numValidSymbols = 0
      , numMatchedSymbols = 0
      , matchedSymbols = []
      , numNotInExomeList = 0
      , notInExomeList = []
      , numUnmatchedSymbols = 0
      , unmatchedSymbols = []
      , showCopiedMessage = False
      }
    , Cmd.none
    )



---- UPDATE ----

-- port to copy to clipboard 
port copyToClipboard : String -> Cmd msg

processedSymbolListToClipboard : List ProcessedSymbol -> String
processedSymbolListToClipboard processedSymbols =
    let
        symbolList =
            List.map (\{ pastedString, symbolResult } -> case symbolResult of
                MatchedWith newSymbol ->
                    newSymbol

                _ ->
                    pastedString
            ) processedSymbols
    in
    String.join "\n" symbolList

type Msg
    = ChangedPastebox String
    | ClickedCopyButton


processSymbol : String -> SymbolResult
processSymbol symbolRaw =
    let
        symbol =
            String.toUpper symbolRaw
    in
    if Set.member symbol validSymbols then
        ValidSymbol

    else
        case Dict.get symbol aliasDict of
            Just alias ->
                MatchedWith alias

            Nothing ->
                if Set.member symbol notInExomeList then
                    NotInExomeList

                else
                    Unmatched


matchedSymbolToPair : ProcessedSymbol -> Maybe ( String, String )
matchedSymbolToPair { pastedString, symbolResult } =
    case symbolResult of
        MatchedWith alias ->
            Just ( pastedString, alias )

        _ ->
            Nothing


extractNotInExomeList : ProcessedSymbol -> Maybe String
extractNotInExomeList { pastedString, symbolResult } =
    case symbolResult of
        NotInExomeList ->
            Just pastedString

        _ ->
            Nothing


extractUnmatchedSymbol : ProcessedSymbol -> Maybe String
extractUnmatchedSymbol { pastedString, symbolResult } =
    case symbolResult of
        Unmatched ->
            Just pastedString

        _ ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedPastebox newPasteBox ->
            let
                trimmedPasteBox =
                    String.trim newPasteBox

                processedSymbols =
                    if String.isEmpty trimmedPasteBox then
                        []

                    else
                        String.split "\n" trimmedPasteBox
                            |> List.map (\symbol -> { pastedString = String.trim symbol, symbolResult = processSymbol (String.trim symbol) })

                numSymbols =
                    List.length processedSymbols

                numValidSymbols =
                    List.length (List.filter (.symbolResult >> isValidSymbol) processedSymbols)

                matchedSymbols =
                    List.filterMap matchedSymbolToPair processedSymbols

                numMatchedSymbols =
                    List.length matchedSymbols

                notInExomeSymbols =
                    List.filterMap extractNotInExomeList processedSymbols

                numNotInExomeList =
                    List.length notInExomeSymbols

                unmatchedSymbols =
                    List.filterMap extractUnmatchedSymbol processedSymbols

                numUnmatchedSymbols =
                    List.length unmatchedSymbols
            in
            ( { model
                | pasteBox = newPasteBox
                , processedSymbols = processedSymbols
                , numSymbols = numSymbols
                , numValidSymbols = numValidSymbols
                , numMatchedSymbols = numMatchedSymbols
                , matchedSymbols = matchedSymbols
                , numNotInExomeList = numNotInExomeList
                , notInExomeList = notInExomeSymbols
                , numUnmatchedSymbols = numUnmatchedSymbols
                , unmatchedSymbols = unmatchedSymbols
                , showCopiedMessage = False
              }
            , Cmd.none
            )

        ClickedCopyButton ->
            ( { model | showCopiedMessage = True }, copyToClipboard (processedSymbolListToClipboard model.processedSymbols) )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "left-container" ]
            [ h1 [ class "title" ] [ text "Quick Alias" ]
            , p [ class "instructions" ] [ text "Paste your gene symbols in the text box below (each symbol on a new line). Each symbol will be normalised to their alias in the IDT Exome v2 gene list. Any symbols which do not have an alias in the IDT Exome v2 gene list will also be identified." ]
            , textarea [ class "pastebox", value model.pasteBox, onInput ChangedPastebox, placeholder "e.g.\nTMEM5\nSEPT9\nISPD\nATP5\netc." ] []
            ]
        , div [ class "right-container" ]
            [ div [ class "inner-left-container" ] [ viewSummary model ]
            , div [ class "inner-right-container" ] [ viewProcessedSymbolTable model model.processedSymbols ]
            ]
        ]



-- geneSpan : String -> String -> Html Msg
-- geneSpan extraClass gene =
--     span [ class "gene-span", class extraClass ] [ a [ target "_blank", href <| "https://www.genecards.org/cgi-bin/carddisp.pl?gene=" ++ gene ] [ text gene ] ]


geneSpan : String -> String -> Html Msg
geneSpan _ gene =
    text gene


viewSummary : Model -> Html Msg
viewSummary model =
    let
        normalisedDiv =
            if List.isEmpty model.matchedSymbols then
                div [] []

            else
                div []
                    [ p [ class "paragraph" ]
                        [ span [class "strong"] [text "The following symbols were normalised to an alias in the IDT Exome v2 gene list: "]
                        , ul []
                            (List.map
                                (\( pastedString, alias ) ->
                                    li [] [ geneSpan "" pastedString, text " → ", geneSpan "" alias ]
                                )
                                model.matchedSymbols
                            )
                        ]
                    ]

        recognisedButNotInExomeDiv =
            if List.isEmpty model.notInExomeList then
                div [] []

            else
                div []
                    [ p [ class "paragraph" ]
                        [span [class "strong"]  [text "The following symbols were recognised but do not have an alias in the IDT Exome v2 gene list:"]
                        , br [] []
                        , text <| String.join ", " model.notInExomeList
                        ]
                    ]

        unmatchedDiv =
            if List.isEmpty model.unmatchedSymbols then
                div [] []

            else
                div []
                    [ p [ class "paragraph" ]
                        [ span [class "strong"] [text "The following symbols were not recognised as gene symbols (make sure there is no leading or trailing punctuation):"]
                        , br [] []
                        , text <| String.join ", " model.unmatchedSymbols
                        ]
                    ]
    in
    div [ class "summary-container" ]
        [ h1 [ class "heading" ] [ text "Paste Summary" ]
        , div [ class "summary-paragraph" ]
            [ p [ class "paragraph" ]
                [ text "A total of "
                , span [ class "strong" ] [ text (String.fromInt model.numSymbols) ]
                , text " symbols have been pasted."
                , text " Of these:"
                , ul []
                    [ li [] [ span [ class "strong" ] [ text (String.fromInt model.numValidSymbols) ], text " were already valid symbols in the IDT Exome v2 gene list; " ]
                    , li [] [ span [ class "strong" ] [ text (String.fromInt model.numMatchedSymbols) ], text " were recognised and normalised to an alias in the IDT Exome v2 gene list;" ]
                    , li [] [ span [ class "strong" ] [ text (String.fromInt model.numNotInExomeList) ], text " were recognised but did not have any aliases in the IDT Exome v2 gene list;" ]
                    , li [] [ span [ class "strong" ] [ text (String.fromInt model.numUnmatchedSymbols) ], text " were not recognised as gene symbols." ]
                    ]
                ]
            ]
        , normalisedDiv
        , recognisedButNotInExomeDiv
        , unmatchedDiv
        ]


viewProcessedSymbolTable : Model -> List ProcessedSymbol -> Html Msg
viewProcessedSymbolTable model processedSymbols =
    let
        copyMessage =
            if model.showCopiedMessage then
                div [ class "copy-message" ] [ text "Copied to clipboard!" ]

            else
                div [] []

        tableSection =
            if List.isEmpty processedSymbols then
                div [] []

            else
                table [ class "processed-symbol-table" ]
                    [ thead []
                        [ tr []
                            [ th [] [ text "Pasted Symbol" ]
                            , th [] [ text "Normalised" ]
                            ]
                        ]
                    , tbody []
                        (List.map viewProcessedSymbol processedSymbols)
                    ]
    in
    div [ class "processed-symbol-table-container" ]
        [ h1 [ class "heading" ] [ text "Processed Symbols" ]
        , button [ class "copy-button", onClick ClickedCopyButton ] [ text "Click to Copy Normalised Gene List to Clipboard", copyMessage ]
        , p [class "paragraph"] 
            [ text "Color key: "
            , br [] []
            , span [class "square valid-symbol"] [], text "= already normalised"
            , br [] []
            , span [class "square matched-symbol"] [], text "= normalised symbol"
            , br [] []
            , span [class "square not-in-exome-list"] [], text "= valid symbol but not in IDT exome"
            , br [] []
            , span [class "square unmatched-symbol"] [], text "= unrecognised gene symbol"
            ]
        , tableSection
        ]


viewProcessedSymbol : ProcessedSymbol -> Html Msg
viewProcessedSymbol { pastedString, symbolResult } =
    let
        ( resultClass, symbolString ) =
            case symbolResult of
                ValidSymbol ->
                    ( "valid-symbol", pastedString )

                MatchedWith newSymbol ->
                    ( "matched-symbol", newSymbol )

                NotInExomeList ->
                    ( "not-in-exome-list", pastedString )

                Unmatched ->
                    ( "unmatched-symbol", pastedString )
    in
    tr []
        [ td [ class "raw-cell" ] [ text pastedString ]
        , td [ class resultClass ] [ text symbolString ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none   
        }
