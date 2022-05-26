module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , aside
        , button
        , div
        , form
        , h1
        , header
        , i
        , input
        , label
        , li
        , main_
        , option
        , p
        , select
        , span
        , table
        , tbody
        , td
        , text
        , th
        , thead
        , tr
        , ul
        )

import Html.Attributes
    exposing
        ( attribute
        , for
        , href
        , id
        , placeholder
        , type_
        )

import Html.Events
    exposing
        ( onClick
        )

import Http
import Json.Decode
    exposing
        ( Decoder
        , andThen
        , dict        
        , nullable
        , field       
        , int
        , list
        , map2
        , map3
        , string
        )
import Svg as Svg
import Svg.Attributes as SvgAttr




-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

-- MODEL

type alias Model =
    { menu : List Item
    , modo : Crud
    , tabelas : Maybe (Dict String (Dict String Campo))
    , showMenu : Bool
    }



type Crud
    = Create
    | List


type Campo
    = Texto String (Maybe String)
    | Id Int


type alias Item =
    { label : String
    , selecionado : Int
    , subItens : List SubItem
    }


type alias SubItem =
    { label : String
    , link : String
    , selecionado : Int
    }


itemDecoder : Decoder Item
itemDecoder =
    map3 Item
        (field "label" string)
        (field "selecionado" int)
        (field "sub-itens" (list subItemDecoder))

campoDecoder =
    field "tipo" string
        |> andThen tipoDoCampo 
 
decoderCampoTexto =
    map2 Texto        
        (field "codinome" string)
        (field "value" (nullable string))   
 
tipoDoCampo : String -> Decoder Campo
tipoDoCampo tipo =
    case tipo of
        "texto" ->
            decoderCampoTexto

        _ ->
            Debug.todo "nenhum decoder"

subItemDecoder : Decoder SubItem
subItemDecoder =
    map3 SubItem
        (field "label" string)
        (field "link" string)
        (field "selecionado" int)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        []
        List
        Nothing
        True
    , getItens
    )


type Msg
    = GotMenu (Result Http.Error (List Item))
    | GotSchema (Result Http.Error (Dict String (Dict String Campo)))
    | Selecionar Item
    | SubSelecionado SubItem
    | MostrarMenu
    | AbrirModal
    | FecharModal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMenu result ->
            case result of
                Ok listItens ->
                    ( { model | menu = listItens }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        Selecionar item ->
            ( { model | menu = atualizarMenu model.menu item }, Cmd.none )

        GotSchema result ->
            case result of
                Ok tabelasOk ->
                    ( { model | tabelas = Just tabelasOk }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        SubSelecionado subitem ->
            ( model, getSchema subitem.link )

        MostrarMenu ->
            ( { model | showMenu = not model.showMenu }, Cmd.none )

        AbrirModal ->
            ( { model | modo = Create }, Cmd.none )

        FecharModal ->
            ( { model | modo = List }, Cmd.none )


atualizarMenu : List Item -> Item -> List Item
atualizarMenu menu item =
    substituirItem menu item


substituirItem : List Item -> Item -> List Item
substituirItem menu item =
    List.map (compararItem item) menu


compararItem : Item -> Item -> Item
compararItem item itemMsg =
    if item == itemMsg then
        { item | selecionado = item.selecionado * -1 }

    else
        itemMsg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- expandir e recolher menu


expandirMenu : Bool -> Attribute msg
expandirMenu cond =
    if cond then
        Html.Attributes.style "display" "block"

    else
        Html.Attributes.style "display" "none"


view : Model -> Html Msg
view model =
    div [ class "mx-auto bg-grey-400" ]
        [ div [ class "min-h-screen flex flex-col" ]
            [ topo
            , div [ class "flex flex-1" ]
                [ div [ class "flex" ] []
                , aside
                    [ class "bg-side-nav w-1/2 md:w-1/6 lg:w-1/6 border-r border-side-nav hidden md:block lg:block"
                    , expandirMenu model.showMenu
                    ]
                    [ viewMenu model.menu ]
                , main_ [ class "bg-white-500 flex-1 p-3 overflow-hidden" ]
                    [ div [ class "flex flex-col" ] [ tabela ] ]
                ]
            ]
        , modal model
        ]



--viewMenu model


viewMenu : List Item -> Html Msg
viewMenu menu =
    ul [ class "list-reset flex flex-col overscroll-contain" ] (List.map (\n -> liMenu n) menu)


gerarSubMenu : Item -> Html Msg
gerarSubMenu item =
    ul [ class "list-reset -mx-2 bg-white-medium-dark" ] (subItens item.subItens)


liClassSelecionado : number -> Attribute msg
liClassSelecionado selecionado =
    if selecionado > 0 then
        class "w-full h-full py-3 px-2"

    else
        class "w-full h-full py-3 px-2 border-b border-300-border"


subItens : List SubItem -> List (Html Msg)
subItens subitens =
    List.map
        (\n ->
            li [ class "border-t mt-2 border-light-border w-full h-full px-2 py-3" ]
                [ a
                    [ href "#"
                    , class "mx-4 font-sans font-hairline font-medium text-base text-nav-item no-underline"
                    , onClick (SubSelecionado n)
                    ]
                    [ text n.label
                    , span [] [ i [ class "fa fa-angle-right float-right" ] [] ]
                    ]
                ]
        )
        subitens


liMenu : Item -> Html Msg
liMenu item =
    if item.selecionado > 0 then
        li [ liClassSelecionado item.selecionado ]
            [ a
                [ href "#"
                , class "mx-2 font-sans font-hairline font-medium text-base text-nav-item no-underline"
                , onClick (Selecionar item)
                ]
                [ text item.label
                , span [] [ i [ class "fa fa-angle-down float-right" ] [] ]
                ]
            , gerarSubMenu item
            ]

    else
        li [ liClassSelecionado item.selecionado ]
            [ a
                [ href "#"
                , class "mx-2 font-sans font-hairline font-medium text-base text-nav-item no-underline"
                , onClick (Selecionar item)
                ]
                [ text item.label
                , span [] [ i [ class "fas fa-angle-right float-right" ] [] ]
                ]
            ]



-- HTTP


getItens : Cmd Msg
getItens =
    Http.get
        { url = "http://localhost:8000/menu.json"
        , expect = Http.expectJson GotMenu (list itemDecoder)
        }


getSchema : String -> Cmd Msg
getSchema url =
    Http.get
        { url = url
        , expect = Http.expectJson GotSchema (dict (dict campoDecoder))
        }


class : String -> Html.Attribute msg
class name =
    attribute "class" name


topo : Html Msg
topo =
    header [ class "bg-nav", onClick MostrarMenu ]
        [ div [ class "flex justify-between" ]
            [ div [ class "p-1 mx-3 inline-flex items-center" ]
                [ i [ class "fas fa-bars pr-2 text-white" ] []
                , h1 [ class "text-white p-2" ] [ text "Siplag" ]
                ]
            ]
        ]


menuLateral : Html msg
menuLateral =
    ul [ class "list-reset flex flex-col" ]
        [ li [ class "w-full h-full py-3 px-2 border-b border-light-border bg-white" ]
            [ a [ href "index.html", class "font-sans font-hairline hover:font-normal text-sm text-nav-item no-underline" ]
                [ i [ class "fas fa-tachometer-alt float-left mx-2" ] []
                , text "Dashboard"
                , span [] [ i [ class "fas fa-angle-right float-right" ] [] ]
                ]
            ]
        , li [ class "w-full h-full py-3 px-2 border-b border-light-border" ]
            [ a [ href "forms.html", class "font-sans font-hairline hover:font-normal text-sm text-nav-item no-underline" ]
                [ i [ class "fab fa-wpforms float-left mx-2" ] []
                , text "Forms"
                , span [] [ i [ class "fa fa-angle-right float-right" ] [] ]
                ]
            ]
        , li [ class "w-full h-full py-3 px-2 border-b border-light-border" ]
            [ a [ href "buttons.html", class "font-sans font-hairline hover:font-normal text-sm text-nav-item no-underline" ]
                [ i [ class "fas fa-grip-horizontal float-left mx-2" ] []
                , text "Buttons"
                , span []
                    [ i [ class "fa fa-angle-right float-right" ] [] ]
                ]
            ]
        , li
            [ class "w-full h-full py-3 px-2 border-b border-light-border" ]
            [ a [ href "tables.html", class "font-sans font-hairline hover:font-normal text-sm text-nav-item no-underline" ]
                [ i [ class "fas fa-table float-left mx-2" ] []
                , text "Tables"
                , span []
                    [ i [ class "fa fa-angle-right float-right" ] [] ]
                ]
            ]
        , li
            [ class "w-full h-full py-3 px-2 border-b border-light-border" ]
            [ a [ href "ui.html", class "font-sans font-hairline hover:font-normal text-sm text-nav-item no-underline" ]
                [ i [ class "fab fa-uikit float-left mx-2" ] []
                , text "Ui components"
                , span []
                    [ i [ class "fa fa-angle-right float-right" ] [] ]
                ]
            ]
        , li [ class "w-full h-full py-3 px-2 border-b border-300-border" ]
            [ a [ href "modals.html", class "font-sans font-hairline hover:font-normal text-sm text-nav-item no-underline" ]
                [ i [ class "fas fa-square-full float-left mx-2" ] []
                , text "Modals"
                , span []
                    [ i [ class "fa fa-angle-right float-right" ] [] ]
                ]
            ]
        , li [ class "w-full h-full py-3 px-2" ]
            [ a [ href "#", class "font-sans font-hairline hover:font-normal text-sm text-nav-item no-underline" ]
                [ i [ class "far fa-file float-left mx-2" ] []
                , text "Pages"
                , span []
                    [ i [ class "fa fa-angle-down float-right" ] [] ]
                ]
            , ul [ class "list-reset -mx-2 bg-white-medium-dark" ]
                [ li [ class "border-t mt-2 border-light-border w-full h-full px-2 py-3" ]
                    [ a [ href "login.html", class "mx-4 font-sans font-hairline hover:font-normal text-sm text-nav-item no-underline" ]
                        [ text "Login Page"
                        , span [] [ i [ class "fa fa-angle-right float-right" ] [] ]
                        ]
                    ]
                , li [ class "border-t border-light-border w-full h-full px-2 py-3" ]
                    [ a [ href "register.html", class "mx-4 font-sans font-hairline hover:font-normal text-sm text-nav-item no-underline" ]
                        [ text "Register Page"
                        , span []
                            [ i [ class "fa fa-angle-right float-right" ] [] ]
                        ]
                    ]
                , li [ class "border-t border-light-border w-full h-full px-2 py-3" ]
                    [ a [ href "404.html", class "mx-4 font-sans font-hairline hover:font-normal text-sm text-nav-item no-underline" ]
                        [ text "404 Page"
                        , span []
                            [ i [ class "fa fa-angle-right float-right" ] [] ]
                        ]
                    ]
                ]
            ]
        ]


tabela : Html Msg
tabela =
    div [ class "flex flex-1  flex-col md:flex-row lg:flex-row mx-2" ]
        [ div [ class "mb-2 border-solid border-gray-300 rounded border shadow-sm w-full" ]
            [ div [ class "bg-gray-200 px-3 py-1 border-solid border-gray-200 border-b text-right" ]
                [ button [ onClick AbrirModal, class "bg-blue-500 hover:bg-blue-400 text-white font-bold py-2 px-4 mr-3 border border-blue-500 rounded" ] [ text "Cadastrar" ]
                ]
            , div [ class "p-3" ]
                [ table [ class "table-responsive w-full rounded" ]
                    [ thead []
                        [ tr []
                            [ th [ class "border w-1/4 px-4 py-2" ] [ text "Student Name" ]
                            , th [ class "border w-1/6 px-4 py-2" ] [ text "City" ]
                            , th [ class "border w-1/6 px-4 py-2" ] [ text "Course" ]
                            , th [ class "border w-1/6 px-4 py-2" ] [ text "Fee" ]
                            , th [ class "border w-1/7 px-4 py-2" ] [ text "Status" ]
                            , th [ class "border w-1/5 px-4 py-2" ] [ text "Actions" ]
                            ]
                        ]
                    , tbody []
                        [ tr []
                            [ td [ class "border px-4 py-2" ] [ text "Micheal Clarke" ]
                            , td [ class "border px-4 py-2" ] [ text "Sydney" ]
                            , td [ class "border px-4 py-2" ] [ text "MS" ]
                            , td [ class "border px-4 py-2" ] [ text "900 $" ]
                            , td [ class "border px-4 py-2" ]
                                [ i [ class "fas fa-check text-green-500 mx-2" ] [] ]
                            , td [ class "border px-4 py-2" ]
                                [ a [ class "bg-teal-300 cursor-pointer rounded p-1 mx-1 text-white" ]
                                    [ i [ class "fas fa-eye" ] [] ]
                                , a [ class "bg-teal-300 cursor-pointer rounded p-1 mx-1 text-white" ]
                                    [ i [ class "fas fa-edit" ] [] ]
                                , a [ class "bg-teal-300 cursor-pointer rounded p-1 mx-1 text-red-500" ]
                                    [ i [ class "fas fa-trash" ] [] ]
                                ]
                            ]
                        ]
                    ]
                ]
            , button [ class "bg-orange-400 hover:bg-orange-500 text-white font-bold py-2 px-4 ml-4 mx-2 mb-3 border border-orange-400 rounded" ] [ text "Voltar" ]
            , button [ class "bg-white hover:bg-gray-500 text-gray-900 font-semibold py-2 px-4 mx-2 mb-3 border border-gray-200 rounded shadow" ] [ text "Opções" ]
            ]
        ]


modalAberto : Model -> Attribute msg
modalAberto model =
    case model.modo of
        Create ->
            class "modal-wrapper modal-is-open"

        List ->
            class "modal-wrapper"


modal : Model -> Html Msg
modal model =
    div [ modalAberto model ]
        [ div [ class "overlay close-modal" ]
            []
        , div [ class "modal modal-centered" ]
            [ div [ class "modal-content shadow-lg p-5" ]
                [ div [ class "border-b p-2 pb-3 pt-0 mb-4" ]
                    [ div [ class "flex justify-between items-center" ]
                        [ text "Modal header" ]
                    ]
                , form [ id "form_id", class "w-full" ]
                    [ div [ class "flex flex-wrap -mx-3 mb-6" ]
                        [ div [ class "w-full md:w-1/2 px-3 mb-6 md:mb-0" ]
                            [ label
                                [ class "block uppercase tracking-wide text-gray-700 text-xs font-light mb-1"
                                , for "grid-first-name"
                                ]
                                [ text "First Name" ]
                            , input
                                [ class "appearance-none block w-full bg-gray-200 text-gray-700 border border-red-500 rounded py-3 px-4 mb-3 leading-tight focus:outline-none focus:bg-white-500"
                                , id "grid-first-name"
                                , type_ "text"
                                , placeholder "Jane"
                                ]
                                []
                            , p [ class "text-red-500 text-xs italic" ]
                                [ text "Please fill out this field." ]
                            ]
                        , div [ class "w-full md:w-1/2 px-3" ]
                            [ label
                                [ class "block uppercase tracking-wide text-gray-700 text-xs font-light mb-1"
                                , for "grid-last-name"
                                ]
                                [ text "Last Name" ]
                            , input
                                [ class "appearance-none block w-full bg-gray-200 text-grey-darker border border-gray-200 rounded py-3 px-4 leading-tight focus:outline-none focus:bg-white-500 focus:border-gray-600"
                                , id "grid-last-name"
                                , type_ "text"
                                , placeholder "Doe"
                                ]
                                []
                            ]
                        ]
                    , div [ class "flex flex-wrap -mx-3 mb-6" ]
                        [ div [ class "w-full px-3" ]
                            [ label
                                [ class "block uppercase tracking-wide text-grey-darker text-xs font-light mb-1"
                                , for "grid-password"
                                ]
                                [ text "Password" ]
                            , input
                                [ class "appearance-none block w-full bg-grey-200 text-grey-darker border border-grey-200 rounded py-3 px-4 mb-3 leading-tight focus:outline-none focus:bg-white focus:border-grey"
                                , id "grid-password"
                                , type_ "password"
                                , placeholder "******************"
                                ]
                                []
                            , p [ class "text-grey-dark text-xs italic" ]
                                [ text "Make it as long and as crazy as you'd like" ]
                            ]
                        ]
                    , div [ class "flex flex-wrap -mx-3 mb-2" ]
                        [ div [ class "w-full md:w-1/3 px-3 mb-6 md:mb-0" ]
                            [ label
                                [ class "block uppercase tracking-wide text-grey-darker text-xs font-light mb-1"
                                , for "grid-city"
                                ]
                                [ text "City" ]
                            , input
                                [ class "appearance-none block w-full bg-grey-200 text-grey-darker border border-grey-200 rounded py-3 px-4 leading-tight focus:outline-none focus:bg-white focus:border-grey"
                                , id "grid-city"
                                , type_ "text"
                                , placeholder "Albuquerque"
                                ]
                                []
                            ]
                        , div [ class "w-full md:w-1/3 px-3 mb-6 md:mb-0" ]
                            [ label
                                [ class "block uppercase tracking-wide text-grey-darker text-xs font-light mb-1"
                                , for "grid-state"
                                ]
                                [ text "State" ]
                            , div [ class "relative" ]
                                [ select
                                    [ class "block appearance-none w-full bg-grey-200 border border-grey-200 text-grey-darker py-3 px-4 pr-8 rounded leading-tight focus:outline-none focus:bg-white focus:border-grey"
                                    , id "grid-state"
                                    ]
                                    [ option [] [ text "New Mexico" ]
                                    , option [] [ text "Missouri" ]
                                    , option [] [ text "Texas" ]
                                    ]
                                , div [ class "pointer-events-none absolute inset-y-0 right-0 flex items-center px-2 text-grey-darker" ]
                                    [ Svg.svg
                                        [ SvgAttr.class "fill-current h-4 w-4"
                                        , SvgAttr.viewBox "0 0 20 20"
                                        ]
                                        [ Svg.path
                                            [ SvgAttr.d "M9.293 12.95l.707.707L15.657 8l-1.414-1.414L10 10.828 5.757 6.586 4.343 8z"
                                            ]
                                            []
                                        ]
                                    ]
                                ]
                            ]
                        , div [ class "w-full md:w-1/3 px-3 mb-6 md:mb-0" ]
                            [ label
                                [ class "block uppercase tracking-wide text-grey-darker text-xs font-light mb-1"
                                , for "grid-zip"
                                ]
                                [ text "Zip" ]
                            , input
                                [ class "appearance-none block w-full bg-grey-200 text-grey-darker border border-grey-200 rounded py-3 px-4 leading-tight focus:outline-none focus:bg-white focus:border-grey"
                                , id "grid-zip"
                                , type_ "text"
                                , placeholder "90210"
                                ]
                                []
                            ]
                        ]
                    , div [ class "mt-5" ]
                        [ span [ class "close-modal cursor-pointer bg-green-500 hover:bg-green-800 text-white font-bold mx-1 py-2 px-4 rounded" ]
                            [ text "Salvar" ]
                        , span [ onClick FecharModal, class "close-modal cursor-pointer bg-red-200 hover:bg-red-500 text-red-900 font-bold mx-1 py-2 px-4 rounded" ]
                            [ text "Cancelar" ]
                        ]
                    ]
                ]
            ]
        ]
