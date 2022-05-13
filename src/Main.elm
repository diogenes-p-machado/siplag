module Main exposing (..)

import Browser
import Html
    exposing
        ( Html
        , a
        , aside
        , div
        , h1
        , header
        , hr
        , i
        , img
        , label
        , li
        , main_
        , p
        , section
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
        ( alt
        , attribute
        , href
        , id
        , src
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
        , field
        , int
        , list
        , map
        , map2
        , map3
        , string
        )



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
    { menu : List Objeto
    , conteudo : Conteudo
    , schema : Schema
    }


type alias Schema =
    { tabela : String
    , schema : String
    , campos : List Campo
    }


type Conteudo
    = Listagem
    | Formulario


type Campo
    = Texto String String


type Objeto
    = Label String
    | Lista (List Item)


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


listObjDecoder : Decoder (List Objeto)
listObjDecoder =
    list objetoDecoder


objetoDecoder : Decoder Objeto
objetoDecoder =
    field "tipo" string
        |> andThen objetoPorTipo


objetoPorTipo : String -> Decoder Objeto
objetoPorTipo tipo =
    case tipo of
        "menu-label" ->
            labelMenuObj

        "menu-list" ->
            listMenuObj

        _ ->
            Debug.todo "nenhum decoder"


labelMenuObj : Decoder Objeto
labelMenuObj =
    map Label
        (field "label" string)


listMenuObj : Decoder Objeto
listMenuObj =
    map Lista
        (field "lista" (list itemDecoder))


itemDecoder : Decoder Item
itemDecoder =
    map3 Item
        (field "label" string)
        (field "selecionado" int)
        (field "sub-itens" (list subItemDecoder))


schemaDecoder : Decoder Schema
schemaDecoder =
    map3 Schema
        (field "tabela" string)
        (field "schema" string)
        (field "campos" (list campoDecoder))


campoDecoder : Decoder Campo
campoDecoder =
    field "tipo" string
        |> andThen tipoDoCampo


tipoDoCampo : String -> Decoder Campo
tipoDoCampo tipo =
    case tipo of
        "texto" ->
            decoderCampoTexto

        _ ->
            Debug.todo "nenhum decoder"


decoderCampoTexto : Decoder Campo
decoderCampoTexto =
    map2 Texto
        (field "nome_campo" string)
        (field "codinome_campo" string)


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
        Listagem
        (Schema "" "" [])
    , getObjetos
    )



-- UPDATE


type Msg
    = GotMenu (Result Http.Error (List Objeto))
    | GotSchema (Result Http.Error Schema)
    | Selecionar Item
    | SubSelecionado SubItem


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMenu result ->
            case result of
                Ok listMenu ->
                    ( { model | menu = listMenu }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        Selecionar item ->
            ( { model | menu = atualizarMenu model.menu item }, Cmd.none )

        GotSchema result ->
            case result of
                Ok schemaOk ->
                    ( { model | schema = schemaOk }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        SubSelecionado subitem ->
            ( model, getSchema subitem.link )



-- SUBSCRIPTIONS


atualizarMenu : List Objeto -> Item -> List Objeto
atualizarMenu menu item =
    List.map
        (\n -> substituirObjeto n item)
        menu


substituirObjeto : Objeto -> Item -> Objeto
substituirObjeto objItem item =
    case objItem of
        Label _ ->
            objItem

        Lista lista ->
            Lista (substituirItem lista item)


substituirItem : List Item -> Item -> List Item
substituirItem lista item =
    List.map (compararItem item) lista


compararItem : Item -> Item -> Item
compararItem item itemMsg =
    if item == itemMsg then
        { item | selecionado = item.selecionado * -1 }

    else
        itemMsg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "mx-auto bg-grey-400" ]
        [ div [ class "min-h-screen flex flex-col" ]
            [ -- todo conteúdo é aqui
              topo -- topo é a tag reader
            , div [ class "flex flex-1" ]
                [ aside [ class "bg-side-nav w-1/2 md:w-1/6 lg:w-1/6 border-r border-side-nav hidden md:block lg:block" ]
                    [ menuLateral ]
                , main_ [class "bg-white-500 flex-1 p-3 overflow-hidden"]
                    [ div [ class "flex flex-col" ] [tabela] ]
                ]
            ]
        ]



--viewMenu model


viewMenu : List Objeto -> Html Msg
viewMenu menu =
    Html.nav [ class "menu section" ] (List.map (\n -> gerarMenu n) menu)


gerarMenu : Objeto -> Html Msg
gerarMenu menu =
    case menu of
        Label label ->
            p [ class "menu-label" ] [ text label ]

        Lista lista ->
            ul [ class "menu-list" ] (List.map (\n -> liMenu n) lista)


gerarSubMenu : Item -> Html Msg
gerarSubMenu item =
    ul [] (subItens item.subItens)


subItens : List SubItem -> List (Html Msg)
subItens subitens =
    List.map (\n -> li [] [ a [ onClick (SubSelecionado n) ] [ text n.label ] ]) subitens


liMenu : Item -> Html Msg
liMenu i =
    if i.selecionado > 0 then
        li [] [ a [ onClick (Selecionar i) ] [ text i.label ], gerarSubMenu i ]

    else
        li [] [ a [ onClick (Selecionar i) ] [ text i.label ] ]



-- HTTP


getObjetos : Cmd Msg
getObjetos =
    Http.get
        { url = "http://localhost:8000/menu.json"
        , expect = Http.expectJson GotMenu listObjDecoder
        }


getSchema : String -> Cmd Msg
getSchema url =
    Http.get
        { url = url
        , expect = Http.expectJson GotSchema schemaDecoder
        }


class : String -> Html.Attribute msg
class name =
    attribute "class" name


topo : Html msg
topo =
    header [ class "bg-nav" ]
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
        [ li [ class " w-full h-full py-3 px-2 border-b border-light-border bg-white" ]
            [ a [ href "index.html", class "font-sans font-hairline hover:font-normal text-sm text-nav-item no-underline" ]
                [ i [ class "fas fa-tachometer-alt float-left mx-2" ] []
                , text "Dashboard"
                , span []
                    [ i [ class "fas fa-angle-right float-right" ] [] ]
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
                        , span []
                            [ i [ class "fa fa-angle-right float-right" ] [] ]
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
            [ div [ class "bg-gray-200 px-2 py-3 border-solid border-gray-200 border-b" ]
                [ text "Full Table" ]
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
                        , tr []
                            [ td [ class "border px-4 py-2" ] [ text "Rickey Ponting" ]
                            , td
                                [ class "border px-4 py-2"
                                ]
                                [ text "Sydney" ]
                            , td
                                [ class "border px-4 py-2"
                                ]
                                [ text "MS" ]
                            , td
                                [ class "border px-4 py-2"
                                ]
                                [ text "300 $" ]
                            , td
                                [ class "border px-4 py-2"
                                ]
                                [ i
                                    [ class "fas fa-times text-red-500 mx-2"
                                    ]
                                    []
                                ]
                            , td
                                [ class "border px-4 py-2"
                                ]
                                [ a
                                    [ class "bg-teal-300 cursor-pointer rounded p-1 mx-1 text-white"
                                    ]
                                    [ i
                                        [ class "fas fa-eye"
                                        ]
                                        []
                                    ]
                                , a
                                    [ class "bg-teal-300 cursor-pointer rounded p-1 mx-1 text-white"
                                    ]
                                    [ i
                                        [ class "fas fa-edit"
                                        ]
                                        []
                                    ]
                                , a
                                    [ class "bg-teal-300 cursor-pointer rounded p-1 mx-1 text-red-500"
                                    ]
                                    [ i
                                        [ class "fas fa-trash"
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        , tr []
                            [ td
                                [ class "border px-4 py-2"
                                ]
                                [ text "Micheal Clarke" ]
                            , td
                                [ class "border px-4 py-2"
                                ]
                                [ text "Sydney" ]
                            , td
                                [ class "border px-4 py-2"
                                ]
                                [ text "MS" ]
                            , td
                                [ class "border px-4 py-2"
                                ]
                                [ text "900 $" ]
                            , td
                                [ class "border px-4 py-2"
                                ]
                                [ i
                                    [ class "fas fa-check text-green-500 mx-2"
                                    ]
                                    []
                                ]
                            , td
                                [ class "border px-4 py-2"
                                ]
                                [ a
                                    [ class "bg-teal-300 cursor-pointer rounded p-1 mx-1 text-white"
                                    ]
                                    [ i
                                        [ class "fas fa-eye"
                                        ]
                                        []
                                    ]
                                , a
                                    [ class "bg-teal-300 cursor-pointer rounded p-1 mx-1 text-white"
                                    ]
                                    [ i
                                        [ class "fas fa-edit"
                                        ]
                                        []
                                    ]
                                , a
                                    [ class "bg-teal-300 cursor-pointer rounded p-1 mx-1 text-red-500"
                                    ]
                                    [ i
                                        [ class "fas fa-trash"
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        , tr []
                            [ td
                                [ class "border px-4 py-2"
                                ]
                                [ text "Micheal Clarke" ]
                            , td
                                [ class "border px-4 py-2"
                                ]
                                [ text "Sydney" ]
                            , td
                                [ class "border px-4 py-2"
                                ]
                                [ text "MS" ]
                            , td
                                [ class "border px-4 py-2"
                                ]
                                [ text "900 $" ]
                            , td
                                [ class "border px-4 py-2"
                                ]
                                [ i
                                    [ class "fas fa-check text-green-500 mx-2"
                                    ]
                                    []
                                ]
                            , td
                                [ class "border px-4 py-2"
                                ]
                                [ a
                                    [ class "bg-teal-300 cursor-pointer rounded p-1 mx-1 text-white"
                                    ]
                                    [ i
                                        [ class "fas fa-eye"
                                        ]
                                        []
                                    ]
                                , a
                                    [ class "bg-teal-300 cursor-pointer rounded p-1 mx-1 text-white"
                                    ]
                                    [ i
                                        [ class "fas fa-edit"
                                        ]
                                        []
                                    ]
                                , a
                                    [ class "bg-teal-300 cursor-pointer rounded p-1 mx-1 text-red-500"
                                    ]
                                    [ i
                                        [ class "fas fa-trash"
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        , tr []
                            [ td
                                [ class "border px-4 py-2"
                                ]
                                [ text "Micheal Clarke" ]
                            , td
                                [ class "border px-4 py-2"
                                ]
                                [ text "Sydney" ]
                            , td
                                [ class "border px-4 py-2"
                                ]
                                [ text "MS" ]
                            , td
                                [ class "border px-4 py-2"
                                ]
                                [ text "900 $" ]
                            , td
                                [ class "border px-4 py-2"
                                ]
                                [ i
                                    [ class "fas fa-check text-green-500 mx-2"
                                    ]
                                    []
                                ]
                            , td
                                [ class "border px-4 py-2"
                                ]
                                [ a
                                    [ class "bg-teal-300 cursor-pointer rounded p-1 mx-1 text-white"
                                    ]
                                    [ i
                                        [ class "fas fa-eye"
                                        ]
                                        []
                                    ]
                                , a
                                    [ class "bg-teal-300 cursor-pointer rounded p-1 mx-1 text-white"
                                    ]
                                    [ i
                                        [ class "fas fa-edit"
                                        ]
                                        []
                                    ]
                                , a
                                    [ class "bg-teal-300 cursor-pointer rounded p-1 mx-1 text-red-500"
                                    ]
                                    [ i
                                        [ class "fas fa-trash"
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
