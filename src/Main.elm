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
        , text
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
                [ aside [] []
                , main_ [] []
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
