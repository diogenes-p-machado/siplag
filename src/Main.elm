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
        , menu
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
        , value
        )
import Html.Events
    exposing
        ( onClick
        , onInput
        )
import Http
import Json.Decode
    exposing
        ( Decoder
        , andThen
        , dict
        , field
        , int
        , lazy
        , list
        , map
        , map3
        , map5
        , nullable
        , string
        )
import Url.Builder exposing (absolute)
import Json.Encode


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { url : List String
    , menu : List Item
    , modo : Maybe Crud
    , schema : Maybe Tabela
    , tabela : Maybe Tabela
    , showMenu : Bool
    , formJson : Dict String String
    , subFormJson : Dict String String
    , listJson : List (Dict String String)
    }


type Crud
    = Create
    | List


type alias Tabela =
    { schema : Maybe String
    , codinome : String
    , nome : String
    , campos : List Campo
    , links : Maybe Links
    }


type Links
    = Links (List Tabela)


type Campo
    = Texto InputText
    | Id Int


type alias InputText =
    { codinome : String
    , nome : String
    , prioridade : Int
    }


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


tabelaDecoder : Decoder Tabela
tabelaDecoder =
    map5 Tabela
        (field "schema" (nullable string))
        (field "codinome" string)
        (field "nome" string)
        (field "campos" (list campoDecoder))
        (field "links" (nullable (map Links (list (lazy (\_ -> tabelaDecoder))))))


campoDecoder : Decoder Campo
campoDecoder =
    field "tipo" string
        |> andThen tipoDoCampo


decoderCampoTexto : Decoder Campo
decoderCampoTexto =
    map Texto decoderInputText


decoderInputText : Decoder InputText
decoderInputText =
    map3 InputText
        (field "codinome" string)
        (field "nome" string)
        (field "prioridade" int)


tipoDoCampo : String -> Decoder Campo
tipoDoCampo tipo =
    case tipo of
        "input-text" ->
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
        []
        Nothing
        Nothing
        Nothing
        True
        Dict.empty
        Dict.empty
        []
    , getItens
    )


type Msg
    = GotMenu (Result Http.Error (List Item))
    | GotList (Result Http.Error (List (Dict String String)))
    | GotSchema (Result Http.Error Tabela)
    | Selecionar Item
    | SubSelecionado SubItem Item
    | MostrarMenu
    | SelectItemList (Dict String String)
    | Trocar Tabela
    | Voltar (Maybe Tabela)
    | AbrirModal
    | PostForm
    | Posted (Result Http.Error ())
    | FecharModal
    | InputTextMsg String String


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
            ( { model
                | schema = Nothing
                , tabela = Nothing
                , menu = atualizarMenu model.menu item
              }
            , Cmd.none
            )

        GotList result ->
            case result of
                Ok res ->
                    ( { model | listJson = res }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotSchema result ->
            case result of
                Ok res ->
                    ( { model
                        | schema = Just res
                        , tabela = Just res
                      }
                    , getList ("http://0.0.0.0:3000/" ++ Maybe.withDefault "" res.schema ++ "/" ++ res.nome)
                    )

                Err _ ->
                    ( model, Cmd.none )

        SubSelecionado subitem item ->
            let
                subItensNew =
                    List.map (\n -> substituirSubItem subitem n) (.subItens item)

                itemNew =
                    { item | subItens = subItensNew }

                menuNew =
                    List.map
                        (\n ->
                            if n == item then
                                itemNew

                            else
                                n
                        )
                        model.menu
            in
            ( { model | menu = menuNew }, getSchema subitem.link )

        MostrarMenu ->
            ( { model | showMenu = not model.showMenu }, Cmd.none )

        AbrirModal ->
            ( { model | modo = Just Create, formJson = Dict.empty }, Cmd.none )

        FecharModal ->
            ( { model | modo = Just List }, Cmd.none )

        Trocar a ->
            ( { model | tabela = Just a }, Cmd.none )

        Voltar a ->
            ( { model | tabela = a }, Cmd.none )

        InputTextMsg name value ->
            ( { model | formJson = Dict.insert name value model.formJson }, Cmd.none )

        PostForm ->
            ( model, Cmd.none )

        Posted result ->
            (model, Cmd.none)    

        SelectItemList dicionario ->
            ( { model
                | formJson =
                    if dicionario == model.formJson then
                        Dict.empty

                    else
                        dicionario
              }
            , Cmd.none
            )


substituirSubItem : SubItem -> SubItem -> SubItem
substituirSubItem subMsg subList =
    if subMsg == subList then
        { subList | selecionado = 1 }

    else
        { subList | selecionado = -1 }


atualizarMenu : List Item -> Item -> List Item
atualizarMenu menu item =
    substituirItem menu item


substituirItem : List Item -> Item -> List Item
substituirItem menu item =
    List.map (compararItem item) menu


zerarSelecao : SubItem -> SubItem
zerarSelecao novo =
    { novo | selecionado = -1 }


compararItem : Item -> Item -> Item
compararItem item itemMsg =
    if item == itemMsg then
        { item | selecionado = item.selecionado * -1, subItens = List.map zerarSelecao item.subItens }

    else
        { itemMsg | selecionado = -1 }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


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
                    [ div [ class "flex flex-col" ] [ tabela model ]
                    ]
                ]
            , modal model
            ]
        ]


viewMenu : List Item -> Html Msg
viewMenu menu =
    ul [ class "list-reset flex flex-col overscroll-contain" ] (List.map (\n -> liMenu n) menu)


gerarSubMenu : Item -> Html Msg
gerarSubMenu item =
    ul [ class "list-reset -mx-2 bg-white-medium-dark" ] (subItens item.subItens item)


liClassSelecionado : number -> Attribute msg
liClassSelecionado selecionado =
    if selecionado > 0 then
        class "w-full h-full py-3 px-2"

    else
        class "w-full h-full py-3 px-2 border-b border-300-border"


subItens : List SubItem -> Item -> List (Html Msg)
subItens subitens item =
    List.map
        (\n ->
            li [ bgColorSub n ]
                [ a
                    [ href "#"
                    , class "mx-4 font-sans font-hairline font-medium text-base text-nav-item no-underline"
                    , onClick (SubSelecionado n item)
                    ]
                    [ text n.label
                    , span [] [ i [ class "fa fa-angle-right float-right" ] [] ]
                    ]
                ]
        )
        subitens


bgColorSub : { a | selecionado : number } -> Attribute msg
bgColorSub subit =
    if .selecionado subit == 1 then
        class "border-t mt-2 border-light-border w-full h-full bg-white px-2 py-3"

    else
        class "border-t mt-2 border-light-border w-full h-full px-2 py-3"


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


getItens : Cmd Msg
getItens =
    Http.get
        { url = "/menu.json"
        , expect = Http.expectJson GotMenu (list itemDecoder)
        }


postFormJson : Model -> Cmd Msg
postFormJson model =
    Http.post
        { url = "implementar url do eschema atual"
        , body = Http.jsonBody (Json.Encode.dict identity Json.Encode.string model.formJson)
        , expect = Http.expectWhatever Posted
        }


getList : String -> Cmd Msg
getList url =
    Http.get
        { url = url
        , expect = Http.expectJson GotList (list (dict string))
        }


getSchema : String -> Cmd Msg
getSchema url =
    Http.get
        { url = url
        , expect = Http.expectJson GotSchema tabelaDecoder
        }


class : String -> Html.Attribute msg
class name =
    attribute "class" name


buttonSelect : Dict String String -> Html.Attribute msg
buttonSelect dic =
    if Dict.isEmpty dic then
        class "bg-white hover:bg-gray-500 text-gray-900 font-semibold py-2 px-4 mx-2 mb-3 border border-gray-200 rounded shadow  opacity-50 cursor-not-allowed"

    else
        class "bg-white hover:bg-gray-500 text-gray-900 font-semibold py-2 px-4 mx-2 mb-3 border border-gray-200 rounded shadow"


trSelect : Dict String String -> Dict String String -> Html.Attribute msn
trSelect jsonForm jsonList =
    if jsonForm == jsonList then
        class "border-b bg-blue-400 text-white px-4 py-2"

    else
        class "hover:border-b hover:bg-gray-300 px-4 py-2"


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


campoToString : Campo -> String
campoToString campo =
    case campo of
        Texto a ->
            .codinome a

        _ ->
            ""


thHeadTabela : Tabela -> List (Html Msg)
thHeadTabela t =
    List.map
        (\n ->
            th [ class "border-b w-1/4 px-4 py-2" ]
                [ text (campoToString n) ]
        )
        t.campos


htmlIf : Html msg -> Bool -> Html msg
htmlIf el cond =
    if cond then
        el

    else
        text ""


tabela : Model -> Html Msg
tabela model =
    case model.tabela of
        Just tabelaOk ->
            div [ class "flex flex-1  flex-col md:flex-row lg:flex-row mx-2" ]
                [ div [ class "mb-2 border-solid border-gray-300 rounded border shadow-sm w-full" ]
                    [ div [ class "bg-gray-200 px-3 py-1 border-solid border-gray-200 border-b text-right" ]
                        [ htmlIf
                            (button
                                [ onClick (Voltar model.schema), class "bg-blue-500 hover:bg-blue-400 text-white font-bold py-2 px-4 mr-3 border border-blue-500 rounded" ]
                                [ text "Anterior" ]
                            )
                            (model.schema /= model.tabela)
                        , button [ onClick AbrirModal, class "bg-blue-500 hover:bg-blue-400 text-white font-bold py-2 px-4 mr-3 border border-blue-500 rounded" ] [ text "Cadastrar" ]
                        ]
                    , div [ class "p-3" ]
                        [ table [ class "table-auto" ]
                            [ thead []
                                [ tr [] <| thHeadTabela tabelaOk ]
                            , tbody [] <|
                                List.map
                                    (\n ->
                                        tr [ onClick (SelectItemList n), trSelect model.formJson n ] <|
                                            List.map (\i -> td [ class "border-b px-4 py-2" ] [ text i ]) <|
                                                Dict.values n
                                    )
                                    model.listJson
                            ]
                        ]
                    , div [] (links tabelaOk model)
                    ]
                ]

        Nothing ->
            text "A implementar"


links : Tabela -> Model -> List (Html Msg)
links tab model =
    case tab.links of
        Just l ->
            case l of
                Links list ->
                    List.map
                        (\n ->
                            button [ onClick (Trocar n), buttonSelect model.formJson ]
                                [ text (.codinome n) ]
                        )
                        list

        _ ->
            []


modalAberto : Model -> Attribute msg
modalAberto model =
    case model.modo of
        Just Create ->
            class "modal-wrapper modal-is-open"

        _ ->
            class "modal-wrapper"


construtorCampo : Campo -> Model -> Html Msg
construtorCampo campo model =
    case campo of
        Texto inputText ->
            div [ class "flex flex-wrap -mx-3 mb-2" ]
                [ div [ class "w-full px-3" ]
                    [ label [ class "block uppercase tracking-wide text-grey-darker text-xs font-light mb-1", for "nome" ]
                        [ text inputText.codinome ]
                    , input
                        [ onInput (InputTextMsg inputText.nome)
                        , class "appearance-none block w-full bg-grey-200 text-grey-darker border border-grey-200 rounded py-3 px-4 mb-3 leading-tight focus:outline-none focus:bg-white focus:border-grey"
                        , type_ "text"
                        , id "nome"
                        , placeholder "Digite seu nome:"
                        , value <| valueString inputText.nome model.formJson
                        ]
                        []
                    ]
                ]

        _ ->
            Debug.todo "A implementar outros tipos de campo"


valueString : String -> Dict String String -> String
valueString string dict =
    Dict.get string dict
        |> Maybe.withDefault ""


construtorForm : Model -> List (Html Msg)
construtorForm model =
    case model.tabela of
        Just t ->
            List.map (\n -> construtorCampo n model) (.campos t)

        Nothing ->
            []


modal : Model -> Html Msg
modal model =
    div [ modalAberto model ]
        [ div [ class "overlay close-modal" ] []
        , div [ class "modal modal-centered" ]
            [ div [ class "modal-content shadow-lg p-5" ]
                [ div [ class "border-b p-2 pb-3 pt-0 mb-4" ]
                    [ div [ class "flex justify-between items-center" ]
                        [ text "Modal header"
                        , span [ onClick FecharModal, class "close-modal cursor-pointer px-3 py-1 rounded-full bg-gray-100 hover:bg-gray-200" ]
                            [ i [ class "fas fa-times text-gray-700" ] []
                            ]
                        ]
                    ]
                , form [ class "w-full" ] (construtorForm model)
                , div [ class "mt-5" ]
                    [ span [ class "close-modal cursor-pointer bg-green-500 hover:bg-green-800 text-white font-bold mx-1 py-2 px-4 rounded" ]
                        [ text "Salvar" ]
                    , span [ onClick FecharModal, class "close-modal cursor-pointer bg-red-200 hover:bg-red-500 text-red-900 font-bold mx-1 py-2 px-4 rounded" ]
                        [ text "Cancelar" ]
                    ]
                ]
            ]
        ]
