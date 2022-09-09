module Urls exposing (..)

type Url
    = Recurso String
    | IdRecurso String String
    | SubRecurso String String String


push : Maybe Url -> String -> Maybe Url
push url str =
    case url of
        Just (Recurso recurso) ->
            Just (IdRecurso recurso str)

        Just (IdRecurso recurso id) ->
            Just (SubRecurso recurso id str)
        
        _ -> Nothing
    

pop : Maybe Url -> Maybe Url
pop url =
    case url of
        Just (SubRecurso recuso id _) ->
            Just (IdRecurso recuso id)

        Just (IdRecurso recurso _ ) ->
            Just (Recurso recurso)          

        _ -> Nothing    


getUrl : Maybe Url -> String
getUrl url =
    case url of
        Just (Recurso recurso) -> 
            "/" ++ recurso

        Just (IdRecurso recurso id ) -> 
            "/" ++ recurso ++ "/" ++ id

        Just (SubRecurso recurso id subrecurso) ->
             "/" ++ recurso ++ "/" ++ id ++ "/" ++ subrecurso

        _ -> ""     