module Paths exposing
    ( zkStatic
    , api
    , images
    )


import Url.Builder as Url


zkStatic : String -> String
zkStatic file = 
    Url.crossOrigin "https://red.zeszytykomiksowe.org" [file] []


apiRoot : String
apiRoot =
    "http://0.0.0.0:4000"


api : List String -> String
api pathElements =
    Url.crossOrigin apiRoot (["api"] ++ pathElements) []


images : String -> String
images fileName =
    "../zk_portal_umbrella/static/upload/" ++ fileName
