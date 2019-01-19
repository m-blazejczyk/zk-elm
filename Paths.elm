module Paths exposing
    ( zkRoot
    , api
    , images
    )


import Url.Builder as Url


zkRoot : List String -> String
zkRoot pathElements = 
    Url.crossOrigin "https://www.zeszytykomiksowe.org" pathElements []


apiRoot : String
apiRoot =
    "http://0.0.0.0:4000"


api : List String -> String
api pathElements =
    Url.crossOrigin apiRoot (["api"] ++ pathElements) []


images : String -> String
images fileName =
    "../zk_portal_umbrella/static/upload/" ++ fileName
