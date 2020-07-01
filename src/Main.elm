module Main exposing (main)

import Browser
import Gallery exposing (..)
import Gallery.Image as Image
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Http
import Json.Decode exposing (..)
import Task exposing (Task)

main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { textGallery : Gallery.State
    , imageGallery : Gallery.State
    , textList : List String
    , urlList : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { textGallery = Gallery.init (List.length (textSlides []))
      , imageGallery = Gallery.init (List.length (imageSlides []))
      , textList = [""]
      , urlList = [""]
      }
    , Cmd.none
    )

type alias ImageInfo =
  { path : String
  , name : String
  }

type Msg
    = TextGalleryMsg Gallery.Msg
    | ImageGalleryMsg Gallery.Msg
    | ErrorOccurred String
    | DataFetched (Result Http.Error (List ImageInfo))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextGalleryMsg textGalleryMsg ->
            ( { model | textGallery = Gallery.update textGalleryMsg model.textGallery }
            , Cmd.none
            )

        ImageGalleryMsg imageGalleryMsg ->
            ( { model | imageGallery = Gallery.update imageGalleryMsg model.imageGallery }
            , Cmd.none
            )
        ErrorOccurred errorMessage ->
          ( model, Cmd.none )
        DataFetched (result, imageList) ->
          ( { model | urlList = imageListUrlDecoder imageList }
          )

view : Model -> Browser.Document Msg
view model =
    { title = "elm-gallery"
    , body =
        [ main_ []
            [ styling
            , Html.map ImageGalleryMsg <|
                Gallery.view imageConfig model.imageGallery [ Gallery.Arrows ] (imageSlides model.urlList)
            , Html.map TextGalleryMsg <|
                Gallery.view textConfig model.textGallery [] (textSlides model.textList)
            ]
        ]
    }


textSlides texts =
    List.map (\x -> ( x, textSlide x )) texts


imageSlides images =
    List.map (\x -> ( x, Image.slide x Image.Cover )) images


textSlide : String -> Html msg
textSlide slide =
    article [] [ h3 [] [ text "Title" ], p [] [ text slide ] ]


imageConfig : Gallery.Config
imageConfig =
    Gallery.config
        { id = "image-gallery"
        , transition = 500
        , width = Gallery.vw 60
        , height = Gallery.px 400
        }


textConfig : Gallery.Config
textConfig =
    Gallery.config
        { id = "text-gallery"
        , transition = 500
        , width = Gallery.px 600
        , height = Gallery.px 400
        }


imageInfoDecoder =
  Json.Decode.map2 ImageInfo
    (field "name" string)
    (field "path" string)

imageTextDecoder : Decoder ImageInfo
imageTextDecoder =
  Json.Decode.map ImageInfo
    (field "name" string)

imageListTextDecoder : Decoder (List ImageInfo)
imageListTextDecoder =
  Json.Decode.list imageTextDecoder

imageUrlDecoder : Decoder ImageInfo
imageUrlDecoder =
  Json.Decode.map ImageInfo
    (field "path" string)

imageListUrlDecoder : Decoder (List ImageInfo)
imageListUrlDecoder =
  Json.Decode.list imageUrlDecoder


fetchData : Cmd Msg
fetchData =
  Http.get
    { url = "https://localhost/gallery/json/images"
    , expect = Http.expectJson DataFetched (Json.Decode.list imageInfoDecoder)
    }


styling : Html msg
styling =
    node "style"
        []
        [ text
           """
                main {
                    font-family: Helvetica, arial, sans-serif;
                }

                a {
                    color: white;
                }

                #image-gallery {
                    margin: 5rem auto;
                    background-color: #eee;
                }

                #text-gallery {
                    margin: 5rem auto;
                    background-color: #eee;
                }

                article {
                    padding: 2rem;
                }

                h4 {
                    color: grey;
                    margin: 1rem 0 0;
                    font-weight: 500;
                }
            """
        ]
