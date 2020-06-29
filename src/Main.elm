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
    , message: String
    }



init : () -> ( Model, Cmd Msg )
init _ =
    ( { textGallery = Gallery.init (List.length textSlides)
      , imageGallery = Gallery.init (List.length imageSlides)
      , message = ""
      }
    , FetchData
    )


type alias ImageInfo =
  { path : String
  , name : String
  }

type Msg
    = TextGalleryMsg Gallery.Msg
    | ImageGalleryMsg Gallery.Msg
    | FetchData
    | ErrorOccurred String
    | DataFetched (List ImageInfo)



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
        FetchData ->
          ( { model | message = "Initiating data fetch!" }, fetchData )
        ErrorOccurred errorMessage ->
          ( { model | message = "Oops! An error occurred: " ++ errorMessage }, Cmd.none )
        DataFetched configdata ->
          ( { model | configdata = configdata, message = "The data has been fetched!" }, Cmd.none)



view : Model -> Browser.Document Msg
view model =
    { title = "elm-gallery"
    , body =
        [ main_ []
            [ styling
            , Html.map ImageGalleryMsg <|
                Gallery.view imageConfig model.imageGallery [ Gallery.Arrows ] imageSlides
            , Html.map TextGalleryMsg <|
                Gallery.view textConfig model.textGallery [] textSlides
            ]
        ]
    }


textSlides : List ( String, Html msg )
textSlides =
    List.map (\x -> ( x.path, textSlide x )) imageListDecoder


imageSlides : List ( String, Html msg )
imageSlides =
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


images : List String
images =
    [ "images/Ambrym_South_Pacific_Ocean.jpg"
    , "images/Irrawaddy_Delta_Myanmar.jpg"
    , "images/Uyuni_salt_flat_Bolivia.jpg"
    ]


imageInfoDecoder : Decoder imageInfo
imageInfoDecoder =
  map2 ImageInfo
    (field "path" string)
    (field "name" string)

imageListDecoder : Decoder (List imageInfo)
imageListDecoder =
  Json.Decode.list imageInfoDecoder

fetchData : Cmd Msg
fetchData =
  Http.get imageListDecoder "https://localhost/gallery/json/images"
    |> Task.mapError Err
    |> Task.perform ErrorOccurred DataFetched

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
