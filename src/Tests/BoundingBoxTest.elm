module Tests.BoundingBoxTest exposing (..)
import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)
import GraphicSVG.Secret exposing (getBoundingBox)
myShapes model = [
        let
            -- Complex shape with various stencils, transformations, and nested groups
            complexShape =
                group
                    [ -- Base shapes
                    circle 20
                        |> filled red
                        |> addOutline (solid 1) black
                        |> move (-30, 10)
                    , rect 30 50
                        |> filled blue
                        |> rotate (degrees 45)
                        |> scale 1.5
                        |> move (20, -20)
                    , roundedRect 40 20 5
                        |> filled green
                        -- |> skew 0.5 0
                        |> move (0, 40)
                    , polygon [ (0, 0), (30, 0), (15, 25) ]
                        |> filled yellow
                        |> addOutline (solid 2) purple
                        |> scale 0.8
                        |> move (-10, -30)
                    , text "Test"
                        -- |> customFont "Arial"
                        |> size 20
                        |> filled black
                        |> rotate (degrees 30)
                        |> move (30, 30)
                    , -- Nested group with transformations
                    group
                        [ oval 25 15
                            |> filled orange
                            |> move (-20, 0)
                        , rect 20 20
                            |> filled pink
                            |> rotate (degrees -60)
                            |> scale 1.2
                            |> move (10, -10)
                        ]
                        |> scale 0.7
                        -- |> skew 0 0.3
                        |> move (40, 0)
                    ]
            
            -- Compute bounding box
            boundingBox = getBoundingBox complexShape
            
            -- Visualize bounding box as a rectangle
            boundingBoxRect =
                rect (boundingBox.maxX - boundingBox.minX) (boundingBox.maxY - boundingBox.minY)
                    |> outlined (dashed 1) black
                    |> move ((boundingBox.minX + boundingBox.maxX) / 2, (boundingBox.minY + boundingBox.maxY) / 2)
            
            -- Display bounding box coordinates
            boundingBoxText =
                text (Debug.toString boundingBox)
                    |> filled black
                    |> scale 0.25
                    |> move (-50, -50)
        in
        group [ complexShape, boundingBoxRect, boundingBoxText ]
    
    ]


type Msg = Tick Float GetKeyState

type alias Model = { time : Float }

update msg model = case msg of
                     Tick t _ -> { time = t }

init = { time = 0 }

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)



