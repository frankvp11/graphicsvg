module GraphicSVG.Secret exposing 
    (Stencil(..), Shape(..), Color(..), Gradient(..), Stop(..), Transform, LineType(..), FontAlign(..), Face(..), Font(..), Pull(..), getBoundingBox)

{-| Advanced Secret module! This is for people who want to access the
underlying types in the library so you can do advanced things. Most people
don't need this much detail! I recommend you look at the source of this 
module to determine how to use these.

# Shapes and Stencils
@docs Stencil, Shape

# Colours and Gradients
@docs Color, Gradient, Stop

# Raw Transformations
@docs Transform

# LineTypes
@docs LineType

# Text and Fonts
@docs FontAlign, Face, Font

# Curve Pulls
@docs Pull

-}

import Html
import Color


{-| A primitive template representing the shape you wish to draw. This must be turned into
a `Shape` before being drawn to the screen with `collage` (see below).
-}
type Stencil
    = Circle Float
    | Rect Float Float
    | RoundRect Float Float Float
    | Oval Float Float
    | BezierPath ( Float, Float ) (List ( ( Float, Float ), ( Float, Float ) ))
    | Polygon (List ( Float, Float ))
    | Path (List ( Float, Float ))
    | Text Face String


{-| A filled, outlined, or filled and outlined object that can be drawn to the screen using `collage`.
-}
type Shape userMsg
    = Inked (Maybe Color) (Maybe ( LineType, Color )) Stencil
    | ForeignObject Float Float (Html.Html userMsg)
    | Move ( Float, Float ) (Shape userMsg)
    | Rotate Float (Shape userMsg)
    | Scale Float Float (Shape userMsg)
    | Skew Float Float (Shape userMsg)
    | Transformed Transform (Shape userMsg)
    | Group (List (Shape userMsg))
    | GroupOutline (Shape userMsg)
    | AlphaMask (Shape userMsg) (Shape userMsg)
    | Clip (Shape userMsg) (Shape userMsg)
    | Everything
    | Notathing
    | Link String (Shape userMsg)
    | Tap userMsg (Shape userMsg)
    | TapAt (( Float, Float ) -> userMsg) (Shape userMsg)
    | EnterShape userMsg (Shape userMsg)
    | EnterAt (( Float, Float ) -> userMsg) (Shape userMsg)
    | Exit userMsg (Shape userMsg)
    | ExitAt (( Float, Float ) -> userMsg) (Shape userMsg)
    | MouseDown userMsg (Shape userMsg)
    | MouseDownAt (( Float, Float ) -> userMsg) (Shape userMsg)
    | MouseUp userMsg (Shape userMsg)
    | MouseUpAt (( Float, Float ) -> userMsg) (Shape userMsg)
    | MoveOverAt (( Float, Float ) -> userMsg) (Shape userMsg)
    | TouchStart userMsg (Shape userMsg)
    | TouchEnd userMsg (Shape userMsg)
    | TouchStartAt (( Float, Float ) -> userMsg) (Shape userMsg)
    | TouchEndAt (( Float, Float ) -> userMsg) (Shape userMsg)
    | TouchMoveAt (( Float, Float ) -> userMsg) (Shape userMsg)
    | GraphPaper Float Float Color

{-| The `Color` type is used for filling or outlining a `Stencil`.
-}
type Color
    = Solid Color.Color
    | Gradient Gradient

{-| A type representing radial and linear gradients.
-}
type Gradient =
      RadialGradient (List Stop)
    | LinearGradient Float {- rotation -} (List Stop)

{-| A type representing stops in a gradient. Consists of one constructor 
with inputs for the position, transparency and colour.
-}
type Stop =
    Stop Float {- stop position -} Float {- transparency -} Color.Color {- colour -}


{-| A matrix representing an SVG transformation matrix of the form:

```
( ( a , c , e ) , ( b , d , f ) )
```

or

```
 a c e
 b d f
```

The a, c, b and d control transformations such as skew, rotate and scale;
e and f control translation.

These matrices are best built up by starting with the identity matrix and
applying any number of `*T` functions (see below); for example,

```
myTransform =
    ident
        |> scaleT 2 2
        |> rotateT (degrees 30)
        |> moveT (0, 50)
```

-}
type alias Transform =
    ( ( Float, Float, Float ), ( Float, Float, Float ) )


{-| The `LineType` type is used to define the appearance of an outline for a `Stencil`.
`LineType` also defines the appearence of `line` and `curve`.
-}
type LineType
    = NoLine
    | Unbroken Float
    | Broken (List ( Float, Float )) Float


{-| A simple algebraic data type with three constructors:
- AlignLeft
- AlignCentred
- AlignRight
-}
type FontAlign
    = AlignLeft
    | AlignCentred
    | AlignRight



{-| The `Face` type describes the appearance of a text `Stencil`.
-}
type Face
    = Face
        Float
        -- size
        Bool
        -- bold
        Bool
        -- italic
        Bool
        -- underline
        Bool
        -- strikethrough
        Bool
        -- selectable
        Font
        -- font alignment
        FontAlign


{-| The `Font` type describes the font of a text `Stencil`.
-}
type Font
    = Serif
    | Sansserif
    | FixedWidth
    | Custom String


{-| To make it easier to read the code defining a `curve`,
and to make sure we always use the right number of curve points
and pull points (which is one more curve point than pull points),
we define a special `Pull` type, whose first point is the point
we pull towards, and second point is the end point for this
curve segments.
-}
type Pull
    = Pull ( Float, Float ) ( Float, Float )


type alias BoundingBox =
    { minX : Float
    , minY : Float
    , maxX : Float
    , maxY : Float
    }


ident : Transform
ident =
    ( ( 1, 0, 0 )
    , ( 0, 1, 0 )
    )

moveT : ( Float, Float ) -> Transform -> Transform
moveT ( u, v ) ( ( a, c, tx ), ( b, d, ty ) ) =
    ( ( a, c, tx + a * u + c * v )
    , ( b, d, ty + b * u + d * v )
    )

rotateT : Float -> Transform -> Transform
rotateT rad ( ( a, c, tx ), ( b, d, ty ) ) =
    let
        sinX =
            sin rad

        cosX =
            cos rad
    in
    ( ( a * cosX + c * sinX, c * cosX - a * sinX, tx )
    , ( b * cosX + d * sinX, d * cosX - b * sinX, ty )
    )


scaleT : Float -> Float -> Transform -> Transform
scaleT sx sy ( ( a, c, tx ), ( b, d, ty ) ) =
    ( ( a * sx, c * sy, tx )
    , ( b * sx, d * sy, ty )
    )


skewT : Float -> Float -> Transform -> Transform
skewT skx sky ( ( a, c, tx ), ( b, d, ty ) ) =
    let
        tanX =
            tan -skx

        tanY =
            tan -sky
    in
    ( ( a + c * tanY, c + a * tanX, tx )
    , ( b + d * tanY, d + b * tanX, ty )
    )


rotateAboutT : ( Float, Float ) -> Float -> Transform -> Transform
rotateAboutT ( u, v ) rad ( ( a, c, tx ), ( b, d, ty ) ) =
    let
        sinX =
            sin rad

        cosX =
            cos rad
    in
    ( ( a * cosX + c * sinX, c * cosX - a * sinX, tx + a * u + c * v - v * (c * cosX - a * sinX) - u * (a * cosX + c * sinX) )
    , ( b * cosX + d * sinX, d * cosX - b * sinX, ty + b * u + d * v - v * (d * cosX - b * sinX) - u * (b * cosX + d * sinX) )
    )



-- Matrix multiplication for transformations
matrixMult : Transform -> Transform -> Transform
matrixMult ( ( a, c, e ), ( b, d, f ) ) ( ( a1, c1, e1 ), ( b1, d1, f1 ) ) =
    ( ( a * a1 + c * b1, a * c1 + c * d1, e + a * e1 + c * f1 )
    , ( b * a1 + d * b1, b * c1 + d * d1, f + b * e1 + d * f1 )
    )

-- Compute the bounding box of a shape with an accumulated transformation
getBoundingBox : Shape userMsg -> BoundingBox
getBoundingBox shape =
    getBoundingBoxWithTransform shape ident

-- Helper function to compute bounding box with an accumulated transformation matrix
getBoundingBoxWithTransform : Shape userMsg -> Transform -> BoundingBox
getBoundingBoxWithTransform shape trans =
    case shape of
        Inked _ _ stencil ->
            stencilBoundingBox stencil |> applyTransform trans

        ForeignObject w h _ ->
            { minX = -w / 2, minY = -h / 2, maxX = w / 2, maxY = h / 2 }
                |> applyTransform trans

        Move (dx, dy) sh ->
            getBoundingBoxWithTransform sh (moveT (dx, dy) trans)

        Rotate angle sh ->
            getBoundingBoxWithTransform sh (rotateT angle trans)

        Scale sx sy sh ->
            getBoundingBoxWithTransform sh (scaleT sx sy trans)

        Skew skx sky sh ->
            getBoundingBoxWithTransform sh (skewT skx sky trans)

        Transformed tm sh ->
            getBoundingBoxWithTransform sh (matrixMult trans tm)

        Group shapes ->
            case shapes of
                [] ->
                    { minX = 0, minY = 0, maxX = 0, maxY = 0 }
                _ ->
                    let
                        boxes = List.map (\sh -> getBoundingBoxWithTransform sh trans) shapes
                        minXs = List.map .minX boxes
                        minYs = List.map .minY boxes
                        maxXs = List.map .maxX boxes
                        maxYs = List.map .maxY boxes
                    in
                    { minX = List.minimum minXs |> Maybe.withDefault 0
                    , minY = List.minimum minYs |> Maybe.withDefault 0
                    , maxX = List.maximum maxXs |> Maybe.withDefault 0
                    , maxY = List.maximum maxYs |> Maybe.withDefault 0
                    }

        GroupOutline sh ->
            getBoundingBoxWithTransform sh trans

        AlphaMask region sh ->
            intersectBoundingBox
                (getBoundingBoxWithTransform region trans)
                (getBoundingBoxWithTransform sh trans)

        Clip region sh ->
            intersectBoundingBox
                (getBoundingBoxWithTransform region trans)
                (getBoundingBoxWithTransform sh trans)

        Everything ->
            { minX = -10000, minY = -10000, maxX = 10000, maxY = 10000 }

        Notathing ->
            { minX = 0, minY = 0, maxX = 0, maxY = 0 }

        Link _ sh ->
            getBoundingBoxWithTransform sh trans

        Tap _ sh ->
            getBoundingBoxWithTransform sh trans

        TapAt _ sh ->
            getBoundingBoxWithTransform sh trans

        EnterShape _ sh ->
            getBoundingBoxWithTransform sh trans

        EnterAt _ sh ->
            getBoundingBoxWithTransform sh trans

        Exit _ sh ->
            getBoundingBoxWithTransform sh trans

        ExitAt _ sh ->
            getBoundingBoxWithTransform sh trans

        MouseDown _ sh ->
            getBoundingBoxWithTransform sh trans

        MouseDownAt _ sh ->
            getBoundingBoxWithTransform sh trans

        MouseUp _ sh ->
            getBoundingBoxWithTransform sh trans

        MouseUpAt _ sh ->
            getBoundingBoxWithTransform sh trans

        MoveOverAt _ sh ->
            getBoundingBoxWithTransform sh trans

        TouchStart _ sh ->
            getBoundingBoxWithTransform sh trans

        TouchEnd _ sh ->
            getBoundingBoxWithTransform sh trans

        TouchStartAt _ sh ->
            getBoundingBoxWithTransform sh trans

        TouchEndAt _ sh ->
            getBoundingBoxWithTransform sh trans

        TouchMoveAt _ sh ->
            getBoundingBoxWithTransform sh trans

        GraphPaper _ _ _ ->
            { minX = -10000, minY = -10000, maxX = 10000, maxY = 10000 }

-- Apply a transformation matrix to a bounding box
applyTransform : Transform -> BoundingBox -> BoundingBox
applyTransform ( (a, c, e), (b, d, f) ) box =
    let
        corners = [ (box.minX, box.minY), (box.minX, box.maxY), (box.maxX, box.minY), (box.maxX, box.maxY) ]
        transformed = List.map (\(x, y) -> (a * x + c * y + e, b * x + d * y + f)) corners
        xs = List.map Tuple.first transformed
        ys = List.map Tuple.second transformed
    in
    { minX = List.minimum xs |> Maybe.withDefault 0
    , minY = List.minimum ys |> Maybe.withDefault 0
    , maxX = List.maximum xs |> Maybe.withDefault 0
    , maxY = List.maximum ys |> Maybe.withDefault 0
    }

-- Compute the bounding box of a stencil
stencilBoundingBox : Stencil -> BoundingBox
stencilBoundingBox stencil =
    case stencil of
        Circle r ->
            { minX = -r, minY = -r, maxX = r, maxY = r }

        Rect w h ->
            { minX = -w / 2, minY = -h / 2, maxX = w / 2, maxY = h / 2 }

        RoundRect w h _ ->
            { minX = -w / 2, minY = -h / 2, maxX = w / 2, maxY = h / 2 }

        Oval w h ->
            { minX = -w / 2, minY = -h / 2, maxX = w / 2, maxY = h / 2 }

        Polygon points ->
            pointsBoundingBox points

        Path points ->
            pointsBoundingBox points

        BezierPath start points ->
            let
                allPoints = start :: List.concatMap (\(p1, p2) -> [p1, p2]) points
            in
            pointsBoundingBox allPoints


        Text (Face size bold italic _ _ _ font align) str ->
            let
                -- Base width estimate: adjust for font type
                charWidthFactor =
                    case font of
                        Serif -> 0.65
                        Sansserif -> 0.6
                        FixedWidth -> 0.7
                        Custom "Arial" -> 0.55 
                        Custom _ -> 0.6 -- Default for other custom fonts

                charWidth = size * charWidthFactor
                width = toFloat (String.length str) * charWidth
                height = size * 1.2
                -- Adjust x bounds based on text alignment
                (xMin, xMax) =
                    case align of
                        AlignLeft -> (0, width)
                        AlignCentred -> (-width / 2, width / 2)
                        AlignRight -> (-width, 0)
            in
            { minX = xMin
            , minY = -height / 2
            , maxX = xMax
            , maxY = height / 2
            }

-- Compute bounding box for a list of points
pointsBoundingBox : List (Float, Float) -> BoundingBox
pointsBoundingBox points =
    case points of
        [] ->
            { minX = 0, minY = 0, maxX = 0, maxY = 0 }
        _ ->
            let
                xs = List.map Tuple.first points
                ys = List.map Tuple.second points
            in
            { minX = List.minimum xs |> Maybe.withDefault 0
            , minY = List.minimum ys |> Maybe.withDefault 0
            , maxX = List.maximum xs |> Maybe.withDefault 0
            , maxY = List.maximum ys |> Maybe.withDefault 0
            }

-- Rotate a point around (0, 0)
rotatePoint : Float -> (Float, Float) -> (Float, Float)
rotatePoint angle (x, y) =
    let
        cosA = cos angle
        sinA = sin angle
    in
    (x * cosA - y * sinA, x * sinA + y * cosA)

-- Skew a point
skewPoint : Float -> Float -> (Float, Float) -> (Float, Float)
skewPoint skx sky (x, y) =
    (x + y * tan skx, x * tan sky + y)

-- Intersect two bounding boxes
intersectBoundingBox : BoundingBox -> BoundingBox -> BoundingBox
intersectBoundingBox box1 box2 =
    { minX = max box1.minX box2.minX
    , minY = max box1.minY box2.minY
    , maxX = min box1.maxX box2.maxX
    , maxY = min box1.maxY box2.maxY
    }