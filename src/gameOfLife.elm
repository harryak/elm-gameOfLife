module GameOfLife where

-- The imports
import Mouse
import Window
import Graphics.Element as GElement
import Graphics.Collage as GCollage
import Graphics.Input as GInput
import Color
import Signal (..)
import Text
import Time
import Dict
import List
import Maybe

import Debug

import Html
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEv

-- own modules
import ListExtension as LEx

{-----------------------------------
 - Config
 -----------------------------------}
tileSize : Int
tileSize = 14

tileGap : Int
tileGap = 1

tileColorOn : Color.Color
tileColorOn = Color.black

tileColorOff : Color.Color
tileColorOff = Color.green

gameWidth : Int
gameWidth = 600

gameHeight : Int
gameHeight = 400

{-----------------------------------
 - Calculated config
 -----------------------------------}
tileNumberXDir : Int
tileNumberXDir = gameWidth // tileSize

tileNumberYDir : Int
tileNumberYDir = gameHeight // tileSize

{-----------------------------------
 - Models
 -----------------------------------}
type alias Board = Dict.Dict (Int, Int) (Bool, GCollage.Form)
type alias Game  = { board:Board,
                     state:State }
type State = Paused | Playing

type Update = Tile (Maybe (Int, Int)) | GameState State |Tick (Maybe ())

{-----------------------------------
 - Initial state
 -----------------------------------}
-- Use configuration to create the empty board.
-- Implementation note: This is rather complicated and holds the GCollage.Elements for fast access
emptyBoard : Board
emptyBoard = LEx.cartesianProduct (,) [0..tileNumberXDir] [0..tileNumberYDir]
                |>  List.map (
                        \pos ->  ( pos,
                                    ( False, getElementAt pos False )
                                  )
                    )
                |>  Dict.fromList

defaultGame : Game
defaultGame = { board = emptyBoard,
                state = Paused }

{-----------------------------------
 - Game rules
 -----------------------------------}
-- The tile neighbours that are in the bounds. Ugly, but fast.
tileNeighbours : (Int, Int) -> List (Int, Int)
tileNeighbours (x, y) = let px = x - 1
                            sx = x + 1
                            py = y - 1
                            sy = y + 1
                        in  if x > 0
                            then if y > 0
                                 then if x < tileNumberXDir
                                      then if y < tileNumberYDir
                                           then [(px, py), (px, y), (px, sy),
                                                 (x, py), (x, sy),
                                                 (sx, py), (sx, y), (sx, sy)]
                                           else [(px, py), (px, y),
                                                 (x, py),
                                                 (sx, py), (sx, y)]
                                      else if y < tileNumberYDir
                                           then [(px, py), (px, y), (px, sy),
                                                 (x, py), (x, sy)]
                                           else [(px, py), (px, y),
                                                 (x, py)]
                                 else if x < tileNumberXDir
                                      then [(px, y), (px, sy),
                                            (x, sy),
                                            (sx, y), (sx, sy)]
                                      else [(px, y), (px, sy),
                                            (x, sy)]
                            else if y > 0
                                 then if y < tileNumberYDir
                                      then [(x, py), (x, sy),
                                            (sx, py), (sx, y), (sx, sy)]
                                      else [(x, py),
                                            (sx, py), (sx, y)]
                                 else [(x, sy),
                                       (sx, y), (sx, sy)]

-- Shorthand for boolean decision whether a tile (if existing) lives. Returns thenVal if it does and elseVal if not.
tileLivesAndThen : comparable -> Dict.Dict comparable (Bool, c) -> (Maybe a, Maybe a) -> Maybe a
tileLivesAndThen key dict (thenVal, elseVal) =
    Dict.get key dict `Maybe.andThen` (
        \(bool, _) -> if bool then thenVal else elseVal
    )

-- Count the tiles which are alive around the given one.
getAliveTilesAround : (Int, Int) -> Dict.Dict (Int, Int) (Bool, c) -> Int
getAliveTilesAround pos dict =
    List.sum (
        List.filterMap (
            \pos' -> tileLivesAndThen pos' dict (Just 1, Just 0)
        ) (tileNeighbours pos)
    )

-- Decide if a tile lives in the next round. Uses bounds under, over and repr to determine
-- when a cell will live.
-- If the cell is alive and more than 'under', but less than 'over' neighbours live it stays
-- alive. Otherwise it dies due do UNDER- or OVERpopulation.
-- If the cell is dead and exactly 'repr' cells live around it, it will live due to REPRoduction
tileWillLive : (Int, Int, Int) -> (Int, Int) -> Dict.Dict (Int, Int) (Bool, c) -> Maybe Bool
tileWillLive (under, over, repr) pos dict =
    Dict.get pos dict `Maybe.andThen` (\(curLives, _) ->
        let aliveTilesAround = getAliveTilesAround pos dict
        in  if   curLives
            then if aliveTilesAround > under && aliveTilesAround < over
                 then (Just True)
                 else (Just False)
            else if aliveTilesAround == repr
                 then (Just True)
                 else (Just False)
    )

-- Using the above function with Conway's boundaries.
tileWillLiveConway = tileWillLive (1, 3, 3)

{-----------------------------------
 - Helpers for display
 -----------------------------------}
tilePosXDir : Int -> Float
tilePosXDir n = 
    let tileOffsetXDir = gameWidth // 2
    in  (n * (tileSize + tileGap)) - tileOffsetXDir |> toFloat

tilePosYDir : Int -> Float
tilePosYDir n = 
    let tileOffsetYDir = gameHeight // 2
    in  (n * (tileSize + tileGap)) - tileOffsetYDir |> toFloat

{-----------------------------------
 - Display
 -----------------------------------}
getElementAt : (Int, Int) -> Bool -> GCollage.Form
getElementAt (x, y) shapeFilled =
    let
        posX = tilePosXDir x
        posY = tilePosYDir y
        tileSizeFloat = tileSize |> toFloat
        
        shapeStyle =
            case shapeFilled of
                True  -> GCollage.filled tileColorOn
                False -> GCollage.filled tileColorOff
    in
        [GCollage.rect tileSizeFloat tileSizeFloat
            |> shapeStyle]
            |> GCollage.collage tileSize tileSize
            |> GInput.clickable (send channelTileUpdate (Tile (Just (x, y))))
            |> GCollage.toForm
            |> GCollage.move (posX, posY)

display : (Int, Int) -> Game -> GElement.Element
display (w, h) game =
    let _               = Debug.watch "game" game.state
        gameOffsetYDir  = (h - gameHeight) // 2
        playing         = game.state == Playing
        buttonSymbol    = if playing then "II" else ">"
        newGameState    = if playing then (GameState Paused) else (GameState Playing)
        playButtonStyle = HtmlAttr.style    [ ("width", "50px")
                                            , ("height", "50px")
                                            , ("font-size", "28pt")
                                            , ("background-color", "lightgray")
                                            , ("color", "darkgray")
                                            , ("border-radius", "5px")
                                            , ("text-align", "center")
                                            , ("font-weight", "bolder")
                                            , ("cursor", "pointer")]
        sideBarStyle = HtmlAttr.style   [ ("width", "150px")
                                        , ("height", (toString gameHeight) ++ "px")
                                        , ("padding-left", "25px")
                                        , ("margin-top", (toString gameOffsetYDir) ++ "px")]
        playButton = Html.div 
                        [ playButtonStyle
                        , HtmlEv.onClick (send channelPlayPause newGameState)
                        ]
                        [ Html.text buttonSymbol ]
        sideBar    = Html.div [ sideBarStyle ] [ playButton ]
                        |> Html.toElement 150 gameHeight
    in  GElement.flow GElement.right
            [ sideBar
            , GCollage.collage (w - 150) h
                (List.map (\(_, elem) -> elem) (Dict.values game.board))
            ]

{-----------------------------------
 - Channels and Signals
 -----------------------------------}
{-channelTileUpdate : Channel (Maybe (Int, Int))
channelTileUpdate = channel Nothing

channelPlayPause : Channel Bool
channelPlayPause = channel False-}

channelTileUpdate : Channel Update
channelTileUpdate = channel (Tile Nothing)

channelPlayPause : Channel Update
channelPlayPause = channel (GameState Paused)

-- updates of game-state
updateGame : Update -> Game -> Game
updateGame action game =
    let _ = Debug.watch "action" action
    in  case action of
            Tile (Just pos) -> updateTile pos game
            GameState state -> updateState state game
            Tick (Just ())  -> if game.state == Playing then updateBoard game else game
            _               -> game

updateState : State -> Game -> Game
updateState newState game = {game | state <- newState}

updateTile : (Int, Int) -> Game -> Game
updateTile pos game =
    let updateVal : Maybe (Bool, GCollage.Form) -> Maybe (Bool, GCollage.Form)
        updateVal value = case value of
                            Just (bool, _) -> Just (not bool, getElementAt pos (not bool))
                            _              -> Nothing
    in  {game | board <- Dict.update pos updateVal game.board}

updateBoard : Game -> Game
updateBoard game =
    let newBoard = Dict.map  (
                            \pos val -> case tileWillLiveConway pos game.board of
                                            Just lives -> (lives, getElementAt pos lives)
                                            _          -> val
                        ) game.board
    in  {game | board <- newBoard}

{-----------------------------------
 - Main-function
 -----------------------------------}
main : Signal GElement.Element
main = display <~ Window.dimensions
                ~ foldp updateGame defaultGame (
                        mergeMany   [ subscribe channelTileUpdate
                                    , subscribe channelPlayPause
                                    , (\time -> Tick (Just ()) ) <~ (Time.fps 0.5)
                                    ]
                  )
                -- ~ foldp updateTile defaultGame (subscribe channelTileUpdate)