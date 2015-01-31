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
tileSize = 18

tileGap : Int
tileGap = 2

tileColorOn : Color.Color
tileColorOn = Color.black

tileColorOff : Color.Color
tileColorOff = Color.green

gameWidth : Int
gameWidth = 800

gameHeight : Int
gameHeight = 600

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
type alias Board = Dict.Dict (Int, Int) Bool
type alias Game  =  { board: Board
                    , state: State
                    , speed: Float }
type State       = Paused | Playing

type TickType    = SingleStep | MultiStep
type Update      = Tile (Maybe (Int, Int)) | GameState State |Tick (Maybe TickType)

{-----------------------------------
 - Initial state
 -----------------------------------}
-- Use configuration to create the empty board.
-- Implementation note: This is rather complicated and holds the GCollage.Elements for fast access
emptyBoard : Board
emptyBoard =    LEx.cartesianProduct (,) (
                    LEx.cartesianProduct (,) [0..tileNumberXDir] [0..tileNumberYDir]
                ) [False]
                    |>  Dict.fromList

defaultGame : Game
defaultGame =   { board = emptyBoard
                , state = Paused
                , speed = 0.5 }

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
tileLivesAndThen : comparable -> Dict.Dict comparable Bool -> (Maybe a, Maybe a) -> Maybe a
tileLivesAndThen key dict (thenVal, elseVal) =
    Dict.get key dict `Maybe.andThen` (
        \bool -> if bool then thenVal else elseVal
    )

-- Count the tiles which are alive around the given one.
getAliveTilesAround : (Int, Int) -> Dict.Dict (Int, Int) Bool -> Int
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
tileWillLive : (Int, Int, Int) -> (Int, Int) -> Dict.Dict (Int, Int) Bool -> Maybe Bool
tileWillLive (under, over, repr) pos dict =
    Dict.get pos dict `Maybe.andThen` (\curLives ->
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
tilePosXDir : Int -> Int -> Int
tilePosXDir x w =   let tileOffsetX = (w - gameWidth) // 2
                    in  (x * (tileSize + tileGap)) + tileOffsetX

tilePosYDir : Int -> Int -> Int
tilePosYDir y h =   let tileOffsetY = (h - gameHeight) // 2
                    in  (y * (tileSize + tileGap)) + tileOffsetY

{-----------------------------------
 - Display
 -----------------------------------}
display : (Int, Int) -> Game -> GElement.Element
display (w, h) game =
    let _               = Debug.watch "game" game.state
        playing         = game.state == Playing
        buttonSymbol    = if playing then "II" else ">"
        newGameState    = if playing then (GameState Paused) else (GameState Playing)
        playButton = Html.div 
                        [ HtmlAttr.class "button"
                        , HtmlEv.onClick (send channelPlayPause newGameState)
                        ]
                        [ Html.text buttonSymbol ]
        nextStepButton = Html.div
                        [ HtmlAttr.class "button"
                        , HtmlEv.onClick (send channelTickGame (Tick (Just SingleStep)) )
                        ]
                        [ Html.text ">|" ]
        sideBar    = Html.div
                        [ HtmlAttr.id "sidebar"
                        , HtmlAttr.style [("top", (toString (tilePosYDir 0 h)) ++ "px")] ]
                        [ Html.div []
                            [ playButton
                            , nextStepButton ]
                        ]
                        |> Html.toElement 150 h
    in  GElement.flow GElement.right
            [ sideBar
            , Html.div [ HtmlAttr.id "boardContainer" ] (
                List.map (
                    \((x, y), alive) -> 
                        let xPos = tilePosXDir x w
                            yPos = tilePosYDir y h
                        in  Html.div
                                [ HtmlAttr.style 
                                    [ ("left", (toString xPos) ++ "px")
                                    , ("top", (toString yPos) ++ "px")
                                    , ("height", (toString tileSize) ++ "px")
                                    , ("width", (toString tileSize) ++ "px")
                                    ]
                                , HtmlAttr.classList
                                    [ ("tile", True)
                                    , ("alive", alive)
                                    , ("dead", not alive)
                                    ]
                                , HtmlEv.onClick (send channelTileUpdate (Tile (Just (x, y))) )
                                ] [ ]
                ) (Dict.toList game.board)) |> Html.toElement gameWidth gameHeight
            ]

{-----------------------------------
 - Channels and Signals
 -----------------------------------}
channelTileUpdate : Channel Update
channelTileUpdate = channel (Tile Nothing)

channelPlayPause : Channel Update
channelPlayPause = channel (GameState Paused)

channelTickGame : Channel Update
channelTickGame = channel (Tick Nothing)

-- updates of game-state
updateGame : Update -> Game -> Game
updateGame action game =
    let _ = Debug.watch "action" action
    in  case action of
            Tile (Just pos)         -> if game.state == Playing then game else updateTile pos game
            GameState state         -> updateState state game
            Tick (Just MultiStep)   -> if game.state == Playing then updateBoard game else game
            Tick (Just SingleStep)  -> if game.state == Paused  then updateBoard game else game
            _                       -> game

updateState : State -> Game -> Game
updateState newState game = {game | state <- newState}

updateTile : (Int, Int) -> Game -> Game
updateTile pos game =
    let updateVal : Maybe Bool -> Maybe Bool
        updateVal value = case value of
                            Just bool -> Just (not bool)
                            _         -> Nothing
    in  {game | board <- Dict.update pos updateVal game.board}

updateBoard : Game -> Game
updateBoard game =
    let newBoard = Dict.map (
                            \pos val -> case tileWillLiveConway pos game.board of
                                            Just lives -> lives
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
                                    , (\_ -> Tick (Just MultiStep) ) <~ (Time.fps 1)
                                    , subscribe channelTickGame
                                    ]
                  )
                -- ~ foldp updateTile defaultGame (subscribe channelTileUpdate)