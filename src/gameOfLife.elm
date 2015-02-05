module GameOfLife where

-- The imports from core libraries
import Dict
import Graphics.Element as GElement
import Graphics.Collage as GCollage
import Graphics.Input as GInput
import List
import Maybe
import Mouse
import Random
import Signal ((<~), (~))               -- can not use this otherwise
import Signal
import Text
import Time
import Window

-- imports from comunity packages
import Html
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEv

-- own modules
import ListExtension as LEx
import GameOfLifeConfig (..)

{-----------------------------------
 - Models
 -----------------------------------}
type alias Board = Dict.Dict (Int, Int) Bool
type alias Game  =  { board: Board
                    , state: State }
type State       = Paused | Playing

type TickType    = SingleStep | MultiStep
type Update      = Tile (Maybe (Int, Int)) | NewBoard Board | GameState State | Tick TickType | Reset

{-----------------------------------
 - Initial state
 -----------------------------------}
-- Use configuration to create the empty board.
emptyBoard : Board
emptyBoard =    LEx.cartesianProduct (,) (
                    LEx.cartesianProduct (,) [0..(tileNumberXDir - 1)] [0..(tileNumberYDir - 1)]
                ) [False]
                    |>  Dict.fromList

defaultGame : Game
defaultGame =   { board = emptyBoard
                , state = Paused }

{-----------------------------------
 - Random board generation
 -----------------------------------}
randomBoard : Random.Seed -> Board
randomBoard seed = 
    let possibleCoordinates = LEx.cartesianProduct (,) [0..(tileNumberXDir - 1)] [0..(tileNumberYDir - 1)]
        (propability, newSeed) = Random.generate (Random.int 1 9) seed
        (randomValues, _)   = Random.generate (Random.list (tileNumberXDir * tileNumberYDir) (Random.int 0 propability)) newSeed
        boardList           = LEx.zip possibleCoordinates randomValues
                                |> List.map (\(pos, flag) -> (pos, flag < 1))
    in  Dict.fromList boardList

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
tileWillLiveConway = tileWillLive (1, 4, 3)

{-----------------------------------
 - Helpers for display
 -----------------------------------}
tilePosXDir : Int -> Int -> Int
tilePosXDir x w =   let tileOffsetX = (w - gameWidth) // 2
                    in  (x * (tileSize + tileGap)) + tileOffsetX

tilePosYDir : Int -> Int -> Int
tilePosYDir y h =   let tileOffsetY = (h - gameHeight) // 2
                    in  (y * (tileSize + tileGap)) + tileOffsetY

getTileFrom : (Int, Int) -> (Int, Int) -> (Int, Int)
getTileFrom (x, y) (w, h) =
    let tileOffsetX = (w - gameWidth)  // 2
        tileOffsetY = (h - gameHeight) // 2
    in  ((x - tileOffsetX) // (tileSize + tileGap), (y - tileOffsetY) // (tileSize + tileGap))

{-----------------------------------
 - Display
 -----------------------------------}
display : (Int, Int) -> Game -> Random.Seed -> GElement.Element
display (w, h) game initSeed =
    let playing         = game.state == Playing
        buttonSymbol    = if playing then "II" else ">"
        newGameState    = if playing then (GameState Paused) else (GameState Playing)
        playButton      = Html.div 
                        [ HtmlAttr.class "button"
                        , HtmlEv.onClick (Signal.send channelPlayPause newGameState)
                        ]
                        [ Html.text buttonSymbol ]
        nextStepButton  = Html.div
                        [ HtmlAttr.classList
                            [ ("button", True)
                            , ("invisible", playing) ]
                        , HtmlEv.onClick (Signal.send channelTickGame (Tick SingleStep) )
                        ]
                        [ Html.text ">|" ]
        resetButton     = Html.div
                        [ HtmlAttr.classList
                            [ ("button", True)
                            , ("invisible", playing) ]
                        , HtmlEv.onClick (Signal.send channelResetGame Reset )
                        ]
                        [ Html.text "R" ]
        randomButton    = Html.div
                        [ HtmlAttr.classList
                            [ ("button", True)
                            , ("invisible", playing) ]
                        , HtmlEv.onClick (Signal.send channelRandomBoard (NewBoard (randomBoard initSeed)) )
                        ]
                        [ Html.text "Z" ]
        sideBar         = Html.div
                        [ HtmlAttr.id "sidebar"
                        , HtmlAttr.style [("top", (toString (tilePosYDir 0 h)) ++ "px")] ]
                        [ Html.div []
                            [ playButton
                            , nextStepButton
                            , resetButton
                            , randomButton ]
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
                                , HtmlEv.onClick (Signal.send channelTileUpdate (Tile (Just (x, y))) )
                                ] [ ]
                ) (Dict.toList game.board)) |> Html.toElement gameWidth gameHeight
            ]

{-----------------------------------
 - Channels and Signals
 -----------------------------------}
channelTileUpdate : Signal.Channel Update
channelTileUpdate = Signal.channel (Tile Nothing)

channelRandomBoard : Signal.Channel Update
channelRandomBoard = Signal.channel (NewBoard emptyBoard)

channelPlayPause : Signal.Channel Update
channelPlayPause = Signal.channel (GameState Paused)

channelTickGame : Signal.Channel Update
channelTickGame = Signal.channel (Tick SingleStep)

channelResetGame : Signal.Channel Update
channelResetGame = Signal.channel Reset

-- updates of game-state
updateGame : Update -> Game -> Game
updateGame action game =
    case action of
        Tile (Just pos)         -> if game.state == Playing then game else updateTile pos game
        NewBoard newBoard       -> {game | board <- newBoard}
        GameState state         -> updateState state game
        Tick MultiStep          -> if game.state == Playing then updateBoard game else game
        Tick SingleStep         -> if game.state == Paused  then updateBoard game else game
        Reset                   -> defaultGame
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
initialSeed : Signal.Signal Random.Seed
initialSeed = (\(time, _) -> Random.initialSeed (round time)) <~ Time.timestamp (Time.every Time.second)

main : Signal.Signal GElement.Element
main = display <~ Window.dimensions
                ~ Signal.foldp updateGame defaultGame (
                    Signal.mergeMany    [ Signal.subscribe channelTileUpdate
                                        , Signal.subscribe channelPlayPause
                                        , Signal.subscribe channelTickGame
                                        , Signal.subscribe channelResetGame
                                        , Signal.subscribe channelRandomBoard
                                        , (\_ -> Tick MultiStep ) <~ (Time.fps 1)
                                        , Signal.dropRepeats (( \(x, y) dim ->
                                                if x < 0 || y < 0
                                                then Tile Nothing
                                                else Tile (Just (getTileFrom (x, y) dim))
                                          ) <~ Signal.keepWhen Mouse.isDown (-1, -1) Mouse.position ~ Window.dimensions)
                                        ]
                  )
                ~ initialSeed