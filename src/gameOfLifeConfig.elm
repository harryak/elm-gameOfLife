module GameOfLifeConfig where

import Color

{-----------------------------------
 - Settable config
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