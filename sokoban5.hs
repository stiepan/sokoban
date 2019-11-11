import System.IO

{- IO -}

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    mainLoop

mainLoop = runInteraction (resettable (withStartScreen (withUndo walk)))


data Interaction world = Interaction
    world
    (Event -> world -> world)
    (world -> Picture)
data Event = KeyPress String deriving Show

data Colour = NoColour | Red | Green | Yellow | Blue | Magenta | Cyan deriving Eq
data Point = Point Char Colour
type Canvas = Integer -> Integer -> Colour -> Point
type Picture = Canvas -> Canvas

blank = id
(&) = (.)


point :: Char -> Picture
point c _ 0 0 = Point c
point _ bg x y = bg x y


textDepicted :: String -> Picture
textDepicted l = translated (-(listLength l) `div` 2) 0 (depicted l)
    where
    depicted = foldList (\e a -> (point e) & (translated 1 0 a)) blank


translated :: Integer -> Integer -> Picture -> Picture
translated tx ty p bg x y = p stillbg (x - tx) (y - ty)
    where stillbg a b = bg (a + tx) (b + ty)


coloredPoint :: Colour -> Point -> Point
coloredPoint clr (Point c _) = Point c clr


colored :: Colour -> Picture -> Picture
colored colour p bg x y clr = p retainClrBg x y colour
    where retainClrBg x y _ = bg x y clr


pictures :: [Picture] -> Picture
pictures = foldList (&) blank


putPoint :: Point -> IO ()
putPoint (Point c colour) = 
    if colour == NoColour then putChar c
    else putStr (scol colour) >> putChar c >> putStr "\ESC[30;47m"
    where 
    scol Red = "\ESC[31m"
    scol Green = "\ESC[32m"
    scol Yellow = "\ESC[33m"
    scol Blue = "\ESC[34m"
    scol Magenta = "\ESC[35m"
    scol Cyan = "\ESC[36m"


drawingOf :: Picture -> IO ()
drawingOf p = foldList (>>) (return ()) (spit (p background))
    where
    background _ _ colour = Point ' ' colour
    spit canvas = [
        if x == 40 then putChar '\n' else putPoint (canvas x y NoColour) 
        | y <- [11, 10..(-11)], x <- [-39..40]]


runInteraction :: Interaction s -> IO ()
runInteraction (Interaction s0 eHandler depictedState) = 
    (drawState s0) >> (go s0 "")
    where
    drawState s = do
        putStr "\ESCc\ESC[30;47m"
        drawingOf (depictedState s)
    go s pref = do
        input <- getChar
        if doesQuit input then putStr "\ESCc\ESC[39;49m"
        else let (npref, event) = pressedKey pref input in
             case event of Nothing -> go s npref
                           Just key -> do
                            let snext = eHandler key s 
                            drawState snext
                            go snext npref
    

pressedKey :: [Char] -> Char -> ([Char], Maybe Event)
pressedKey = pressed
    where
    back = ([], Just (KeyPress "U"))
    esc = ([], Just (KeyPress "Esc"))
    pressed _ 'u' = back
    pressed _ 'U' = back
    pressed _ 'e' = esc
    pressed _ 'E' = esc
    pressed _ ' ' = ([], Just (KeyPress " "))
    pressed _ '\ESC' = ("\ESC", Nothing)
    pressed "\ESC" '[' = ("\ESC[", Nothing)
    pressed "\ESC[" c
        | c == 'D' = ([], Just (KeyPress "Left"))
        | c == 'A' = ([], Just (KeyPress "Up"))
        | c == 'C' = ([], Just (KeyPress "Right"))
        | c == 'B' = ([], Just (KeyPress "Down"))
    pressed _ _ = ([], Nothing)



doesQuit :: Char -> Bool
doesQuit 'q' = True
doesQuit 'Q' = True
doesQuit _ = False


{-- List functions --}


foldList :: (a -> b -> b) -> b -> [a] -> b
foldList _ a [] = a
foldList f a (h:t) = f h (foldList f a t)


elemList :: Eq a => a -> [a] -> Bool
elemList e = foldList ((||).(e == )) False


appendList :: [a] -> [a] -> [a]
appendList = flip (foldList (:))


listLength :: [a] -> Integer
listLength = foldList (\_ -> (1+)) 0


filterList :: (a -> Bool) -> [a] -> [a]
filterList p = foldList (\h -> if p h then (h:) else id) [] 


maybeNth :: [a] -> Integer -> Maybe a
maybeNth = foldList dropFirstFew (\_ -> Nothing)
    where 
        dropFirstFew h _ 1 = Just h
        dropFirstFew _ innerDrop n = innerDrop (n - 1)

nth :: [a] -> Integer -> a
nth l n = e where (Just e) = maybeNth l n


mapList :: (a -> b) -> [a] -> [b]
mapList f = foldList ((:).f) []


andList :: [Bool] -> Bool
andList = foldList (&&) True


allList :: (a-> Bool) -> [a] -> Bool
allList p = foldList ((&&).p) True


flattened :: [[a]] -> [a]
flattened = foldList (++) []


{-- Sokoban --}


type Moves = Integer
data WithUndo a = WithUndo a [a]
data GameState state = StartScreen | Running state
data PendingMazes = Pending Maze [Maze]
data State = GS PendingMazes LevelState deriving Eq
data LevelState = LS PlayerPosition BoxesLoc Moves deriving Eq
data PlayerPosition = PP Coord Direction deriving Eq
data Direction = R | U | L | D deriving Eq


instance Eq PendingMazes where
  Pending _ p1 == Pending _ p2 = listLength p1 == listLength p2


{-- Board --}


type Board = Coord -> Tile
data Maze = Maze Coord Board 
data Coord = C Integer Integer deriving (Eq, Show)
data Tile = Wall | Ground | Storage | Box | Blank deriving (Enum, Eq, Show)
type BoxesLoc = [Coord]
type Neighbours = Board -> Coord -> [Coord]


mazes, badMazes :: [Maze]
mazes = [defaultLevel, level2, level3]
badMazes = [openLevel, insaneLevel, blatantlyWrongMaze]


defaultLevel :: Maze
defaultLevel = Maze start maze
  where
    start = (C (-3) 3)
    maze (C x y)
      | abs x > 4  || abs y > 4  = Blank
      | abs x == 4 || abs y == 4 = Wall
      | x ==  2 && y <= 0        = Wall
      | x ==  3 && y <= 0        = Storage
      | x >= -2 && y == 0        = Box
      | otherwise                = Ground      


level2 :: Maze
level2 = Maze start maze
  where
    start = (C 0 0)
    maze (C x y)
      | abs x > 4 || abs y > 4               = Blank
      | abs x == 4 || abs y == 4             = Wall
      | abs y == 2 && abs x > 1              = Wall
      | y == 0 && (abs x == 2 || abs x == 1) = Box
      | abs y == 3 && abs x == 3             = Storage
      | otherwise                            = Ground


level3 :: Maze
level3 = Maze start maze
  where
    start = (C 0 0)
    maze (C x y)
      | abs x > 4 || y > 4 || y < (-2)    = Blank
      | y == 4 || y == (-2) || abs x == 4 = Wall
      | y == (-1) && x /= 0 && x /= (-1)  = Wall
      | y == 1 && x > -1 && x < 3         = Wall
      | y == 3 && x > -1                  = Wall
      | y == 2 && x > 0 && x < 4          = Storage
      | y == 2 && x == 0                  = Box
      | y == 0 && (x == -1 || x == -2)    = Box
      | otherwise                         = Ground


openLevel :: Maze
openLevel = Maze start maze
  where
    start = (C (-3) 3)
    maze (C x y)
      | abs x > 4  || abs y > 4  = Blank
      | abs x == 4 || y == 4 = Wall
      | x ==  2 && y <= 0        = Wall
      | x ==  3 && y <= 0        = Storage
      | x >= -2 && y == 0        = Box
      | otherwise                = Ground      


insaneLevel :: Maze
insaneLevel = Maze start maze
  where
    start = (C (-3) 3)
    maze (C x y)
      | abs x > 4  || abs y > 4  = Blank
      | abs x == 4 || abs y == 4 = Wall
      | x ==  2 && y <= 0        = Wall
      | x ==  3 && y <= -1       = Storage
      | x >= -2 && y == 0        = Box
      | otherwise                = Ground 


blatantlyWrongMaze :: Maze
blatantlyWrongMaze = Maze start maze
  where 
    (Maze _ maze) = defaultLevel
    start = C 100 100


adherent :: Coord -> [Coord]
adherent (C x y) = [C (x + a) (y + b) | a <- [-1..1], b <- [-1..1], (abs a) + (abs b) == 1]


noWallTrespassing :: Neighbours
noWallTrespassing board c = filterList (\v -> (board v) /= Wall)(adherent c)


retrieveCoords :: Neighbours -> Maze -> Tile -> [Coord]
retrieveCoords neighbours (Maze initial board) tile = foldGraph pickTile (neighbours board) initial []
    where pickTile u picked = if board u == tile then u:picked else picked


getReachable :: Maze -> Tile -> [Coord]
getReachable = retrieveCoords noWallTrespassing


isClosed :: Maze -> Bool
isClosed (Maze initial board) = (isOk initial) && noBlankAccessible
    where
        noBlankAccessible = isGraphClosed initial (noWallTrespassing board) notBlank
        isOk v = elemList (board v) [Ground, Storage]
        notBlank c = board c /= Blank


isSane :: Maze -> Bool
isSane maze = boxNo <= storageNo
    where 
      (boxNo:storageNo:_) = mapList (listLength.(getReachable maze)) [Box, Storage]


initialBoxes :: Maze -> BoxesLoc
initialBoxes maze = getReachable maze Box


removeBoxes :: Board -> Board
removeBoxes m = eraseBox . m
  where eraseBox Box = Ground
        eraseBox t = t


addBoxes :: BoxesLoc -> Board -> Board
addBoxes [] = id
addBoxes (c:crds) = (addBox c) . (addBoxes crds)
  where
    addBox boxCoord board coord
        | boxCoord == coord = Box
        | otherwise = board coord


rearrangedBoard :: BoxesLoc -> Board -> Board
rearrangedBoard bxs = addBoxes bxs . removeBoxes


legalCoord :: Coord -> Direction -> Board -> Coord
legalCoord c d b = if elemList (b nc) [Storage, Ground] then nc else c
  where
    nc = adjacentCoord d c


movedBox :: Board -> Coord -> Direction -> BoxesLoc -> BoxesLoc
movedBox board coord d bxs = mapList moveParticularBox bxs
  where
    currentBoard = rearrangedBoard bxs board
    moveParticularBox c 
      | coord == c = legalCoord c d currentBoard
      | otherwise = c


adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x + 1) y
adjacentCoord L (C x y) = C (x - 1) y
adjacentCoord U (C x y) = C x (y + 1)
adjacentCoord D (C x y) = C x (y - 1)
 

{-- Graphics --}


wall, ground, storage, box, blankTile :: Picture


wall = colored Cyan (point '#')
storage = colored Red (point '.')
box = colored Blue (point '$')
blankTile = point ' '
ground = point ' '


player :: Direction -> Picture
player R = point '>'
player L = point '<'
player _ = point '@'


wonScreen :: State -> Picture
wonScreen (GS _ (LS _ _ mvs)) = info
  where info = textDepicted ("Poziom ukończony, liczba ruchów: " ++ (show mvs))


etap4 = pictureOfBools(mapList (\maze -> isClosed maze && isSane maze) allmzs)
  where allmzs = appendList mazes badMazes


withStartScreen :: Interaction state -> Interaction (GameState state)
withStartScreen (Interaction initial handle drawState) = 
  Interaction initial' handle' drawState'
  where
    initial' = StartScreen
    
    handle' (KeyPress key) StartScreen
         | key == " " = Running initial
    handle' _ StartScreen = StartScreen
    handle' e (Running s) = Running (handle e s)
    
    drawState' StartScreen = etap4
    drawState' (Running s) = drawState s


resettable :: Interaction state -> Interaction state
resettable (Interaction initial handle drawState) = 
  Interaction initial handle' drawState
  where handle' (KeyPress key) _ | key == "Esc" = initial
        handle' e s = handle e s


withUndo :: Eq a => Interaction a -> Interaction (WithUndo a)
withUndo (Interaction state0 handle draw) = Interaction state0' handle' draw'
  where
    state0' = WithUndo state0 []
    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
       where s' = handle e s
    draw' (WithUndo s _) = draw s


walk :: Interaction State
walk = Interaction initialState handleEvent draw


initialState :: State
initialState = GS (Pending maze pending) (initialLevelState maze)
  where (maze:pending) = mazes


initialLevelState :: Maze -> LevelState
initialLevelState maze = LS (PP coord U) boxesLoc 0
  where
    (Maze coord _) = maze
    boxesLoc = initialBoxes maze


handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s
      | key == "Right" = handleDirection R
      | key == "Up"    = handleDirection U
      | key == "Left"  = handleDirection L
      | key == "Down"  = handleDirection D
      | key == " " = nextLevel
      where
        nextLevel
          | isWinning s = newLevel s
          | otherwise = s
          where
            newLevel (GS (Pending _ []) _) = s
            newLevel (GS (Pending _ (newMaze:pending)) _) = 
              GS (Pending newMaze pending) (initialLevelState newMaze)
        handleDirection d
          | isWinning s = s
          | otherwise = GS mazesState (handleLevelState levelState)
          where
            (GS mazesState levelState) = s
            (Pending (Maze _ board) _) = mazesState
            handleLevelState (LS (PP c _) bxs moves) = LS npp nbxs (moves + 1)
              where
                nbxs = movedBox board (adjacentCoord d c) d bxs
                npp = PP (legalCoord c d (rearrangedBoard nbxs board)) d

handleEvent _ s = s


isWinning :: State -> Bool
isWinning (GS (Pending (Maze _ board) _) (LS _ bxs _)) = allList boxInStorage bxs
  where
    boxInStorage c = board c == Storage


{-- Drawing states --}


pictureOfBools :: [Bool] -> Picture
pictureOfBools xs = translated (-fromIntegral k `div` 2) (fromIntegral k) (go 0 xs)
  where n = length xs
        k = findK 0 -- k is the integer square of n
        findK i | i * i >= n = i
                | otherwise  = findK (i+1)
        go _ [] = blank
        go i (b:bs) =
          translated (fromIntegral (i `mod` k))
                     (-fromIntegral (i `div` k))
                     (pictureOfBool b)
          & go (i+1) bs

        pictureOfBool True =  colored Green (point '*')
        pictureOfBool False = colored Red   (point 'x')


drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage 
drawTile Box     = box
drawTile Blank   = blankTile


drawPlayer :: PlayerPosition -> Picture
drawPlayer (PP (C x y) d) = 
  translated (fromIntegral x) (fromIntegral y) (player d)


draw :: State -> Picture
draw s 
  | isWinning s = wonScreen s
  | otherwise = (drawPlayer pos) & (pictureOfMaze rearrangedMaze)
    where (GS (Pending (Maze initial board) _) (LS pos bxs _)) = s
          rearrangedMaze = Maze initial (rearrangedBoard bxs board)


pictureOfMaze :: Maze -> Picture
pictureOfMaze maze = picture
  where
    ((left, right), (bottom, top)) = mazeLimits maze
    (Maze _ board) = maze
    row y = pictures([translated (fromIntegral x) 0 (drawTile (board (C x y))) | 
                      x <- [(left - 5)..(right  + 5)]])
    picture = pictures([translated 0 (fromIntegral y) (row y) |
                      y <- [(bottom - 5)..(top + 5)]])


{-- Graph functions --}


mazeLimits :: Maze -> ((Integer, Integer), (Integer, Integer))
mazeLimits (Maze initial board) = foldGraph extendedLimits neighbours initial ac
  where
    ac = ((0, 0), (0, 0))
    neighbours = (noWallTrespassing board)
    extendedLimits (C x y) ((left, right), (bottom, top)) = 
      (((min left x), (max right x)), ((min bottom y), (max top y)))


foldGraph :: Eq a => (a -> t -> t) -> (a -> [a]) -> a -> t -> t
foldGraph f neighbours v a0 = dfs [v] []
    where 
        dfs [] _ = a0
        dfs (u:us) visited
            | elemList u visited = dfs us visited
            | otherwise = f u (dfs (appendList (neighbours u) us) (u:visited))


foldGraphUntil :: Eq t => (t -> Bool) -> (t -> [t]) -> t -> Bool
foldGraphUntil cond neighbours v = foldGraph areYouTheOne neighbours v False
    where areYouTheOne u visitRest = cond u || visitRest


isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk = not (foldGraphUntil (not.isOk) neighbours initial)


reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours = foldGraphUntil (== v) neighbours initial


allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours = allList (\v -> reachable v initial neighbours) vs
