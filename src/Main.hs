{-#Language MultiWayIf#-}
module Go where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Map as M


data St = St Bool Bool M
data Box = Empty | Black Bool | White Bool | P deriving (Show, Eq)
type M = M.Map (Int,Int) Box
type Field = St

main :: IO ()
main = startGame

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

fieldSize@(fieldWidth, fieldHeight) = (9, 9) :: (Int, Int)
    
startGame = play
    (InWindow "Go" windowSize (240, 160))
    white 30
    (St False True emptyBord) -- initState
    renderer handler updater

windowSize = both (* (round cellSize)) fieldSize
cellSize = 40 :: Float

updater _ = id

handler (EventKey key Down _ mouse) st@(St f b bord) = case key of
    MouseButton LeftButton | not f -> case coord`M.lookup`bord of
        Just Empty      -> St False (not b) $ up b $ M.insert coord (if b then White False else Black False) bord
        _               -> st
    SpecialKey KeyEnter -> St (not f) b bord
    SpecialKey KeySpace -> St False True emptyBord
    _                   -> st
  where
    coord = (fromEnum $ fst $ screenToCell mouse,fromEnum  $ snd $ screenToCell mouse)
handler _ s = s


up :: Bool -> M -> M
up b m = del b $ iterate (\m -> myMap (\x y z -> let
    i = or [toBool (fff (fromJust ((x,y)`M.lookup`m))) $ o`M.lookup`m | o <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]]
    in case z of
        Black q -> Black $ q || i 
        White q -> White $ q || i 
        P       -> P
        _       -> Empty)
            m) (nullBord m) !! 100

fff (Black i) = False
fff (White i) = True
fff Empty = True
fff _     = False


toBool b a = case a of
    Just Empty               -> True
    Just (Black i) | not b   -> i
    Just (White i) | b       -> i
    _                        -> False

nullBord = myMap (\_ _ x -> toNull x)

toNull a = case a of
    Empty -> Empty
    Black _ -> Black False
    White _ -> White False

del b = myMap $ \_ _ x -> case x of
    Black x | x || not b -> Black True
    White x | x || b     -> White True
    _                    -> Empty

screenToCell = both (round . (/ cellSize)) . invertViewPort viewPort
viewPort = ViewPort (both (negate . (/ 2) . (subtract cellSize)) $ cellToScreen fieldSize) 0 1

renderer (St True _ bord) = pictures [label $ show $ res bord]
renderer (St _ _ bord ) = applyViewPortToPicture viewPort $
     pictures $ [uncurry translate (cellToScreen (x, y)) $ drawCell x y 
                | x <- [0.. fieldWidth-1], y <- [0 .. fieldHeight-1]] ++ grid
    where
    drawCell x y = case M.lookup (x, y) bord of
        Just (Black _) -> pictures [color black $ rectangleSolid cellSize cellSize]
        Just (White _) -> pictures [color white $ rectangleSolid cellSize cellSize]
        Just P         -> pictures [color red $ rectangleSolid cellSize cellSize]
        _ -> color green $ rectangleSolid cellSize cellSize --клетка пустая

grid = [uncurry translate (cellToScreen (x, y)) $ color black $ rectangleWire cellSize cellSize | x <- [0 .. fieldWidth - 1], y <- [0 .. fieldHeight - 1]]

cellToScreen = both ((* cellSize) . fromIntegral)   

emptyBord :: M.Map (Int,Int) Box
emptyBord = M.fromList
    [((x,y),if x`elem`[-1,9] || y`elem`[-1,9] then P else Empty)
    | x <- [-1..9], y <- [-1..9]]




myMap :: (Int -> Int -> Box -> Box) -> M.Map (Int,Int) Box -> M.Map (Int,Int) Box
myMap f bord = M.fromList $ [
    ((x,y),
        if x`elem`[-1,9] || y`elem`[-1,9]
            then P
            else f x y $ fromJust $ (x,y)`M.lookup`bord)
    | x <- [-1..9], y <-[-1..9]]


myFold :: (b -> a -> b) -> b -> (Int -> Int -> s -> a) -> M.Map (Int, Int) s -> b
myFold f1 n f2 m = foldl f1 n [ f2 x y $ fromJust $ (x,y)`M.lookup`m | x <- [0..8],y <-[0..8]]


res m = myFold (+) 0 (\ _ _ x -> case x of Black _ -> -1; White _ -> 1; _ -> 0) m

fromJust (Just a) = a
label = translate (-5) (-5) . scale 0.15 0.15 . color black . text
