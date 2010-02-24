module LightsOut where
import Data.List (intercalate)
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Text.Regex.Posix ((=~))

type Light = Bool
type Grid = [[Light]]

main :: IO ()
main = newGame

newGame :: IO ()
newGame = do grid <- newGrid 10 3
             play grid

newGrid :: Int -> Int -> IO Grid
newGrid w h = replicateM h $ replicateM w $ randomRIO (True, False) >>= return

width :: Grid -> Int
width  = length . head

height :: Grid -> Int
height = length

isGameOver :: Grid -> Bool
isGameOver grid = all and grid || not (any or grid)

prompt :: String
prompt = "> "

solvedGrid :: Int -> Int -> Grid
solvedGrid w h = take h (cycle [(take w (cycle [True]))])

play :: Grid -> IO ()
play grid = do clearScreen
               printGrid grid
               putStrLn "What do you want to do? (press 'h' for help)\n"
               if isGameOver grid
                 then mapM_ putStrLn ["YOU'VE WON!!!","Play again! Press 'n' to restart."]
                 else return ()
               putStr prompt
               input <- getLine
               case input of
                 "h"            -> help >> play grid
                 ('t':(' ':xy)) -> toggle xy grid >>= \g -> play g
                 "q"            -> quit
                 "s"            -> play (solvedGrid (width grid) (height grid))
                 "n"            -> newGame
                 _              -> play grid

clearScreen :: IO ()
clearScreen = putStrLn "\ESC[2J\ESC[H"

help :: IO ()
help = do clearScreen
          putStrLn $ unlines ["Welcome to lights out! The game is simple. The goal is to get all of the lights either on or off.",
                              "Toggling a light on or off also toggles it's adjacent neighbours.",
                              "",
                              "Here are some options:",
                              "\th     Show this help page.",
                              "\tt x,y Toggle the light at cell x,y (and adjacent neighbours).",
                              "\tn     New game",
                              "\ts     Solve current game",
                              "\tq     Quit the game",
                              "\n\n",
                              "[Press enter to continue]" ]
          getLine
          return ()

quit :: IO ()
quit = do clearScreen
          putStrLn "I miss you already.. Play again!"

toXY :: String -> Maybe (Int, Int)
toXY string = case string =~ "[0-9]+" :: [String] of
                [x, y] -> Just (read x::Int, read y::Int)
                _      -> Nothing

toggle :: String -> Grid -> IO Grid
toggle xy grid = case toXY xy of
                   Nothing    -> noLightExists >> return grid
                   Just (x,y) -> if outOfBounds (x,y) grid
                                   then noLightExists >> return grid
                                   else return $ toggleAt (x,y) grid

noLightExists :: IO ()
noLightExists = do clearScreen
                   putStrLn "That room doesn't exist! Try again.\n\n"
                   putStrLn "[Press enter to continue]"
                   getLine
                   return ()

toggleAt :: (Int, Int) -> Grid -> Grid
toggleAt (x,y) xs = foldr toggleAt' xs xys
                    where
                    xys = (x,y) : filter (not . flip outOfBounds xs) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

toggleAt' :: (Int, Int) -> Grid -> Grid
toggleAt' _     []     = []
toggleAt' (x,0) (y:ys) = modifyAt x not y : ys
toggleAt' (x,y) (z:zs) = z : toggleAt' (x, y - 1) zs

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt _ _ []     = []
modifyAt 0 f (x:xs) = f x : xs
modifyAt i f (x:xs) = x : (modifyAt (i - 1) f xs)

outOfBounds :: (Int, Int) -> Grid -> Bool
outOfBounds (x,y) grid | x < 0 || y < 0      = True
                       | x > width grid - 1  = True
                       | y > height grid - 1 = True
                       | otherwise           = False

isOn :: Light -> Bool
isOn = id

ansiColour :: String -> (String -> String)
ansiColour code = \s -> concat ["\ESC[", code, "m", s, "\ESC[0m"]

ansiYellow :: String -> String
ansiYellow = ansiColour "33"

ansiBlue :: String -> String
ansiBlue   = ansiColour "34"

--printGrid :: Grid -> IO ()
--printGrid xs = putStrLn $ (unlines .  map (concatMap lightAsString)) xs
--
printGrid xs = do putStrLn $ "   " ++ (intercalate "  " $ map show [0..(width xs - 1)])
                  let lights = map (concatMap lightAsString) xs
                      ys     = map ((++ " ") . show) [0..(height xs - 1)]
                  putStrLn $ unlines $ zipWith (flip (++)) lights ys

lightAsString :: Light -> String
lightAsString light | isOn light = ansiYellow "[=]"
                    | otherwise  = ansiBlue   "[=]"
