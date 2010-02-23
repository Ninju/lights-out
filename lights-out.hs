module Main where

main = newGame
newGame = play (newGrid 5 5)
newGrid w h = []

printNewLines n = putStrLn (take n (cycle "\n"))

prompt = "> "

play grid = do putStrLn (show grid)
               putStrLn "What do you want to do? (press 'h' for help)\n"
               if gameOver grid
                 then do putStrLn "YOU'VE WON!!!"
                         putStrLn "Play again! Press 'n' to restart."
               putStr prompt
               input <- getLine
               case input of
                 "h"                 -> help >> play grid
                 ('t':(' ':address)) -> toggle (toXY address) grid >>= \g -> play g
                 "q"                 -> quit
                 "s"                 -> play (take (height grid) (cycle (take (width grid) (cycle [True]))))
                 "n"                 -> newGame
                 _                   -> play grid

clearScreen = putStrLn "\ESC[2J"


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
                              "\n\n\n",
                              "[Press enter to continue]" ]
          getLine

quit = do clearScreen
          putStrLn "I miss you already.. Play again!"

-- string: "x,y" or "x, y"
toXY string = (0,0)

toggle (x,y) grid = if outOfBounds (x,y) grid
                       then do clearScreen
                               putStrLn "That room doesn't exist! Try again."
                               printNewLines 3
                               putStrLn "[Press enter to continue]"
                               getLine
                               return grid
                        else return $ toggleAt (x,y) grid

toggleAt (x,y) xs = map toggleAt' $ filter (not . outOfBounds) [(x,y), (x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

toggleAt' _     []     = []
toggleAt' (x,0) (y:ys) = modifyAt x not y : ys
toggleAt' (x,y) (z:zs) = z : toggleAt' (x, y - 1) zs

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt _ _ []     = []
modifyAt 0 f (x:xs) = f x : xs
modifyAt i f (x:xs) = x : (modifyAt (i - 1) f xs)

outOfBounds (x,y) grid | x < 0 || y < 0      = True
                       | x > width grid - 1  = True
                       | y > height grid - 1 = True
                       | otherwise           = False
