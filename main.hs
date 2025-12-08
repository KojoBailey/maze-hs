type Position = (Integer, Integer)

data Maze = Maze {
  getMazeWidth :: Integer,
  getMazeGrid  :: [Bool] }

printMaze :: Maze -> IO ()
printMaze maze = putStrLn $ unlines $ formatMaze maze ' ' '#'

formatMaze :: Maze -> Char -> Char -> [String]
formatMaze maze spaceSymbol wallSymbol = [topAndBottom, wrap (helper $ getMazeGrid maze), topAndBottom]
  where
    topAndBottom = repeatChar wallSymbol (getMazeWidth maze + 2)

    wrap :: String -> String
    wrap x = [wallSymbol] ++ x ++ [wallSymbol]

    helper :: [Bool] -> String
    helper = map (\x -> if x then spaceSymbol else wallSymbol)

repeatChar :: Char -> Integer -> String
repeatChar _ 0   = ""
repeatChar c len = c : repeatChar c (len-1)