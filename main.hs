import GHC.Natural ( Natural )

data Maze = Maze {
  getMazeWidth :: Natural,
  getMazeGrid  :: [Bool] }

type Position = (Natural, Natural)

type Graph = [Node Position Index]
data Node v c = Node {
  getNodeValue       :: v,
  getNodeConnections :: [Index] }
type Index = Maybe Natural

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

repeatChar :: Char -> Natural -> String
repeatChar _ 0   = ""
repeatChar c len = c : repeatChar c (len-1)