import GHC.Natural ( Natural )
import qualified Graph2D

data Maze = Maze {
  getMazeWidth :: Natural,
  getMazeGraph  :: MazeGraph }
type MazeGraph = Graph2D.Graph2D Coordinate2D

type Coordinate2D = (Natural, Natural)

printMaze :: Maze -> IO ()
printMaze maze = putStrLn $ unlines $ formatMaze maze ' ' '#'

formatMaze :: Maze -> Char -> Char -> [String]
formatMaze maze spaceSymbol wallSymbol = [topAndBottom, wrap (helper $ getMazeGraph maze), topAndBottom]
  where
    topAndBottom = repeatChar wallSymbol (getMazeWidth maze + 2)

    wrap :: String -> String
    wrap x = [wallSymbol] ++ x ++ [wallSymbol]

    helper :: MazeGraph -> String
    helper = map (\x -> if x then spaceSymbol else wallSymbol)

repeatChar :: Char -> Natural -> String
repeatChar _ 0   = ""
repeatChar c len = c : repeatChar c (len-1)