import GHC.Natural ( Natural )
import qualified Graph2D

data Maze = Maze {
  getMazeWidth :: Natural,
  getMazeGraph  :: MazeGraph }
type MazeGraph = Graph2D.Graph2D Coordinate2D
type MazeNode = Graph2D.Node2D Coordinate2D

type Coordinate2D = (Natural, Natural)

printMaze :: Maze -> IO ()
printMaze maze = putStrLn $ unlines $ formatMaze maze ' ' '#'

formatMaze :: Maze -> Char -> Char -> [String]
formatMaze maze spaceSymbol wallSymbol = [topAndBottom, wrap (helper $ getMazeGraph maze Graph2D.!! (1, 1)), topAndBottom]
  where
    topAndBottom = repeatChar wallSymbol (getMazeWidth maze + 2)

    wrap :: String -> String
    wrap x = [wallSymbol] ++ x ++ [wallSymbol]

    helper :: MazeNode -> String
    helper node = "" -- incomplete

repeatChar :: Char -> Natural -> String
repeatChar _ 0   = ""
repeatChar c len = c : repeatChar c (len-1)