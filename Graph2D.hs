module Graph2D (Graph2D(..), Node2D(..), (Graph2D.!!)) where

import GHC.Natural ( Natural )

type Coordinate2D = (Natural, Natural)

type Graph2D v = [[Node2D v]]
data Node2D v = Node2D {
  getNode2DValue       :: v,
  getNode2DConnections :: [Index2D] }
type Index2D = Maybe Coordinate2D

(!!) :: Graph2D a -> Coordinate2D -> Node2D a
(!!) graph (x, y) = graph Prelude.!! fromIntegral y Prelude.!! fromIntegral x