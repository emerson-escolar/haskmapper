module Types where

import qualified Numeric.LinearAlgebra.Data as LAD
import qualified Data.Set as DS

type RowData = LAD.Matrix Double

type Point = LAD.Vector Double

type RectBound = (Point, Point)

type Cluster = DS.Set Point
