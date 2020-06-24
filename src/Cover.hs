module Cover where

import Types

import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Data as LAD


bounds :: RowData -> RectBound
bounds a =
  let cols = LAD.toColumns a
  in  (LAD.vector (map LAD.minElement cols),
       LAD.vector (map LAD.maxElement cols))

uniformFences1D :: (Integral a, Enum b, Fractional b) =>
  a -> b -> (b, b) -> ([b], [b])
uniformFences1D n p (x_min, x_max) =
  let dn = fromIntegral n
      leg = (x_max-x_min) / (dn - p * (dn-1))
      stride = (1 - p) * leg
      lowerFence j = x_min + j * stride
  in (map lowerFence [0..dn-1],
      map ((+leg).lowerFence) [0..dn-2] ++ [x_max])

genHyperCubes :: (Integral a) =>
  a -> Double -> RectBound -> [RectBound]
genHyperCubes n p bds =
  let (lowerBd, upperBd) = bds
      bds1D = zip (LAD.toList lowerBd) (LAD.toList upperBd)
      (lbs, ubs) = unzip $ map (uniformFences1D n p) bds1D
      lbPoints = map LAD.fromList (sequence lbs)
      ubPoints = map LAD.fromList (sequence ubs)
  in zip lbPoints ubPoints





uniformCover n p lens = ()
