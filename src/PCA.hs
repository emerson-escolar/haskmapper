module PCA
  (
    pca,
    pcaTransform,
    pcaFull,
    pcaFullTransform
  ) where

import Types

import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Data as LAD

mean :: RowData -> RowData
mean a =
  let n = LAD.rows a
      v = (LAD.konst (recip . fromIntegral $ n) (1,n)) :: RowData
  in v LA.<> a

deMean :: RowData -> RowData
deMean a = a - mean a

pca :: Int -> RowData -> ([Double], RowData)
pca nComponents a =
  let (_, vec, uni) = LA.svd $ deMean a
      singVals = take nComponents (LAD.toList vec)
      pcaAxes = LAD.fromRows $ take nComponents (LAD.toRows $ LAD.tr uni)
  in (singVals, pcaAxes)

pcaTransform :: Int -> RowData -> RowData
pcaTransform nComponents a =
  let (_, u) = pca nComponents a in (deMean a) LA.<> LAD.tr u

pcaFull a = pca (minimum (LAD.size a)) a
pcaFullTransform a = pcaTransform (minimum (LAD.size a)) a

main = do m <- LAD.loadMatrix "sample.txt";
          print m;
          print $ mean m;
          print $ pcaFull m;
          return $ pcaFullTransform m

fooBar =
  (LAD.loadMatrix "sample.txt") >>=
  (\m -> return $ LA.svd m) >>=
  (\t -> (let (u,s,v) = t in return $ u LA.<> (LAD.diagRect 0 s 4 3) LA.<> LAD.tr v))
