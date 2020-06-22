module PCA
  (
    pca,
    pcaTransform,
    pcaFull,
    pcaFullTransform
  ) where

import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Data as LAD

mean :: LAD.Matrix Double -> LAD.Matrix Double
mean a =
  let n = LAD.rows a
      v = (LAD.konst (recip . fromIntegral $ n) (1,n)) :: LAD.Matrix Double
  in v LA.<> a

deMean a = a - mean a

pca nComponents a =
  let (_, vec, uni) = LA.svd $ deMean a
      vec' = take nComponents (LAD.toList vec)
      uni' = LAD.fromColumns $ take nComponents (LAD.toColumns $ LAD.tr uni)
  in (vec', uni')

pcaTransform nComponents a =
  let (_, u) = pca nComponents a in (deMean a) LA.<> LAD.tr u

pcaFull a = pca (minimum (LAD.size a)) a
pcaFullTransform a = pcaTransform (minimum (LAD.size a)) a

main = do m <- LAD.loadMatrix "sample.txt";
          print m;
          print $ mean m;
          print $ pcaFull m;
          return $ pcaFullTransform m
          -- let obs = toRows m
          --     dat = array (1, length obs) (zip [1..] obs)
          -- print obs;
          -- return $ pca dat (-1)
