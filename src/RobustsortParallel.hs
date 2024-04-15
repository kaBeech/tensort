module RobustsortParallel (rsortpb, rsortpm, rsortpq) where

import BubblesortParallel qualified (bsortp)
import MergesortParallel qualified (msortp)
import QuicksortParallel qualified (qsortp)

rsortpb :: IO ()
rsortpb = BubblesortParallel.bsortp

rsortpm :: IO ()
rsortpm = MergesortParallel.msortp

rsortpq :: IO ()
rsortpq = QuicksortParallel.qsortp
