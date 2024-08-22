module Benchmarking.MakeDeckShuffleChart (makeDeckShuffleChart) where

import Benchmarking.Score (getTotalErrorsScore)
import Data.Tensort.OtherSorts.Mergesort (mergesort)
import Data.Tensort.OtherSorts.Quicksort (quicksort)
import Data.Tensort.Robustsort (robustsortB, robustsortM)
import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Tensort (tensortBL)
import Data.Tensort.Utils.Types (SortAlg)
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy

wonkyChance :: Int
wonkyChance = 10

stuckChance :: Int
stuckChance = 0

score :: SortAlg -> Int
score sortAlg =
  getTotalErrorsScore
    1000
    sortAlg
    56
    wonkyChance
    stuckChance
    `div` 1000

titles :: [String]
titles = ["Total card positional error (average)"]

values :: [(String, [Int])]
values =
  [ ("Quick", [score quicksort]),
    ("Merge", [score mergesort]),
    ("Bubble", [score bubblesort]),
    ("Tensor", [score tensortBL]),
    ("Mundane Robust", [score robustsortB]),
    ("Magic Robust", [score robustsortM])
  ]

makeDeckShuffleChart :: IO ()
makeDeckShuffleChart =
  toFile def "data/WonkyComparator/chart_deck_shuffle_wonky.svg" $ do
    layout_title
      .= "Errors sorting a deck of cards with 10% failed comparisons"
    layout_title_style . font_size .= 15
    layout_all_font_styles . font_size .= 18
    layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
    plot (plotBars <$> bars titles (addIndexes (map snd values)))
