import NSSpeechRecognizer()
import Criterion.Main

main = defaultMain [
  bgroup "NSSpeechRecognizer"
    [ bench "1" $ nf   length [1..1000::Int]
    , bench "2" $ whnf length [1..1000::Int]
    ]
  ]

