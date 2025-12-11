# ? From To AdjacencyMap
CountPaths ← |3 memo(
  ◡≍
  ⨬(⍣(⊃(get⊙◌)⋅⊙∘
      /+ ≡⌟₂CountPaths°□
    )0
  )1
)

# ? Route AdjacencyMap
CountRoutePaths ← (
  ⧈∘ 2
  /× ≡⌟(CountPaths°⊟)
)

Star₁ ← CountPaths □"you" □"out"
Star₂ ← +∩⌞CountRoutePaths {"svr" "fft" "dac" "out"} {"svr" "dac" "fft" "out"}

&fras ⍣◇⊏₁"input.txt" &args
$AdjacencyMap map ⊜(∩□ ⊙(⊜□ ⊸≠@ ) °$"_: _") ⊸≠@\n
⟜Star₁
Star₂
