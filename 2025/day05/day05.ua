# IntervalSet ? NewInteral IntervalSet
MergeStep ← (
  # Check NewInterval.start <= last(IntervalSet).end
  ≥◡⊓⊢⍣(⊣⊣)¯∞
  ⨬( # NewInterval.start > last(IntervalSet).end
    # Append a new interval to IntervalSet
    ˜⊂¤
  | # NewInterval.start <= last(IntervalSet).end
    # Merge the new interval with the last interval set
    ˜⍜(⊣⊣)(↥ ⊙⊣)
  )
)

MergeIntervals ← ∧MergeStep⊙[] ⊏⍏⊸≡⊢

Star₁ ← ⧻⊚ /↥ ×⊓⌟⊞≥⊞≤ °⊟ ⍉
Star₂ ← /+ +1≡/- MergeIntervals

&fras ⍣(◇⊏1|"input.txt") &args
°$"_\n\n_"
∩(⊸≠@\n)
⊓⊜[∩⋕ °$"_-_"]⊜⋕

⟜Star₁
Star₂
