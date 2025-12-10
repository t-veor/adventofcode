Areas ← $Areas ≡(/×+1⌵/-) ⧅<2
Star₁ ← /↥♭ Areas

Trace ← (
  ⧈∘2 ˜⊂⊸⊢
  >0 °⊚ /◇⊂ ≡(□⍜⍉(≡\↥ ⬚¯1≡(⍜-(⇡+1) °⊟))⍆)
)

ValidNeighbors ← (
  ⊓(+A₂¤|△)
  ▽ ↧∩(/↧⍉) ⤚⊃(≥0|˜< ⊙¤)
)
FloodFillStep ← (
  °˜⊂
  ⊙(◡⊡
    ⨬(⟜⍜⊡⋅1
      ⊸ValidNeighbors
      ▽¬◡⊡ # perf: don't bother adding already filled neighbors
    )⋅[]
  )
  ⊂
)
FloodFill ← ◌ ⍢FloodFillStep(>0⧻) ¤

Pad       ← ⌝↘¯1_¯1 ⌝↘1_1
FillTrace ← ⍜Pad(↥ ¬⊸FloodFill 0_0)

FilledTrace ← FillTrace Trace

# Note: the *2 here is needed, it defeats various bugs relating to line
# segments being 1 away from each other.
Compress ← $CompressedCoords ×2 ⍜(°⊟⍉)∩(⨂⊸(▽⊸⧆₁⍆))

PrefixTrace ← $PrefixTrace \+≡\+ ⌝↘1_1 ¬FilledTrace

GetRect ← (
  ≡⍆ ⍉⊟
  ⍜⍉(+0_1)
  ≡(⊟ ⊓⊏⊏ ⊙˜⊙∘ ⊓°⊟°⊟) ⧅⋅⋅1 2 ⇡2 ¤
  ⊡
  ×1_¯1_¯1_1
  /+
)
ValidRect ← =0 GetRect

ValidRectMask ← (
  Compress
  ⊃($RectIdxs ⧅<2 ⇡⧻
  | ¤
  | ¤PrefixTrace
  )
  ≡(ValidRect °⊟ ⊏)
)

Star₂ ← (
  ▽ ⊃ValidRectMask Areas
  /↥
)
#
&fras ⍣(◇⊏1)"input.txt" &args
⊜[∩⋕ °$"_,_"] ⊸≠@\n

⟜Star₁
Star₂
