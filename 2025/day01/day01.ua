ParseRot ← ⍣(
  ¯⋕ °$"L_"
| ⋕ °$"R_"
)

Star₁ ← /+ =0 ◿100 +50 \+

RangeTo ← +⊃⊢(+ >0 ⟜⇡ /-)
Star₂ ← (
  \+ ⊂50
  ⧈(/+ =0 ◿100 RangeTo) 2
  /+
)

&fras ⍣(◇⊏1|"input.txt") &args
⊜ParseRot ⊸≠@\n
⟜Star₁
Star₂
