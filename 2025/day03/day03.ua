# Idx Max ? Arr
ArgMax ← ⊸⨂ ⟜/↥

# Batteries' Bank' Acc ? Batteries Bank
JoltStep ← (
  -1
  ⊃⊙∘(ArgMax ↘¯)
  ⊙˜(↘+1)
)

# ? Batteries Bank
Joltage ← /(+×10) ⍢JoltStep(>0)

Star₁ ← /+ ≡Joltage 2
Star₂ ← /+ ≡Joltage 12

&fras ⍣(◇⊏1|"input.txt") &args
⊜≡⋕ ⊸≠@\n
⟜Star₁
Star₂
