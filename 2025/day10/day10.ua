ParseMachine ← (
  ⊜□ +1 ↥⊃\×⍜⇌\× ⊸≠@ 
  ⍜°□₃⊓(
    =@# °$"[_]"
  | ⬚0⊜(°⊚ ⊜⋕ ⊸≠@, °$"(_)") ⊸≠@ 
  | ⊜⋕ ⊸≠@, °$"{_}"
  )
)

ConfigurePresses ← (
  ⊃(×0|⋅∘|∘|⋅⋅◌) °□₃

  -1 ⧻⊢ path(◿2+¤|≍⊙◌)
)

# This is too slow on the real input.
# Looks like you _have_ to resort to a ILP solver...
JoltPresses ← (
  ⊃(×0|⋅∘|⋅⋅∘) °□₃
  -1 ⧻⊢ path(
    +¤
  | ≍⊙◌
  | <0 ⊃/↧/↥ - ⊙◌
    ⨬∘∞
  )
  ⊸&p
)

Star₁ ← /+ ≡ConfigurePresses
Star₂ ← /+ ≡JoltPresses

&fras ⍣(◇⊏1)"input.txt" &args
⊜ParseMachine ⊸≠@\n

Star₁
