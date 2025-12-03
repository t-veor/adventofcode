RangeInclusive ← +⊃(⇡+1/-|⊢)

# ? Repeats MaxDigitsPerRepeat
RepTable ← /+ ⊞˜ⁿ ₑ₁₀ ⊓⇡⇡₁

# ? Repeats Nums
IsRepeat ← (
  ⊃(RepTable⟜(⌊÷) ⊙(+1⌊ °ₑ₁₀ /↥))⋅∘
  ×=0 ⊞⊃◿÷
  ⌊°ₑ₁₀
  /↥ = ⇡⊸(⊢△)
)

# ? Nums
IsAnyRepeat ← (
  ⊃(⇡₂ ⌊ °ₑ₁₀ /↥)¤
  ⬚0/↥ ≡IsRepeat
)

Star₁ ← /+ ≡(/+× ⊸(IsRepeat 2) RangeInclusive)
Star₂ ← /+ ≡(/+× ⊸IsAnyRepeat RangeInclusive)

&fras ⍣(◇⊏1|"input.txt") &args
⊜[∩⋕ °$"_-_"] ¬⊸∊",\n"
⟜Star₁
Star₂
