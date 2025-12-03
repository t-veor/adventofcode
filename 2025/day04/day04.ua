Neighbors  ← /+ ⬚0↻ ⊂A₂C₂
Accessible ← × <4 ⊸Neighbors

Star₁ ← ⧻⊚ Accessible
Star₂ ← ⧻⊚ - ⊸⍥(×¬⊸Accessible)∞

&fras ⍣(◇⊏1|"input.txt") &args
⊜(=@@) ⊸≠@\n
⟜Star₁
Star₂
