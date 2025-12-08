┌─╴DisjointSet
  ~ {Parent Rank Sets}
  FromCount ← New ⊃(⇡|×0 ⇡|∘)

  # xParent DSNew ? x DS
  FindCompressStep ↚ (
    ⊃(∘|⊏|⊏ ⊸⊏) ⊙⊸Parent # x xParent xParentParent DS ?
    ⊃(⋅∘
    | ⊃⋅⋅⋅∘⊙⋅∘    # DS x xParentParent ?
      ⍜Parent⍜˜⊡◌ # DSNew ?
    )
  )

  # y DSNew ? x DS
  Find ← ⍥FindCompressStep∞

  # DSNew ? x y DS
  Union ← (
    Find ⊃⊙⋅∘⋅∘      # xRoot DS1 y ?
    Find ⊃(⋅⋅∘|⋅∘|∘) # yRoot DS2 xRoot ?
    ⊃⋅⋅∘⊙∘           # xRoot yRoot DS2

    ¬◡=
    ⨬(⋅⋅∘
    | # xRoot != yRoot
      # xRoot yRoot DS ?

      # Swap xRoot and yRoot if xRootRank > yRootRank
      ⍥⤙⊙◌ ◡(> ∩⌟⊏ ⊙⊙Rank)

      # set yRootParent to xRoot
      ⊃(⊙∘
      | ⊃(⋅⋅∘|⋅∘|∘) # DS yRoot xRoot ?
        ⍜Parent⍜˜⊡◌
      ) # xRoot yRoot DS ?

      # Increment xRootRank if xRootRank = yRootRank
      ◡(= ∩⌟⊏ ⊙⊙Rank)
      ⨬(⋅⋅∘
      | ⊃⋅⋅∘∘        # DS xRoot ? xRoot yRoot DS
        ⍜Rank⍜˜⊡(+1) # DS ?
      )              # DS ?

      ⍜Sets(-1)
    ) # DS ?
  )

  # ? DS
  SetSizes ← (
    ∧(⤙⊙◌ Find) ⇡⧻⊸Parent
    ⊜[⊃⊢⧻] +1⊸⊛ ⍆
  )
└─╴

MagSq  ← ≡₁/+˙×
DistSq ← MagSq -

&fras ⍣(◇⊏1|"input.txt") &args
⊜[∩₃⋕ °$"_,_,_"] ⊸≠@\n

⊸˙⊞DistSq

⧅< 2 ⇡⊢⊸△
$SortedEdges ⊏⍏ ⤚⊡

Star₁ ← (
  ⊙(DisjointSet~FromCount⧻)
  ⍣(↙1000)∘
  ∧(DisjointSet~Union°⊟)

  DisjointSet~SetSizes
  ⇌⍆ ⊡1 ⍉
  /× ⍣(↙3)∘
)

Star₂ ← (
  ⊙⊸(DisjointSet~FromCount⧻)
  ⍢(⊃(+1|⋅∘|DisjointSet~Union°⊟ ⊏)
  | >1 DisjointSet~Sets⋅◌
  )0

  /× ⊏0 ⍉ ⊏ ⊙◌ ⊏ -1
)

⊃Star₂ Star₁
