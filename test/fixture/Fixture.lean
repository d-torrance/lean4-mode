-- Fixture for lean4-mode's end-to-end tests.
-- Line numbers are asserted in test/lean4-e2e-test.el; keep them stable.

theorem proved : 1 + 1 = 2 := by
  rfl

theorem admitted : 2 + 2 = 4 := by
  sorry

def bad : Nat := "not a number"
