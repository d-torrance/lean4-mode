-- Fixture for lean4-mode's end-to-end tests.
-- Line numbers are asserted in test/lean4-e2e-test.el; keep them stable.

theorem proved : 1 + 1 = 2 := by
  rfl

theorem admitted : 2 + 2 = 4 := by
  sorry

def bad : Nat := "not a number"

-- Produces a nested, collapsible trace.  Appended at the end so the line
-- numbers asserted above stay valid.
set_option trace.Meta.synthInstance true in
example : Inhabited (Nat × Nat) := inferInstance

-- Hypotheses of every kind the goal display's filters tell apart: a type,
-- a typeclass instance, and an ordinary one.  Appended at the end so the
-- line numbers asserted above stay valid.
example {α : Type} [Inhabited α] (h : α) : True := by
  sorry

-- An auto-bound implicit: `α` is never declared, so Lean binds it and offers
-- an inlay hint saying so.  Appended at the end so the line numbers asserted
-- above stay valid.
def autoBound (a : α) : α := a

-- `simp?` reports what it used as a "Try this" suggestion, which the server
-- offers as a code action.  Appended at the end so the line numbers asserted
-- above stay valid.
example : 1 = 1 := by simp?

-- A command with a `set_option ... in' prefix, which Lean folds as one and
-- which therefore begins where the option does.  Appended at the end so the
-- line numbers asserted above stay valid.
set_option maxHeartbeats 400000 in
theorem prefixed : True := by
  trivial
