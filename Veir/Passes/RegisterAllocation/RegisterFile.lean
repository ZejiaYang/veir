module

public import Veir.IR.Attribute
meta import Veir.IR.Attribute

namespace Veir

public section

/-!
  Base class to specify register files

-/
class RegisterFile (α : Type) where
  /-- type name, e.g. 'riscv.reg' -/
  typeName : String
  /-- prefix used to mark infinite registers -/
  infinitePrefix : String
  /-- names of physical registers according to ABI, e.g., abiNames[0] = "zero" -/
  abiNames : Array String
  /-- alternative names for registers, e.g., x8 on RISC-V might be s0 or fp -/
  abiAliases : Array (String × Nat) := #[]
  /-- get index of a register, None if unallocated -/
  index : α → Option Int
  /-- construct a register with given index, if index is None, unallocated register is returned -/
  withIndex : Option Int → α

  toTypeAttr : α → TypeAttr
  ofTypeAttr? : TypeAttr → Option α

namespace RegisterFile

variable (α : Type) [RegisterFile α]

/-- construct unallocated register -/
def unallocated : α := withIndex none

/-- number of physical registers in the file -/
def numRegisters : Nat := (abiNames (α := α)).size

/-- construct physical register with index `i` -/
def physical (i : Nat) : α := withIndex (some i)

/--
  construct the `k`-th virtual register (0-based), stored as index `-(k + 1)`
  and printed `j<k>`. The offset keeps index `0` free for the physical
  register 0, so every `Nat` is a valid virtual register.
-/
def virtual (k : Nat) : α := withIndex (some (-(k + 1 : Int)))

/--
  Construct a register by spelling: a canonical ABI name, an alias, or a
  virtual register `<infinitePrefix><k>`.
-/
def fromName? (s : String) : Option α :=
  match (abiNames (α := α)).findIdx? (· == s) with
  | some i => some (physical α i)
  | none =>
    match (abiAliases (α := α)).find? (·.1 == s) with
    | some (_, i) => some (physical α i)
    | none =>
      let p := infinitePrefix (α := α)
      if s.startsWith p then
        (s.drop p.length).toNat?.map (virtual α)
      else none

variable {α}

/-- Whether the register is allocated (physical or virtual) -/
def isAllocated (r : α) : Bool := (index r).isSome

/-- The physical register index, if the register is physical. -/
def physIndex? (r : α) : Option Nat :=
  (index r).bind fun i => if 0 ≤ i then some i.toNat else none

/-- Whether the register is allocated to a physical register. -/
def isPhysical (r : α) : Bool := (physIndex? r).isSome

/-- The 0-based virtual register id, if the register is virtual: index `-(k + 1)` is `j<k>`. -/
def virtualIndex? (r : α) : Option Nat :=
  (index r).bind fun i => if i < 0 then some (-i - 1).toNat else none


/--
  The canonical spelling of an allocated register
-/
def registerName (r : α) : Option String :=
  (index r).bind fun i =>
    if i >= 0 then (abiNames (α := α))[i.toNat]?
    else some (infinitePrefix (α := α) ++ toString (-i - 1))


end RegisterFile

/-- The RV64 integer register file. -/
instance : RegisterFile RegisterType where
  typeName := "riscv.reg"
  infinitePrefix := "j"
  abiNames := #["zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2",
                "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5",
                "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7",
                "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"]
  abiAliases := #[("fp", 8)] ++ (Array.range 32).map (fun i => (s!"x{i}", i))
  index := RegisterType.index
  withIndex := RegisterType.mk
  toTypeAttr := TypeAttr.of RegisterType
  ofTypeAttr? := (TypeAttr.cast? · RegisterType)

namespace RegisterFile

#guard virtual RegisterType 0 = RegisterType.mk (some (-1))
#guard registerName (virtual RegisterType 4) = some "j4"
#guard fromName? RegisterType "j4" = some (virtual RegisterType 4)
#guard registerName (physical RegisterType 10) = some "a0"
#guard fromName? RegisterType "x0" = some (physical RegisterType 0)

end RegisterFile

end

end Veir
