module

public import Veir.IR.Attribute
public import Veir.Passes.RegisterAllocation.RegisterFile

meta import Veir.IR.Attribute

namespace Veir

public section

structure RegStack where
  /-- Registers released during allocation -/
  free : List RegisterType
  /-- Next infinite register id, used when `free` is empty -/
  next : Nat
deriving Inhabited

namespace RegStack

def pop (s: RegStack) : RegisterType × RegStack :=
  match s.free with
  | r :: rest => (r, {s with free := rest})
  | [] => (RegisterFile.virtual RegisterType s.next, {s with next := s.next + 1})

def push (s: RegStack) (r: RegisterType) : RegStack :=
  {s with free := r :: s.free}

end RegStack

end

end Veir
