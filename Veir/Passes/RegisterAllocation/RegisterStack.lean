module

public import Veir.IR.Attribute

meta import Veir.IR.Attribute

namespace Veir

public section

structure RegStack where
  /-- Registers released during allocation. -/
  free : List RegisterType
  /-- The next virtual register index, used when `free` is empty. -/
  next : Nat
deriving Inhabited

namespace RegStack

def pop (s: RegStack) : RegisterType × RegStack :=
  match s.free with
  -- reuse registers in free stack
  | r :: rest => (r, {s with free := rest})
  -- if free stack is empty, get next virtual register index
  | [] => (RegisterType.mk (some (-((s.next : Int) + 1))), {s with next := s.next + 1})

def push (s: RegStack) (r: RegisterType) : RegStack :=
  {s with free := r :: s.free}

end RegStack

end

end Veir
