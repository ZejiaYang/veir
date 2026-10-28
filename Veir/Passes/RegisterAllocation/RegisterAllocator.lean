module

public import Veir.IR.Attribute
public import Veir.Passes.RegisterAllocation.RegisterStack
public import Veir.Rewriter.WfRewriter
public import Veir.GlobalOpInfo

namespace Veir


def allocateRegister (ctx : WfIRContext OpCode) (value : ValuePtr) (stack : RegStack) :
    Except String (WfIRContext OpCode × RegStack × RegisterType) := do
  let .registerType reg := (value.getType! ctx.raw).val
    | throw "expected a register-typed value"
  if reg.index.isSome then return (ctx, stack, reg)
  let (reg, s) := stack.pop
  let ctx := WfRewriter.setType! ctx value reg
  return (ctx, s, reg)

def allocateOps (ctx: WfIRContext OpCode) (op: OperationPtr) (stack: RegStack) :
    Except String (WfIRContext OpCode × RegStack) := do
    let mut ctx := ctx
    let mut stack := stack
    let mut resultRegs : Array RegisterType := #[]

    -- free allocated virtual registers in results
    for result in op.getResults! ctx.raw do
        let (newCtx, newStack, reg) ← allocateRegister ctx result stack
        ctx := newCtx
        stack := newStack
        resultRegs := resultRegs.push reg

    for reg in resultRegs do
        if reg.index.any (· < 0) then
            -- only push virtual registers to the reusable stack
            stack := stack.push reg

    -- allocate virtual registers to operands
    for operand in op.getOperands! ctx.raw do
        let (newCtx, newStack, _) ← allocateRegister ctx operand stack
        ctx := newCtx
        stack := newStack

    return (ctx, stack)

public section
def allocateBlock (ctx: WfIRContext OpCode) (block: BlockPtr):
    Except String (WfIRContext OpCode) := do

    let mut ctx := ctx
    let mut stack : RegStack := default
    let mut cur? := block.getLastOp! ctx.raw

    while let some cur := cur? do
        let (newCtx, newStack) ← allocateOps ctx cur stack
        ctx := newCtx
        stack := newStack
        cur? := cur.getPrevOp! ctx.raw

    return ctx
end
end Veir
