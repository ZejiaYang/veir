module

public import Veir.Pass
public import Veir.Passes.RegisterAllocation.RegisterFile
public import Veir.Passes.RegisterAllocation.RegisterStack

namespace Veir

public section

/-- Allocate one block, walking its operations in reverse. -/
def allocateBlock (ctx : WfIRContext OpCode) (block : BlockPtr) : WfIRContext OpCode := Id.run do
  let mut ctx := ctx
  let mut stack : RegStack := default
  let mut cur? := block.getLastOp! ctx.raw
  while let some cur := cur? do
    dbg_trace "visiting {String.fromUTF8! (cur.getOpType! ctx.raw).name}"

    -- (1) Free the results.
    for v in cur.getResults! ctx.raw do
      let some reg := (v.getType! ctx.raw).cast? RegisterType | continue
      if RegisterFile.isAllocated reg then
        stack := stack.push reg
        dbg_trace "Freed {reg.index}"
      else
        -- Dead result: never used, but the op still writes a register —
        -- assign one and release it immediately.
        dbg_trace "Dead operand use and release {reg.index}"
        let (r, stack') := stack.pop
        ctx := WfRewriter.setType! ctx v (TypeAttr.of RegisterType r)
        stack := stack'.push r
    -- (2) Allocate the operands.
    for v in cur.getOperands! ctx.raw do
      let some reg := (v.getType! ctx.raw).cast? RegisterType | continue
      if !RegisterFile.isAllocated reg then
        let (r, stack') := stack.pop
        stack := stack'
        ctx := WfRewriter.setType! ctx v (TypeAttr.of RegisterType r)
        dbg_trace "Allocated {r.index}"
    cur? := cur.getPrevOp! ctx.raw

  return ctx

def RegisterAllocationPass.impl (ctx : WfIRContext OpCode) (op : OperationPtr)
    (_ : op.InBounds ctx.raw) : ExceptT String IO (WfIRContext OpCode) := do
    let mut ctx := ctx
  -- `op` is the module: one region, one block, whose ops are the functions.
    let region := (op.getRegions! ctx.raw)[0]!
    IO.eprintln "hllo"
    let some body := region.getFirstBlock! ctx.raw | return ctx
    let some f := body.getFirstOp! ctx.raw | return ctx
    dbg_trace "allocating block {f.id}"
    let some funcOp := FunctionOp.of? f ctx.raw | return ctx
    let some entry := funcOp.getEntryBlock? | return ctx
      ctx := allocateBlock ctx entry


    return ctx

public def RegisterAllocationPass : Pass OpCode :=
  { name := "register-allocation-riscv64"
    description := "Assign RV64 registers to the values of single-block riscv functions."
    run := fun _ => RegisterAllocationPass.impl }

end

end Veir
