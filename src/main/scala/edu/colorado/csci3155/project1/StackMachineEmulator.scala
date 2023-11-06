package edu.colorado.csci3155.project1


sealed trait StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case class PushI(f: Double) extends StackMachineInstruction
case object PopI extends StackMachineInstruction


object StackMachineEmulator {

    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack and a single instruction of type StackMachineInstruction
        Return a stack that results when the instruction is executed from the stack.
        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.

     */
    def emulateSingleInstruction(stack: List[Double], ins: StackMachineInstruction): List[Double] = {
        ins match {
            case AddI => 
                if (stack.length < 2) throw new IllegalArgumentException("Insufficient size of stack.")
                val (x :: y :: rem) = stack 
                (x + y) :: rem

            case SubI =>
                if (stack.length < 2) throw new IllegalArgumentException("Insufficient size of stack.")
                val (x :: y :: rem) = stack
                (y - x) :: rem

            case MultI => 
                if (stack.length < 2) throw new IllegalArgumentException("Insufficient size of stack.")
                val (x :: y :: rem) = stack
                (x * y) :: rem
            
            case DivI => 
                if (stack.length < 2) throw new IllegalArgumentException("Insufficient size of stack.")
                val (x :: y :: rem) = stack
                if (x == 0) throw new ArithmeticException("Division by zero not allowed.")
                (y / x) :: rem

            case ExpI => 
                if (stack.isEmpty) throw new IllegalArgumentException("Inffucient size of stack.")
                val (x :: rem) = stack
                math.pow(math.E, x) :: rem
            
            case LogI => 
                if (stack.isEmpty) throw new IllegalArgumentException("Insufficient size of stack.")
                val (x :: rem) = stack
                if (x <= 0) throw new ArithmeticException("Log of a non-positive number not allowed.")
                math.log(x) :: rem

            case SinI => 
                if (stack.isEmpty) throw new IllegalArgumentException("Inffucient size of stack.")
                val (x :: rem) = stack
                math.sin(x) :: rem

            case CosI => 
                if (stack.isEmpty) throw new IllegalArgumentException("Inffucient size of stack.")
                val (x :: rem) = stack
                math.cos(x) :: rem
            
            case PushI(f) => f :: stack
            case PopI => 
                if (stack.isEmpty) throw new IllegalArgumentException("Cannot pop from empty stack.")
                stack.tail
        }
    }
    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be a double that is the top of the stack after all instructions
       are executed.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Double = {
        val startStack: List[Double] = List()
        val finalStack = instructionList.foldLeft(startStack) { (stack, ins) =>
            emulateSingleInstruction(stack, ins)
        }
        if (finalStack.isEmpty) { throw new IllegalArgumentException("Stack is empty.") }
        else if (finalStack.length > 1) { throw new IllegalArgumentException("Invalid final stack.") }
        else { finalStack.head }
    }
}