# Supported
Most opcodes, branching, comment lines  

Can create methods that do or don't return a result. Can create methods that take arguments, but not vararg methods.

Each line may begin with whitespace.

If the first non-whitespace character is a semicolon, the line is considered a comment and ignored.

Temporarily, until directives are implement: if the first character is a period, the line is also ignored.

Each line may contain a label and an instruction (with its arguments), only a label or only an instruction (with its arguments).

A label is any string of nonwhitespace characters ending with a colon

The opcodes are named as in the System.Reflection.Emit.Opcodes class fields. That is, instead of 'Tail.' we have 'TailCall'. Case insensitive. Underscores are replaced with periods. Instead of ld_i4_1 you should write ld.i4.1

The assembler checks for some errors, invalid opcodes, invalid arguments, generally when it can not continue or can not complete the process. Warns if a ret is missing or a label is marked but unused. Fails if a label is used but unmarked.

The TILAsm Class does the assembly upon instantiation.

First argument, mandatory, is the source code, either as a string or a string array.

Second argument, optional, is the return type.

Third argument, optional, is the parameter type array.

After assembly, the messages field (a List of SMessage) contains messages about the assembly and the method field contains the resulting method. It's up to you to cast it to whatever you've asked to make.

# Unsupported
Local variables, calli opcode

# Possible issues

Haven't tried it from inside a class, it's likely it will complain about the missing first object argument. We'll see.
