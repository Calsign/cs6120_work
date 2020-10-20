
CS 6120 Assignment #6.

My LLVM pass adds instructions around every function call to print
messages for entering and leaving. The idea is that these messages can
be used to construct the stack trace at any given point in execution.

I couldn't figure out how to invoke an external C library function
(printf), so instead I did a hacky thing where the first function in
the code I am compiling is a proxy for printf so that the LLVM pass
can grab that function.

I haven't tested it on a larger C program yet.
