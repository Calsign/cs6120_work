
The code for dominators, dominator trees, and dominator frontiers all
works well. I tested this with a combination of a test suite (in
`src/test.ml`) and a set of turnt tests in the various `test-`
folders. I believe that these tests are comprehensive and thus I am
confident in this functionality.

The SSA transformation is not quite working. For some simple programs
it produces runnable code with phi nodes, but there is at least one
problem with the selection of the variable names in the phi nodes that
makes the transformation broken for any non-trivial program.
Unfortunately I don't know what the problem is, and I currenty think
that this is due to a lack of understanding about how the SSA
transformation is supposed to work. This would not be an issue as I
would simply figure it out, but I am out of time so I am submitting
what I have. I did start this assignment considerably earlier than the
previous assignment, and I have spent 15-20 hours on it so far.
