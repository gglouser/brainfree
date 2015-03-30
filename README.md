brainfree - brainfuck for free
===============================

[Brainfuck](http://en.wikipedia.org/wiki/Brainfuck) is an esoteric programming
language well-known for its simplicity. That simplicity makes it difficult to
write programs in brainfuck, but also makes it easy to write interpreters and
compilers for the language.

BrainFree is a brainfuck interpreter in Haskell that uses a free monad as an
intermediate form to represent the brainfuck program. In a sense, it's not all that
different from using an algebraic data type to represent the program instructions,
but here, the free monad acts as our AST. Once the program is in free monad
form, we can evaluate it in different ways corresponding to different concrete
implementations of the abstract brainfuck machine. Three "back ends" are provided,
selectable with command line option:

- `-v` -- a mutable unboxed Vector (default)
- `-f` -- a Foreign pointer into an array
- `-t` -- an infinite stream-based tape

We can also perform some limited optimizations on the program in free monad form
(enabled with `-o`).

### Usage summary ###

    usage: brainfree [FLAGS] SOURCEFILE
    
    Flags:
    -o      enable optimizations
    -v      use Vector data store (default)
    -f      use Foreign array data store
    -t      use infinite tape data store

### Performance ###

Performance is not expected to be comparable to a highly optimizing interpreter.
The sample runtimes below are just to give a sense of the relative performance
of the different runtime options.

Sample runtimes (completely unscientific) of long.bf:

| Data store   | No optimization | With optimization |
|--------------|-----------------|-------------------|
| Vector (-v)  |         103.65s |            29.36s |
| Foreign (-f) |          89.05s |            23.60s |
| tape (-t)    |         290.47s |           149.40s |


The BF Monad
-------------

Let's interpret brainfuck using a free monad!

Here is a (simplified) functor that represents the things the abstract
brainfuck machine can do (where Cell is the type stored in the data array,
classically one byte):

    data BFF k = MovePtr Int k          -- move the data pointer
               | ReadPtr (Cell -> k)    -- read from the data pointer
               | WritePtr Cell k        -- write to the data pointer
               | GetChar (Char -> k)    -- accept input
               | PutChar Char k         -- send output

This is not the only possible functor that can represent brainfuck programs,
and indeed, if you look at the actual BrainFree implementation, you will
see some differences to support some optimizations and handling end-of-file.
It has the nice property that data pointer operations and I/O are decoupled.

Now we can translate brainfuck programs into the free monad on BFF,
specifically `Free BFF ()` because they do not produce any value. Here is
how each basic command can be implemented:

| Command | Translation                          |
|:-------:|--------------------------------------|
| `>`     | `movePtr   1`                        |
| `<`     | `movePtr (-1)`                       |
| `+`     | `readPtr >>= \n -> writePtr (n + 1)` |
| '-'     | `readPtr >>= \n -> writePtr (n - 1)` |
| '.'     | `readPtr >>= putChar . cellToChar`   |
| ','     | `getChar >>= writePtr . charToCell`  |

Loops can be implemented using the full power of recursion and monads.
Assume that a loop body (everything between a balanced pair of `[...]`)
has already been translated into our monad `Free BFF ()`. Then the loop is:

    loop body = do x <- readPtr
                   when (x /= 0) $ body >> loop body

This works, but it turns out to be slower than we would like. I believe this 
is because we have to re-interpret the free monad actions inside the loop at 
every iteration. If we add another action to our functor to represent loops, 
`Loop (Free BFF ()) k`, then it is possible to interpret the loop body 
just once. The downside is that every interpreter of Free BFF is now required 
to implement the correct loop behavior. The upside is that programs run
much faster.

### End-of-file ###

In order to handle end-of-file conditions, the actual form of GetChar is
`GetChar (Maybe Char -> k)`. The default behavior if `Nothing` is returned
is to leave the current cell's value unchanged. A back end implementation
could support one of the other "standard" end-of-file behaviors by
returning `Just (coerce (0::Cell))` or `Just (coerce (-1::Cell))`, or
equivalent, instead of Nothing.


Credit
-------

I took inspiration from
[SamB's BF interpreter](http://www.haskell.org/haskellwiki/Short_examples/BF_interpreter)
which parses a brainfuck program directly into IO monad actions, and shows how
to use a Foreign pointer into an array as a data store.
