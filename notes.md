# Notes

Just a collection of ideas for potential future features to add or ways to improve the interpreter.

# Features

## Garbage Collection
Since we're using Zig, we have to manage manually manage memory allocations. Right now we are using an arena allocator and simply deallocating the entire chunk of memory upon exit of the program. My intuition is that this is problematic as we could run out of memory in the arena for allocations depending on the program (e.g. recursive function calls could result in out of memory errors).

Thorsten's book doesn't cover the details of implementing a garbage collector, but he does provide a general overview of the core functionality the gc needs to implement (i.e. keep track of  allocattions and references to objects, make enough memory available for future object allocations ands give memory back when it's no longer needed). The simplest way to go about this would probably be a simple ref-counting gc. 

+ _Crafting Interpreters_ might cover some things about implementing a gc?
+ TJ's boot.dev course goes over implementing a ref counting gc in C?
