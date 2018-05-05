# Erlang Distributed Key-Value Store

## Launch the program and specify the input file

Here is a small example of the program with 3 Stores, 2 Managers
and one client reading an input file.

Client launch the input file reading by calling :read_input, then
specifies the chosen Manager and the file path as a string.

```
f().
c(store).
c(manager).
c(client).

Store1 = store:start().
Store2 = store:start().
Store3 = store:start().
Manager1 = manager:start([Store1, Store2, Store3]).
Manager2 = manager:start([Store1, Store2, Store3]).
client:read_input(Manager1, "test_input").
```

## Example Scripts

The folder **test_scripts** contains some series of Erlang commands
you can copy/paste in your ERL Shell to test the program.
