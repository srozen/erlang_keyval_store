% Copy Paste this in your ERL Machine to test

f().
c(store).
c(manager).
c(old_client).

Store1 = store:start().
Store2 = store:start().
Store3 = store:start().

Manager1 = manager:start([Store1, Store2, Store3]).
Manager2 = manager:start([Store1, Store2, Store3]).

old_client:up(Manager1, foo, 3).
old_client:read(Manager2, [foo]).


old_client:up(Manager1, bar, 68).
old_client:up(Manager1, baz, 86).
old_client:read(Manager2, [foo, bar, baz]).


% Delayed read with too high timestamp
Store1 ! {self(), {up, foo, 7}}.
receive X -> X end.
{Hours, Seconds, Milli} = os:timestamp().
TimeStamp = {Hours, Seconds + 10, Milli}.
Store1 ! {self(), {read, TimeStamp, foo}}.
receive {Store1, Values} -> Values end.
