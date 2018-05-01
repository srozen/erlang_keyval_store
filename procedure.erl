% Copy Paste this in your ERL Machine to test

c(store).
c(manager).
c(client).

Store = store:start().
Manager = manager:start(Store).
client:up(Manager, foo, 3).
client:read(Manager, [foo]).
