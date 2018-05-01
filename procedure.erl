% Copy Paste this in your ERL Machine to test

f().
c(store).
c(manager).
c(old_client).

Store = store:start().
Manager = manager:start(Store).
old_client:up(Manager, foo, 3).
old_client:read(Manager, [foo]).
