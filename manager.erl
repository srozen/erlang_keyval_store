-module (manager).
-export ([start/1, loop/1]).

start(Store) -> spawn(manager, loop, [Store]).

loop(Store) ->
  receive
    % Receive an UP request and transmit to Store
    {Client, {up, Key, Value}} ->
      Status = Store ! {self(), {up, Key, Value}},
      Client ! {self(), Status};
    % Receive a READ request with Keys and transmit to Store
    {Client, {read, Keys}} ->
      Values = process_reads(Store, Keys),
      Client ! {self(), Values};
    % Receive a GC, do nothing for the moment
    {Client, {gc}} ->
      Client ! {self(), ok}
  end.

process_reads(Store, [Head|Tail]) ->
  Response = Store ! {self(), {read, timestamp(), Head}},
  [Response, process_reads(Store, Tail)];

process_reads(_, []) -> [].

%%----------------------------------------------------------------------
%% Function: timestamp/0
%% Purpose:  Return system time as timestamp
%% Returns:  timestamp
%%----------------------------------------------------------------------
timestamp() ->
  os:timestamp().
