-module (manager).
-export ([start/1, loop/1]).

%%----------------------------------------------------------------------
%% Function: start/1
%% Purpose:  Spawns a Manager process
%% Args:     Store pid
%% Returns:  PID of the spawned Manager
%%--------------

start(Store) -> spawn(manager, loop, [Store]).

%%----------------------------------------------------------------------
%% Function: loop/1
%% Purpose:  Main loop of the process, pattern matching on received messages
%% Args:     Store pid
%% Returns:  N/A
%%--------------
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

%%----------------------------------------------------------------------
%% Function: process_reads/2
%% Purpose:  Return a list of Values, acquired from the Store(s)
%% Args:     Store pid
%%           List of Keys
%% Returns:  List of Values corresponding to asked Keys, order preserved
%%----------------------------------------------------------------------

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
