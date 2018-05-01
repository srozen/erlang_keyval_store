-module (client).
-export ([up/3, read/2, loop/2, gc/1, sleep/1, start/1, read_input/1]).

% [Manager] is the PID of the TransactionManager, this could be changed if we
% start using Sockets instead, for real Distributed System

% NOTE : Might be transformed as a process, for now it's just a module offering
% functions to talk with a TransactionManager

%%----------------------------------------------------------------------
%% Function: up/2
%% Purpose:  Ask TransactionManager to update a key/value pair in the Store
%% Args:     Manager pid
%%           Key
%%           Value
%% Returns:  Server response "ok"
%%----------------------------------------------------------------------

 up(Manager, Key, Value) ->
   Manager ! {self(), {up, Key, Value}},
   receive
     {Manager, Status} -> io:format(Status)
   end.

 %%----------------------------------------------------------------------
 %% Function: read/2
 %% Purpose:  Ask TransactionManager to read a series of keys to get Values
 %% Args:     Manager pid
 %%           Keys
 %% Returns:  List of Values corresponding to asked Keys, order preserved
 %%----------------------------------------------------------------------

read(Manager, Keys) ->
  Manager ! {self(), {read, Keys}},
  receive
    {Manager, Values} -> io:format(Values)
  end.

%%----------------------------------------------------------------------
%% Function: gc/1
%% Purpose:  Ask TransactionManager to launch a GrabageCollection
%% Args:     Manager pid
%% Returns:  Server response "ok"
%%----------------------------------------------------------------------

gc(Manager) ->
  Manager ! {self(), {gc}},
  receive
    {Manager, Status} -> io:format(Status)
  end.

%%----------------------------------------------------------------------
%% Function: sleep/1
%% Purpose:  Sleep a certain amount of time
%% Args:     Time of sleep desired
%% Returns:  Response "ok"
%%----------------------------------------------------------------------

sleep(Time) ->
  timer:sleep(Time),
  ok.


start(Manager) -> spawn(client, read_input, [Manager]).

read_input(Manager) ->
  %% read the input files to read and write to the manager
  {ok, Binary} = file:read_file("test_input"),
  Content = unicode:characters_to_list(Binary),
  Lines = string:tokens(Content, "\n"),
  loop(Manager, Lines).

loop(Manager, []) ->
  io:format("End~n");
loop(Manager, [H|T]) ->
  List = string:tokens(H," "),
  case List of
    [A|B] when A =:= "up" ->
      [X|Y] = B,
      up(Manager,X,Y);
    [A|B] when A =:= "read" ->
      read(Manager,B);
    [A|B] when A =:= "sleep" ->
      [X|_] = B,
      sleep(list_to_integer(X));
    [A] when A =:= "gc" -> gc(Manager);
    [] -> io:format("Other~n")
  end,
  %%Should we have a loop
  timer:sleep(500),
  loop(Manager, T).
