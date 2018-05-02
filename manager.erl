-module (manager).
-export ([start/1, loop/1]).

%%----------------------------------------------------------------------
%% Function: start/1
%% Purpose:  Spawns a Manager process
%% Args:     List of Store pid
%% Returns:  PID of the spawned Manager
%%--------------

start(Stores) -> spawn(manager, loop, [Stores]).

%%----------------------------------------------------------------------
%% Function: loop/1
%% Purpose:  Main loop of the process, pattern matching on received messages
%% Args:     Store pid
%% Returns:  N/A
%%--------------
loop(Stores) ->
  receive
    % Receive an UP request and transmit to Store
    {Client, {up, Key, Value}} ->
      Store = select_store(Stores, Key),
      Store ! {self(), {up, Key, Value}},
      receive
        {Store, Status} -> Client ! {self(), Status}
      end;
    % Receive a READ request with Keys and transmit to Store
    {Client, {read, Keys}} ->
      Values = process_reads(Stores, Keys),
      Client ! {self(), Values};
    % Receive a GC, do nothing for the moment
    {Client, {gc}} ->
      Client ! {self(), ok}
  end,
  loop(Stores).

%%----------------------------------------------------------------------
%% Function: process_reads/2
%% Purpose:  Return a list of Values, acquired from the Store(s)
%% Args:     Store pid
%%           List of Keys
%% Returns:  List of Values corresponding to asked Keys, order preserved
%%----------------------------------------------------------------------

process_reads(Stores, [Head|Tail]) ->
  Store = select_store(Stores, Head),
  Store ! {self(), {read, timestamp(), Head}},
  receive
    {Store, Val} -> [Val | process_reads(Stores, Tail)]
  end;

process_reads(_, []) -> [].

%%----------------------------------------------------------------------
%% Function: timestamp/0
%% Purpose:  Return system time as timestamp
%% Returns:  timestamp
%%----------------------------------------------------------------------
timestamp() ->
  os:timestamp().


select_store(Stores, Key) ->
  select_store(Stores, 1, length(Stores), Key, {0, -1}).

select_store(Stores, Elem, LastElem, Key, {Champion, Score}) ->
  case Elem > LastElem of
    true -> lists:nth(Champion, Stores);
    false ->
      NewScore = score(Elem, Key),
      case NewScore > Score of
        true -> select_store(Stores, Elem+1, LastElem, Key, {Elem, NewScore});
        false -> select_store(Stores, Elem+1, LastElem, Key, {Champion, Score})
      end
  end.

score(Num, Key) ->
  List = lists:concat([Num, Key]),
  Hash = binary_to_list(crypto:hash(sha, List)),
  Score = sum_list(Hash),
  1.0 / math:log(Score).

sum_list(List) ->
  sum_list(List, 0).

sum_list([H|T], Acc) ->
  sum_list(T, Acc+H);

sum_list([], Acc) ->
  Acc.
