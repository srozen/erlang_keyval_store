-module (store).
-behaviour (gen_server).
-define(SERVER, ?MODULE).
-define(GARBAGE_DELAY,1000).
-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start() ->
  {_, Pid} = gen_server:start_link(?MODULE, #{}, []),
  Pid.

init(Partition) -> {ok, Partition}.

% Callback Routines
handle_call({gc}, _From, Partition) ->
  PrunedPart = gc(maps:keys(Partition), Partition, timestamp()),
  {reply, ok, PrunedPart};

handle_call({up, Key, Value}, _From, Partition) ->
  NewPart = update_partition(Partition, Key, Value),
  {reply, ok, NewPart};

handle_call({read, SnapshotTime, Key}, Manager, Partition) ->
  try
    #{Key := Submap} = Partition,
    case SnapshotTime =< timestamp() of
      true ->
        Val = process_read(Submap, SnapshotTime),
        {reply, Val, Partition};
      % We store request in buffer for further treatment
      false ->
        %UpdatedBuffer = [{Manager, SnapshotTime, Key} | Buffer],
        Delay = SnapshotTime - timestamp(),
        erlang:send_after(Delay, self(), {retransmit, Manager, SnapshotTime, Key}),
        {noreply, Partition}
    end
  catch
    error:_ ->
      {reply, nil, Partition}
  end.


handle_cast(_Msg, State) -> {noreply, State}.
handle_info({retransmit, Manager, SnapshotTime, Key}, [Partition, Buffer]) ->
  #{Key := Submap} = Partition,
  Val = process_read(Submap, SnapshotTime),
  gen_server:reply(Manager, Val),
  {noreply, [Partition, Buffer]};

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%----------------------------------------------------------------------
%% Function: process_read/3
%% Purpose:  From a preselected Map of a certain Key, get the most recent
%%           value for the SnapshotTime and send to Manager
%% Args:     Map : A submap of key/val which are timestamps/vals
%%           SnapshotTime : The SnapshotTime from Manager
%%           Manager : ManagerPID
%% Returns:  N/A
%%--------------

process_read(Map, SnapshotTime) ->
  Timestamps = lists:reverse(maps:keys(Map)),
  read_most_recent_value(Map, Timestamps, SnapshotTime).

%%----------------------------------------------------------------------
%% Function: read_most_recent_value/3
%% Purpose:  Read the most recent version of an item in the data store
%% Args:     Map, a map of Timestamp/Value pairs
%%           List, a list of Timestamp, from the highest to lowest
%%           SnapshotTime, the ManagerTimeStamp
%% Returns:  Value from the most recent TimeStamp
%%--------------

read_most_recent_value(Map, [Head | Tail], SnapshotTime) ->
  case Head =< SnapshotTime of
    true -> maps:get(Head, Map);
    false -> read_most_recent_value(Map, Tail, SnapshotTime)
  end;

read_most_recent_value(Map, Elem, _) -> maps:get(Elem, Map).


%%----------------------------------------------------------------------
%% Function: update_partition/3
%% Purpose:  Updates the held partition with new Key/Value
%% Args:     Current partitions
%%           Key,
%%           Value
%% Returns:  Updated Partition
%%--------------
update_partition(Partition, Key, Value) ->
  try
      #{Key := Submap} = Partition,
      UpdatedSubmap = Submap#{timestamp() => Value},
      Partition#{Key => UpdatedSubmap}
  catch
    error:_ ->
      Partition#{Key => #{timestamp() => Value}}
  end.

%%----------------------------------------------------------------------
%% Function: gc/3
%% Purpose:  Start a garbage collection on the data store excluding the most recent tuples
%% Args:     Keys of the data store
%%           Data store,
%%           Timestamp of the transaction
%% Returns:  Pruned partition
%%--------------

gc([], Partition, _) -> Partition;
gc([Key|T], Partition, Now) ->
  #{Key := Submap} = Partition,
  [_|Timestamps] = lists:reverse(maps:keys(Submap)),
  PrunedSubmap = prune_data(Submap, Timestamps, Now),
  gc(T, Partition#{Key => PrunedSubmap}, Now).

%%----------------------------------------------------------------------
%% Function: prune_data/3
%% Purpose:  Remove previous version of an item if they are older than X seconds
%% Args:     Map with timestamps as key and a string value
%%           Keys of the map,
%%           Timestamp of the transaction
%% Returns:  Pruned submap

prune_data(Submap, [], _) -> Submap;
prune_data(Submap, [Timestamp|T], Now) ->
  if
    Now -  Timestamp > ?GARBAGE_DELAY ->
      prune_data(maps:remove(Timestamp, Submap), T, Now);
    true ->
      prune_data(Submap, T, Now)
  end.
%%----------------------------------------------------------------------
%% Function: timestamp/0
%% Purpose:  Return system time as timestamp
%% Returns:  timestamp
%%----------------------------------------------------------------------
timestamp() ->
  os:system_time(millisecond).
