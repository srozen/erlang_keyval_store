-module (store).
-export ([start/0, loop/2]).

%%----------------------------------------------------------------------
%% Function: start/0
%% Purpose:  Spawns a Store process
%% Returns:  PID of the spawned Store
%%--------------
start() -> spawn(store, loop, [#{}, []]).

%%----------------------------------------------------------------------
%% Function: loop/2
%% Purpose:  Main loop of the process, pattern matching on received messages
%% Args:     Partition, basically a Map of Maps,
%%           Buffer, a buffer of request that weren't processable
%% Returns:  N/A
%%--------------
loop(Partition, Buffer) ->
  receive
    % Receive and process an Up instruction from manager
    {Manager, {up, Key, Value}} ->
      NewPart = update_partition(Partition, Key, Value),
      Manager ! {self(), ok},
      loop(NewPart, Buffer);

    % Receive and process a Read instruction
    {Manager, {read, SnapshotTime, Key}} ->
      try
        #{Key := Submap} = Partition,
        case SnapshotTime =< timestamp() of
          true ->
            Timestamps = lists:reverse(maps:keys(Submap)),
            Val = read_most_recent_value(Submap, Timestamps, SnapshotTime),
            Manager ! {self(), Val},
            loop(Partition, Buffer);
          % We store request in buffer for further treatment
          false ->
            UpdatedBuffer = [{Manager, SnapshotTime, Key} | Buffer],
            loop(Partition, UpdatedBuffer)
        end
      catch
        error:_ ->
          Manager ! {self(), nil},
          loop(Partition, Buffer)
      end
  end.

%%----------------------------------------------------------------------
%% Function: read_most_recent_value/3
%% Purpose:  Updates the held partition with new Key/Value
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
%% Function: timestamp/0
%% Purpose:  Return system time as timestamp
%% Returns:  timestamp
%%----------------------------------------------------------------------
timestamp() ->
  os:timestamp().
