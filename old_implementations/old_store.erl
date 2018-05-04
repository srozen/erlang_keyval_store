-module (store).
-define(TIMEOUT, 1000).
-define(GARBAGE_DELAY,1).
-export ([start/0, loop/2, gc/3]).

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
    {Manager, {gc}} ->
      PrunedPart = gc(maps:keys(Partition), Partition, timestamp()),
      erlang:display(PrunedPart),
      loop(PrunedPart, Buffer);

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
            process_read(Submap, SnapshotTime, Manager),
            loop(Partition, Buffer);
          % We store request in buffer for further treatment
          false ->
            UpdatedBuffer = [{Manager, SnapshotTime, Key} | Buffer],
            loop(Partition, UpdatedBuffer)
        end
      catch
        error:_ ->
          Manager ! {self(), nil},
          loop(Partition, Buffer)
      end
    % If we have TIMEOUT without activity -> treat stalled requests
    after ?TIMEOUT ->
      case length(Buffer) > 0 of
        true ->
          UpdatedBuffer = process_bufferized_requests(Partition, Buffer),
          loop(Partition, UpdatedBuffer);
        false -> loop(Partition, Buffer)
      end
  end.

%%----------------------------------------------------------------------
%% Function: process_read/3
%% Purpose:  From a preselected Map of a certain Key, get the most recent
%%           value for the SnapshotTime and send to Manager
%% Args:     Map : A submap of key/val which are timestamps/vals
%%           SnapshotTime : The SnapshotTime from Manager
%%           Manager : ManagerPID
%% Returns:  N/A
%%--------------

process_read(Map, SnapshotTime, Manager) ->
  Timestamps = lists:reverse(maps:keys(Map)),
  Val = read_most_recent_value(Map, Timestamps, SnapshotTime),
  Manager ! {self(), Val}.

%%----------------------------------------------------------------------
%% Function: process_bufferized_requests/2
%% Purpose:  Fetch the BufferizedRequests and process the Reads that can
%%           be processed (timestamp comparison)
%% Args:     Partition : Collection of Data Stored
%%           Buffer : The Buffer of stalled request
%% Returns:  Updated Buffer without process
%%--------------

process_bufferized_requests(Partition, Buffer) ->
  process_bufferized_requests(Partition, lists:reverse(Buffer), []).

process_bufferized_requests(Partition, [Head | Tail], UpdatedBuffer) ->
  {Manager, SnapshotTime, Key} = Head,
  case SnapshotTime =< timestamp() of
    true ->
      #{Key := Submap} = Partition,
      process_read(Submap, SnapshotTime, Manager),
      process_bufferized_requests(Partition, Tail, UpdatedBuffer);
    false -> process_bufferized_requests(Partition, Tail, [Head | UpdatedBuffer])
  end;
process_bufferized_requests(_, [], UpdatedBuffer) -> lists:reverse(UpdatedBuffer).

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
%% Function: timestamp/0
%% Purpose:  Return system time as timestamp
%% Returns:  timestamp
%%----------------------------------------------------------------------
timestamp() ->
  os:system_time(seconds).

%%----------------------------------------------------------------------
%% Function: gc/3
%% Purpose:  Start a garbage collection on the data store excluding the most recent tuples
%% Args:     Keys of the data store
%%           Data store,
%%           Timestamp of the transaction
%% Returns:  Pruned partition
%%--------------
gc([], Partition, Now) -> Partition;
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
prune_data(Submap, [], Now) ->
  Submap;
prune_data(Submap, [Timestamp|T], Now) ->
  if
    Now -  Timestamp > ?GARBAGE_DELAY ->
      prune_data(maps:remove(Timestamp, Submap), T, Now);
    true ->
      prune_data(Submap, T, Now)
  end.
