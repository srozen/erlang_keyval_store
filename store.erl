-module (store).
-export ([start/0, loop/2]).

%%----------------------------------------------------------------------
%% Function: start/0
%% Purpose:  Spawns a Store process
%% Returns:  PID of the spawned Store
%%--------------
start() -> spawn(store, loop, [#{}, []]).

loop(Partition, Buffer) ->
  receive
    {Manager, {up, Key, Value}} ->
      NewPart = update_partition(Partition, Key, Value),
      Manager ! {self(), ok},
      loop(NewPart, Buffer)
  end.


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
