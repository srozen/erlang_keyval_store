-module (manager).
-behaviour (gen_server).
-define(SERVER, ?MODULE).
-export([start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start(Stores) ->
  {_, Pid} = gen_server:start_link(?MODULE, Stores, []),
  Pid.

init(Stores) -> {ok, Stores}.

% Callback Routines
handle_call({up, Key, Value}, _From, Stores) ->
  Store = select_store(Stores, Key),
  Store ! {self(), {up, Key, Value}},
  receive
    {Store, Status} ->
      {reply, Status, Stores}
  end;

handle_call({read, Keys}, _From, Stores) ->
  Values = process_reads(Stores, Keys),
  {reply, Values, Stores};

handle_call({gc}, _From, Stores) ->
  gc(Stores),
  {reply, ok, Stores}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

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
%% Function: gc/1
%% Purpose:  Request a garbage collection on every stores.
%% Args:     Stores, list of Stores
%% Returns:  Request ok
%%--------------

gc([Head | Tail]) ->
  Head ! {self(), {gc}},
  gc(Tail);

gc([]) -> ok.


%%----------------------------------------------------------------------
%% Function: select_store/2
%% Purpose:  From the list of Stores, elect the target Store to acces the Key
%%           using a deterministic hash function.
%% Args:     Stores, list of Stores
%%           Key from the key/val pair
%% Returns:  Elected Store
%%--------------

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

%%----------------------------------------------------------------------
%% Function: score/2
%% Purpose:  Compute a score based on a value of the Store and desired Key
%% Args:     Position of the Store in list,
%%           Key from the key/val pair
%% Returns:  Score
%%--------------

score(Num, Key) ->
  List = lists:concat([Num, Key]),
  Hash = binary_to_list(crypto:hash(sha, List)),
  Score = sum_list(Hash),
  1.0 / math:log(Score).

%%----------------------------------------------------------------------
%% Function: sum_list/1
%% Purpose:  Sums a list of integers
%% Args:     List of integers
%% Returns:  Sum
%%--------------

sum_list(List) ->
  sum_list(List, 0).

sum_list([H|T], Acc) ->
  sum_list(T, Acc+H);

sum_list([], Acc) ->
  Acc.

%%----------------------------------------------------------------------
%% Function: timestamp/0
%% Purpose:  Return system time as timestamp
%% Returns:  timestamp
%%----------------------------------------------------------------------
timestamp() ->
  os:system_time(seconds).
