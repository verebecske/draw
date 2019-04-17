%%%-------------------------------------------------------------------
%% @doc draw top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(draw_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	tester(),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================

tester() ->
	WDE4 = [ {total_memory,		[3000,4300,5005,3040,6005,1300,5002]},
			{process_memory,	[4200,8597,2040,5400,1002,7006,7040]},
			{system_memory,		[4003,4500,4070,4005,6500,4020,5040]}], 
	Dates = ["14/02", "15/02","16/02", "17/02","18/02", "19/02", "20/02"],
	Filename = generate_filename("src","w.png"),
	wgraph:wombat_graph(WDE4,Dates,["(MB)",Filename]).

generate_filename(Path,Filename) ->
	Abs = filename:absname("."),
	Split = filename:split(Abs),
	List = lists:dropwhile(fun(A) -> A /= "_build" end, lists:reverse(Split)),
	NewList = lists:droplast(lists:reverse(List)),
	PathList = filename:split(Path),
	filename:join(NewList ++ PathList ++ [Filename]).
