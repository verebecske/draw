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
%	draw_me_a_line(),
%	draw_something({me,3}),
	draw_something(1),
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
draw_me_a_line() ->
	Color = egd:color(silver),
	Image = egd:create(300,300),
	P0 = {0,0},
	P1 = {2,4},
	P2 = {600,900},
	egd:line(Image,P1,P2,Color),
	Png = egd:render(Image, png, [{render_engine, opaque}]),
	Count = 2,
	FileName = "pn" ++ [Count] ++ ".png",
	egd:save(Png, FileName),
    egd:destroy(Image),
    Png.

draw_something({me,Count}) ->
	Color = egd:color(silver),
	List = [black, silver, gray, white, maroon, red, purple, fuchia, green, lime, olive, yellow, navy, blue, teal, aqua],
	Number = lists:seq(1,length(List)),

	Image = egd:create(300,300),
	P0 = {10,30},
	P1 = {50,50},
	P2 = {150,150},
	case Count of 
		1 -> egd:line(Image,P1,P2,Color);
		2 -> egd:rectangle(Image,P1,P2,Color);
		3 -> egd:polygon(Image,[P0,P1,P2],Color);
		_ -> egd:arc(Image,P1,P2,Color)
	end,
	Png = egd:render(Image, png, [{render_engine, opaque}]),
	FileName = "k" ++ erlang:integer_to_list(Count) ++ ".png",
	egd:save(Png, FileName),
    egd:destroy(Image),
	case Count > 1 of
		true -> draw_something({me,Count-1});
		false -> ok
	end,
	Png;

draw_something(1) ->
	L1 = {test, [{1,1},{2,2},{8,7},{10,13}]},
	L2 = {test2, [{1,11},{2,21},{5,30},{10,50}]},
	L3 = {test3, [{1,5},{2,13},{5,35},{10,70}]},
	Bin = make_graph:graph([L1,L2,L3]),
	egd:save(Bin, "w2.png").


