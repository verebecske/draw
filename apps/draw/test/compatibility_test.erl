-module(compatibility_test).
-include_lib("eunit/include/eunit.hrl").
-export([]).

transform_test() ->
 WData = [
		{{{2019,4,1},{14,43,37,0}},1030764162.6666666},
		{{{2019,4,2},{14,58,37,0}},10977806360.0},
		{{{2019,4,3},{14,58,37,0}},10637780760.0},
		{{{2019,4,4},{14,58,37,0}},1047597780760.0},
		{{{2019,4,5},{14,58,37,0}},109788780760.0},
		{{{2019,4,6},{14,58,37,0}},109772280760.0},
		{{{2019,4,7},{14,58,37,0}},109772280760.0},
		{{{2019,4,8},{14,58,37,0}},109734780760.0}
		],
 EGData = 
 	[ 
 		{a, [{1,14},{2,14},{3,14},{4,14},{5,14},{6,14},{7,14},{8,14}]},
 		{b, [{1,43},{2,58},{3,58},{4,58},{5,58},{6,58},{7,58},{8,58}]},
 		{c, [{1,37},{2,37},{3,37},{4,37},{5,37},{6,37},{7,37},{8,37}]}
 	],

   Opt = [
		    {height,500},
		    {width,800},
		    {date, { {2019,4,1},{2019,4,8} } }
	    ],
	?assertMatch(EGData,wombat_graph(WData)).

wombat_graph(Data) ->
	transform(Data,[],[],[],[]).

transform([],Date,A,B,C) -> 
	?debugFmt("D: ~p A: ~p B: ~p C: ~p",[Date,A,B,C]),
	Opt = [],
	NA = lists:zip(lists:seq(1,length(A)),A),
	NB = lists:zip(lists:seq(1,length(B)),B),
	NC = lists:zip(lists:seq(1,length(C)),C),
	Data = [{a,NA},{b,NB},{c,NC}],
	Data;

transform(Datas,Date,A,B,C) ->
	{{D0,{A0,B0,C0,_}}, _ } = hd(Datas),
	ND = Date ++ [D0],
	NA = A ++ [A0],
	NB = B ++ [B0],
	NC = C ++ [C0],
	transform(tl(Datas),ND,NA,NB,NC).
%	graph(Data,Opt).