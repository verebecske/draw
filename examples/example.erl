-module(example).
-export([run/0]).

run() ->
	tester1(),
	tester2(). 

tester1() -> 
Data = [ { test1, [ {0,0}, {10, 40}, {20, 50}, {30, 69}, {40,76}, {50,94}] },
 		 { test2, [ {0,0}, {10, 10}, {20, 25}, {30, 30}, {40,45}, {50,60}] }
 		],
Filename = generate_filename("examples","ex1.png"),
Labels = ["0","10","20","30","40","50"],
Unit = "Hello",
OptsMap = #{width => 800, height => 500, marginwidth => 80, marginheight => 90},
wgraph:graph(Data,Labels,Unit,Filename,OptsMap).

tester2() -> 
Data = [ {one, [ {0,0}, {20,100}, {40,900}, {60,400}, {80,500}, {100,700} ] }, 
 		 {two, [ {0,500}, {20,400}, {40,700}, {60,600}, {80,500}, {100,600} ] },
 		 {tree,[ {0,900}, {20,700}, {40,500}, {60,300}, {80,900}, {100,400} ] },
 		 {four,[ {0,500}, {20,300}, {40,200}, {60,10}, {80,200}, {100,300} ] }
 		],
Filename = generate_filename("examples","ex2.png"),
Labels = ["0","20","40","60","80","100"],
Unit = "Hello",
OptsMap = #{width => 800, height => 500, marginwidth => 80, marginheight => 90},
wgraph:graph(Data,Labels,Unit,Filename,OptsMap).

generate_filename(Path,Filename) ->
Abs = filename:absname("."),
Split = filename:split(Abs),
List = lists:dropwhile(fun(A) -> A /= "_build" end, lists:reverse(Split)),
NewList = lists:droplast(lists:reverse(List)),
PathList = filename:split(Path),
filename:join(NewList ++ PathList ++ [Filename]).