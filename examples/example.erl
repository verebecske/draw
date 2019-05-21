-module(example).
-export([run/0]).

run() ->
    basic(),
    one_line(),
    points(),
    three_line(),
    example1(),
    example2().

basic() -> 
    Data = [ 
            { basic, [ {1,0} ] } ],
    Filename = generate_filename("doc","basic3.png"),
    Labels = ["1"],
    Unit = "Hello",
    OptsMap = #{width => 800, height => 500, margin_width => 80, 
                margin_height => 90, main_label => "Basic"},
    wgraph:graph(Data,Labels,Unit,Filename,OptsMap).

one_line() -> 
    Data = [ { basic, [ {0,0},{1,1} ] } ],
    Filename = generate_filename("doc","one_line3.png"),
    Labels = ["{0,0}","{1,1}"],
    Unit = "Hello",
    OptsMap = #{width => 800, height => 500, margin_width => 80, 
                margin_height => 90, main_label => "One line"},
    wgraph:graph(Data,Labels,Unit,Filename,OptsMap).

points() -> 
    Data = [ 
            {one,  [ {0,0}, {90,50} ] }, 
            {two,  [ {0,10}, {20,20}, {40,70}, {80,95} ] },
            {three,[ {0,0}, {10,20}, {30,50}, {60,15}, {100,90} ] }
           ],
    Filename = generate_filename("doc","points3.png"),
    Labels = ["0","10","30","60","100"],
    Unit = "Hello",
    OptsMap = #{width => 800, height => 500, margin_width => 80, 
                margin_height => 90, main_label => "Points"},
    wgraph:graph(Data,Labels,Unit,Filename,OptsMap).

three_line() ->
    Data = [ 
            {one,  [ {0,0},   {20,10}, {40,40}, {60,10}, {80,40}, {100,50} ] }, 
            {two,  [ {0,20}, {20,70}, {40,50}, {60,40}, {80,50}, {100,60} ] },
            {three,[ {0,90}, {20,30}, {40,60}, {60,80}, {80,60}, {100,30} ] }
           ],
    Filename = generate_filename("doc","three_line.png"),
    Labels = ["0","20","40","60","80","100"],
    Unit = "Hello",
    OptsMap = #{width => 800, height => 500, margin_width => 80, 
                margin_height => 90, main_label => "Graph"},
    wgraph:graph(Data,Labels,Unit,Filename,OptsMap).

example1() -> 
    Data = [ 
            { test1, [ {0,0}, {10, 40}, {20, 50}, {30, 69}, {40,76}, {50,94}] },
            { test2, [ {0,0}, {10, 10}, {20, 25}, {30, 30}, {40,45}, {50,60}] }
           ],
    Filename = generate_filename("examples","ex1.png"),
    Labels = ["0","10","20","30","40","50"],
    Unit = "Hello",
    OptsMap = #{width => 800, height => 500, margin_width => 80, 
                margin_height => 90, main_label => "Example 1"},
    wgraph:graph(Data,Labels,Unit,Filename,OptsMap).

example2() -> 
    Data = [ 
            {one,  [ {0,0},   {20,100}, {40,900}, {60,400}, {80,500}, {100,700} ] }, 
            {two,  [ {0,500}, {20,400}, {40,700}, {60,600}, {80,500}, {100,600} ] },
            {three,[ {0,900}, {20,700}, {40,500}, {60,300}, {80,900}, {100,400} ] },
            {four, [ {0,500}, {20,300}, {40,200}, {60,10},  {80,200}, {100,300} ] }
           ],
    Filename = generate_filename("examples","ex2.png"),
    Labels = ["0","20","40","60","80","100"],
    Unit = "Hello",
    OptsMap = #{width => 800, height => 500, margin_width => 80, 
                margin_height => 90, main_label => "Example 2"},
    wgraph:graph(Data,Labels,Unit,Filename,OptsMap).

generate_filename(Path,Filename) ->
    Abs = filename:absname("."),
    Split = filename:split(Abs),
    List = lists:dropwhile(fun(A) -> A /= "_build" end, lists:reverse(Split)),
    NewList = lists:droplast(lists:reverse(List)),
    PathList = filename:split(Path),
    filename:join(NewList ++ PathList ++ [Filename]).
