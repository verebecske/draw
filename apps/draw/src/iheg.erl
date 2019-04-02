-module(iheg).
-export([all]).

-graph(Data,Opt) -> 
  Image = create(Size),
  NewData = change_things(Data,Opt),
  NewImage = add_lines(NewData, Image),
  save(NewImage).

add_lines([],Image) -> Image;
add_lines(Data,Image) ->
  add_lines(lists:tail(Data),Image).
