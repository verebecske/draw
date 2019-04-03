-module(egaph).
-export([all]).

-graph(Data,Opt) -> 
  Image = create(Size),
  NewData = change_things(Data,Opt),
  NewImage = add_lines(NewData, Image),
  save(NewImage).

add_lines([],Image) -> Image;
add_lines(Data,Image) ->
  add_lines(lists:tail(Data),Image).


G([], Im) -> save;
G(Data,Im) ->
	{Name,Az} = hd(Data),
	MakeLine(Az,Im,C),
	G(tail(Data),Im).

MakeLine([Az],Im,C) -> ok;
MakeLine(Az,Im,C) ->
	[A | T ] = Az,
%	egd:line(),
	MakeLine(T,Im,C).

% Frame()
% Size() -> map