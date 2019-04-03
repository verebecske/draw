-module(egraph). %sorry for my english
-export([init/0]).

init() -> 
	L1 = {test1, [{1,1}, {2,2}, {8,7}, {10,13}]},
	L2 = {test2, [{1,11},{2,21},{5,30},{10,50}]},
	L3 = {test3, [{1,5}, {2,13},{5,35},{10,70}]},
	graph([L1,L2,L3],opt).

graph(Data,Opt) -> 
	Image = create(Opt),
	NewData = change_position(Data,Opt),
	NewImage = add_lines(NewData,Image),
	save(Image).

add_lines(Data,Image) -> 
	add_lines(Data,Image,0).
add_lines([],Image,Number) -> Image;
add_lines([ {Name, Points} | Data],Image,Number) ->
	Color = color(Number),
	make_label(Name, Image, Color),
	make_line(Points,Image, Color),
	add_lines(Data,Image,Number+1).

make_line([Point],Image,Color) -> Image;
make_line([P0,P1 | Points], Image, Color) ->
	egd:line(Image,P0,P1,Color),
	make_line([P1 | Points], Image, Color).

% in this part create the basic image, the line, the frame 
create(Opt) ->
	Width = 600,
	Height = 300,
	MW = trunc(Width / 10),
	MH = trunc(Height / 10),
	Image = egd:create(Width,Height),
	Color = egd:color(silver),
	P0 = {MW,MH},
	P1 = {MW,Height-MH},
	P2 = {Width-MW,Height-MH},
	egd:line(Image,P0,P1,Color),
	egd:line(Image,P1,P2,Color),
%	make_number(Opt),
	Image.

 %choose color and use an algoritm. I don't no yet what algoritm
color(Number) ->  
	List = [black, silver, gray, white, maroon, red, purple, fuchia, green, lime, olive, yellow, navy, blue, teal, aqua],
	Color = case Number of
		0 -> green;
		1 -> blue;
		2 -> yellow;
		_ -> red
	end,
	egd:color(Color).

save(Image) ->
	Count = 2,
	Png = egd:render(Image, png, [{render_engine, opaque}]),
	FileName = "wgraph" ++ erlang:integer_to_list(Count) ++ ".png",
	egd:save(Png, FileName),
    egd:destroy(Image),
    Png.

make_number(Opt) -> ok.
make_label(Name,Image,Color) -> ok. %it make from a label that show what line what color
change_position(Data,Opt) -> Data. % in this part change the Datas positions, if it need


% Size() -> map


