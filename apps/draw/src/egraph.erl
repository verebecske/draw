-module(egraph). %sorry for my english
-export([init/0]).

-record(opts, {width,height,numberOfLine,margowidth,margoheight}).

init() -> 
	L1 = {test1, [{0,1}, {2,2}, {8,7}, {10,13}, {15,40}, {20, 56}, {25, 60}, {30, 100}]},
	L2 = {test2, [{0,11},{2,21},{5,30},{10,50}, {17, 57}, {23, 71}, {42, 80}, {50,100}]},
	L3 = {test3, [{0,5}, {2,13},{5,35},{10,100}]},
	Opt = [{height,300},{width,600}],
	graph([L1,L2,L3],Opt).

graph(Data,Opt) -> 
	GraphOpt = create_options_record(Opt),
	Image = create(GraphOpt),
	NewData = change_position(Data,GraphOpt),
	NewImage = add_lines(NewData,Image,GraphOpt),
	save(Image).

create_options_record(Opts) ->
	GraphOpt = #opts{
	numberOfLine = 0,
	width = 800,
	height = 800
	},
	create_options_record(Opts,GraphOpt).
create_options_record([],GraphOpt) -> made_margo(GraphOpt);
create_options_record([{Label, Value} | Opts],GraphOpt) ->
	NewGraphOpt = case Label of
		width -> GraphOpt#opts{width = Value};
		height -> GraphOpt#opts{height = Value};
		_ -> GraphOpt
	end,
	create_options_record(Opts,NewGraphOpt).

made_margo(GraphOpt) ->
	Width = GraphOpt#opts.width,
	Height = GraphOpt#opts.height,
	NewGraphOpt = GraphOpt#opts{margowidth = trunc(Width / 10), margoheight = trunc(Height / 10)},
	NewGraphOpt.

add_lines([],Image,GraphOpt) -> Image;
add_lines([ {Name, Points} | Data],Image,GraphOpt) ->
	{Color,NewGraphOpt} = color(GraphOpt),
	make_label(Name, Image, Color,NewGraphOpt),
	make_line(Points,Image, Color),
	add_lines(Data,Image,NewGraphOpt).

make_line([Point],Image,Color) -> Image;
make_line([P0,P1 | Points], Image, Color) ->
	egd:line(Image,P0,P1,Color),
	make_line([P1 | Points], Image, Color).

% in this part create the basic image, the line, the frame 
create(GraphOpt) ->
	Width = GraphOpt#opts.width,
	Height = GraphOpt#opts.height,
	MW = GraphOpt#opts.margowidth,
	MH = GraphOpt#opts.margoheight,
	Image = egd:create(Width,Height),
	Color = egd:color(silver),
	P0 = {MW,MH},
	P1 = {MW,Height-MH},
	P2 = {Width-MW,Height-MH},
	egd:line(Image,P0,P1,Color),
	egd:line(Image,P1,P2,Color),
	make_number(GraphOpt),
	Image.

 %choose color and use an algoritm. I don't no yet what algoritm
color(GraphOpt) ->  
%	List = [black, silver, gray, white, maroon, red, purple, fuchia, green, lime, olive, yellow, navy, blue, teal, aqua],
	Number = GraphOpt#opts.numberOfLine,
	Color = 
		case Number of
			0 -> {58,135,189};
			1 -> {255,127,14};
			2 -> {44,160,44};
			_ -> yellow
		end,
	{egd:color(Color),GraphOpt#opts{numberOfLine = Number + 1}}.

save(Image) ->
	Count = 10,
	Png = egd:render(Image, png, [{render_engine, opaque}]),
	FileName = "wgraph" ++ erlang:integer_to_list(Count) ++ ".png",
	egd:save(Png, FileName),
    egd:destroy(Image),
    Png.

% in this part change the Datas positions, if it need
change_position(Data,GraphOpt) -> 
	MW = GraphOpt#opts.margowidth,
	MH = GraphOpt#opts.margoheight,
	LW = GraphOpt#opts.width - (2 * MW),
	LH = GraphOpt#opts.height - (2 * MH),
	{MinW,MinH,MaxW,MaxH} = edges(Data),
	SW = new_value(LW,MinW,MaxW),
	SH = new_value(LH,MinH,MaxH),
	NewData = lists:map(
		fun({Name,Points}) -> 
			{Name,
			lists:map(
				fun({W,H}) ->
					{(W * SW) + MW,mirroring((H * SH),LH)+ MH}
				end,
				Points)}
		end, Data),
	NewData.

edges(Data) ->
	MaxW = 0,
	MaxH = 0, 
	{_, Points} = hd(Data),
	{MinH,MinW} = hd(Points),
	Edges = {MinW,MinH,MaxW,MaxH},
	{_,Acc} = lists:mapfoldl(
			fun({Name,Points},NewEdges) -> 
				NewAcc = find_acc(Points,NewEdges),
				{{ Name,  NewAcc},
				acc(NewEdges, NewAcc)}
			end, Edges, Data),
	Acc.

find_acc(Points,Edges) ->
	{MinW,MinH,MaxW,MaxH} = Edges,
	{_,Acc} = 
	lists:mapfoldl(
		fun(A,B) ->
			{A, acc(A,B)}
		end,
	{MinW,MinH,MaxW,MaxH},Points),
	Acc.

acc(A,B) ->
	{MinW,MinH,MaxW,MaxH} = B,
	NewB = 
		case A of
			{W,H} -> acc({W,H,W,H},B);
			{OMinW,OMinH,OMaxW,OMaxH} -> 
				{
					min_v(OMinW,MinW),
					min_v(OMinH,MinH),
					max_v(OMaxW,MaxW),
					max_v(OMaxH,MaxH) 
				}
	end,
	NewB.

min_v(W,Nw) ->
	case Nw > W of
		true -> W;
		false -> Nw
	end.

max_v(H,Nh) ->
	case Nh > H of 
		true -> Nh;
		false -> H
	end.

new_value(L,Min,Max) ->
	trunc(L / (Max - Min)).

mirroring(Y, AY) ->
	SAxis = trunc(AY/2),
	case Y < SAxis of 
		true -> Y - 2 * (Y - SAxis);
		false -> Y + 2 * (SAxis - Y)
	end.


make_number(Opt) -> ok.
make_label(Name,Image,Color, GraphOpt) -> ok. %it make from a label that show what line what color
% Size() -> map


