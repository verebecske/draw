-module(egraph). %sorry for my english
-export([init/0]).

-record(opts, {width,height,numberOfLine,margowidth,margoheight,date}).

init() -> 
	L1 = {test1, [{0,150}, {100,40}, {150,95}, {200,160}, {300,190}, {350, 270}, {400, 50}, {600, 20}]},
    L2 = {test2, [{0,200}, {100,60}, {150,190}, {200,10}, {300,90}, {350, 220}, {400, 90}, {600, 20}]},
    L3 = {test3, [{0,50},  {100,100}, {150,9}, {200,100}, {300,100}, {350, 20}, {400, 150}, {600, 120}]},
    Opt = [{height,400},{width,800},{date,{firstDay,firstMonth,lastDay,lastMonth}}],
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
	height = 800,
	date = false
	},
	create_options_record(Opts,GraphOpt).
create_options_record([],GraphOpt) -> made_margo(GraphOpt);
create_options_record([{Label, Value} | Opts],GraphOpt) ->
	NewGraphOpt = case Label of
		width -> GraphOpt#opts{width = Value};
		height -> GraphOpt#opts{height = Value};
		date -> GraphOpt#opts{date = true};
		_ -> GraphOpt
	end,
	create_options_record(Opts,NewGraphOpt).

made_margo(GraphOpt) ->
	Width = GraphOpt#opts.width,
	Height = GraphOpt#opts.height,
	NewGraphOpt = GraphOpt#opts{margowidth = trunc(Width / 10), margoheight = 3 * 24},
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
	Count = 13,
	Png = egd:render(Image, png, [{render_engine, opaque}]),
	FileName = "wgraph" ++ erlang:integer_to_list(Count) ++ ".png",
	egd:save(Png, FileName),
    egd:destroy(Image),
    Png.

% in this part change the Datas positions, if it need
change_position(Data,GraphOpt = #opts{date = true}) ->
	NewData = lists:map(
			fun({Name,Points}) ->
				{_, NewPoints} 
					= lists:mapfoldl(fun({X,Y},NX) ->
						{
						{NX,Y},
						NX + 50
						}
					end, 0, Points),
			{Name, NewPoints}
			end,
			Data),
	change_position(NewData,GraphOpt#opts{date = false});
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
	io:format("Acc : ~p ~n", [Acc]),
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
					erlang:min(OMinW,MinW),
					erlang:min(OMinH,MinH),
					erlang:max(OMaxW,MaxW),
					erlang:max(OMaxH,MaxH) 
				}
		end,
	NewB.

new_value(L,Min,Max) ->
	round(L / (Max - Min)).

mirroring(Y, AY) ->
	SAxis = trunc(AY/2),
	case Y < SAxis of 
		true -> Y - 2 * (Y - SAxis);
		false -> Y + 2 * (SAxis - Y)
	end.

%it make from a label that show what line what color
make_label(Name,Image,Color,GraphOpt) -> 
	Font = load_font("Terminus22.wingsfont"),
	StringName = erlang:atom_to_list(Name),
	Y = GraphOpt#opts.height - (GraphOpt#opts.numberOfLine * 22) - 5,
	P = {15,Y},
	P0 = {7,Y+12},
	P1 = {12,Y+17},
	egd:filledEllipse(Image,P0,P1,Color),
	egd:text(Image, P, Font, StringName, Color),
	ok. 

load_font(Font) ->
    case erl_prim_loader:get_file(filename:join([code:priv_dir(draw),"fonts",Font])) of
        {ok,FontBinary,_} ->
            egd_font:load_binary(FontBinary);
        _ ->
            {ok,FontBinary,_} = erl_prim_loader:get_file(filename:join([code:priv_dir(draw),"draw/priv/fonts",Font])),
            egd_font:load_binary(FontBinary)
    end.


make_number(GraphOpt) -> ok.
