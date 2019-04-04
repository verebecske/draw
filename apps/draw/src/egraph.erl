-module(egraph). %sorry for my english
-export([init/0]).

-record(opts, {width,height,numberOfLine,margowidth,margoheight,date,dateValue}).

init() -> 
	L1 = {test1, [{0,150}, {100,40}, {150,95}, {200,160}, {300,190}, {350, 270}, {400, 50}, {600, 20}]},
    L2 = {test2, [{0,200}, {100,60}, {150,190}, {200,10}, {300,90}, {350, 220}, {400, 90}, {600, 20}]},
    L3 = {test3, [{0,50},  {100,100}, {150,9}, {200,100}, {300,100}, {350, 20}, {400, 150}, {600, 120}]},
    Opt = [
		    {height,500},
		    {width,800},
		    {date, { {2019,2,4},{2019,2,12} } }
	    ],
    % {date, {date(), date()}}
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
		date = false,
		dateValue = {{2019,2,4},{2019,6,25}}
	},
	create_options_record(Opts,GraphOpt).
create_options_record([],GraphOpt) -> made_margo(GraphOpt);
create_options_record([{Label, Value} | Opts],GraphOpt) ->
	NewGraphOpt = case Label of
		width -> GraphOpt#opts{width = Value};
		height -> GraphOpt#opts{height = Value};
		date -> add_date(Value,GraphOpt);
		_ -> GraphOpt
	end,
	create_options_record(Opts,NewGraphOpt).

%I trust you and than you use valid values
add_date(Value, GraphOpt) -> 
	GraphOpt#opts{date = true, dateValue = Value}.

made_margo(GraphOpt) ->
	Width = GraphOpt#opts.width,
	Height = GraphOpt#opts.height,
	NewGraphOpt = GraphOpt#opts{margowidth = trunc(Width / 10), margoheight = 3 * 30},
	NewGraphOpt.

add_lines([ {Name, Points} ],Image,GraphOpt) -> 
	{Color,NewGraphOpt} = color(GraphOpt),
	make_label(Name, Image, Color, NewGraphOpt),
	make_line(Points,Image, Color),
	make_number(Points, Image, NewGraphOpt),
	Image;

add_lines([ {Name, Points} | Data],Image,GraphOpt) ->
	{Color,NewGraphOpt} = color(GraphOpt),
	make_label(Name, Image, Color,NewGraphOpt),
	make_line(Points,Image, Color),
	add_lines(Data,Image,NewGraphOpt).

make_line([Point],Image,Color) -> Image;
make_line([P0,P1 | Points], Image, Color) ->
	egd:line(Image,P0,P1,Color),
	make_line([P1 | Points], Image, Color).

% this part creates the basic image and the magic silver lines
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
	Count = 15,
	Png = egd:render(Image, png, [{render_engine, opaque}]),
	FileName = "wgraph" ++ erlang:integer_to_list(Count) ++ ".png",
	egd:save(Png, FileName),
    egd:destroy(Image),
    Png.

%this part changes the data's positions
change_position(Data,GraphOpt = #opts{date = true}) ->
	NewData = lists:map(
			fun({Name,Points}) ->
				{NewPoints,_} = lists:mapfoldl(
					fun({X,Y},NX) ->
						{ {NX,Y}, NX + 45 } %black magic that doesn't make me happy....
					end, 0, Points),
				{Name, NewPoints}
			end, Data),
	change_position(NewData,GraphOpt#opts{date = false});

change_position(Data,GraphOpt) -> 
	MW = GraphOpt#opts.margowidth,
	MH = GraphOpt#opts.margoheight,
	LW = GraphOpt#opts.width - (2 * MW),
	LH = GraphOpt#opts.height - (2 * MH),
	{MinW,MinH,MaxW,MaxH} = edges(Data),
	SW = new_value(LW,MinW,MaxW),
	SH = new_value(LH,MinH,MaxH),
	lists:map(
		fun({Name,Points}) -> 
			{Name, lists:map(
					fun({W,H}) ->
						{(W * SW) + MW,mirroring((H * SH),LH)+ MH}
					end, Points)}
		end, Data).

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
		end, {MinW,MinH,MaxW,MaxH},Points),
	Acc.

%yes, I was sooooo creative
acc(A,B) ->
	{MinW,MinH,MaxW,MaxH} = B,
	case A of
		{W,H} -> acc({W,H,W,H},B);
		{OMinW,OMinH,OMaxW,OMaxH} -> 
			{
				erlang:min(OMinW,MinW),
				erlang:min(OMinH,MinH),
				erlang:max(OMaxW,MaxW),
				erlang:max(OMaxH,MaxH) 
			}
	end.

new_value(L,Min,Max) ->
	round(L / (Max - Min)).

mirroring(Y, AY) ->
	SAxis = trunc(AY/2),
	case Y < SAxis of 
		true -> Y - 2 * (Y - SAxis);
		false -> Y + 2 * (SAxis - Y)
	end.

%this adds the label to the graph
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

make_number(Points, Image, GraphOpt) -> 
	LabelPoints = lists:map(fun({X,Y}) ->
		{X - 15, (GraphOpt#opts.height - GraphOpt#opts.margoheight)}
	end,Points),
	{BeginDate,EndDate} = GraphOpt#opts.dateValue,
	Dates = days({BeginDate,EndDate}) ++ [BeginDate],
	NewLabelPoints = LabelPoints ++ [{GraphOpt#opts.width - 100, 15}],
	make_day_label(NewLabelPoints,Dates,Image),
	Image.

make_day_label([WP],[{Y,_,_}],Image) -> 
	Color = egd:color(silver),
	StringName = integer_to_list(Y),
	Font = load_font("Helvetica20.wingsfont"),
	P = {15,15},
	egd:text(Image, P, Font, StringName, Color),
	WColor = egd:color({58,135,189}),
	WStringName = "Wombat",
	WFont = load_font("Helvetica20.wingsfont"),
	egd:text(Image, WP, WFont, WStringName, WColor),
	Image;

make_day_label([P | LabelPoints],[{Y,M,D} | Dates],Image) ->
	Color = egd:color(silver),
	StringName = integer_to_list(D) ++ "/ " ++ integer_to_list(M),
	Font = load_font("Helvetica20.wingsfont"),
	egd:text(Image, P, Font, StringName, Color),
	make_day_label(LabelPoints,Dates,Image).

-spec days({calerdar:date(),calendar:date()}) -> [calendar:date()].
days({BeginDate,EndDate}) ->
	Dates = [{2019,1,1},{2019,1,2},{2019,1,3},{2019,1,4},{2019,1,5},{2019,1,6},{2019,1,7},{2019,1,8}],	
	Dates.


%make_silver_lines(Image,GraphOps) -> 
% v = round(maxY / len(maxY)) * len(maxY),
% lists:mapfoldl(fun({X,Y},L) -> 
%{ {MW,L+len} , {W-MW, L+len} } end, 0, Points),
%egd:line().
	
	
