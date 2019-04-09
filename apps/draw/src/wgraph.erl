-module(wgraph). 
-export([wombat_graph/2,graph/2]).

-record(opts, {width,height,numberOfLine,marginwidth,marginheight,dates,maxValue}).

wombat_graph([{LA,A},{LB,B},{LC,C}],Date) ->
	NA = lists:zip(lists:seq(0,length(A)-1),A),
	NB = lists:zip(lists:seq(0,length(B)-1),B),
	NC = lists:zip(lists:seq(0,length(C)-1),C),
	graph([{LA,NA},{LB,NB},{LC,NC}],Date).

graph(Data,Date) -> 
	GraphOpt = create_options_record(Date),
	Image = create(GraphOpt),
	{NewData,NewGraphOpt} = change_position(Data,GraphOpt),
	make_silver_lines(Image, NewGraphOpt),
	make_day_label(NewData, Date, Image, NewGraphOpt),
	add_lines(NewData,Image,NewGraphOpt),
	add_year_label(2019,Image),
	save(Image).

create_options_record(Date) ->
	Width = 800,
	#opts{
		numberOfLine = 0,
		width = Width,
		height = 500,
		dates = Date,
		marginwidth = trunc(Width / 10), 
		marginheight = 3 * 30}.

create(GraphOpt) ->
	Width = GraphOpt#opts.width,
	Height = GraphOpt#opts.height,
	MarginWidth = GraphOpt#opts.marginwidth,
	MarginHeight = GraphOpt#opts.marginheight,
	Image = egd:create(Width,Height),
	Color = egd:color(silver),
	P0 = {MarginWidth,MarginHeight},
	P1 = {MarginWidth,Height-MarginHeight},
	P2 = {Width-MarginWidth,Height-MarginHeight},
	egd:line(Image,P0,P1,Color),
	egd:line(Image,P1,P2,Color),
	make_wombat_label(Image,GraphOpt),
	Image.

change_position(Data,GraphOpt) ->
	MW = GraphOpt#opts.marginwidth,
	MH = GraphOpt#opts.marginheight,
	LW = GraphOpt#opts.width - (2 * MW),
	LH = GraphOpt#opts.height - (2 * MH),
	{MinW,MinH,MaxW,MaxH} = edges(Data),  
	Len = round(math:pow(10,length(integer_to_list(MaxH))-1)),
	Estimation = (floor(MaxH / Len)+1) * Len,
	SW = new_value(LW,MinW,MaxW),
	SH = new_value(LH,MinH,Estimation),
	NewData = lists:map( 
		fun({Name,Points}) -> 
			{Name, lists:map(
					fun({W,H}) ->
						NW = ((W * SW) + MW),
						NH = (mirroring((H * SH),LH)+ MH),
						{trunc(NW),trunc(NH)}
					end, Points)}
		end, Data),	
	{NewData,GraphOpt#opts{maxValue = Estimation}}.

new_value(L,Min,Max) ->
	(L / (Max - Min)).

mirroring(Y, AY) ->
	SAxis = trunc(AY/2),
	case Y < SAxis of 
		true -> Y - 2 * (Y - SAxis);
		false -> Y + 2 * (SAxis - Y)
	end.

edges(Data) ->
	MaxW = 0,
	MaxH = 0, 
	{_, Points} = hd(Data),
	{MinH,MinW} = hd(Points),
	Edges = {MinW,MinH,MaxW,MaxH},
	{_,Acc} = lists:mapfoldl(
			fun({Name,Points},NewEdges) -> 
				NewAcc = find_acc(Points,NewEdges),
				{{ Name, NewAcc},
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

add_lines([ {Name, Points} ],Image,GraphOpt) -> 
	{Color,NewGraphOpt} = color(GraphOpt),
	make_label(Name, Image, Color, NewGraphOpt),
	make_line(Points,Image, Color),
	Image;

add_lines([ {Name, Points} | Data],Image,GraphOpt) ->
	{Color,NewGraphOpt} = color(GraphOpt),
	make_label(Name, Image, Color,NewGraphOpt),
	make_line(Points,Image, Color),
	add_lines(Data,Image,NewGraphOpt).

make_line([_],Image, _) -> Image;
make_line([P0,P1 | Points], Image, Color) ->
	egd:line(Image,P0,P1,Color),
	make_line([P1 | Points], Image, Color).

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

make_wombat_label(Image,GraphOpt) ->
	WP = {GraphOpt#opts.width - 100, 15},
	WColor = egd:color({58,135,189}),
	WStringName = "Wombat",
	WFont = load_font("Helvetica20.wingsfont"),
	egd:text(Image, WP, WFont, WStringName, WColor),
	Image.

save(Image) ->
	Png = egd:render(Image, png, [{render_engine, opaque}]),
	FileName = "w1graph.png",
	egd:save(Png, FileName),
    egd:destroy(Image),
    Png.

color(GraphOpt) ->  
	Number = GraphOpt#opts.numberOfLine,
	Color = 
		case Number of
			0 -> {58,135,189};
			1 -> {255,127,14};
			2 -> {44,160,44};
			_ -> yellow
		end,
	{egd:color(Color),GraphOpt#opts{numberOfLine = Number + 1}}.  

load_font(Font) ->
    case erl_prim_loader:get_file(filename:join([code:priv_dir(draw),"fonts",Font])) of
        {ok,FontBinary,_} ->
            egd_font:load_binary(FontBinary);
        _ ->
            {ok,FontBinary,_} = erl_prim_loader:get_file(filename:join([code:priv_dir(draw),"draw/priv/fonts",Font])),
            egd_font:load_binary(FontBinary)
    end.

make_day_label(Data, Date, Image, GraphOpt) -> 
	{_, Points} = hd(Data),
 	LabelPoints = lists:map(fun({X,_}) ->
 		{X - 15, (GraphOpt#opts.height - GraphOpt#opts.marginheight)}
 	end,Points),
 	Color = egd:color(silver),
 	Font = load_font("Helvetica20.wingsfont"),
 	add_day_label(LabelPoints,Date,Image,Font,Color).	

add_day_label([],_,Image,_,_) -> 
	Image;
add_day_label([P | LabelPoints],[StringName | Date],Image,Font,Color) ->
	egd:text(Image, P, Font, StringName, Color),
	add_day_label(LabelPoints, Date ,Image,Font,Color).

add_year_label(Year,Image) ->
	Color = egd:color(silver),
	StringName = integer_to_list(Year),
	Font = load_font("Helvetica20.wingsfont"),
	P = {15,15},
	egd:text(Image, P, Font, StringName, Color).

make_silver_lines(Image,GraphOps) -> 
	MaxY = GraphOps#opts.maxValue,
	W = GraphOps#opts.width,
	H = GraphOps#opts.height,
	MW = GraphOps#opts.marginwidth,
	MH = GraphOps#opts.marginheight,
	Color = egd:color({211,211,211,1}),
	Font = load_font("Helvetica20.wingsfont"),
	Len = round(math:pow(10,length(integer_to_list(MaxY))-1)),
	Flo = (floor(MaxY / Len)),
	SL = floor((H - 2 * MH) / Flo),
	Labels = lists:seq(0,MaxY,Len),
	Heights = lists:seq(H-MH,MH,-SL),
	add_silver_line(Heights,Labels,Image,[MW,W-MW,Color,Font]).

add_silver_line([], _, Image, _) -> 
	Image;

add_silver_line([H| Heights], [Label |Lts],Image, A = [SBeginPoint, SEndPoint, Color, Font]) ->
	StringName = integer_to_list(Label),
	P0 = {SBeginPoint - 45,H - 15},
	P1 = {SBeginPoint, H},
	P2 = {SEndPoint, H},
	egd:text(Image, P0, Font, StringName, Color),
	egd:line(Image,P1,P2,Color),
	add_silver_line(Heights,Lts, Image, A).	

