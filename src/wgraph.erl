-module(wgraph). 
-export([wombat_graph/3]).

-record(opts, {width,height,numberOfLine,marginwidth,marginheight,dates,maxValue}).

wombat_graph([{Label1,A},{Label2,B},{Label3,C}],Date,Opt) ->
	NewA = lists:zip(lists:seq(0,length(A)-1),A),
	NewB = lists:zip(lists:seq(0,length(B)-1),B),
	NewC = lists:zip(lists:seq(0,length(C)-1),C),
	graph([{Label1,NewA},{Label2,NewB},{Label3,NewC}],Date,Opt),
	ok.

graph(Data,Date,[Unit,Filename]) -> 
	GraphOpt = create_options_record(Date),
	Image = create(GraphOpt),
	{NewData,NewGraphOpt} = change_position(Data,GraphOpt),
	make_silver_lines(Image, NewGraphOpt),
	make_day_label(NewData, Date, Image, NewGraphOpt),
	add_unit_label(Unit,Image,NewGraphOpt),
	add_lines(NewData,Image,NewGraphOpt),
	save(Image,Filename).

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
	MarginWidth = GraphOpt#opts.marginwidth,
	MarginHeight = GraphOpt#opts.marginheight,
	LineWidth = GraphOpt#opts.width - (2 * MarginWidth),
	LineHeight = GraphOpt#opts.height - (2 * MarginHeight),
	{MinW,MinH,MaxW,MaxH} = edges(Data),  
	Len = round(math:pow(10,length(integer_to_list(MaxH))-1)),
	Estimation = (floor(MaxH / Len)+1) * Len,
	SW = new_value(LineWidth,MinW,MaxW),
	SH = new_value(LineHeight,MinH,Estimation),
	NewData = lists:map( 
		fun({Name,Points}) -> 
			{Name, lists:map(
					fun({W,H}) ->
						NewW = ((W * SW) + MarginWidth),
						NewH = (mirroring((H * SH),LineHeight)+ MarginHeight),
						{trunc(NewW),trunc(NewH)}
					end, Points)}
		end, Data),	
	{NewData,GraphOpt#opts{maxValue = Estimation}}.

new_value(L,Min,Max) ->
	(L / (Max - Min)).

mirroring(Y, AY) ->
	SymmetryAxis = trunc(AY/2),
	case Y < SymmetryAxis of 
		true -> Y - 2 * (Y - SymmetryAxis);
		false -> Y + 2 * (SymmetryAxis - Y)
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
	H = GraphOpt#opts.height,
	MH = GraphOpt#opts.marginheight,
	W = GraphOpt#opts.width,
	N = GraphOpt#opts.numberOfLine,
	Y = H - trunc(MH / 2),
	X = (trunc(W / 4) * N) - 100,
	P = {X,Y},
	P0 = {X-5,Y+12},
	P1 = {X-10,Y+17},
	egd:filledEllipse(Image,P0,P1,Color),
	egd:text(Image, P, Font, StringName, Color),
	ok. 

make_wombat_label(Image,GraphOpt) ->
	P = {GraphOpt#opts.width - 100, 15},
	Color = egd:color({58,135,189}),
	StringName = "Wombat",
	Font = load_font("Helvetica20.wingsfont"),
	egd:text(Image, P, Font, StringName, Color),
	Image.

save(Image,Filename) ->
	Png = egd:render(Image, png, [{render_engine, opaque}]),
	file:write_file(Filename,Png),
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

make_silver_lines(Image,GraphOps) -> 
	MaxY = GraphOps#opts.maxValue,
	Width = GraphOps#opts.width,
	Height = GraphOps#opts.height,
	MarginWidth = GraphOps#opts.marginwidth,
	MarginHeight = GraphOps#opts.marginheight,
	Color = egd:color({211,211,211,1}),
	Font = load_font("Helvetica20.wingsfont"),
	Len = round(math:pow(10,length(integer_to_list(MaxY))-1)),
	Flo = floor(MaxY / Len),
	SL = floor((Height - 2 * MarginHeight) / Flo),
	Labels = lists:seq(0,MaxY,Len),
	Heights = lists:seq(Height-MarginHeight,MarginHeight,-SL),
	add_silver_line(Heights,Labels,Image,[MarginWidth,Width-MarginWidth,Color,Font]).

add_silver_line([], _, Image, _) -> 
	Image;

add_silver_line([Height| Heights], [Label |Labels],Image, Const = [SBeginPoint, SEndPoint, Color, Font]) ->
	StringLabel = integer_to_list(Label),
	StringName = integer_to_list(Label),
	P0 = {SBeginPoint - (length(StringLabel))* 10,Height - 15},
	P1 = {SBeginPoint, Height},
	P2 = {SEndPoint, Height},
	egd:text(Image, P0, Font, StringName, Color),
	egd:line(Image,P1,P2,Color),
	add_silver_line(Heights,Labels, Image, Const).	

add_unit_label(Unit, Image, GraphOpt) ->
	MW = GraphOpt#opts.marginwidth,
	MH = GraphOpt#opts.marginheight,
	Color = egd:color(silver),
	Font = load_font("Helvetica20.wingsfont"),
	P = {round(MW / 2),MH-50},
	egd:text(Image, P, Font, Unit, Color).
