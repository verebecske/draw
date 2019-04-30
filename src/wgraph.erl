-module(wgraph). 
-export([wombat_graph/3, graph/5]).

-record(wgraph_opts, {width,height,numberOfLine,marginwidth,marginheight,maxValue}).

%transform data from wombat, add coord X
%return binary 
wombat_graph(Data,Date,Unit) ->
	NewData = add_parameter_X(Data,[]),
	OptsMap = #{width => 800, height => 500, marginwidth => 80, marginheight => 90},
	graph(NewData,Date,Unit,"wgraph.png",OptsMap).

graph(Data,Labels,Unit,Filename,OptsMap) -> 
	GraphOpt = create_options_record(OptsMap),
	Image = create(GraphOpt),
	{NewData,NewGraphOpt} = change_position(Data,GraphOpt),
	create_silver_lines(Image, NewGraphOpt),
	create_day_label(NewData, Labels, Image, NewGraphOpt),
	add_unit_label(Unit,Image,NewGraphOpt),
	add_lines(NewData,Image,NewGraphOpt),
	save(Image,Filename).

create_options_record(OptsMap) ->
	#wgraph_opts{
		numberOfLine = 0,
		width = maps:get(width,OptsMap),
		height = maps:get(height,OptsMap),
		marginwidth = maps:get(marginwidth,OptsMap), 
		marginheight = maps:get(marginheight,OptsMap)
		}.

%it create the basic white image with the time and value axis
%and add text "Wombat"
create(GraphOpt) ->
	Width = GraphOpt#wgraph_opts.width,
	Height = GraphOpt#wgraph_opts.height,
	MarginWidth = GraphOpt#wgraph_opts.marginwidth,
	MarginHeight = GraphOpt#wgraph_opts.marginheight,
	Image = egd:create(Width,Height),
	Color = egd:color(silver),
	P0 = {MarginWidth,MarginHeight},
	P1 = {MarginWidth,Height-MarginHeight},
	P2 = {Width-MarginWidth,Height-MarginHeight},
	egd:line(Image,P0,P1,Color),
	egd:line(Image,P1,P2,Color),
	create_wombat_label(Image,GraphOpt),
	Image.

%transform values to X and Y coordinate
change_position(Data,GraphOpt) ->
	MarginWidth = GraphOpt#wgraph_opts.marginwidth,
	MarginHeight = GraphOpt#wgraph_opts.marginheight,
	LineWidth = GraphOpt#wgraph_opts.width - (2 * MarginWidth),
	LineHeight = GraphOpt#wgraph_opts.height - (2 * MarginHeight),
	{MinW,MinH,MaxW,MaxH} = edges(Data),  
	Len = round(math:pow(10,length(integer_to_list(MaxH))-1)),
	Estimation = (floor(MaxH / Len)+1) * Len,
	SW = new_value(LineWidth,MinW,MaxW),
	SH = new_value(LineHeight,MinH,Estimation),
	NewData = lists:map( 
		fun({Name,Points}) -> 
			{Name, lists:map(
					fun({W,H}) ->
						NewW = (W * SW) + MarginWidth,
						NewH = mirroring((H * SH),LineHeight) + MarginHeight,
						{trunc(NewW),trunc(NewH)}
					end, Points)}
		end, Data),	
	{NewData,GraphOpt#wgraph_opts{maxValue = Estimation}}.

new_value(L,Min,Max) ->
	(L / (Max - Min)).

mirroring(Y, AY) ->
	SymmetryAxis = trunc(AY/2),
	case Y < SymmetryAxis of 
		true -> Y - 2 * (Y - SymmetryAxis);
		false -> Y + 2 * (SymmetryAxis - Y)
	end.

%find the minimal and maximal X and Y
edges(Data) ->
	MaxW = 0,
	MaxH = 0, 
	{_, DataPoints} = hd(Data),
	{MinH,MinW} = hd(DataPoints),
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
	create_label(Name, Image, Color, NewGraphOpt),
	create_line(Points,Image, Color),
	Image;

add_lines([ {Name, Points} | Data],Image,GraphOpt) ->
	{Color,NewGraphOpt} = color(GraphOpt),
	create_label(Name, Image, Color,NewGraphOpt),
	create_line(Points,Image, Color),
	add_lines(Data,Image,NewGraphOpt).

create_line([_],Image, _) -> Image;
create_line([P0,P1 | Points], Image, Color) ->
	egd:line(Image,P0,P1,Color),
	create_line([P1 | Points], Image, Color).

create_label(Name,Image,Color,GraphOpt) -> 
	Font = load_font("Terminus22.wingsfont"),
	StringName = erlang:atom_to_list(Name),
	H = GraphOpt#wgraph_opts.height,
	MH = GraphOpt#wgraph_opts.marginheight,
	W = GraphOpt#wgraph_opts.width,
	N = GraphOpt#wgraph_opts.numberOfLine,
	Y = H - trunc(MH / 2),
	X = (trunc(W / 4) * N) - 100,
	P = {X,Y},
	P0 = {X-5,Y+12},
	P1 = {X-10,Y+17},
	egd:filledEllipse(Image,P0,P1,Color),
	egd:text(Image, P, Font, StringName, Color),
	ok. 

create_wombat_label(Image,GraphOpt) ->
	P = {GraphOpt#wgraph_opts.width - 100, 15},
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
	Number = GraphOpt#wgraph_opts.numberOfLine,
	Color = 
		case Number of
			0 -> {58,135,189};
			1 -> {255,127,14};
			2 -> {44,160,44};
			_ -> green
		end,
	{egd:color(Color),GraphOpt#wgraph_opts{numberOfLine = Number + 1}}.  

load_font(Font) ->
    case erl_prim_loader:get_file(filename:join([code:priv_dir(draw),"fonts",Font])) of
        {ok,FontBinary,_} ->
            egd_font:load_binary(FontBinary);
        _ ->
            {ok,FontBinary,_} = erl_prim_loader:get_file(filename:join([code:priv_dir(draw),"draw/priv/fonts",Font])),
            egd_font:load_binary(FontBinary)
    end.

create_day_label(Data, Date, Image, GraphOpt) -> 
	Points = element(2,lists:max(lists:map( fun({_,Datas}) -> {length(Datas),Datas} end, Data))),
 	LabelPoints = lists:map(fun({X,_}) ->
 		{X - 15, (GraphOpt#wgraph_opts.height - GraphOpt#wgraph_opts.marginheight)}
 	end,Points),
 	Color = egd:color(silver),
 	Font = load_font("Helvetica20.wingsfont"),
 	add_day_label(LabelPoints,Date,Image,Font,Color).	

add_day_label([],_,Image,_,_) -> 
	Image;
add_day_label(_,[],Image,_,_) ->
	Image;
add_day_label([P | LabelPoints],[StringName | Date],Image,Font,Color) ->
	egd:text(Image, P, Font, StringName, Color),
	add_day_label(LabelPoints, Date ,Image,Font,Color).

create_silver_lines(Image,GraphOps) -> 
	MaxY = GraphOps#wgraph_opts.maxValue,
	Width = GraphOps#wgraph_opts.width,
	Height = GraphOps#wgraph_opts.height,
	MarginWidth = GraphOps#wgraph_opts.marginwidth,
	MarginHeight = GraphOps#wgraph_opts.marginheight,
	Color = egd:color({211,211,211,1}),
	Font = load_font("Helvetica20.wingsfont"),
	Unit = round(math:pow(10,length(integer_to_list(MaxY))-1)),
	LineCount = floor(MaxY / Unit), 
	LineDistance = floor((Height - 2 * MarginHeight) / LineCount),
	Labels = lists:seq(0,MaxY,Unit),
	Heights = lists:seq(Height-MarginHeight,MarginHeight,-LineDistance),
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
	MW = GraphOpt#wgraph_opts.marginwidth,
	MH = GraphOpt#wgraph_opts.marginheight,
	Color = egd:color(silver),
	Font = load_font("Helvetica20.wingsfont"),
	P = {round(MW / 2),MH-50},
	egd:text(Image, P, Font, Unit, Color).

add_parameter_X([],NewList) ->
	NewList;
add_parameter_X([{Label,List} | Rest], ListWithX) ->
	MinList = case List of
		[] -> [0,0];
		[One] -> [0,One];
		More -> More
	end,
	DataWithX = lists:zip(lists:seq(0,length(MinList)-1),MinList),
	NewListWithX = ListWithX ++ [{Label, DataWithX}],
	add_parameter_X(Rest,NewListWithX).
