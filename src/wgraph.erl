%%------------------------------------------------------------------------------
%% @doc wgraph modul
%% @end
%%------------------------------------------------------------------------------
-module(wgraph). 
-export([wombat_graph/3, graph/5]).

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------
-record(wgraph_opts, {width,height,numberOfLine,marginwidth,marginheight,maxValue}).
-type egd_image() :: pid().
-type point() :: {non_neg_integer(), non_neg_integer()}.
-type egd_color() :: {float(), float(), float(), float()}.

%%------------------------------------------------------------------------------
%% @doc Transform data from wombat, add coord X and default values.
%% 		Return binary.
%% @end
%%------------------------------------------------------------------------------
-spec wombat_graph( [{ atom(), [number()] }], [string()], string()) -> binary().
wombat_graph(Data,Date,Unit) ->
	NewData = add_parameter_X(Data,[]),
	OptsMap = #{width => 800, height => 500, marginwidth => 80, marginheight => 90},
	graph(NewData,Date,Unit,"wgraph.png",OptsMap).

%%------------------------------------------------------------------------------
%% @doc Transform the data, draw the image and save it.
%% 		It's important that number must be non-negative value!
%%		that doesn't kill the program, just generate ungly picture.
%% @end
%%------------------------------------------------------------------------------
-spec graph( [ {atom(), {number(),number()} } ], [string()], string(), string(), #wgraph_opts{}) -> binary().
graph(Data,Labels,Unit,Filename,OptsMap) -> 
	GraphOpt = create_options_record(OptsMap),
	Image = create(GraphOpt),
	{[GridLines|NewData],NewGraphOpt} = change_position(Data,GraphOpt),
	grid_lines(Image,GridLines,NewGraphOpt),
	create_x_labels(NewData, Labels, Image, NewGraphOpt),
	add_unit_label(Unit,Image,NewGraphOpt),
	add_lines(NewData,Image,NewGraphOpt),
	save(Image,Filename).

%%------------------------------------------------------------------------------
%% @doc Create options record, and add default values.
%% @end
%%------------------------------------------------------------------------------
-spec create_options_record(#wgraph_opts{}) -> #wgraph_opts{}.
create_options_record(OptsMap) ->
	#wgraph_opts{
		numberOfLine = 0,
		width = maps:get(width,OptsMap),
		height = maps:get(height,OptsMap),
		marginwidth = maps:get(marginwidth,OptsMap), 
		marginheight = maps:get(marginheight,OptsMap)
		}.

%%------------------------------------------------------------------------------
%% @doc It create the basic white image with the time and value axis, 
%% 		and add text "Wombat". See doc/just_create.png
%% @end
%%------------------------------------------------------------------------------
-spec create(#wgraph_opts{}) -> egd_image().
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

%%------------------------------------------------------------------------------
%% @doc Transform values to X and Y coordinate.
%% @end
%%------------------------------------------------------------------------------
-spec change_position([{atom(), [{number(),number()}]} ], #wgraph_opts{}) -> 
		{[{atom(), [{number(),number()}]} ], #wgraph_opts{}}.
change_position(Data,GraphOpt) ->
	MarginWidth = GraphOpt#wgraph_opts.marginwidth,
	MarginHeight = GraphOpt#wgraph_opts.marginheight,
	LineWidth = GraphOpt#wgraph_opts.width - (2 * MarginWidth),
	LineHeight = GraphOpt#wgraph_opts.height - (2 * MarginHeight),
	{MaxW, MaxH} = find_maxs(Data),
	MinW = 0, 
	MinH = 0,
	Len = round(math:pow(10,length(integer_to_list(floor(MaxH)))-1)),
	Estimation = (floor(MaxH / Len)+1) * Len,
	SW = new_value(LineWidth,MinW,MaxW),
	SH = new_value(LineHeight,MinH,Estimation),
	Grids = lists:zipwith(fun(A,B) -> [A,B] end,
			grid_list(Len,Estimation,MinW),grid_list(Len,Estimation,MaxW)),
	GridLines = [{grid, lists:flatten(Grids)}],
	NewData = lists:map( 
		fun({Name,Points}) -> 
			{Name, lists:map(
					fun({W,H}) ->
						NewW = (W * SW) + MarginWidth,
						NewH = mirroring((H * SH),LineHeight) + MarginHeight,
						{trunc(NewW),trunc(NewH)}
					end, Points)}
		end, GridLines ++ Data),	
	{NewData,GraphOpt#wgraph_opts{maxValue = Estimation}}.

-spec new_value(number(),number(),number()) -> number().
new_value(Line,Min,Max) ->
	case (Max - Min) of
		0 -> Line;
		_ ->(Line / (Max - Min))
	end.

%%------------------------------------------------------------------------------
%% @doc Mirroring the Y axis, because egd(0,0) not equal wgraph's data (0,0)
%% 		See in doc/just_create.png
%% @end
%%------------------------------------------------------------------------------
-spec mirroring(number(), number()) -> number().
mirroring(Y, AY) ->
	SymmetryAxis = trunc(AY/2),
	case Y < SymmetryAxis of 
		true -> Y - 2 * (Y - SymmetryAxis);
		false -> Y + 2 * (SymmetryAxis - Y)
	end.

%%------------------------------------------------------------------------------
%% @doc Find the maximum values in Data.	
%% @end
%%------------------------------------------------------------------------------
-spec find_maxs([{atom(),[{number(),number()}]}]) -> {number(),number()}.
find_maxs(Data) -> 
	lists:foldl(
		fun({_,Points},{MW,MH}) ->
		  lists:foldl(
			 	fun( {W,H}, {MaxW,MaxH} ) ->
			 		{ max(W,MaxW), max(H,MaxH)}
			 	end,{MW,MH},Points)
		end,{0,0},Data).

%%------------------------------------------------------------------------------
%% @doc It generate the X and Y coordinate for the girds.	
%% @end
%%------------------------------------------------------------------------------
-spec grid_list(number(),number(),number()) -> [ {number(),number()} ].
grid_list(Len,Estimation,XValue) -> 
	Ys = lists:seq(0,Estimation,Len),
	Xs = lists:duplicate(length(Ys),XValue),
	lists:zip(Xs,Ys).

%%------------------------------------------------------------------------------
%% @doc  Add gaphs' lines and labels.		
%% @end
%%------------------------------------------------------------------------------
-spec add_lines([{atom(),[point()]}], egd_image(),#wgraph_opts{}) -> egd_image().
add_lines([ {Name, Points} ],Image,GraphOpt) -> 
	{Color,NewGraphOpt} = color(GraphOpt),
	create_graph_label(Name, Image, Color, NewGraphOpt),
	create_line(Points,Image, Color),
	Image;

add_lines([ {Name, Points} | Data],Image,GraphOpt) ->
	{Color,NewGraphOpt} = color(GraphOpt),
	create_graph_label(Name, Image, Color,NewGraphOpt),
	create_line(Points,Image, Color),
	add_lines(Data,Image,NewGraphOpt).

-spec create_line([point()], egd_image(),egd_color()) -> egd_image().
create_line([_],Image, _) -> Image;
create_line([P0,P1 | Points], Image, Color) ->
	egd:line(Image,P0,P1,Color),
	create_line([P1 | Points], Image, Color).

%%------------------------------------------------------------------------------
%% @doc If you use more than 3 graph, I offer that change X coordianate
%%		the magical 4 constans is "split the picture 4 part",
%%		and the number of line is different all graph, 1,2,3...
%% @end
%%------------------------------------------------------------------------------
-spec create_graph_label([char()],pid(),egd_color(),#wgraph_opts{}) -> egd_image().
create_graph_label(Name,Image,Color,GraphOpt) -> 
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
	Image. 

%%------------------------------------------------------------------------------
%% @doc Create wombat label. In doc/points.png can you see the point P.
%% @end
%%------------------------------------------------------------------------------
-spec create_wombat_label(egd_image(),#wgraph_opts{}) -> egd_image().
create_wombat_label(Image,GraphOpt) ->
	P = {GraphOpt#wgraph_opts.width - 100, 15},
	Color = egd:color({58,135,189}),
	StringName = "Wombat",
	Font = load_font("Helvetica20.wingsfont"),
	egd:text(Image, P, Font, StringName, Color),
	Image.

%%------------------------------------------------------------------------------
%% @doc It render the image and save it. Return binary. 
%% @end
%%------------------------------------------------------------------------------
-spec save(egd_image(),string()) -> binary().
save(Image,Filename) ->
	Png = egd:render(Image, png, [{render_engine, opaque}]),
	file:write_file(Filename,Png),
    egd:destroy(Image),
    Png.

%%------------------------------------------------------------------------------
%% @doc It choose the color of lines, it garantee the all lines
%% 		(if you use just 4) are different color. 
%% @end
%%------------------------------------------------------------------------------
-spec color(#wgraph_opts{}) -> {egd_color(),#wgraph_opts{}}.
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
    case erl_prim_loader:get_file(filename:join([code:priv_dir(wgraph),"fonts",Font])) of
        {ok,FontBinary,_} ->
            egd_font:load_binary(FontBinary);
        _ ->
            {ok,FontBinary,_} = erl_prim_loader:get_file(filename:join([code:priv_dir(wgraph),"wgraph/priv/fonts",Font])),
            egd_font:load_binary(FontBinary)
    end.

%%------------------------------------------------------------------------------
%% @doc It create the labels under X axis. It use the most length 
%% 		datalist's x points. It can you see in doc/points.png
%% @end
%%------------------------------------------------------------------------------
create_x_labels(Data, Date, Image, GraphOpt) -> 
	Points = element(2,lists:max(lists:map(fun({_,Datas}) -> {length(Datas),Datas} end,Data))),
 	LabelPoints = lists:map(fun({X,_}) ->
 		{X - 15, (GraphOpt#wgraph_opts.height - GraphOpt#wgraph_opts.marginheight)}
 							end,Points),
 	Color = egd:color(silver),
 	Font = load_font("Helvetica20.wingsfont"),
 	add_x_label(LabelPoints,Date,Image,Font,Color).	

add_x_label([],_,Image,_,_) -> 
	Image;
add_x_label(_,[],Image,_,_) ->
	Image;
add_x_label([P | LabelPoints],[StringName | Strings],Image,Font,Color) ->
	egd:text(Image, P, Font, StringName, Color),
	add_x_label(LabelPoints, Strings,Image,Font,Color).

%%------------------------------------------------------------------------------
%% @doc It create the grid lines. 	
%% @end
%%------------------------------------------------------------------------------
-spec grid_lines(egd_image(),[{number(),number()}], #wgraph_opts{}) -> egd_image().
grid_lines(Image,GridLines,GraphOpt) ->
	MaxY = GraphOpt#wgraph_opts.maxValue,
	Len = length(integer_to_list(MaxY)),
	Unit = case MaxY == round(math:pow(10,Len-1)) of
		true -> round(math:pow(10,Len-2));
		_ -> round(math:pow(10,Len-1))
	end,
	Labels = case Unit of
		0 -> lists:seq(0,MaxY);
		_ -> lists:seq(0,MaxY,Unit)
			end,
	Color = egd:color(silver),
	{_, GridPoints} = GridLines,
	Font = load_font("Helvetica20.wingsfont"),
	add_grid_lines(GridPoints,Image,Color,Labels,Font),
	Image.

%%------------------------------------------------------------------------------
%% @doc  I use the Label length because the PT is in the begin of the text.
%%		 Just like the create_wombat_label/2, see in doc/points.png	
%% @end
%%------------------------------------------------------------------------------
add_grid_lines(_,Image,_,[],_) -> Image;
add_grid_lines([],Image,_,_,_) -> Image;
add_grid_lines([ {X,Y} = P0,P1 | GridPoints],Image,Color,[Label | Labels],Font) ->
	StringLabel = integer_to_list(Label),
	PT = {X - (length(StringLabel))* 10,Y - 15},
	egd:line(Image,P0,P1,Color),
	egd:text(Image, PT, Font, StringLabel, Color),
	add_grid_lines(GridPoints,Image,Color,Labels,Font).

%%------------------------------------------------------------------------------
%% @doc Add unit label. 	
%% @end
%%------------------------------------------------------------------------------
-spec add_unit_label([char()], egd_image(), #wgraph_opts{}) -> egd_image().
add_unit_label(Unit, Image, GraphOpt) ->
	MW = GraphOpt#wgraph_opts.marginwidth,
	MH = GraphOpt#wgraph_opts.marginheight,
	Color = egd:color(silver),
	Font = load_font("Helvetica20.wingsfont"),
	P = {round(MW / 2),MH-50},
	egd:text(Image, P, Font, Unit, Color),
	Image.

%%------------------------------------------------------------------------------
%% @doc It transform a list of values to list of {n,values}, where n is a 
%% 		non-negative integer.
%% @end
%%------------------------------------------------------------------------------
-spec add_parameter_X([{atom(), [number()]}],[{atom(),[{number(),number()}]}]) -> 
		[{atom(),[{number(),number()}]}].
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
