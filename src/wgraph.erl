%%------------------------------------------------------------------------------
%% @doc wgraph modul
%% @end
%%------------------------------------------------------------------------------
-module(wgraph).
-export([graph/5, wgraph/4]).

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------
-record(wgraph_opts,
	{width, height, actual_line_number, margin_width,
	 margin_height, max_y, main_label}).

-type egd_image() :: pid().
-type point() :: {non_neg_integer(), non_neg_integer()}.
-type egd_color() :: {float(), float(), float(), float()}.

%%------------------------------------------------------------------------------
%% @doc Transform data from wombat, add coord X and default filename.
%%      Returns image as binary.
%%      Data format: [{metric_label,[Y1,Y2,Y3, ... ]}, ... ]
%% @end
%%------------------------------------------------------------------------------
-spec wgraph([{atom(), [number()]}], [string()],
	     string(), #wgraph_opts{}) -> binary().

wgraph(Data, Labels, Unit, OptsMap) ->
    NewData = add_parameter_x(Data, []),
    graph(NewData, Labels, Unit, "wgraph.png", OptsMap).

%%------------------------------------------------------------------------------
%% @doc Transforms the data, draws the image and saves it.
%%      It's important that values must be non-negative!
%%      Don't use empty list.
%% 		Data format: [{metric_label,[{X1,Y1},{X2,Y2},{X3,Y3}, ... ]}, ... ]
%% @end
%%------------------------------------------------------------------------------
-spec graph([{atom(), {number(), number()}}], [string()], string(), string(),
	    #wgraph_opts{}) -> binary().

graph(Data, Labels, Unit, Filename, OptsMap) ->
    GraphOpt = create_options_record(OptsMap),
    Image = create(GraphOpt),
    {[GridLines | NewData], NewGraphOpt} =
	change_position(Data, GraphOpt),
    grid_lines(Image, GridLines, NewGraphOpt),
    create_x_labels(NewData, Labels, Image, NewGraphOpt),
    add_unit_label(Unit, Image, NewGraphOpt),
    add_lines(NewData, Image, NewGraphOpt),
    save(Image, Filename).

%%------------------------------------------------------------------------------
%% @doc Create an options record with default values.
%% @end
%%------------------------------------------------------------------------------
-spec create_options_record(#wgraph_opts{}) -> #wgraph_opts{}.

create_options_record(OptsMap) ->
    #wgraph_opts{actual_line_number = 0,
		 width = maps:get(width, OptsMap, 800),
		 height = maps:get(height, OptsMap, 500),
		 margin_width = maps:get(margin_width, OptsMap, 80),
		 margin_height = maps:get(margin_height, OptsMap, 90),
		 main_label = maps:get(main_label, OptsMap, "Wombat")}.

%%------------------------------------------------------------------------------
%% @doc It creates the basic white image with the time and value axis,
%%      and adds the main label. See doc/just_create.png
%% @end
%%------------------------------------------------------------------------------
-spec create(#wgraph_opts{}) -> egd_image().

create(GraphOpt) ->
    Width = GraphOpt#wgraph_opts.width,
    Height = GraphOpt#wgraph_opts.height,
    MarginWidth = GraphOpt#wgraph_opts.margin_width,
    MarginHeight = GraphOpt#wgraph_opts.margin_height,
    Image = egd:create(Width, Height),
    Color = egd:color(silver),
    P0 = {MarginWidth, MarginHeight},
    P1 = {MarginWidth, Height - MarginHeight},
    P2 = {Width - MarginWidth, Height - MarginHeight},
    egd:line(Image, P0, P1, Color),
    egd:line(Image, P1, P2, Color),
    create_main_label(Image, GraphOpt),
    Image.

%%------------------------------------------------------------------------------
%% @doc Transform values to X and Y coordinates.
%% @end
%%------------------------------------------------------------------------------
-spec change_position([{atom(), [{number(), number()}]}], #wgraph_opts{}) -> 
		{[{atom(), [{number(), number()}]}], #wgraph_opts{}}.

change_position(Data, GraphOpt) ->
    MarginWidth = GraphOpt#wgraph_opts.margin_width,
    MarginHeight = GraphOpt#wgraph_opts.margin_height,
    LineWidth = GraphOpt#wgraph_opts.width - 2 * MarginWidth,
    LineHeight = GraphOpt#wgraph_opts.height - 2 * MarginHeight,
    {MaxW, MaxH} = find_maxs(Data),
    MinW = 0,
    MinH = 0,
    Len = round(math:pow(10, length(integer_to_list(floor(MaxH))) - 1)),
    EstimatedHeight = (floor(MaxH / Len) + 1) * Len,
    SW = scale_factor(LineWidth, MinW, MaxW),
    SH = scale_factor(LineHeight, MinH, EstimatedHeight),
    GridLines = calculate_grid_positions(Len, EstimatedHeight, MaxW),
    NewData = lists:map(fun ({Name, Points}) ->
				{Name,
				 lists:map(fun ({W, H}) ->
						   NewW = W * SW + MarginWidth,
						   NewH = mirroring(H * SH, LineHeight) + MarginHeight,
						   {trunc(NewW), trunc(NewH)}
					   end,
					   Points)}
			end,
			GridLines ++ Data),
    {NewData,
     GraphOpt#wgraph_opts{max_y = EstimatedHeight}}.

calculate_grid_positions(Len, EstimatedHeight, MaxW) ->
    MinW = 0,
    Grids = lists:zipwith(fun (A, B) -> [A, B] end,
			  grid_list(Len, EstimatedHeight, MinW),
			  grid_list(Len, EstimatedHeight, MaxW)),
    [{grid, lists:flatten(Grids)}].

-spec scale_factor(number(), number(), number()) -> number().

scale_factor(LineLength, Min, Max) ->
    case Max - Min of
      0 -> LineLength;
      _ -> LineLength / (Max - Min)
    end.

%%------------------------------------------------------------------------------
%% @doc Mirrors the Y axis, because egd(0,0) doesn't equal wgraph's data (0,0)
%%      See in doc/just_create.png
%% @end
%%------------------------------------------------------------------------------
-spec mirroring(number(), number()) -> number().

mirroring(Y, AY) ->
    SymmetryAxis = trunc(AY / 2),
    case Y < SymmetryAxis of
      true -> Y - 2 * (Y - SymmetryAxis);
      false -> Y + 2 * (SymmetryAxis - Y)
    end.

%%------------------------------------------------------------------------------
%% @doc Find the maximum values in Data.
%% @end
%%------------------------------------------------------------------------------
-spec find_maxs([{atom(), [{number(), number()}]}]) -> {number(), number()}.

find_maxs(Data) ->
    lists:foldl(fun ({_, Points}, {MW, MH}) ->
			lists:foldl(fun ({W, H}, {MaxW, MaxH}) ->
					    {max(W, MaxW), max(H, MaxH)}
				    end,
				    {MW, MH}, Points)
		end,
		{0, 0}, Data).

%%------------------------------------------------------------------------------
%% @doc It generates the X and Y coordinates for the girds.
%% @end
%%------------------------------------------------------------------------------
-spec grid_list(number(), number(), number()) -> [{number(), number()}].

grid_list(Len, EstimatedHeight, XValue) ->
    Ys = lists:seq(0, EstimatedHeight, Len),
    Xs = lists:duplicate(length(Ys), XValue),
    lists:zip(Xs, Ys).

%%------------------------------------------------------------------------------
%% @doc  Add graphs' lines and legend.
%% @end
%%------------------------------------------------------------------------------
-spec add_lines([{atom(), [point()]}], egd_image(), #wgraph_opts{}) -> 
		egd_image().

add_lines([{Name, Points}], Image, GraphOpt) ->
    {Color, NewGraphOpt} = color(GraphOpt),
    create_graph_label(Name, Image, Color, NewGraphOpt),
    create_line(Points, Image, Color),
    Image;
add_lines([{Name, Points} | Data], Image, GraphOpt) ->
    {Color, NewGraphOpt} = color(GraphOpt),
    create_graph_label(Name, Image, Color, NewGraphOpt),
    create_line(Points, Image, Color),
    add_lines(Data, Image, NewGraphOpt).

-spec create_line([point()], egd_image(), egd_color()) -> egd_image().

create_line([_], Image, _) -> Image;
create_line([P0, P1 | Points], Image, Color) ->
    egd:line(Image, P0, P1, Color),
    create_line([P1 | Points], Image, Color).

%%------------------------------------------------------------------------------
%% @doc It doesn't work with more than 6 labels.
%% @end
%%------------------------------------------------------------------------------
-spec create_graph_label([char()], pid(), egd_color(), #wgraph_opts{}) -> 
		egd_image().

create_graph_label(Name, Image, Color, GraphOpt) ->
    Font = load_font("Terminus22.wingsfont"),
    StringName = erlang:atom_to_list(Name),
    H = GraphOpt#wgraph_opts.height,
    MH = GraphOpt#wgraph_opts.margin_height,
    W = GraphOpt#wgraph_opts.width,
    N = GraphOpt#wgraph_opts.actual_line_number,
    X = trunc(W / 4) * ((N - 1) rem 3 + 1) - 100,
    Y = case N < 4 of
	  true -> H - trunc(MH / 2);
	  _ -> H - trunc(MH / 2) + 20
	end,
    P = {X, Y},
    P0 = {X - 5, Y + 12},
    P1 = {X - 10, Y + 17},
    egd:filledEllipse(Image, P0, P1, Color),
    egd:text(Image, P, Font, StringName, Color),
    Image.

%%------------------------------------------------------------------------------
%% @doc Create main label. See the point P in doc/points.png.
%% @end
%%------------------------------------------------------------------------------
-spec create_main_label(egd_image(), #wgraph_opts{}) -> egd_image().

create_main_label(Image, GraphOpt) ->
    P = {GraphOpt#wgraph_opts.width - 100, 15},
    Color = egd:color({58, 135, 189}),
    StringName = GraphOpt#wgraph_opts.main_label,
    Font = load_font("Helvetica20.wingsfont"),
    egd:text(Image, P, Font, StringName, Color),
    Image.

%%------------------------------------------------------------------------------
%% @doc It renders the image and saves it. Returns binary.
%% @end
%%------------------------------------------------------------------------------
-spec save(egd_image(), string()) -> binary().

save(Image, Filename) ->
    Png = egd:render(Image, png, [{render_engine, opaque}]),
    file:write_file(Filename, Png),
    egd:destroy(Image),
    Png.

%%------------------------------------------------------------------------------
%% @doc It chooses the color of the lines, it guarantees that all the lines
%%      (if you use just 4) have different color.
%% @end
%%------------------------------------------------------------------------------
-spec color(#wgraph_opts{}) -> {egd_color(), #wgraph_opts{}}.

color(GraphOpt) ->
    Number = GraphOpt#wgraph_opts.actual_line_number,
    Color = case Number of
	      0 -> {58, 135, 189};
	      1 -> {255, 127, 14};
	      2 -> {44, 160, 44};
	      _ -> green
	    end,
    {egd:color(Color), GraphOpt#wgraph_opts{actual_line_number = Number + 1}}.

load_font(Font) ->
    case
      erl_prim_loader:get_file(filename:join([code:priv_dir(wgraph),
					      "fonts", Font]))
	of
      {ok, FontBinary, _} -> egd_font:load_binary(FontBinary);
      _ ->
	  {ok, FontBinary, _} =
	      erl_prim_loader:get_file(filename:join([code:priv_dir(wgraph),
						      "wgraph/priv/fonts",
						      Font])),
	  egd_font:load_binary(FontBinary)
    end.

%%------------------------------------------------------------------------------
%% @doc Create the labels under the X axis. It uses the x coordinates from
%%      the longest datalist. You can see in doc/points.png
%% @end
%%------------------------------------------------------------------------------
create_x_labels(Data, Date, Image, GraphOpt) ->
    Points = element(2,
		     lists:max(lists:map(fun ({_, Datas}) ->
						 {length(Datas), Datas}
					 end,
					 Data))),
    LabelPoints = lists:map(fun ({X, _}) ->
				    {X - 15,
				     GraphOpt#wgraph_opts.height -
				       GraphOpt#wgraph_opts.margin_height}
			    			end,
			    Points),
    Color = egd:color(silver),
    Font = load_font("Helvetica20.wingsfont"),
    add_x_label(LabelPoints, Date, Image, Font, Color).

add_x_label([], _, Image, _, _) -> Image;
add_x_label(_, [], Image, _, _) -> Image;
add_x_label([P | LabelPoints], [StringName | Strings],
	    Image, Font, Color) ->
    egd:text(Image, P, Font, StringName, Color),
    add_x_label(LabelPoints, Strings, Image, Font, Color).

%%------------------------------------------------------------------------------
%% @doc Create the grid lines.
%% @end
%%------------------------------------------------------------------------------
-spec grid_lines(egd_image(), [{number(), number()}], #wgraph_opts{}) -> 
		egd_image().

grid_lines(Image, GridLines, GraphOpt) ->
    MaxY = GraphOpt#wgraph_opts.max_y,
    Len = length(integer_to_list(MaxY)),
    Unit = case MaxY == round(math:pow(10, Len - 1)) of
	     true -> round(math:pow(10, Len - 2));
	     _ -> round(math:pow(10, Len - 1))
	   end,
    Labels = case Unit of
	       0 -> lists:seq(0, MaxY);
	       _ -> lists:seq(0, MaxY, Unit)
	     end,
    Color = egd:color(silver),
    {_, GridPoints} = GridLines,
    Font = load_font("Helvetica20.wingsfont"),
    add_grid_lines(GridPoints, Image, Color, Labels, Font),
    Image.

%%------------------------------------------------------------------------------
%% @doc  I use the Label length because the PT is at the begining of the text.
%%       Just like the create_main_label/2, see in doc/points.png
%% @end
%%------------------------------------------------------------------------------
add_grid_lines(_, Image, _, [], _) -> Image;
add_grid_lines([], Image, _, _, _) -> Image;
add_grid_lines([{X, Y} = P0, P1 | GridPoints], Image,
	       Color, [Label | Labels], Font) ->
    StringLabel = integer_to_list(Label),
    PT = {X - length(StringLabel) * 10, Y - 15},
    egd:line(Image, P0, P1, Color),
    egd:text(Image, PT, Font, StringLabel, Color),
    add_grid_lines(GridPoints, Image, Color, Labels, Font).

%%------------------------------------------------------------------------------
%% @doc Add unit label. See doc/basic.png
%% @end
%%------------------------------------------------------------------------------
-spec add_unit_label([char()], egd_image(), #wgraph_opts{}) -> egd_image().

add_unit_label(Unit, Image, GraphOpt) ->
    MW = GraphOpt#wgraph_opts.margin_width,
    MH = GraphOpt#wgraph_opts.margin_height,
    Color = egd:color(silver),
    Font = load_font("Helvetica20.wingsfont"),
    P = {round(MW / 2), MH - 50},
    egd:text(Image, P, Font, Unit, Color),
    Image.

%%------------------------------------------------------------------------------
%% @doc It transforms a list of values to list of {n,values}, where n is a
%%      monotonically increasing non-negative integer.
%% @end
%%------------------------------------------------------------------------------
-spec add_parameter_x([{atom(), [number()]}],
		      [{atom(), [{number(), number()}]}]) -> [{atom(),
							       [{number(), number()}]}].

add_parameter_x([], NewList) -> NewList;
add_parameter_x([{Label, List} | Rest], ListWithX) ->
    MinList = case List of
		[] -> [0, 0];
		[One] -> [0, One];
		More -> More
	      end,
    DataWithX = lists:zip(lists:seq(0, length(MinList) - 1), MinList),
    NewListWithX = ListWithX ++ [{Label, DataWithX}],
    add_parameter_x(Rest, NewListWithX).
