-module(egraph). %sorry for my english
-export([wombat_graph/1,graph/2,days/1,next_day/1]).

-record(opts, {width,height,numberOfLine,margowidth,margoheight,date,dateValue,maxValue}).

wombat_graph(Data) ->
	graph(Data,[]).

graph(Data,Opt) -> 
	GraphOpt = create_options_record(Opt),
	Image = create(GraphOpt),
	{NewData,NewGraphOpt} = change_position(Data,GraphOpt),
	make_silver_lines(Image, NewGraphOpt),
	add_lines(NewData,Image,NewGraphOpt),
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
create_options_record([],GraphOpt) -> made_margin(GraphOpt);
create_options_record([{Label, Value} | Opts],GraphOpt) ->
	NewGraphOpt = case Label of
		width -> GraphOpt#opts{width = Value};
		height -> GraphOpt#opts{height = Value};
		date -> add_date(Value,GraphOpt);
		_ -> GraphOpt
	end,
	create_options_record(Opts,NewGraphOpt).

add_date(Value, GraphOpt) -> 
	GraphOpt#opts{date = true, dateValue = Value}.

made_margin(GraphOpt) ->
	Width = GraphOpt#opts.width,
	%Height = GraphOpt#opts.height,
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
	Png = egd:render(Image, png, [{render_engine, opaque}]),
	FileName = "wgraph.png",
	egd:save(Png, FileName),
    egd:destroy(Image),
    Png.

%this part changes the data's positions
change_position(Data,GraphOpt = #opts{date = true}) ->
	NewData = lists:map(
			fun({Name,Points}) ->
				{NewPoints,_} = lists:mapfoldl(
					fun({_,Y},NX) ->
						{ {NX,Y}, NX + 45 } 
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
	NewData = lists:map(
		fun({Name,Points}) -> 
			{Name, lists:map(
					fun({W,H}) ->
						{(W * SW) + MW,mirroring((H * SH),LH)+ MH}
					end, Points)}
		end, Data),
	{NewData,GraphOpt#opts{maxValue = MaxH}}.

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
	LabelPoints = lists:map(fun({X,_}) ->
		{X - 15, (GraphOpt#opts.height - GraphOpt#opts.margoheight)}
	end,Points),
	{BeginDate,EndDate} = GraphOpt#opts.dateValue,
	Dates = days({BeginDate,EndDate}) ++ [BeginDate],
	NewLabelPoints = LabelPoints ++ [{GraphOpt#opts.width - 100, 15}],
	make_day_label(NewLabelPoints,Dates,Image),
	Image.

make_day_label([WP],[{Y,_,_} | _ ],Image) -> 
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

make_day_label([P | LabelPoints],[{_,M,D} | Dates],Image) ->
	Color = egd:color(silver),
	StringName = integer_to_list(D) ++ "/ " ++ integer_to_list(M),
	io:format("Hello : ~p Dates : ~p ~n", [StringName,Dates]),
	Font = load_font("Helvetica20.wingsfont"),
	egd:text(Image, P, Font, StringName, Color),
	make_day_label(LabelPoints,Dates,Image);

%this guarantee that it don't crash when the days number is too few or too mutch
make_day_label(_,_,Image) ->
	Image.

-spec days({calerdar:date(),calendar:date()}) -> [calendar:date()].
days({BeginDate,EndDate}) -> 
	days([BeginDate],EndDate).

days(Days, EndDate) -> 
	BeginDate = lists:last(Days),
	NextDay = next_day(BeginDate),
	Dates = Days ++ [NextDay],
	case NextDay of
		EndDate -> Dates;
		_ -> days(Dates,EndDate)
	end.

next_day({Y,M,D}) -> 
	NextDay = 
		case calendar:valid_date({Y,M,D+1}) of 
			true -> {Y,M,D+1};
			false -> case calendar:valid_date({Y,M+1,1}) of 
						true -> {Y,M+1,1};
						false -> {Y+1,1,1}
					end
		end,
	NextDay.
 
make_silver_lines(Image,GraphOps) -> 
	MaxY = GraphOps#opts.maxValue,
	MW = GraphOps#opts.margowidth,
	W = GraphOps#opts.width,
	H = GraphOps#opts.height,
	MH = GraphOps#opts.margoheight,
	Color = egd:color({211,211,211,1}),
	Font = load_font("Helvetica20.wingsfont"),
	Len = round(math:pow(10,length(integer_to_list(MaxY))-1)),
%	Estimation = round(MaxY / Len) * Len,
	P = lists:seq(1,length(integer_to_list(MaxY)) + 1),
	lists:mapfoldl(
		fun(_,{L, Ihm}) -> 
			P0 = {MW,L - MH},
			P1 = {W-MW, L - MH},
			StringName = integer_to_list(Ihm),
			egd:text(Image, {MW - 45,L - MH - 15}, Font, StringName, Color),
			egd:line(Image,P0,P1,Color),
			{ { P0 , P1 } ,{L-Len,Ihm + Len}}
		end, {H,0}, P),
	ok.

	
