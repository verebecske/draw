-module(egraph_test).
-include_lib("eunit/include/eunit.hrl").
-export([]).


date_maker_test() ->
	BD = {2019,1,1},
	ED = {2019,1,8},
	Dates = days({BD,ED}),
	D = [{2019,1,1},{2019,1,2},{2019,1,3},{2019,1,4},{2019,1,5},{2019,1,6},{2019,1,7},{2019,1,8}],
	?assertMatch(8, length(Dates)),
	?assertMatch(BD, hd(Dates)),
	?assertMatch(ED, hd(lists:reverse(Dates))),
	?assertMatch(D,Dates).

nextDay_test() -> 
	D0 = {2019,1,1},
	N0 = {2019,1,2},

	D1 = {2019,1,31},
	N1 = {2019,2,1},

	D2 = {2019,2,28},
	N2 = {2019,3,1},

	D3 = {2019,12,31},
	N3 = {2020,1,1},
	?assertMatch(N0, next_day(D0)),	
	?assertMatch(N1, next_day(D1)),
	?assertMatch(N2, next_day(D2)),
	?assertMatch(N3, next_day(D3)).

days({BeginDate,EndDate}) -> 
	days([BeginDate],EndDate).

days(Days, EndDate) -> 
	BeginDate = lists:last(Days),
	NextDay = next_day(BeginDate),
	Dates = Days ++ [NextDay],
	case NextDay of
		EndDate -> ?debugFmt("Yeey: ~p : ~p ~n", [NextDay,EndDate]), Dates;
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