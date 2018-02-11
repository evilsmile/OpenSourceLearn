-module(tut5).

-export([format_temps/1, format_temp2/1]).

format_temps([]) ->
    ok;

%% When format_temps is called first time, 
%% City gets the first Value, and convert_to_celsius(First) is called
format_temps([City | Rest]) ->
    print_temp(convert_to_celsius(City)),
    format_temps(Rest).

convert_to_celsius({Name, {c, Temp}}) ->
    {Name, {c, Temp}};
convert_to_celsius({Name, {f, Temp}}) ->
    {Name, {c, (Temp - 32) * 5 / 9 }}.

print_temp({Name, {c, Temp}}) ->
    io:format("~-15w ~w c~n", [Name, Temp]).


%% version 2

format_temp2(List_of_cities) ->
    Converted_List = convert_list_to_c(List_of_cities),
    print_temp2(Converted_List).

%% convert if matched 'f' param
convert_list_to_c([{Name, {f, F}} | Rest]) ->
    Converted_city = {Name, {c, (F-32)*5/9}},
    [Converted_city | convert_list_to_c(Rest)];

convert_list_to_c([City | Rest]) ->
    %% if 'City' contains 'f', it'll call uppper function
    [City | convert_list_to_c(Rest)];

convert_list_to_c([]) ->
    [].

print_temp2([{Name, {c, Temp}} | Rest]) ->
    io:format("~-15w ~w c~n", [Name, Temp]),
    print_temp2(Rest);
print_temp2([]) ->
    ok.
