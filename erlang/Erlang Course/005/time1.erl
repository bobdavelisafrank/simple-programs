% The following is code made for the erlang.org course, under exercises (simple recursive programs). 

-module(time1).
-export([swedish_date/0, swedish/1]).

% Converts any basic date from YYYY-MM-DD format to YYMMDD.
swedish({YYYY, MM, DD}) ->
	integer_to_list(((YYYY rem 100)*10000) + (MM * 100) + DD).

% Function that gets the swedish date of the current date.
swedish_date() ->
	swedish(date()).