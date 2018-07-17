%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2018, Fred Youhanaie
%%% @doc
%%% run the EUnit tests for the matrix module.
%%% @end
%%% Created : 16 Jul 2018 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(matrix_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% The tests
%%--------------------------------------------------------------------

% new matrix
new_test() ->
    M = matrix:new(2, 3, 42),
    ?assertMatch(2, maps:get(nrows, M)),
    ?assertMatch(3, maps:get(ncols, M)),
    ?assertMatch(42, maps:get(default, M)),
    Data = maps:get(data, M),
    Rows = array:to_list(Data),
    List = lists:map(fun (X) -> array:to_list(X) end, Rows),
    ?assertMatch([[42, 42, 42], [42, 42, 42]], List).

% get the default value
default_test() ->
    M = matrix:new(2, 3, 42),
    ?assertMatch(42, matrix:default(M)).

% get the matrix size
size_test() ->    
    M = matrix:new(2, 3, 42),
    ?assertMatch({2, 3}, matrix:size(M)).

% get a value without setting
get_test() ->
    M = matrix:new(2, 3, 42),
    ?assertMatch(42, matrix:get(1, 1, M)).

% set an get a non-dfault value
set_test() ->
    M0 = matrix:new(2, 3, 42),
    M1 = matrix:set(1, 1, 24, M0),
    ?assertMatch(24, matrix:get(1, 1, M1)).

% get_row
get_row_test() ->
    M = matrix:new(2, 3, 42),
    ?assertMatch([42, 42, 42], matrix:get_row_list(1, M)).

% set/get row
set_row_test() ->
    M0 = matrix:new(2, 3, 42),
    M1 = matrix:set_row_list(1, lists:seq(1,3), M0),
    ?assertMatch([1, 2, 3], matrix:get_row_list(1, M1)).

% to_list
to_list_test() ->
    M0 = matrix:new(2, 3, 42),
    M1 = matrix:set_row_list(1, lists:seq(1,3), M0),
    M2 = matrix:set_row_list(2, lists:seq(4,6), M1),
    ?assertMatch([[1, 2, 3], [4, 5, 6]], matrix:to_list(M2)).
