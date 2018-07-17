%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2018, Fred Youhanaie
%%% @doc
%%% A module for handling matrices.
%%%
%%% The array module is used internally to store the matrix data as an
%%% array of arrays. However, the internal representation may change
%%% in future and should not be relied upon.
%%%
%%% The matrix indices start at 1, unlike the array module, which
%%% starts at zero. This is to be in line with the way matrices are
%%% used in maths and engineering applications.
%%%
%%% This module has been written as an accompaniment to the mtxmkt
%%% module.
%%%
%%% @end
%%% Created : 11 Jul 2018 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(matrix).

-export([new/3, default/1, size/1, to_list/1]).
-export([get/3, set/4]).
-export([get_row_list/2, set_row_list/3]).

% we have our own size/1 function
-compile({no_auto_import, [size/1]}).

-type matrix() :: #{nrows => integer(), ncols => integer(), default => term(), data => array:array()}.

%%--------------------------------------------------------------------
%% @doc Create a new matrix.
%%
%% The matrix will have fixed size of <b>Nrows</b> rows and
%% <b>Ncols</b> columns.
%%
%% @end
%%--------------------------------------------------------------------
-spec new(integer(), integer(), term()) -> matrix().
new(Nrows, Ncols, Default) ->
    OneRow = array:new( [Ncols, {default, Default}] ),
    Data = array:new( [Nrows, {default, OneRow}] ),
    #{nrows => Nrows, ncols => Ncols, default => Default, data => Data}.

%%--------------------------------------------------------------------
%% @doc Return the default term for the missing values in the matrix.
%%
%% @end
%%--------------------------------------------------------------------
-spec default(matrix()) -> term().
default(M) ->
    maps:get(default, M).

%%--------------------------------------------------------------------
%% @doc Returns the number of rows and columns of the matrix.
%%
%% The size is returned as a tuple {Nrows, Ncols}.
%%
%% @end
%%--------------------------------------------------------------------
-spec size(matrix()) -> {integer(), integer()}.
size(M) ->
    {maps:get(nrows, M), maps:get(ncols, M)}.

%%--------------------------------------------------------------------
%% @doc Check the array bounds for a matrix.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_bounds(integer(), integer(), matrix()) -> ok | outofbounds.
check_bounds(Row, Col, M) ->
    case check_bounds_row(Row, M) of
	ok ->
	    check_bounds_col(Col, M);
	_ ->
	    outofbounds
    end.

-spec check_bounds_row(integer(), matrix()) -> ok | outofbounds.
check_bounds_row(Row, M) ->
    Nrows = maps:get(nrows, M),
    if
	Row < 1 orelse Row > Nrows ->
	    outofbounds;
	true ->
	    ok
    end.
    
-spec check_bounds_col(integer(), matrix()) -> ok | outofbounds.
check_bounds_col(Col, M) ->
    Ncols = maps:get(ncols, M),
    if
	Col < 1 orelse Col > Ncols ->
	    outofbounds;
	true ->
	    ok
    end.
    
%%--------------------------------------------------------------------
%% @doc The value at the given row and column is returned.
%%
%% The row and column indices start at 1.
%%
%% If either or both of row or column indices are outside the matrix
%% range, the atom outofbounds will be returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec get(integer(), integer(), matrix()) -> term() | outofbounds.
get(Row, Col, M) ->
    case check_bounds(Row, Col, M) of
	ok ->
	    array:get(Col-1, array:get(Row-1, maps:get(data, M)));
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @doc Set the value at the given row and column.
%%
%% The row and column indices start at 1.
%%
%% If either or both of row or column indices are out side the matrix
%% range, the atom outofbounds will be returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec set(integer(), integer(), term(), matrix()) -> matrix() | outofbounds.
set(Row, Col, Value, M) ->
    case check_bounds(Row, Col, M) of
	ok ->
	    RowData = array:set(Col-1, Value, array:get(Row-1, maps:get(data, M))),
	    MatData = array:set(Row-1, RowData, maps:get(data, M)),
	    maps:update(data, MatData, M);
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @doc Returns the contents of a row.
%%
%% The row index starts at 1.
%%
%% If the row index is outside the matrix range, the atom outofbounds
%% will be returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_row_list(integer(), matrix()) -> list() | outofbounds.
get_row_list(Row, M) ->
    case check_bounds_row(Row, M) of
	ok ->
	    array:to_list( array:get(Row-1, maps:get(data, M)) );
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @doc Sets the contents of a row from a list of values.
%%
%% If the list is longer than the number of columns, the extra
%% elements in the list will be ignored.
%%
%% If the list is shorter than the number of columns, the missing
%% elements will take the default value.
%%
%% @end
%%--------------------------------------------------------------------
-spec set_row_list(integer(), list(), matrix()) -> matrix() | outofbounds.
set_row_list(Row, List, M) ->
    case check_bounds_row(Row, M) of
	ok ->
	    Ncols = maps:get(ncols, M),
	    Row_array = array:from_list(lists:sublist(List, Ncols), maps:get(default, M)),
	    Data = array:set(Row-1, Row_array, maps:get(data, M)),
	    maps:update(data, Data, M);
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @doc Return the contents of the matrix as a list of lists.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_list(matrix()) -> [list()].
to_list(M) ->
    lists:map(fun (Row) -> array:to_list(Row) end,
	      array:to_list(maps:get(data, M))
	     ).
