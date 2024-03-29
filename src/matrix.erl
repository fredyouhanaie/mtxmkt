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

-export([new/3, default/1, size/1]).
-export([get/3, set/4]).
-export([get_row_list/2, set_row_list/3]).
-export([get_col_list/2, set_col_list/3]).
-export([to_list/1, from_list/2]).
-export([map_matrix/2, foldl_rows/3]).
-export([default_count/1]).

% we have our own size/1 function
-compile({no_auto_import, [size/1]}).

-opaque matrix() :: #{nrows => integer(), ncols => integer(), default => term(), data => array:array()}.
-export_type([matrix/0]).

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
%% @doc Return the values of a column as a list.
%%
%% The column number starts at 1.
%%
%% If the column number is outside the matrix range, the atom
%% outofbounds will be returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_col_list(integer(), matrix()) -> list() | outofbounds.
get_col_list(Col, M) ->
    case check_bounds_col(Col, M) of
        ok ->
            Col_array = array:map(
                          fun (_Rnum, Row) ->
                                  array:get(Col-1, Row)
                          end,
                          maps:get(data, M)
                         ),
            array:to_list(Col_array);
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Return a matrix with the given column set to the list.
%%
%% The column number starts at 1.
%%
%% If the column number is outside the matrix range, the atom
%% outofbounds will be returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec set_col_list(integer(), list(), matrix()) -> matrix() | outofbounds.
set_col_list(Col, List, M) ->
    case check_bounds_col(Col, M) of
        ok ->
            Data = array:map(
                     fun (Rnum, Row) ->
                             array:set(Col-1, lists:nth(Rnum+1, List), Row)
                     end,
                     maps:get(data, M)),
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

%%--------------------------------------------------------------------
%% @doc Return a matrix with its content replaced with the list of
%% lists.
%%
%% The List is in the form of a list of rows. If the outer list is
%% longer than the number of rows, or the internal lists are longer
%% than the number of columns, then the extra elements will be
%% ignored.
%%
%% for shorter lists, the missing values will take the default values
%% of the matrix M.
%%
%% An empty list, such as `[]' or `[[],[]]', has the same effect as
%% new/3, where the resulting matrix will have the same size and
%% default value as `M'.
%%
%% @end
%%--------------------------------------------------------------------
-spec from_list([list()], matrix()) -> matrix().
from_list(List, M) ->
    Nrows = maps:get(nrows, M),
    Ncols = maps:get(ncols, M),
    Default = maps:get(default, M),
    DefaultRow = array:new( [Ncols, {default, Default}] ),
    RowList = lists:map(
                fun (Row) ->
                        L = lists:sublist(Row, Ncols),    % remove extra cols in row
                        A = array:from_list(L, Default),  % create the array of cols
                        array:fix(array:resize(Ncols, A)) % ensure row is a fixed array
                end,
                lists:sublist(List, Nrows) % remove extra rows
               ),
    RowsArray = array:from_list(RowList, DefaultRow), % create array of rows
    Data = array:fix(array:resize(Nrows, RowsArray)), % ensure it's a fixed array
    maps:update(data, Data, M).

%%--------------------------------------------------------------------
%% @doc Apply a function against all elements of a matrix
%%
%% @end
%%--------------------------------------------------------------------
-spec map_matrix(function(), matrix:matrix()) -> matrix:matrix().
map_matrix(Fun, M) ->
    Mdata1 = maps:get(data, M),
    Mdata2 = array:map(
               fun (_Rnum, Row) ->
                       array:map(
                         fun (_Cnum, X) -> Fun(X) end,
                         Row)
               end,
               Mdata1
              ),
    maps:update(data, Mdata2, M).

%%--------------------------------------------------------------------
%% @doc Fold rows of a matrix, and return a list of terms.
%%
%% The same fold function will be applied to each row from left to
%% right. The result is a list of values corresponding to the folded
%% rows.
%%
%% @end
%%--------------------------------------------------------------------
-spec foldl_rows(function(), term(), matrix:matrix()) -> list().
foldl_rows(Fun, Acc0, M) ->
    Mdata = maps:get(data, M),
    lists:map(fun (Row) -> fold_one_row(Fun, Acc0, Row) end,
              array:to_list(Mdata)
             ).

%%--------------------------------------------------------------------
%% @doc Fold a single row (array) from left to right.
%%
%% @end
%%--------------------------------------------------------------------
-spec fold_one_row(function(), term(), array:array()) -> term().
fold_one_row(Fun, Acc0, Row) ->
    array:foldl(fun (_, AccIn, X) ->
                        Fun(X, AccIn)
                end,
                Acc0,
                Row).

%%--------------------------------------------------------------------
%% @doc Count the number of element with default value.
%%
%% @end
%%--------------------------------------------------------------------
-spec default_count(matrix:matrix()) -> integer().
default_count(M) ->
    Default = default(M),
    Count_default = fun (Acc, X) ->
                            if
                                X == Default -> Acc+1;
                                true -> Acc
                            end
                    end,
    lists:sum(foldl_rows(Count_default, 0, M)).
