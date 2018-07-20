%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2018, Fred Youhanaie
%%% @doc
%%% A module for handling Matrix Market files.
%%%
%%% The functions read and write the file contents into "matrix"
%%% types. See the accompanying matrix module for details.
%%%
%%% @end
%%% Created : 5 Jul 2018 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(mtxmkt).

%% API exports
-export([mm_openread/1, mm_read_banner/1]).
-export([mm_read_mtx_crd_size/1, mm_read_mtx_array_size/1]).
-export([mm_read_matrix_data/2]).
-export([mm_readfile/1]).

%%====================================================================
%% Macros
%%====================================================================

-define(Blanks, " \t").
-define(MtxType, [array, coordinate]).
-define(DataType, [real, complex, pattern, integer]).
-define(SymmType, [general, symmetric, hermitian, 'skew-symmetric']).
-define(MatrixMarketBanner, "%%MatrixMarket ").

%%====================================================================
%% Types
%%====================================================================
-type mtxerror() ::{error, atom(), string()}.
-type mtxtype() :: {
		    array | coordinate,
		    real | complex | pattern | integer,
		    general | symmetric | hermitian | 'skew-symmetric'
		   }.

%%====================================================================
%% API functions
%%====================================================================


%%--------------------------------------------------------------------
%% @doc Open a matrix file for reading.
%%
%% @end
%%--------------------------------------------------------------------
-spec mm_openread(string()) -> pid() | {error, atom(), string()}.
mm_openread(Filename) ->
    case file:open(Filename, [read]) of
	{ok, IOdev} ->
	    IOdev;
	{error, Reason} ->
	    {error, Reason, "Could not open the file"}
    end.

%%--------------------------------------------------------------------
%% @doc Read and process the matrix file header/banner
%%
%% For this to succeed the file pointer should be at the very start of
%% the file.
%%
%% If successful, returns {ok, Banner}, where Banner is a quadruple of
%% the fields of the banner line.
%%
%% @end
%% --------------------------------------------------------------------
-spec mm_read_banner(pid()) -> {atom(), atom(), atom(), atom()} | {error, atom(), string()}.
mm_read_banner(IOdev) ->
    case file:read_line(IOdev) of
	{ok, Banner} ->
	    process_banner(Banner);
	eof ->
	    {error, eof, "Premature end of file"};
	{error, Reason} ->
	    {error, Reason, "Could not read from file"}
    end.

%%--------------------------------------------------------------------
%% @doc Get the size of the matrix in coordinate format.
%%
%% The number of rows, columns and total elements in the file are
%% returned as a tuple of three integers.
%%
%% @end
%%--------------------------------------------------------------------
-spec mm_read_mtx_crd_size(pid()) -> {integer(), integer(), integer()} | mtxerror().
mm_read_mtx_crd_size(IOdev) ->
    case read_ints(IOdev) of
	[Rows, Cols, Elems] ->
	    {Rows, Cols, Elems};
	Ints when is_list(Ints) ->
	    {error, mm_invalid_line, "Expected three integers"};
	{error, Reason, Msg} ->
	    {error, Reason, Msg}
    end.

%%--------------------------------------------------------------------
%% @doc Get the size of the matrix in array format.
%%
%% The number of rows, columns of the matrix are returned as a tuple
%% of two integers.
%%
%% @end
%%--------------------------------------------------------------------
-spec mm_read_mtx_array_size(pid()) -> {integer(), integer()}.
mm_read_mtx_array_size(IOdev) ->
    case read_ints(IOdev) of
	[Rows, Cols] ->
	    {Rows, Cols};
	Ints when is_list(Ints) ->
	    {error, mm_invalid_line, "Expected two integers"};
	{error, Reason, Msg} ->
	    {error, Reason, Msg}
    end.

%%====================================================================
%% Internal functions
%%====================================================================


%%--------------------------------------------------------------------
%% @doc Check and process the banner line.
%%
%% @end
%%--------------------------------------------------------------------
-spec process_banner(string()) -> {atom(), atom(), atom(), atom()} | {error, atom(), string()}.
process_banner(Banner) ->
    Rest = string:prefix(Banner, ?MatrixMarketBanner),
    case Rest of
	nomatch ->
	    {error, mm_no_header, "Not a valid Matrix Market file"};
	_ ->
	    Fields = str2atoms(string:trim(Rest)),
	    case length(Fields) of
		4 ->
		    process_banner_rest(Fields);
		_ ->
		    {error, mm_invalid_header, "Invalid banner line"}
	    end
    end.

%%--------------------------------------------------------------------
%% @doc Check the quadruple fields on the banner line.
%%
%% @end
%%--------------------------------------------------------------------
-spec process_banner_rest(list()) -> {atom(), atom(), atom(), atom()} | {error, tuple(), string()}.
process_banner_rest([Obj, Fmt, Type, Symm]) ->
    case Obj of
	matrix ->
	    case check_banner_mtxtype(Fmt, Type, Symm) of
		ok ->
		    {Obj, Fmt, Type, Symm};
		Error ->
		    Error
	    end;
	_ ->
	    {error, mm_unsupported_type, "Unsupported object type, must be matrix"}
    end.

%%--------------------------------------------------------------------
%% @doc Check the second field, format, of the banner.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_banner_mtxtype(atom(), atom(), atom()) -> ok | {error, atom(), string()}.
check_banner_mtxtype(Fmt, Type, Symm) ->
    case lists:member(Fmt, ?MtxType) of
	true ->
	    check_banner_datatype(Type, Symm);
	_ ->
	    {error, mm_unsupported_type, "Unsupported matrix type, not array/coordinate"}
    end.

%%--------------------------------------------------------------------
%% @doc Check the third field, data type, of the banner.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_banner_datatype(atom(), atom()) -> ok | {error, atom(), string()}.
check_banner_datatype(Type, Symm) ->
    case lists:member(Type, ?DataType) of
	true ->
	    check_banner_symmetry(Symm);
	_ ->
	    {error, mm_unsupported_type, "Unsupported matrix data type"}
    end.

%%--------------------------------------------------------------------
%% @doc Check the fourth field, symmetry, of the banner line.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_banner_symmetry(atom()) -> ok | {error, atom(), string()}.
check_banner_symmetry(Symm) ->
    case lists:member(Symm, ?SymmType) of
	true ->
	    ok;
	_ ->
	    {error, mm_unsupported_type, "Unsupported matrix symmetry type"}
    end.

%%--------------------------------------------------------------------
%% @doc Return the blank separated tokens in a string as a list of atoms.
%%
%% @end
%%--------------------------------------------------------------------
-spec str2atoms(string()) -> list().
str2atoms (Line) ->
    StrList = string:lexemes(Line, ?Blanks),
    lists:map(
      fun (S) ->
	      erlang:list_to_atom(string:lowercase(S))
      end,
      StrList).

%%--------------------------------------------------------------------
%% @doc Return the blank separated tokens in a string as a list of integers.
%%
%% @end
%%--------------------------------------------------------------------
-spec str2ints(string()) -> [integer() | atom()].
str2ints(Line) ->
    StrList = string:lexemes(Line, ?Blanks),
    lists:map(fun (S) ->
		      case string:to_integer(S) of
			  {I, []} -> I;
			  {error, Reason} -> Reason
		      end
	      end,
	      StrList).

%%--------------------------------------------------------------------
%% @doc return the next non-blank line
%%
%% Empty lines and lines with white space are considered as blank.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_next_line(pid()) -> string() | {error, atom(), string()}.
read_next_line(IOdev) ->
    case file:read_line(IOdev) of
	{ok, Line} ->
	    case string:is_empty(string:trim(Line)) of
		true ->
		    read_next_line(IOdev);
		_ ->
		    string:chomp(Line)
	    end;
	eof ->
	    {error, eof, "Error reading from file"};
	{error, Reason} ->
	    {error, Reason, "Error reading from file"}
    end.

%%--------------------------------------------------------------------
%% @doc return the next non-comment/non-blank line
%%
%% Comments are lines with "%" in the first column.
%%
%% @end
%%--------------------------------------------------------------------
-spec skip_comments(pid()) -> string() | {error, atom(), string()}.
skip_comments(IOdev) ->
    case read_next_line(IOdev) of
	{error, Reason, Msg} ->
	    {error, Reason, Msg};
	Line ->
	    case erlang:hd(Line) of
		$% ->
		    skip_comments(IOdev);
		_ ->
		    Line
	    end
    end.


%%--------------------------------------------------------------------
%% @doc Read a line of integers, return them as a list.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_ints(pid()) -> [integer()] | {error, atom(), string()}.
read_ints(IOdev) ->
    case skip_comments(IOdev) of
	{error, Reason, Msg} ->
	    {error, Reason, Msg};
	Line ->
	    Ints = str2ints(Line),
	    case all_int(Ints) of
		true -> Ints;
		false -> {error, mm_invalid_line, "Expected list of integers"}
	    end
    end.

%%--------------------------------------------------------------------
%% @doc test if a list has all integers.
%%
%% @end
%%--------------------------------------------------------------------
-spec all_int(list()) -> boolean().
all_int(Ints) when is_list(Ints) ->
    lists:all(fun (I) -> is_integer(I) end, Ints).

%%--------------------------------------------------------------------
%% @doc Read an return the matrix data for a given
%% format/type/symmetry
%%
%% @end
%%--------------------------------------------------------------------
-spec mm_read_matrix_data(pid(), mtxtype()) -> matrix:matrix() | mtxerror().


mm_read_matrix_data(IOdev, {coordinate, pattern, general}) ->
    case mm_read_mtx_crd_size(IOdev) of
	Error = {error, _Reason, _Msg} ->
	    Error;
	{Nrows, Ncols, Nelems} ->
	    io:format("new matrix ~p x ~p, with ~p elements.~n", [Nrows, Ncols, Nelems]),
	    M = matrix:new(Nrows, Ncols, 0),
	    read_data_crd_pattern(IOdev, Nelems, M)
    end;

mm_read_matrix_data(_IOdev, _Banner) ->
    {error, mm_notsupported, "Unsupported matrix type!"}.

%%--------------------------------------------------------------------
%% @doc Read the `coordinate' `pattern' data from the open file.
%%
%% An initialized matrix with the correct dimentsion should be
%% provided. The data is returned in a `matrix'.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_data_crd_pattern(pid(), integer(), matrix:matrix()) -> matrix:matrix() | mtxerror().
read_data_crd_pattern(_IOdev, 0, M) ->
    M;
read_data_crd_pattern(IOdev, Nelems, M) ->
    [Row, Col] = read_ints(IOdev),
    read_data_crd_pattern(IOdev, Nelems-1, matrix:set(Row, Col, 1, M)).

%%--------------------------------------------------------------------
%% @doc Read an entire matrix market data file.
%%
%% The data, if read successfully, will be retured as a `matrix'.
%%
%% @end
%%--------------------------------------------------------------------
-spec mm_readfile(string()) -> matrix:matrix() | mtxerror().
mm_readfile(Filename) ->
    case  mm_openread(Filename) of
	Error = {error, _Reason, _Msg} ->
	    Error;
	IOdev ->
	    Banner = {matrix, Fmt, Type, Symm} = mm_read_banner(IOdev),
	    io:format("Banner=~p.~n", [Banner]),
	    M = mm_read_matrix_data(IOdev, {Fmt, Type, Symm}),
	    file:close(IOdev),
	    M
    end.
