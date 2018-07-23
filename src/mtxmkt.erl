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
-type mtxformat() :: array | coordinate.
-type mtxtype() :: real | complex | pattern | integer.
-type mtxsymm() :: general | symmetric | hermitian | 'skew-symmetric'.
-type mtxcode() :: {mtxformat(), mtxtype(), mtxsymm()}.
-type complex() :: {float(), float()}.
-type iodev() :: file:io_device().


%%====================================================================
%% API functions
%%====================================================================


%%--------------------------------------------------------------------
%% @doc Open a matrix file for reading.
%%
%% @end
%%--------------------------------------------------------------------
-spec mm_openread(string()) -> iodev()| mtxerror().
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
-spec mm_read_banner(iodev()) -> {atom(), atom(), atom(), atom()} | mtxerror().
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
-spec mm_read_mtx_crd_size(iodev()) -> {integer(), integer(), integer()} | mtxerror().
mm_read_mtx_crd_size(IOdev) ->
    read_mtx_size(IOdev, "~d ~d ~d").

%%--------------------------------------------------------------------
%% @doc Get the size of the matrix in array format.
%%
%% The number of rows, columns of the matrix are returned as a tuple
%% of two integers.
%%
%% @end
%%--------------------------------------------------------------------
-spec mm_read_mtx_array_size(iodev()) -> {integer(), integer()}.
mm_read_mtx_array_size(IOdev) ->
    read_mtx_size(IOdev, "~d ~d").

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
	    Result = case mm_read_banner(IOdev) of
			 Error = {error, _Reason, _Msg} ->
			     Error;
			 Mtx_code ->
			     case mm_read_matrix_data(IOdev, Mtx_code) of
				 Error = {error, _Reason, _Msg} ->
				     Error;
				 M ->
				     M
			     end
		     end,
	    file:close(IOdev),
	    Result
    end.

%%--------------------------------------------------------------------
%% @doc Read an return the matrix data for a given
%% format/type/symmetry
%%
%% @end
%%--------------------------------------------------------------------
-spec mm_read_matrix_data(iodev(), mtxcode()) -> matrix:matrix() | mtxerror().
mm_read_matrix_data(IOdev, {coordinate, pattern, general}) ->
    case mm_read_mtx_crd_size(IOdev) of
	Error = {error, _Reason, _Msg} ->
	    Error;
	{Nrows, Ncols, Nelems} ->
	    M = matrix:new(Nrows, Ncols, 0),
	    read_data_crd_pattern(IOdev, Nelems, M)
    end;

mm_read_matrix_data(IOdev, {coordinate, integer, general}) ->
    case mm_read_mtx_crd_size(IOdev) of
	Error = {error, _Reason, _Msg} ->
	    Error;
	{Nrows, Ncols, Nelems} ->
	    M = matrix:new(Nrows, Ncols, 0),
	    read_data_crd_integer(IOdev, Nelems, M)
    end;

mm_read_matrix_data(IOdev, {coordinate, real, general}) ->
    case mm_read_mtx_crd_size(IOdev) of
	Error = {error, _Reason, _Msg} ->
	    Error;
	{Nrows, Ncols, Nelems} ->
	    M = matrix:new(Nrows, Ncols, 0.0),
	    read_data_crd_real(IOdev, Nelems, M)
    end;

mm_read_matrix_data(IOdev, {coordinate, complex, general}) ->
    case mm_read_mtx_crd_size(IOdev) of
	Error = {error, _Reason, _Msg} ->
	    Error;
	{Nrows, Ncols, Nelems} ->
	    M = matrix:new(Nrows, Ncols, {0.0, 0.0}),
	    read_data_crd_complex(IOdev, Nelems, M)
    end;

mm_read_matrix_data(_IOdev, _Banner) ->
    {error, mm_notsupported, "Unsupported matrix type!"}.

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
-spec process_banner_rest(list()) -> mtxcode() | {error, tuple(), string()}.
process_banner_rest([Obj, Fmt, Type, Symm]) ->
    case Obj of
	matrix ->
	    case check_banner_mtxtype(Fmt, Type, Symm) of
		ok ->
		    {Fmt, Type, Symm};
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
-spec check_banner_mtxtype(atom(), atom(), atom()) -> ok | mtxerror().
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
-spec check_banner_datatype(atom(), atom()) -> ok | mtxerror().
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
-spec check_banner_symmetry(atom()) -> ok | mtxerror().
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
%% @doc return the next non-blank line
%%
%% Empty lines and lines with white space are considered as blank.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_next_line(iodev()) -> string() | mtxerror().
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
-spec skip_comments(iodev()) -> string() | mtxerror().
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
%% @doc Read matrix sizes
%%
%% `Line_fmt' should be either "~d ~d" or "~d ~d ~d".
%%
%% @end
%%--------------------------------------------------------------------
-spec read_mtx_size(iodev(), string()) -> {integer(), integer()} | {integer(), integer(), integer()} | mtxerror().
read_mtx_size(IOdev, Line_fmt) ->
    case skip_comments(IOdev) of
	Line when is_list(Line) ->
	    case io_lib:fread(Line_fmt, Line) of
		{ok, [Row, Col], []} ->
		    {Row, Col};
		{ok, [Row, Col, Nelems], []} ->
		    {Row, Col, Nelems};
		_ ->
		    {error, mm_invalid_entry, "Expected int/int or int/int/int"}
	    end;
	Error ->
	    Error
	end.

%%--------------------------------------------------------------------
%% @doc Read a line of coordinates (for pattern type) from the open file.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_crd_patt(iodev()) -> {integer(), integer()} | mtxerror().
read_crd_patt(IOdev) ->
    case read_next_line(IOdev) of
	Line when is_list(Line) ->
	    case io_lib:fread("~d ~d", Line) of
		{ok, [Row, Col], []} ->
		    {Row, Col};
		_ ->
		    {error, mm_invalid_entry, "Expected int/int"}
	    end;
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @doc Read a line of coordinates and integer value from the open
%% file.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_crd_int(iodev()) -> {integer(), integer(), integer()} | mtxerror().
read_crd_int(IOdev) ->
    case read_next_line(IOdev) of
	Line when is_list(Line) ->
	    case io_lib:fread("~d ~d ~d", Line) of
		{ok, [Row, Col, Value], []} ->
		    {Row, Col, Value};
		_ ->
		    {error, mm_invalid_entry, "Expected int/int/int"}
	    end;
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @doc Read a line of coordinates and float value from the open file.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_crd_float(iodev()) -> {integer(), integer(), float()} | mtxerror().
read_crd_float(IOdev) ->
    case read_next_line(IOdev) of
	Line when is_list(Line) ->
	    case io_lib:fread("~d ~d ~f", Line) of
		{ok, [Row, Col, Value], []} ->
		    {Row, Col, Value};
		_ ->
		    {error, mm_invalid_entry, "Expected int/int/float"}
	    end;
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @doc Read a line of coordinates and complex value from the open
%% file.
%%
%% The complex value is stored as a tuple of two floats.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_crd_complex(iodev()) -> {integer(), integer(), complex()} | mtxerror().
read_crd_complex(IOdev) ->
    case read_next_line(IOdev) of
	Line when is_list(Line) ->
	    case io_lib:fread("~d ~d ~f ~f", Line) of
		{ok, [Row, Col, Real, Imag], []} ->
		    {Row, Col, {Real, Imag}};
		_ ->
		    {error, mm_invalid_entry, "Expected int/int/float/float"}
	    end;
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @doc Read the `coordinate' `pattern' data from the open file.
%%
%% An initialized matrix with the correct dimentsion should be
%% provided. The data is returned in a `matrix'.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_data_crd_pattern(iodev(), integer(), matrix:matrix()) -> matrix:matrix() | mtxerror().
read_data_crd_pattern(_IOdev, 0, M) ->
    M;
read_data_crd_pattern(IOdev, Nelems, M) ->
    case read_crd_patt(IOdev) of
	{Row, Col} ->
	    read_data_crd_pattern(IOdev, Nelems-1, matrix:set(Row, Col, 1, M));
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @doc Read the `coordinate' `integer' data from the open file.
%%
%% An initialized matrix with the correct dimentsion should be
%% provided. The data is returned in a `matrix'.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_data_crd_integer(iodev(), integer(), matrix:matrix()) -> matrix:matrix() | mtxerror().
read_data_crd_integer(_IOdev, 0, M) ->
    M;
read_data_crd_integer(IOdev, Nelems, M) ->
    case read_crd_int(IOdev) of
	Error = {error, _Reason, _Msg} ->
	    Error;
	{Row, Col, Value} ->
	    read_data_crd_integer(IOdev, Nelems-1, matrix:set(Row, Col, Value, M))
    end.

%%--------------------------------------------------------------------
%% @doc Read the `coordinate' `real' data from the open file.
%%
%% `Nelems' lines of `integer' `integer' `float' will be read from the
%% open file. The data is returned in a `matrix'.
%%
%% An initialized matrix with the correct dimension should be
%% provided.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_data_crd_real(iodev(), integer(), matrix:matrix()) -> matrix:matrix() | mtxerror().
read_data_crd_real(_IOdev, 0, M) ->
    M;
read_data_crd_real(IOdev, Nelems, M) ->
    case read_crd_float(IOdev) of
	Error = {error, _Reason, _Msg} ->
	    Error;
	{Row, Col, Value} ->
	    read_data_crd_real(IOdev, Nelems-1, matrix:set(Row, Col, Value, M))
    end.

%%--------------------------------------------------------------------
%% @doc Read the `coordinate' `complex' data from the open file.
%%
%% `Nelems' lines of `integer' `integer' `float' `float' will be read
%% from the open file. The data is returned in a `matrix'.
%%
%% Internally the complex value is stored as a tuple of two floats.
%%
%% An initialized matrix with the correct dimension should be
%% provided.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_data_crd_complex(iodev(), integer(), matrix:matrix()) -> matrix:matrix() | mtxerror().
read_data_crd_complex(_IOdev, 0, M) ->
    M;
read_data_crd_complex(IOdev, Nelems, M) ->
    case read_crd_complex(IOdev) of
	Error = {error, _Reason, _Msg} ->
	    Error;
	{Row, Col, Value} ->
	    read_data_crd_complex(IOdev, Nelems-1, matrix:set(Row, Col, Value, M))
    end.
