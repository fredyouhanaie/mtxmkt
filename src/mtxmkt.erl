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
-type mtxerror() :: {error, atom(), string()}.
-type mtxformat() :: array | coordinate.
-type mtxtype() :: real | complex | pattern | integer.
-type mtxsymm() :: general | symmetric | hermitian | 'skew-symmetric'.
-type mtxcode() :: {mtxformat(), mtxtype(), mtxsymm()}.
-type iodev() :: file:io_device().
-type complex() :: {float(), float()}.

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
    case read_mtx_size(IOdev, "~d ~d ~d") of
	Error = {error, _Reason, _Msg} ->
	    Error;
	{Nrows, Ncols, Nelems} ->
	    if
		Nrows*Ncols < Nelems ->
		    {error, mm_invalid_size, "Matrix element count is greater than its size."};
		true ->
		    {Nrows, Ncols, Nelems}
	    end
    end.

%%--------------------------------------------------------------------
%% @doc Get the size of the matrix in array format.
%%
%% The number of rows, columns of the matrix are returned as a tuple
%% of two integers.
%%
%% @end
%%--------------------------------------------------------------------
-spec mm_read_mtx_array_size(iodev()) -> {integer(), integer()} | mtxerror().
mm_read_mtx_array_size(IOdev) ->
    read_mtx_size(IOdev, "~d ~d").

%%--------------------------------------------------------------------
%% @doc Read an entire matrix market data file.
%%
%% The data, if read successfully, will be retured as a `matrix'.
%%
%% @end
%%--------------------------------------------------------------------
-spec mm_readfile(string()) -> {mtxcode(), matrix:matrix()} | mtxerror().
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
				     {Mtx_code, M}
			     end
		     end,
	    file:close(IOdev),
	    Result
    end.

%%--------------------------------------------------------------------
%% @doc Read and return the matrix data for a given
%% format/type/symmetry
%%
%% @end
%%--------------------------------------------------------------------
-spec mm_read_matrix_data(iodev(), mtxcode()) -> matrix:matrix() | mtxerror().
mm_read_matrix_data(IOdev, {Mtx_fmt, Data_type, Symm_type}) ->
    case read_matrix_size(IOdev, Mtx_fmt) of
	Error = {error, _Reason, _Msg} ->
	    Error;
	Size ->
	    process_matrix(IOdev, {Mtx_fmt, Data_type, Symm_type}, Size)
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
		{ok, [Nrows, Ncols], []}
		  when Nrows>0 andalso Ncols>0 ->
		    {Nrows, Ncols};
		{ok, [Nrows, Ncols, Nelems], []}
		  when Nrows>0 andalso Ncols>0 andalso Nelems>0 ->
		    {Nrows, Ncols, Nelems};
		_ ->
		    {error, mm_invalid_size, "Expected positive int/int or int/int/int"}
	    end;
	Error ->
	    Error
	end.

%%--------------------------------------------------------------------
%% @doc Read the `coordinate' data from the open file.
%%
%% `Nelems' lines of coordinates `integer' `integer' along with the
%% data values will be read.  The `Fmt' argument determines wheter we
%% should read just a pair of coordinates for pattern data, or single
%% or pairs of values.
%%
%% `Fmt' should be one of "~d ~d" (patterm), "~d ~d ~d" (integer), "~d
%% ~d ~f" (real) or "~d ~d ~f ~f" (complex).
%%
%% An initialized matrix with the correct dimensions and default value
%% should be provided. The data is returned in a `matrix'.
%%
%% The `matrix:set/4' function will check if the coordinates are
%% within the matrix dimensions.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_data_crd(iodev(), string(), integer(), matrix:matrix(), mtxsymm()) -> matrix:matrix()|mtxerror().
read_data_crd(_IOdev, _Fmt, 0, M, _Symm) ->
    M;
read_data_crd(IOdev, Fmt, Nelems, M, Symm) ->
    case read_data_entry(IOdev, Fmt) of
	Error = {error, _Reason, _Msg} ->
	    Error;
	[Row, Col|Entry] ->
	    Value = case Entry of
			[] ->		% pattern
			    1;
			[Scalar] ->	% integer or real
			    Scalar;
			[Real, Imag] ->	% complex number
			    {Real, Imag}
		    end,
	    case matrix:set(Row, Col, Value, M) of
		outofbounds ->
		    {error, mm_invalid_coord, "Invalid matrix coordinates"};
		M2 ->
		    case set_mat_symmetry(Row, Col, Value, M2, Symm) of
			Error = {error, _Reason, _Msg} ->
			    Error;
			M3 ->
			    read_data_crd(IOdev, Fmt, Nelems-1, M3, Symm)
		    end
	    end
    end.

%%--------------------------------------------------------------------
%% @doc set the symmtric value of a matrix, if appropriate.
%%
%% No values are set for diagonal values, or `general' matrices.
%%
%% For symmetric matrices, the matrix market file should only contain
%% elements below the main diagonal.
%%
%% @end
%%--------------------------------------------------------------------
-spec set_mat_symmetry(integer(), integer(), term(), matrix:matrix(), mtxsymm()) -> matrix:matrix() | mtxerror().
set_mat_symmetry(_Row, _Col, _Value, M, general) ->
    M;
set_mat_symmetry(Row, Col, _Value, M, _Symm) when Row == Col ->
    M;
set_mat_symmetry(Row, Col, Value, M, Symm) when Row > Col ->
    case Symm of
	symmetric ->
	    matrix:set(Col, Row, Value, M);
	hermitian ->
	    matrix:set(Col, Row, conjugate(Value), M)
    end;
set_mat_symmetry(_Row, _Col, _Value, _M, _Symm) ->
    {error, mm_invalid_symm, "Encountered value above diagonal"}.

%%--------------------------------------------------------------------
%% @doc Return the conjugate of a complex number.
%%
%% The complex number is represented as a tuple of two floats.
%%
%% @end
%%--------------------------------------------------------------------
-spec conjugate(complex()) -> complex().
conjugate({Real, Imag}) ->
    {Real, -Imag}.

%%--------------------------------------------------------------------
%% @doc Read and parse single line of data.
%%
%% The format `Fmt' should follow the `io' format, e.g. "~d" or "~d ~d ~f".
%%
%% @end
%%--------------------------------------------------------------------
-spec read_data_entry(iodev(), string()) -> list() | mtxerror().
read_data_entry(IOdev, Fmt) ->
    case read_next_line(IOdev) of
	Error = {error, _Reason, _Msg} ->
	    Error;
	Line when is_list(Line) ->
	    case io_lib:fread(Fmt, Line) of
		{ok, Data, []} ->
		    Data;
		_ ->
		    {error, mm_invalid_entry, "Invalid data entry."}
	    end
    end.

%%--------------------------------------------------------------------
%% @doc Read a column of data for the given number of rows.
%%
%% The column data is returned as a list of values, based on `Fmt'.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_data_col(iodev(), string(), integer(), [integer()] | [float()] | complex())
		   -> [integer()] | [float() | complex()] | mtxerror().
read_data_col(_IOdev, _Fmt, 0, Col_list) ->
    lists:reverse(Col_list);
read_data_col(IOdev, Fmt, Nelems, Col_list) ->
    case read_data_entry(IOdev, Fmt) of
	Error = {error, _Reason, _Msg} ->
	    Error;
	[Value] ->
	    read_data_col(IOdev, Fmt, Nelems-1, [Value|Col_list]);
	[Value1, Value2] ->	% complex numbers
	    read_data_col(IOdev, Fmt, Nelems-1, [{Value1, Value2}|Col_list])
    end.

%%--------------------------------------------------------------------
%% @doc Read the `array' `integer'/`real'/`complex' data from the open file.
%%
%% The data is returned as a matrix.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_data_array(iodev(), string(), integer(), integer(), integer(), matrix:matrix()) -> matrix:matrix() | mtxerror().
read_data_array(_IOdev, _Fmt, _Nrows, 0, _Col_num, M) ->
    M;
read_data_array(IOdev, Fmt, Nrows, Ncols, Col_num, M) ->
    Col_list = read_data_col(IOdev, Fmt, Nrows, []),
    M2 = matrix:set_col_list(Col_num, Col_list, M),
    read_data_array(IOdev, Fmt, Nrows, Ncols-1, Col_num+1, M2).

%%--------------------------------------------------------------------
%% @doc Read the `symmetric' `array' `integer'/`real'/`complex' data
%% from the open file.
%%
%% We read one column at a time, starting from column 1. The number
%% elements in each column decrease by 1 each time we move to the next
%% column.
%%
%% when we assign a list of values to a column, we also assign the
%% same list to the corresponding row. The list read from the file
%% only correspond to the elements below or to the right of the
%% diagonal.
%%
%% Since the `matrix' module only allows setting of a complete
%% row/column from a list, we need to read current contents of the
%% row/column up to the diagonal and
%%
%% @end
%%--------------------------------------------------------------------
-spec read_data_array_symm(iodev(), string(), integer(), integer(), integer(), matrix:matrix())
			  -> matrix:matrix() | mtxerror().
read_data_array_symm(_IOdev, _Fmt, 0, 0, _Col_num, M) ->
    M;
read_data_array_symm(IOdev, Fmt, Nrows, Ncols, Col_num, M) ->
    Col_list = read_data_col(IOdev, Fmt, Nrows, []),
    Col_pref = lists:sublist(matrix:get_col_list(Col_num, M), 1, Col_num-1),
    List = Col_pref++Col_list,
    M2 = matrix:set_col_list(Col_num, List, M),
    M3 = matrix:set_row_list(Col_num, List, M2),
    read_data_array_symm(IOdev, Fmt, Nrows-1, Ncols-1, Col_num+1, M3).

%%--------------------------------------------------------------------
%% @doc Read the matrix size for a given matrix format.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_matrix_size(iodev(), mtxformat()) -> {integer(), integer()} | {integer(), integer(), integer()} | mtxerror().
read_matrix_size(IOdev, coordinate) ->
    mm_read_mtx_crd_size(IOdev);
read_matrix_size(IOdev, array) ->
    mm_read_mtx_array_size(IOdev).

%%--------------------------------------------------------------------
%% @doc read the matrix data.
%%
%% @end
%%--------------------------------------------------------------------
-spec process_matrix(iodev(), mtxcode(), {integer(), integer(), integer()}) ->  matrix:mtrix()| mtxerror().
process_matrix(IOdev, {coordinate, Data_type, Symm_type}, {Nrows, Ncols, Nelems}) ->
    {Fmt, Mtx_default} = datatype2fmt(coordinate, Data_type),
    M = matrix:new(Nrows, Ncols, Mtx_default),
    read_data_crd(IOdev, Fmt, Nelems, M, Symm_type);

% below implementation is due to be revised, and will be simplified in
% due course.
process_matrix(IOdev, {array, Data_type, Symm_type}, {Nrows, Ncols}) ->
    {Fmt, Mtx_default} = datatype2fmt(array, Data_type),
    M = matrix:new(Nrows, Ncols, Mtx_default),
    case Symm_type of
	general ->
	    read_data_array(IOdev, Fmt, Nrows, Ncols, 1, M);
	symmetric ->
	    read_data_array_symm(IOdev, Fmt, Nrows, Ncols, 1, M);
	_ ->
	    {error, mm_notsupported, "Unsupported matrix type!"}
    end.

%%--------------------------------------------------------------------
%% @doc Given a matrix data type, return the fread format and the zero
%% term for sparse matrices.
%%
%% Array formats have one value per line, while in the coordinate
%% format the pair of matrix coordinates precedes the value. In
%% coordinate/pattern matrices we only have the coordinates.
%%
%% @end
%%--------------------------------------------------------------------
-spec datatype2fmt(mtxformat(), mtxtype()) -> {string(), integer()|float()|complex()}.
datatype2fmt(coordinate, pattern) ->
    {"~d ~d", 0};

datatype2fmt(coordinate, Type) ->
    {Format, Default} = datatype2fmt(array, Type),
    {"~d ~d " ++ Format, Default};

datatype2fmt(array, Type) ->
    case Type of
	integer ->
	    {"~d", 0};
	real ->
	    {"~f", 0.0};
	complex ->
	    {"~f ~f", {0.0, 0.0}}
    end.
