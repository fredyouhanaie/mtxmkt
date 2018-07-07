-module(mtxmkt).

%% API exports
-export([mm_openread/1, mm_read_banner/1]).

%%====================================================================
%% Macros
%%====================================================================

-define(Blanks, " \t").
-define(MtxType, [array, coordinate]).
-define(DataType, [real, complex, pattern, integer]).
-define(SymmType, [general, symmetric, hermitian, 'skew-symmetric']).
-define(MatrixMarketBanner, "%%MatrixMarket ").


%%====================================================================
%% API functions
%%====================================================================


%%--------------------------------------------------------------------
%% @doc Open a matrix file for reading.
%%
%% @spec
%% mm_openread(filename()) -> io_device() | {error, atom(), string()}
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
%% @spec
%% mm_read_banner(io_device()) -> {atom(), atom(), atom(), atom()} | {error, atom(), string()}
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


%%====================================================================
%% Internal functions
%%====================================================================


%%--------------------------------------------------------------------
%% @doc Check and process the banner line.
%%
%% @spec
%% process_banner(string()) -> {atom(), atom(), atom(), atom()} | {error, atom(), string()}
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
%% @spec
%% process_banner_rest(list()) -> {atom(), atom(), atom(), atom()} | {error, tuple(), string()}
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
%% @spec
%% check_banner_mtxtype(atom(), atom(), atom()) -> ok | {error, atom(), string()}
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
%% @spec
%% check_banner_datatype(atom(), atom()) -> ok | {error, atom(), string()}
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
%% @spec
%% check_banner_symmetry(atom()) -> ok | {error, atom(), string()}
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
%% @spec
%% str2atoms(string()) -> list()
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
