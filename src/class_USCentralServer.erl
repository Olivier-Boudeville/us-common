% Copyright (C) 2025-2025 Olivier Boudeville
%
% This file belongs to the US-Main project, a part of the Universal Server
% framework.
%
% This program is free software: you can redistribute it and/or modify it under
% the terms of the GNU Affero General Public License as published by the Free
% Software Foundation, either version 3 of the License, or (at your option) any
% later version.
%
% This program is distributed in the hope that it will be useful, but WITHOUT
% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
% FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
% details.
%
% You should have received a copy of the GNU Affero General Public License along
% with this program. If not, see <http://www.gnu.org/licenses/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Wednesday, July 30, 2025.

-module(class_USCentralServer).

-moduledoc """
Mother class for the **front entry point** of an actual US application.

Allows notably to centralise the management of naming registration, paths,
configuration and automated actions, for all the servers that this application
comprises.
""".


-define( class_description,
		 "Mother class for the **front entry point** of an actual US "
         "application." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_USServer ] ).

-type central_server_pid() :: server_pid().


-export_type([ central_server_pid/0 ]).


% For us_common_actions_key:
-include("class_USServer.hrl").



% Implementation notes:
%
% A US central server is responsible for the parsing and sharing of all US
% configuration information, and for the management of the actions supported by
% this US application (named 'xxx' in examples).


% The class-specific attributes:
%
% (now, for a better robustness, servers are resolved on the fly, their PIDs are
% not to be stored anymore)
%
-define( class_attributes, [

    { app_short_name, app_short_name(), "the short name of this US application "
      "(for example `xxx`, as a binary string)" },

	{ execution_context, option( execution_context() ),
	  "tells whether this server is to run in development or production mode" },

	% As it impacts at least various paths:
	{ app_run_context, application_run_context(),
	  "tells how the US application is run, natively (using the Ceylan "
      "build/run system) or as an OTP release" },

	{ config_base_directory, option( bin_directory_path() ),
	  "the base directory where all US configuration is to be found "
	  "(not the us_xxx/priv/conf internal directory)" },

	{ app_base_directory, option( bin_directory_path() ),
	  "the base directory of the US application (the root where src, priv, "
      "ebin, etc. can be found)" },

	{ conf_directory, option( bin_directory_path() ),
	  "the US internal configuration directory, 'us_xxx/priv/conf'" },

	{ data_directory, option( bin_directory_path() ),
	  "the directory where the US-xxx working data is to be stored "
	  "(typically [...]/us_xxx/priv/data)" },

	{ log_directory, option( bin_directory_path() ),
      "the directory where (non-VM) US-xxx logs shall be written, "
      "notably traces" },

    { waited_operations, option( count() ),
      "the number of any ongoing asynchronous operations that are currently "
      "still taking place (typically notifications that are waited for); used "
      "so that this instance remains a (never-blocking) server, to avoid the "
      "deadlocks bound to happen with the other US-Main servers if it sent "
      "(blocking) requests to them" }

    % (action_table and all inherited for class_USServer)

                           ] ).


% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.Central" ).


% Exported helpers:
-export([ get_execution_target/0 ]).


-doc """
The short name of an US application.

It is `<<"xxx">>` in the examples, and in practice could be `<<"main">>`,
`<<"web">>`, etc.

See also `app_name/O` (and note the different type).
""".
-type app_short_name() :: bin_string().


-doc """
The name of an US application; to be prefixed with `us_`.

So it is `"us_xxx"` in the examples, and in practice this could be `"us_main"`,
`"us_web"`, etc.

In our conventions:
- any corresponding project would be named `us-xxx` (note the dash)
- the clone of such a project would be created as `us_xxx` (rather than
`us-xxx`, as obtained for example with a CI)
""".
-type app_name() :: ustring().


-export_type([ app_short_name/0, app_name/0 ]).



% Note: include order matters.

% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").

% To define get_execution_target/0:
-include_lib("myriad/include/utils/basic_utils.hrl").



% Type shorthands:

%-type execution_context() :: basic_utils:execution_context().
-type three_digit_version() :: basic_utils:three_digit_version().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().


-type directory_path() :: file_utils:directory_path().
%-type bin_directory_path() :: file_utils:bin_directory_path().

-type env_variable_name() :: system_utils:env_variable_name().

-type epmd_port() :: net_utils:epmd_port().

-type application_run_context() :: otp_utils:application_run_context().

-type registration_name() :: naming_utils:registration_name().
-type registration_scope() :: naming_utils:registration_scope().


-type config_key() :: app_facilities:config_key().

%-type user_action_spec() :: us_action:user_action_spec().
-type action_result( T ) :: us_action:action_result( T ).
-type action_table() :: us_action:action_table().


-type server_pid() :: class_USServer:server_pid().
-type config_table() :: class_USServer:config_table().


% No way of centralising here start_link/1 and init/1 (e.g. they require
% ?MODULE). At least for consistency, terminate/2 not centralised here either.



-doc """
Constructs a US central server.

`AppRunContext` tells how the US application is being run.
""".
-spec construct( wooper:state(), any_string(), application_run_context() ) ->
                                                    wooper:state().
construct( State, USAppShortName, AppRunContext ) ->

	TraceCateg = ?trace_categorize("Central server"),

	% First the direct mother classes, then this class-specific actions:

	SrvState = class_USServer:construct( State, TraceCateg, _TrapExits=true ),

	?send_info_fmt( SrvState, "Creating a US central server, running ~ts.",
		[ otp_utils:application_run_context_to_string( AppRunContext ) ] ),

	?send_debug_fmt( SrvState, "Running Erlang ~ts, whose ~ts",
		[ system_utils:get_interpreter_version(),
		  code_utils:get_code_path_as_string() ] ),

	?send_debug_fmt( SrvState, "System description: ~ts",
		[ system_utils:get_system_description() ] ),

	SetState = setAttributes( SrvState, [
        { app_short_name, text_utils:ensure_binary( USAppShortName ) },
        { execution_context, undefined },
        { app_run_context, AppRunContext },
        { config_base_directory, undefined },
        { app_base_directory, undefined },
        { conf_directory, undefined },
        { data_directory, undefined },
        { log_directory, undefined },
        { waited_operations, undefined } ] ),

	?send_info_fmt( SetState, "Constructed: ~ts.", [ to_string( SetState ) ] ),

    % Log directory not known yet (possibly application-specific), too early for
    % finaliseTraceSetup/1.

    SetState.



-doc "Overridden destructor.".
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?debug( "Deletion initiated." ),

	?info( "Deleted." ),
	State.



% Method section.


-doc """
Finalises the setup of the trace system.

To be done rather late on purpose, so that the existence of that file can
be seen as a sign that the initialisation went well (used by
`start-xxx-main-{native-build,release}.sh`).

Requires notably the `log_directory` attribute to have been set.
""".
-spec finaliseTraceSetup( wooper:state() ) -> const_oneway_return().
finaliseTraceSetup( State ) ->

	% Now that the log directory is known, we can properly redirect the traces.
	% Already a trace emitter:

	LogDirBin = basic_utils:check_defined( ?getAttr(log_directory) ),

	file_utils:create_directory_if_not_existing( LogDirBin, create_parents ),

    TraceFilename = text_utils:format( "us_~ts.traces", [ ?getAttr(us_name) ] ),

	NewBinTraceFilePath = file_utils:bin_join( LogDirBin, TraceFilename ),

	?debug_fmt( "Requesting the renaming of trace file to '~ts'.",
                [ NewBinTraceFilePath ] ),

	?getAttr(trace_aggregator_pid) ! { renameTraceFile, NewBinTraceFilePath },

    wooper:const_return().




-doc "Callback triggered whenever a linked process exits.".
-spec onWOOPERExitReceived( wooper:state(), pid(),
						basic_utils:exit_reason() ) -> const_oneway_return().
onWOOPERExitReceived( State, _StoppedPid, _ExitType=normal ) ->

	% Not even a trace sent for that, as too many of them.
	%
	%?notice_fmt( "Ignoring normal exit from process ~w.", [ StoppedPid ] ),

	wooper:const_return();


onWOOPERExitReceived( State, CrashedPid, ExitType ) ->

	% Typically: "Received exit message '{{nocatch,
	%   {wooper_oneway_failed,<0.44.0>,class_XXX,
	%       FunName,Arity,Args,AtomCause}}, [...]}"

	?error_fmt( "Received and ignored an exit message '~p' from ~w.",
				[ ExitType, CrashedPid ] ),

	wooper:const_return().



-doc """
Centralises all automated actions, supported directly by this server or by the
other servers of this US application, as specified by their classnames.

All US-level user-configured, general-purpose automated actions are to be
managed by this server, so that the user code has to interact with only a single
overall server/service.
""".
-spec manageAutomatedActions( wooper:state(), config_table(),
                              [ classname() ] ) -> oneway_return().
manageAutomatedActions( State, ConfigTable, SrvClassnames ) ->

    % (to be added some day: credential_server_pid, comm_gateway_pid)

    SrvPids = [ SrvCn:get_server_pid() || SrvCn <- SrvClassnames ],

    % The aggregation on this central server of the actions of all other servers
    % of this US application should not be done based on blocking primitives
    % such as (concurrent) requests like in:
    %
    %% Interleaving:
    %% ConcurrentWaitInfo = wooper:send_concurrent_request(
    %%    _TargetInstancePids=SrvPids, _ReqName=getAutomatedActions,
    %%    _RequestArgs=[], _TimeOutMs=1000 ),
    %
    % Indeed this would be a certain cause of deadlocks, as these servers
    % require information obtained from the current server, whereas it is
    % already blocked by design.

    % So instead, as the role of this instance is essentially a server one, it
    % should refrain from sending requests (thus as a client) to other servers,
    % and act upon them asynchronously instead, i.e. based on oneways:

    % Hence not a request; could be named getAutomatedActionsAsync:
    OnewayMsg = { notifyAutomatedActions, self() },

    % Will result in each of these servers to answer with a
    % onAutomatedActionsNotified/3 oneway call:
    %
    [ SrvPid ! OnewayMsg || SrvPid <- SrvPids ],

    SrvCount = length( SrvPids ),

    % Also a check:
    { WaitState, undefined } =
        swapInAttribute( State, waited_operations, SrvCount ),

    % Integrate any user-defined actions in the meantime:
    IntegState = executeOneway( WaitState, addConfiguredAutomatedActions,
                                [ ConfigTable ] ),

    ResType = { tuple, [ {atom,success}, {string,[]} ] },

    UserResSpec = { ResType, "Help contents" },

    HelpUserActSpec = { _ActName=help, _UserArgSpec=[], UserResSpec,
                        _UserDesc="help about the US-Main supported actions" },

    HelpState = executeOneway( IntegState, addAutomatedAction,
                               HelpUserActSpec ),

    % Not applicable anymore, now that asynchronous:

    % End of interleaving:
    %% SrvActions = case wooper:wait_for_concurrent_request_results(
    %%         ConcurrentWaitInfo ) of

    %%     { Results, _TimedOutInstances=[] } ->
    %%         Results;

    %%     { _MaybeResults, TimedOutInstances } ->
    %%         throw( { fetch_action_time_out_from, TimedOutInstances } )

    %% end,

    %% ActState = executeOneway( HelpState, addAutomatedActions,
    %%     [ SrvActions ] ),

    ActState = HelpState,

    cond_utils:if_defined( us_common_debug_actions, ?debug_fmt(
        "Whereas waiting for the notifications of ~B servers, "
        "starting with an ~ts",
        [ SrvCount, us_action:action_table_to_string(
            getAttribute( ActState, action_table ) ) ] ) ),

    wooper:return_state( ActState ).



-doc """
Records the actions sent back by the specified US server.

Typically triggered by a prior `notifyAutomatedActions/2` oneway call.
""".
-spec onAutomatedActionsNotified( wooper:state(), action_table() ) ->
                                                    oneway_return().
onAutomatedActionsNotified( State, AddActTable ) ->

    WaitedCount = ?getAttr(waited_operations),

    NewMaybeWaitedCount = case WaitedCount of

        undefined ->
            throw( { unsollicited_action_table, AddActTable } );

        1 ->
            ?debug( "All action tables received." ),
            % No extra operation to be done for the moment.
            undefined;

        _ ->
           WaitedCount-1

    end,

    ?debug_fmt( "Notified of an ~ts, whereas was waiting for ~B operations.",
        [ us_action:action_table_to_string( AddActTable ), WaitedCount ] ),

    MergedActTable = us_action:merge_action_table( AddActTable,
                                                   ?getAttr(action_table) ),

    ActState = setAttributes( State, [
        { action_table, MergedActTable },
        { waited_operations, NewMaybeWaitedCount } ] ),

    wooper:return_state( ActState ).



-doc "Built-in help action request.".
-spec help( wooper:state() ) ->
                    const_request_return( action_result( ustring() ) ).
help( State ) ->

    HelpText = text_utils:format(
        "The following actions are supported by the US-~ts application:",
        [ ?getAttr(us_name) ] ),

    wooper:const_return_result( { success, HelpText } ).



% Static section.


% Version-related static methods.


-doc "Returns the version of the US application being used.".
-spec get_us_app_version() -> static_return( three_digit_version() ).
get_us_app_version() ->
	wooper:return_static(
		basic_utils:parse_version( get_us_app_version_string() ) ).


-doc "Returns the version of the US application being used, as a string.".
-spec get_us_app_version_string() -> static_return( ustring() ).
get_us_app_version_string() ->
	% As defined (uniquely) in GNUmakevars.inc:
	wooper:return_static( ?us_app_version ).





% Helper section.


-doc """
Manages any US application-configured EPMD port.

The port may be already set at the US overall level, but it can be overridden on
a per-US application basis, as it may be convenient to share one's `us.config`
between multiple applications (e.g. `US-xxx` and `US-yyy`).
""".
-spec manageEPMDPort( wooper:state(), config_table(), config_key(),
                      epmd_port() ) -> const_oneway_return().
manageEPMDPort( State, ConfigTable, EPMDPortKey, DefaultEPMDPort ) ->

	% No simple, integrated way of checking the actual port currently in use:
	{ Port, Origin } = case table:lookup_entry( EPMDPortKey, ConfigTable ) of

		key_not_found ->
			% No application EPMD port defined, so its default will apply unless
			% a port was explicitly set at the US-level:
			%

			?info_fmt( "No EPMD TCP port configured for US application, "
				"proposing its default one, ~B.", [ DefaultEPMDPort  ] ),

			{ DefaultEPMDPort, as_default };


		{ value, AppEPMDPort } when is_integer( AppEPMDPort ) ->
			?info_fmt( "Supposing already running using the US application "
					   "EPMD TCP port #~B.", [ AppEPMDPort ] ),

			{ AppEPMDPort, explicit_set };


		{ value, InvalidEPMDPort } ->
			?error_fmt(
                "Read invalid EPMD port configured for US application: '~p'.",
				[ InvalidEPMDPort ] ),

			throw( { invalid_us_application_epmd_port, InvalidEPMDPort,
                     EPMDPortKey } )

	end,

	% For correct information; available by design:
    class_USConfigServer:get_server_pid() !
		{ notifyEPMDPort, [ Port, Origin, ?MODULE, self() ] },

    wooper:const_return().



-doc """
Manages any application-configured naming registration for this instance.
""".
-spec manageRegistrations( wooper:state(), config_table(),
    registration_name(), registration_scope() ) -> oneway_return().
manageRegistrations( State, _ConfigTable, DefRegName, DefRegScope ) ->

    % Overriding these defaults would be unnecessarily complex:
    CfgRegName = DefRegName,
    CfgRegScope = DefRegScope,

	naming_utils:register_as( CfgRegName, CfgRegScope ),

	?info_fmt( "Registered this US application configuration server as '~ts' "
               "(scope: ~ts).", [ CfgRegName, CfgRegScope ] ),

	RegState = setAttributes( State, [
		% Inherited attributes:
		{ registration_name, CfgRegName },
		{ registration_scope, CfgRegScope } ] ),

    wooper:return_state( RegState ).



-doc """
Manages any application-configured specification regarding the (operating-system
level) user running this US application.
""".
-spec manageSystemUser( wooper:state(), config_table(), config_key() ) ->
                                            oneway_return().
manageSystemUser( State, ConfigTable, UsernameKey ) ->

    ActualUsername = system_utils:get_user_name(),

	% Mostly used by start/stop/kill scripts:
	AppUsername = case table:lookup_entry( UsernameKey, ConfigTable ) of

		key_not_found ->
			?info_fmt( "No configured US application operating-system "
                "username set for this server; runtime-detected: '~ts'.",
				[ ActualUsername ] ),
			ActualUsername;

		{ value, CfgUsername } when is_list( CfgUsername ) ->

			% No overriding expected:
			basic_utils:check_undefined( ?getAttr(username) ),

			case ActualUsername of

				CfgUsername ->
					?info_fmt( "Using configured US application "
                        "operating-system username '~ts' for this server, "
                        "which matches the current runtime user.",
                        [ CfgUsername ] ),
					CfgUsername;

				OtherUsername ->
					?error_fmt( "The configured US application "
                        "operating-system username '~ts' for this server "
                        "does not match the current runtime user, '~ts'.",
						[ ActualUsername, OtherUsername ] ),
					throw( { inconsistent_os_us_user, OtherUsername,
							 ActualUsername, UsernameKey } )

			end

	end,

	SetState = setAttribute( State, username,
                             text_utils:string_to_binary( AppUsername ) ),

    wooper:return_state( SetState ).



-doc """
Manages any application-configured application base directory, and sets related
directories.
""".
-spec manageAppBaseDirectories( wooper:state(), config_table(), config_key(),
                                env_variable_name() ) -> oneway_return().
manageAppBaseDirectories( State, ConfigTable, BaseDirKey, BaseDirEnvVarName ) ->

	% As opposed to, say, start/stop script, the Erlang code does not care so
	% much about these directories, so warnings, not errors, were issued if
	% not found (the US framework being also launchable thanks to, for example,
	% 'make debug'). We finally opted for a stricter policy, as errors could be
	% induced afterwards.

	AppRunContext = ?getAttr(app_run_context),

    AppShortName = ?getAttr(app_short_name),

	MaybeConfBaseDir = case table:lookup_entry( BaseDirKey, ConfigTable ) of

		key_not_found ->
			undefined;

		{ value, D } when is_list( D ) ->
			?info_fmt( "The configured US application base directory is '~ts'.",
                       [ D ] ),
			D;

		{ value, InvalidDir }  ->
			?error_fmt( "Read invalid US application base directory in "
                        "configuration: '~p'.", [ InvalidDir ] ),
			throw( { invalid_us_app_base_directory, InvalidDir, BaseDirKey,
					 AppRunContext } )

	end,

	MaybeBaseDir = case MaybeConfBaseDir of

		undefined ->
			case system_utils:get_environment_variable( BaseDirEnvVarName ) of

				false ->
					undefined;

				% Might be set, yet to an empty string, typically because of
				% US_XXX_APP_BASE_DIR="${US_XXX_APP_BASE_DIR}":
				%
				"" ->
					undefined;

				EnvDir ->
					?info_fmt( "No US application base directory set in "
                        "configuration file, using the value of the '~ts' "
                        "environment variable: '~ts'.",
                        [ BaseDirEnvVarName, EnvDir ] ),
					EnvDir

			end;

		_ ->
			MaybeConfBaseDir

	end,

    USAppStrWithUnderscore = text_utils:format( "us_~ts", [ AppShortName ] ),

	RawBaseDir = case MaybeBaseDir of

		undefined ->
			guess_app_dir( AppRunContext, USAppStrWithUnderscore,
                           BaseDirEnvVarName, State );

		_ ->
			MaybeBaseDir

	end,

	BaseDir = file_utils:ensure_path_is_absolute( RawBaseDir ),

	% We check not only that this candidate app directory exists, but also that
	% it is a right one, expecting to have a 'priv' direct subdirectory then:

	MaybeBaseBinDir =
			case file_utils:is_existing_directory_or_link( BaseDir ) of

		true ->
			BinBaseDir = text_utils:string_to_binary( BaseDir ),

			case AppRunContext of

				as_otp_release ->

                    USAppStrWithDash = text_utils:format( "us-~ts",
                        [ AppShortName ] ),

					% As, if run as a release, it may end with a version (e.g.
					% "us_xxx-0.0.1"), or a "us_xxx-latest" symlink thereof, or
					% directly as "us-xxx":
					%
                    BaseDirName = filename:basename( BaseDir ),

					% From a clone made with our deployment conventions:
                    case text_utils:is_prefixed_with( BaseDirName,
                                USAppStrWithUnderscore ) orelse
                            % For a clone made to a default directory (e.g. by
                            % CI):
                            %
                            text_utils:is_prefixed_with( BaseDirName,
                                USAppStrWithDash ) of

                        true ->
							?info_fmt( "US (release) application base "
								"directory set to '~ts'.", [ BaseDir ] ),
							BinBaseDir;

                        false ->
                            %?warning_fmt( "The US application base "
							%  "directory '~ts' does not seem legit (it "
							%  "should start with 'us_xxx'), thus considering "
							%  "knowing none.", [ BaseDir ] ),
							%undefined
							throw( { incorrect_us_application_base_directory,
									 BaseDir, BaseDirKey, AppRunContext,
                                     as_otp_release } )

					end;

				as_native ->
					case file_utils:get_last_path_element( BaseDir ) of

						USAppStrWithUnderscore ->
							?info_fmt( "US (native) application base "
									   "directory set to '~ts'.", [ BaseDir ] ),
							BinBaseDir;

						_Other ->
							throw( { incorrect_us_application_base_directory,
									 BaseDir, BaseDirKey, AppRunContext,
                                     as_native } )

					end

			end,

			% Final paranoid check:
			PrivDir = file_utils:join( BinBaseDir, "priv" ),
			case file_utils:is_existing_directory_or_link( PrivDir ) of

				true ->
					BinBaseDir;

				false ->
					?error_fmt( "The determined US application base "
						"directory '~ts' does not have a 'priv' subdirectory.",
						[ BinBaseDir ] ),
					throw( { no_priv_us_application_base_directory, BaseDir,
                             BaseDirKey } )

			end;


		false ->
			%?warning_fmt( "The US application base directory '~ts' does "
			%   "not exist, thus considering knowing none.", [ BaseDir ] ),
			%undefined
			throw( { non_existing_us_applocation_base_directory, BaseDir,
					 BaseDirKey } )


	end,

	% The internal US-Main directory (see the conf_directory attribute) used to
	% be derived from the app base one (as a 'conf' subdirectory thereof), yet
	% because of that it was not included in releases. So instead this 'conf'
	% directory is a subdirectory of 'priv':
	%
	% (for some reason, using this module, although it is listed in us_xxx.app,
	% results, for code:priv_dir/1, in a bad_name exception)
	%
	%TargetMod = ?MODULE,
	%TargetMod = us_xxx_app,
	TargetMod = text_utils:atom_format( "~ts_sup", [ AppShortName ] ),

	ConfBinDir = file_utils:bin_join(
		otp_utils:get_priv_root( TargetMod, _BeSilent=true ), "conf" ),

	% Set in all cases:
	SetState = setAttributes( State, [ { app_base_directory, MaybeBaseBinDir },
                                       { conf_directory, ConfBinDir } ] ),

    wooper:return_state( SetState ).


-doc "Tries to guess the US-Main application directory.".
-spec guess_app_dir( application_run_context(), app_name(),
                     env_variable_name(), wooper:state() ) -> directory_path().

guess_app_dir( AppRunContext, USAppStrWithUnderscore, BaseDirEnvVarName,
               State ) ->

	CurrentDir = file_utils:get_current_directory(),

	GuessingDir = case AppRunContext of

		as_otp_release ->
			% In [...]/us_xxx/_build/default/rel/us_xxx, and we want the first
			% us_xxx, so:
			%
			OTPPath = file_utils:normalise_path( file_utils:join(
				[ CurrentDir, "..", "..", "..", ".." ] ) ),

			case file_utils:get_base_path( OTPPath ) of

				USAppStrWithUnderscore ->
					% Looks good:
					OTPPath;

				% Not found; another try, if running as a test (from
				% us_xxx/test):
				%
				_ ->
					file_utils:get_base_path( CurrentDir )

			end;

		as_native ->
			% In the case of a native build, running from us_xxx/src (covers
			% also the case where a test is being run from us_xxx/test), so:
			%
			file_utils:get_base_path( CurrentDir )

	end,

	% Was a warning:
	?info_fmt( "No configured US application base directory set "
		"(neither in configuration file nor through the '~ts' environment "
		"variable), hence trying to guess it, in a ~ts context, as '~ts'.",
		[ BaseDirEnvVarName, AppRunContext, GuessingDir ] ),

	GuessingDir.



-doc """
Manages any user-configured data directory to rely on, creating it if necessary.
""".
-spec manageDataDirectory( wooper:state(), config_table(), config_key(),
                           directory_path() ) -> oneway_return().
manageDataDirectory( State, ConfigTable, DataDirKey, DefaultDataBaseDir ) ->

	BaseDir = case table:lookup_entry( DataDirKey, ConfigTable ) of

		key_not_found ->
			file_utils:ensure_path_is_absolute( DefaultDataBaseDir,
                                                ?getAttr(app_base_directory) );

		{ value, D } when is_list( D ) ->
			file_utils:ensure_path_is_absolute( D,
				?getAttr(app_base_directory) );

		{ value, InvalidDir }  ->
			?error_fmt(
                "Read invalid configured application data directory: '~p'.",
                [ InvalidDir ] ),
			throw( { invalid_data_directory, InvalidDir, DataDirKey } )

	end,

	file_utils:is_existing_directory( BaseDir ) orelse
		?warning_fmt( "The base data directory '~ts' does not exist, "
					  "creating it.", [ BaseDir ] ),

	% Would lead to inconvenient paths, at least if defined as relative:
	%DataDir = file_utils:join( BaseDir, ?app_subdir ),
	DataDir = BaseDir,

	try

		file_utils:create_directory_if_not_existing( DataDir, create_parents )

	catch

		{ create_directory_failed, _DataDir, access_denied, ErrorInfo } ->

			?error_fmt( "Unable to create the directory for working data "
				"'~ts': please ensure its parent directory can be written "
				"by the current user, or set it to a different path thanks "
                "to the '~ts' key. Detailed error information: ~p",
                [ DataDir, DataDirKey, ErrorInfo ] ),

			throw( { data_directory_creation_failed, DataDir, access_denied,
                     ErrorInfo } );

		E ->
			throw( { data_directory_creation_failed, DataDir, E } )

	end,

	% Enforce security in all cases ("chmod 700"); if it fails here, the
	% combined path/user configuration must be incorrect; however we might not
	% be the owner of that directory (e.g. if the US application user is
	% different from the US one). So:
	%
	CurrentUserId = system_utils:get_user_id(),

	% If not owned, do nothing:
	file_utils:get_owner_of( DataDir ) =:= CurrentUserId andalso
		file_utils:change_permissions( DataDir,
			[ owner_read, owner_write, owner_execute,
			  group_read, group_write, group_execute ] ),

	BinDataDir = text_utils:ensure_binary( DataDir ),

	SetState = setAttribute( State, data_directory, BinDataDir ),

    wooper:return_state( SetState ).



-doc """
Manages any configured US application log directory to rely on, creating it if
necessary.
""".
-spec manageLogDirectory( wooper:state(), config_table(), config_key(),
                          directory_path() ) -> oneway_return().
manageLogDirectory( State, ConfigTable, LogDirKey, DefaultLogDir ) ->

	% Longer paths if defined as relative, yet finally preferred as
	% '/var/log/universal-server/us-main' (rather than
	% '/var/log/universal-server') as it allows separating US-Main from any
	% other US-* services:
	%
	LogDir = case table:lookup_entry( LogDirKey, ConfigTable ) of

		key_not_found ->
			% Bound to require special permissions:
			DefaultLogDir;

		{ value, D } when is_list( D ) ->
			file_utils:ensure_path_is_absolute( D,
												?getAttr(app_base_directory) );

		{ value, InvalidDir }  ->
			?error_fmt( "Read invalid application configured log "
                        "directory: '~p'.", [ InvalidDir ] ),
			throw( { invalid_log_directory, InvalidDir, LogDirKey } )

	end,

	file_utils:is_existing_directory( LogDir ) orelse
		begin

			%throw( { non_existing_base_us_app_log_directory, LogDir } )

			?warning_fmt( "The base US application log directory '~ts' "
                          "does not exist, creating it.", [ LogDir ] ),

			% As for example the default path would require to create
			% /var/log/universal-server/us-xxx:
			%
			file_utils:create_directory_if_not_existing( LogDir,
														 create_parents )

		end,

	% Enforce security in all cases ("chmod 700"); if it fails here, the
	% combined path/user configuration must be incorrect; however we might not
	% be the owner of that directory (e.g. if the US application user is
	% different from the US-Common one).
	%
	% So:
	%
	CurrentUserId = system_utils:get_user_id(),

	% If not owned, does nothing:
	CurrentUserId =:= file_utils:get_owner_of( LogDir ) andalso
		begin

			Perms = [ owner_read, owner_write, owner_execute,
					  group_read, group_write, group_execute ],

			file_utils:change_permissions( LogDir, Perms )

		end,

	BinLogDir = text_utils:ensure_binary( LogDir ),

	SetState = setAttribute( State, log_directory, BinLogDir ),

    wooper:return_state( SetState ).



-doc "Returns a textual description of this central server.".
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->
	text_utils:format( "US-~ts central ~ts; it is running ~ts, "
		"running in the ~ts execution context, "
		"relying on the '~ts' configuration directory",
		[ ?getAttr(app_short_name), class_USServer:to_string( State ),
		  otp_utils:application_run_context_to_string(
			?getAttr(app_run_context) ), ?getAttr(execution_context),
		  ?getAttr(config_base_directory) ] ).
