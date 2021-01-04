% Copyright (C) 2019-2021 Olivier Boudeville
%
% This file belongs to the US-Common project, a part of the Universal Server
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
% Creation date: Tuesday, December 24, 2019.


-module(class_USConfigServer).

-define( class_description,
		 "Singleton server holding the configuration information of the "
		 "Universal Server, at the level of US-Common." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_USServer ] ).


% Shorthands:
-type directory_path() :: file_utils:directory_path().
-type bin_directory_path() :: file_utils:bin_directory_path().

-type bin_file_path() :: file_utils:bin_file_path().
-type server_pid() :: class_UniversalServer:server_pid().


% A table holding US configuration information:
-type us_config_table() :: table( atom(), term() ).



% Design notes:
%
% This overall, singleton server registers itself globally, so that other
% services can interact with it even if running in separate virtual machines
% (ex: US-web).
%
% The base directories for configuration information are, by decreasing order of
% priority:
%  - $XDG_CONFIG_HOME (default: "$HOME/.config")
%  - $XDG_CONFIG_DIRS (default: "/etc/xdg", directories being separated by ':')
%
% See
% https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
% and
% https://stackoverflow.com/questions/1024114/location-of-ini-config-files-in-linux-unix
% for more information.
%
%
% All base directories shall be absolute directories.
%
% The configuration directory is defined as the ?app_subdir sub-directory of the
% base directories that contains the ?config_filename files, and all other
% related configuration files.
%
% Ex: "~/.config/universal-server".

% See also: the start/stop scripts and us-common.sh, which apply mostly the same
% look-up logic.


-define( class_attributes, [

	{ config_base_directory, bin_directory_path(),
	  "the base directory where all US configuration is to be found" },

	% No vm_cookie :: net_utils:cookie() stored, as can be read directly from
	% the VM.

	{ epmd_port, net_utils:tcp_port(),
	  "the EPMD TCP port presumably in use (as read from the configuration)" },

	{ tcp_port_range, maybe( net_utils:tcp_port_range() ),
	  "the range (if any) of TCP ports to use for out-of-band inter-VM "
	  "communication (not using the Erlang carrier; ex: for send_file)" },

	{ execution_context, basic_utils:execution_context(),
	  "tells whether this server is to run in development or production mode" },

	{ log_directory, bin_directory_path(),
	  "the directory where all US-specific, low-level logging (as opposed to "
	  "Erlang-level log files such as erlang.log.* or to higher-level traces) "
	  "will be done" },

	{ web_config_filename, maybe( bin_file_path() ),
	  "the path to the configuration file (if any) regarding US-web (i.e. "
	  "webserver, virtual hosting, etc.)" },

	{ us_server_pid, maybe( server_pid() ),
	  "the PID of the associated US server (if any)" },

	{ us_username, system_utils:user_name(),
	  "the user who runs the Universal server application (note that there "
	  "may be discrepancies between the one of US and the one of other "
	  "servers such as US-web)" },

	{ us_groupname, system_utils:group_name(),
	  "the group that shall be common to all US-related users" },


	{ us_server_registration_name, naming_utils:registration_name(),
	  "the name under which the US server may be registered" },

	{ us_web_config_server_pid, maybe( class_UniversalServer:server_pid() ),
	  "the PID of the US web configuration server (if any)" },

	% Cannot easily be obtained otherwise:
	{ registration_name, naming_utils:registration_name(),
	  "the name under which this configuration server is registered" },

	{ app_base_directory, bin_directory_path(),
	  "the base directory where this US application is located (ex: where "
	  "the 'priv' directory can be found)" } ] ).


% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.US-Common.Configuration" ).


% For various defines:
-include("class_USConfigServer.hrl").



% The defaut registration name of the overall US configuration server:
-define( default_us_config_reg_name, us_config_server ).

% The defaut registration name of the overall US server:
-define( default_us_reg_name, us_server ).


% The defaut registration scope of the overall US configuration server:
-define( default_registration_scope, global_only ).


% The name of the main Universal Server configuration file:
-define( us_config_filename, "us.config" ).


% Keys possibly read from the US configuration filename:
-define( vm_cookie_key, vm_cookie ).
-define( epmd_port_key, epmd_port ).
-define( tcp_port_range_key, tcp_port_range ).

-define( execution_context_key, execution_context ).

-define( us_username_key, us_username ).
-define( us_groupname_key, us_groupname ).

-define( us_server_registration_name_key,
		 us_server_registration_name ).

-define( us_config_server_registration_name_key,
		 us_config_server_registration_name ).


-define( us_app_base_dir_key, us_app_base_dir ).

-define( us_log_dir_key, us_log_dir ).

-define( us_web_config_filename_key, us_web_config_filename ).


% All known, licit keys for the US configuration file:
-define( known_config_keys, [ ?vm_cookie_key, ?epmd_port_key,
	?tcp_port_range_key, ?execution_context_key,
	?us_username_key, ?us_groupname_key,
	?us_server_registration_name_key, ?us_config_server_registration_name_key,
	?us_app_base_dir_key, ?us_log_dir_key, ?us_web_config_filename_key ] ).


% The last-resort environment variable:
-define( us_app_env_variable, "US_APP_BASE_DIR" ).

-define( default_log_base_dir, "/var/log" ).

% Exported helpers:
-export([ get_execution_target/0 ]).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").

% To define get_execution_target/0:
-include_lib("myriad/include/utils/basic_utils.hrl").



% Constructs the US configuration server, using the default logic to find its
% configuration file.
%
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->

	% Wanting a better control by resisting to exit messages being received:
	erlang:process_flag( trap_exit, true ),

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_USServer:construct( State,
								   ?trace_categorize("Configuration Server") ),

	?send_info( TraceState, "Creating a US configuration server." ),

	BinCfgDir = case get_us_config_directory() of

		{ undefined, CfgMsg } ->
			?send_error_fmt( TraceState, "Unable to determine the US "
							 "configuration directory: ~s.", [ CfgMsg ] ),
			throw( us_configuration_directory_not_found );

		{ BinFoundCfgDir, CfgMsg } ->
			?send_info( TraceState, CfgMsg ),
			BinFoundCfgDir

	end,

	CfgState = load_configuration( BinCfgDir, TraceState ),

	% Enforce security in all cases ("chmod 700"); if it fails here, the
	% combined path/user configuration must be incorrect; however this server
	% may be run from another US application (typically US-web), possibly
	% running as a user of their own, different from the main US user (yet
	% supposedly in the same US group).
	%
	% So:

	CurrentUserId = system_utils:get_user_id(),

	LogDir = getAttribute( CfgState, log_directory ),

	case file_utils:get_owner_of( LogDir ) of

		CurrentUserId ->
			file_utils:change_permissions( LogDir,
			  [ owner_read, owner_write, owner_execute,
				group_read, group_write, group_execute ] );

		% Not owned, do nothing:
		_OtherId ->
			ok

	end,

	ReadyState = setAttributes( CfgState, [
					{ config_base_directory, BinCfgDir },
					{ us_server_pid, undefined },
					{ us_web_config_server_pid, undefined } ] ),

	?send_info_fmt( TraceState, "Now ready: ~s.", [ to_string( ReadyState ) ] ),

	ReadyState.




% Constructs the US configuration server, using specified configuration
% directory.
%
% Useful for example to create auxiliary universal servers.
%
-spec construct( wooper:state(), directory_path() ) -> wooper:state().
construct( State, ConfigDir ) when is_list( ConfigDir ) ->

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_USServer:construct( State,
									   ?trace_categorize("USConfigServer") ),

	?send_info_fmt( TraceState, "Creating the overall US configuration server, "
		"using the '~s' configuration directory for that.", [ ConfigDir ] ),

	BinCfgDir = text_utils:string_to_binary( ConfigDir ),

	CfgState = load_configuration( BinCfgDir, TraceState ),

	ReadyState = setAttributes( CfgState, [
					{ config_base_directory, BinCfgDir },
					{ us_web_config_server_pid, undefined } ] ),

	?send_info( TraceState, to_string( ReadyState ) ),

	ReadyState.



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Automatic unregistering.

	?info( "Deleted." ),

	State.



% Method section.


% Notifies this server about the specified US web configuration server, and
% requests web-information from it.
%
-spec getWebRuntimeSettings( wooper:state() ) -> request_return(
			   { bin_directory_path(), basic_utils:execution_context(),
				 maybe( bin_file_path() ), maybe( server_pid() ) } ).
getWebRuntimeSettings( State ) ->

	USWebConfigServerPid = ?getSender(),

	RegState = case ?getAttr(us_web_config_server_pid) of

		undefined ->
			?info_fmt( "Registering web configuration server ~w.",
					   [ USWebConfigServerPid ] ),
			setAttribute( State, us_web_config_server_pid,
						  USWebConfigServerPid );

		USWebConfigServerPid ->
			State;

		OtherPid ->
			?error_fmt( "Notified of web configuration server ~w; ignored, as "
				"already knowing ~w.", [ USWebConfigServerPid, OtherPid ] ),
			State

	end,

	wooper:return_state_result( RegState, { ?getAttr(config_base_directory),
			?getAttr(execution_context), ?getAttr(web_config_filename),
			?getAttr(us_server_pid) } ).



% Static section.


% Returns the main default settings regarding the US configuration server, for
% its clients.
%
-spec get_default_settings() -> static_return(
		  { file_utils:file_name(), basic_utils:atom_key(),
			naming_utils:registration_name(), naming_utils:look_up_scope() } ).
get_default_settings() ->

	wooper:return_static( { ?us_config_filename,
		?us_config_server_registration_name_key, ?default_us_config_reg_name,
		naming_utils:registration_to_look_up_scope(
		  ?default_registration_scope ) } ).



% Returns any found configuration directory and a corresponding trace message.
%
% This is a static method (no state involved), so that both this kind of servers
% and others (ex: web configuration ones) can use the same, factored, logic.
%
-spec get_us_config_directory() -> static_return(
		   { maybe( bin_directory_path() ), text_utils:ustring() } ).
get_us_config_directory() ->

	HomeDir = system_utils:get_user_home_directory(),

	% See design notes about directory selection.

	FirstEnvVar = "XDG_CONFIG_HOME",

	% We prefer devising a single trace message rather than too many:
	{ FirstPath, FirstMsg } =
			case system_utils:get_environment_variable( FirstEnvVar ) of

		false ->
			CfgHomeDefaultPath = file_utils:join( HomeDir, ".config" ),
			CfgHomeMsg = text_utils:format(
					"no '~s' environment variable defined, defaulting to '~s'",
					[ FirstEnvVar, CfgHomeDefaultPath ] ),
			{ CfgHomeDefaultPath, CfgHomeMsg };

		Path ->
			{ Path, text_utils:format( "path '~s' was obtained from "
					   "environment variable '~s'", [ Path, FirstEnvVar ] ) }

	end,

	SecondEnvVar = "XDG_CONFIG_DIRS",

	{ ListedPathsAsStrings, SecondMsg } =
		case system_utils:get_environment_variable( SecondEnvVar ) of

		false ->
			% A single one here:
			DefaultCfgDirs = "/etc/xdg",
			CfgDirsMsg = text_utils:format(
				"no '~s' environment variable defined, defaulting to '~s'",
				[ SecondEnvVar, DefaultCfgDirs ] ),
			{ DefaultCfgDirs, CfgDirsMsg };

		Paths ->
			{ Paths, text_utils:format( "paths '~s' was obtained from "
						"environment variable '~s'", [ Paths, SecondEnvVar ] ) }

	end,

	ListedPaths = text_utils:split( ListedPathsAsStrings, _Seps=[ $: ] ),

	AllBasePaths = [ FirstPath | ListedPaths ],

	CfgSuffix = file_utils:join( ?app_subdir, ?us_config_filename ),

	BaseMsg = text_utils:format( "searched for Universal Server "
		"configuration directory, based on suffix '~s', knowing that: ~s~n"
		"Configuration directory ", [ CfgSuffix,
				text_utils:strings_to_string( [ FirstMsg, SecondMsg ] ) ] ),

	ResPair = find_file_in( AllBasePaths, CfgSuffix, BaseMsg, _Msgs=[] ),

	wooper:return_static( ResPair ).




% Helper section.



% (helper)
find_file_in( _AllBasePaths=[], CfgSuffix, BaseMsg, Msgs ) ->

	% Configuration directory not found:

	FullMsg = BaseMsg ++ text_utils:format( "could not be determined, "
		"short of locating a relevant configuration file ('~s') for that: ",
		[ CfgSuffix ] )
		++ text_utils:strings_to_enumerated_string( lists:reverse( Msgs ) ),

	{ undefined, FullMsg };


find_file_in( _AllBasePaths=[ Path | T ], CfgSuffix, BaseMsg, Msgs ) ->

	CfgFilePath = file_utils:normalise_path(
					file_utils:join( Path, CfgSuffix ) ),

	case file_utils:is_existing_file_or_link( CfgFilePath ) of

		true ->
			CfgDir = filename:dirname( CfgFilePath ),

			FullMsg = text_utils:format( "Configuration directory found "
				"as '~s', as containing '~s'", [ CfgDir, CfgFilePath ] )
				++ case Msgs of

					   [] ->
						   "";

					   _ ->
						   ", after having searched through: "
							   ++ text_utils:strings_to_enumerated_string(
									lists:reverse( Msgs ) )

				   end,

			{ text_utils:string_to_binary( CfgDir ), FullMsg };

		false ->
			NewMsgs = [ text_utils:format( "not found as '~s'",
										   [ CfgFilePath ] ) | Msgs ],
			find_file_in( T, CfgSuffix, BaseMsg, NewMsgs )

	end.



% Returns the Universal Server configuration table (i.e. the one of US, not
% specifically of US web), and directly applies some of the read settings.
%
-spec load_configuration( bin_directory_path(), wooper:state() ) ->
								wooper:state().
load_configuration( BinCfgDir, State ) ->

	CfgFilename = file_utils:join( BinCfgDir, ?us_config_filename ),

	% Should, by design, never fail:
	case file_utils:is_existing_file_or_link( CfgFilename ) of

		true ->
			ok;

		false ->
			?error_fmt( "The overall US configuration file ('~s') "
				"could not be found.", [ CfgFilename ] ),
			% Must have disappeared then:
			throw( { us_config_file_not_found, CfgFilename } )

	end,

	?info_fmt( "Reading the Universal Server configuration from '~s'.",
			   [ CfgFilename ] ),

	% Ensures as well that all top-level terms are pairs indeed:
	ConfigTable = table:new_from_unique_entries(
					file_utils:read_terms( CfgFilename ) ),

	?info_fmt( "Read US configuration ~s", [ table:to_string( ConfigTable ) ] ),

	% We follow the usual order in the configuration file:

	% Const:
	manage_vm_cookie( ConfigTable, State ),

	EpmdState = manage_epmd_port( ConfigTable, State ),

	TCPState = manage_tcp_port_range( ConfigTable, EpmdState ),

	ExecState = manage_execution_context( ConfigTable, TCPState ),

	UserState = manage_os_user_group( ConfigTable, ExecState ),

	RegState = manage_registration_names( ConfigTable, UserState ),

	DirState = manage_app_base_directory( ConfigTable, RegState ),

	LogState = manage_log_directory( ConfigTable, DirState ),

	WebState = manage_web_config( ConfigTable, LogState ),

	% Detect any extraneous, unexpected entry:
	LicitKeys = ?known_config_keys,

	case list_utils:difference( table:keys( ConfigTable ), LicitKeys ) of

		[] ->
			WebState;

		UnexpectedKeys ->
			?error_fmt( "Unknown key(s) in '~s': ~s~nLicit keys: ~s",
				[ CfgFilename, text_utils:terms_to_string( UnexpectedKeys ),
				  text_utils:terms_to_string( LicitKeys ) ] ),
			throw( { invalid_configuration_keys, UnexpectedKeys, CfgFilename } )

	end.




% Manages any user-configured VM cookie.
-spec manage_vm_cookie( us_config_table(), wooper:state() ) -> void().
manage_vm_cookie( ConfigTable, State ) ->

	case table:lookup_entry( ?vm_cookie_key, ConfigTable ) of

		key_not_found ->
			CurrentCookie = net_utils:get_cookie(),
			?info_fmt( "No user-configured cookie, sticking to original one, "
					   "'~s'.", [ CurrentCookie ] );

		{ value, UserCookie } when is_atom( UserCookie ) ->
			InitialCookie = net_utils:get_cookie(),

			?info_fmt( "Switching the Erlang cookie of the current VM from "
				"the current one, '~s', to the specified one, '~s'.",
				[ InitialCookie, UserCookie ] ),

			net_utils:set_cookie( UserCookie );

		{ value, InvalidCookie } ->
			?error_fmt( "Read invalid user-configured Erlang cookie: '~p'.",
						[ InvalidCookie ] ),
			throw( { invalid_vm_cookie, InvalidCookie, ?vm_cookie_key } )

	end.



% Manages any user-configured EPMD port.
-spec manage_epmd_port( us_config_table(), wooper:state() ) -> wooper:state().
manage_epmd_port( ConfigTable, State ) ->

	% No simple, integrated way of checking the actual port currently in use:
	Port = case table:lookup_entry( ?epmd_port_key, ConfigTable ) of

		key_not_found ->
			DefaultEpmdPort = net_utils:get_default_epmd_port(),
			?info_fmt( "No user-configured EPMD TCP port, supposing already "
			  "using the Erlang-level default one, ~B.", [ DefaultEpmdPort ] ),
			DefaultEpmdPort;

		{ value, UserEPMDPort } when is_integer( UserEPMDPort ) ->
			?info_fmt( "Supposing already running using the user-defined "
					   "EPMD TCP port #~B.", [ UserEPMDPort ] ),
			UserEPMDPort;

		{ value, InvalidEPMDPort } ->
			?error_fmt( "Read invalid user-configured EPMD port: '~p'.",
						[ InvalidEPMDPort ] ),
			throw( { invalid_epmd_port, InvalidEPMDPort, ?epmd_port_key } )

	end,

	setAttribute( State, epmd_port, Port ).



% Manages any user-configured TCP port range.
-spec manage_tcp_port_range( us_config_table(), wooper:state() ) ->
								   wooper:state().
manage_tcp_port_range( ConfigTable, State ) ->

	PortRange = case table:lookup_entry( ?tcp_port_range_key, ConfigTable ) of

		key_not_found ->
			?info( "No user-configured TCP port range." ),
			undefined;

		{ value, R={ MinTCPPort, MaxTCPPort } } when is_integer( MinTCPPort )
				andalso is_integer( MaxTCPPort )
				andalso MinTCPPort < MaxTCPPort ->
			?info_fmt( "User-configured TCP port range is [~w,~w[.",
					   [ MinTCPPort, MaxTCPPort ] ),
			R;

		{ value, InvalidTCPPortRange } ->
			?error_fmt( "Read invalid user-configured TCP port range: '~p'.",
						[ InvalidTCPPortRange ] ),
			throw( { invalid_tcp_port_range, InvalidTCPPortRange,
					 ?tcp_port_range_key } )

	end,

	setAttribute( State, tcp_port_range, PortRange ).



% Manages any user-configured execution context.
-spec manage_execution_context( us_config_table(), wooper:state() ) ->
									wooper:state().
manage_execution_context( ConfigTable, State ) ->

	MyriadExecStr= text_utils:format( "for Ceylan-Myriad: ~s",
									  [ basic_utils:get_execution_target() ] ),

	WOOPERExecStr = text_utils:format( "for Ceylan-WOOPER: ~s",
									   [ wooper:get_execution_target() ] ),

	TracesExecStr = text_utils:format( "for Ceylan-Traces: ~s",
									   [ traces:get_execution_target() ] ),

	USCommonExecTarget = get_execution_target(),

	USCommonExecStr = text_utils:format( "for US-Common: ~s",
										 [ USCommonExecTarget ] ),

	CompileContextStr = text_utils:format( "while the following compilation "
		"contexts were applied: ~s", [ text_utils:strings_to_string( [
		MyriadExecStr, WOOPERExecStr, TracesExecStr, USCommonExecStr ] ) ] ),

	Context = case table:lookup_entry( ?execution_context_key, ConfigTable ) of

		key_not_found ->
			DefaultContext = production,
			?info_fmt( "No user-configured execution context, "
				"defaulting to '~s', ~s.",
				[ DefaultContext, CompileContextStr ] ),
			DefaultContext;

		{ value, development } ->
			?info_fmt( "The 'development' execution context has been "
					   "configured by the user, ~s.", [ CompileContextStr ] ),
			development;

		{ value, production } ->
			?info_fmt( "The 'production' execution context has been configured "
					   "by the user, ~s.", [ CompileContextStr ] ),
			production;

		{ value, InvalidContext } ->
			?error_fmt( "Read invalid user-configured execution context: '~p'.",
						[ InvalidContext ] ),
			throw( { invalid_execution_context, InvalidContext,
					 ?execution_context_key } )

	end,

	case Context of

		USCommonExecTarget ->
			ok;

		_OtherContext ->
			?warning_fmt( "The runtime user-configured execution context (~s) "
				"does not match the compile-time execution target of "
				"this Universal Server (~s).",
				[ Context, USCommonExecTarget ] )

	end,

	setAttribute( State, execution_context, Context ).



% Manages any user-configured specification regarding the (operating-system
% level) US user and group.
%
-spec manage_os_user_group( us_config_table(), wooper:state() ) ->
								  wooper:state().
manage_os_user_group( ConfigTable, State ) ->

	% Mostly used by start/stop scripts:
	UsUsername = case table:lookup_entry( ?us_username_key, ConfigTable ) of

		key_not_found ->
			ActualUsername = system_utils:get_user_name(),
			?info_fmt( "No user-configured US operating-system username set "
			  "for this server; runtime-detected: '~s'.", [ ActualUsername ] ),
			ActualUsername;

		{ value, Username } when is_list( Username ) ->

			case system_utils:get_user_name() of

				Username ->
					?info_fmt( "Using user-configured US operating-system "
						"username '~s' for this server, which matches "
						"the current runtime user.", [ Username ] ),
					Username;

				OtherUsername ->

					% Currently not a blocking error as this US configuration
					% server might be run in the context of US-web, hence with
					% its username, which might differ.

					?warning_fmt( "The user-configured US operating-system "
						"username '~s' for this server does not match the "
						"current runtime user, '~s' (acceptable if created "
						"on behalf on a US-related service - typically a "
						"standalone US-web server, i.e. one with no prior "
						"US-server companion running).",
						[ Username, OtherUsername ] ),
					OtherUsername
					%throw( { inconsistent_os_us_user, OtherUsername,
					%		 Username, ?us_username_key } )

			end

	end,


	UsGroupname = case table:lookup_entry( ?us_groupname_key, ConfigTable ) of

		key_not_found ->
			ActualGroupname = system_utils:get_group_name(),
			?info_fmt( "No group-configured US operating-system group name set "
			  "for this server; runtime-detected: '~s'.", [ ActualGroupname ] ),
			ActualGroupname;

		{ value, Groupname } when is_list( Groupname ) ->
			case system_utils:get_group_name() of

				Groupname ->
					?info_fmt( "Using group-configured US operating-system "
						"groupname '~s' for this server, which matches "
						"the current runtime group.", [ Groupname ] ),
					Groupname;

				OtherGroupname ->
					?error_fmt( "The group-configured US operating-system "
						"groupname '~s' for this server does not match "
						"the current runtime group, '~s'.",
						[ Groupname, OtherGroupname ] ),
					throw( { inconsistent_os_us_group, OtherGroupname,
							 Groupname, ?us_groupname_key } )

			end

	end,

	setAttributes( State, [ { us_username,
							  text_utils:string_to_binary( UsUsername ) },
							{ us_groupname,
							  text_utils:string_to_binary( UsGroupname ) } ] ).



% Manages any user-configured registration names for this instance and for the
% US server.
%
-spec manage_registration_names( us_config_table(), wooper:state() ) ->
									   wooper:state().
manage_registration_names( ConfigTable, State ) ->

	ThisRegName = case table:lookup_entry(
		   ?us_config_server_registration_name_key, ConfigTable ) of

		key_not_found ->
			DefaultThisRegName = ?default_us_config_reg_name,
			?info_fmt( "No user-configured registration name for this server, "
					   "using default name '~s'.", [ DefaultThisRegName ] ),
			DefaultThisRegName;

		{ value, UserThisRegName } when is_atom( UserThisRegName ) ->
			?info_fmt( "Using user-configured registration name '~s' for "
					   "this server.", [ UserThisRegName ] ),
			UserThisRegName;

		{ value, InvalidThisRegName } ->
			?error_fmt( "Read invalid user-configured registration name for "
						"this server: '~p'.", [ InvalidThisRegName ] ),
			throw( { invalid_us_config_registration_name, InvalidThisRegName,
					 ?us_config_server_registration_name_key } )

	end,

	Scope = ?default_registration_scope,

	naming_utils:register_as( ThisRegName, Scope ),

	?info_fmt( "Registered as '~s' (scope: ~s).", [ ThisRegName, Scope ] ),


	% We read the registration name of the US server knowing that its upcoming
	% creation is likely (it will have to be told about the name it shall use):

	USRegName = case table:lookup_entry( ?us_server_registration_name_key,
										 ConfigTable ) of

		key_not_found ->
			DefaultRegName = ?default_us_reg_name,
			?info_fmt( "No user-configured registration name for the US "
				"server, using default name '~s'.", [ DefaultRegName ] ),
			DefaultRegName;

		{ value, UserRegName } when is_atom( UserRegName ) ->
			?info_fmt( "Using user-configured registration name '~s' for "
					   "the US server.", [ UserRegName ] ),
			UserRegName;

		{ value, InvalidRegName } ->
			?error_fmt( "Read invalid user-configured registration name for "
						"the US server: '~p'.", [ InvalidRegName ] ),
			throw( { invalid_us_registration_name, InvalidRegName,
					 ?us_server_registration_name_key } )

	end,

	setAttributes( State, [ { registration_name, ThisRegName },
							{ registration_scope, Scope },
							{ us_server_registration_name, USRegName } ] ).



% Manages any user-configured application base directory.
-spec manage_app_base_directory( us_config_table(), wooper:state() ) ->
									   wooper:state().
manage_app_base_directory( ConfigTable, State ) ->

	% As opposed to, say, start/stop script, the Erlang code does not care so
	% much about these directories, so warnings, not errors, will be issued if
	% not found (the US framework being also launchable thanks to, for example,
	% 'make debug').

	RawBaseDir = case table:lookup_entry( ?us_app_base_dir_key, ConfigTable ) of

		key_not_found ->

			case system_utils:get_environment_variable(
				   ?us_app_env_variable ) of

				false ->
					% Guessing then, typically current directory is:
					% [...]/universal_server/_build/default/rel/universal_server
					% and we want the first universal_server, so:
					%
					GuessedDir = file_utils:normalise_path( file_utils:join( [
						file_utils:get_current_directory(), "..", "..", "..",
																 ".." ] ) ),

					?info_fmt( "No user-configured US application base "
						"directory set (neither in configuration file nor "
						"through the '~s' environment variable), "
						"hence trying to guess it as '~s'.",
						[ ?us_app_env_variable, GuessedDir ] ),
					GuessedDir;

				EnvDir ->
					?info_fmt( "No user-configured US application base "
						"directory set in configuration file, using the value "
						"of the '~s' environment variable: '~s'.",
						[ ?us_app_env_variable, EnvDir ] ),
					EnvDir

			end;

		{ value, D } when is_list( D ) ->
			D;

		{ value, D } when is_binary( D ) ->
			file_utils:binary_to_string( D );

		{ value, InvalidDir }  ->
			?error_fmt( "Read invalid user-configured US application base "
						"directory: '~p'.", [ InvalidDir ] ),
			throw( { invalid_us_app_base_directory, InvalidDir,
					 ?us_app_base_dir_key } )

	end,

	BaseDir = file_utils:ensure_path_is_absolute( RawBaseDir ),

	BinDir = case file_utils:is_existing_directory_or_link( BaseDir ) of

		true ->
			text_utils:string_to_binary( BaseDir );

		false ->
			?error_fmt( "No US application base directory could be determined "
						"(tried '~s').", [ BaseDir ] ),
			throw( { non_existing_us_app_base_directory, BaseDir,
					 ?us_app_base_dir_key } )

	end,

	setAttribute( State, app_base_directory, BinDir ).



% Manages any user-configured log directory to rely on, creating it if
% necessary.
%
-spec manage_log_directory( us_config_table(), wooper:state() ) ->
							   wooper:state().
manage_log_directory( ConfigTable, State ) ->

	BaseDir = case table:lookup_entry( ?us_log_dir_key, ConfigTable ) of

		key_not_found ->
			?default_log_base_dir;

		{ value, D } when is_list( D ) ->
			file_utils:ensure_path_is_absolute( D,
												?getAttr(app_base_directory) );

		{ value, InvalidDir }  ->
			?error_fmt( "Read invalid user-configured log directory: '~p'.",
						[ InvalidDir ] ),
			throw( { invalid_log_directory, InvalidDir, ?us_log_dir_key } )

	end,

	case file_utils:is_existing_directory_or_link( BaseDir ) of

		true ->
			ok;

		false ->
			?warning_fmt( "The log directory '~s' does not exist, "
						  "creating it.", [ BaseDir ] )
			%throw( { non_existing_log_directory, BaseDir } )

	end,

	% Would lead to inconvenient paths, at least if defined as relative:
	%LogDir = file_utils:join( BaseDir, ?app_subdir ),
	LogDir = BaseDir,

	file_utils:create_directory_if_not_existing( LogDir, create_parents ),

	BinLogDir = text_utils:string_to_binary( LogDir ),

	setAttribute( State, log_directory, BinLogDir ).



% Manages any user-configured configuration filename for webservers and virtual
% hosting.
%
-spec manage_web_config( us_config_table(), wooper:state() ) -> wooper:state().
manage_web_config( ConfigTable, State ) ->

	MaybeBinWebFilename = case table:lookup_entry( ?us_web_config_filename_key,
												   ConfigTable ) of

		key_not_found ->
			?info( "No user-configured configuration filename for webservers "
				   "and virtual hosting." ),
			undefined;

		% Read but not checked intentionally (to be done by the web
		% configuration server):
		%
		{ value, WebFilename } when is_list( WebFilename ) ->
			?info_fmt( "Obtained user-configured configuration filename for "
				"webservers and virtual hosting: '~s'.", [ WebFilename ] ),
			text_utils:string_to_binary( WebFilename );

		{ value, InvalidWebFilename } ->
			?error_fmt( "Obtained invalid user-configured configuration "
				"filename for webservers and virtual hosting: '~p'.",
				[ InvalidWebFilename ] ),
			throw( { invalid_us_web_config_filename, InvalidWebFilename,
					 ?us_web_config_filename_key } )

	end,

	setAttribute( State, web_config_filename, MaybeBinWebFilename ).



% Returns a textual description of this server.
-spec to_string( wooper:state() ) -> text_utils:ustring().
to_string( State ) ->

	RegString = text_utils:format( "registered as '~s' (scope: ~s)",
		 [ ?getAttr(registration_name), ?default_registration_scope ] ),

	SrvString = case ?getAttr(us_server_pid) of

		undefined ->
			"no US server";

		SrvPid ->
			text_utils:format( "US server ~w", [ SrvPid ] )

	end,

	WebString = case ?getAttr(us_web_config_server_pid) of

		undefined ->
			"no web configuration server";

		WebPid ->
			text_utils:format( "web configuration server ~w", [ WebPid ] )

	end,

	text_utils:format( "US overall configuration server, ~s, running in "
		"the ~s execution context, presumably on a VM using EPMD port #~B, "
		"using configuration directory '~s' and log directory '~s', "
		"having found as US-web configuration file '~s', knowing ~s and ~s",
		[ RegString, ?getAttr(execution_context), ?getAttr(epmd_port),
		  ?getAttr(config_base_directory), ?getAttr(log_directory),
		  ?getAttr(web_config_filename), SrvString, WebString ] ).
