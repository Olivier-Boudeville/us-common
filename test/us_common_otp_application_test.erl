% Copyright (C) 2020-2023 Olivier Boudeville
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
% Creation date: Thursday, May 7, 2020.


% @doc Testing of <b>US-Common as an OTP active application</b>, directly from
% within its code base (hence without needing to create a separate, mock-up test
% OTP release for that).
%
-module(us_common_otp_application_test).


% Exported for re-use by other tests (ex: in US-Web):
-export([ get_us_information/0 ]).


% For run/0 export and test traces:
-include_lib("traces/include/traces_for_tests.hrl").

% For us_common_scheduler_registration_{name,scope}:
% (path not set yet from here)
%
%-include_lib("us_common/include/us_common_defines.hrl").
-include("us_common_defines.hrl").


% Shorthands:

-type bin_directory_path() :: file_utils:bin_directory_path().
-type file_path() :: file_utils:file_path().

-type registration_name() :: naming_utils:registration_scope().
-type registration_scope() :: naming_utils:registration_scope().

-type us_config_table() :: class_USConfigServer:us_config_table().


-spec get_us_information() -> { bin_directory_path(), file_path(),
			us_config_table(), registration_name(), registration_scope() }.
get_us_information() ->

	% We have to link notably to the upcoming US-Common configuration
	% server. However starting an application does not provide a means of
	% knowing its PID (none returned) and this server is not registered under a
	% fixed name, as it may be read from the US configuration file. So this test
	% has also to locate and read that file in order to determine the name to
	% query:

	BinCfgDir = case class_USConfigServer:get_us_config_directory() of

		{ undefined, CfgDirMsg } ->

			trace_bridge:error_fmt( "Test is unable to determine the US "
				"configuration directory; ~ts", [ CfgDirMsg ] ),

			% CfgDirMsg too verbose for:
			throw( us_configuration_directory_not_found );

		{ BinFoundCfgDir, CfgDirMsg } ->
			trace_bridge:info( CfgDirMsg ),
			BinFoundCfgDir

	end,

	{ ConfigTable, CfgFilePath } = case
			class_USConfigServer:get_configuration_table( BinCfgDir ) of

		{ ok, P } ->
			P;

		{ error, DiagnosedReason } ->
			basic_utils:throw_diagnosed( DiagnosedReason )

	end,

	test_facilities:display( "Read US configuration from '~ts'.",
							 [ CfgFilePath ] ),

	{ CfgRegName, CfgRegScope, CfgNamingMsg } =
		case class_USConfigServer:get_registration_info( ConfigTable ) of

			{ ok, T } ->
				T;

			{ error, { InvalidThisRegName, CfgRefNameKey } } ->
				trace_bridge:error_fmt( "Read invalid user-configured "
					"registration name for this US configuration server "
					"(key: '~ts'): '~p'.",
					[ CfgRefNameKey, InvalidThisRegName ] ),
				throw( { invalid_us_config_registration_name,
						 InvalidThisRegName, CfgRefNameKey } )

	end,

	trace_bridge:info( CfgNamingMsg ),

	% Now we are able to link to the future US configuration server.

	{ BinCfgDir, CfgFilePath, ConfigTable, CfgRegName, CfgRegScope }.



% Actual test:
test_us_common_application( OrderedAppNames ) ->

	test_facilities:display( "Starting the US-Common OTP active application." ),

	% We did not trap EXIT messages, as we wanted this test to crash (thanks to
	% the links below) in case of problem (and not to receive an EXIT message
	% bound not to be read, as it happened when no US configuration file was
	% found).
	%
	% However such tests may crash even when stopping (normally) applications,
	% as apparently an OTP application has its child processes terminated with
	% reason 'shutdown' (not 'normal').
	%
	% So now this test process traps EXIT messages, and ensures that none
	% besides {'EXIT',P,shutdown}, P being the PID of a US-Common process, is
	% received (actually for US-Common no such message is received, unlike for
	% the WOOPER counterpart test case).
	%
	false = erlang:process_flag( trap_exit, true ),

	{ _BinCfgDir, _CfgFilePath, _ConfigTable, CfgRegName, CfgRegScope } =
		get_us_information(),

	% No ?test_start/?test_stop here, as we start/stop Traces through
	% OTP-related operations.
	%
	% If in batch mode (not in a release, hence no sys.config read here, so only
	% the --batch command-line option matters here), the trace aggregator will
	% record that a trace supervisor is wanted later (iff renamed), otherwise
	% (not in batch mode), no trace supervisor is wanted at all.
	%
	otp_utils:start_applications( OrderedAppNames ),

	USCfgSrvPid = naming_utils:wait_for_registration_of( CfgRegName,
		naming_utils:registration_to_look_up_scope( CfgRegScope ) ),

	% The top-level user process may not be aware that an OTP application fails
	% (ex: because its main process crashed), which is a problem for a test. So
	% here we link explicitly this test process to the US configuration server,
	% to have a chance of detecting issues:
	%
	erlang:link( USCfgSrvPid ),

	% The same (simpler - less choices) for the US-Common scheduler:
	SchedPid = naming_utils:wait_for_registration_of(
		?us_common_scheduler_registration_name,
		naming_utils:registration_to_look_up_scope(
			?us_common_scheduler_registration_scope ) ),

	erlang:link( SchedPid ),


	% If not in batch mode, this renaming will trigger the launch of the trace
	% supervisor whose activation was deferred until then:
	%
	traces_utils:name_trace_file_from( ?MODULE ),

	?test_info( "Starting the US-Common OTP active application." ),

	?test_info_fmt( "US-Common version: ~p.",
					[ system_utils:get_application_version( us_common ) ] ),


	% Of course shall be sent before the stopping of Traces:
	?test_info( "Successful test (not fully ended yet) of the US-Common OTP "
				"application." ),

	% Including US-Common:
	?test_info( "Stopping all user applications." ),
	otp_utils:stop_user_applications( OrderedAppNames ),


	% Not able to use Traces anymore:
	trace_utils:debug_fmt( "Waiting for the termination of the US-Common "
						   "configuration server (~w).", [ USCfgSrvPid ] ),

	receive

		{'EXIT', USCfgSrvPid, normal } ->
			ok

	end,


	trace_utils:debug_fmt( "Waiting for the termination of the US-Common "
						   "scheduler (~w).", [ SchedPid ] ),

	receive

		{'EXIT', SchedPid, normal } ->
			ok

	end,

	% None expected to be left:
	basic_utils:check_no_pending_message(),

	test_facilities:display(
		"Successful end of test of the US-Common OTP application." ).



% Note that the {us_common, traces, wooper, myriad}.app files will have to be
% found and used for this test to succeed: US-Common, Traces, WOOPER and Myriad
% must be already available as prerequisite, fully-built OTP applications.
%
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Build root directory from which sibling prerequisite applications may be
	% found:
	%
	BuildRootDir = "..",

	OrderedAppNames =
		otp_utils:prepare_for_execution( _ThisApp=us_common, BuildRootDir ),

	trace_bridge:info_fmt( "Resulting applications to start, in order: ~w.",
						   [ OrderedAppNames ] ),

	test_us_common_application( OrderedAppNames ),

	test_facilities:stop().
