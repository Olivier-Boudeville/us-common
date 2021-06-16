% Copyright (C) 2014-2020 Olivier Boudeville
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
% Creation date: Saturday, June 28, 2014.

% @doc The <b>mother class</b> of all US servers.
-module(class_USServer).

-define( class_description, "Mother class of all US servers." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_TraceEmitter ] ).


% Exported helpers:
-export([ register_name/3, unregister_name/1, to_string/1 ]).


-type server_pid() :: class_TraceEmitter:emitter_pid().

-export_type([ server_pid/0 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().
-type server_name() :: ustring().

-type registration_name() :: naming_utils:registration_name().
-type registration_scope() :: naming_utils:registration_scope().



% Implementation notes:
%
% We have to store to time-related information, one, 'server_start', to
% determine actual durations, and one, 'server_gregorian_start', to record the
% actual time at which this server was started.


% Class-specific attributes of a service are:
-define( class_attributes, [

	% Attempt also to keep this millisecond count not too large:
	{ server_start, time_utils:ms_monotonic(),
	 "a point of time reference for later duration measurements, in VM "
	 "monotonic time (milliseconds)" },

	% As Gregorian conventions are used for conversions (from a given measured
	% duration, obtained through monotonic times), adding this value allows,
	% with the Gregorian result, to establish directly a proper (absolute)
	% timestamp:
	%
	{ server_gregorian_start, ms_since_year_0(),
	 "the internal system time at which this server was started "
	 "since year 0, to facilitate timestamp conversions to/from user time" },

	% The reference to tell whether a server shall be registered (regardless of
	% the scope):
	%
	{ registration_name, maybe( registration_name() ),
	 "records the name of this server, as possibly registered in the "
	  "naming service" },

	{ registration_scope, maybe( registration_scope() ),
	 "records the scope of the registration of this server in the naming "
	 "service" } ] ).



% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


-define( trace_emitter_categorization, "US" ).


% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").



% @doc Constructs a new server instance.
%
% Parameter is ServerName, the name of that server.
%
-spec construct( wooper:state(), server_name() ) -> wooper:state().
construct( State, ServerName ) ->
	init_common( ServerName, _RegistrationName=undefined,
				 _RegistrationScope=undefined, _TrapExits=true, State ).


% @doc Constructs a new server instance, with may trap exits.
%
% Parameter is ServerName, the name of that server, and whether it should trap
% EXITS, if wanting a better control by resisting to exit messages being
% received (see the onWOOPERExitReceived/3 callback):
%
-spec construct( wooper:state(), server_name(), boolean() ) -> wooper:state().
construct( State, ServerName, TrapExits ) ->
	init_common( ServerName, _RegistrationName=undefined,
				 _RegistrationScope=undefined, TrapExits, State ).



% @doc Constructs a new, registered, server instance.
%
% Parameters are:
%
% - ServerName, the name of that server
%
% - RegistrationName, the name under which this server shall be registered
%
% - RegistrationScope, the scope at which this server shall be registered
%
-spec construct( wooper:state(), server_name(), registration_name(),
				 registration_scope() ) -> wooper:state().
construct( State, ServerName, RegistrationName, RegistrationScope ) ->
	init_common( ServerName, RegistrationName, RegistrationScope,
				 _TrapExits=true, State ).


% (helper)
-spec init_common( server_name(), maybe( registration_name() ),
		maybe( registration_scope() ), boolean(), wooper:state() ) ->
							wooper:state().
init_common( ServerName, MaybeRegistrationName, MaybeRegistrationScope,
			 TrapExits, State ) ->

	case TrapExits of

		true ->
			% Wanting a better control by resisting to exit messages being
			% received (see the onWOOPERExitReceived/3 callback):
			%
			erlang:process_flag( trap_exit, true );

		false ->
			ok

	end,

	% First the direct mother classes:
	TraceState = class_TraceEmitter:construct( State,
										?trace_categorize(ServerName) ),

	% Constant based on the number of milliseconds of the EPOCH, since year 0;
	% used in order to compute the most complete offset (in UTC):
	%
	MsOfEpoch = time_utils:get_epoch_milliseconds_since_year_0(),

	SetState = setAttributes( TraceState, [

		{ server_start, time_utils:get_monotonic_time() },

		% Hence since year 0 (so a large number), based on "user" time:
		{ server_gregorian_start,
		  MsOfEpoch + time_utils:get_system_time() } ] ),

	register_name( MaybeRegistrationName, MaybeRegistrationScope, SetState ).



% @doc Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?info_fmt( "Deleting server ~ts.", [ to_string( State ) ] ),

	UnregState = unregister_name( State ),

	?info( "Server deleted." ),

	UnregState.



% Method section.


% @doc Pings this server.
%
% Any server must be able to answer to (asynchronous) ping requests from a
% monitoring server.
%
% (const oneway, as meant to be asynchronous)
%
-spec ping( wooper:state(), class_Supervisor:ping_id(), pid() ) ->
					const_oneway_return().
ping( State, PingId, MonitorPid ) ->

	% Sends back another oneway (no result expected here):
	MonitorPid ! { pong, [ PingId, self() ] },

	wooper:const_return().



% @doc Callback triggered, if this server enabled the trapping of exits,
% whenever a linked process terminates.
%
-spec onWOOPERExitReceived( wooper:state(), pid(),
		basic_utils:exit_reason() ) -> const_oneway_return().
onWOOPERExitReceived( State, StopPid, _ExitType=normal ) ->
	?info_fmt( "Ignoring normal exit from process ~w.", [ StopPid ] ),
	wooper:const_return();

onWOOPERExitReceived( State, CrashPid, ExitType ) ->

	% Typically: "Received exit message '{{nocatch,
	%						{wooper_oneway_failed,<0.44.0>,class_XXX,
	%							FunName,Arity,Args,AtomCause}}, [...]}"

	?error_fmt( "Received and ignored an exit message '~p' from ~w.",
				[ ExitType, CrashPid ] ),

	wooper:const_return().



% Exported helpers.


% @doc Registers this (supposedly not registered) server to naming server.
%
% (exported helper)
%
-spec register_name( registration_name(), registration_scope(),
					 wooper:state() ) -> wooper:state().
register_name( _RegistrationName=undefined, RegistrationScope, State ) ->

	% May be done later in the construction of the actual instance (ex: based on
	% a configuration file being then read):
	%
	cond_utils:if_defined( us_common_debug_registration,
		?debug( "As a US server: no name to register, "
				"no registration performed." ) ),

	setAttributes( State, [ { registration_name, undefined },
							{ registration_scope, RegistrationScope } ] );


register_name( RegistrationName, RegistrationScope, State ) ->

	naming_utils:register_as( RegistrationName, RegistrationScope ),

	cond_utils:if_defined( us_common_debug_registration,
		?debug_fmt( "Registered (~ts) as '~ts'.",
					[ RegistrationScope, RegistrationName ] ) ),

	setAttributes( State, [ { registration_name, RegistrationName },
							{ registration_scope, RegistrationScope } ] ).



% @doc Unregisters this (supposedly registered) server from naming server.
%
% (exported helper)
%
-spec unregister_name( wooper:state() ) -> wooper:state().
unregister_name( State ) ->

	case ?getAttr(registration_name) of

		undefined ->
			cond_utils:if_defined( us_common_debug_registration, ?info(
			  "No registration name available, no unregistering performed." ) ),
			State;

		RegName ->

			RegScope = ?getAttr(registration_scope),

			cond_utils:if_defined( us_common_debug_registration, ?info_fmt(
				"Unregistering from name '~ts' (scope: ~ts).",
				[ RegName, RegScope ] ) ),

			naming_utils:unregister( RegName, RegScope ),

			State

	end.



% @doc Returns a textual description of this server.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	StartTimestamp = time_utils:gregorian_ms_to_timestamp(
						?getAttr(server_gregorian_start) ),

	UptimeStr = time_utils:duration_to_string(
			time_utils:get_monotonic_time() - ?getAttr(server_start) ),

	TimeStr = text_utils:format( "started on ~ts (uptime: ~ts)",
		[ time_utils:timestamp_to_string( StartTimestamp ), UptimeStr ] ),

	RegString = case ?getAttr(registration_name) of

		undefined ->
			"with no registration name defined";

		RegName ->
			text_utils:format( "whose registration name is '~ts' (scope: ~ts)",
							   [ RegName, ?getAttr(registration_scope) ] )

	end,

	text_utils:format( "server named '~ts', ~ts, ~ts",
					   [ ?getAttr(name), TimeStr, RegString ] ).
