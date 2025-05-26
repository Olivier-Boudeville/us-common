% Copyright (C) 2014-2025 Olivier Boudeville
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

-module(class_USServer).

-moduledoc """
The **mother class** of all US servers.
""".


-define( class_description, "Mother class of all US servers." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_TraceEmitter ] ).


% Exported helpers:
-export([ register_name/3, unregister_name/1, to_string/1 ]).


-doc "The PID of a US server.".
-type server_pid() :: class_TraceEmitter:emitter_pid().


-doc "The PID of a client of a US server.".
-type client_pid() :: pid().


-type ping_id() :: count().


-export_type([ server_pid/0, client_pid/0, ping_id/0 ]).



% Type shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type registration_name() :: naming_utils:registration_name().
-type registration_scope() :: naming_utils:registration_scope().

-type user_name() :: system_utils:user_name().

-type emitter_init() :: class_TraceEmitter:emitter_init().


% Design notes:
%
% Servers may depend on others. A given server shall register its name as early
% as possible, and shall look-up the servers it relies on as late as possible.



% Implementation notes:
%
% We have to store to time-related information, one, 'server_start', to
% determine actual durations, and one, 'server_gregorian_start', to record the
% actual time at which this server was started.


% Class-specific attributes of a server are:
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
	{ registration_name, option( registration_name() ),
	  "records the name of this server, as possibly registered in the "
	  "naming service" },

	{ registration_scope, option( registration_scope() ),
	  "records the scope of the registration of this server in the naming "
	  "service" },

	{ username, option( user_name() ),
	  "the user (if any) under which this server is expected to run" } ] ).



% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


-define( trace_emitter_categorization, "US" ).


% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").



-doc """
Creates a server instance that is not registered, and that traps exits.

Parameter is ServerInit, the name of that server.

It is not expected to be run as any specific user.
""".
-spec construct( wooper:state(), emitter_init() ) -> wooper:state().
construct( State, ServerInit ) ->
	construct( State, ServerInit, _TrapExits=true ).



-doc """
Creates a server instance that is not registered, and that traps exits if
requested.

Parameter is ServerInit, the name of that US server, and whether it should trap
EXITS, if wanting a better control by resisting to exit messages being received
(see the onWOOPERExitReceived/3 callback).

It is not expected to be run as any specific user.
""".
-spec construct( wooper:state(), emitter_init(), boolean() ) -> wooper:state().
construct( State, ServerInit, TrapExits ) ->
	construct( State, ServerInit, _MaybeRegistrationName=undefined,
		_MaybeRegistrationScope=undefined, TrapExits ).



-doc """
Creates a server instance that is registered, and traps exits if requested.

Parameters are:
- ServerInit, the name of that US server
- MaybeRegistrationName, any name under which this server shall be registered
- MaybeRegistrationScope, any scope at which this server shall be registered

It is not expected to be run as any specific user.
""".
-spec construct( wooper:state(), emitter_init(), option( registration_name() ),
				 option( registration_scope() ) ) -> wooper:state().
construct( State, ServerInit, MaybeRegistrationName, MaybeRegistrationScope ) ->
	construct( State, ServerInit, MaybeRegistrationName, MaybeRegistrationScope,
			   _TrapExits=true ).



-doc """
Creates a server instance that is registered, and traps exits if requested.

Parameters are:
- ServerInit, the name of that US server
- MaybeRegistrationName, any name under which this server shall be registered
- MaybeRegistrationScope, any scope at which this server shall be registered
- TrapExits tells whether EXIT messages shall be trapped

It is not expected to be run as any specific user.
""".
-spec construct( wooper:state(), emitter_init(), option( registration_name() ),
				 option( registration_scope() ), boolean() ) -> wooper:state().
construct( State, ServerInit, MaybeRegistrationName, MaybeRegistrationScope,
		   TrapExits ) ->
	construct( State, ServerInit, MaybeRegistrationName, MaybeRegistrationScope,
			   TrapExits, _MaybeUserName=undefined ).



-doc """
Creates a server instance that is registered, traps exits if requested, and is
expected to be run as specified user (if any).

Parameters are:
- ServerInit, the name of that US server
- RegistrationName, the name under which this server shall be registered
- RegistrationScope, the scope at which this server shall be registered
- TrapExits, whether EXIT messages shall be trapped
- MaybeUserName, the name of any user under which this server is expected to run
""".
-spec construct( wooper:state(), emitter_init(), option( registration_name() ),
		option( registration_scope() ), boolean(), option( user_name() ) ) ->
													wooper:state().
construct( State, ServerInit, MaybeRegistrationName, MaybeRegistrationScope,
		   TrapExits, MaybeUserName ) ->

	TrapExits =:= true andalso
		% Wanting a better control by resisting to exit messages being received
		% (see the onWOOPERExitReceived/3 callback):
		%
		erlang:process_flag( trap_exit, true ),

	% First the direct mother classes:
	TraceState = class_TraceEmitter:construct( State,
		?trace_categorize(ServerInit) ),

    % Inconvenient (too verbose/too early: at start-up, gets printed on the
    % console):
    %
	%TrapExits =:= true andalso
    %    trace_bridge:debug( "Will be trapping EXIT messages." ),

	% Constant based on the number of milliseconds of the EPOCH, since year 0;
	% used in order to compute the most complete offset (in UTC):
	%
	MsOfEpoch = time_utils:get_epoch_milliseconds_since_year_0(),

	SetState = setAttributes( TraceState, [

		{ server_start, time_utils:get_monotonic_time() },

		% Hence since year 0 (so a large number), based on "user" time:
		{ server_gregorian_start,
		  MsOfEpoch + time_utils:get_system_time() },

		{ username, text_utils:maybe_string_to_binary( MaybeUserName ) } ] ),

	%trace_bridge:debug_fmt( "Registering server as '~ts' for scope ~ts.",
	%    [ MaybeRegistrationName, MaybeRegistrationScope ] ),

	register_name( MaybeRegistrationName, MaybeRegistrationScope, SetState ).



-doc "Overridden destructor.".
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?info_fmt( "Deleting server ~ts.", [ to_string( State ) ] ),

	UnregState = unregister_name( State ),

	?info( "Server deleted." ),

	UnregState.



% Method section.


-doc """
Pings this server.

Any server must be able to answer to (asynchronous) ping requests from a
monitoring server.

(const oneway, not request, as meant to be asynchronous)
""".
-spec ping( wooper:state(), ping_id(), pid() ) -> const_oneway_return().
ping( State, PingId, MonitorPid ) ->

	% Sends back another oneway (no result expected here):
	MonitorPid ! { pong, [ PingId, self() ] },

	wooper:const_return().



-doc """
Callback triggered, if this server enabled the trapping of exits, whenever a
linked process terminates.
""".
-spec onWOOPERExitReceived( wooper:state(), pid(),
		basic_utils:exit_reason() ) -> const_oneway_return().
onWOOPERExitReceived( State, StoppedPid, _ExitType=normal ) ->
	?info_fmt( "Ignoring normal exit from process ~w.", [ StoppedPid ] ),
	wooper:const_return();

onWOOPERExitReceived( State, CrashPid, ExitType ) ->

	% Typically: "Received exit message '{{nocatch,
	%   {wooper_oneway_failed,<0.44.0>,class_XXX,
	%    FunName,Arity,Args,AtomCause}}, [...]}"

	% Redundant information yet useful for console outputs:
	?warning_fmt( "US Server ~w received and ignored following exit message "
				  "from ~w:~n  ~p", [ self(), CrashPid, ExitType ] ),

	wooper:const_return().



% Exported helpers.


-doc """
Registers this (supposedly not registered) server to the naming server.

(exported helper)
""".
-spec register_name( registration_name(), registration_scope(),
					 wooper:state() ) -> wooper:state().
register_name( _RegistrationName=undefined, RegistrationScope, State ) ->

	% May be done later in the construction of the actual instance (e.g. based
	% on a configuration file being then read):
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



-doc """
Unregisters this (supposedly registered) server from the naming server.

(exported helper)
""".
-spec unregister_name( wooper:state() ) -> wooper:state().
unregister_name( State ) ->

	case ?getAttr(registration_name) of

		undefined ->
			cond_utils:if_defined( us_common_debug_registration, ?info(
				"No registration name available, "
				"no unregistering performed." ) ),
			State;

		RegName ->

			RegScope = ?getAttr(registration_scope),

			cond_utils:if_defined( us_common_debug_registration, ?info_fmt(
				"Unregistering from name '~ts' (scope: ~ts).",
				[ RegName, RegScope ] ) ),

			naming_utils:unregister( RegName, RegScope ),

			State

	end.



-doc "Returns a textual description of this server.".
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	StartTimestamp = time_utils:gregorian_ms_to_timestamp(
		?getAttr(server_gregorian_start) ),

	UptimeStr = time_utils:duration_to_string(
		time_utils:get_monotonic_time() - ?getAttr(server_start) ),

	TimeStr = text_utils:format( "started on ~ts (uptime: ~ts)",
		[ time_utils:timestamp_to_string( StartTimestamp ), UptimeStr ] ),

	RegStr = case ?getAttr(registration_name) of

		undefined ->
			"with no registration name defined";

		RegName ->
			text_utils:format( "whose registration name is '~ts' (scope: ~ts)",
							   [ RegName, ?getAttr(registration_scope) ] )

	end,

	UserStr = case ?getAttr(username) of

		undefined ->
			"not expected to run as a specific user";

		BinUsrName ->
			text_utils:format( "expected to run as user '~ts'",
							   [ BinUsrName ] )

	end,

	text_utils:format( "server named '~ts', ~ts, ~ts, ~ts",
					   [ ?getAttr(name), TimeStr, RegStr, UserStr ] ).
