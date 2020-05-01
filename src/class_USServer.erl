% Copyright (C) 2014-2020 Olivier Boudeville
%
% This file is part of US-Common, part of the Universal Server framework.
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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Creation date: Saturday, June 28, 2014.


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
	% with the Gregorian result, to establish directly a proper (absolut)
	% timestamp:
	%
	{ server_gregorian_start, ms_since_year_0(),
	 "the internal system time at which this server was started "
	 "since year 0, to facilitate timestamp conversions to/from user time" },

	{ registration_name, maybe( naming_utils:registration_name() ),
	 "records the name of this server, as registered in the naming service" },

	{ registration_scope, maybe( naming_utils:registration_scope() ),
	 "records the scope of the registration of this server in the naming "
	 "service" } ] ).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


-define( trace_emitter_categorization, "US" ).


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% Constructs a new server instance.
%
% Parameter is ServerName, the name of that server.
%
-spec construct( wooper:state(), ustring() ) -> wooper:state().
construct( State, ServerName ) ->
	init_common( ServerName, _RegistrationName=undefined,
				 _RegistrationScope=undefined, State ).



% Constructs a new, registered, server instance.
%
% Parameters are:
%
% - ServerName, the name of that server
% - RegistrationName, the name under which this server shall be registered
% - RegistrationScope, the scope at which this server shall be registered
%
-spec construct( wooper:state(), ustring(), naming_utils:registration_name(),
				 naming_utils:registration_scope() ) -> wooper:state().
construct( State, ServerName, RegistrationName, RegistrationScope ) ->
	init_common( ServerName, RegistrationName, RegistrationScope, State ).



% (helper)
-spec init_common( ustring(), naming_utils:registration_name(),
		 naming_utils:registration_scope(), wooper:state() ) -> wooper:state().
init_common( ServerName, RegistrationName, RegistrationScope, State ) ->

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
	 { server_gregorian_start, MsOfEpoch + time_utils:get_system_time() } ] ),

	register_name( RegistrationName, RegistrationScope, SetState ).



% Destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?info_fmt( "Deleting server ~s.", [ to_string( State ) ] ),

	UnregState = unregister_name( State ),

	?info( "Server deleted." ),

	UnregState.



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



% Registers this (supposedly not registered) server to naming server.
%
% (exported helper)
%
-spec register_name( naming_utils:registration_name(),
					 naming_utils:registration_scope(), wooper:state() ) ->
						  wooper:state().
register_name( _RegistrationName=undefined, _RegistrationScope, State ) ->
	?info( "No name to register, no registration performed." ),
	State;

register_name( RegistrationName, RegistrationScope, State ) ->

	naming_utils:register_as( RegistrationName, RegistrationScope ),

	?info_fmt( "Registered (~s) as '~s'.",
			  [ RegistrationScope, RegistrationName ] ),

	setAttributes( State, [ { registration_name, RegistrationName },
							{ registration_scope, RegistrationScope } ] ).



% Unregisters this (supposedly registered) server from naming server.
%
% (exported helper)
%
-spec unregister_name( wooper:state() ) -> wooper:state().
unregister_name( State ) ->

	case ?getAttr(registration_name) of

		undefined ->
			?info( "No registration name available, "
				  "no unregistering performed." ),
			State;

		RegName ->

			RegScope = ?getAttr(registration_scope),

			?info_fmt( "Unregistering from name '~s' (scope: ~s).",
					  [ RegName, RegScope ] ),

			naming_utils:unregister( RegName, RegScope ),

			State

	end.



% Returns a textual description of this server.
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	StartTimestamp = time_utils:gregorian_ms_to_timestamp(
					  ?getAttr(server_gregorian_start) ),

	UptimeStr = time_utils:duration_to_string(
		 time_utils:get_monotonic_time() - ?getAttr(server_start) ),

	TimeStr = text_utils:format( "started on ~s (uptime: ~s)",
		[ time_utils:timestamp_to_string( StartTimestamp ), UptimeStr ] ),

	RegString = case ?getAttr(registration_name) of

		undefined ->
			"with no registration name defined";

		RegName ->
			text_utils:format( "whose registration name is '~s' (scope: ~s)",
							  [ RegName, ?getAttr(registration_scope) ] )

	end,

	text_utils:format( "server named '~s', ~s, ~s",
					  [ ?getAttr(name), TimeStr, RegString ] ).
