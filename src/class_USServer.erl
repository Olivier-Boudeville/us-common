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

It centralises states and behaviours on their behalf.
""".


-define( class_description, "Mother class of all US servers." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_TraceEmitter ] ).


% Exported helpers:
-export([ register_name/3, unregister_name/1,
          send_action_trace/3, send_action_trace_fmt/4,
          to_string/1 ]).


-doc "The PID of a US server.".
-type server_pid() :: class_TraceEmitter:emitter_pid().


-doc "The PID of a client of a US server.".
-type client_pid() :: pid().


-type ping_id() :: count().

-doc "A table holding US-related configuration information.".
-type config_table() :: app_facilities:config_table().


-export_type([ server_pid/0, client_pid/0, ping_id/0, config_table/0 ]).


-include("class_USServer.hrl").

% All known, licit (top-level) base keys for the US common configuration file:
% (other keys are in their respective server classes)
%
-define( known_us_common_config_keys, [ ?us_actions_key ] ).


% For the action_info record:
-include("us_action.hrl").



% Type shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().
-type trace_format() :: text_utils:trace_format().
-type trace_values() :: text_utils:trace_values().

-type registration_name() :: naming_utils:registration_name().
-type registration_scope() :: naming_utils:registration_scope().
-type lookup_info() :: naming_utils:lookup_info().

-type user_name() :: system_utils:user_name().

-type trace_severity() :: trace_utils:trace_severity().
-type trace_message() :: trace_utils: trace_message().

-type emitter_init() :: class_TraceEmitter:emitter_init().


-type user_action_spec() :: us_action:user_action_spec().
-type action_table() :: us_action:action_table().
-type action_token() :: us_action:action_token().
-type action_outcome() :: us_action:action_outcome().
-type action_info() :: us_action:action_info().



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
	  "the user (if any) under which this server is expected to run" },

    { action_table, action_table(),
      "the table recording all the actions supported by this server" } ] ).



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
expected to be run as the specified user (if any).

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

    % Enable all US servers to rely on a trace bridge, notably if using
    % dependencies that can optionally rely on Ceylan-Traces, or for their local
    % functions that do not have a relevant TraceEmitter state available in
    % order to send their traces:
    %
    class_TraceEmitter:register_bridge( TraceState ),

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

		{ username, text_utils:maybe_string_to_binary( MaybeUserName ) },

        { action_table, list_table:new() } ] ),

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

Not to be mixed up with the `ping` direct WOOPER message.

(const oneway, not request, as meant to be asynchronous)
""".
-spec ping( wooper:state(), ping_id(), pid() ) -> const_oneway_return().
ping( State, PingId, MonitorPid ) ->

	% Sends back another oneway (no result expected here):
	MonitorPid ! { pong, [ PingId, self() ] },

	wooper:const_return().



-doc """
Integrates the actions in the specified table (possibly read from a
configuration file) in the internal action table of this server.
""".
-spec addConfiguredAutomatedActions( wooper:state(), config_table() ) ->
                                                        oneway_return().
addConfiguredAutomatedActions( State, ConfigTable ) ->

    case table:lookup_entry( ?us_actions_key, ConfigTable ) of

        { value, UserActSpecs } when is_list( UserActSpecs ) ->

            send_action_trace_fmt( debug,
                "Integrating the following user action specs: ~p.",
                [ UserActSpecs ], State ),

            AddState = addAutomatedActionSpecs( State, UserActSpecs ),

            wooper:return_state( AddState );


        { value, InvalidUserActSpecs }  ->
            send_action_trace_fmt( error,
                "Invalid (non-list) user-level action specifications: ~p.",
                [ InvalidUserActSpecs ], State ),

            throw( { invalid_user_action_specs, non_list,
                     InvalidUserActSpecs } );


        key_not_found ->
            send_action_trace( debug, "No automated actions defined.", State ),
            wooper:const_return()

    end.



-doc """
Adds (in first position and in-order) the specified user-level action to the
internal action table of this server.
""".
-spec addAutomatedActionSpec( wooper:state(), user_action_spec() ) ->
                                                    oneway_return().
addAutomatedActionSpec( State, UserActSpec ) ->

    AddState = wooper:executeOneway( State, addAutomatedActionSpecs,
                                     [ [ UserActSpec ] ] ),

    wooper:return_state( AddState ).



-doc """
Adds (in first position) the specified user-level actions to the internal action
table of this server.
""".
-spec addAutomatedActionSpecs( wooper:state(), [ user_action_spec() ] ) ->
                                                    oneway_return().
addAutomatedActionSpecs( State, UserActSpecs ) ->

    RegActTable = us_action:register_action_specs( UserActSpecs,
        ?getAttr(action_table), wooper:get_classname( State ) ),

    ?error_fmt( "from ~w to ~w.", [ ?getAttr(action_table), RegActTable ] ),
    RegState = setAttribute( State, action_table, RegActTable ),

    wooper:return_state( RegState ).



% Not of use anymore:
%-doc """
%A concurrent request to return the automated actions known of this server.
%""".
% -spec getAutomatedActions( wooper:state() ) ->
%                 const_request_return( concurrent_result( action_table() ) ).
% getAutomatedActions( State ) ->
%     ActTable = ?getAttr(action_table),
%     ConcurrentRes = wooper:forge_concurrent_result( ActTable ),
%
%     cond_utils:if_defined( us_common_debug_actions, send_action_trace_fmt(
%         debug, "Returning ~ts",
%         [ us_action:action_table_to_string( ActTable ) ], State ) ),
%
%     wooper:const_return_result( ConcurrentRes ).



-doc """
Registers the additional actions in the specified table in our internal one.
""".
-spec registerAutomatedActions( wooper:state(), action_table() ) ->
                                            oneway_return().
registerAutomatedActions( State, AddActTable ) ->
    MergedActTable = us_action:merge_action_table( AddActTable,
                                                   ?getAttr(action_table) ),
    MergedState = setAttribute( State, action_table, MergedActTable ),

    wooper:return_state( MergedState ).



-doc """
Tells that this server should notify - thanks to the sending of an
`onAutomatedActionsNotified/3` oneway call - the specified instance (generally
the one of the caller) of the automated actions that it supports.

This asynchronous form has for purpose to avoid the deadlocks that
`getAutomatedActions/1` would induce.

Typically called based on `class_USCentralServer:manageAutomatedActions/3`.
""".
-spec notifyAutomatedActions( wooper:state(), instance_pid() ) ->
                                                    const_oneway_return().
notifyAutomatedActions( State, InstToNotifyPid ) ->

    OurLookupInfo = executeConstRequest( State, getLookupInformation ),

    OurLookupInfo =:= undefined andalso throw( no_lookup_info ),

    % The local actions shall here bear a relevant, absolute lookup info:
    ToSendActTable = list_table:map_on_values(
        fun( AI=#action_info{ server_lookup_info=undefined } ) ->
            AI#action_info{ server_lookup_info=OurLookupInfo };

           ( AI ) ->
            AI
        end,
        % For a given server, we want the actions to be listed by action name:
        lists:sort( ?getAttr(action_table) ) ),

    cond_utils:if_defined( us_common_debug_actions,
        send_action_trace_fmt( debug, "Sending to ~w ~ts",
            [ InstToNotifyPid,
              us_action:action_table_to_string( ToSendActTable ) ],
        State ) ),

    % Classname added to be able to sort first by server:
    InstToNotifyPid !
        { onAutomatedActionsNotified,
            [ ToSendActTable, wooper:get_classname( State ) ] },

    wooper:const_return().



-doc """
Requests this server to perform the automated action corresponding to the
specified tokens.

The PID of this server is returned so that callers may request multiple actions
to be performed concurrently yet still be able to match each outcome to each
call.
""".
-spec performActionFromTokens( wooper:state(), [ action_token() ] ) ->
                        request_return( { action_outcome(), server_pid() } ).
performActionFromTokens( State, Tokens ) ->

    send_action_trace_fmt( debug,
        "Performing action from the following ~B tokens: ~p.",
        [ length( Tokens ), Tokens ], State ),

    ActId = us_action:get_action_id( Tokens ),

    ActTable = ?getAttr(action_table),

    { Outcome, ActState } = case list_table:lookup_entry( _K=ActId,
                                                          ActTable ) of

        { value, ActionInfo } ->
            execute_action( ActionInfo, Tokens, State );

        key_not_found ->
            % Only a *user* error:
            send_action_trace_fmt( notice,
                "No action ~ts found; the ones known of this server are: ~ts",
                [ us_action:action_id_to_string( ActId ),
                  text_utils:strings_to_string(
                    [ us_action:action_id_to_string( AId )
                        || AId <- list_table:keys( ActTable ) ] ) ], State ),

            ActOutcome ={ action_failed, _FailureReport=action_not_found },

            { ActOutcome, State }

    end,

    wooper:return_state_result( ActState, { Outcome, self() } ).



% The caller is expected to have ensured that the correct number of argument
% tokens has been provided.
%
% (helper)
-spec execute_action( action_info(), [ action_token() ], wooper:state() ) ->
                                        { action_outcome(), wooper:state() }.
% No lookup information, hence action implemented locally:
execute_action( ActInfo=#action_info{ server_lookup_info=undefined,
                                      arg_specs=ArgSpecs,
                                      result_spec=ResSpec,
                                      request_name=ReqName },
                Tokens, State ) ->

    cond_utils:if_defined( us_common_debug_actions, send_action_trace_fmt(
        debug, "Executing locally ~ts, based on the following tokens;~n ~p",
        [ us_action:action_info_to_string( ActInfo ), Tokens ], State ) ),

    case us_action:coerce_token_arguments( Tokens, ArgSpecs ) of

        { ok, ActualArgs } ->
            apply_arguments( ReqName, ActualArgs, Tokens, ResSpec, State );

        { error, DiagStr } ->
            Outcome= { action_failed, { wrong_argument_count, DiagStr } },
            { Outcome, State }

    end;

% Action implemented by another server, forwarding it:
execute_action( ActInfo=#action_info{ server_lookup_info=ImplSrvLookupInfo },
                Tokens, State ) ->

    case naming_utils:get_maybe_registered_pid_for( ImplSrvLookupInfo ) of

        undefined ->

            send_action_trace_fmt( error, "Failed to resolve the ~ts for ~ts.",
                [ naming_utils:lookup_info_to_string( ImplSrvLookupInfo ),
                  us_action:action_info_to_string( ActInfo ) ], State ),

            %throw( { impl_server_lookup_info_lookup_failed, ImplSrvLookupInfo,
            %         ActInfo } );

            FailureReport =
                { implementation_server_not_found, ImplSrvLookupInfo },

            { _Outcome={ action_failed, FailureReport }, State };


        SrvPid ->

            cond_utils:if_defined( us_common_debug_actions,
                send_action_trace_fmt( debug,
                    "Forwarding to server ~w (~ts) ~ts.", [ SrvPid,
                        naming_utils:lookup_info_to_string( ImplSrvLookupInfo ),
                        us_action:action_info_to_string( ActInfo ) ], State ) ),

            SrvPid ! { performActionFromTokens, [ Tokens ], self() },
            receive

                { wooper_result, { ActOutcome, SrvPid } } ->

                    cond_utils:if_defined( us_common_debug_actions,
                       send_action_trace_fmt( debug,
                           "Returning received action outcome from "
                           "~w:~n ~p", [ SrvPid, ActOutcome ], State ) ),

                    { ActOutcome, State }

            % A time-out might be used here.

            end

    end.



% (helper)
-spec apply_arguments( request_name(), [ us_action:argument() ],
    [ action_token() ], us_action:result_spec(), wooper:state() ) ->
                                        { action_outcome(), wooper:state() }.
apply_arguments( ReqName, ActualArgs, Tokens, ResSpec, State ) ->

    cond_utils:if_defined( us_common_debug_actions, send_action_trace_fmt(
        debug, "Executing now request ~ts/~B, with non-state arguments ~p.",
        [ ReqName, length( ActualArgs )+1, ActualArgs ], State ) ),

    % ActualRes is opaque (possibly of the fallible/2 type):
    try executeRequest( State, ReqName, ActualArgs ) of

        { ExecState, ActualRes } ->

            case us_action:is_result_matching_spec( ActualRes, ResSpec ) of

                true ->
                    send_action_trace_fmt( debug, "Executed request ~ts/~B "
                        "with non-state arguments ~p, got result:~n ~p",
                        [ ReqName, length( ActualArgs )+1, ActualArgs,
                          ActualRes ], ExecState ),

                    { _Outcome={ action_done, ActualRes }, ExecState };


                false ->
                     send_action_trace_fmt( error, "Executed request ~ts/~B "
                        "with non-state arguments ~p, returned a result that "
                        "does not match its result specification (~p):~n ~p",
                        [ ReqName, length( ActualArgs )+1, ActualArgs,
                          us_action:result_spec_to_string( ResSpec ),
                          ActualRes ], ExecState ),

                    Outcome= { action_failed,
                               _FailureReport=invalid_result_type },

                    { Outcome, State }

            end

    catch

        throw:Error ->

            ArgTokens = tl( Tokens ),

            send_action_trace_fmt( error, "Exception thrown when executing "
                "the action-implementation ~ts/~B request, based on "
                "non-state argument tokens ~p: ~p",
                [ ReqName, length( ArgTokens )+1, ArgTokens, Error ],
                State ),

            FailureReport = { exception_thrown, Error },

           { _Outcome={ action_failed, FailureReport }, State }

    end.



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



% Static section.


-doc """
Resolves the PID of the specified server, based on the specified naming
information, with some waiting if needed.

Centralised here on behalf of the implementation (notably of their
`get_server_pid/0` static method) of all US servers, for example to be able to
define any relevant time-out duration once.
""".
-spec resolve_server_pid( registration_name(), registration_scope() ) ->
                                static_return( server_pid() ).
resolve_server_pid( RegName, RegScope ) ->

    cond_utils:if_defined( us_common_debug_registration,
        trace_bridge:debug_fmt( "Resolving the US server registered as "
            "name '~ts' for scope ~ts.", [ RegName, RegScope ] ) ),

    % Relying on the default time-out currently:
    SrvPid = naming_utils:wait_for_registration_of( RegName,
		naming_utils:registration_to_lookup_scope( RegScope ) ),

    cond_utils:if_defined( us_common_debug_registration,
        trace_bridge:debug_fmt( "Resolved as ~w.", [ SrvPid ] ) ),

	wooper:return_static( SrvPid ).



-doc """
Returns the naming lookup information of this server.

Meant to be possibly defined by each actual US server, albeit generally
`get_server_pid/0` is sufficient.
""".
-spec get_lookup_info() -> static_return( lookup_info() ).
get_lookup_info() ->

    %wooper:return_static( { ?foo_server_registration_name,
    %   ?foo_server_registration_scope } ).

    throw( not_implemented ).



-doc "Returns any naming lookup information of this server.".
-spec getLookupInformation( wooper:state() ) ->
                                const_request_return( option( lookup_info() ) ).
getLookupInformation( State ) ->

    MaybeLI = case ?getAttr(registration_name) of

        undefined ->
            undefined;

        RegName ->
            LookupScope = naming_utils:registration_to_lookup_scope(
                ?getAttr(registration_scope) ),
            { RegName, LookupScope }

    end,

    wooper:const_return_result( MaybeLI ).



-doc """
Returns the PID of the current server, possibly waiting for it.

It is better to obtain it each time from the naming service rather than to
resolve and store its PID once for all, as, for an increased robustness, servers
may be restarted (hence any former PID may not reference a live process
anymore).

Meant to be defined by each actual US server.
""".
-spec get_server_pid() -> static_return( server_pid() ).
get_server_pid() ->

	%SrvPid = class_USServer:resolve_server_pid(
    %   _RegName=?foo_server_registration_name,
    %   _RegScope=?foo_server_registration_scope ),

	%wooper:return_static( SrvPid ).

    throw( not_implemented ).




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



-doc "Sends a trace categorised as belonging to the automated actions.".
-spec send_action_trace( trace_severity(), trace_message(), wooper:state() ) ->
                                                    void().
send_action_trace( TraceSeverity, TraceMsg, State ) ->
    class_TraceEmitter:send_named_emitter( TraceSeverity, State, TraceMsg,
                                           _EmitterName="Automated actions" ).


-doc "Sends a trace categorised as belonging to the automated actions.".
-spec send_action_trace_fmt( trace_severity(), trace_format(), trace_values(),
                             wooper:state() ) -> void().
send_action_trace_fmt( TraceSeverity, TraceFormat, TraceValues, State ) ->
	TraceMsg = text_utils:format( TraceFormat, TraceValues ),
    send_action_trace( TraceSeverity, TraceMsg, State ).



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
			text_utils:format( "expected to run as user '~ts'", [ BinUsrName ] )

	end,

    ActStr = us_action:action_table_to_string( ?getAttr(action_table) ),

	text_utils:format( "server named '~ts', ~ts, ~ts, ~ts; this server has ~ts",
					   [ ?getAttr(name), TimeStr, RegStr, UserStr, ActStr ] ).
