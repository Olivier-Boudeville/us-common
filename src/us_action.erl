% Copyright (C) 2025-2025 Olivier Boudeville
%
% This file belongs to the US-Common project, a part of the Universal Server
% framework.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Monday, June 9, 2025.

-module(us_action).

-moduledoc """
Module in charge of the management of server-side automated actions, which are
higher-level operations that a US-Server may operate on behalf of third parties
(other servers, command-line tools, SMSs, etc.).

An example of strings triggering an automated action: `"start_alarm"` or
`"switch_on tv_plug"`.

An action is to be triggered thanks to `perform_action/{2,3}`

In practice it will be executed as a local (non-const) request, based on the
`performAction/3` request inherited from `class_USServer`.
""".


-doc "A user-level description of an action-related element.".
-type user_description() :: any_string().

-doc "A description of an action-related element.".
-type description() :: bin_string().


-doc """
The user-level specification of an action.

The designated request will be executed, based on the specified arguments
(static or dynamic).

For example
- `startAlarm`
- `{stop_room_heating, [{static, room_heater_plug, "Target device short name"}, {static, switch_off, "Device operation"}], void, "switch off the heating of my room", actOnDevice}`
- `{start_tv, [{static, tv_plug, "Target device short name"}, {static, switch_on, "Device operation"}, {static, {next_possible_today, {19,54,0}}, "Start time"}, {static, {1,0,0,0}, "DHMS periodicity"}], action_outcome, "switch on the television each day at 19h54, starting from today", schedulePeriodicalActionOnDevice}`
- `{switch_device, [{dynamic, string, "Device identifier"}, {dynamic, atom, "'on' or 'off'"}, {static, no_timeout}],
    {my_action_mod, switch_device_impl}}`
""".
-type user_action_spec() ::

    % Most complete form:
    { action_name(), [ user_arg_spec() ], user_result_spec(),
      user_description(), action_mapping() }

    % No action mapping (request name is the same as action one, directly
    % implemented in the server module):
    %
  | { action_name(), [ user_arg_spec() ], user_result_spec(),
      user_description() }

    % Additionally, no description:
  | { action_name(), [ user_arg_spec() ], user_result_spec() }

    % Then no expected result ('void'):
  | { action_name(), [ user_arg_spec() ] }

    % Then no argument expected (and returning void):
  | action_name().


-doc """
The internal specification of an action.
""".
-type action_spec() :: { action_name(), [ arg_spec() ], result_spec(),
                         option( description() ), action_mapping() }.


-doc """
The reference name of an action, used to trigger it.

It usually describes with a verb as first element the action to undertake.

It acts as a user-friendly identifier (and thus no duplicates are allowed).

The `snake_case` is preferred for them.

For example: `start_alarm`.
""".
-type action_name() :: atom().


-doc "Tells whether an action argument is to be statically or dynamically set.".
-type arg_kind() :: 'static' | 'dynamic'.


-doc """
A statically-defined actual argument (specified at action definition time),
which will be used literally.

For example: `{14, top}`.
""".
-type static_argument() :: term().


-doc """
The user-level specification of an action argument, either statically-defined
(i.e. at action definition time) or dynamically-supplied (i.e. at call time).

For example: `{dynamic, lengths, [float], "The rod lengths, in meters"}`.
""".
-type user_arg_spec() ::

    { 'static', static_argument(), user_description() } % Statically defined
  | { 'static', static_argument() } % Same with no description

  | { 'dynamic', arg_name(), type(), user_description() } % Set dynamically
  | { 'dynamic', arg_name(), type() } % Same with no description
  | { 'dynamic', arg_name() }. % 'string' type implied



-doc """
The internal specification of a dynamically-supplied action argument.

For example: `{dynamic, lengths, [float], <<"The rod lengths, in meters">>}`.
""".
-type arg_spec() :: { 'static', static_argument(), option( description() ) }
                  | { 'dynamic', arg_name(), type(), option( description() ) }.


-doc """
Describes, as a name, an argument of an action.

The `snake_case` is preferred for them.

For example: `target_actuator`.
""".
-type arg_name() :: atom().


-doc """
Describes the type of an argument of an action.

For example: `float`.
""".
-type arg_type() :: type().



-doc """
An actual argument for an action, presumably respecting its specification.
""".
-type argument() :: any().


-doc """
The user-level specification of a result.
""".
-type user_result_spec() :: { result_type(), user_description() }
                          | result_type().


-doc """
The internal specification of a result.
""".
-type result_spec() :: { result_type(), option( description() ) }.


-doc """
The type expected to be returned from an executed action.

`action_outcome` designates the `action_outcome()` type, whose use is
recommended, for controllability.
""".
-type result_type() :: type() | 'action_outcome'.


-doc """
An actual result for an action, presumably respecting its specification.
""".
-type result() :: any().



-doc """
The description of an actual request to which an action maps.

No arity listed, as it is deduced from the declared action arguments.

Such a request is expected to be defined and exported in the corresponding
module.

Mapping to a mere module rather than to a class (even if, as relying on a state
is generally essential, targeting a request rather than a mere function) to let
the possibility of gathering any set of action-related methods in a separate,
dedicated (server) module.
""".
-type action_mapping() :: { module_name(), request_name() }
                          % Direct implementation in the server module implied:
                        | request_name().



-doc "The description of an actual error.".
-type error_report() :: ustring() | 'timed_out'.


-doc """
The actual outcome of a performed action.
""".
-type action_outcome() :: { 'success', result() }
                        | { 'error', error_report() }.



-doc """
A table recording, for a given US-Server, all information about known, supported
actions.
""".
-type action_table() :: table( action_id(), action_info() ).


-doc "The identifier of an action (typically in an action table).".
-type action_id() :: { action_name(), action_arity() }.


-doc """
The arity (number of "actual" arguments) of an action - not counting the first,
State one of its mapped request).
""".
-type action_arity() :: arity().




% Internal view onto an action, notably used to coerce the arguments it
% receives; a value of an action_table/0.
%
-record( action_info, {

    % No action_{name,arity}() as they constitute the associated key.

    % Information regarding the (ordered) arguments of that action entry:
    arg_specs :: [ arg_spec() ],

    % Type and possibly description:
    result_spec :: result_spec(),

    % Mapping to an actual request:
    mapping :: action_mapping(),

    description :: option( description() ) } ).


-doc """
Internal view onto an action, notably used to coerce the arguments it receives;
a value of an `action_table/0`.
""".
-type action_info() :: #action_info{}.



-doc "A token describing an element of call to an action.".
-type action_token() :: any_string().


-doc """
Specifies the actual WOOPER request call induced by an action.

A request is used even if not specific result is expected (`void`), for
synchronisation.
""".
-type action_request() :: { wooper:request_name(), wooper:method_arguments() }.



-export_type([ user_description/0, description/0,
               user_action_spec/0, action_spec/0,
               action_name/0, action_mapping/0,

               arg_kind/0, static_argument/0,
               user_arg_spec/0, arg_spec/0, arg_name/0, arg_type/0, argument/0,
               user_result_spec/0, result_spec/0, result/0,
               error_report/0, action_outcome/0,
               action_table/0, action_id/0, action_arity/0, action_info/0,
               action_token/0, action_request/0 ]).


-export([ register_action_specs/2, perform_action/2, perform_action/3,
          action_table_to_string/1, action_entry_to_string/1,
          help/1 ]).




% Type shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().

-type module_name() :: meta_utils:module_name().

-type type() :: type_utils:type().

-type request_name() :: wooper:request_name().

-type server_pid() :: class_USServer:server_pid().



-doc """
Registers the specified user action specifications in the specified action
table.
""".
-spec register_action_specs( [ user_action_spec() ], action_table() ) ->
                                            action_table().
register_action_specs( _UserActSpecs=[], ActTable ) ->
    ActTable;

% Full spec:
register_action_specs( _UserActSpecs=[ Spec={ ActName, ArgSpecs,
        ResSpec, MaybeDesc, ActionMapping } | T ], ActTable ) ->

    is_atom( ActName ) orelse begin

		trace_bridge:error_fmt( "Invalid action name ('~p') "
			"in user action specification ~p (not an atom).",
            [ ActName, Spec ] ),

       throw( { invalid_action_name, ActName, Spec } )

                              end,

    CanonArgSpecs = canonicalise_arg_specs( ArgSpecs, ActName ),

    CanonResSpec = canonicalise_res_spec( ResSpec, ActName ),

    MaybeBinDesc = text_utils:ensure_maybe_binary( MaybeDesc ),

    ArgCount = length( CanonArgSpecs ),

    check_valid_action_mapping( ActionMapping, ArgCount ),

    ActId = { ActName, ArgCount },

    table:has_entry( _K=ActId, ActTable ) andalso begin

		trace_bridge:error_fmt( "Multiple actions ~ts defined "
            "in user action specification ~p.",
            [ action_id_to_string( ActId ), Spec ] )

                                                  end,

        throw( { multiple_action_definions, ActName, Spec } ),

    ActInfo = #action_info{ arg_specs=CanonArgSpecs,
                            result_spec=CanonResSpec,
                            mapping=ActionMapping,
                            description=MaybeBinDesc },

    NewActTable = table:add_entry( ActId, ActInfo, ActTable ),

    register_action_specs( T, NewActTable );

% No action mapping:
register_action_specs( _UserActSpecs=[
        { ActName, ArgSpecs, ResSpec, MaybeDesc } | T ], ActTable ) ->
    register_action_specs( [ { ActName, ArgSpecs, ResSpec, MaybeDesc,
                             _ActionMapping=ActName } | T ], ActTable );

% No description:
register_action_specs( _UserActSpecs=[ { ActName, ArgSpecs, ResSpec } | T ],
                       ActTable ) ->
    register_action_specs( [ { ActName, ArgSpecs, ResSpec,
                               _MaybeDesc=undefined } | T ], ActTable );

% No result type:
register_action_specs( _UserActSpecs=[ { ActName, ArgSpecs } | T ],
                       ActTable ) ->
    register_action_specs( [ { ActName, ArgSpecs, _ResSpec=void } | T ],
                           ActTable );

% No argument spec:
register_action_specs( _UserActSpecs=[ ActName | T ], ActTable ) ->
    register_action_specs( [ { ActName,  _ArgSpecs=[] } | T ], ActTable );

register_action_specs( Other, _ActTable ) ->
    throw( { invalid_action_specifications, not_list, Other } ).



-doc "Checks that the specified term is a valid action mapping.".
-spec check_valid_action_mapping( term(), arity() ) -> void().
check_valid_action_mapping( AM={ ModName, ReqName }, Arity ) ->

    is_atom( ModName ) orelse throw( { non_atom_action_module, ModName } ),

    code_utils:is_beam_in_path( ModName ) =:= not_found andalso
        throw( { action_module_not_found, ModName, AM } ),

    wooper_method_management:is_valid_request_name( ReqName ) orelse
        throw( { invalid_action_request_mapping, ReqName } ),

    FunName = ReqName,

    % For the State:
    FunArity = Arity+1,

    meta_utils:is_function_exported( ModName, FunName, FunArity )
        orelse throw( { action_request_not_exported,
                        { ModName, FunName, FunArity } } );

check_valid_action_mapping( ReqName, Arity ) when Arity > 0 ->

    % Cannot determine here the class module to check that it defines that
    % request.

    wooper_method_management:is_valid_request_name( ReqName ) orelse
        throw( { invalid_action_request_mapping, ReqName } );

check_valid_action_mapping( ReqName, Arity ) ->
    throw( { invalid_action_request_arity, Arity, ReqName } ).



-doc """
Returns an internal, canonicalised version of the specified user argument
specifications of the specified action.
""".
-spec canonicalise_arg_specs( term(), action_name() ) -> [ arg_spec() ].
canonicalise_arg_specs( ArgSpecs, ActName ) when is_list( ArgSpecs )->
    [ canonicalise_arg_spec( AS, ActName ) || AS <- ArgSpecs ];

canonicalise_arg_specs( Other, ActName ) ->
    throw( { invalid_action_argument_specs, non_list, Other, ActName } ).



-doc """
Returns an internal, canonicalised version of the specified user argument
specification of the specified action.
""".
-spec canonicalise_arg_spec( term(), action_name() ) -> arg_spec().

% Full static arg spec:
canonicalise_arg_spec( _ArgSpec={ static, Arg, MaybeUserDesc }, _ActName ) ->
    MaybeBinDesc = text_utils:ensure_maybe_binary( MaybeUserDesc ),
    { static, Arg, MaybeBinDesc };


% No description static arg spec:
canonicalise_arg_spec( _ArgSpec={ static, Arg }, _ActName ) ->
    { static, Arg, _MaybeBinDesc=undefined };

% Full dynamic arg spec:
canonicalise_arg_spec( ArgSpec={ dynamic, ArgName, ArgType, MaybeUserDesc },
                       ActName ) ->

    is_atom( ArgName ) orelse
        throw( { invalid_argument_name, ArgName, ArgSpec, ActName } ),

    type_utils:is_type( ArgType ) orelse
        throw( { invalid_argument_type, ArgType, ArgSpec, ActName } ),

    MaybeBinDesc = text_utils:ensure_maybe_binary( MaybeUserDesc ),

    { dynamic, ArgName, ArgType, MaybeBinDesc };

canonicalise_arg_spec( _ArgSpec={ dynamic, ArgName, ArgType }, ActName ) ->
    canonicalise_arg_spec( { dynamic, ArgName, ArgType,
                             _MaybeUserDesc=undefined }, ActName );

canonicalise_arg_spec( _ArgSpec={ dynamic, ArgName }, ActName ) ->
    canonicalise_arg_spec( { dynamic, ArgName, _ArgType=string }, ActName );

canonicalise_arg_spec( Other, ActName ) ->
    throw( { invalid_argument_spec, Other, ActName } ).



-doc """
Returns an internal, canonicalised version of the specified user result
specification of the specified action.
""".
-spec canonicalise_res_spec( term(), action_name() ) -> result_spec().
canonicalise_res_spec( ResSpec={ ResType, MaybeUserDesc }, ActName ) ->

    type_utils:is_type( ResType ) orelse
        throw( { invalid_result_type, ResType, ResSpec, ActName } ),

     MaybeBinDesc = text_utils:ensure_maybe_binary( MaybeUserDesc ),

    { ResType, MaybeBinDesc };

canonicalise_res_spec( _ResSpec=ResType, ActName ) ->
    canonicalise_res_spec( { ResType, _MaybeUserDesc=undefined }, ActName ).




-doc """
Performs the specified action, with no specific arguments, on the specified
US-Server instance, and returns the corresponding outcome.
""".
-spec perform_action( action_name(), server_pid() ) -> action_outcome().
perform_action( ActionName, ServerPid ) ->
    perform_action( ActionName, _Args=[], ServerPid ).


-doc """
Performs the specified action with the specified arguments on the specified
US-Server instance, and returns the corresponding outcome.
""".
-spec perform_action( action_name(), [ argument() ], server_pid() ) ->
                                                action_outcome().
perform_action( ActionName, Args, ServerPid ) ->

    cond_utils:if_defined( us_common_debug_actions, trace_bridge:debug_fmt(
        "Requesting the US-Server ~w to perform the action '~ts' with "
        "arguments ~p.", [ ServerPid, ActionName, Args ] ) ),

    ServerPid ! { performAction, [ ActionName, Args ], ServerPid },

    receive

        { action_outcome, Outcome } ->
            cond_utils:if_defined( us_common_debug_actions,
                trace_bridge:debug_fmt( "Outcome of action: ~p.",
                                        [ Outcome ] ) ),
            Outcome

    after 5000 ->

        trace_bridge:error_fmt( "Action '~ts' with arguments ~p timed-out." ),
        { error, timed_out }

    end.



-doc "Returns a textual description of the specified action table.".
-spec action_table_to_string( action_table() ) -> ustring().
action_table_to_string( ActTable ) ->
    case table:enumerate( ActTable ) of

        [] ->
            "no automated action defined";

        [ SingleActEntry ] ->
            text_utils:format( "a single automated action defined: ~ts",
                               [ action_entry_to_string( SingleActEntry ) ] );

        ActEntries ->
            text_utils:format( "~B automated actions defined: ~ts",
                [ length( ActEntries ), text_utils:strings_to_string(
                    [ action_entry_to_string( AE ) || AE <- ActEntries ] ) ] )

    end.



-doc "Returns a textual description of the specified action entry.".
-spec action_entry_to_string( { action_id(), action_info() } ) -> ustring().
action_entry_to_string( { ActId, #action_info{ arg_specs=ArgSpecs,
                                               result_spec=ResultSpec,
                                               mapping=Mapping,
                                               description=undefined } } ) ->
    text_utils:format( "action ~ts, ~ts, returning ~ts, mapped to ~ts",
        [ action_id_to_string( ActId ), args_to_string( ArgSpecs ),
          result_spec_to_string( ResultSpec ),
          mapping_to_string( Mapping, length( ArgSpecs ) ) ] );

action_entry_to_string( { ActId, #action_info{ arg_specs=ArgSpecs,
                                               result_spec=ResultSpec,
                                               mapping=Mapping,
                                               description=BinDescStr } } ) ->
    text_utils:format( "action ~ts, described as '~ts', ~ts, returning ~ts, "
        "mapped to ~ts",
        [ action_id_to_string( ActId ), BinDescStr,
          args_to_string( ArgSpecs ), result_spec_to_string( ResultSpec ),
          mapping_to_string( Mapping, length( ArgSpecs ) ) ] ).



-doc "Returns a textual description of the specified action identifier.".
-spec action_id_to_string( action_id() ) -> ustring().
action_id_to_string( { ActName, ActArity } ) ->
    text_utils:format( "~ts/~B", [ ActName, ActArity ] ).


-doc "Returns a textual description of the specified arguments.".
-spec args_to_string( [ arg_spec() ] ) -> ustring().
args_to_string( _ArgSpecs=[] ) ->
    "taking no argument";

args_to_string( _ArgSpecs=[ SingleArgSpec ] ) ->
    text_utils:format( "taking the following argument: ~ts",
                       [ arg_spec_to_string( SingleArgSpec ) ] );

% Arity already known:
args_to_string( ArgSpecs ) ->
    text_utils:format( "taking the following arguments: ~ts",
        [ text_utils:strings_to_enumerated_string(
            [ arg_spec_to_string( S ) || S <- ArgSpecs ],
            _IndentationLevel=1 ) ] ).



-doc "Returns a textual description of the specified argument specification.".
-spec arg_spec_to_string( arg_spec() ) -> ustring().
arg_spec_to_string( { ArgName, ArgType, _MaybeDesc=undefined } ) ->
    text_utils:format( "~ts, of type ~w", [ ArgName, ArgType ] );

arg_spec_to_string( { ArgName, ArgType, BinDescStr } ) ->
    text_utils:format( "~ts, of type ~w and described as '~ts'",
                       [ ArgName, ArgType, BinDescStr ] ).


-doc "Returns a textual description of the specified result specification.".
-spec result_spec_to_string( result_spec() ) -> ustring().
result_spec_to_string( _ResSpec={ ResType, DescStr } ) ->
    text_utils:format( "type ~w, described as '~ts'", [ ResType, DescStr ] );

result_spec_to_string( _ResSpec=ResType ) ->
    text_utils:format( "type ~w", [ ResType ] ).



-doc "Returns a textual description of the specified request mapping.".
-spec mapping_to_string( action_mapping(), arity() ) -> ustring().
mapping_to_string( { ModName, ReqName }, Arity ) ->
    % Arity increased for first parameter State:
    text_utils:format( "request ~ts:~ts/~B", [ ModName, ReqName, Arity+1 ] );

mapping_to_string( ReqName, Arity ) ->
    % Arity increased for first parameter State:
    text_utils:format( "local request ~ts/~B", [ ReqName, Arity+1 ] ).



-doc "Returns an overall help text, based on the specified actions.".
-spec help( action_table() ) -> ustring().
help( ActTable ) ->
    action_table_to_string( ActTable ).


% actions:
% alarm:
%%  * ${start_alarm_opt}: starts the alarm (siren)
%%  * ${stop_alarm_opt}: stops the alarm
%%  * ${get_alarm_opt}: tells whether the alarm is currently activated (hence wit a roaring siren)

%%  - regarding presence:
%%  * ${declare_presence_opt}: declares that somebody is at home (hence for example deactivate alarm)
%%  * ${declare_absence_opt}: declares that nobody is at home (hence for example activate alarm)
%%  * ${get_presence_opt}: tells whether US-Main considers that somebody is at home

%%  - regarding (presence-related) lighting:
%%  * ${start_lighting_opt}: starts all registered presence lighting
%%  * ${stop_lighting_opt}: stops all registered presence lighting
%% """.
