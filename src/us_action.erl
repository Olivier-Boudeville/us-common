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
Module in charge of the management of **server-side automated actions**, which
are higher-level operations that a US-Server may operate on behalf of third
parties (other servers, command-line tools, SMSs, etc.).

This allows the user to define their own actions, as data rather than as code,
as of course the code of the US-Server should not be directly impacted by user
customisation. Defining actions as data is preferred to introducing/requiring a
plugin system for that, which would be too intrusive/overkill/complex to
manage.

An example of strings triggering, typically thanks to `perform_action/{2,3}`, an
automated action: `"start_alarm"` or `"switch_on tv_plug"`.

In practice it will be executed, on the relevant US thematical server, as a
local (const or not) request, based on the `performActionFromTokens/2` request
inherited from `class_USServer`.
""".


-doc "A user-level description of an action-related element.".
-type user_description() :: any_string().

-doc "A description of an action-related element.".
-type description() :: bin_string().


-doc """
The user-level specification of an automated action.

The designated request will be executed based on the specified arguments (static
or dynamic).

Examples of such specs:

- `startAlarm`

- `{stop_room_heating, [{static, room_heater_plug, "Target device short name"},
  {static, switch_off, "Device operation"}], "void()", "switch off the heating
  of my room"}`

- `{start_tv, [{static, tv_plug, "Target device short name"}, {static,
  switch_on, "Device operation"}, {static, {next_possible_today, {19,54,0}},
  "Start time"}, {static, {1,0,0,0}, "DHMS periodicity"}], "action_outcome()",
  "switch on the television each day at 19h54, starting from today",
  schedulePeriodicalActionOnDevice}`

- `{switch_device, [{dynamic, string, "Device identifier"}, {dynamic, "atom()",
  "'on' or 'off'"}, {static, no_timeout}], switchDevice}}`
""".
-type user_action_spec() ::

    % Most complete form:
    { action_name(), [ user_arg_spec() ], user_result_spec(),
      user_description(), request_name() }

    % No request name mapping (thus the request name is expected to be the same
    % as the action one):
    %
  | { action_name(), [ user_arg_spec() ], user_result_spec(),
      user_description() }

    % Additionally, no description:
  | { action_name(), [ user_arg_spec() ], user_result_spec() }

    % Thus no expected result ('void') here:
  | { action_name(), [ user_arg_spec() ] }

    % Thus no argument expected (and returning void):
  | action_name().


-doc """
The internal specification of an action, corresponding to a request.
""".
-type action_spec() :: { action_name(), [ arg_spec() ], result_spec(),
                         option( description() ), request_name() }.


-doc """
The reference name of an action, used to trigger it.

It usually describes (as first element) the action to undertake, with a verb.

Together with the action arity, it acts, in the context of an `action_id/0`, as
a user-friendly identifier (for which no duplicates are thus allowed).

The `snake_case` is preferred for them.

For example: `start_alarm`.
""".
-type action_name() :: atom().


-doc "Tells whether an action argument is to be statically or dynamically set.".
-type arg_kind() :: 'static' | 'dynamic'.


-doc """
A statically-defined value of an actual argument (specified at action definition
time), which will be used directly as specified.

For example: `{14, top}`.
""".
-type static_value() :: term().


-doc """
The user-level specification of an action argument, either statically-defined
(i.e. at action definition time) or dynamically-supplied (i.e. at call time).

For example: `{dynamic, lengths, "[float()]", "The rod lengths, in
meters"}`.
""".
-type user_arg_spec() ::

    { 'static', static_value(), user_description() } % Statically defined
  | { 'static', static_value() } % Same with no description

  | { 'dynamic', arg_name(), text_type(),
      user_description() } % Set dynamically

  | { 'dynamic', arg_name(), text_type() } % Same with no description
  | { 'dynamic', arg_name() }. % 'string' type implied



-doc """
The internal specification of an action argument.

For example: `{dynamic, lengths, {list, {float,[]}}, <<"The rod lengths, in
meters">>}`.
""".
-type arg_spec() :: static_arg_spec() | dynamic_arg_spec().


-doc "The internal specification of a static action argument.".
-type static_arg_spec() ::
    { 'static', static_value(), option( description() ) }.


-doc "The internal specification of a dynamic action argument.".
-type dynamic_arg_spec() ::
    { 'dynamic', arg_name(), contextual_type(), option( description() ) }.


-doc """
Describes, as a name, an argument of an action.

The `snake_case` is preferred for them.

For example: `target_actuator`.
""".
-type arg_name() :: atom().



-doc """
An actual argument for an action, presumably respecting its
specification.

It corresponds to a dynamic argument in the action specification.
""".
-type argument() :: any().


-doc "The user-level specification of a result.".
-type user_result_spec() :: { text_type(), option( user_description() ) }.


-doc "The internal specification of a result.".
-type result_spec() :: { contextual_type(), option( description() ) }.



-doc """
An actual, successful (non-error) result for an action, presumably respecting
its specification.
""".
-type action_raw_result() :: any().


-doc "The description of an actual error regarding action execution.".
-type failure_report() :: ustring()
                      | 'implementation_server_not_found'
                      | 'action_not_found'
                      | 'timed_out'
                      | any(). % Typically if an exception is thrown.



-doc """
An actual result for an action, presumably respecting its specification.
""".
-type action_result() :: action_result( any() ).


-doc """
An actual result for an action, presumably respecting its specification.
""".
-type action_result( TSuccess ) :: { 'success', TSuccess }
                                 | { 'failure', failure_report() }.


-doc """
The actual outcome of a performed action.

This is an ad hoc type defined in the context of actions, based on an actual
result that is tagged in order to facilitate, on the caller side, the matching
of answers to asynchronous calls.
""".
-type action_outcome() :: action_outcome( any() ).


-doc """
The actual outcome of a performed action.

This is an ad hoc type defined in the context of actions, tagged in order to
facilitate, on the caller side, the matching of answers to asynchronous calls.
""".
-type action_outcome( TSuccess ) ::
    { 'action_outcome', action_result( TSuccess ) }.


-doc """
A table recording, for a given US-Server, all information about known, supported
actions.

Note that list_table/2 has been preferred to `table/2`, as it allows to define a
(thematical, and then defined per-action for each server) order
""".
-type action_table() :: list_table( action_id(), action_info() ).


-doc "The identifier of an action (typically in an action table).".
-type action_id() :: { action_name(), action_arity() }.


-doc """
The arity of an action is the number of its actual **dynamic** arguments.

Therefore it takes into account neither the static arguments (if any) nor the
first, `State` one of its mapped request.
""".
-type action_arity() :: arity().


% For the action_info record:
-include("us_action.hrl").


-doc """
Internal view onto an action, notably used to coerce the arguments it receives;
a value of an `action_table/0`.
""".
-type action_info() :: #action_info{}.



-doc "A token describing a dynamic argument in a call to an action.".
-type action_token() :: any_string().


-doc """
Specifies the actual WOOPER request call induced by an action.

A request is used even if not specific result is expected (`void`), for
synchronisation.
""".
-type action_request() :: { wooper:request_name(), wooper:method_arguments() }.



-export_type([ user_description/0, description/0,
               user_action_spec/0, action_spec/0,
               action_name/0,

               arg_kind/0, static_value/0,
               user_arg_spec/0,
               arg_spec/0, static_arg_spec/0, dynamic_arg_spec/0,
               arg_name/0, argument/0,
               user_result_spec/0, result_spec/0, action_raw_result/0,
               action_result/0, action_result/1,
               failure_report/0,
               action_outcome/0, action_outcome/1,
               action_table/0, action_id/0, action_arity/0, action_info/0,
               action_token/0, action_request/0 ]).


-export([ register_action_specs/3, merge_action_table/2,
          perform_action/2, perform_action/3,
          get_action_id/1, coerce_argument_tokens/2, check_result/2,
          action_id_to_string/1,
          action_table_to_string/1, action_info_to_string/1,
          action_info_to_help_string/1, action_info_to_help_string/2 ]).


% Local types:

-type text_type() :: type_utils:text_type().
-type contextual_type() :: type_utils:contextual_type().


% Type shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().

-type list_table( K, V ) :: list_table:list_table( K, V ).

-type lookup_info() :: naming_utils:lookup_info().

-type classname() :: wooper:classname().

% No request name allowed to be 'undefined'.
-type request_name() :: wooper:request_name().
-type request_result() :: wooper:request_result().

-type server_pid() :: class_USServer:server_pid().



-doc """
Registers the specified user action specifications in the specified action
table (at its tail, and in reverse order).

Note that the specified classname is used just for checking at
registration-time, and is not stored.
""".
-spec register_action_specs( [ user_action_spec() ], action_table(),
                             classname() ) -> action_table().
register_action_specs( _UserActSpecs=[], ActTable, _SrvClassname ) ->
    ActTable;

% Full spec:
register_action_specs( _UserActSpecs=[ Spec={ ActName, ArgSpecs,
        ResSpec, MaybeDesc, RequestName } | T ], ActTable,
        SrvClassname ) ->

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

    % Incremented for the State:
    check_action_mapping( RequestName, _Arity=ArgCount+1, SrvClassname ),

    ActId = { ActName, count_dynamic_arguments( CanonArgSpecs ) },

    list_table:has_entry( _K=ActId, ActTable ) andalso begin

		trace_bridge:error_fmt( "Multiple actions ~ts defined "
            "in user action specification ~p.",
            [ action_id_to_string( ActId ), Spec ] ),

        throw( { multiple_action_definitions, ActName, Spec } )

                                                  end,

    ActInfo = #action_info{ action_name=ActName,
                            arg_specs=CanonArgSpecs,
                            result_spec=CanonResSpec,
                            request_name=RequestName,
                            description=MaybeBinDesc },

    % Not in the best trace categorisation:
    cond_utils:if_defined( us_common_debug_actions, trace_bridge:debug_fmt(
        "Registering ~ts.", [ action_info_to_string( ActInfo ) ] ) ),

    NewActTable = list_table:add_entry( ActId, ActInfo, ActTable ),

    register_action_specs( T, NewActTable, SrvClassname );

% No request name, using action one:
register_action_specs(
        _UserActSpecs=[ { ActName, ArgSpecs, ResSpec, MaybeDesc } | T ],
        ActTable, SrvClassname ) ->
    register_action_specs( [ { ActName, ArgSpecs, ResSpec, MaybeDesc,
        _ReqName=ActName } | T ], ActTable, SrvClassname );

% No description:
register_action_specs( _UserActSpecs=[ { ActName, ArgSpecs, ResSpec } | T ],
                       ActTable, SrvClassname ) ->
    register_action_specs(
        [ { ActName, ArgSpecs, ResSpec, _MaybeDesc=undefined } | T ], ActTable,
        SrvClassname );

% No result type:
register_action_specs( _UserActSpecs=[ { ActName, ArgSpecs } | T ],
                       ActTable, SrvClassname ) ->
    register_action_specs( [ { ActName, ArgSpecs, _ResSpec=void } | T ],
                           ActTable, SrvClassname );

% No argument spec:
register_action_specs( _UserActSpecs=[ ActName | T ], ActTable,
                       SrvClassname ) ->
    register_action_specs( [ { ActName,  _ArgSpecs=[] } | T ], ActTable,
                           SrvClassname );

register_action_specs( Other, _ActTable, _SrvClassname ) ->
    throw( { invalid_action_specifications, not_list, Other } ).



-doc """
Registers the specified action table in the specified master one.

Preserves action order ultimately.

Any collision in terms of action identifiers is an error.
""".
-spec merge_action_table( action_table(), action_table() ) -> action_table().
merge_action_table( AddActTable, MasterActTable ) ->
    NewMasterActTable = lists:reverse( AddActTable ) ++ MasterActTable,
    list_table:check_keys_unique( NewMasterActTable ).

    % When it was still a table/2; clearer than a fold:
    %merge_action_entries( table:enumerate( AddActTable ), MasterActTable ).


% (helper)
% merge_action_entries( _ActEntries=[], ActTable ) ->
%     ActTable;

% merge_action_entries( _ActEntries=[ { ActId, ActInfo } | T ], ActTable ) ->
%     case table:lookup_entry( ActId, ActTable ) of

%         key_not_found ->
%             NewActTable = table:add_entry( _K=ActId, ActInfo, ActTable ),
%             merge_action_entries( T, NewActTable );

%         { value, V } ->
%             throw( { colliding_actions, ActId, V, ActInfo } )

%     end.



-doc "Checks that the specified term is a valid request action mapping.".
-spec check_action_mapping( term(), arity(), classname() ) -> void().
check_action_mapping( ReqName, ReqArity, SrvClassname ) ->

    wooper_class_management:check_classname( SrvClassname ),

    wooper_method_management:is_valid_request_name( ReqName ) orelse
        throw( { invalid_action_request_mapping, ReqName } ),

    ReqId = { ReqName, ReqArity },

    % Failing would be surprising, as typically called from that class:
    code_utils:is_beam_in_path( SrvClassname ) =:= not_found andalso
        throw( { action_class_not_found, SrvClassname, ReqId } ),

    % Takes inheritance into account:
    wooper_info:implements_method( SrvClassname, ReqId ) orelse
         begin

             trace_bridge:error_fmt( "No ~ts request is supported by ~ts.",
                 [ ast_info:function_id_to_string( ReqId ), SrvClassname ] ),

             throw( { action_request_not_available, { SrvClassname, ReqId } } )

         end.



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
% term() expected to be user_action_spec():
-spec canonicalise_arg_spec( term(), action_name() ) -> arg_spec().
% Full static arg spec:
canonicalise_arg_spec( _ArgSpec={ static, ArgValue, MaybeUserDesc },
                       _ActName ) ->
    MaybeBinDesc = text_utils:ensure_maybe_binary( MaybeUserDesc ),
    { static, ArgValue, MaybeBinDesc };


% No description static arg spec:
canonicalise_arg_spec( _ArgSpec={ static, ArgValue }, _ActName ) ->
    { static, ArgValue, _MaybeBinDesc=undefined };

% Full dynamic arg spec:
canonicalise_arg_spec( ArgSpec={ dynamic, ArgName, ArgTypeStr, MaybeUserDesc },
                       ActName ) ->

    is_atom( ArgName ) orelse
        throw( { invalid_argument_name, ArgName, ArgSpec, ActName } ),

    ArgCtxtType = case type_utils:parse_type( ArgTypeStr ) of

        { ok, ContextualType } ->
            ContextualType;

        { error, TaggedErrInfo } ->
            trace_bridge:error_fmt( "Invalid type in action argument spec: ~ts "
                "(argument spec: ~p, action name: ~p).",
                [ type_utils:interpret_parse_type_error( TaggedErrInfo ),
                  ArgSpec, ActName ] ),
            throw( { invalid_argument_type, ArgTypeStr, TaggedErrInfo,
                     ArgSpec, ActName } )

    end,

    MaybeBinDesc = text_utils:ensure_maybe_binary( MaybeUserDesc ),

    { dynamic, ArgName, ArgCtxtType, MaybeBinDesc };

canonicalise_arg_spec( _ArgSpec={ dynamic, ArgName, ArgTypeStr }, ActName ) ->
    canonicalise_arg_spec( { dynamic, ArgName, ArgTypeStr,
                             _MaybeUserDesc=undefined }, ActName );

canonicalise_arg_spec( _ArgSpec={ dynamic, ArgName }, ActName ) ->
    canonicalise_arg_spec( { dynamic, ArgName, _ArgTypeStr="string()" },
                           ActName );

canonicalise_arg_spec( Other, ActName ) ->
    throw( { invalid_argument_spec, Other, ActName } ).



-doc """
Returns an internal, canonicalised version of the specified user result
specification of the specified action.
""".
% term() expected to be user_result_spec():
-spec canonicalise_res_spec( term(), action_name() ) -> result_spec().
canonicalise_res_spec( ResSpec={ ResTypeStr, MaybeUserDesc }, ActName ) ->

    ResCtxtType = case type_utils:parse_type( ResTypeStr ) of

        { ok, ContextualType } ->
            ContextualType;

        { error, TaggedErrInfo } ->
            trace_bridge:error_fmt( "Invalid type in action result spec: ~ts "
                "(result spec: ~p, action name: ~p).",
                [ type_utils:interpret_parse_type_error( TaggedErrInfo ),
                  ResSpec, ActName ] ),
            throw( { invalid_argument_type, ResTypeStr, TaggedErrInfo,
                     ResSpec, ActName } )

    end,

    MaybeBinDesc = text_utils:ensure_maybe_binary( MaybeUserDesc ),

    { ResCtxtType, MaybeBinDesc };

canonicalise_res_spec( _ResSpec=ResTypeStr, ActName ) ->
    canonicalise_res_spec( { ResTypeStr, _MaybeUserDesc=undefined }, ActName ).




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

        { wooper_result, { action_outcome, Outcome } } ->
            cond_utils:if_defined( us_common_debug_actions,
                trace_bridge:debug_fmt( "Outcome of action: ~p.",
                                        [ Outcome ] ) ),
            Outcome

    after 5000 ->

        trace_bridge:error_fmt( "Action '~ts' with arguments ~p timed-out." ),
        { error, timed_out }

    end.



% Helpers to implement actions.

-doc "Returns the number of dynamic arguments among the specified arguments.".
-spec count_dynamic_arguments( [ arg_spec() ] ) -> count().
count_dynamic_arguments( ArgSpecs ) ->
    length( [ DynArgSpec || DynArgSpec={ dynamic, _ArgName, _ArgTypeStr,
                                         _MaybeUserDesc } <- ArgSpecs ] ).


-doc """
Returns the action identifier corresponding to the token-based specified
command.
""".
-spec get_action_id( [ action_token() ] ) -> action_id().
get_action_id( _Tokens=[ ActionNameBinStr | Args ] ) ->
    ActName = text_utils:binary_to_atom( ActionNameBinStr ),
    ActArity = length( Args ),
    { ActName, ActArity };

get_action_id( Other ) ->
    throw( { invalid_action_tokens, Other } ).



-doc """
Tries to coerce any arguments obtained from the specified tokens into the
expected ones, as they were specified, and to produce the expected final list of
actual arguments.

Throws an exception on failure.
""".
-spec coerce_argument_tokens( [ action_token() ], [ arg_spec() ] ) ->
                                            [ argument() ].
% First token is the action name, the next ones are the arguments:
coerce_argument_tokens( _Tokens=[ _ActName ], _ArgSpecs=[] ) ->
    [];

coerce_argument_tokens( _Tokens=[ _ActName ], _ArgSpecs ) ->
    [];

coerce_argument_tokens( ArgsTokens, ArgSpecs ) ->
    coerce_argument_tokens( ArgsTokens, ArgSpecs, _Args=[] ).


% (helper)
coerce_argument_tokens( _ArgsTokens=[], _ArgSpecs=[], Args ) ->
    lists:reverse( Args );

% Insert literally, in-place, any static argument:
coerce_argument_tokens( ArgsTokens,
                        _ArgSpecs=[ { static, Arg, _MaybeBinDesc } | T ],
                        Args ) ->
    coerce_argument_tokens( ArgsTokens, T, [ Arg | Args ] );


% Associate a dynamic argument to its type:
coerce_argument_tokens( _ArgsTokens=[ ArgToken | TTokens ],
        _ArgSpecs=[ { dynamic, _ArgName, ArgType, _MaybeBinDesc } | TArgSpecs ],
        Args ) ->
    ActualArg = type_utils:coerce_string_to_term( ArgToken, ArgType ),
    coerce_argument_tokens( TTokens, TArgSpecs, [ ActualArg | Args ] ).



-doc """
Checks that the specified request result matches the specified action result
specification.
""".
-spec check_result( request_result(), result_spec() ) -> void().
% Outcome sent back, no available type to further check:
check_result( _Res={ action_outcome, _ActualRes={ OutcomeTag, _OutcomeRes } },
              _ResSpec={ _Type=action_outcome, _MaybeBinDesc } )
                    when OutcomeTag =:= success orelse OutcomeTag =:= failure ->
    ok;

check_result( Res, _ResSpec={ _Type=action_outcome, MaybeBinDesc } ) ->
    throw( { invalid_action_outcome, Res, MaybeBinDesc } );

% Here we have a type to check against:
check_result( Res, _ResSpec={ CtxtType, MaybeBinDesc } ) ->
    type_utils:is_of_type( Res, CtxtType ) orelse
        throw( { invalid_action_result_type, Res, CtxtType, MaybeBinDesc } ).



-doc "Returns a textual description of the specified action identifier.".
-spec action_id_to_string( action_id() ) -> ustring().
action_id_to_string( _ActionInfo={ ActName, ActArity } ) ->
    text_utils:format( "~ts/~B", [ ActName, ActArity ] ).



-doc "Returns a textual description of the specified action information.".
-spec action_info_to_string( action_info() ) -> ustring().
action_info_to_string( #action_info{ server_lookup_info=SrvLookupInfo,
                                     action_name=ActName,
                                     arg_specs=ArgSpecs,
                                     result_spec=ResultSpec,
                                     request_name=ReqName,
                                     description=undefined } ) ->
    ArgCount = length( ArgSpecs ),
    text_utils:format( "action ~ts/~B, ~ts and returning ~ts, ~ts, "
        "mapped to ~ts",
        [ ActName, ArgCount, args_to_string( ArgSpecs ),
          result_spec_to_string( ResultSpec ), get_impl_string( SrvLookupInfo ),
          mapping_to_string( ReqName, ArgCount ) ] );

action_info_to_string( #action_info{ server_lookup_info=SrvLookupInfo,
                                     action_name=ActName,
                                     arg_specs=ArgSpecs,
                                     result_spec=ResultSpec,
                                     request_name=ReqName,
                                     description=BinDescStr } ) ->
    ArgCount = length( ArgSpecs ),
    text_utils:format( "action ~ts/~B, described as '~ts', ~ts "
        "and returning ~ts, ~ts, mapped to ~ts",
        [ ActName, ArgCount, BinDescStr, args_to_string( ArgSpecs ),
          result_spec_to_string( ResultSpec ), get_impl_string( SrvLookupInfo ),
          mapping_to_string( ReqName, ArgCount ) ] ).



% (helper)
-spec get_impl_string( option( lookup_info() ) ) -> ustring().
get_impl_string( _MaybeImplSrvLookupInfo=undefined ) ->
    "directly implemented by this server";

get_impl_string( SrvLookupInfo ) ->
    text_utils:format( "implemented by server referenced by a ~ts",
        [ naming_utils:lookup_info_to_string( SrvLookupInfo ) ] ).



-doc "Returns a textual description of the specified action table.".
-spec action_table_to_string( action_table() ) -> ustring().
action_table_to_string( ActTable ) ->
    case list_table:values( ActTable ) of

        [] ->
            "no automated action defined";

        [ SingleActInfo ] ->
            text_utils:format( "a single automated action defined: ~ts",
                               [ action_info_to_string( SingleActInfo ) ] );

        ActInfos ->
            text_utils:format( "~B automated actions defined: ~ts",
                [ length( ActInfos ), text_utils:strings_to_string(
                    [ action_info_to_string( AI ) || AI <- ActInfos ] ) ] )

    end.



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
arg_spec_to_string( { static, ArgValue, _MaybeDesc=undefined } ) ->
    text_utils:format( "static argument of value '~p'", [ ArgValue ] );

arg_spec_to_string( { static, ArgValue, BinDesc } ) ->
    text_utils:format( "static argument of value '~p', described as '~ts'",
                       [ ArgValue, BinDesc ] );

arg_spec_to_string( { dynamic, ArgName, ArgCtxtType, _MaybeDesc=undefined } ) ->
    text_utils:format( "dynamic argument named '~ts', of contextual type ~ts",
                       [ ArgName, type_utils:type_to_string( ArgCtxtType ) ] );

arg_spec_to_string( { dynamic, ArgName, ArgCtxtType, BinDesc } ) ->
    text_utils:format( "dynamic argument named '~ts', of contextual type ~ts, "
        "described as '~ts'",
        [ ArgName, type_utils:type_to_string( ArgCtxtType ), BinDesc ] ).



-doc "Returns a textual description of the specified result specification.".
-spec result_spec_to_string( result_spec() ) -> ustring().
result_spec_to_string(
        _ResSpec={ action_outcome, _MaybeDescBinStr=undefined } ) ->
    "an action outcome";

result_spec_to_string( _ResSpec={ ResCtxtType, _MaybeDescBinStr=undefined } ) ->
    text_utils:format( "a value of type ~ts",
                       [ type_utils:type_to_string( ResCtxtType ) ] );

result_spec_to_string( _ResSpec={ action_outcome, DescBinStr } ) ->
    text_utils:format( "an action outcome, described as '~ts'",
                       [ DescBinStr ] );

result_spec_to_string( _ResSpec={ ResCtxtType, DescBinStr } ) ->
    text_utils:format( "a value of type ~ts, described as '~ts'",
        [ type_utils:type_to_string( ResCtxtType ), DescBinStr ] ).



-doc "Returns a textual description of the specified request mapping.".
-spec mapping_to_string( request_name(), arity() ) -> ustring().
mapping_to_string( ReqName, Arity ) ->
    % Arity increased for first parameter State:
    text_utils:format( "request ~ts/~B", [ ReqName, Arity+1 ] ).



-doc "Returns an overall help text, based on the specified actions.".
-spec action_info_to_help_string( action_table(), ustring() ) -> ustring().
action_info_to_help_string( ActTable, AppName ) ->
    % We ensured that the native order of these entries is the intended one:
   case list_table:values( ActTable ) of

        [] ->
            text_utils:format( "This ~ts application does not support "
                "any specific automated action.", [ AppName ] );

        [ SingleActInfo ] ->
            text_utils:format( "This ~ts application supports a single "
                "automated action: ~ts",
                [ AppName, action_info_to_help_string( SingleActInfo ) ] );

        ActInfos ->
            text_utils:format( "This ~ts application supports ~B automated "
                "actions: ~ts", [ AppName, length( ActInfos ),
                    text_utils:strings_to_string( [ action_info_to_help_string(
                        AI ) || AI <- ActInfos ] ) ] )

    end.



-doc """
Returns a textual usage help deriving from the specified action information.
""".
-spec action_info_to_help_string( action_info() ) -> ustring().
action_info_to_help_string( #action_info{ action_name=ActName,
                                          arg_specs=ArgSpecs,
                                          %result_spec=ResultSpec,
                                          description=undefined } ) ->

    % Shorter is better:
    ArgStr = case describe_dynamic_arguments( ArgSpecs ) of

        undefined ->
            "";

        DynStr ->
           ", expecting " ++ DynStr

    end,


    % Too verbose: "and returning ~ts" / result_spec_to_string( ResultSpec )
    text_utils:format( "'~ts'~ts", [ ActName, ArgStr ] );

action_info_to_help_string( #action_info{ action_name=ActName,
                                          arg_specs=ArgSpecs,
                                          %result_spec=ResultSpec,
                                          description=BinDesc } ) ->
    % Shorter is better:
    ArgStr = case describe_dynamic_arguments( ArgSpecs ) of

       undefined ->
            "";

        DynStr ->
           "; expects " ++ DynStr

    end,

    % Too verbose: "and returning ~ts" / result_spec_to_string( ResultSpec )
    text_utils:format( "'~ts': ~ts~ts", [ ActName, BinDesc, ArgStr ] ).



-doc "Describes any dynamic arguments among the specified action arguments.".
-spec describe_dynamic_arguments( arg_spec() ) -> option( ustring() ).
describe_dynamic_arguments( ArgSpecs ) ->

    % Filter out static arguments:
    DynamicArgSpec = [ DArgS
        || DArgS={ dynamic, _ArgName, _ArgCtxtType, _BinDesc } <- ArgSpecs ],

    case DynamicArgSpec of

        [] ->
            %"no user-supplied argument";
            % Better (shorter/clearer):
            undefined;

        [ SingleDArgSpec ] ->
            text_utils:format( "a single user-supplied argument: ~ts",
                               [ arg_spec_to_help_string( SingleDArgSpec ) ] );

        DArgSpecs ->
            text_utils:format( "~B user-supplied arguments: ~ts",
                [ length( DArgSpecs ), text_utils:strings_to_enumerated_string(
                    [ arg_spec_to_help_string( S ) || S <- DArgSpecs ],
                    _IndentationLevel=1 ) ] )

    end.


-doc "Returns a textual help for the specified dynamic argument specification.".
-spec arg_spec_to_help_string( dynamic_arg_spec() ) -> ustring().
arg_spec_to_help_string(
        { dynamic, ArgName, CtxtType, _MaybeBinDesc=undefined } ) ->
    text_utils:format( "'~ts' of type ~ts",
                       [ ArgName, type_utils:type_to_string( CtxtType )] );

arg_spec_to_help_string( { dynamic, ArgName, CtxtType, BinDesc } ) ->
    text_utils:format( "~ts: '~ts' of type ~ts",
        [ BinDesc, ArgName, type_utils:type_to_string( CtxtType ) ] ).
