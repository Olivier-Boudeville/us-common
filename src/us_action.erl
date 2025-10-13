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

If no result spec is defined, the `unchecked_result` default will apply: the
type of the result returned by the implementation request will not be checked
against a user-specified one.

Examples of such user action specs:

- `startAlarm` (yet we recommend defining a user-targeted, snake-cased, specific
  action name)

- `{stop_room_heating, "switch off the heating of my room", actOnDevice,
  [{static, room_heater_plug, "target device short name"}, {static, switch_off,
  "Device operation"}]}`

- `{start_tv, "switch on the television each day at 19h54, starting from today",
  schedulePeriodicalActionOnDevice, [{static, tv_plug, "target device short
  name"}, {static, switch_on, "Device operation"}, {static,
  {next_possible_today, {19,54,0}}, "Start time"}, {static, {1,0,0,0}, "DHMS
  periodicity"}]}`

- `{switch_device, undefined, switchDevice, [{dynamic, string, "Device
  identifier"}, {dynamic, "atom()", "'on' or 'off'"}, {static, no_timeout}],
  "boolean()"}`
""".
-type user_action_spec() ::

    % Most complete form:
    { action_name(), option( user_description() ),
      request_name(), [ user_arg_spec() ], user_result_spec() }

    % Nnote that the last element of the tuple is removed at each next new type:

    % No result spec specified, so the 'unchecked_result' policy will apply:
  | { action_name(), option( user_description() ),
      request_name(), [ user_arg_spec() ] }


    % No request name mapping (thus the request name is expected to be the same
    % as the action one, like `stop`):
    %
  | { action_name(), option( user_description() ), request_name() }

    % Additionally, no argument expected:
  | { action_name(), option( user_description() ) }

    % Thus not even a description:
  | action_name().



-doc """
A user-defined header text describing a thematical set of actions.

A header corresponds at least generally to a given topic (e.g. "security
management").

Only of use for user interaction.
""".
-type user_action_header() :: any_string().



-doc """
A user-defined, internal header text describing a thematical set of actions (a
topic).
""".
-type action_header() :: bin_string().


-doc "A list of user action specs, possibly with (non-nested) header texts.".
-type user_action_specs() ::
    [ user_action_spec() | { user_action_header(), [ user_action_spec() ] } ].


-doc """
The internal, canonical specification of an action, branching to its
implementation request.
""".
-type action_spec() :: { action_name(), option( description() ), request_name(),
                         [ arg_spec() ], option( result_spec() ) }.


-doc """
The reference name of an action, used to trigger it.

It usually describes (as first element) the action to undertake, with a verb.

It might be convenient to retain the shortest action names with as little common
prefixes as possible (e.g. avoiding generalising too much `start-` and `stop-`
prefixes, so that they get automatically abbreviated with shorter prefixes.

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


-doc """
The user-level specification of a result.

The textual type corresponds to the returned type of the corresponding
implementation request.

If controlling the return type, we recommend relying on fallible-like types (for
example `ReturnT :: basic_utils:fallible/2`, or `successful/1`).

Mildy interesting to specify, as type-checking the result of an implementation
request may have little interest.
""".
-type user_result_spec() ::
    { text_type(), option( user_description() ) }
  | text_type()
  | 'unchecked_result'. % Default: do not typecheck result



-doc """
The internal specification of a result.

The (contextual) type corresponds to the returned type of the corresponding
implementation request (for example `ReturnT :: basic_utils:fallible/2`).

If controlling the return type, we recommend relying on fallible-like types (for
example `ReturnT :: basic_utils:fallible/2`, or `successful/1`).
""".
-type result_spec() :: option( { contextual_type(), option( description() ) } ).



-doc """
The description of an error triggered when trying to execute an action.

This describes a failure at the level of the action system; this is not a term
to be directly returned by an implementation request.
""".
-type failure_report() ::
    { 'implementation_server_not_found', lookup_info() }
  | 'action_not_found'
  | 'timed_out'
  | { 'wrong_argument_count', DiagStr :: ustring() }
  | 'invalid_result_type'
  | { 'exception_thrown', term() }. % When executing said request




-doc """
The actual outcome of a (locally) performed action.

This is an ad hoc type defined in the context of actions, based on an actual
result that is tagged in order to facilitate, on the caller side, the matching
of answers to asynchronous calls.

An action being done (and thus not failed) means that its implementation request
could be executed as intended - this does not imply that it reports any success;
indeed the implementation code may have reported (its own way) an error. Yet in
both cases its actual result shall be of the `ReturnT` type, which shall be the
one reported by that request (typically with `request_return(ReturnT)`).

An action being reported as failed means that the corresponding request was not
even executed.

Often, when possible (i.e. when they are defined on purpose), the implementation
requests return actual results of the `ReturnT :: basic_utils:fallible/2` type,
for an easier error management.
""".
-type action_outcome( ReturnT ) :: { 'action_done', ReturnT }
                                 | { 'action_failed', failure_report() }.


-doc """
The actual outcome of a performed action.

This is an ad hoc type defined in the context of actions, based on an actual
result that is tagged in order to facilitate, on the caller side, the matching
of answers to asynchronous calls.

Here no specific typing information regarding to the possible actual results is
specified.
""".
-type action_outcome() :: action_outcome( request_result() ).



-doc """
A table recording, for a given US-Server, all information about known, supported
actions.

Their order/sorting is determined solely by the header table.
""".
-type action_table() :: table( action_id(), action_info() ).


-doc "The identifier of an action (typically in an action table).".
-type action_id() :: { action_name(), action_arity() }.


-doc """
Stores (ordered) headers and the (ordered) actions that each federates, through
their identifiers (which correspond to the keys of the action table).

Actions that are declared within no specific header are listed separately.
""".
-type header_info() ::
    % A list_table, to preserve order in headers:
    { HdTable :: list_table( action_header(), [ action_id() ] ),
      HeaderLessActIds :: [ action_id() ] }.



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
               user_action_header/0, action_header/0,
               user_action_spec/0, user_action_specs/0,
               action_spec/0, action_name/0,

               arg_kind/0, static_value/0,
               user_arg_spec/0,
               arg_spec/0, static_arg_spec/0, dynamic_arg_spec/0,
               arg_name/0, argument/0,
               user_result_spec/0, result_spec/0,
               failure_report/0, action_outcome/0, action_outcome/1,
               action_table/0, header_info/0,
               action_id/0, action_arity/0, action_info/0,
               action_token/0, action_request/0 ]).


-export([ register_action_specs/4, init_header_info/0, merge_action_tables/4,
          perform_action/2, perform_action/3,
          get_action_id/1, coerce_token_arguments/3, is_result_matching_spec/2,
          action_id_to_string/1,
          action_table_to_string/1, action_tables_to_string/2,
          header_to_string/3,
          args_to_string/1, arg_spec_to_string/1,
          result_spec_to_string/1, mapping_to_string/2,
          action_info_to_string/1,
          action_tables_to_help_string/3, headerless_actions_to_help_string/2,
          action_info_to_help_string/1 ]).


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
table (at its tail, and in reverse order) and header information.

Note that the specified classname is just used for checking at
registration-time, and is not stored.
""".
-spec register_action_specs( [ user_action_spec() ], action_table(),
    header_info(), classname() ) -> { action_table(), header_info() }.
register_action_specs( _UserActSpecs=[], ActTable, HdInfo, _SrvClassname ) ->
    { ActTable, HdInfo };


% Here actions with an header:
register_action_specs( _UserActSpecs=[ { UAHAnyStr, UASs } | T ], ActTable,
        _HdInfo={ HdTable, HdLessActIds }, SrvClassname ) when
            ( is_list( UAHAnyStr ) orelse is_binary( UAHAnyStr ) )
            andalso is_list( UASs ) -> % (actual list)

    { ActIds, NewActTable } = lists:foldl(
        fun( UAS, _Acc={ AccIds, AccActTable } ) ->
            { ActId, NewAccAccTable } = register_action_spec( UAS,
                AccActTable, SrvClassname ),
            { [ ActId | AccIds ], NewAccAccTable }
        end,
        _Acc0={ _AccIds0=[], ActTable },
        _List=UASs ),

    HdBinStr = text_utils:ensure_binary( UAHAnyStr ),

    % Creates that entry if not already existing:
    NewHdInfo = { list_table:append_list_to_entry( _K=HdBinStr, _Elem=ActIds,
                                                   HdTable ), HdLessActIds },

    register_action_specs( T, NewActTable, NewHdInfo, SrvClassname );


% No header, just a standalone action:
register_action_specs( _UserActSpecs=[ UAS | T ], ActTable,
                       _HdInfo={ HdTable, HdLessActIds }, SrvClassname ) ->

    { ActId, NewActTable } =
        register_action_spec( UAS, ActTable, SrvClassname ),

    NewHdInfo = { HdTable,
                  list_utils:append_at_end( _Elem=ActId, HdLessActIds ) },

    register_action_specs( T, NewActTable, NewHdInfo, SrvClassname ).



-doc """
Registers the specified user action specification in the specified action table.

Note that the specified classname is used just for checking at
registration-time, and is not stored.
""".
-spec register_action_spec( user_action_spec(), action_table(), classname() ) ->
                                { action_id(), action_table() }.
% Full spec:
register_action_spec( UserActSpec={ ActName, MaybeDesc, RequestName,
        ArgSpecs, ResSpec }, ActTable, SrvClassname ) ->

    is_atom( ActName ) orelse begin

        trace_bridge:error_fmt( "Invalid action name ('~p') "
            "in user action specification ~p (not an atom).",
            [ ActName, UserActSpec ] ),

       throw( { invalid_action_name, ActName, UserActSpec } )

                              end,

    MaybeBinDesc = text_utils:ensure_maybe_binary( MaybeDesc ),

    CanonArgSpecs = canonicalise_arg_specs( ArgSpecs, ActName ),

    CanonResSpec = canonicalise_result_spec( ResSpec, ActName ),

    ArgCount = length( CanonArgSpecs ),

    % Incremented for the State:
    check_action_mapping( RequestName, _Arity=ArgCount+1, SrvClassname ),

    ActId = { ActName, count_dynamic_arguments( CanonArgSpecs ) },

    table:has_entry( _K=ActId, ActTable ) andalso begin

        trace_bridge:error_fmt( "Multiple actions ~ts defined "
            "in user action specification ~p.",
            [ action_id_to_string( ActId ), UserActSpec ] ),

        throw( { multiple_action_definitions, ActName, UserActSpec } )

                                                  end,

    ActInfo = #action_info{ action_name=ActName,
                            arg_specs=CanonArgSpecs,
                            result_spec=CanonResSpec,
                            request_name=RequestName,
                            description=MaybeBinDesc },

    % Not in the best trace categorisation:
    cond_utils:if_defined( us_common_debug_actions, trace_bridge:debug_fmt(
        "Registering ~ts.", [ action_info_to_string( ActInfo ) ] ) ),

    NewActTable = table:add_entry( ActId, ActInfo, ActTable ),

    { ActId, NewActTable };

% No request spec specified, so the 'unchecked_result' policy will apply:
register_action_spec(
        _UserActSpec={ ActName, MaybeDesc, RequestName, ArgSpecs },
        ActTable, SrvClassname ) ->
    register_action_spec( { ActName, MaybeDesc, RequestName, ArgSpecs,
        _UsrResSpec=unchecked_result }, ActTable, SrvClassname );

% No argument spec:
register_action_spec( _UserActSpec={ ActName, MaybeDesc, ReqName },
                       ActTable, SrvClassname ) ->
    register_action_spec( { ActName, MaybeDesc, ReqName, _ArgSpecs=[] },
                          ActTable, SrvClassname );

% No request name, using the action one:
register_action_spec( _UserActSpec={ ActName, MaybeDesc }, ActTable,
                      SrvClassname ) ->
    register_action_spec( { ActName, MaybeDesc, _ReqName=ActName }, ActTable,
                          SrvClassname );

% Not even a description:
register_action_spec( _UserActSpec=ActName, ActTable, SrvClassname ) ->
    register_action_spec( { ActName, _MaybeDesc=undefined }, ActTable,
                           SrvClassname ).

% (no catch-all error clause useful here)



-doc "Returns an initialised header table.".
-spec init_header_info() -> header_info().
init_header_info() ->
    { list_table:new(), _HeaderLessActIds=[] }.



-doc """
Registers the specified action and header tables in the specified master ones.

Preserves action order ultimately.

Any collision in terms of action identifiers is an error.
""".
-spec merge_action_tables( action_table(), header_info(),
        action_table(), header_info() ) -> { action_table(), header_info() }.
merge_action_tables( AddActTable, _AddHdInfo={ AddHdTable, AddHdLessActIds },
        MasterActTable, _MasterHdInfo={ MasterHdTable, MasterHdLessActIds } ) ->

    NewMasterActTable = table:merge_unique( AddActTable, MasterActTable ),

    % Swapped order to respect header orders:
    MergedHdTable = list_table:merge_unique( MasterHdTable, AddHdTable ),

    MergedHdLessActIds = MasterHdLessActIds ++ AddHdLessActIds,

    { NewMasterActTable, _NewHdInfo={ MergedHdTable, MergedHdLessActIds } }.




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

             trace_bridge:error_fmt( "No ~ts request is supported by ~ts, "
                 "whereas it is designated as the implementation of an action.",
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
-spec canonicalise_result_spec( term(), action_name() ) ->
                                            option( result_spec() ).
canonicalise_result_spec( _ResSpec=unchecked_result, _ActName ) ->
    undefined;

canonicalise_result_spec( ResSpec={ ResTypeStr, MaybeUserDesc }, ActName ) ->

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

canonicalise_result_spec( _ResSpec=ResTypeStr, ActName ) ->
    canonicalise_result_spec( { ResTypeStr, _MaybeUserDesc=undefined },
                              ActName ).




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
-spec get_action_id( [ action_token() ] ) -> fallible( action_id() ).
get_action_id( _Tokens=[ ActionNameBinStr | Args ] ) ->
    ActName = text_utils:binary_to_atom( ActionNameBinStr ),
    ActArity = length( Args ),
    { ok, _ActId={ ActName, ActArity } };

get_action_id( Other ) ->
    { error, { invalid_action_tokens, Other } }.



-doc """
Tries to coerce any arguments obtained from the specified tokens into the
expected ones, as they were specified, and to produce the expected final list of
actual arguments.

Throws an exception on failure.
""".
-spec coerce_token_arguments( [ action_token() ], [ arg_spec() ],
        action_name() ) -> fallible( [ argument() ], ustring() ).
coerce_token_arguments( ArgTokens, ArgSpecs, ActName ) ->
    coerce_token_arguments( ArgTokens, ArgSpecs, ActName, _Args=[] ).


% (helper)
coerce_token_arguments( _ArgsTokens=[], _ArgSpecs=[], _ActName, Args ) ->
    { ok, lists:reverse( Args ) };

coerce_token_arguments( ExtraArgsTokens, _ArgSpecs=[], ActName, _Args ) ->

    DiagStr = text_utils:format( "~B extraneous argument(s) received "
        "for action '~ts':~n ~p",
        [ length( ExtraArgsTokens ), ActName, ExtraArgsTokens ] ),

    { error, DiagStr };


% Insert literally, in-place, any static argument:
coerce_token_arguments( ArgsTokens,
                        _ArgSpecs=[ { static, Arg, _MaybeBinDesc } | T ],
                        ActName, Args ) ->
    coerce_token_arguments( ArgsTokens, T, ActName, [ Arg | Args ] );


% Associate a dynamic argument to its type:
coerce_token_arguments( _ArgsTokens=[ ArgToken | TTokens ],
        _ArgSpecs=[ { dynamic, ArgName, ArgType, _MaybeBinDesc } | TArgSpecs ],
        ActName, Args ) ->

    case type_utils:coerce_stringified_to_type( ArgToken, ArgType ) of

        { ok, ActualArg } ->
            coerce_token_arguments( TTokens, TArgSpecs, ActName,
                                    [ ActualArg | Args ] );

        { error, Reason } ->
            { error, { argument_coercing_failed, ArgName, Reason, ActName } }

    end;

coerce_token_arguments( _ArgsTokens=[], ArgSpecs, ActName, _Args ) ->

    DiagStr = text_utils:format( "~B argument(s) lacking for action '~ts'.",
                                 [ length( ArgSpecs ), ActName ] ),

    { error, DiagStr }.




-doc """
Tells whether the specified result conforms to the specified result
specification.
""".
-spec is_result_matching_spec( request_result(), result_spec() ) -> boolean().
is_result_matching_spec( _Res, _ResSpec=undefined ) ->
   true;

is_result_matching_spec( Res, _ResSpec={ CtxtType, _MaybeBinDesc } ) ->
   type_utils:is_of_type( Res, CtxtType ).



-doc "Returns a textual description of the specified action identifier.".
-spec action_id_to_string( action_id() ) -> ustring().
action_id_to_string( _ActionInfo={ ActName, ActArity } ) ->
    text_utils:format( "~ts/~B", [ ActName, ActArity ] ).



-doc "Returns a textual description of the specified action information.".
-spec action_info_to_string( action_info() ) -> ustring().
action_info_to_string( #action_info{ server_lookup_info=SrvLookupInfo,
                                     action_name=ActName,
                                     splitter=Splitter,
                                     arg_specs=ArgSpecs,
                                     result_spec=ResultSpec,
                                     request_name=ReqName,
                                     description=undefined } ) ->
    ArgCount = length( ArgSpecs ),
    text_utils:format( "action ~ts/~B (splitter: ~p), ~ts and "
        "returning ~ts, ~ts, mapped to ~ts",
        [ ActName, ArgCount, Splitter, args_to_string( ArgSpecs ),
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
    case table:values( ActTable ) of

        [] ->
            "no automated action is defined";

        [ SingleActInfo ] ->
            text_utils:format( "a single automated action is defined: ~ts",
                               [ action_info_to_string( SingleActInfo ) ] );

        ActInfos ->
            text_utils:format( "~B automated actions are defined: ~ts",
                [ length( ActInfos ), text_utils:strings_to_string(
                    [ action_info_to_string( AI )
                        % Misleading: || AI <- lists:sort( ActInfos ) ] ) ] )
                        || AI <- ActInfos ] ) ] )

    end.



-doc "Returns a textual description of the specified action-related tables.".
-spec action_tables_to_string( header_info(), action_table() ) -> ustring().
action_tables_to_string( _HdInfo={ HdTable, _HeaderLessActIds=[] },
                         ActTable ) ->
    text_utils:format( "~B headers defined: ~ts~n; and ~ts",
        [ list_table:size( HdTable ), text_utils:strings_to_string( [
            header_to_string( HdBinStr, ActIds, ActTable )
                || { HdBinStr, ActIds } <- list_table:enumerate( HdTable ) ] ),
          action_table_to_string( ActTable ) ] );

action_tables_to_string( _HdInfo={ HdTable, HeaderLessActIds }, ActTable ) ->
    text_utils:format( "~B headers defined: ~ts~n"
        "with ~B header-less actions: ~w; and ~ts",
        [ list_table:size( HdTable ), text_utils:strings_to_string( [
            header_to_string( HdBinStr, ActIds, ActTable )
                || { HdBinStr, ActIds } <- list_table:enumerate( HdTable ) ] ),
          length( HeaderLessActIds ), HeaderLessActIds,
          action_table_to_string( ActTable ) ] ).



-doc """
Returns a textual description of the specified actions set in the specified
header.
""".
-spec header_to_string( action_header(), [ action_id() ], action_table() ) ->
                                                    ustring().
header_to_string( ActHeaderBinStr, ActIds, ActTable ) ->
     text_utils:format( "header '~ts' defines ~B actions: ~ts",
         [ ActHeaderBinStr, length( ActIds ), text_utils:strings_to_string( [
            action_info_to_string( table:get_value( ActId, ActTable ) )
                %|| ActId <- lists:sort( ActIds ) ] ) ] ).
                || ActId <- ActIds ] ) ] ).



-doc """
Returns a textual description of the specified actions set in the specified
header.
""".
-spec header_to_help_string( action_header(), [ action_id() ],
                             action_table() ) -> ustring().
header_to_help_string( ActHeaderBinStr, ActIds, ActTable ) ->
     text_utils:format( "'~ts', with ~B actions: ~ts",
         [ ActHeaderBinStr, length( ActIds ), text_utils:strings_to_string( [
            action_info_to_string( table:get_value( ActId, ActTable ) )
                || ActId <- ActIds ] ) ] ).




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
result_spec_to_string( _ResSpec=undefined ) ->
    "a value whose type will not be checked";

result_spec_to_string( _ResSpec={ ResCtxtType, _MaybeDescBinStr=undefined } ) ->
    text_utils:format( "a value of type ~ts",
                       [ type_utils:type_to_string( ResCtxtType ) ] );

result_spec_to_string( _ResSpec={ ResCtxtType, DescBinStr } ) ->
    text_utils:format( "a value of type ~ts, described as '~ts'",
        [ type_utils:type_to_string( ResCtxtType ), DescBinStr ] ).



-doc "Returns a textual description of the specified request mapping.".
-spec mapping_to_string( request_name(), arity() ) -> ustring().
mapping_to_string( ReqName, Arity ) ->
    % Arity increased for first parameter State:
    text_utils:format( "request ~ts/~B", [ ReqName, Arity+1 ] ).



-doc "Returns an overall help text, based on the specified action information.".
-spec action_tables_to_help_string( action_table(), header_info(),
                                    ustring() ) -> ustring().
action_tables_to_help_string( ActTable, _HdInfo={ HdTable, HeaderLessActIds },
                              AppName ) ->

    case table:enumerate( HdTable ) of

        % No actual header, so just headerless actions:
        [] ->
            % "enumerate" (no-op):
            case list_utils:enumerate( HeaderLessActIds ) of

                [] ->
                    text_utils:format( "This ~ts application does not support "
                        "any specific automated action.", [ AppName ] );

                [ SingleActId ] ->
                    SingleActInfo =
                        table:get_value( _K=SingleActId, ActTable ),

                    text_utils:format( "This ~ts application supports a single "
                        "automated action: ~ts ~ts", [ AppName,
                            action_info_to_help_string( SingleActInfo ),
                            explain_splitter() ] );

                ActInfos ->
                    text_utils:format( "This ~ts application supports ~B "
                        "automated actions: ~ts~n~ts",
                        [ AppName, length( ActInfos ),
                          text_utils:strings_to_string(
                              [ action_info_to_help_string( AI )
                                || AI <- ActInfos ] ), explain_splitter() ] )

                end;


        % Single header:
        [ { HdBinStr, ActIds } ] ->
            text_utils:format( "This ~ts application manages a single topic: "
                "~ts~n~ts", [ AppName,
                    header_to_help_string( HdBinStr, ActIds, ActTable ),
                    headerless_actions_to_help_string( HeaderLessActIds,
                                                       ActTable ) ] );

        HdActIdsPairs ->
            text_utils:format( "This ~ts application manages ~B topics: "
                "~ts~ts",
                [ AppName, length( HdActIdsPairs ),
                  text_utils:strings_to_string(
                      [ header_to_help_string( Hd, Ids, ActTable )
                            || { Hd, Ids } <- HdActIdsPairs ] ),
                  headerless_actions_to_help_string( HeaderLessActIds,
                                                     ActTable ) ] )

    end.


explain_splitter() ->
    "(a pipe character in a name denotes the possibility of abbreviating "
    "this name by not specifying the characters on the right of that pipe)".



-doc "Returns a textual description of the specified headerless actions.".
-spec headerless_actions_to_help_string( [ action_id() ], action_table() ) ->
                                                ustring().
headerless_actions_to_help_string( _ActIds=[], _ActTable ) ->
    "";

headerless_actions_to_help_string( _ActIds=[ ActId ], ActTable ) ->
    text_utils:format( "A single action belongs to no specific topic: ~ts",
        [ action_info_to_help_string(
            list_table:get_value( ActId, ActTable ) ) ] );

headerless_actions_to_help_string( ActIds, ActTable ) ->
    text_utils:format( "~B actions belong to no specific topic:~ts",
        [ length( ActIds ),
          [ text_utils:strings_to_string( action_info_to_help_string(
                list_table:get_value( ActId, ActTable ) ) )
                    || ActId <- ActIds ] ] ).



-doc """
Returns a textual usage help deriving from the specified action information.
""".
-spec action_info_to_help_string( action_info() ) -> ustring().
% No description here:
action_info_to_help_string( #action_info{ splitter=ActSplitter,
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
    text_utils:format( "'~ts'~ts",
        [ spell_tree:splitter_to_string( ActSplitter ), ArgStr ] );

action_info_to_help_string( #action_info{ splitter=ActSplitter,
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
    text_utils:format( "'~ts': ~ts~ts",
        [ spell_tree:splitter_to_string( ActSplitter ), BinDesc, ArgStr ] ).



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
