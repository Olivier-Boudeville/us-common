% Copyright (C) 2024-2026 Olivier Boudeville
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
% Creation date: Tuesday, December 31, 2024.

-module(us_client).

-moduledoc """
Gathering of code for **clients of all kinds of US servers**.

Typically used by the monitor and control scripts.

Designed to help interacting with a given instance of US server, typically from
any remote host able to connect to the VM hosting that instance.
""".


-doc """
The prefix to be used for elements (e.g. entry keys, node names) specific to a
given type of US server.

For example: `us_{main,web}` or `us_foo`, the example taken here.
""".
-type server_prefix() :: atom().


-export_type([ server_prefix/0 ]).



-export([ setup/1, setup/2,
          get_config_server_info/1, teardown/0, teardown/1,
          get_tcp_port_range/1 ]).


% For update_code_path_for_myriad/0 and all:
-include_lib("myriad/include/myriad_script_include.hrl").


-define( wait, 0 ).


% Type shorthands:

-type atom_node_name() :: net_utils:atom_node_name().

-type lookup_info() :: naming_utils:lookup_info().

-type argument_table() :: cmd_line_utils:argument_table().

-type file_path() :: file_utils:file_path().

-type cfg_table() :: cmd_line_utils:argument_table().



-doc """
Setups this US client.

Returns the name of the target US node, whether this client should be verbose
(by default: no), the corresponding configuration, and the table of remaining
command-line arguments (if any).
""".
-spec setup( server_prefix() ) ->
    { atom_node_name(), IsVerbose :: boolean(), cfg_table(), argument_table() }.
setup( ServerPrefix ) ->
    setup( ServerPrefix, _IsVerboseByDefault=false ).


-doc """
Setups this US client, possibly verbose.

Returns the name of the target US node, whether this client should be verbose,
the corresponding configuration, and the table of remaining command-line
arguments (if any).
""".
-spec setup( server_prefix(), IsVerboseByDefault :: boolean() ) ->
    { atom_node_name(), IsVerbose :: boolean(), cfg_table(), argument_table() }.
setup( ServerPrefix, IsVerboseByDefault ) ->

    % First, enable all possible helper code (hence to be done first of all):
    update_code_path_for_myriad_from_module(),

    % Allows to support both OTP conventions and ad hoc, automatic ones:
    wooper_utils:start_for_app(),

    { CfgFilePath, IsVerbose, FinalArgTable } =
        init_from_command_line( IsVerboseByDefault ),

    CfgTable = file_utils:read_etf_file( CfgFilePath ),

    %trace_utils:debug_fmt( "Read configuration from '~ts':~n ~p",
    %                       [ CfgFilePath, CfgTable ] ),

    { MainTargetNodeName, UserTargetNodeName } =
        get_target_node_names( CfgTable, ServerPrefix ),

    IsVerbose andalso app_facilities:display(
        "Trying to connect to US server node '~ts', as client node '~ts'.",
        [ MainTargetNodeName, node() ] ),

    % Test regarding the problem of overlapping partitions:
    %timer:sleep( 500 ),

    ActualTargetNodeName = case net_adm:ping( MainTargetNodeName ) of

        pong ->
           MainTargetNodeName;

        pang ->
            trace_utils:warning_fmt( "Unable to connect to a target main "
                "node '~ts'; trying an alternate one, based on "
                "user name: '~ts' (in case of ad hoc launch).",
                [ MainTargetNodeName, UserTargetNodeName ] ),

            case net_adm:ping( UserTargetNodeName ) of

                pong ->
                    UserTargetNodeName;

                pang ->
                    trace_utils:error_fmt( "Unable to connect to either node "
                        "names, the main one ('~ts') or the user one ('~ts')."
                        "~nIf the target node is really running and is named "
                        "like either of the two, check that the cookies match "
                        "and, finally, that no firewall is in the way "
                        "(e.g. a server may filter the EPMD port of interest).",
                        [ MainTargetNodeName, UserTargetNodeName ] ),

                    throw( { unable_to_connect_to,
                             { MainTargetNodeName, UserTargetNodeName } } )

            end

    end,

    % Otherwise the remote node could not be known before use:
    global:sync(),

    %app_facilities:display( "Globally registered names: ~w.",
    %                        [ global:registered_names() ] ),

    { ActualTargetNodeName, IsVerbose, CfgTable, FinalArgTable }.



-doc """
Returns the information needed to look up the (registered) target server.
""".
-spec get_config_server_info( cfg_table() ) -> lookup_info().
get_config_server_info( CfgTable ) ->

    Key = us_server_registration_name,

    RegName = case list_table:get_value_with_default( Key, _Def=undefined,
                                                      CfgTable ) of

        undefined ->
            throw( { lacking_configuration_entry, Key, CfgTable } );

        RgName when is_atom( RgName ) ->
            RgName;

        RgName ->
            throw( { invalid_us_server_registration_name, RgName } )

    end,

    LookupScope = list_table:get_value_with_default( us_server_lookup_scope,
                                                     _Default=local, CfgTable ),

    naming_utils:vet_lookup_scope( LookupScope ) orelse
            throw( { invalid_us_server_registration_lookup, LookupScope } ),

    { RegName, LookupScope }.




-doc """
Initialises this application from the command line.

Returns the path to the selected configuration file, whether verbose mode is
activated, and the remaining argument table.
""".
-spec init_from_command_line( IsVerboseByDefault :: boolean() ) ->
          { CfgFilePath :: file_path(), IsVerbose :: boolean(), cfg_table() }.
init_from_command_line( IsVerboseByDefault ) ->

    % To force options for testing:
    %ArgTable = cmd_line_utils:generate_argument_table( "--help" ),

    ArgTable = cmd_line_utils:get_argument_table(),

    %trace_utils:debug_fmt( "Argument table: ~ts",
    %                       [ list_table:to_string( ArgTable ) ] ),

    % Argument expected to be set by the caller script:
    { CfgFilePath, ConfigShrunkTable } =
            case list_table:extract_entry_if_existing( '-config-file',
                                                       ArgTable ) of

        false ->
            throw( no_configuration_file_set );

        { [ [ CfgPath ] ], CfgShrunkTable } ->
            case file_utils:is_existing_file_or_link( CfgPath ) of

                true ->
                    { CfgPath, CfgShrunkTable };

                false ->
                    throw( { configuration_file_not_found, CfgPath } )

            end;

        { OtherCfgArg, _CfgTable } ->
            throw( { unexpected_configuration_argument, OtherCfgArg } )


    end,

    %trace_utils:debug_fmt( "Configuration file: '~ts'.", [ CfgFilePath ] ),


    { IsVerbose, VerbShrunkArgTable } =
            case cmd_line_utils:extract_command_arguments_for_option(
                _Opt='-verbose', ConfigShrunkTable ) of

        { undefined, VArgTable } ->
            { IsVerboseByDefault, VArgTable };

        { [ [] ], VArgTable } ->
            { true, VArgTable };

        { UnexpectedVArgs, _VArgTable } ->
            throw( { unexpected_verbose_args, UnexpectedVArgs } )

     end,


    % Argument also expected to be set by the caller script:
    { RemoteCookie, CookieShrunkTable } =
            case list_table:extract_entry_if_existing( '-target-cookie',
                                                       VerbShrunkArgTable ) of

        false ->
            throw( no_target_cookie_set );

        { [ [ Cookie ] ], CookShrunkTable } ->
            { text_utils:string_to_atom( Cookie ), CookShrunkTable };

        { OtherCookieArg, _CookTable } ->
            throw( { unexpected_cookie_argument, OtherCookieArg } )

    end,

    %trace_utils:debug_fmt( "Setting remote cookie: '~ts'.", [ RemoteCookie ] ),

    net_utils:set_cookie( RemoteCookie ),

    % Depends on the client:

    %trace_utils:debug_fmt( "Remaining arguments: ~ts",
    %   [ cmd_line_utils:argument_table_to_string( CookieShrunkTable ) ] ),

    %list_table:is_empty( CookieShrunkTable ) orelse
    %   throw( { unexpected_arguments,
    %            list_table:enumerate( CookieShrunkTable ) } ),

    { CfgFilePath, IsVerbose, CookieShrunkTable }.




-doc """
Returns the possible node name corresponding to the target server US server.

Either it is set in the configuration of this client, or the default name of
that server, deriving from the server prefix, is returned.
""".
-spec get_target_node_names( cfg_table(), server_prefix() ) ->
                                        { atom_node_name(), atom_node_name() }.
get_target_node_names( CfgTable, ServerPrefix ) ->

    %HostnameKey = text_utils:atom_format( "~ts_hostname", [ ServerPrefix ] ),
    HostnameKey = 'us_server_hostname',

    RemoteHostnameStr = list_table:get_value( HostnameKey, CfgTable ),

    %trace_utils:debug_fmt( "Remote host: '~ts'.", [ RemoteHostname ] ),

    net_utils:localnode() =/= local_node orelse
        throw( { node_not_networked, node() } ),

    % Supposing here uniform client/server conventions in terms of short or long
    % names:
    %
    NodeNamingMode = net_utils:get_node_naming_mode(),

    % Note that two hardcoded node names are used here, the main one (when run
    % as a service) and one embedding the name of the current user (when run as
    % an app, typically for testing; we used to suppose that the user names -
    % local and server - matched, but now we expect a conventional user to be
    % used on the server):
    %
    % (resulting ultimately in e.g. 'us_foo@hurricane')
    MainBaseNodeNameStr = text_utils:atom_to_string( ServerPrefix ),

    % Not relying on system_utils:get_user_name(), as server and clients are
    % usually running under different users; supposing a server prefix 'us_foo'
    % is run by a user named 'foo-srv':
    %
    % (resulting ultimately in for example 'us_foo_exec-foo-srv@hurricane';
    % anyway not much used nowadays)
    %
    BaseUserName = case atom_to_list( ServerPrefix ) of

        "us_" ++ BaseSrvName ->
            BaseSrvName;

        % If no matching prefix, use the whole name:
        FullBaseSrvName ->
            FullBaseSrvName

    end,

    UserNameStr = text_utils:format( "~ts-srv", [ BaseUserName ] ),

    % '_exec', not '_app':
    UserBaseNodeNameStr = text_utils:format( "~ts_exec-~ts",
                                             [ ServerPrefix, UserNameStr ] ),

    { net_utils:get_complete_node_name( MainBaseNodeNameStr, RemoteHostnameStr,
                                        NodeNamingMode ),

      net_utils:get_complete_node_name( UserBaseNodeNameStr, RemoteHostnameStr,
                                        NodeNamingMode ) }.



-doc "Returns the TCP port range to use (if any).".
get_tcp_port_range( CfgTable ) ->

    MaybePortRange = list_table:get_value_with_default( _K=tcp_port_range,
        _Default=undefined, CfgTable ),

    %trace_utils:debug_fmt( "TCP port range: ~p.", [ MaybePortRange ] ),

    MaybePortRange.




-doc "Tears down the client gracefully.".
-spec teardown() -> void().
teardown() ->
    teardown( _IsVerbose=false ).


-doc "Tears down the client gracefully.".
-spec teardown( IsVerbose :: boolean() ) -> void().
teardown( IsVerbose ) ->

    IsVerbose andalso app_facilities:display( "Client terminating now "
        "(while known other nodes are ~w).", [ nodes() ] ),

    % Feeble attempt of avoiding non-systematic "'global' at node us_foo@fff
    % requested disconnect from node 'us_foo_controller_exec-uu@mmm' in order
    % to prevent overlapping partitions":
    %
    % (far less brutal than erlang:halt/{0,1}, yet awfully slow, and
    % actually non-blocking)
    %
    global:disconnect(),

    %timer:sleep( ?wait ),

    global:sync(),

    init:stop( _StatusCode=0 ),

    %timer:sleep( ?wait ),

    % We thought that the actual reason was actually that the client host had a
    % firewall that blocked incoming EPMD (on a specific port) connections from
    % the US server host, yet after fixing that the "overlapping partitions"
    % problem remained. DNS names, EPMD ports, node names, cookies, TCP ranges
    % seem to be legit, though.
    %
    % Finally the only solution left would be to resort to disabling this
    % prevent_overlapping_partitions options (see DIST_OPTS in Myriad's
    % GNUmakevars.inc), which is worse.
    %
    % Maybe that the actual reason for these overlapping warnings is in link
    % with messages like ** Connection attempt from node us_foo@mmm
    % rejected. Invalid challenge reply. ** and/or with the fact that sometimes
    % us_foo@fff may be brutally killed (hence without letting its EPMD and
    % other EPMDs know that it does not exist anymore).
    %
    % Or could it be that:
    %  - another us_foo@mmm node interfere with us_foo@fff?
    %  - or us_foo_controller_exec-uu@mmm is started, it connects directly to
    %  us_foo@fff, which tries to contact back us_foo_controller_exec-uu@mmm
    %  whereas the EMPD of the latter is not yet aware of its
    %  us_foo_controller_exec-uu node?


    % ?app_stop should not be used here as its wait_for_any_trace_supervisor
    % macro would wait for a non-launched supervisor.
    %
    % ?app_stop_without_waiting_for_trace_supervisor() is not used either, as
    % no aggregator was started from that client.

    % Otherwise too slow:
    app_facilities:finished( IsVerbose, _BeQuick=true ).
