% Copyright (C) 2020-2021 Olivier Boudeville
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
% Creation date: Wednesday, May 6, 2020.


% Module implementing the supervisor bridge for the US-Common configuration
% server, so that this (singleton) server is attached to the US-Common
% supervision tree, through the US-Common root supervisor, defined in the
% us_common_sup module.
%
-module(us_common_config_bridge_sup).


% The US configuration server is not a gen_server but a WOOPER instance,
% therefore a supervisor bridge is needed in order to connect this server to
% an OTP supervision tree.
%
% As a result, the process whose code is defined in the current module, being a
% supervisor bridge, behaves like a real supervisor to its own supervisor (the
% root supervisor of US-Common, namely us_common_sup), but has a different
% interface than a real supervisor to the US-Common subsystem.
%
% Hence used for (optional) OTP compliance (see
% http://erlang.org/doc/man/supervisor_bridge.html).
%
% We suppose such a supervisor bridge cannot be used as a root supervisor.
%
% See also within the Erlang codebase itself, as an example, the user_sup
% supervisor bridge, created by kernel:init/1.
%
-behaviour(supervisor_bridge).


% User API:
-export([ start_link/0 ]).


% Callbacks of the supervisor_bridge behaviour:
-export([ init/1, terminate/2 ]).


-define( bridge_name, ?MODULE ).



% Starts and links a US-Common supervision bridge to the US configuration
% server.
%
% Note: typically spawned as a supervised child of the US-Common root supervisor
% (see us_common_sup:init/1), hence generally triggered by the application
% initialisation.
%
-spec start_link() -> term().
start_link() ->

	% Apparently not displaying, yet executed:
	trace_utils:debug( "Starting the US-Common supervisor bridge for "
					   "the US configuration server." ),

	supervisor_bridge:start_link( { local, ?bridge_name },
								  _Module=?MODULE, _Args=[] ).



% Callback to initialise this supervisor bridge, typically in answer to
% start_link/1 above being executed.
%
-spec init( list() ) -> { 'ok', pid(), State :: term() }
							| 'ignore' | { 'error', Error :: term() }.
init( _Args=[] ) ->

	trace_utils:info( "Initializing the US-Common supervisor bridge for "
					  "the US configuration server." ),

	% Registration name and all details set through the US configuration file:
	CfgServerPid = class_USConfigServer:new_link(),

	{ ok, CfgServerPid, _State=CfgServerPid }.



% Callback to terminate this supervisor bridge.
-spec terminate( Reason :: 'shutdown' | term(), State :: term() ) -> void().
terminate( Reason, _State=CfgServerPid ) when is_pid( CfgServerPid ) ->

	trace_utils:info_fmt( "Terminating the US-Common supervisor bridge for "
		"the US configuration server (reason: ~w, configuration server: ~w).",
		[ Reason, CfgServerPid ] ),

	% No synchronicity especially needed:
	CfgServerPid ! delete.
