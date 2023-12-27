% Copyright (C) 2020-2024 Olivier Boudeville
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


% @doc Module implementing the <b>OTP supervisor bridge for the US-Common main
% scheduler</b>, so that this server is attached to the US-Common supervision
% tree, through the US-Common root supervisor, defined in the us_common_sup
% module.
%
-module(us_common_scheduler_bridge_sup).


% A US scheduler is not a gen_server but a WOOPER instance, therefore a
% supervisor bridge is needed in order to connect this server to an OTP
% supervision tree.
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


% For us_common_scheduler_registration_{name,scope}:
-include("us_common_defines.hrl").


% @doc Starts and links a US-Common supervision bridge to the US scheduler.
%
% Note: typically spawned as a supervised child of the US-Common root supervisor
% (see us_common_sup:init/1), hence generally triggered by the application
% initialisation.
%
-spec start_link() -> term().
start_link() ->

	% Apparently not displaying, yet executed:
	trace_bridge:info( "Starting the US-Common supervisor bridge for "
					   "the US main scheduler." ),

	supervisor_bridge:start_link( { local, ?bridge_name },
								  _Module=?MODULE, _Args=[] ).



% @doc Callback to initialise this supervisor bridge, typically in answer to
% start_link/1 being executed.
%
-spec init( list() ) -> { 'ok', pid(), State :: term() }
							| 'ignore' | { 'error', Error :: term() }.
init( _Args=[] ) ->

	trace_bridge:info( "Initializing the US-Common supervisor bridge for "
					   "the US main scheduler." ),

	SchedServerPid = class_USScheduler:new_link( "US-Common Scheduler",
		?us_common_scheduler_registration_name,
		?us_common_scheduler_registration_scope ),

	{ ok, SchedServerPid, _State=SchedServerPid }.



% @doc Callback to terminate this supervisor bridge.
-spec terminate( Reason :: 'shutdown' | term(), State :: term() ) -> void().
terminate( Reason, _State=SchedServerPid ) when is_pid( SchedServerPid ) ->

	trace_bridge:info_fmt( "Terminating the US-Common supervisor bridge for "
		"the US-Common scheduler (reason: ~w, scheduler: ~w).",
		[ Reason, SchedServerPid ] ),

	% Synchronicity needed, otherwise a potential race condition exists, leading
	% this process to be killed by its OTP supervisor instead of being normally
	% stopped:
	%
	wooper:delete_synchronously_instance( SchedServerPid ),

	trace_bridge:debug_fmt( "US-Common scheduler ~w terminated.",
						   [ SchedServerPid ] ).
