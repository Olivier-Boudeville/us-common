% Copyright (C) 2019-2020 Olivier Boudeville
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
% Creation date: Sunday, July 14, 2019.


% Module implementing the root supervisor of US-Common.
%
% In practice, it will supervise a single process, the one of the (singleton) US
% configuration server, through a dedicated supervision bridge, defined in the
% us_common_bridge_sup module.
%
-module(us_common_sup).


% Implementing the OTP supervisor behaviour:
-behaviour(supervisor).


% User API, typically triggered from us_common_app:
-export([ start_link/0 ]).


% Callback of the supervisor behaviour:
%
% (see https://erlang.org/doc/design_principles/sup_princ.html)
%
-export([ init/1 ]).


-define( root_supervisor_name, ?MODULE ).


% Starts and links the US-Common root supervisor, creating in turn a proper
% supervision bridge.
%
% Note: typically called by us_common_app:start/2, hence generally triggered by
% the application initialisation.
%
-spec start_link() -> supervisor:startlink_ret().
start_link() ->

	trace_utils:debug( "Starting the US-Common root supervisor." ),

	% Local registration is better, to avoid clashes:
	supervisor:start_link( _Reg={ local, ?root_supervisor_name },
						   _Mod=?MODULE, _Args=[] ).



% Callback to initialise this US-Common root supervisor bridge, typically in
% answer to start_link/0 above being executed.
%
-spec init( list() ) -> { 'ok',
	   { supervisor:sup_flags(), [ supervisor:child_spec() ] } } | 'ignore'.
init( Args=[] ) ->

	trace_utils:debug_fmt(
	  "Initializing the US-Common root supervisor (args: ~p).", [ Args ] ),

	% We always create a US configuration server and a scheduler that are
	% specific to the current US application so that they can all be started,
	% stopped, upgraded, etc., independently.

	% Restart only children that terminate, and enforces intensity and periods
	% corresponding to the execution target this layer was compiled with:
	%
	SupSettings = otp_utils:get_supervisor_settings(
					_RestartStrategy=one_for_one,
					class_USConfigServer:get_execution_target() ),

	% First child, a bridge in charge of the US configuration server:
	CfgBridgeChildSpec = get_config_bridge_spec(),

	% Second child, a bridge in charge of the US-Common base scheduler:
	SchedBridgeChildSpec = get_scheduler_bridge_spec(),

	ChildSpecs = [ CfgBridgeChildSpec, SchedBridgeChildSpec ],

	{ ok, { SupSettings, ChildSpecs } }.



get_config_bridge_spec() ->
	#{ id => us_common_config_bridge_id,

	   start => { _Mod=us_common_config_bridge_sup, _Fun=start_link, _Args=[] },

	   % Always restarted:
	   restart => permanent,

	   % 2-second termination allowed before brutal killing:
	   shutdown => 2000,

	   type => supervisor,

	   modules => [ us_common_config_bridge_sup ] }.



get_scheduler_bridge_spec() ->
	#{ id => us_common_scheduler_bridge_id,

	   start => { _Mod=us_common_scheduler_bridge_sup, _Fun=start_link,
				  _Args=[] },

	   % Always restarted:
	   restart => permanent,

	   % 2-second termination allowed before brutal killing:
	   shutdown => 2000,

	   type => supervisor,

	   modules => [ us_common_scheduler_bridge_sup ] }.
