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
% In practice, it currently supervise no specific process.
%
-module(us_common_sup).


% Implementing the OTP supervisor behaviour:
-behaviour(supervisor).


% User API:
-export([ start_link/0 ]).


% Callback of the supervisor behaviour:
-export([ init/1 ]).



-define( supervisor_name, ?MODULE ).


% Starts and links the WOOPER root supervisor.
start_link() ->

	trace_utils:debug( "Starting the US-Common root supervisor." ),

	supervisor:start_link( { local, ?supervisor_name },
						   _Module=?MODULE, _Args=[] ).


% Callback to initialise this supervisor.
init( Args ) ->

	trace_utils:debug_fmt(
	  "Initializing the US-Common root supervisor (args: ~p).", [ Args ] ),

	% None for the moment:
	%ChildrenSpec = [],

	%{ ok, { RestartStrategy, ChildrenSpec } }.
	{ ok, ignore }.
