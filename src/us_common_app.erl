% Copyright (C) 2019-2021 Olivier Boudeville
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
% Creation date: Saturday, July 20, 2019.


% Module implementing the US-Common (active) application behaviour.
-module(us_common_app).


% Implementing the (active, OTP) application behaviour:
%
% (see https://erlang.org/doc/design_principles/applications.html)
%
-behaviour(application).


% Callbacks of the application behaviour:
-export([ start/2, stop/1 ]).



% Starts the US-Common services.
-spec start( application:start_type(), StartArgs :: term() ) -> { 'ok', pid() }
		| { 'ok', pid(), State :: term() } | { 'error', Reason :: term() }.
start( Type, StartArgs ) ->

	trace_utils:debug_fmt( "Starting the US-Common application (type: ~w, "
						   "start arguments: ~w).", [ Type, StartArgs ] ),

	case us_common_sup:start_link() of

		R={ ok, UsCommonRootSupervisorPid }
		  when is_pid( UsCommonRootSupervisorPid ) ->
			R;

		% Includes 'ignore':
		Other ->
			trace_utils:error_fmt( "The US-Common root supervisor did not "
								   "start properly:~n  ~p.", [ Other ] ),
			{ error, Other }

	end.



% Stops the US-Common services.
-spec stop( State :: term() ) -> void().
stop( State ) ->

	trace_utils:debug_fmt( "Stopping the US-Common application (state: ~w).",
						   [ State ] ),

	ok.
