% Copyright (C) 2025-2025 Olivier Boudeville
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
% Creation date: Friday, July 25, 2025.


% Defines for US actions.


% Internal view onto an action, notably used to coerce the arguments that it
% receives; a value of an action_table/0.
%
-record( action_info, {

    % Any naming-related lookup information for the server actually implementing
    % that action ('undefined' if directly implemented by this local server):
    %
    server_lookup_info = 'undefined'
        :: option( naming_utils:lookup_info() ),

    % The name of that action:
    action_name :: us_action:action_name(),

    % Information regarding the (ordered) arguments of that action entry:
    arg_specs :: [ us_action:arg_spec() ],

    % Any type and possibly description, or no checking at all:
    result_spec :: us_action:result_spec(),

    % The actual request this action maps to:
    request_name :: wooper:request_name(),

    description :: option( us_action:description() ) } ).
