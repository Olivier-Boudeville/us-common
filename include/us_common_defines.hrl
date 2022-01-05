% Copyright (C) 2020-2022 Olivier Boudeville
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
% Creation date: Sunday, January 19, 2020.


% US-Common defines.


-define( us_common_scheduler_registration_name, us_common_scheduler ).

% Not global, so that two applications relying on US-Common may interact:
-define( us_common_scheduler_registration_scope, local_only ).


% For most US-related filesystem elements, the us-srv:us-srv ownership shall be
% enforced:

% Just pick the current user, in a development context:
-define( us_user, undefined ).

% In a production context, rely on uncoupled, dedicated (per-application) users:
%-define( us_user, "us-srv" ).
