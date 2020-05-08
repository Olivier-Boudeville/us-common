% Copyright (C) 2020-2020 Olivier Boudeville
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


% Testing of US-Common as an OTP active application, directly from within its
% code base (hence without needing to create a separate, mock-up test release
% for that).
%
-module(us_common_otp_application_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% Actual test:
test_us_common_application( OrderedAppNames ) ->

	test_facilities:display( "Starting the US-Common OTP active application." ),
	otp_utils:start_applications( OrderedAppNames ),


	test_facilities:display( "US-Common version: ~p.",
				 [ system_utils:get_application_version( us_common ) ] ),


	test_facilities:display( "Stopping the US-Common application." ),
	otp_utils:stop_applications( OrderedAppNames ),

	test_facilities:display(
	  "Successful end of test of the US-Common OTP application." ).



% Note that the us_common.app, traces.app, wooper.app and myriad.app files will
% have to be found and used for this test to succeed: US-Common, Traces, WOOPER
% and Myriad must be already available as prerequisite, fully-built OTP
% applications.
%
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Build root directory from which prerequisite applications may be found:
	BuildRootDir = "..",

	OrderedAppNames = [ myriad, wooper, traces, sasl, us_common ],

	case otp_utils:prepare_for_test( OrderedAppNames, BuildRootDir ) of

		ready ->
			test_us_common_application( OrderedAppNames ) ;

		{ lacking_app, _App } ->
			% (a detailed warning message has been issued by
			% otp_utils:prepare_for_test/2)
			%
			ok

	end,

	test_facilities:stop().
