% Description of the US-common OTP active application, typically used by rebar3.
%
% The real source of this file is conf/us_common.app.src, from which
% _build/lib/us_common/ebin/us_common.app is generated and copied to
% ebin/us_common.app; finally src/us_common.app.src is a mere symlink to this
% last file, so we have:
%
% ./conf/us_common.app.src [only real source]
% ./_build/lib/us_common/ebin.us_common.app
% ./ebin/us_common.app
% ./src/us_common.app.src -> ../ebin/us_common.app
%
% For more information see the Myriad 'rebar3-create-app-file' make target and
% its associated comments.

% See also:
% - http://erlang.org/doc/man/app.html
% - https://learnyousomeerlang.com/building-otp-applications


{application, us_common,
 [{description, "US-common, the base OTP active application on which the various Universal Service elements are built (see http://us.esperide.org)"},
  {vsn, "0.0.1"},
  {registered, []},

  % Regarding:
  %  - WOOPER, see http://wooper.esperide.org/wooper.html#otp
  %  - Myriad, see http://myriad.esperide.org/myriad.html#otp
  %
  % myriad is a dependency of wooper, which is itself a dependency of traces and
  % as such may not be listed here, however we stay conservative;
  %
  {applications, [kernel, stdlib, sasl, myriad, wooper, traces]},
  {env,[]},

  % Flat hierarchy in ebin here:
  {modules, [class_USServer, class_USTaskRing, class_USScheduler, class_USConfigServer]},

  {licenses, ["US-Common is licensed by its author (Olivier Boudeville) under the GNU Affero General Public License (AGPL), version 3.0 or later"]},

  % Active application:
  %
  % (no specific relevant startup argument to specify here)
  %
  {mod, {us_common_app, []}},

  {links, [ {"Official website", "http://us.esperide.org" },
			{"Github", "https://github.com/Olivier-Boudeville/us-common"} ]}

  %{exclude_files, []}

 ]}.
