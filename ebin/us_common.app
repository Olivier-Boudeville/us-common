% Description of the US-Common OTP active application, typically used by rebar3.

% Note: if this file is named us_common.app, it is a *generated* file, whose
% real source is conf/us_common.app.src, from which
% _build/lib/us_common/ebin/us_common.app is obtained and copied to
% ebin/us_common.app; finally src/us_common.app.src is a mere symlink to this
% last file, so we have:
%
% ./conf/us_common.app.src [only real source]
% ./_build/lib/us_common/ebin/us_common.app
% ./ebin/us_common.app
% ./src/us_common.app.src -> ../ebin/us_common.app
%
% For more information see the Ceylan-Myriad 'create-app-file' make target and
% its associated comments.

% See also:
% - http://erlang.org/doc/man/app.html
% - https://learnyousomeerlang.com/building-otp-applications


{application, us_common,
 [{description, "US-Common, the base OTP active application on which the various Universal Service elements are built (see http://us-common.esperide.org)"},
  {vsn, "0.1.6"},
  {registered, []},

  % Regarding:
  %  - Traces, see http://traces.esperide.org/traces.html#otp
  %  - WOOPER, see http://wooper.esperide.org/wooper.html#otp
  %  - Myriad, see http://myriad.esperide.org/myriad.html#otp
  %
  % myriad is a dependency of wooper, which is itself a dependency of traces and
  % as such may not be listed here, however we stay conservative;
  %
  {applications, [kernel, stdlib, sasl, myriad, wooper, traces]},
  {env,[]},

  % Flat hierarchy in ebin here:
  {modules, [class_USConfigServer, class_USScheduler, class_USServer, class_USTaskRing, us_common_app, us_common_config_bridge_sup, us_common_scheduler_bridge_sup, us_common_sup]},

  {licenses, ["US-Common is licensed by its author (Olivier Boudeville) under the GNU Affero General Public License (AGPL), version 3.0 or later"]},

  % Active application:
  %
  % (no specific relevant startup argument to specify here)
  %
  {mod, {us_common_app, []}},

  {links, [ {"Official website", "http://us-common.esperide.org" },
			{"Github", "https://github.com/Olivier-Boudeville/us-common"} ]}

  %{exclude_files, []}

 ]}.
