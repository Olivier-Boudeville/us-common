% Copyright (C) 2020-2025 Olivier Boudeville
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
% Creation date: Wednesday, March 18, 2020.

-module(class_USScheduler).

-moduledoc """
Class corresponding to the **task scheduler** of the US framework.
""".


-define( class_description, "Task scheduler for the US framework." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_USServer ] ).




% Scheduling of tasks on behalf of the US framework.


-doc "PID of a scheduler.".
-type scheduler_pid() :: class_USServer:server_pid().



-doc """
The command corresponding to a task to execute respects the general form of a
WOOPER oneway, i.e. `OnewayName` or `{OnewayName, Args}`, where `Args` is
conventionally a single non-list term or a list of any arguments.

We considered, yet finally did not keep, the idea of always adding as last
element the PID of the sending scheduler. So specified commands are simply sent
verbatim to their actuators.
""".
-type task_command() :: wooper:oneway_call().




% To avoid mistakes between amounts of milliseconds:

-doc "Milliseconds since beginning of year 0 (in monotonic time).".
-type ms_since_year_0() :: unit_utils:milliseconds().


-doc "Milliseconds since the start of this scheduler (in monotonic time).".
-type ms_since_start() :: unit_utils:milliseconds().



-doc "Specifies the start time of a task scheduling.".
-type start_time() ::

	% As soon as possible:
	'asap'

	% When relevant/applicable (flexible start):
  | 'flexible'

	% Preferably after this specified duration from receiving:
  | dhms_duration()
  | seconds() % (not milliseconds)

	% Preferably at this specified (future) time:
  | timestamp().



-doc """
Time between two schedulings of a task, as expressed by the user.

Note that user periodicities are significantly coarser (second-based) than
internal ones (millisecond-based).
""".
-type user_periodicity() ::

	% Just to be executed once (one shot):
	'once'

	% Actual period:
  | dhms_duration()

  | seconds().



-doc """
Time between two schedulings of a task, as used internally.

Not to be mixed up with user periodicities, which are significantly coarser than
internal ones.
""".
-type periodicity() :: option( ms_duration() ).



-doc "The number of times a task shall be scheduled.".
-type schedule_count() :: 'unlimited' | count().



-doc "The PID of a process requesting a task to be scheduled.".
-type requester_pid() :: pid().



-doc """
The PID of the process to which a task command will be sent whenever scheduled.
""".
-type actuator_pid() :: pid().



-doc "The outcome of a task registration.".
-type task_registration_outcome() ::

	% The task command has been immediately and fully processed (no identifier
	% returned, as no task entry was even created):
    %
	'task_done'

    % Planned whenever appropriate (in the future and/or periodically):
  | { 'task_registered', task_id() }.



-doc """
Outcome of a task unregistration.

The identifier of the unregistered task is returned, to allow for concurrent
caller-side operations.
""".
-type task_unregistration_outcome()::

    % If the unregistration succeeded:
    { 'task_unregistered', task_id() }

    % If the task was already fully done:
    %
    % (thus any number of next unregistrations will result in this outcome)
    %
  | { 'task_already_done', task_id() }

    % If the unregistration failed:
    %
    % (term returned if returning any caller-specified invalid task identifier)
    %
  | { 'task_unregistration_failed', error_reason(), task_id() | term() }.




-doc "Identifier of a task, as assigned by a scheduler.".
-type task_id() :: count().



-doc "Reference to a timer.".
-type timer_ref() :: timer:tref().



-doc "Associates to a schedule offset the reference of a live timer.".
-type timer_table() :: table( schedule_offset(), timer_ref() ).



-export_type([ scheduler_pid/0, task_command/0,
			   ms_since_year_0/0, ms_since_start/0, start_time/0,
			   user_periodicity/0, periodicity/0, schedule_count/0,
			   requester_pid/0, actuator_pid/0,
			   task_registration_outcome/0, task_unregistration_outcome/0,
			   task_id/0, timer_ref/0, timer_table/0 ]).



-doc """
A millisecond-based offset relative to the start time of this scheduler (hence
designed to be rather small).
""".
-type schedule_offset() :: ms_since_start().


% Describes a task to schedule:
-record( task_entry, {

	% Unique identifier of this task:
	id :: task_id(),

	% The command to trigger on the actuator:
	command :: task_command(),

	% The offset (relative to the start time of this scheduler) at which the
	% next scheduling of this task shall happen:
	%
	next_schedule :: schedule_offset(),

	% The periodicity at which this task shall be scheduled:
	periodicity :: periodicity(),

	% The number of times this task shall still be scheduled:
	count :: schedule_count(),

	% The number of times this task has already been scheduled:
	schedule_count = 0 :: schedule_count(),

	% The internal time offset (if any) at which this task was first scheduled:
	started_on = undefined :: option( schedule_offset() ),

	% The internal time offset (if any) at which this task was last scheduled:
	last_schedule = undefined :: option( schedule_offset() ),

	% The PID of the process having registered this task:
	requester_pid :: requester_pid(),

	% The PID of the process that will be triggered whenever this task is
	% scheduled:
	%
	actuator_pid :: actuator_pid() } ).



-doc "Describes a task to schedule.".
-type task_entry() :: #task_entry{}.



-doc "To keep track of task information.".
-type task_table() :: table( task_id(), task_entry() ).



-doc "Registers which tasks shall be scheduled at a given time offset.".
-type schedule_pair() :: { schedule_offset(), [ task_id() ] }.



-doc """
Schedule pairs, ordered from closest future (sooner) to most remote one (later).
""".
-type schedule_plan() :: [ schedule_pair() ].


% The class-specific attributes:
-define( class_attributes, [

	{ task_table, task_table(), "a table registering all ongoing tasks" },

	{ schedule_plan, schedule_plan(), "the ordered list of next schedulings" },

	{ timer_table, timer_table(), "a table registering live timers" },

	% Allows also to count all tasks that have been submitted:
	{ next_task_id, task_id(), "identifier of the next task to register" } ] ).



% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.Scheduling" ).

% For us_common_scheduler_registration_{name,scope}:
-include("us_common_defines.hrl").


% Exported helpers:
-export([ vet_user_periodicity/2 ]).


% To silence about unused functions:
-export([ get_schedule_offset_for/2, get_current_timestamp/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").



% Implementation notes:
%
% On task commands:
%
% We preferred to limit the ability of a scheduler only to sending *messages* to
% whichever process needed (e.g. no MFA direct calls), for a better separation
% of concerns / state management.
%
%
% On synchronicity:
%
% By default all interactions with the scheduler are synchronous (a result is
% sent to the caller).
%
% At least for some operations, asynchronous variations (the base, synchronous
% operations whose names are suffixed with 'Async') have also been defined, if
% the caller has no interest in result and does not want to perform any kind of
% synchronisation.
%
%
% On time:
%
% The monotonic time is relied on internally, to avoid any warp/leap performed
% by the system time. See http://erlang.org/doc/apps/erts/time_correction.html
% for more information.
%
% Timestamps (e.g. to designate Sunday, March 22, 2020 at noon) are converted
% into internal time offsets whenever a scheduler registers a task.
%
% As a result, correct periodicities will be enforced (e.g. really a 1-hour
% wallclock duration between two schedulings, regardless of any time warp) but,
% for example due to DST (Daylight Saving Time), tasks may appear to be executed
% with a time offset (e.g. at 1 PM instead of noon). Working with a monotonic,
% UTC (Universal Coordinated Time)-like time is thus intentional.
%
% Due to some external event (e.g. system overload or suspension), task
% deadlines may be missed, in which case they will be rescheduled in a row at a
% faster pace on a best-effort basis (rather than being skipped as a whole). As
% a result, the number of schedulings should match the expected one, yet the
% average periodicities may not always be preserved (trade-off: not loosing
% schedulings, yet preparing one scheduling from the previous one, hence trying
% to preserve specified periodicity).
%
% Internal durations are expressed in milliseconds, although the accuracy is
% most probably coarser than that, and at least most of the user-defined
% durations are only expressed at the second level.
%
% All internal durations are relative to the start time of this scheduler
% (rather than for example to year 0 of the Gregorian calendar, or to the EPOCH,
% i.e. 1/1/1970 at 00:00:00), as we prefer smaller durations (offsets) than
% "absolute" ones; we used the 'server_start' class_USServer-inherited attribute
% and monotonic time for that.
%
% For any time unit TU (e.g. TU=millisecond here):
% erlang:monotonic_time(TU) + erlang:time_offset(TU) = erlang:system_time(TU).
%
% This scheduler uses internally monotonic time; at its boundaries (e.g. at task
% submission), whenever having to deal with absolute timestamps, these are
% immediately converted to internal conventions (e.g. a task deadline is to be
% expressed by the user in system time, and will be converted directly into a
% monotonic timestamp when received; scheduling reports proceed the other way
% round).
%
% Note that, should the operating system be suspended (typically if the host
% computer itself is suspended), the monotonic time will be suspended as well,
% unless the --enable-prefer-elapsed-monotonic-time-during-suspend command-line
% argument was specified when building Erlang/OTP (possibly then inducing a
% performance penalty). We do not expect servers to be suspended.
%
% All kinds of internal system times (notably the VM one) are in UTC, so
% depending on time zones and DST, hour-based offsets apply. At the interfaces
% between the scheduler and the user, universal time is converted into local
% time, and the other way round.
%
% The timer module is used, rather than a utility process using for example
% receive/after, presumably for a better accuracy.



% Type shorthands:

-type count() :: basic_utils:count().
-type error_reason() :: basic_utils:error_reason().

-type ustring() :: text_utils:ustring().

-type seconds() :: unit_utils:seconds().

% No shorthand, as too ambiguous (w.r.t. start time) to be used here:
%-type milliseconds() :: unit_utils:milliseconds().

-type timestamp() :: time_utils:timestamp().
-type dhms_duration() :: time_utils:dhms_duration().
-type ms_duration() :: time_utils:ms_duration().

-type registration_name() :: naming_utils:registration_name().
-type registration_scope() :: naming_utils:registration_scope().



-doc "Constructs the main (singleton), default US scheduler.".
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->

	% First the direct mother classes, then this class-specific actions:
	SrvState = class_USServer:construct( State,
		?trace_categorize("Main US Scheduler"),
		?us_common_scheduler_registration_name,
		?us_common_scheduler_registration_scope ),

	init_common( SrvState ).



-doc "Constructs a (named, unregistered) US scheduler.".
-spec construct( wooper:state(), ustring() ) -> wooper:state().
construct( State, SchedulerName ) ->

	% First the direct mother classes, then this class-specific actions:
	% (traps EXITs)
	%
	SrvState = class_USServer:construct( State,
										 ?trace_categorize(SchedulerName) ),

	init_common( SrvState ).



-doc "Constructs a (named, registered with specified scope) US scheduler.".
-spec construct( wooper:state(), ustring(), registration_name(),
				 registration_scope() ) -> wooper:state().
construct( State, SchedulerName, RegistrationName, RegistrationScope ) ->

	% First the direct mother classes, then this class-specific actions:
	SrvState = class_USServer:construct( State,
		?trace_categorize(SchedulerName), RegistrationName, RegistrationScope ),

	init_common( SrvState ).



% (helper)
-spec init_common( wooper:state() ) -> wooper:state().
init_common( State ) ->

	EmptyTable = table:new(),

	ReadyState = setAttributes( State, [ { task_table, EmptyTable },
										 { schedule_plan, [] },
										 { timer_table, EmptyTable },
										 { next_task_id, 1 } ] ),

	trace_bridge:notice_fmt( "Scheduler ready, at ~ts.",
							 [ get_current_timestamp_string( ReadyState ) ] ),

	ReadyState.



-doc "Overridden destructor.".
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?info_fmt( "Deleting ~ts", [ to_string( State ) ] ),

	% Cancelling all still-live timers, while resisting to any error:
	[ timer:cancel( TimerRef )
		|| TimerRef <- table:values( ?getAttr(timer_table) ) ],

	State.




% Method section.


-doc """
Triggers immediately (synchronously) the specified one-shot task: the specified
command will be triggered at once, a single time, being assigned to the actuator
process.

Returns either `task_done` if the task was done on the fly (hence is already
triggered; then no task identifier applies), or `{'task_registered', TaskId}` if
it is registered for a later trigger (then its assigned task identifier is
returned).
""".
-spec triggerOneshotTask( wooper:state(), task_command(), actuator_pid() ) ->
								request_return( task_registration_outcome() ).
triggerOneshotTask( State, UserTaskCommand, UserActPid ) ->

	% Note: in this case the sender could have contacted directly the actuator.

	{ NewState, Result } = registerOneshotTask( State, UserTaskCommand,
												_StartTime=asap, UserActPid ),

	wooper:return_state_result( NewState, Result ).



-doc """
Registers (synchronously) the specified one-shot task: the specified command
will be executed once, at the specified time, being assigned to the requesting
process (as actuator).

Returns either `task_done` if the task was done on the fly (hence is already
triggered; then no task identifier applies), or `{'task_registered', TaskId}` if
it is registered for a later trigger (then its assigned task identifier is
returned).

Note: if the deadline is specified in absolute terms (e.g. as `{{2020,3,22},
{16,1,48}}`), the conversion to internal time will be done immediately (at task
submission time), resulting in any system time change (e.g. DST) not being taken
into account (as the respect of actual periodicities is preferred over the one
of literal timestamps).
""".
-spec registerOneshotTask( wooper:state(), task_command(), start_time() ) ->
								request_return( task_registration_outcome() ).
registerOneshotTask( State, UserTaskCommand, UserStartTime ) ->

	{ NewState, Result } = registerTask( State, UserTaskCommand, UserStartTime,
		_Periodicity=once, _Count=1, _ActPid=?getSender() ),

	wooper:return_state_result( NewState, Result ).


-doc """
Registers (synchronously) the specified one-shot task: the specified command
will be executed once, after the specified duration, being assigned to the
requesting process (as actuator).

Returns either `task_done` if the task was done on the fly (hence is already
triggered; then no task identifier applies), or `{'task_registered', TaskId}` if
it is registered for a later trigger (then its assigned task identifier is
returned).
""".
-spec registerOneshotTaskIn( wooper:state(), task_command(),
		dhms_duration() | seconds() ) ->
			request_return( task_registration_outcome() ).
registerOneshotTaskIn( State, UserTaskCommand, AfterDuration ) ->
	StartTime = time_utils:timestamp_in( AfterDuration ),
	{ NewState, Result } = registerTask( State, UserTaskCommand, StartTime,
		_Periodicity=once, _Count=1, _ActPid=?getSender() ),

	wooper:return_state_result( NewState, Result ).



-doc """
Registers (synchronously) the specified one-shot task: the specified command
will be executed once, at the specified time, as assigned to requesting and
specified actuator process.

Returns either `task_done` if the task was done on the fly (hence is already
triggered; then no task identifier applies), or `{'task_registered', TaskId}` if
it is registered for a later trigger (then its assigned task identifier is
returned).

Note: if the deadline is specified in absolute terms (e.g. as `{{2020,3,22},
{16,1,48}}`), the conversion to internal time will be done immediately (at task
submission time), resulting in any system time change (e.g. DST) not being taken
into account (as the respect of actual periodicities is preferred over the one
of literal timestamps).
""".
-spec registerOneshotTask( wooper:state(), task_command(), start_time(),
			actuator_pid() ) -> request_return( task_registration_outcome() ).
registerOneshotTask( State, UserTaskCommand, UserStartTime, UserActPid ) ->

	{ NewState, Result } = registerTask( State, UserTaskCommand, UserStartTime,
		_Periodicity=once, _Count=1, UserActPid ),

	wooper:return_state_result( NewState, Result ).



-doc """
Registers (synchronously) the specified (potentially periodical) task: the
specified command will be executed starting immediately (in a flexible manner),
at the specified user periodicity and indefinitely, being assigned to the
requesting process (as actuator).

Returns either `task_done` if the task was a one-shot one that was done on the
fly (hence is already triggered, in a case where no task identifier applies
since it is fully completed), or `{'task_registered', TaskId}` if it is
registered for a later trigger (then its assigned task identifier is returned).
""".
-spec registerTask( wooper:state(), task_command(), user_periodicity() ) ->
							request_return( task_registration_outcome() ).
registerTask( State, UserTaskCommand, UserPeriodicity ) ->

	{ RegOutcome, RegState } = register_task( UserTaskCommand,
		_StartTime=flexible, UserPeriodicity, _Count=unlimited,
		_UserActPid=?getSender(), State ),

	wooper:return_state_result( RegState, RegOutcome ).



-doc """
Registers (synchronously) the specified (potentially periodical) task: the
specified command will be executed starting immediately (in a flexible manner),
at the specified user periodicity, for the specified number of times, being
assigned to the requesting process (as actuator).

Returns either `task_done` if the task was a one-shot one that was done on the
fly (hence is already triggered, in a case where no task identifier applies
since it is fully completed), or `{'task_registered', TaskId}` if it is
registered for a later trigger (then its assigned task identifier is returned).
""".
-spec registerTask( wooper:state(), task_command(), user_periodicity(),
			schedule_count() ) -> request_return( task_registration_outcome() ).
registerTask( State, UserTaskCommand, UserPeriodicity, UserCount ) ->

	{ RegOutcome, RegState } = register_task( UserTaskCommand,
		_StartTime=flexible, UserPeriodicity, UserCount,
		_UserActPid=?getSender(), State ),

	wooper:return_state_result( RegState, RegOutcome ).



-doc """
Registers (synchronously) the specified (potentially periodical) task: the
specified command will be executed starting from the specified time, at the
specified user periodicity, for the specified number of times, being assigned to
the requesting process.

Returns either `task_done` if the task was a one-shot one that was done on the
fly (hence is already triggered, in a case where no task identifier applies
since it is fully completed), or `{'task_registered', TaskId}` if it is
registered for a later trigger (then its assigned task identifier is returned).

Note: if the deadline is specified in absolute terms (e.g. as `{{2020,3,22},
{16,1,48}}`), the conversion to internal time will be done immediately (at task
submission time), resulting in any future system time change (e.g. DST) not
being taken into account at this level (as the respect of actual periodicities
is preferred over the one of literal timestamps).
""".
-spec registerTask( wooper:state(), task_command(), start_time(),
					user_periodicity(), schedule_count() ) ->
						request_return( task_registration_outcome() ).
registerTask( State, UserTaskCommand, UserStartTime, UserPeriodicity,
			  UserCount ) ->

	{ RegOutcome, RegState } = register_task( UserTaskCommand, UserStartTime,
		UserPeriodicity, UserCount, _UserActPid=?getSender(), State ),

	wooper:return_state_result( RegState, RegOutcome ).



-doc """
Registers asynchronously (hence with neither result - not even the task
identifier, nor synchronisation) the specified (potentially periodical) task:
the specified command will be executed starting from the specified time, at the
specified user periodicity, for the specified number of times, being assigned to
the requesting process.

Note: if the deadline is specified in absolute terms (e.g. as `{{2020,3,22},
{16,1,48}}`), the conversion to internal time will be done immediately (at task
submission time), resulting in any future system time change (e.g. DST) not
being taken into account at this level (as the respect of actual periodicities
is preferred over the one of literal timestamps).
""".
-spec registerTaskAsync( wooper:state(), task_command(), start_time(),
					user_periodicity(), schedule_count() ) -> oneway_return().
registerTaskAsync( State, UserTaskCommand, UserStartTime, UserPeriodicity,
				   UserCount ) ->

	{ RegOutcome, RegState } = register_task( UserTaskCommand, UserStartTime,
		UserPeriodicity, UserCount, _UserActPid=?getSender(), State ),

	cond_utils:if_defined( us_common_debug_scheduling,
		?debug_fmt( "Asynchronous registering of task '~p' resulted in "
			"the following ignored outcome: ~p.",
			[ UserTaskCommand, RegOutcome ] ),
		basic_utils:ignore_unused( RegOutcome ) ),

	wooper:return_state( RegState ).



-doc """
Registers (synchronously) the specified (potentially periodical) task: the
specified command will be executed starting from the specified time, at the
specified user periodicity, for the specified number of times, being assigned to
the specified actuator process.

Returns either `task_done` if the task was a one-shot one that was done on the
fly (hence is already triggered, in a case where no task identifier applies
since it is fully completed), or `{'task_registered', TaskId}` if it is
registered for a later trigger (then its assigned task identifier is returned).

Note: if the deadline is specified in absolute terms (e.g. as `{{2020,3,22},
{16,1,48}}`), the conversion to internal time will be done immediately (at task
submission time), resulting in any future system time change (e.g. DST) not
being taken into account at this level (as the respect of actual periodicities
is preferred over the one of literal timestamps).
""".
-spec registerTask( wooper:state(), task_command(), start_time(),
					user_periodicity(), schedule_count(), actuator_pid() ) ->
						request_return( task_registration_outcome() ).
registerTask( State, UserTaskCommand, UserStartTime, UserPeriodicity,
			  UserCount, UserActPid ) ->

	{ RegOutcome, RegState } = register_task( UserTaskCommand, UserStartTime,
		UserPeriodicity, UserCount, UserActPid, State ),

	wooper:return_state_result( RegState, RegOutcome ).



-doc """
Registers asynchronously (hence with neither result - not even the task
identifier, nor synchronisation) the specified (potentially periodical) task:
the specified command will be executed starting from the specified time, at the
specified user periodicity, for the specified number of times, being assigned to
the specified actuator process.

Note: if the deadline is specified in absolute terms (e.g. as `{{2020,3,22},
{16,1,48}}`), the conversion to internal time will be done immediately (at task
submission time), resulting in any future system time change (e.g. DST) not
being taken into account at this level (as the respect of actual periodicities
is preferred over the one of literal timestamps).
""".
-spec registerTaskAsync( wooper:state(), task_command(), start_time(),
					user_periodicity(), schedule_count(), actuator_pid() ) ->
											oneway_return().
registerTaskAsync( State, UserTaskCommand, UserStartTime, UserPeriodicity,
				   UserCount, UserActPid ) ->

	{ RegOutcome, RegState } = register_task( UserTaskCommand, UserStartTime,
		UserPeriodicity, UserCount, UserActPid, State ),

	cond_utils:if_defined( us_common_debug_scheduling,
		?debug_fmt( "Asynchronous registering of task '~p' resulted in "
			"the following ignored outcome: ~p.",
			[ UserTaskCommand, RegOutcome ] ),
		basic_utils:ignore_unused( RegOutcome ) ),

	wooper:return_state( RegState ).



-doc """
The actual registering of new tasks.

(helper)
""".
-spec register_task( task_command(), start_time(), user_periodicity(),
					 schedule_count(), actuator_pid(), wooper:state() ) ->
							{ task_registration_outcome(), wooper:state() }.
register_task( UserTaskCommand, UserStartTime, UserPeriodicity, UserCount,
			   UserActPid, State ) ->

	% Checks and canonicalises specified elements:
	TaskCommand = vet_task_command( UserTaskCommand, State ),
	MsDurationBeforeStart = vet_start_time( UserStartTime, State ),
	Count = vet_count( UserCount, State ),
	MaybePeriodicity = vet_periodicity( UserPeriodicity, Count, State ),
	ReqPid = ?getSender(),
	ActPid = vet_actuator_pid( UserActPid ),

	ActStr = case ActPid of

		ReqPid ->
			"which is also the requester";

		_ ->
			text_utils:format( "whereas the requester is ~w", [ ReqPid ] )

	end,

	HappenStr = case MsDurationBeforeStart of

		0 ->
			"immediately";

		_ ->
			text_utils:format( "in ~ts",
				[ time_utils:duration_to_string( MsDurationBeforeStart ) ] )

	end,

	?info_fmt( "Registering task whose command is '~p', whose declared start "
		"time is ~ts (hence to happen ~ts), to be triggered ~ts with ~ts "
		"on actuator ~w (~ts).",
		[ TaskCommand, start_time_to_string( UserStartTime ), HappenStr,
		  schedule_count_to_string( Count ),
		  periodicity_to_string( MaybePeriodicity ), ActPid, ActStr ] ),

	% Immediate launch requested?
	case MsDurationBeforeStart of

		% Immediate launch here:
		0 ->
			% Thus, in all cases:
			launch_task( TaskCommand, ActPid, State ),

			case MaybePeriodicity of

				undefined ->
					% Just to be executed once (implied and checked: Count=1).
					%
					% Not even recording it then, it was just fire and forget:
					% neither task entry, just updating the task count.
					%
					{ task_done, incrementAttribute( State, next_task_id ) };

				MsPeriod ->
					case decrement_count( Count ) of

						0 ->
							% Here also, Count was 1, same case as before, no
							% future for this task:
							%
							{ task_done,
							  incrementAttribute( State, next_task_id ) };

						NewCount ->
							% The count is thus now still 1 or more, or
							% unlimited; in all cases a next scheduling will
							% happen, and must be recorded:

							TaskId = ?getAttr(next_task_id),
							NowMs = get_current_schedule_offset( State ),
							NextSchedule = NowMs + MsPeriod,

							TI = #task_entry{ id=TaskId,
											  command=TaskCommand,
											  next_schedule=NextSchedule,
											  periodicity=MsPeriod,
											  count=NewCount,
											  schedule_count=1,
											  started_on=NowMs,
											  last_schedule=NowMs,
											  requester_pid=ReqPid,
											  actuator_pid=ActPid },

							RegState = register_task_schedule( TaskId, TI,
								NextSchedule, MsPeriod, State ),

							{ { task_registered, TaskId }, RegState }

					end

			end;

		% Deferred launch here (most common case):
		_ ->
			TaskId = ?getAttr(next_task_id),
			NowMs = get_current_schedule_offset( State ),
			NextSchedule = NowMs + MsDurationBeforeStart,
			TI = #task_entry{ id=TaskId,
							  command=TaskCommand,
							  next_schedule=NextSchedule,
							  periodicity=MaybePeriodicity,
							  count=Count,

							  % Defaults:
							  %schedule_count=0
							  %started_on=undefined,
							  %last_schedule=undefined,

							  requester_pid=ReqPid,
							  actuator_pid=ActPid },

			RegState = register_task_schedule( TaskId, TI, NextSchedule,
											   MsDurationBeforeStart, State ),

			cond_utils:if_defined( us_common_debug_scheduling,
				?debug_fmt( "Resulting scheduler state: ~ts.",
							[ to_string( RegState ) ] ) ),

			{ { task_registered, TaskId }, RegState }

	end.



-doc """
Unregisters the specified task, based on its identifier.

Returns a suitable outcome to the caller.
""".
-spec unregisterTask( wooper:state(), task_id() ) ->
						request_return( task_unregistration_outcome() ).
unregisterTask( State, TaskId ) when is_integer( TaskId ) andalso TaskId > 0 ->
	{ Outcome, NewState } = unregister_task( TaskId, State ),
	wooper:return_state_result( NewState, Outcome );

unregisterTask( _State, Other ) ->
	throw( { invalid_unregister_task_id, Other } ).



-doc """
Unregisters the specified tasks, based on their identifier.

Returns outcomes in order to the caller.
""".
-spec unregisterTasks( wooper:state(), [ task_id() ] ) ->
							request_return( [ task_unregistration_outcome() ] ).
unregisterTasks( State, TaskIds ) when is_list( TaskIds ) ->

	{ RevOutcomes, NewState } = lists:foldl(
		fun( TaskId, _Acc={ AccOutcomes, AccState } ) ->
			{ Outcome, NewAccState } = unregister_task( TaskId, AccState ),
			{ [ Outcome | AccOutcomes ], NewAccState }
		end,
		_Acc0={ [], State },
		_List=TaskIds ),

	wooper:return_state_result( NewState, lists:reverse( RevOutcomes ) );

unregisterTasks( _State, Other ) ->
	throw( { invalid_unregister_task_ids, Other } ).



% (helper)
-spec unregister_task( task_id(), wooper:state() ) ->
							{ task_unregistration_outcome(), wooper:state() }.
unregister_task( TaskId, State ) when is_integer( TaskId ) andalso TaskId > 0 ->

	NextTaskId = ?getAttr(next_task_id),

	case TaskId >= NextTaskId of

		% Could not have been allocated:
		true ->
			?error_fmt( "Requested to unregister task #~B, which never existed "
				"(as the next task identifier is #~B).",
				[ TaskId, NextTaskId ] ),
			{ { task_unregistration_failed, never_existed, TaskId }, State };

		false ->
			case table:extract_entry_if_existing( TaskId,
												  ?getAttr(task_table) ) of

				% Already dropped, hence done:
				false ->
					?info_fmt( "Unregistered task #~B, which was actually "
							   "already done.", [ TaskId ] ),
					{ { task_already_done, TaskId }, State };

				{ TaskEntry, ShrunkTaskTable } ->

					?info_fmt( "Task #~B unregistered, was: ~ts",
						[ TaskId, task_entry_to_string( TaskEntry, State ) ] ),

					SchedulePlan = ?getAttr(schedule_plan),

					case unschedule_task( TaskId,
							TaskEntry#task_entry.next_schedule, SchedulePlan,
							?getAttr(timer_table) ) of

						not_found ->

							?error_fmt( "Internal error: task id #~B not found "
								"in schedule plan, which ~ts",
								[ TaskId, schedule_plan_to_string( SchedulePlan,
																   State ) ] ),

							NewState = setAttribute( State, task_table,
													 ShrunkTaskTable ),

							{ { task_unregistration_failed, internal_error,
                                TaskId }, NewState };


						{ NewPlan, NewTimerTable } ->

							NewState = setAttributes( State, [
								{ task_table, ShrunkTaskTable },
								{ schedule_plan, NewPlan },
								{ timer_table, NewTimerTable } ] ),

							cond_utils:if_defined( us_common_debug_scheduling,
								?debug_fmt( "Resulting scheduler state after "
									"unregistering: ~ts.",
									[ to_string( NewState ) ] ) ),

							{ { task_unregistered, TaskId }, NewState }

					end

			end

	end;

unregister_task( TaskId, State ) ->
	?error_fmt( "Task unregistering failed, invalid task identifier: '~p'.",
				[ TaskId ] ),
	{ { task_unregistration_failed, invalid_task_id, TaskId }, State }.



-doc """
Unregisters asynchronously the specified task, based on its identifier.

As a result, no outcome will be available to the caller.
""".
-spec unregisterTaskAsync( wooper:state(), task_id() ) -> oneway_return().
unregisterTaskAsync( State, TaskId )
						when is_integer( TaskId ) andalso TaskId > 0 ->

	% Already logged:
	{ _Outcome, NewState } = unregister_task( TaskId, State ),

	wooper:return_state( NewState );

unregisterTaskAsync( _State, Other ) ->
	throw( { invalid_unregister_task_id, Other } ).



-doc """
Triggers the specified scheduling.

Expected to be called through a timer having been set beforehand.

Note: a reference() or any other non-guessable element could be used to avoid
any process to be able to interfere by triggering schedulings.
""".
-spec timerTrigger( wooper:state(), schedule_offset() ) -> oneway_return().
timerTrigger( State, ScheduleOffsetMs ) ->

	NowMs = get_current_schedule_offset( State ),

	cond_utils:if_defined( us_common_debug_scheduling,
		begin
			% As long as a drift is below this millisecond threshold, we do not
			% worry:
			%
			OffsetThresholdMs = 250,

			DiffMs = NowMs - ScheduleOffsetMs,

			erlang:abs( DiffMs ) > OffsetThresholdMs andalso
				?warning_fmt( "Triggered for offset ~B (~ts), while being at "
					"offset ~B (~ts), hence with a signed drift of ~ts "
					"(thus being late iff positive).",
					[ ScheduleOffsetMs,
					  get_timestamp_string_for( ScheduleOffsetMs, State ),
					  NowMs, get_timestamp_string_for( NowMs, State ),
					  time_utils:duration_to_string( DiffMs ) ] )
		end ),

	TimerTable = ?getAttr(timer_table),

	% Resorb any pending schedule:
	{ TriggeredPlan, TriggeredTimerTable, TriggeredTaskTable } =
		perform_schedule( ScheduleOffsetMs, NowMs, ?getAttr(schedule_plan),
						  TimerTable, ?getAttr(task_table), State ),

	cond_utils:if_defined( us_common_debug_scheduling,
		?debug_fmt( "After having handled the triggered tasks for offset ~B, "
			"went from ~ts to ~ts", [ ScheduleOffsetMs,
				timer_table_to_string( TimerTable, State ),
				timer_table_to_string( TriggeredTimerTable, State ) ] ) ),

	% Intercept any scheduling already late and process it:
	{ LatePlan, LateTimerTable, LateTaskTable } = piggy_back_late_schedules(
		NowMs, TriggeredPlan, TriggeredTimerTable, TriggeredTaskTable, State ),

	TrigState = setAttributes( State, [ { schedule_plan, LatePlan },
										{ timer_table, LateTimerTable },
										{ task_table, LateTaskTable } ] ),

	cond_utils:if_defined( us_common_debug_scheduling,
		?debug_fmt( "Resulting scheduler state after performing trigger: ~ts.",
					[ to_string( TrigState ) ] ) ),

	wooper:return_state( TrigState ).



% (helper; State specified for traces only)
%
% Take care of any lingering schedule:
-spec perform_schedule( schedule_offset(), schedule_offset(), schedule_plan(),
						timer_table(), task_table(), wooper:state() ) ->
							{ schedule_plan(), timer_table(), task_table() }.
% Abnormal case of an unhandled past schedule (we still compensate for it):
perform_schedule( ScheduleOffsetMs, NowMs,
				  _SchedulePlan=[ { OffMs, TaskIds } | T ], TimerTable,
				  TaskTable, State ) when OffMs < ScheduleOffsetMs ->

	?error_fmt( "While scheduling target offset ~B (~ts), found past "
		"non-scheduled offset ~B (~ts), triggering its delayed tasks first: "
		"#~ts.",
		[ ScheduleOffsetMs, get_timestamp_string_for( ScheduleOffsetMs, State ),
		  OffMs, get_timestamp_string_for( OffMs, State ),
		  text_utils:integers_to_listed_string( TaskIds ) ] ),

	% Using OffMs (second parameter) rather than ScheduleOffsetMs here:
	{ NewPlan, NewTimerTable, NewTaskTable } = trigger_tasks( TaskIds,
		OffMs, NowMs, _NewerPlan=T, TimerTable, TaskTable, State ),

	% Drops this offset entry:
	perform_schedule( ScheduleOffsetMs, NowMs, NewPlan, NewTimerTable,
					  NewTaskTable, State );


% Matching offsets:
perform_schedule( ScheduleOffsetMs, NowMs,
				  _SchedulePlan=[ { ScheduleOffsetMs, TaskIds } | T ],
				  TimerTable, TaskTable, State ) ->

	%?debug_fmt( "Normal scheduling of offset ~B (~ts), "
	%   "triggering its tasks: ~w.",
	%   [ ScheduleOffsetMs, get_timestamp_string_for( ScheduleOffsetMs, State ),
	%     TaskIds ] ),

	% Dropping current offset, stop recursing here, keeping the next schedules
	% to come, returning {NewPlan, NewTimerTable, NewTaskTable}:
    %
    trigger_tasks( TaskIds, ScheduleOffsetMs, NowMs, _NewerPlan=T, TimerTable,
                   TaskTable, State );


% This triggered offset is not found; this is possible: should a T1 timer be
% late, a later one T2 might trigger before (despite registered_offset(T1) <
% registered_offset(T2)), and T2 should have taken care of all preceding tasks
% still registered (through first clause), including those of T1. So, when T1 is
% finally triggered, none of its tasks is left, and this should not be then a
% fatal error.
%
% So, in all other cases (either future offsets exist, or none at all), we do
% not trigger offsets anymore this time, we just disregard the current one:
%
perform_schedule( ScheduleOffsetMs, _NowMs, SchedulePlan, TimerTable, TaskTable,
				  State ) ->

	?warning_fmt( "Triggered schedule offset ~B (~ts) not found (whereas "
		"schedule plan ~ts), ignoring it, as supposing this is a late "
		"scheduling already applied.",
		[ ScheduleOffsetMs, get_timestamp_string_for( ScheduleOffsetMs, State ),
		  schedule_plan_to_string( SchedulePlan, State ) ] ),

	% Hopefully this lacking timer can still be cancelled:
	ShrunkTimerTable = remove_timer( ScheduleOffsetMs, TimerTable ),

	{ SchedulePlan, ShrunkTimerTable, TaskTable }.



-doc """
Flushes and executes any schedule known to be already late.

This extra security should be useless, as the very last clause of
trigger_tasks/7, when testing whether NextScheduleMs is lower or equal to NowMs,
should have already managed such cases.
""".
-spec piggy_back_late_schedules( schedule_offset(), schedule_plan(),
						timer_table(), task_table(), wooper:state() ) ->
							{ schedule_plan(), timer_table(), task_table() }.
piggy_back_late_schedules( NowMs, _SchedulePlan=[ { OffMs, TaskIds } | T ],
		TimerTable, TaskTable, State ) when OffMs =< NowMs ->

	?error_fmt( "Scheduling directly task(s) ~ts that were already "
		"in the past (at ~B, while now is ~B).",
		[ text_utils:integers_to_listed_string( TaskIds ), OffMs,
          NowMs ] ),

	{ NewPlan, NewTimerTable, NewTaskTable } = trigger_tasks( TaskIds,
		OffMs, NowMs, _NewerPlan=T, TimerTable, TaskTable, State ),

	piggy_back_late_schedules( NowMs, NewPlan, NewTimerTable, NewTaskTable,
							   State );

% Here OffMs > NowMs (normal situation):
piggy_back_late_schedules( _NowMs, SchedulePlan, TimerTable, TaskTable,
						   _State ) ->
	{ SchedulePlan, TimerTable, TaskTable }.



-doc """
Triggers the specified tasks, and returns updated schedule plan, timer and task
tables.

Note that:
- the specified offset (ScheduleOffsetMs) is the planned one; if being a late
trigger, it may be significantly in the past of the current offset
- the specified schedule plan is supposed to have already the entry for the
specified schedule offset removed
- the specified State is const (used for traces)

(helper)
""".
-spec trigger_tasks( [ task_id() ], schedule_offset(), schedule_offset(),
			schedule_plan(), timer_table(), task_table(), wooper:state() ) ->
							{ schedule_plan(), timer_table(), task_table() }.
trigger_tasks( _TaskIds=[], ScheduleOffsetMs, _NowMs, SchedulePlan, TimerTable,
			   TaskTable, _State ) ->

    % In the final clause, as a single timer per offset is created, even if
    % multiple tasks are scheduled then:
    %
	ShrunkTimerTable = remove_timer( ScheduleOffsetMs, TimerTable ),

	{ SchedulePlan, ShrunkTimerTable, TaskTable };


trigger_tasks( _TaskIds=[ TaskId | T ], ScheduleOffsetMs, NowMs, SchedulePlan,
			   TimerTable, TaskTable, State ) ->

	{ TaskEntry, ShrunkTaskTable } = table:extract_entry( TaskId, TaskTable ),

	% Check:
	TaskId = TaskEntry#task_entry.id,

	% next_schedule shall at least roughly match.

	cond_utils:if_defined( us_common_debug_scheduling,
		?debug_fmt( "Triggering task #~B (difference between now and "
			"scheduled: ~ts): ~ts.", [ TaskId,
				time_utils:duration_to_string( NowMs - ScheduleOffsetMs ),
				task_entry_to_string( TaskEntry, State ) ] ) ),

	launch_task( TaskEntry#task_entry.command,
				 TaskEntry#task_entry.actuator_pid, State ),

    % In all cases this timer is removed exactly once (by the final clause):
	case decrement_count( TaskEntry#task_entry.count ) of

		0 ->
			?debug_fmt( "Dropping task #~B for good, as fully done.",
						[ TaskId ] ),
			{ SchedulePlan, TimerTable, ShrunkTaskTable };

		% Possibly unlimited:
		NewCount ->
			% Will thus be still scheduled again afterwards.

			% If first scheduling:
			StartOffsetMs = case TaskEntry#task_entry.started_on of

				undefined ->
					% Now rather than scheduled (ScheduleOffsetMs):
					NowMs;

				AlreadyStartOffsetMs ->
					AlreadyStartOffsetMs

			end,

			% Periodicity not expected to be 'undefined' here:
			Periodicity = TaskEntry#task_entry.periodicity,

			% Basing on planned (not actual, i.e. NowMs) offset to (attempt to)
			% resorb any delay:
			%
			NextScheduleMs = ScheduleOffsetMs + Periodicity,

			NewTaskEntry = TaskEntry#task_entry{
				next_schedule=NextScheduleMs,
				count=NewCount,
				schedule_count=TaskEntry#task_entry.schedule_count+1,
				started_on=StartOffsetMs,
				last_schedule=NowMs },

			% Updating previous version thereof:
			NewTaskTable = table:add_entry( TaskId, NewTaskEntry,
											ShrunkTaskTable ),

			case NextScheduleMs > NowMs of

				true ->

					% Knowing that we do not program timer at absolute times but
					% after a given duration, if DurationFromNowMs was just
					% Periodicity, then the duration of the trigger logic would
					% not be taken into account, and delays would accumulate
					% (e.g. 1ms every 5s, leading quickly to too large errors)
					%
					% So we subtract the current error (which is
					% Err = NowMs - ScheduleOffsetMs > 0):
					%
					DurationFromNowMs = Periodicity + ScheduleOffsetMs - NowMs,

					{ NewPlan, NewTimerTable } = insert_task_at( TaskId,
						NextScheduleMs, DurationFromNowMs, SchedulePlan,
						TimerTable ),

					cond_utils:if_defined( us_common_debug_scheduling,
						?debug_fmt( "New plan for offset ~B after trigger "
							"of task #~B: ~ts",
							[ ScheduleOffsetMs, TaskId,
							  schedule_plan_to_string( NewPlan, State ) ] ) ),

					trigger_tasks( T, ScheduleOffsetMs, NowMs, NewPlan,
						NewTimerTable, NewTaskTable, State );

				false ->
					?warning_fmt( "Next scheduling of task #~B to happen in "
						"the past (at offset ~B), forcing it now.",
						[ TaskId, NextScheduleMs ] ),
					trigger_tasks( [ TaskId | T ], ScheduleOffsetMs, NowMs,
						SchedulePlan, TimerTable, NewTaskTable, State )

			end

	end.



-doc "Effective launching of the specified task.".
-spec launch_task( task_command(), actuator_pid(), wooper:state() ) -> void().
launch_task( Cmd, ActuatorPid, State ) ->

	% We want to let the requester be able to specify exactly any command term;
	% otherwise we would have added automatically for example the PID of the
	% sending scheduler and the corresponding task identifier.

	cond_utils:if_defined( us_common_debug_scheduling,
		?debug_fmt( "Sending command '~p' to actuator ~w.",
					[ Cmd, ActuatorPid ] ),
		basic_utils:ignore_unused( State ) ),

	ActuatorPid ! Cmd.



-doc """
Requests this scheduler to log its state.

Useful to check for example that there is no accumulation of lost tasks.
""".
-spec logState( wooper:state() ) -> const_oneway_return().
logState( State ) ->

	?info_fmt( "Reporting current state: ~ts", [ to_string( State ) ] ),
	wooper:const_return().



% onWOOPERExitReceived/3 inherited.



% Static section.


-doc """
Returns the PID of the current, supposedly already-launched, main US scheduler,
waiting if needed.

It is better to obtain the PID of a server each time from the naming service
rather than to resolve and store its PID once for all, as, for an increased
robustness, servers may be restarted (hence any stored PID may not reference a
live process anymore).
""".
-spec get_server_pid () -> static_return( scheduler_pid() ).
get_server_pid() ->

	SchedPid = class_USServer:resolve_server_pid(
        _RegName=?us_common_scheduler_registration_name,
        _RegScope=?us_common_scheduler_registration_scope ),

	wooper:return_static( SchedPid ).



% Helper section.


-doc """
Registers a future scheduling of the specified task.

(both ScheduleOffset and DurationFromNow specified to avoid a recomputation)
""".
-spec register_task_schedule( task_id(), task_entry(), schedule_offset(),
							  ms_duration(), wooper:state() ) -> wooper:state().
register_task_schedule( TaskId, TaskEntry, ScheduleOffsetMs, DurationFromNowMs,
						State ) ->

	%?debug_fmt
	?notice_fmt( "Registering task #~B for schedule offset ~B (duration from "
		"now: ~ts): ~ts.", [ TaskId, ScheduleOffsetMs,
			time_utils:duration_to_string( DurationFromNowMs ),
			task_entry_to_string( TaskEntry, State ) ] ),

	NewTaskTable = table:add_new_entry( TaskId, TaskEntry,
										?getAttr(task_table) ),

	{ NewPlan, NewTimerTable } = insert_task_at( TaskId, ScheduleOffsetMs,
		DurationFromNowMs, ?getAttr(schedule_plan), _AccPlan=[],
		?getAttr(timer_table) ),

	RegState = setAttributes( State, [ { task_table, NewTaskTable },
									   { schedule_plan, NewPlan },
									   { timer_table, NewTimerTable },
									   { next_task_id, TaskId+1 } ] ),

	%cond_utils:if_defined( us_common_debug_scheduling,
	%   ?debug_fmt( "New plan: ~p~nNew timer table: ~p.",
	%               [ NewPlan, NewTimerTable ] ) ),

	% Even more complete:
	cond_utils:if_defined( us_common_debug_scheduling,
		?debug_fmt( "Resulting scheduler state after registering: ~ts.",
					[ to_string( RegState ) ] ) ),

	RegState.



-doc """
Inserts the specified task at the specified offset in the specified plan.
""".
-spec insert_task_at( task_id(), schedule_offset(), ms_duration(),
		schedule_plan(), timer_table() ) -> { schedule_plan(), timer_table() }.
insert_task_at( TaskId, ScheduleOffsetMs, DurationFromNowMs, Plan,
				TimerTable ) ->

	%NewP = { NewPlan, _NewTimerTable } =
	NewP = insert_task_at( TaskId, ScheduleOffsetMs, DurationFromNowMs, Plan,
						   _AccPlan=[], TimerTable ),

	%trace_bridge:debug_fmt( "After having inserted task #~B at offset ~B "
	%   "(duration from now: ~ts), new plan is:~n ~p",
	%   [ TaskId, ScheduleOffsetMs,
	%     time_utils:duration_to_string( DurationFromNowMs ), NewPlan ] ),

	NewP.


% (helper)
%
% Schedule plan exhausted, adding it at end:
insert_task_at( TaskId, ScheduleOffsetMs, DurationFromNowMs, _SchedulePlan=[],
				AccPlan, TimeTable ) ->

	% Not set yet:
	NewTimeTable = add_timer( ScheduleOffsetMs, DurationFromNowMs, TimeTable ),

	{ lists:reverse( [ { ScheduleOffsetMs, [ TaskId ] } | AccPlan ] ),
	  NewTimeTable };


% Too early in plan, ScheduleOffsetMs not reached, continue:
insert_task_at( TaskId, ScheduleOffsetMs, DurationFromNowMs,
				_SchedulePlan=[ H={ OffMs, _Ids } | T ], AccPlan, TimeTable )
										when OffMs < ScheduleOffsetMs ->
	insert_task_at( TaskId, ScheduleOffsetMs, DurationFromNowMs, T,
					[ H | AccPlan ], TimeTable );

% Current offset found matching ScheduleOffsetMs, registering and stopping:
insert_task_at( TaskId, ScheduleOffsetMs, _DurationFromNowMs,
		_SchedulePlan=[ { ScheduleOffsetMs, Ids } | T ], AccPlan, TimeTable ) ->

	% Timer already set for that offset, so none to add:
	{ lists:reverse( AccPlan )
			++ [ { ScheduleOffsetMs, [ TaskId | Ids ] } | T ],
	  TimeTable };

% Gone past ScheduleOffsetMs target:
insert_task_at( TaskId, ScheduleOffsetMs, DurationFromNowMs,
		SchedulePlan, % Implicit: SchedulePlan=[ { OffMs, Ids } | T ],
		AccPlan, TimeTable ) -> % Implicit: when OffMs > ScheduleOffsetMS

	% Not set yet, registering and stop recursing:
	NewTimeTable = add_timer( ScheduleOffsetMs, DurationFromNowMs, TimeTable ),

	{ lists:reverse( [ { ScheduleOffsetMs, [ TaskId ] } | AccPlan ] )
		++ SchedulePlan, NewTimeTable }.



-doc "Removes the specified task from schedule plan.".
-spec unschedule_task( task_id(), schedule_offset(), schedule_plan(),
			timer_table() ) -> 'not_found' | { schedule_plan(), timer_table() }.
unschedule_task( TaskId, PlannedNextSchedule, SchedulePlan, TimerTable ) ->
	unschedule_task( TaskId, PlannedNextSchedule, SchedulePlan, _AccPlan=[],
					 TimerTable ).


% (helper)
unschedule_task( _TaskId, _PlannedNextSchedule, _SchedulePlan=[], _AccPlan,
				 _TimerTable ) ->
	not_found;

unschedule_task( TaskId, PlannedNextSchedule,
		_SchedulePlan=[ { PlannedNextSchedule, TaskIds } | T ], AccPlan,
		TimerTable ) ->

	case lists:member( TaskId, TaskIds ) of

		true ->
			case lists:delete( TaskId, TaskIds ) of

				% Empty slot now, hence removed as a whole, together with the
				% corresponding timer that is now useless:
				%
				[] ->

					%trace_bridge:debug_fmt(
					%  "Removing empty schedule slot at offset ~B.",
					%  [ PlannedNextSchedule ] ),

					NewSchedPlan = lists:reverse( AccPlan ) ++ T,

					ShrunkTimerTable =
						remove_timer( PlannedNextSchedule, TimerTable ),

					{ NewSchedPlan, ShrunkTimerTable };

				ShrunkTaskIds ->
					{ lists:reverse( AccPlan ) ++
						[ { PlannedNextSchedule, ShrunkTaskIds } | T ],
					  TimerTable }

			end;

		false ->
			not_found

	end;

% Not the shedule offset you are looking for:
unschedule_task( TaskId, PlannedNextSchedule, _SchedulePlan=[ P | T ], Acc,
				 TimerTable ) ->
	unschedule_task( TaskId, PlannedNextSchedule, T, [ P | Acc ], TimerTable ).



-doc "Adds a timer to trigger a future scheduling.".
-spec add_timer( schedule_offset(), ms_duration(), timer_table() ) ->
						timer_table().
add_timer( ScheduleOffsetMs, DurationFromNowMs, TimerTable ) ->

	% WOOPER oneway to be sent to this instance:
	Message = { timerTrigger, [ ScheduleOffsetMs ] },

	% Note that we cannot set an absolute timer, but one relative to the current
	% time (be careful not to accumulate errors between planned and current
	% times, for example by adding constant, "theoritical" durations here):
	%
	case timer:send_after( DurationFromNowMs, Message ) of

		% TimerRef useful to cancel; not expected to be already existing:
		{ ok, TimerRef } ->
			table:add_new_entry( ScheduleOffsetMs, TimerRef, TimerTable );

		{ error, Reason } ->
			throw( { timer_setting_failed, Reason, ScheduleOffsetMs,
					 DurationFromNowMs } )

	end.



-doc "Removes the timer at the specified schedule offset.".
-spec remove_timer( schedule_offset(), timer_table() ) -> timer_table().
remove_timer( ScheduleOffsetMs, TimerTable ) ->

	%trace_bridge:debug_fmt( "Removing timer for schedule offset #~B.",
	%   [ ScheduleOffsetMs ] ),

	{ TimerRef, ShrunkTimerTable } =
		table:extract_entry( ScheduleOffsetMs, TimerTable ),

	case timer:cancel( TimerRef ) of

		{ ok, cancel } ->
			ok;

		{ error, Reason } ->
			trace_bridge:error_fmt( "The cancellation of timer '~p' "
				"failed for schedule offset ~B (reason: '~p').",
				[ TimerRef, ScheduleOffsetMs, Reason ] )
			%throw( { timer_cancellation_failed, Reason,
			%         TimerRef, ScheduleOffset } )

	end,

	ShrunkTimerTable.




-doc """
Returns the time offset of the specified (absolute) time, thus expressed in
internal time, hence relative to the start time of this scheduler.

Corresponds to the (real, actual) number of milliseconds since the start of this
scheduler.
""".
-spec get_current_schedule_offset( wooper:state() ) -> schedule_offset().
get_current_schedule_offset( State ) ->
	time_utils:get_monotonic_time() - ?getAttr(server_start).



-doc "Returns the (approximate) user-level timestamp corresponding to now.".
-spec get_current_timestamp( wooper:state() ) -> timestamp().
get_current_timestamp( State ) ->
	% Better (more homogeneous) than using calendar:local_time/0 for example:
	get_timestamp_for( get_current_schedule_offset( State ), State ).



-doc """
Returns a textual description of the (approximate) user-level timestamp
corresponding to now.
""".
-spec get_current_timestamp_string( wooper:state() ) -> ustring().
get_current_timestamp_string( State ) ->
	get_timestamp_string_for( get_current_schedule_offset( State ), State ).



-doc """
Returns the internal time offset corresponding to this user-level timestamp
(such as `{{2020,3,23},{16,44,0}}`).
""".
-spec get_schedule_offset_for( timestamp(), wooper:state() ) ->
									schedule_offset().
get_schedule_offset_for( UserTimestamp, State ) ->

	% Number of milliseconds since year 0:
	GregorianMillisecs =
		1000 * calendar:datetime_to_gregorian_seconds(
			time_utils:local_to_universal_time( UserTimestamp ) ),

	GregorianMillisecs - ?getAttr(server_gregorian_start).



-doc """
Returns the (approximate) user-level timestamp (e.g. `{{2020,3,23},{16,44,0}}`),
in VM system time (UTC), corresponding to the specified internal time offset.
""".
-spec get_timestamp_for( schedule_offset(), wooper:state() ) -> timestamp().
get_timestamp_for( OffsetMs, State ) ->

	GregorianSecs =
		round( ( OffsetMs + ?getAttr(server_gregorian_start) ) / 1000 ),

	time_utils:universal_to_local_time(
		calendar:gregorian_seconds_to_datetime( GregorianSecs ) ).



-doc """
Returns a textual description of the user-level timestamp corresponding to the
specified internal time offset.
""".
-spec get_timestamp_string_for( schedule_offset(), wooper:state() ) ->
										ustring().
get_timestamp_string_for( OffsetMs, State ) ->
	time_utils:timestamp_to_string( get_timestamp_for( OffsetMs, State ) ).



-doc "Registers the fact that a task has just been triggered once more.".
-spec decrement_count( schedule_count() ) -> option( schedule_count() ).
decrement_count( _Count=unlimited ) ->
	unlimited;

% (guard just for safety, should never be negative):
decrement_count( Count ) when Count > 0 ->
	Count-1.



% Vet helpers, to check and canonicalise.


-doc "Checks and canonicalises this user-specified task command.".
vet_task_command( UserTaskCommand=Oneway, _State ) when is_atom( Oneway ) ->
	UserTaskCommand;

% If Args is not z list, expecting that this is a single non-list element that
% will be considered to be wrapped in a list by the callee, so both are correct
% and they cover all cases:
%
vet_task_command( UserTaskCommand={ Oneway, _Args }, _State )
							when is_atom( Oneway ) ->
	UserTaskCommand;

vet_task_command( UserTaskCommand, State ) ->
	?error_fmt( "Invalid user-specified task command: '~p'.",
				[ UserTaskCommand ] ),
	throw( { invalid_task_command, UserTaskCommand } ).



-doc """
Checks and canonicalises this user-specified start time: returns the number of
milliseconds before starting any corresponding task (possibly zero).
""".
-spec vet_start_time( term(), wooper:state() ) -> ms_duration().
vet_start_time( _UserStartTime=asap, _State ) ->
	0;

vet_start_time( _UserStartTime=flexible, _State ) ->
    % At least currently:
	0;

vet_start_time( StartTimeInSecs, _State ) when is_integer( StartTimeInSecs ) ->
	case StartTimeInSecs > 0 of

		true ->
			1000 * StartTimeInSecs;

		false ->
			throw( { non_strictly_positive_start_duration, StartTimeInSecs } )

	end;

vet_start_time( _UserStartTime=StartTime, State ) ->

	case time_utils:is_timestamp( StartTime ) of

		true ->

			% We consider that this scheduler-side local time (including time
			% zone and Daylight Saving Time) and the time received from the
			% user obey the same conventions:
			%
			Now = time_utils:get_timestamp(),

			% So the duration (only value that will matter) shall be correct:
			case time_utils:get_duration( Now, StartTime ) of

				SecD when SecD > 0 ->
					% In the future, perfect:
					1000 * SecD;

				SecD ->
					?warning_fmt( "Specified user start time (~p, i.e. ~ts) "
						"is in the past (i.e. ~ts before current time, which "
						"is ~ts), requesting immediate scheduling instead.",
						[ StartTime,
						  time_utils:timestamp_to_string( StartTime ),
						  time_utils:duration_to_string( -1000 * SecD ),
						  time_utils:timestamp_to_string( Now ) ] ),
					0

			end;

		false ->
			% Durations are more reliable:
			case time_utils:is_dhms_duration( StartTime ) of

				true ->
					MsDuration = 1000 *
						time_utils:dhms_to_seconds( StartTime ),

					case MsDuration of

						D when D > 0 ->
							% In the future, perfect:
							D;

						D ->
							?warning_fmt( "Specified user duration "
								"(~p, i.e. ~ts) is negative (i.e. in the "
								"past), requesting immediate scheduling.",
								[ StartTime,
								  time_utils:duration_to_string( D ) ] ),
							0

					end;

				false ->
					?error_fmt( "Invalid user-specified start time (neither "
						"timestamp nor DHMS duration):~n ~p", [ StartTime ] ),
					throw( { invalid_start_time, StartTime } )

			end

	end.



-doc "Returns the user-specified schedule count.".
-spec vet_count( term(), wooper:state() ) -> schedule_count().
vet_count( ScheduleCount=unlimited, _State ) ->
	ScheduleCount;

vet_count( C, _State ) when is_integer( C ) andalso C > 0 ->
	C;

vet_count( Other, State ) ->
	?error_fmt( "Invalid user-specified schedule count: ~p.", [ Other ] ),
	throw( { invalid_schedule_count, Other } ).



-doc "Returns any user-specified periodicity.".
-spec vet_periodicity( term(), term(), wooper:state() ) ->
								option( periodicity() ).
vet_periodicity( _UserPeriodicity=once, _Count=1, _State ) ->
	undefined;

vet_periodicity( UserPeriodicity=once, Count, State ) ->

	?error_fmt( "Task periodicity is 'once', whereas specified schedule count "
				"is ~p.", [ Count ] ),

	throw( { periodicity_count_mismatch, UserPeriodicity, Count } );

vet_periodicity( UserPeriodicity, _Count, State ) ->
	vet_user_periodicity( UserPeriodicity, State ).



-doc """
Returns a vetted, internal periodicity.

(helper, defined for reuse)
""".
-spec vet_user_periodicity( term(), wooper:state() ) -> ms_duration().
vet_user_periodicity( UserPeriodicity, State ) ->

	case time_utils:is_dhms_duration( UserPeriodicity ) of

		true ->
			case 1000 * time_utils:dhms_to_seconds( UserPeriodicity ) of

				D when D > 0 ->
					D;

				D ->
					?error_fmt( "Invalid user-specified non strictly positive "
						"task periodicity ~p, i.e. ~ts).",
						[ UserPeriodicity,
						  time_utils:duration_to_string( D ) ] ),
					throw( { non_strictly_positive_user_periodicity,
							 UserPeriodicity } )

			end;

		false ->
			case is_integer( UserPeriodicity ) andalso UserPeriodicity > 0 of

				% Then are seconds:
				true ->
					1000 * UserPeriodicity;

				false ->
					throw( { invalid_user_periodicity, UserPeriodicity } )

			end

	end.



-doc "Returns the user-specified actuator PID.".
-spec vet_actuator_pid( term() ) -> actuator_pid().
vet_actuator_pid( Pid ) when is_pid( Pid ) ->
	Pid;

vet_actuator_pid( Other ) ->
	throw( { invalid_actuator_pid, Other } ).




% Section for textual representations.


-doc "Returns a textual description of this server.".
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	TaskStr = case table:enumerate( ?getAttr(task_table) ) of

		[] ->
			"no task";

		TaskPairs ->
			TaskDescs = [ text_utils:format( "task #~B: ~ts",
				[ TId, task_entry_to_string( TE, State ) ] )
							|| { TId, TE } <- lists:sort( TaskPairs ) ],
			text_utils:format( "~B tasks: ~ts", [ length( TaskPairs ),
				text_utils:strings_to_string( TaskDescs ) ] )

	end,

	SchedStr = schedule_plan_to_string( ?getAttr(schedule_plan), State ),

	TimerStr = timer_table_to_string( ?getAttr(timer_table), State ),

	TotalTaskCount = ?getAttr(next_task_id) - 1,

	text_utils:format( "US scheduler, a ~ts; "
		"registering ~ts (with a total of ~B task(s) already declared); "
		"current schedule ~ts; with ~ts",
		[ class_USServer:to_string( State ), TaskStr, TotalTaskCount,
		  SchedStr, TimerStr ] ).



-doc "Returns a textual description of the specified schedule plan.".
-spec schedule_plan_to_string( schedule_plan(), wooper:state() ) -> ustring().
schedule_plan_to_string( _SchedulePlan=[], _State ) ->
	"is empty";

schedule_plan_to_string( SchedulePlan, State ) ->

	NowOffset = get_current_schedule_offset( State ),
	text_utils:format( "registers ~B trigger(s) (while being at offset ~B, "
		"i.e. ~ts): ~ts",
		[ length( SchedulePlan ), NowOffset,
		  get_timestamp_string_for( NowOffset, State ),
		  text_utils:strings_to_string(
			[ trigger_to_string( P, State ) || P <- SchedulePlan ] ) ] ).



-doc "Returns a textual description of the specified timer table.".
-spec timer_table_to_string( timer_table(), wooper:state() ) -> ustring().
timer_table_to_string( TimerTable, State ) ->

	case table:keys( TimerTable ) of

		[] ->
			"no timer set";

		[ Offset ] ->
			text_utils:format( "a single timer set, at offset ~B (~ts)",
				[ Offset, get_timestamp_string_for( Offset, State ) ] );

		Offsets ->
			text_utils:format( "~B timers set, at: ~ts", [ length( Offsets ),
				text_utils:strings_to_string(
					[ text_utils:format( "offset ~B (~ts)",
						[ Off, get_timestamp_string_for( Off, State ) ] )
							|| Off <- Offsets ] ) ] )

	end.



-doc "Returns a textual description of the specified schedule pair.".
-spec trigger_to_string( schedule_pair(), wooper:state() ) -> ustring().
trigger_to_string( { Offset, TaskIds }, State ) ->
	text_utils:format( "at offset ~B (~ts), ~B task(s) registered: ~w",
		[ Offset, get_timestamp_string_for( Offset, State ),
		  length( TaskIds ), TaskIds ] ).



-doc "Returns a textual description of the specified task entry.".
-spec task_entry_to_string( task_entry(), wooper:state() ) -> ustring().
task_entry_to_string( #task_entry{ id=_TaskId,
							 command=Cmd,
							 next_schedule=NextSchedOffset,
							 periodicity=Periodicity,
							 count=Count,
							 schedule_count=SchedCount,
							 started_on=MaybeStartOffset,
							 last_schedule=MaybeLastOffset,
							 requester_pid=RequesterPid,
							 actuator_pid=ActuatorPid }, State ) ->

	ExecStr = case MaybeStartOffset of

		undefined ->
			SchedCount = 0,
			"never executed yet";

		StartOffset ->
			case SchedCount of

				1 ->
					% First and last are the same here:
					MaybeStartOffset = MaybeLastOffset,
					text_utils:format(
						"already executed a single time, at offset ~B (~ts)",
						[ StartOffset,
						  get_timestamp_string_for( StartOffset, State ) ] );

				% Expected higher than 1:
				C when C > 1 ->
					text_utils:format( "already executed ~B times, the first "
						"at offset ~B (~ts) and the last at offset ~B (~ts)",
						[ C, StartOffset,
						  get_timestamp_string_for( StartOffset, State ),
						  MaybeLastOffset,
						  get_timestamp_string_for( MaybeLastOffset, State ) ] )

			end

	end,

	NextSchedTime = get_timestamp_string_for( NextSchedOffset, State ),

	PeriodStr = periodicity_to_string( Periodicity ),

	CountStr = schedule_count_to_string( Count ),

	text_utils:format( "task to trigger command '~p' on actuator ~w, ~ts, "
		"to be scheduled next at offset ~B (~ts) according to ~ts, "
		"and for ~ts; it was declared by ~w",
		[ Cmd, ActuatorPid, ExecStr, NextSchedOffset, NextSchedTime, PeriodStr,
		  CountStr, RequesterPid ] ).



-doc "Returns a textual description of the specified periodicity.".
-spec periodicity_to_string( periodicity() ) -> ustring().
% Should never happen:
periodicity_to_string( _Periodicity=undefined ) ->
	"no periodicity";

periodicity_to_string( Periodicity ) ->
	text_utils:format( "a periodicity of ~ts",
					   [ time_utils:duration_to_string( Periodicity ) ] ).



-doc "Returns a textual description of the specified schedule count.".
-spec schedule_count_to_string( schedule_count() ) -> ustring().
schedule_count_to_string( _Count=unlimited ) ->
	"an unlimited number of times";

schedule_count_to_string( _Count=1 ) ->
	"a single time";

schedule_count_to_string( Count ) ->
	text_utils:format( "~B times", [ Count ] ).



-doc "Returns a textual description of the specified start time information.".
-spec start_time_to_string( start_time() ) -> ustring().
start_time_to_string( _StartTime=asap ) ->
	"as soon as possible";

start_time_to_string( _StartTime=flexible ) ->
	"flexible";

start_time_to_string( _StartTime=Seconds ) when is_integer( Seconds ) ->
	text_utils:format( "in ~ts",
					   [ time_utils:duration_to_string( 1000 * Seconds ) ] );

start_time_to_string( StartTime ) ->
	case time_utils:is_timestamp( StartTime ) of

		true ->
			time_utils:timestamp_to_string( StartTime );

		false ->
			% By elimination, a DHMS:
			Secs = time_utils:dhms_to_seconds( StartTime ),
			start_time_to_string( Secs )

	end.
