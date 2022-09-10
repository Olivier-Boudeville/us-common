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
% Creation date: Wednesday, March 18, 2020.


% @doc Class corresponding to the <b>task scheduler</b> of the US framework.
-module(class_USScheduler).


-define( class_description, "Task scheduler for the US framework." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_USServer ] ).



% Scheduling of tasks on behalf of the US framework.

-type scheduler_pid() :: class_UniversalServer:server_pid().


-type task_command() :: wooper:oneway_call().
% The command corresponding to a task to execute respects the general form of a
% WOOPER oneway, i.e. OnewayName or {OnewayName, Args}, where Args is
% conventionally a single non-list term or a list of any arguments.
%
% We considered, yet finally did not keep, the idea of always adding as last
% element the PID of the sending scheduler. So specified commands are simply
% sent verbatim to their actuators.




% Shorthands:

-type ustring() :: text_utils:ustring().

-type ms_duration() :: time_utils:ms_duration().


% No shorthand, as too ambiguous to be used here:
%-type milliseconds() :: unit_utils:milliseconds().

% To avoid mistakes between amounts of milliseconds:

%-type ms_since_year_0() :: unit_utils:milliseconds().


-type ms_since_start() :: unit_utils:milliseconds().
% Since start of this scheduler (in monotonic time).


-type timestamp() :: time_utils:timestamp().
-type dhms_duration() :: time_utils:dhms_duration().


-type start_time() ::

		% As soon as possible:
		'asap'

		% When relevant/applicable (flexible start):
	  | 'flexible'

		% Preferably after this specified duration from receiving:
	  | dhms_duration()
	  | unit_utils:seconds() % (not milliseconds)

		% Preferably at this specified (future) time:
	  | timestamp().
% Specifies the start time of a task scheduling.



-type user_periodicity() ::

		% Just to be executed once (one shot):
		'once'

		% Actual period:
	  | dhms_duration()

	  | unit_utils:seconds().
% Time between two schedulings of a task, as expressed by the user.
% Note that user periodicities are significantly coarser than internal ones.


-type periodicity() :: maybe( ms_duration() ).
% Time between two schedulings of a task, as used internally.
%
% Not to be mixed up with user periodicities, which are significantly coarser
% than internal ones.


-type schedule_count() :: 'unlimited' | basic_utils:count().
% The number of times a task shall be scheduled.


-type requester_pid() :: pid().
% The PID of the process requesting a task to be scheduled.


-type actuator_pid() :: pid().
% The PID of the process to which a task command will be sent whenever
% scheduled.


-type task_registration_outcome() ::
		'task_done' | { 'task_registered', task_id() }.

-type task_unregistration_outcome():: 'task_unregistered' | 'task_already_done'
			| { 'task_unregistration_failed', basic_utils:error_reason() }.


-type task_id() :: basic_utils:count().
% Identifier of a task, as assigned by a scheduler.


-type timer_ref() :: timer:tref().
% Reference to a timer.


-type timer_table() :: table( schedule_offset(), timer_ref() ).
% Associates the reference of a live timer to a schedule offset.


-export_type([ scheduler_pid/0, task_command/0, start_time/0,
			   user_periodicity/0, requester_pid/0, actuator_pid/0,
			   task_registration_outcome/0, task_unregistration_outcome/0,
			   task_id/0, timer_ref/0, timer_table/0 ]).



-type schedule_offset() :: ms_since_start().
% A millisecond-based offset relative to the start time of this scheduler (hence
% designed to be rather small).


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
	started_on = undefined :: maybe( schedule_offset() ),

	% The internal time offset (if any) at which this task was last scheduled:
	last_schedule = undefined :: maybe( schedule_offset() ),

	% The PID of the process having registered this task:
	requester_pid :: requester_pid(),

	% The PID of the process that will be triggered whenever this task is
	% scheduled:
	%
	actuator_pid :: actuator_pid() } ).


-type task_entry() :: #task_entry{}.
% Describes a task to schedule.


-type task_table() :: table( task_id(), task_entry() ).
% To keep track of task information.


-type schedule_pair() :: { schedule_offset(), [ task_id() ] }.
% Registers which tasks shall be scheduled at a given time offset.


-type schedule_plan() :: [ schedule_pair() ].
% Schedule pairs, ordered from closest future to most remote one.


% The class-specific attributes:
-define( class_attributes, [

	{ task_table, task_table(), "a table registering all ongoing tasks" },

	{ schedule_plan, schedule_plan(), "the ordered list of next schedulings" },

	{ timer_table, timer_table(), "a table registering live timers" },

	% Allows also to count all tasks that have been submitted:
	{ next_task_id, task_id(), "identifier of the next registered task" } ] ).



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
% On task commands.
%
% We preferred to limit the ability of a scheduler only to sending messages to
% whichever process needed (ex: no MFA direct calls), for a better separation of
% concerns.
%
%
% On time.
%
% The monotonic time is relied on internally, to avoid any warp/leap performed
% by the system time. See http://erlang.org/doc/apps/erts/time_correction.html
% for more information.
%
% Timestamps (ex: to designate Sunday, March 22, 2020 at noon) are converted
% into internal time offsets whenever a scheduler registers a task.
%
% As a result, correct periodicities will be enforced (ex: really a 1-hour
% wallclock duration between two schedulings, regardless of any time warp) but,
% for example due to DST (Daylight Saving Time), tasks may appear to be executed
% with a time offset (ex: at 1 PM instead of noon). Working with a monotonic,
% UTC (Universal Coordinated Time)-like time is thus intentional.
%
% Due to some external event (ex: system overload or suspension), task deadlines
% may be missed, in which case they will be rescheduled in a row at a faster
% pace on a best-effort basis (rather than being skipped as a whole). As a
% result, the number of schedulings should match the expected one, yet the
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
% "absolute" ones; we used the 'server_start' attribute and monotonic time for
% that.
%
% For any time unit TU (ex: TU=millisecond here):
% erlang:monotonic_time(TU) + erlang:time_offset(TU) = erlang:system_time(TU).
%
% This scheduler uses internally monotonic time, and, at its boundaries (ex: at
% task submission), whenever having to deal with absolute timestamps, these are
% immediately converted to internal conventions (ex: a task deadline is to be
% expressed by the user in system time, and will be converted directly into a
% monotonic timestamp when received; scheduling reports proceed the other way
% round).
%
% Note that, should the operating system be suspended (typically if the host
% computer itself is suspended), the monotonic time will be suspended as well,
% unless the --enable-prefer-elapsed-monotonic-time-during-suspend command-line
% argument was specified when building Erlang/OTP (possibly then inducing a
% performance penalty).
%
% All kinds of internal system times (notably the VM one) are in UTC, so
% depending on time zones and DST, hour-based offsets apply. At the interfaces
% between the scheduler and the user, universal time is converted into local
% time, and the other way round.

% The timer module is used, rather than a utility process using for example
% receive/after, presumably for a better accuracy.



% @doc Constructs the main (singleton), default US scheduler.
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->

	% First the direct mother classes, then this class-specific actions:
	SrvState = class_USServer:construct( State,
		?trace_categorize("Main US Scheduler"),
		?us_common_scheduler_registration_name,
		?us_common_scheduler_registration_scope ),

	init_common( SrvState ).



% @doc Constructs a (named, unregistered) US scheduler.
-spec construct( wooper:state(), ustring() ) -> wooper:state().
construct( State, SchedulerName ) ->

	% First the direct mother classes, then this class-specific actions:
	% (traps EXITs)
	%
	SrvState = class_USServer:construct( State,
										 ?trace_categorize(SchedulerName) ),

	init_common( SrvState ).



% @doc Constructs a (named, registered with specified scope) US scheduler.
-spec construct( wooper:state(), ustring(), naming_utils:registration_name(),
				 naming_utils:registration_name() ) -> wooper:state().
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

	trace_bridge:info_fmt( "Scheduler ready, at ~ts.",
						   [ get_current_timestamp_string( ReadyState ) ] ),

	ReadyState.



% @doc Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?info_fmt( "Deleting ~ts", [ to_string( State ) ] ),

	% Cancelling all still live timers, while resisting to any error:
	[ timer:cancel( TimerRef )
		|| TimerRef <- table:values( ?getAttr(timer_table) ) ],

	State.



% Method section.


% @doc Triggers immediately specified one-shot task: specified command will be
% triggered at once, a single time, being assigned to actuator process.
%
% Returns either 'task_done' if the task was done on the fly (hence is already
% triggered, and no task identifier applies), or {'task_registered', TaskId} if
% it is registered for a later trigger (then its assigned task identifier is
% returned).
%
-spec triggerOneshotTask( wooper:state(), task_command(), actuator_pid() ) ->
								request_return( task_registration_outcome() ).
triggerOneshotTask( State, UserTaskCommand, UserActPid ) ->

	% Note: in this case the sender could have contacted directly the actuator.

	{ NewState, Result } = registerOneshotTask( State, UserTaskCommand,
												_StartTime=asap, UserActPid ),

	wooper:return_state_result( NewState, Result ).



% @doc Registers specified one-shot task: specified command will be executed
% once, at specified time, as assigned to requesting and specified actuator
% process.
%
% Returns either 'task_done' if the task was done on the fly (hence is already
% triggered, and no task identifier applies), or {'task_registered', TaskId} if
% it is registered for a later trigger (then its assigned task identifier is
% returned).
%
% Note: if the deadline is specified in absolute terms (ex: as {{2020,3,22},
% {16,1,48}}), the conversion to internal time will be done immediately (at task
% submission time), resulting in any system time change (ex: DST) not being
% taken into account (as the respect of periodicities is preferred over the one
% of literal timestamps).
%
-spec registerOneshotTask( wooper:state(), task_command(), start_time(),
			actuator_pid() ) -> request_return( task_registration_outcome() ).
registerOneshotTask( State, UserTaskCommand, UserStartTime, UserActPid ) ->

	{ NewState, Result } = registerTask( State, UserTaskCommand, UserStartTime,
							_Periodicity=once, _Count=1, UserActPid ),

	wooper:return_state_result( NewState, Result ).



% @doc Registers specified task: specified command will be executed starting
% immediately (in a flexible manner), at specified user periodicity and
% indefinitely, being assigned to the requesting process (as actuator).
%
% Returns {'task_registered', TaskId}, where TaskId is its assigned task
% identifier is returned).
%
-spec registerTask( wooper:state(), task_command(), user_periodicity() ) ->
							request_return( task_registration_outcome() ).
registerTask( State, UserTaskCommand, UserPeriodicity ) ->

	{ RegOutcome, RegState } = register_task( UserTaskCommand,
		_StartTime=flexible, UserPeriodicity, _Count=unlimited,
		_UserActPid=?getSender(), State ),

	wooper:return_state_result( RegState, RegOutcome ).



% @doc Registers specified task: specified command will be executed starting
% immediately (in a flexible manner), at specified user periodicity, for
% specified number of times, being assigned to the requesting process (as
% actuator).
%
% Returns either 'task_done' if the task was done on the fly (hence is already
% triggered, and no task identifier applies since it is fully completed), or
% {'task_registered', TaskId} if it is registered for a later trigger (then its
% assigned task identifier is returned).
%
-spec registerTask( wooper:state(), task_command(), user_periodicity(),
			schedule_count() ) -> request_return( task_registration_outcome() ).
registerTask( State, UserTaskCommand, UserPeriodicity, UserCount ) ->

	{ RegOutcome, RegState } = register_task( UserTaskCommand,
		_StartTime=flexible, UserPeriodicity,  UserCount,
		_UserActPid=?getSender(), State ),

	wooper:return_state_result( RegState, RegOutcome ).



% @doc Registers specified task: specified command will be executed starting
% from specified time, at specified user periodicity, for specified number of
% times, being assigned to requesting and specified actuator process.
%
% Returns either 'task_done' if the task was done on the fly (hence is already
% triggered, and no task identifier applies since it is fully completed), or
% {'task_registered', TaskId} if it is registered for a later trigger (then its
% assigned task identifier is returned).
%
% Note: if the deadline is specified in absolute terms (ex: as
% {{2020,3,22}, {16,1,48}}), the conversion to internal time will be done
% immediately (at task submission time), resulting in any future system time
% change (ex: DST) not being taken into account at this level (as the respect of
% periodicities is preferred over the one of literal timestamps).
%
-spec registerTask( wooper:state(), task_command(), start_time(),
					user_periodicity(), schedule_count(), actuator_pid() ) ->
						request_return( task_registration_outcome() ).
registerTask( State, UserTaskCommand, UserStartTime, UserPeriodicity, UserCount,
			  UserActPid ) ->

	{ RegOutcome, RegState } = register_task( UserTaskCommand, UserStartTime,
							UserPeriodicity, UserCount, UserActPid, State ),

	wooper:return_state_result( RegState, RegOutcome ).



% (helper)
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

	%?info_fmt
	?warning_fmt( "Registering task whose command is '~p', whose declared start "
		"time is ~w (hence to happen in ~ts), to be triggered ~ts with ~ts "
		"on actuator ~w (whereas requester is ~w).",
		[ TaskCommand, UserStartTime,
		  time_utils:duration_to_string( MsDurationBeforeStart ),
		  schedule_count_to_string( Count ),
		  periodicity_to_string( MaybePeriodicity ), ActPid, ReqPid ] ),

	% Immediate launch requested?
	case MsDurationBeforeStart of

		% Immediate launch here:
		0 ->
			% Thus, in all cases:
			launch_task( TaskCommand, ActPid, State ),

			case MaybePeriodicity of

				undefined ->
					% Just to execute once (implied and checked: Count=1).
					%
					% Not even recording it then, it was just fire and forget:
					% not task entry, just updating the task count.
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
							% happen and must be recorded:

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

		% Deferred launch here:
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

			{ { task_registered, TaskId }, RegState }

	end.



% @doc Unregisters specified task, based on its identifier.
%
% Returns either 'task_unregistered' if the operation succeeded, or
% 'task_already_done' if the task was already fully done, or
% {'task_unregistration_failed',Reason} if the operation failed.
%
-spec unregisterTask( wooper:state(), task_id() ) ->
						request_return( task_unregistration_outcome() ).
unregisterTask( State, TaskId ) when is_integer( TaskId ) andalso TaskId > 0 ->
	{ Outcome, NewState } = unregister_task( TaskId, State ),
	wooper:return_state_result( NewState, Outcome ).



% @doc Unregisters specified tasks, based on their identifier.
%
% Returns, in order, for each task either 'task_unregistered' if the operation
% succeeded, 'task_already_done' if the task was already fully done, or
% {'task_unregistration_failed',Reason} if the operation failed.
%
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

	wooper:return_state_result( NewState, lists:reverse( RevOutcomes ) ).



% (helper)
-spec unregister_task( task_id(), wooper:state() ) ->
							{ task_unregistration_outcome(), wooper:state() }.
unregister_task( TaskId, State ) when is_integer( TaskId ) andalso TaskId > 0 ->

	NextTaskId = ?getAttr(next_task_id),

	case TaskId >= NextTaskId of

		% Could not have been allocated:
		true ->
			?error_fmt( "Requested to unregister task ~B, which never existed "
				"(as the next task identifier is ~B).",
				[ TaskId, NextTaskId ] ),
			{ { task_unregistration_failed, never_existed }, State };

		false ->
			case table:extract_entry_if_existing( TaskId,
												  ?getAttr(task_table) ) of

				% Already dropped, hence done:
				false ->
					?info_fmt( "Unregistered task ~B, which was actually done.",
							   [ TaskId ] ),
					{ task_already_done, State };

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

							{ { task_unregistration_failed, internal_error },
							  NewState };


						{ NewPlan, NewTimerTable } ->

							NewState = setAttributes( State, [
								{ task_table, ShrunkTaskTable },
								{ schedule_plan, NewPlan },
								{ timer_table, NewTimerTable } ] ),

							{ task_unregistered, NewState }

					end

			end

	end;

unregister_task( TaskId, State ) ->
	?error_fmt( "Task unregistering failed, invalid task identifier: '~p'.",
				[ TaskId ] ),
	{ { task_unregistration_failed, invalid_task_id }, State }.



% @doc Triggers specified scheduling.
%
% Expected to be called through a timer having set beforehand.
%
% Note: a reference() or any other non-guessable element could be used to avoid
% any process to be able to interfere by triggering schedulings.
%
-spec timerTrigger( wooper:state(), schedule_offset() ) -> oneway_return().
timerTrigger( State, ScheduleOffset ) ->

	NowMs = get_current_schedule_offset( State ),

	DiffMs = NowMs - ScheduleOffset,

	cond_utils:if_defined( us_common_debug_scheduling,
		?debug_fmt( "Timer trigger at schedule offset ~w, for an expected one "
			"of ~w: delayed of ~ts.", [ NowMs, ScheduleOffset,
				time_utils:duration_to_string( DiffMs ) ] ) ),

	% As long as a drift is below this threshold, we do not worry:
	OffsetThreshold = 250,

	case erlang:abs( DiffMs ) > OffsetThreshold of

		true ->
			?debug_fmt( "Triggered for offset #~B (~ts), while being at #~B "
				"(~ts), hence with a signed drift of ~ts (late if positive).",
				[ ScheduleOffset,
				  get_timestamp_string_for( ScheduleOffset, State ), NowMs,
				  get_timestamp_string_for( NowMs, State ),
				  time_utils:duration_to_string( NowMs - ScheduleOffset ) ] );

		false ->
			ok

	end,

	TimerTable = ?getAttr(timer_table),

	% Resorb any pending schedule:
	{ NewPlan, NewTimerTable, NewTaskTable } = perform_schedule( ScheduleOffset,
		NowMs, ?getAttr(schedule_plan), TimerTable, ?getAttr(task_table),
		State ),

	cond_utils:if_defined( us_common_debug_scheduling,
		?debug_fmt( "After having being triggered for #~B, went "
			"from ~ts to ~ts", [ ScheduleOffset,
				timer_table_to_string( TimerTable, State ),
				timer_table_to_string( NewTimerTable, State ) ] ) ),

	TrigState = setAttributes( State, [ { schedule_plan, NewPlan },
										{ timer_table, NewTimerTable },
										{ task_table, NewTaskTable } ] ),

	wooper:return_state( TrigState ).



% (helper; State specified for traces only)
%
% Take care of any lingering schedule:
-spec perform_schedule( schedule_offset(), schedule_offset(), schedule_plan(),
						timer_table(), task_table(), wooper:state() ) ->
							{ schedule_plan(), timer_table(), task_table() }.
perform_schedule( ScheduleOffset, NowMs, _SchedulePlan=[ { Off, TaskIds } | T ],
				  TimerTable, TaskTable, State ) when Off < ScheduleOffset ->

	?error_fmt( "While scheduling #~B (~ts), found late offset #~B (~ts), "
		"triggering its delayed tasks first: ~w.",
		[ ScheduleOffset, get_timestamp_string_for( ScheduleOffset, State ),
		  Off, get_timestamp_string_for( Off, State ), TaskIds ] ),

	% Using Off rather than ScheduleOffset here:
	{ NewPlan, NewTimerTable, NewTaskTable } = trigger_tasks( TaskIds,
		Off, NowMs, _NewerPlan=T, TimerTable, TaskTable, State ),

	% Drops this offset entry:
	perform_schedule( ScheduleOffset, NowMs, NewPlan, NewTimerTable,
					  NewTaskTable, State );


% Matching offsets:
perform_schedule( ScheduleOffset, NowMs,
				  _SchedulePlan=[ { ScheduleOffset, TaskIds } | T ],
				  TimerTable, TaskTable, State ) ->

	%?debug_fmt( "Normal scheduling of #~B (~ts), triggering its tasks: ~w.",
	%   [ ScheduleOffset, get_timestamp_string_for( ScheduleOffset, State ),
	%     TaskIds ] ),

	% Dropping current offset:
	{ NewPlan, NewTimerTable, NewTaskTable } = trigger_tasks( TaskIds,
		ScheduleOffset, NowMs, _NewerPlan=T, TimerTable, TaskTable, State ),

	% Stop recursing here, keeping the next schedules to come:
	{ NewPlan, NewTimerTable, NewTaskTable };


% This triggered offset is not found; this is possible: should a T1 timer be
% late, a later one T2 might trigger before (despite registered_offset(T1) <
% registered_offset(T2)), and T2 should have taken care of all preceding tasks
% still registered (through first clause), including those of T1. So, when T1 is
% finally triggered, none of its tasks is left, and this should not be then a
% fatal error.
%
% So, in all other cases (either there exists future offsets or none at all), we
% do not trigger offsets anymore this time, just disregarding the current one:
%
perform_schedule( ScheduleOffset, _NowMs, SchedulePlan, TimerTable, TaskTable,
				  State ) ->

	?warning_fmt( "Triggered schedule offset #~B (~ts) not found (whereas "
		"schedule plan ~ts), ignoring it, as supposing this is a late "
		"scheduling already applied.",
		[ ScheduleOffset, get_timestamp_string_for( ScheduleOffset, State ),
		  schedule_plan_to_string( SchedulePlan, State ) ] ),

	% Hopefully this lacking timer can still be cancelled:
	ShrunkTimerTable = remove_timer( ScheduleOffset, TimerTable ),

	{ SchedulePlan, ShrunkTimerTable, TaskTable }.



% @doc Triggers specified tasks, and returns updated schedule plan, timer and
% task tables.
%
% Note that:
%
% - the specified offset (ScheduleOffset) is the planned one; if being a late
% trigger, it may be significantly in the past of the current offset
%
% - the specified schedule plan is supposed to have already the entry for the
% specified schedule offset removed.
%
% (helper)
%
-spec trigger_tasks( [ task_id() ], schedule_offset(), schedule_offset(),
			schedule_plan(), timer_table(), task_table(), wooper:state() ) ->
							{ schedule_plan(), timer_table(), task_table() }.
trigger_tasks( _TaskIds=[], ScheduleOffset, _NowMs, SchedulePlan, TimerTable,
			   TaskTable, _State ) ->

	 ShrunkTimerTable = remove_timer( ScheduleOffset, TimerTable ),

	{ SchedulePlan, ShrunkTimerTable, TaskTable };


trigger_tasks( _TaskIds=[ TaskId | T ], ScheduleOffset, NowMs, SchedulePlan,
			   TimerTable, TaskTable, State ) ->

	{ TaskEntry, ShrunkTaskTable } = table:extract_entry( TaskId, TaskTable ),

	% Check:
	TaskId = TaskEntry#task_entry.id,

	% next_schedule shall at least roughly match.

	%cond_utils:if_defined( us_common_debug_scheduling,
		%?debug_fmt
		?warning_fmt( "Triggering task ~B: ~ts.",
					[ TaskId, task_entry_to_string( TaskEntry, State ) ] ),% ),

	launch_task( TaskEntry#task_entry.command,
				 TaskEntry#task_entry.actuator_pid, State ),

	case decrement_count( TaskEntry#task_entry.count ) of

		0 ->
			?debug_fmt( "Dropping task ~B for good.", [ TaskId ] ),
			ShrunkTimerTable = remove_timer( ScheduleOffset, TimerTable ),
			{ SchedulePlan, ShrunkTimerTable, ShrunkTaskTable };

		NewCount ->
			% Will thus be still scheduled again afterwards.

			% If first scheduling:
			StartOffset = case TaskEntry#task_entry.started_on of

				undefined ->
					% Now rather than scheduled:
					NowMs;

				AlreadyStartOffset ->
					AlreadyStartOffset

			end,

			% Periodicity not expected to be 'undefined' here:
			Periodicity = TaskEntry#task_entry.periodicity,

			% Basing on planned (not actual, i.e. NowMs) offset to (attempt to)
			% resorb any delay:
			%
			NextSchedule = ScheduleOffset + Periodicity,

			NewTaskEntry = TaskEntry#task_entry{
				next_schedule=NextSchedule,
				count=NewCount,
				schedule_count=TaskEntry#task_entry.schedule_count+1,
				started_on=StartOffset,
				last_schedule=NowMs },

			NewTaskTable = table:add_entry( TaskId, NewTaskEntry,
											ShrunkTaskTable ),

			{ NewPlan, NewTimerTable } = insert_task_at( TaskId, NextSchedule,
							Periodicity, SchedulePlan, TimerTable ),

			cond_utils:if_defined( us_common_debug_scheduling,
				?debug_fmt( "New plan for #~B after trigger of task ~B: ~ts",
					[ ScheduleOffset, TaskId,
					  schedule_plan_to_string( NewPlan, State ) ] ) ),

			trigger_tasks( T, ScheduleOffset, NowMs, NewPlan, NewTimerTable,
						   NewTaskTable, State )

	end.



% @doc Effective launching of specified task.
-spec launch_task( task_command(), actuator_pid(), wooper:state() ) -> void().
launch_task( Cmd, ActuatorPid, State ) ->

	% We want to let the requester be able to specify exactly any command term;
	% otherwise we would have added automatically for example the PID of the
	% sending scheduler and the corresponding task identifier.

	cond_utils:if_defined( us_common_debug_scheduling,
		?debug_fmt( "Sending command '~p' to actuator ~w.",
					[ Cmd, ActuatorPid ] ),
		basic_utils:ignore_unused( State ) ),

	?warning_fmt( "Sending command '~p' to actuator ~w.",
				  [ Cmd, ActuatorPid ] ),

	ActuatorPid ! Cmd.


% onWOOPERExitReceived/3 inherited.



% Static section.


% @doc Returns the main US scheduler (if any).
-spec get_main_scheduler() -> static_return( maybe( scheduler_pid() ) ).
get_main_scheduler() ->

	LookupScope = naming_utils:registration_to_look_up_scope(
		_RegScope=?us_common_scheduler_registration_scope ),

	% Supposing here that no ongoing launch is happening (no race condition):
	%case naming_utils:wait_for_registration_of( _RegName=?registration_name,
	%                                            LookupScope ) of
	case naming_utils:is_registered(
			_RegName=?us_common_scheduler_registration_name, LookupScope ) of

		not_registered ->
			wooper:return_static( undefined );

		SchedPid ->
			wooper:return_static( SchedPid )

	end.



% Helper section.


% @doc Registers a future scheduling of the specified task.
%
% (both ScheduleOffset and DurationFromNow specified to avoid a recomputation)
%
-spec register_task_schedule( task_id(), task_entry(), schedule_offset(),
							  ms_duration(), wooper:state() ) -> wooper:state().
register_task_schedule( TaskId, TaskEntry, ScheduleOffset, DurationFromNow,
						State ) ->

	%?debug_fmt
	?warning_fmt( "Registering task #~B for schedule offset ~B (duration from "
		"now: ~ts): ~ts.", [ TaskId, ScheduleOffset,
			time_utils:duration_to_string( DurationFromNow ),
			task_entry_to_string( TaskEntry, State ) ] ),

	NewTaskTable = table:add_new_entry( TaskId, TaskEntry,
										?getAttr(task_table) ),

	{ NewPlan, NewTimerTable } = insert_task_at( TaskId, ScheduleOffset,
		DurationFromNow, ?getAttr(schedule_plan), _AccPlan=[],
		?getAttr(timer_table) ),

	cond_utils:if_defined( us_common_debug_scheduling,
		?debug_fmt( "New plan: ~p~nNew timer table: ~p.",
					[ NewPlan, NewTimerTable ] ) ),

	setAttributes( State, [ { task_table, NewTaskTable },
							{ schedule_plan, NewPlan },
							{ timer_table, NewTimerTable },
							{ next_task_id, TaskId+1 } ] ).



% @doc Inserts specified task at specified offset in plan.
-spec insert_task_at( task_id(), schedule_offset(), ms_duration(),
		schedule_plan(), timer_table() ) -> { schedule_plan(), timer_table() }.
insert_task_at( TaskId, ScheduleOffset, DurationFromNow, Plan, TimerTable ) ->

	%NewP = { NewPlan, _NewTimerTable } =
	NewP = insert_task_at( TaskId, ScheduleOffset, DurationFromNow, Plan,
						   _AccPlan=[], TimerTable ),

	%trace_bridge:debug_fmt( "After having inserted task ~B at offset #~B "
	%   "(duration from now: ~ts), new plan is:~n ~p",
	%   [ TaskId, ScheduleOffset,
	%     time_utils:duration_to_string( DurationFromNow ), NewPlan ] ),

	NewP.


% (helper)
%
% Schedule plan exhausted, adding it at end:
insert_task_at( TaskId, ScheduleOffset, DurationFromNow, _SchedulePlan=[],
				AccPlan, TimeTable ) ->

	% Not set yet:
	NewTimeTable = add_timer( ScheduleOffset, DurationFromNow, TimeTable ),

	{ lists:reverse( [ { ScheduleOffset, [ TaskId ] } | AccPlan ] ),
	  NewTimeTable };


% Too early in plan, continue:
insert_task_at( TaskId, ScheduleOffset, DurationFromNow,
				_SchedulePlan=[ H={ Off, _Ids } | T ], AccPlan, TimeTable )
  when Off < ScheduleOffset ->
	insert_task_at( TaskId, ScheduleOffset, DurationFromNow, T,
					[ H | AccPlan ], TimeTable );

% Matching offset found, registering and stopping:
insert_task_at( TaskId, ScheduleOffset, _DurationFromNow,
		_SchedulePlan=[ { ScheduleOffset, Ids } | T ], AccPlan, TimeTable ) ->

	% Timer already set for that offset, none to add:
	{ lists:reverse( AccPlan ) ++ [ { ScheduleOffset, [ TaskId | Ids ] } | T ],
	  TimeTable };

% Gone past target:
insert_task_at( TaskId, ScheduleOffset, DurationFromNow,
				SchedulePlan, % Implicit: SchedulePlan=[ { Off, Ids } | T ],
				AccPlan, TimeTable ) -> % Implicit: when Off > ScheduleOffset

	% Not set yet, registering and stop recursing:
	NewTimeTable = add_timer( ScheduleOffset, DurationFromNow, TimeTable ),

	{ lists:reverse( [ { ScheduleOffset, [ TaskId ] } | AccPlan ] )
		++ SchedulePlan, NewTimeTable }.



% @doc Removes specified task from schedule plan.
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
					%  "Removing empty schedule slot at #~B.",
					%  [ PlannedNextSchedule ] ),

					NewSchedPlan = lists:reverse( AccPlan ) ++ T,

					ShrunkTimerTable = remove_timer( PlannedNextSchedule,
													 TimerTable ),

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



% @doc Adds a timer to trigger a future scheduler.
-spec add_timer( schedule_offset(), ms_duration(), timer_table() ) ->
						timer_table().
add_timer( ScheduleOffset, DurationFromNow, TimerTable ) ->

	% WOOPER oneway to be sent to this instance:
	Message = { timerTrigger, [ ScheduleOffset ] },

	% Duration not exact, but is at least as long as requested:
	case timer:send_after( DurationFromNow, Message ) of

		% TimerRef useful to cancel; not expected to be already existing:
		{ ok, TimerRef } ->
			table:add_new_entry( ScheduleOffset, TimerRef, TimerTable );

		{ error, Reason } ->
			throw( { timer_setting_failed, Reason, ScheduleOffset,
					 DurationFromNow } )

	end.



% @doc Removes a timer.
-spec remove_timer( schedule_offset(), timer_table() ) -> timer_table().
remove_timer( ScheduleOffset, TimerTable ) ->

	%trace_bridge:debug_fmt( "Removing timer for #~B.", [ ScheduleOffset ] ),

	{ TimerRef, ShrunkTimerTable } =
		table:extract_entry( ScheduleOffset, TimerTable ),

	case timer:cancel( TimerRef ) of

		{ ok, cancel } ->
			ok;

		{ error, Reason } ->
			trace_bridge:error_fmt( "The cancellation of timer '~p' "
				"failed for schedule offset #~B (reason: '~p').",
				[ TimerRef, ScheduleOffset, Reason ] )
			%throw( { timer_cancellation_failed, Reason,
			%         TimerRef, ScheduleOffset } )

	end,

	ShrunkTimerTable.




% @doc Returns the time offset of specified (absolute) time, thus expressed in
% internal time, hence relative to the start time of this scheduler.
%
% Corresponds to the (real, actual) number of milliseconds since the start of
% this scheduler.
%
-spec get_current_schedule_offset( wooper:state() ) -> schedule_offset().
get_current_schedule_offset( State ) ->
	time_utils:get_monotonic_time() - ?getAttr(server_start).



% @doc Returns the (approximate) user-level timestamp corresponding to now.
-spec get_current_timestamp( wooper:state() ) -> timestamp().
get_current_timestamp( State ) ->
	% Better (more homogeneous) than using calendar:local_time/0 for example:
	get_timestamp_for( get_current_schedule_offset( State ), State ).



% @doc Returns a textual description of the (approximate) user-level timestamp
% corresponding to now.
%
-spec get_current_timestamp_string( wooper:state() ) -> ustring().
get_current_timestamp_string( State ) ->
	get_timestamp_string_for( get_current_schedule_offset( State ), State ).



% @doc Returns the internal time offset corresponding to this user-level
% timestamp (such as {{2020,3,23},{16,44,0}}).
%
-spec get_schedule_offset_for( timestamp(), wooper:state() ) ->
									schedule_offset().
get_schedule_offset_for( UserTimestamp, State ) ->

	% Number of milliseconds since year 0:
	GregorianMillisecs =
		1000 * calendar:datetime_to_gregorian_seconds(
				time_utils:local_to_universal_time( UserTimestamp ) ),

	GregorianMillisecs - ?getAttr(server_gregorian_start).



% @doc Returns the (approximate) user-level timestamp (ex:
% {{2020,3,23},{16,44,0}}), in VM system time (UTC), corresponding to specified
% internal time offset.
%
-spec get_timestamp_for( schedule_offset(), wooper:state() ) -> timestamp().
get_timestamp_for( Offset, State ) ->

	GregorianSecs =
		round( ( Offset + ?getAttr(server_gregorian_start) ) / 1000 ),

	time_utils:universal_to_local_time(
		calendar:gregorian_seconds_to_datetime( GregorianSecs ) ).



% @doc Returns a textual description of the user-level timestamp corresponding
% to specified internal time offset.
%
-spec get_timestamp_string_for( schedule_offset(), wooper:state() ) ->
										ustring().
get_timestamp_string_for( Offset, State ) ->
	time_utils:timestamp_to_string( get_timestamp_for( Offset, State ) ).



% @doc Registers the fact that a task has just been triggered once more.
-spec decrement_count( schedule_count() ) -> maybe( schedule_count() ).
decrement_count( _Count=unlimited ) ->
	unlimited;

% (guard just for safety, should never be negative):
decrement_count( Count ) when Count > 0 ->
	Count - 1.



% Vet helpers, to check and canonicalise.


% @doc Checks and canonicalises this user-specified task command.
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



% @doc Checks and canonicalises this user-specified start time: returns the
% number of milliseconds before starting any corresponding task (possibly zero).
%
-spec vet_start_time( term(), wooper:state() ) -> ms_duration().
vet_start_time( _UserStartTime=asap, _State ) ->
	0;

vet_start_time( _UserStartTime=flexible, _State ) ->
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
						"timestamp nor DHMS duration): '~p'.", [ StartTime ] ),
					throw( { invalid_start_time, StartTime } )

			end

	end.



% @doc Returns the user-specified schedule count.
-spec vet_count( term(), wooper:state() ) -> schedule_count().
vet_count( ScheduleCount=unlimited, _State ) ->
	ScheduleCount;

vet_count( C, _State ) when is_integer( C ) andalso C > 0 ->
	C;

vet_count( Other, State ) ->
	?error_fmt( "Invalid user-specified schedule count: ~p.", [ Other ] ),
	throw( { invalid_schedule_count, Other } ).



% @doc Returns any user-specified periodicity.
-spec vet_periodicity( term(), term(), wooper:state() ) ->
								maybe( periodicity() ).
vet_periodicity( _UserPeriodicity=once, _Count=1, _State ) ->
	undefined;

vet_periodicity( UserPeriodicity=once, Count, State ) ->

	?error_fmt( "Task periodicity is 'once', whereas specified schedule count "
				"is ~p.", [ Count ] ),

	throw( { periodicity_count_mismatch, UserPeriodicity, Count } );

vet_periodicity( UserPeriodicity, _Count, State ) ->
	vet_user_periodicity( UserPeriodicity, State ).



% @doc Returns a vetted, internal periodicity.
%
% (helper, defined for reuse)
%
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



% @doc Returns the user-specified actuator PID.
-spec vet_actuator_pid( term() ) -> actuator_pid().
vet_actuator_pid( Pid ) when is_pid( Pid ) ->
	Pid;

vet_actuator_pid( Other ) ->
	throw( { invalid_actuator_pid, Other } ).




% Section for textual representations.


% @doc Returns a textual description of this server.
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

	text_utils:format( "US scheduler, a ~ts; "
		"registering ~ts (with a total of ~B task(s) already declared); "
		"current schedule ~ts; with ~ts",
		[ class_USServer:to_string( State ), TaskStr,
		  ?getAttr(next_task_id) - 1, SchedStr, TimerStr ] ).



% @doc Returns a textual description of the specified schedule plan.
-spec schedule_plan_to_string( schedule_plan(), wooper:state() ) -> ustring().
schedule_plan_to_string( _SchedulePlan=[], _State ) ->
	"is empty";

schedule_plan_to_string( SchedulePlan, State ) ->

	NowOffset = get_current_schedule_offset( State ),
	text_utils:format( "registers ~B trigger(s) (at #~B, i.e. ~ts): ~ts",
		[ length( SchedulePlan ), NowOffset,
		  get_timestamp_string_for( NowOffset, State ),
		  text_utils:strings_to_string(
			[ trigger_to_string( P, State ) || P <- SchedulePlan ] ) ] ).



% @doc Returns a textual description of the specified timer table.
-spec timer_table_to_string( timer_table(), wooper:state() ) -> ustring().
timer_table_to_string( TimerTable, State ) ->

	case table:keys( TimerTable ) of

		[] ->
			"no timer set";

		[ Offset ] ->
			text_utils:format( "a single timer set, at #~B (~ts)",
				[ Offset, get_timestamp_string_for( Offset, State ) ] );

		Offsets ->
			text_utils:format( "~B timers set, at: ~ts", [ length( Offsets ),
				text_utils:strings_to_string( [ text_utils:format( "#~B (~ts)",
					[ Off, get_timestamp_string_for( Off, State ) ] )
						|| Off <- Offsets ] ) ] )

	end.



% @doc Returns a textual description of the specified schedule pair.
-spec trigger_to_string( schedule_pair(), wooper:state() ) -> ustring().
trigger_to_string( { Offset, TaskIds }, State ) ->
	text_utils:format( "at offset #~B (~ts), ~B task(s) registered: ~w",
		[ Offset, get_timestamp_string_for( Offset, State ),
		  length( TaskIds ), TaskIds ] ).



% @doc Returns a textual description of the specified task entry.
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
						"already executed a single time, at #~B (~ts)",
						[ StartOffset,
						  get_timestamp_string_for( StartOffset, State ) ] );

				% Expected higher than 1:
				C when C > 1 ->
					text_utils:format( "already executed ~B times, the "
						"first at #~B (~ts) and the last at #~B (~ts)",
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
		"to be scheduled next at offset #~B (~ts) according to ~ts, "
		"and for ~ts; it was declared by ~w",
		[ Cmd, ActuatorPid, ExecStr, NextSchedOffset, NextSchedTime, PeriodStr,
		  CountStr, RequesterPid ] ).



% @doc Returns a textual description of specified periodicity.
-spec periodicity_to_string( periodicity() ) -> ustring().
% Should never happen:
periodicity_to_string( _Periodicity=undefined ) ->
	"no periodicity";

periodicity_to_string( Periodicity ) ->
	text_utils:format( "a periodicity of ~ts",
					   [ time_utils:duration_to_string( Periodicity ) ] ).



% @doc Returns a textual description of specified schedule count.
-spec schedule_count_to_string( schedule_count() ) -> ustring().
schedule_count_to_string( _Count=unlimited ) ->
	"an unlimited number of times";

schedule_count_to_string( _Count=1 ) ->
	"a single time";

schedule_count_to_string( Count ) ->
	text_utils:format( "~B times", [ Count ] ).
