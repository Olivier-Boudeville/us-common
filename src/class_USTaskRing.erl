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
% Creation date: Wednesday, April 22, 2020.

-module(class_USTaskRing).

-moduledoc """
Class implementing a <b>task ring</b>, in order to schedule evenly and
synchronously a set of tasks.
""".



-define( class_description,
		 "Ring to schedule evenly and synchronously a set of tasks, as a "
		 "series (i.e. sequentially)." ).


% Design notes:
%
% This class allows to schedule a set of tasks (corresponding to as many
% actuators) observing the same periodicity:
%  - synchronously: a given task will not start until the preceding one is over
%  - evenly: if having N tasks at periodicity P, a new task will be scheduled
% every P/N
%
% A key property is that all registered tasks will be serialized (no
% overlapping happening), and uniformly.
%
% A typical use-case is having a resource (e.g. a tool for web analysis) used by
% multiple consumers (e.g. several websites) whose (single) state may not be
% protected against concurrent accesses.
%
% At any time, up to one consumer shall access that resource, none shall be
% skipped, the expected pace shall be respected as much as possible, and
% resource use shall be spaced as evenly as possible (e.g. to avoiding spikes in
% resource consumption such as CPU).
%
% For that, a task ring registering all tasks is created;, it will register to
% the scheduler, and will chain these tasks accordingly.
%
% No blocking receive of the triggered actuator is done by a task ring, so that
% it remains responsive.
%
% However, should a task fail to report its completion, that ring will never
% schedule any other task.


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_USServer ] ).


-doc "Not a request, but a oneway expected to send in turn a oneway callback.".
-type task_sync_command() :: wooper:oneway_call().

-type ring_pid() :: pid().

-export_type([ task_sync_command/0, ring_pid/0 ]).



% Type shorthands:

-type ustring() :: text_utils:ustring().

-type user_periodicity() :: class_USScheduler:user_periodicity().

-type ms_duration() :: time_utils:ms_duration().


-type periodicity() :: ms_duration().

-type actuator_pid() :: class_USScheduler:actuator_pid() .

-type schedule_count() :: class_USScheduler:schedule_count().

-type scheduler_pid() :: class_USScheduler:scheduler_pid().

-type task_id() :: class_USScheduler:task_id().

-type actuator_ring() :: ring_utils:ring( actuator_pid() ).


% To silence unused shorthands:
-export_type([ actuator_ring/0, periodicity/0, task_id/0 ]).




% The class-specific attributes:
-define( class_attributes, [

	{ actuator_ring, actuator_ring(),
	  "the actuators, cycling in a round-robin manner" },

	{ task_periodicity, ms_duration(),
	  "the periodicity at which each task will be triggered" },

	% Not useful to record:
	%{ ring_periodicity, ms_duration(),
	%  "the periodicity at which this ring will be triggered, i.e. the duration"
	%  " between two successive task triggers" },

	{ task_call, task_sync_command(),
	  "the task synchronous command (oneway triggering in turn a callback) to "
	  "be sent to each actuator in a row" },

	{ scheduler_pid, scheduler_pid(),
	  "the PID of the scheduler used by this ring" },

	{ waited_actuator_pid, option( actuator_pid() ),
	  "the PID of any waited actuator" },

	{ task_id, task_id(),
	  "the task identifier of that ring as set by its scheduler" } ] ).




% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "US.Scheduling.TaskRing" ).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").



-doc """
Creates a task ring for specified actuators, so that they are (immediately, yet
flexibly) triggered with the specified request (a synchronous task command) at
the specified overall periodicity, for the specified number of times, by the
specified scheduler.
""".
-spec construct( wooper:state(), ustring(), [ actuator_pid() ],
		wooper:oneway_name(), wooper:method_arguments(), user_periodicity(),
		schedule_count(), scheduler_pid() ) -> wooper:state().
construct( _State, _RingName, _Actuators=[], _TaskOnewayName, _TaskOnewayArgs,
		   _TaskPeriodicity, _ScheduleCount, _SchedulerPid ) ->
	throw( no_actuator_defined );

construct( State, RingName, Actuators, TaskOnewayName, TaskOnewayArgs,
		   UserTaskPeriodicity, ScheduleCount, SchedulerPid )
                                        when is_list( TaskOnewayArgs ) ->

	% First the direct mother classes, then this class-specific actions:
	SrvState = class_USServer:construct( State, ?trace_categorize(RingName) ),

	TaskPeriodicity = class_USScheduler:vet_user_periodicity(
		UserTaskPeriodicity, SrvState ),

	TaskCall = { TaskOnewayName,
				 list_utils:append_at_end( self(), TaskOnewayArgs ) },

	SetState = setAttributes( SrvState, [
		{ task_periodicity, TaskPeriodicity },
		{ task_call, TaskCall },
		{ scheduler_pid, SchedulerPid },
		{ waited_actuator_pid, undefined } ] ),

	{ RingPeriodicity, ActState } =
		set_actuators( Actuators, TaskPeriodicity, SetState ),

	% No need to perform immediately the first trigger here: the scheduler will
	% by itself ensure that.

	% Self-registering:
	SchedulerPid ! { registerTask, [ _Cmd=triggerNextTask, _StartTime=flexible,
						RingPeriodicity, ScheduleCount, _ActPid=self() ],
					 self() },

	FinalState = receive

		% No other result expected:
		{ wooper_result, { task_registered, TaskId } } ->
			setAttribute( ActState, task_id, TaskId )

	end,

	?send_info_fmt( FinalState, "Started ~ts.", [ to_string( FinalState ) ] ),

	FinalState.



-doc "Overridden destructor.".
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	TaskId = ?getAttr(task_id),

	SchedPid = ?getAttr(scheduler_pid),

	SchedPid ! { unregisterTask, [ TaskId ], self() },

	?info_fmt( "Being destructed, unregistering from scheduler ~w "
			   "(task: #~B).", [ SchedPid, TaskId ] ),

	receive

		{ task_unregistered, TaskId } ->
			ok;

		{ task_already_done, TaskId } ->
			ok;

		{ task_unregistration_failed, Error, TaskId } ->
			?error_fmt( "Unregistration of task #~B failed "
						"at deletion: ~p.", [ TaskId, Error ] )

	end,

	% Nothing to be done for actuators.

	?info( "Deleted." ),

	State.




% Method section.


-doc """
Requires this ring to trigger its next task (typically triggered itself by a
scheduler, as a class_USScheduler:task_command()).

Expects this task, triggered synchronously, to call back notifyTaskDone/2.
""".
-spec triggerNextTask( wooper:state() ) -> oneway_return().
triggerNextTask( State ) ->

	case ?getAttr(waited_actuator_pid) of

		% Normal case:
		undefined ->
			{ ThisActuatorPid, NewRing } =
				ring_utils:head( ?getAttr(actuator_ring) ),

			TaskCall = ?getAttr(task_call),

			ThisActuatorPid ! TaskCall,

			%?debug_fmt( "Triggered actuator ~w with ~p.",
			%            [ ThisActuatorPid, TaskCall ] ),

			TrigState = setAttributes( State, [
				{ actuator_ring, NewRing },
				{ waited_actuator_pid, ThisActuatorPid } ] ),

			wooper:return_state( TrigState );

		% A task is supposedly still in progress:
		LingeringActPid ->
			% No other measure really possible (no task overlapping):
			?error_fmt( "Next task triggered whereas current actuator (~w) "
				"was not reported as having finished. Not triggering "
				"a new task, skipping this period as a whole "
				"to wait for the lingering actuator.", [ LingeringActPid ] ),
			wooper:const_return()

	end.



-doc "Notifies this ring that the specified actuator completed its task.".
-spec notifyTaskDone( wooper:state(), actuator_pid() ) -> oneway_return().
notifyTaskDone( State, ActuatorPid ) ->

	% Check:
	ActuatorPid = ?getAttr(waited_actuator_pid),

	%?debug_fmt( "Actuator ~w reported as having operated.",
	%            [ ActuatorPid ] ),

	DoneState = setAttribute( State, waited_actuator_pid, undefined ),

	wooper:return_state( DoneState ).



% Helper section.


-doc "Sets new actuators (no interaction done with the scheduler).".
-spec set_actuators( [ actuator_pid() ], ms_duration(), wooper:state() ) ->
							{ unit_utils:seconds(), wooper:state() }.
set_actuators( _NewActuators=[], _TaskPeriodicity, _State ) ->
	throw( no_actuator_defined );

set_actuators( NewActuators, TaskPeriodicity, State ) ->

	NewActuatorRing = ring_utils:from_list( NewActuators ),

	ActuatorCount = length( NewActuators ),

	% By design not a division by zero; seconds wanted for the scheduler:
	RingPeriodicity = erlang:round( TaskPeriodicity / ActuatorCount ),

	?debug_fmt( "This ring is to be triggered by its scheduler every ~ts, as "
		"task-level periodicity is ~ts, and ~B actuators are being "
		"synchronised.",
		[ time_utils:duration_to_string( RingPeriodicity ),
		  time_utils:duration_to_string( TaskPeriodicity ), ActuatorCount ] ),

	SetState = setAttribute( State, actuator_ring, NewActuatorRing ),

	% Seconds wanted for the scheduler:
	{ RingPeriodicity div 1000, SetState }.



-doc "Returns a textual description of this task ring.".
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	Ring = ?getAttr(actuator_ring),

	RingSize = ring_utils:size( Ring ),

	TaskPeriodicity = ?getAttr(task_periodicity),

	text_utils:format( "task ring '~ts' alternating between ~B actuators (~ts) "
		"each with a periodicity of ~ts (hence ~ts between triggers; "
		"using for that task id ~B, assigned by scheduler ~w) for the sending "
		"of following synchronised call:~n'~p'",
		[ ?getAttr(name), RingSize,
		  text_utils:pids_to_short_string( ring_utils:to_list( Ring ) ),
		  time_utils:duration_to_string( TaskPeriodicity ),
		  % Not a division by zero:
		  time_utils:duration_to_string( TaskPeriodicity / RingSize ),
		  ?getAttr(task_id), ?getAttr(scheduler_pid), ?getAttr(task_call) ] ).
