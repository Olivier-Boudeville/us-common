% Copyright (C) 2020-2024 Olivier Boudeville
%
% This file is part of US-Common, part of the Universal Server framework.
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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Creation date: Saturday, March 28, 2020.


% @doc Test of the US-Common <b>scheduling service</b>.
-module(class_USScheduler_test).


% For spawn:
-export([ operate_loop/0 ]).

% To silence unused code:
-export([ check_command_acks/1 ]).


-include_lib("traces/include/traces_for_tests.hrl").



% @doc Main loop of an actuator test process.
operate_loop() ->

	receive

		{ operate, [ Pid, Name ] } ->

			trace_utils:debug_fmt( "--> Actuator ~w just operated on behalf "
								   "of ~ts.", [ self(), Name ] ),

			Pid ! { operated, Name, self() },

			operate_loop();

		stop ->
			trace_utils:debug_fmt( "--> Actuator ~w stopped.", [ self() ] )

	end.



% @doc Checks that the exact number of command acks has been received.
check_command_acks( TotalExpectedSchedulings ) ->

	% Hopefully at their right moment:
	test_facilities:display(
		"Checking that all commands were actually executed." ),

	wait_for_command_acks( TotalExpectedSchedulings ),

	test_facilities:display(
		"Checking that no extraneous command has been executed." ),

	% Wait for any extraneous, faulty command ack:
	receive

		{ operated, _AnyPid } ->
			throw( extraneous_command_ack )

	after 1000 ->

			ok

	end.



% @doc Waits for the specified number of command acks.
wait_for_command_acks( _Count=0 ) ->
	trace_utils:debug( "All command acks received." ),
	ok;

wait_for_command_acks( Count ) ->

	receive

		{ operated, Name, _AnyPid } ->
			NewCount = Count - 1,

			trace_utils:debug_fmt( "Received a command ack regarding ~ts; "
				"still waiting for ~B of them.", [ Name, NewCount ] ),

			wait_for_command_acks( NewCount )

	end.



% @doc Returns a suitable task command for the specified named requester.
get_command( Name ) ->
	% Oneway call:
	{ operate, [ self(), Name ] }.



% @doc Runs the tests.
-spec run() -> no_return().
run() ->

	?test_start,

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing the scheduling services." ),

	SchedPid = class_USScheduler:new_link(),

	FirstActuatorPid = spawn_link( ?MODULE, operate_loop, _Args=[] ),

	test_facilities:display( "Scheduler ~w created, interacting with test "
		"actuator ~w by registering new tasks.",
		[ SchedPid, FirstActuatorPid ] ),


	test_facilities:display( "Testing single schedulings." ),

	test_facilities:display( "First testing, as immediate schedulings." ),

	SchedPid ! { triggerOneshotTask, [ get_command( first ), FirstActuatorPid ],
				 self() },

	task_done = test_receive(),


	SchedPid ! { registerOneshotTask, [ get_command( second ),
		_SecondOneshotStartTime=asap, FirstActuatorPid ], self() },

	task_done = test_receive(),


	SchedPid ! { registerOneshotTask, [ get_command( third ),
		_ThirdOneshotStartTime=flexible, FirstActuatorPid ], self() },

	task_done = test_receive(),



	test_facilities:display( "Second testing, as deferred schedulings." ),

	% Duration-based schedule, in one second from now:
	SchedPid ! { registerOneshotTask, [ get_command( fourth ),
		_OneshotDHMSDuration={ 0, 0, 0, 1 }, FirstActuatorPid ], self() },

	{ task_registered, _IdFourthTask=4 } = test_receive(),


	% Timestamp-based schedule, in two seconds from now:
	NowTimestamp = time_utils:get_timestamp(),

	OneshotFutureTimestamp =
		time_utils:offset_timestamp( NowTimestamp, _Seconds=2 ),

	SchedPid ! { registerOneshotTask, [ get_command( fifth ),
						OneshotFutureTimestamp, FirstActuatorPid ], self() },

	{ task_registered, _IdFifthTask=5 } = test_receive(),



	test_facilities:display( "Testing multiple (deferred) schedulings." ),

	% Expected to be unregistered prior its sole trigger:
	SixthDHMSDuration={ 0, 0, 0, 30 },

	% Actually a single scheduling:
	SchedPid ! { registerTask, [ get_command( sixth ), SixthDHMSDuration,
		_SixthPeriodicity=once, _SixthCount=1, FirstActuatorPid ], self() },

	{ task_registered, IdSixthTask=6 } = test_receive(),


	% After 1 second: every 2 seconds, for a total of 5 times.

	SeventhDHMSDuration = { 0, 0, 0, 1 },

	SchedPid ! { registerTask, [ get_command( seventh ), SeventhDHMSDuration,
		_SeventhPeriodicity=2, _SeventhCount=5, FirstActuatorPid ], self() },

	{ task_registered, IdSeventhTask=7 } = test_receive(),


	% To wait not a lot, for the seventh task to still have triggers:
	WaitMs = 2000,

	% To wait for the seventh task to be fully over:
	%WaitMs = 15000,

	test_facilities:display( "Waiting ~ts before unregistering tasks.",
							 [ time_utils:duration_to_string( WaitMs ) ] ),

	timer:sleep( WaitMs ),


	test_facilities:display( "Testing the unregistering of tasks." ),

	NonExistingId = 1000,

	test_facilities:display( "Next error message about the unregistering "
							 "of task ~B is expected:", [ NonExistingId ] ),

	SchedPid ! { unregisterTask, NonExistingId, self() },

	receive

		{ wooper_result,
		  { task_unregistration_failed, never_existed } } ->
			ok

	end,


	IdFirstTask = 1,

	% First task already done by design:
	SchedPid ! { unregisterTask, IdFirstTask, self() },

	receive

		{ wooper_result, task_already_done } ->
			ok

	end,

	% Supposedly still active:
	SchedPid ! { unregisterTask, IdSixthTask, self() },

	receive

		{ wooper_result, task_unregistered } ->
			ok

	end,

	% First done, seventh supposedly still active:
	ToUnregister = [ IdSixthTask, IdSeventhTask ],
	SchedPid ! { unregisterTasks, [ ToUnregister ], self() },

	% Very approximate, but sufficient for this test (false increment to
	% outsmart the compiler):
	%
	case WaitMs + basic_utils:identity( 0 ) > 10000 of

		% Task 7 must be over then:
		true ->
			receive

				{ wooper_result, [ task_already_done, task_already_done ] } ->
					ok

			end;

		% Task 7 not fully completed:
		false ->
			receive

				{ wooper_result, [ task_already_done, task_unregistered ] } ->
					ok

			end

	end,

	test_facilities:display(
		"Checking that no extraneous registration ack was received." ),

	receive

		{ wooper_result, { task_registered, AnyCount } } ->
			throw( { extra_task_registration_ack, AnyCount } )

	after 1000 ->

			ok

	end,

	% Cannot be done now that the seventh task is unregistered as an unspecified
	% time:
	%
	%check_command_acks( _TotalExpectedSchedulings=11 ),

	test_facilities:display( "Test success, tearing down." ),

	FirstActuatorPid ! stop,

	wooper:delete_synchronously_instance( SchedPid ),

	?test_stop.
