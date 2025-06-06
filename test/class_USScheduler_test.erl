% Copyright (C) 2020-2025 Olivier Boudeville
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

-module(class_USScheduler_test).

-moduledoc """
Test of the US-Common **scheduling service**.
""".


% For spawn:
-export([ operate_loop/0 ]).

% To silence unused code:
-export([ check_command_acks/1 ]).


-include_lib("traces/include/traces_for_tests.hrl").



-doc "Main loop of an actuator test process.".
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



-doc "Checks that the exact number of command acks has been received.".
check_command_acks( TotalExpectedSchedulings ) ->

	% Hopefully at their right moment:
	test_facilities:display(
		"Checking that all commands were actually executed." ),

	wait_for_command_acks( TotalExpectedSchedulings ),

	test_facilities:display(
		"Checking that no extraneous command has been executed." ),

	% Wait for any extraneous, faulty command ack:
	receive

		{ operated, Name, AnyPid } ->
			throw( { extraneous_command_ack, { Name, AnyPid } } )

	after 1000 ->

			ok

	end.



-doc "Waits for the specified number of command acks.".
wait_for_command_acks( _Count=0 ) ->
	trace_utils:debug( "All command acks received." );

wait_for_command_acks( Count ) ->

	receive

		{ operated, Name, _AnyPid } ->
			NewCount = Count - 1,

			trace_utils:debug_fmt( "Received a command ack regarding ~ts; "
				"still waiting for ~B of them.", [ Name, NewCount ] ),

			wait_for_command_acks( NewCount )

	end.



-doc "Returns a suitable task command for the specified named requester.".
get_command( Name ) ->
	% Oneway call:
	{ operate, [ self(), Name ] }.



-doc "Runs the tests.".
-spec run() -> no_return().
run() ->

	?test_start,

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing the scheduling services." ),

	test_facilities:display( "The version of this currently tested US-Common "
		"library is ~ts (i.e. ~w).", [
			class_USConfigServer:get_us_common_version_string(),
			class_USConfigServer:get_us_common_version() ] ),

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

    receive

        { operated, _Name=first, FirstActuatorPid } ->
            ok

    end,


	test_facilities:display( "Next immediate schedulings." ),

	SchedPid ! { registerOneshotTask, [ get_command( second ),
		_SecondOneshotStartTime=asap, FirstActuatorPid ], self() },

	task_done = test_receive(),

    receive

        { operated, second, FirstActuatorPid } ->
            ok

    end,


	SchedPid ! { registerOneshotTask, [ get_command( third ),
		_ThirdOneshotStartTime=flexible, FirstActuatorPid ], self() },

	task_done = test_receive(),

    receive

        { operated, third, FirstActuatorPid } ->
            ok

    end,


	test_facilities:display( "Second testing, as deferred schedulings." ),

	% Duration-based schedule, in one second from now:
	SchedPid ! { registerOneshotTask, [ get_command( fourth ),
		_OneshotDHMSDuration={ 0, 0, 0, 1 }, FirstActuatorPid ], self() },

	{ task_registered, _IdFourthTask=4 } = test_receive(),

    receive

        { operated, fourth, FirstActuatorPid } ->
            ok

    end,


	% Timestamp-based schedule, in two seconds from now:
	NowTimestamp = time_utils:get_timestamp(),

	OneshotFutureTimestamp =
		time_utils:offset_timestamp( NowTimestamp, _Seconds=2 ),

	SchedPid ! { registerOneshotTask, [ get_command( fifth ),
		OneshotFutureTimestamp, FirstActuatorPid ], self() },

	{ task_registered, _IdFifthTask=5 } = test_receive(),

    receive

        { operated, fifth, FirstActuatorPid } ->
            ok

    end,


	test_facilities:display( "Testing multiple (deferred) schedulings." ),

	% Expected to be unregistered prior its sole trigger:
	SixthDHMSDuration={ 0, 0, 0, _Secs=300 },

	% Actually a single scheduling:
	SchedPid ! { registerTask, [ get_command( sixth ), SixthDHMSDuration,
		_SixthPeriodicity=once, _SixthCount=1, FirstActuatorPid ], self() },

	{ task_registered, IdSixthTask=6 } = test_receive(),

    % No waiting for any {operated, sixth, FirstActuatorPid}

	% After 2 seconds: every 1 second, for a total of 5 times.

    SeventhOffsetSecs = 2,

	SeventhDHMSDuration = { 0, 0, 0, SeventhOffsetSecs },

	SchedPid ! { registerTask, [ get_command( seventh ), SeventhDHMSDuration,
		SeventhPeriodicitySecs=1, SeventhCount=5, FirstActuatorPid ],
                 self() },

	{ task_registered, IdSeventhTask=7 } = test_receive(),

	% To wait not a lot, for the seventh task to still have triggers:
    WaitMs = 2000,

	% To wait for the seventh task to be fully over:
	%WaitMs = 15000,

	test_facilities:display( "Waiting ~ts before unregistering tasks.",
							 [ time_utils:duration_to_string( WaitMs ) ] ),

	timer:sleep( WaitMs ),

    % Managed below:
	%% basic_utils:repeat(
    %%     fun() ->
    %%         %trace_utils:debug( "Waiting for task registration." ),
    %%         receive

    %%             { operated, seventh, FirstActuatorPid } ->
    %%                 ok

    %%         end

    %%     end,
    %%     SeventhCount ),


	test_facilities:display( "Testing the unregistering of tasks." ),

	NonExistingId = 1000,

	test_facilities:display( "Next error message about the unregistering "
							 "of task ~B *is* expected:", [ NonExistingId ] ),

	SchedPid ! { unregisterTask, NonExistingId, self() },

	receive

		{ wooper_result, { task_unregistration_failed, never_existed,
                           NonExistingId } } ->
			ok
	end,


    test_facilities:display( "Unregistering first task (done)." ),

	IdFirstTask = 1,

	% First task already done by design:
	SchedPid ! { unregisterTask, IdFirstTask, self() },

	receive

		{ wooper_result, { task_already_done, IdFirstTask } } ->
			ok

	end,


    test_facilities:display( "Unregistering sixth (not done yet) task." ),

	% Supposedly still active:
	SchedPid ! { unregisterTask, IdSixthTask, self() },

	receive

		{ wooper_result, { task_unregistered, IdSixthTask } } ->
			ok

	end,

    test_facilities:display( "Unregistering seventh (possibly partly done) "
                             "task." ),

	% First done, seventh supposedly still active:
	ToUnregister = [ IdSixthTask, IdSeventhTask ],
	SchedPid ! { unregisterTasks, [ ToUnregister ], self() },

	% Very approximate, but sufficient for this test:

    SeventhTotalDurSecs = SeventhOffsetSecs
        + SeventhPeriodicitySecs * SeventhCount,

	case WaitMs div 1000 > SeventhTotalDurSecs of

		true ->
            test_facilities:display( "Expecting task 7 to be over." ),
			receive

				{ wooper_result, [ { task_already_done, IdSixthTask },
                                   { task_already_done, IdSeventhTask } ] } ->
					ok

			end;

		false ->
            test_facilities:display( "Expecting task 7 to be still ongoing." ),

			receive

				{ wooper_result, [ { task_already_done, IdSixthTask },
                                   { task_unregistered, IdSeventhTask } ] } ->
					ok

			end

	end,

	test_facilities:display( "Checking that no extraneous (un)registration "
                             "ack was received." ),

	receive

		{ wooper_result, { task_registered, AnyId } } ->
			throw( { extra_task_registration_ack, AnyId } );

		{ wooper_result, { task_unregistered, AnyId } } ->
			throw( { extra_task_unregistration_ack, task_unregistered,
                     AnyId } );

		{ wooper_result, { task_already_done, AnyId } } ->
			throw( { extra_task_unregistration_ack, task_already_done,
                     AnyId } );

		{ wooper_result, { task_unregistration_failed, Reason, AnyId } } ->
			throw( { extra_task_unregistration_ack, task_unregistration_failed,
                     Reason, AnyId } )

	after 1000 ->

			ok

	end,

	% Cannot be done now that the seventh task is unregistered at an unspecified
	% time:
	%
	%check_command_acks( _TotalExpectedSchedulings=11 ),

	test_facilities:display( "Test success, tearing down." ),

	FirstActuatorPid ! stop,

	wooper:delete_synchronously_instance( SchedPid ),

	?test_stop.
