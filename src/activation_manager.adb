------------------------------------------------------------------------------
--                                                                          --
--                            ACTIVTION MANAGER                             --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
------------------------------------------------------------------------------
with Ada.Strings;
with Experiment_Info;
with Core_Execution_Modes;
with Ada.Strings.Unbounded;
with Initial_Delay;

package body Activation_Manager is

   procedure Synchronize_Activation_Cyclic (Next_Time : in out Ada.Real_Time.Time) is
   begin
      Next_Time := Ada.Real_Time.Time_First;
   end Synchronize_Activation_Cyclic;

   procedure Start is
      Experiment_Parameters : Experiment_Info.Exp_Params;
   begin

      --  We start the experiment configuring its parameters.
      Experiment_Parameters.Experiment_Hyperperiods :=
        (System.Multiprocessors.CPU'First => Experiment_Hyperperiods (System.Multiprocessors.CPU'First),
         System.Multiprocessors.CPU'Last  => Experiment_Hyperperiods (System.Multiprocessors.CPU'Last));
      Experiment_Parameters.Id_Experiment := Experiment_Id;
      Experiment_Parameters.Approach      := Ada.Strings.Unbounded.To_Unbounded_String (Approach);
      Experiment_Parameters.Taskset_Id    := Taskset_Id;
      Experiment_Parameters.Id_Execution  := Ada.Strings.Unbounded.To_Unbounded_String (Execution_Id);
      Experiment_Info.Set_Parameters(Experiment_Parameters);

      --  Then we wait until completion.
      Core_Execution_Modes.Wait_Experiment_Over;
   end Start;

   --  Termination in the current implementation requires
   --  a task to stuck the second core.

   function Get_Longest_Hyperperiod return Natural is
   begin
      if Experiment_Hyperperiods (System.Multiprocessors.CPU'First) > Experiment_Hyperperiods (System.Multiprocessors.CPU'Last) then
         return Experiment_Hyperperiods (System.Multiprocessors.CPU'First);
      end if;
      return Experiment_Hyperperiods (System.Multiprocessors.CPU'Last);
   end Get_Longest_Hyperperiod;

   --  This task stucks the second core's execution when current experiment should stop.
   task End_Task_Second_Core with
         Priority => System.Priority'Last,
         CPU      => System.Multiprocessors.CPU'Last;

   task body End_Task_Second_Core is
   begin
      --  Stuck until someone states that experiment is over.
      Core_Execution_Modes.Wait_Experiment_Over;
      loop
         null;
      end loop;
   end End_Task_Second_Core;

   task Notify_Major_Hyperperiod_Has_Been_Expired with
         Priority => System.Priority'Last - 1,
         CPU      => System.Multiprocessors.CPU'Last;

   task body Notify_Major_Hyperperiod_Has_Been_Expired is
      use Ada.Real_Time;
      Next_Time : constant Ada.Real_Time.Time :=
        Ada.Real_Time.Time_First + Ada.Real_Time.Microseconds (Initial_Delay.Delay_Time);
      Task_Period : constant Ada.Real_Time.Time_Span :=
        Ada.Real_Time.Microseconds (Get_Longest_Hyperperiod);
   begin
      delay until Next_Time + Task_Period;

      Core_Execution_Modes.Set_Parameters_Referee
        (Safe_Boundary_Exceeded => False,
         Experiment_Not_Valid => False,
         Finish_Experiment => True);

   end Notify_Major_Hyperperiod_Has_Been_Expired;

end Activation_Manager;
