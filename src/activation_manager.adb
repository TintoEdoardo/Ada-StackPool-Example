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

end Activation_Manager;
