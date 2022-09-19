------------------------------------------------------------------------------
--                                                                          --
--                            ACTIVTION MANAGER                             --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
------------------------------------------------------------------------------

with System.Multiprocessors;
with Ada.Real_Time;

package Activation_Manager is
   
   --  Experiment data of previous, original experiment. 
   --  Not significan for this specific demonstration. 
   Experiment_Hyperperiods : array (System.Multiprocessors.CPU) of Natural := 
     (System.Multiprocessors.CPU'First => 56700000, System.Multiprocessors.CPU'Last => 1417500);
   
   Experiment_Id       : Integer := 1;
   Approach            : String  := "SEMI2WF";
   Taskset_Id          : Integer := 1;
   Execution_Id        : String  := "stack-pool-test";
   Taskset_Size        : String  := "4";
   --  Taskset_Utilization : String  := "1.686";
   --  Criticality_Factor  : String  := "2";
   HI_Crit_Proportion  : String  := "0.5";
   
   procedure Synchronize_Activation_Cyclic 
     (Next_Time : in out Ada.Real_Time.Time);
   
   procedure Start;

end Activation_Manager;

	
