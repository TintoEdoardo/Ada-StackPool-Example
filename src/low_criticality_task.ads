------------------------------------------------------------------------------
--                                                                          --
--                           LOW CRITICALITY TASK                           --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
------------------------------------------------------------------------------
with System;
with System.Multiprocessors;  use System.Multiprocessors;

pragma Warnings (Off);
with System.BB.Time;
pragma Warnings (On);

package Low_Criticality_Task is

   task type Low_Criticality_Task
      (Id                              : Natural;
      Priority                         : System.Priority;
      Hosting_Migrating_Tasks_Priority : Integer;
      On_Target_Core_Priority          : Integer;
      Low_Critical_Budget              : Natural;
      Is_Migrable                      : Boolean;
      Workload                         : Positive;
      Period                           : Positive;
      Reduced_Deadline                 : Positive;
      CPU_Id                           : CPU)
  is
      pragma Priority (Priority);
      pragma CPU (CPU_Id);
   end Low_Criticality_Task;

end Low_Criticality_Task;
