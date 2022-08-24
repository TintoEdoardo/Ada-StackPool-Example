------------------------------------------------------------------------------
--                                                                          --
--                                  TASKSET                                 --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
------------------------------------------------------------------------------

with Low_Criticality_Task;
with High_Criticality_Task;

package Taskset is
   
   package Low_Crit_Task  renames Low_Criticality_Task;
   package High_Crit_Task renames High_Criticality_Task;
   
   --  Low criticality tasks
   Low_Crit_Task_1 : Low_Crit_Task.Low_Criticality_Task 
     (Id                                => 1,
      Priority                          => 5, 
      Hosting_Migrating_Tasks_Priority  => 5, 
      On_Target_Core_Priority           => -1, 
      Low_Critical_Budget               => 60, 
      Is_Migrable                       => False, 
      Workload                          => 1, 
      Period                            => 100, 
      Reduced_Deadline                  => 90, 
      CPU_Id                            => 1);
   
   Low_Crit_Task_2 : Low_Crit_Task.Low_Criticality_Task 
     (Id                                => 2,
      Priority                          => 4, 
      Hosting_Migrating_Tasks_Priority  => 4, 
      On_Target_Core_Priority           => -1, 
      Low_Critical_Budget               => 60, 
      Is_Migrable                       => True, 
      Workload                          => 1, 
      Period                            => 100, 
      Reduced_Deadline                  => 90, 
      CPU_Id                            => 1);
   
   -- High criticality tasks
   High_Crit_Task_1 : High_Criticality_Task.High_Criticality_Task 
     (Id                               => 3, 
      Priority                         => 1, 
      Hosting_Migrating_Tasks_Priority => -1, 
      Low_Critical_Budget              => 80, 
      High_Critical_Budget             => 160, 
      Workload                         => 1, 
      Period                           => 200, 
      Reduced_Deadline                 => 180, 
      Could_Exceed                     => True, 
      CPU_Id                           => 1);
  
   High_Crit_Task_2 : High_Criticality_Task.High_Criticality_Task 
     (Id                               => 4, 
      Priority                         => 2, 
      Hosting_Migrating_Tasks_Priority => -1, 
      Low_Critical_Budget              => 80, 
      High_Critical_Budget             => 160, 
      Workload                         => 1, 
      Period                           => 200, 
      Reduced_Deadline                 => 180, 
      Could_Exceed                     => True, 
      CPU_Id                           => 2);
  
end Taskset;  
