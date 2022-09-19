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
      Low_Critical_Budget               => 5554, 
      Is_Migrable                       => False, 
      Workload                          => 693, 
      Period                            => 54000, 
      Reduced_Deadline                  => 53987, 
      CPU_Id                            => 1);
   
   Low_Crit_Task_2 : Low_Crit_Task.Low_Criticality_Task 
     (Id                                => 2,
      Priority                          => 4, 
      Hosting_Migrating_Tasks_Priority  => 4, 
      On_Target_Core_Priority           => -1, 
      Low_Critical_Budget               => 20225, 
      Is_Migrable                       => False, 
      Workload                          => 2637, 
      Period                            => 94500, 
      Reduced_Deadline                  => 88708, 
      CPU_Id                            => 2);
   
   --  High criticality tasks
   High_Crit_Task_1 : High_Criticality_Task.High_Criticality_Task 
     (Id                               => 3, 
      Priority                         => 1, 
      Hosting_Migrating_Tasks_Priority => 1, 
      Low_Critical_Budget              => 15431, 
      High_Critical_Budget             => 30863, 
      Workload                         => 4046, 
      Period                           => 175000, 
      Reduced_Deadline                 => 126685, 
      Could_Exceed                     => False, 
      CPU_Id                           => 1);
  
   High_Crit_Task_2 : High_Criticality_Task.High_Criticality_Task 
     (Id                               => 4, 
      Priority                         => 0, 
      Hosting_Migrating_Tasks_Priority => 0, 
      Low_Critical_Budget              => 25715, 
      High_Critical_Budget             => 51430, 
      Workload                         => 6771, 
      Period                           => 420000, 
      Reduced_Deadline                 => 356029, 
      Could_Exceed                     => False, 
      CPU_Id                           => 2);
  
end Taskset;  
