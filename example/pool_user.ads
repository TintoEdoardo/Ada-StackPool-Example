pragma Profile (Ravenscar);
with System;

package Pool_User is

   task type Dynamic_Allocating_Task (Priority: System.Priority; Cycle_Time : Positive) is
      pragma Priority (Priority);
   end Dynamic_Allocating_Task;

   task type Deallocating_Task (Priority: System.Priority; Cycle_Time : Positive) is
      pragma Priority (Priority);
   end Deallocating_Task;

end Pool_User;
