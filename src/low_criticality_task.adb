------------------------------------------------------------------------------
--                                                                          --
--                           LOW CRITICALITY TASK                           --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
------------------------------------------------------------------------------

--  A simple example of a low criticality task implementation.

pragma Warnings (Off);
with Ada.Text_IO;
with System.Task_Primitives.Operations;
with Stack_Pool;
pragma Warnings (On);

with Activation_Manager;
with Ada.Real_Time;
with Local_Objects;

package body Low_Criticality_Task is

   package Task_Primitive_Operations renames System.Task_Primitives.Operations;

   task body Low_Criticality_Task is
      use Ada.Real_Time;
      Task_Period : constant Ada.Real_Time.Time_Span :=
        Ada.Real_Time.Microseconds(Period);
      Next_Time  : Ada.Real_Time.Time;

      -- Local pool types and objects.
      Local_Pool : Stack_Pool.Stack_Bounded_Pool (16, 8, 4);
      type Object_Ptr is access Local_Objects.Local_Object;
      for Object_Ptr'Storage_Pool use Local_Pool;
      Pointer_to_Local_Object : constant Object_Ptr := new Local_Objects.Local_Object;

   begin
      Task_Primitive_Operations.Initialize_LO_Crit_Task
        (Task_Primitive_Operations.Self,
         Id,
         Hosting_Migrating_Tasks_Priority,
         On_Target_Core_Priority,
         System.BB.Time.Microseconds (Low_Critical_Budget),
         Period,
         Reduced_Deadline,
         Is_Migrable);
      Activation_Manager.Synchronize_Activation_Cyclic (Next_Time);

      --  Print something from the object in the Local Pool.
      Ada.Text_IO.Put_Line("<msg>" & "Task: " & Id'Image & " has an object with a field One. " & "</msg>");
      Ada.Text_IO.Put_Line("<msg-content>" & "This is its value: " & Pointer_to_Local_Object.all.One'Image & " </msg-content>");

      loop

         Next_Time := Next_Time + Task_Period;

         delay until Next_Time;
      end loop;
   end Low_Criticality_Task;

end Low_Criticality_Task;
