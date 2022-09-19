------------------------------------------------------------------------------
--                                                                          --
--                           LOW CRITICALITY TASK                           --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
------------------------------------------------------------------------------

--  A simple example of a low criticality task implementation.

pragma Warnings (Off);
with Ada.Finalization;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with System.Task_Primitives.Operations;
with Stack_Pool;
with Stack_Pool_Access; use Stack_Pool_Access;
pragma Warnings (On);

with Activation_Manager;
with Ada.Real_Time;
with Local_Objects;

with Stack_Pool_Logger;

package body Low_Criticality_Task is

   package Task_Primitive_Operations renames System.Task_Primitives.Operations;

   task body Low_Criticality_Task is
      use Ada.Real_Time;
      Task_Period : constant Ada.Real_Time.Time_Span :=
        Ada.Real_Time.Microseconds(Period);
      Next_Time  : Ada.Real_Time.Time;

      Test_1 : Boolean;
      Test_2 : Boolean;

      package I is new Stack_Pool_Access.Shared_Pointer
        (Pool_Size => 24,
         Elmt_Size => 8,
         Alignment => 4,
         Number_of_References => 3,
         Element_Type => Local_Objects.Local_Object);

      Ref_1 : I.Reference_Type;
      Ref_2 : I.Reference_Type;

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

      --  Generate debug messages.

      I.Allocate (Ref_1);
      I.Allocate (Ref_2);
      Stack_Pool_Logger.Stapoo_Logger.Write_Message ("<msg>" & "Task: " & Id'Image & " - alloc R1" & "</msg>");
      Stack_Pool_Logger.Stapoo_Logger.Write_Message ("<msg>" & "Task: " & Id'Image & " - alloc R2" & "</msg>");

      Ref_1 := Ref_2;

      Test_1 := (I.Is_Initialized (Ref_1));
      Test_2 := (I.Is_Initialized (Ref_2));
      Stack_Pool_Logger.Stapoo_Logger.Write_Message ("<msg>" & "Task: " & Id'Image & " - R1 is not null?" & Test_1'Image & "</msg>");
      Stack_Pool_Logger.Stapoo_Logger.Write_Message ("<msg>" & "Task: " & Id'Image & " - R2 is not null?" & Test_2'Image & "</msg>");

      Stack_Pool_Logger.Stapoo_Logger.Write_Message ("<msg>" & "Task: " & Id'Image & " - access R1" & Ref_1.Element.One'Image & "</msg>");

      Stack_Pool_Logger.Stapoo_Logger.Write_Message ("<msg>" & "Task: " & Id'Image & " - Start Free" & "</msg>");
      I.Free;
      Stack_Pool_Logger.Stapoo_Logger.Write_Message ("<msg>" & "Task: " & Id'Image & " - Free ended" & "</msg>");

      Test_1 := (I.Is_Initialized (Ref_1));
      Test_2 := (I.Is_Initialized (Ref_2));
      Stack_Pool_Logger.Stapoo_Logger.Write_Message ("<msg>" & "Task: " & Id'Image & " - R1 is not null?" & Test_1'Image & "</msg>");
      Stack_Pool_Logger.Stapoo_Logger.Write_Message ("<msg>" & "Task: " & Id'Image & " - R2 is not null?" & Test_2'Image & "</msg>");

      Stack_Pool_Logger.Stapoo_Logger.Write_Message ("<msg>" & "Task: " & Id'Image & " - access R1" & Ref_1.Element.One'Image & "</msg>");

      loop

         Next_Time := Next_Time + Task_Period;

         delay until Next_Time;
      end loop;
   end Low_Criticality_Task;

end Low_Criticality_Task;
