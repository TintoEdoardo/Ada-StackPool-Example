------------------------------------------------------------------------------
--                                                                          --
--                          HIGH CRITICALITY TASK                           --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
------------------------------------------------------------------------------

pragma Warnings (Off);
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with System.Task_Primitives.Operations;
with Stack_Pool;
pragma Warnings (On);

with Activation_Manager;
with Ada.Real_Time; 
with Local_Objects;
with Stack_Pool_Logger;

package body High_Criticality_Task is

   package Task_Primitive_Operations renames System.Task_Primitives.Operations;
   
   task body High_Criticality_Task is
      use Ada.Real_Time;
      Task_Period : constant Ada.Real_Time.Time_Span := 
        Ada.Real_Time.Microseconds (Period);
      Next_Time  : Ada.Real_Time.Time;
      
      -- Local pool types and objects.
      Local_Pool : Stack_Pool.Stack_Bounded_Pool (16, 8, 4);
      type Object_Ptr is access Local_Objects.Local_Object;
      for Object_Ptr'Storage_Pool use Local_Pool;
      procedure Free is new Ada.Unchecked_Deallocation (Local_Objects.Local_Object, Object_Ptr);
      Pointer_to_Local_Object : Object_Ptr;
      
   begin
      Task_Primitive_Operations.Initialize_HI_Crit_Task
        (Task_Primitive_Operations.Self,
         Id, 
         Hosting_Migrating_Tasks_Priority,
         System.BB.Time.Microseconds (Low_Critical_Budget),
         System.BB.Time.Microseconds (High_Critical_Budget),
         Period);
      
      Activation_Manager.Synchronize_Activation_Cyclic (Next_Time);

       --  Generate debug messages. 
      Pointer_to_Local_Object  := new Local_Objects.Local_Object;
      Stack_Pool_Logger.Stapoo_Logger.Write_Message ("<msg>" & "Task: " & Id'Image & " has initialized its local object " & "</msg>");
      Free(Pointer_to_Local_Object);
      Stack_Pool_Logger.Stapoo_Logger.Write_Message ("<msg>" & "Task: " & Id'Image & " has deleted its local object " & "</msg>");
      Pointer_to_Local_Object  := new Local_Objects.Local_Object;
      Stack_Pool_Logger.Stapoo_Logger.Write_Message ("<msg>" & "Task: " & Id'Image & " has initialized its local object " & "</msg>");
      
      --  Print something from the object in the Local Pool.
      Stack_Pool_Logger.Stapoo_Logger.Write_Message ("<msg>" & "Task: " & Id'Image & " has an object with a field One, and value " & Pointer_to_Local_Object.all.One'Image & "</msg>");
      
      loop
         Next_Time := Next_Time + Task_Period;
         
         delay until Next_Time;
      end loop;
   end High_Criticality_Task;
   

end High_Criticality_Task;
