------------------------------------------------------------------------------
--                                                                          --
--                                   MAIN                                   --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
------------------------------------------------------------------------------
pragma Restrictions (No_Dynamic_Sized_Objects);
pragma Default_Storage_Pool (null);
with System;
with Taskset;
pragma Unreferenced(Taskset);
with Activation_Manager;
with Ada.Text_IO;

procedure main is
    pragma Priority (System.Priority'Last);
    pragma CPU (1);
begin
   Ada.Text_IO.Put_Line("<executionid>" & "Inside the main procedure" & "</executionid>");
   Activation_Manager.Start;
end main;

