with Ada.Real_Time;
with Stack_Pool_Access;
with Ada.Assertions; use Ada.Assertions;

package body Pool_User is
   
   type Local_Object is record
      null;
   end record;
   
   task body Dynamic_Allocating_Task is
      use Ada.Real_Time;
      Next_Period : Time;
      Period : constant Time_Span := Microseconds (Cycle_Time);
      
      --  Initialization of the Stack Pool instance. 
      --  The pool is accessed by means of Referene_Type 
      --  objects. 
      package SPA is new Stack_Pool_Access.Shared_Pointer
           (Pool_Size            => 24,
            Elmt_Size            => 8,
            Alignment            => 8,
            Number_of_References => 3,
            Element_Type         => Local_Object);
      use SPA;
      
      --  Instead of using access types as allocators, 
      --  Reference_Type could be applied. 
      --  This type allows to: 
      --   1. Discourage deallocation of single object,
      --   which could not be easily achieved using it. 
      --   2. Track how allocations take place, in particular
      --   all objects referenced by an initialized Reference_Type 
      --   are surely not null, nor have been deallocated. 
      --   3. Avoid copy (move) of allocator (Reference_Type is a
      --   limited type).
      Reference_1 : Reference_Type;
      Reference_2 : Reference_Type;
      Reference_3 : Reference_Type; 
      --  Reference_4 : Reference_Type;  ERROR during initialization.

   begin
      Next_Period := Ada.Real_Time.Clock + Period;
      
      Allocate (Reference_1); 
      Allocate (Reference_2); 
      Allocate (Reference_3);

      --  Allocate (Reference_1);  ERROR at run-time.
      
      SPA.Free;  --  Free all the elements in the pool.
      
      loop
         delay until Next_Period;

         Allocate (Reference_1);            --  Reference_1 -> Ob1
         Allocate (Reference_2);            --  Reference_2 -> Ob2
         Assign (Reference_3, Reference_1); --  Reference_3 -> Ob1
         Allocate (Reference_1);            --  Reference_1 -> Ob3
         
         SPA.Free;

         
         
         Next_Period := Next_Period + Period;
      end loop;
   end Dynamic_Allocating_Task;

   --  Temporal memory safety, premature deallocation
   --  is handled as follows:
   --  1. The user should not deallocate memory directly.
   --  2. The application should invoke a specific function
   --  to deallocate all the elements in the pool reserved
   --  for a specific object type.
   --  3. After deallocation, all references are marked uninitialized.
   
   task body Deallocating_Task is
      use Ada.Real_Time;
      Next_Period : Time;
      Period : constant Time_Span := Microseconds (Cycle_Time);
      
      --  Initialization of the Stack Pool instance. 
      --  The pool is accessed by means of Referene_Type 
      --  objects. 
      package SPA is new Stack_Pool_Access.Shared_Pointer
           (Pool_Size            => 24,
            Elmt_Size            => 8,
            Alignment            => 8,
            Number_of_References => 3,
            Element_Type         => Local_Object);
      use SPA;
      
      Reference_1 : Reference_Type;
      Reference_2 : Reference_Type;
      
   begin
      Next_Period := Ada.Real_Time.Clock + Period;
      
      Allocate (Reference_1);  -- Allocation of a new Local_Object
      --  Reference_2 := Reference_1;  ERROR: Reference_Type is a limited type.
      Assign (Reference_2, Reference_1); 
      
      loop
         delay until Next_Period;
         
         Assert
           (Is_Initialized (Reference_1) = True and 
                Is_Initialized (Reference_2) = True);
         
         SPA.Free;
         
         --  At this point, all referenced object have been deallocated,
         --  and each reference should now be initialized again. 
         Assert 
           (Is_Initialized (Reference_1) = False and 
                Is_Initialized (Reference_2) = False);
         
         Next_Period := Next_Period + Period;
      end loop;
   end Deallocating_Task;

end Pool_User;
