--------------------------------------------------------------------------------
-- Implementation of `echo` in Ada.                                           --
--                                                                            --
-- Author : Maxwell Powlison (bobdavelisafrank@protonmail.com)                --
--------------------------------------------------------------------------------
with
  Ada.Text_IO,
  Ada.Command_Line;



procedure Echo is
   package IO renames Ada.Text_IO;
   package CL renames Ada.Command_Line;
begin
   for I in Integer range 1 .. CL.Argument_Count loop
      IO.Put (CL.Argument(I));
      IO.Put (' ');
   end loop;
end Echo;
