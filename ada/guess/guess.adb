--------------------------------------------------------------------------------
-- Guessing game in Ada.                                                      --
--                                                                            --
-- Author : Maxwell Powlison (bobdavelisafrank@protonmail.com)                --
--------------------------------------------------------------------------------
with
  Ada.Text_IO;



procedure Guess is
   package IO renames Ada.Text_IO;
   
   -- Get_and_Test_Guess
   --
   -- Main loop logic and user-input exception handling.
   function Get_And_Test_Guess return Boolean is
      Secret_Integer: constant Integer := 42;
      Guess_Integer: Integer;
   begin
      IO.Put ("? ");
      Guess_Integer := Integer'Value (IO.Get_Line);
      
      if Guess_Integer = Secret_Integer then
	 IO.Put_Line ("That was correct. 1 trillion internet points.");
	 return False;
      elsif Guess_Integer < Secret_Integer then
	 IO.Put_Line ("Nah, a bit too small.");
      else
	 IO.Put_Line ("Nah, a bit too large.");
      end if;
      
      return True;
      
   exception
      when IO.End_Error =>
	 -- Elegant handling of EOF.
	 IO.New_Line;
	 IO.Put_Line ("Goodbye!");
	 return False;
	 
      when Constraint_Error =>
	 -- Handling of what is *probably* a bad input integer.
	 IO.Put_Line 
	   ("That's a creative number, but not the one I was thinking of.");
	 return True;
	 
   end Get_And_Test_guess;
   
   Continue: Boolean := True;
   
begin
   IO.Put_Line ("What number am I thinking of?");
   
   while Continue loop
      Continue := Get_And_Test_Guess;
   end loop;
   
end Guess;
