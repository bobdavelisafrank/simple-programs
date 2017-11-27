--------------------------------------------------------------------------------
-- Argument parsing for integer-collection REPLs.                             --
--                                                                            --
-- Author : Maxwell Powlison (bobdavelisafrank@protonmail.com)                --
--------------------------------------------------------------------------------
with
  Ada.Text_IO,
  Ada.Strings,
  Ada.Strings.Unbounded,
  Ada.Characters.Handling;



package body Collect_CLI is
   
   package IO renames Ada.Text_IO;
   package SU renames Ada.Strings.Unbounded;
   package CH renames Ada.Characters.Handling;
   
   -- Next_Command
   --
   -- Retrieves the next action for the REPL.
   function Next_Command return Command is
      
      Argument_Failure: exception;
      
      -- Get_Argument
      --
      -- Attempts to retrieve the integer argument for the command.
      function Get_Argument 
	(Input_Text: in SU.Unbounded_String) 
	return Integer 
      is
	 Number_Text: SU.Unbounded_String;
	 
      begin
	 Number_Text := SU.Unbounded_Slice 
	   (Input_Text, 3, SU.Length (Input_Text));
	 return Integer'Value (SU.To_String (Number_Text));
      exception
	 -- Bad input integer.
	 when Constraint_Error =>
	    IO.Put_Line 
	      ("That's a creative number, but I can't really use it.");
	    raise Argument_Failure;
	    
	    -- No argument present.
	 when Ada.Strings.Index_Error =>
	    IO.Put_Line
	      ("No argument given. I mean, that command takes an " &
		 "argument, right?");
	    raise Argument_Failure;
      end Get_Argument;
      
      
      
      Input_Text: SU.Unbounded_String;
      Element: Integer := 0;
      
   begin
      loop
	 IO.Put ("? ");
	 Input_Text := SU.To_Unbounded_String (IO.Get_Line);
	 
	 if SU.Length (Input_Text) = 0 then
	    goto Continue;
	 end if;
	 
	 case CH.To_Lower (SU.Element (Input_Text, 1)) is
	    when 'q' | 'e' => 
	       return Command'(Action => Quit, Argument => Element);
	       
	    when 'p' => 
	       return Command'(Action => Print, Argument => Element);
	       
	    when 'i' =>
	       Element := Get_Argument (Input_Text);
	       return Command'(Action => Insert, Argument => Element);
	       
	    when 'r' =>
	       Element := Get_Argument (Input_Text);
	       return Command'(Action => Remove, Argument => Element);
	       
	    when others =>
	       IO.Put_Line ("I didn't plan for that command. Try another?");
	       
	 end case;
	 
     <<Continue>>
      end loop;
      
   exception
      -- EOF handling.
      when IO.End_Error =>
	 IO.New_Line;
	 return Command'(Action => Quit, Argument => 0);
	 
	 -- When arguments can't be read.
      when Argument_Failure =>
	 return Command'(Action => Retry, Argument => 0);
   end Next_Command;
   
end Collect_CLI;
