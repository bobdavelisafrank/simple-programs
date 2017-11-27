--------------------------------------------------------------------------------
-- Argument parsing for integer-collection REPLs.                             --
--                                                                            --
-- Author : Maxwell Powlison (bobdavelisafrank@protonmail.com)                --
--------------------------------------------------------------------------------



package Collect_CLI is
   
   type Command_Type is
     (Quit,
      Insert,
      Remove,
      Print,
      Retry
     );
   
   type Command is
      record
	 Action: Command_Type;
	 Argument: Integer;
      end record;
   
   function Next_Command return Command;
   
end Collect_CLI;
