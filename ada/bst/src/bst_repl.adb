--------------------------------------------------------------------------------
-- Binary Search Tree REPL in Ada.                                            --
--                                                                            --
-- Author : Maxwell Powlison (bobdavelisafrank@protonmail.com)                --
--------------------------------------------------------------------------------

-- Standard libraries.
with
  Ada.Text_IO;

-- Local imports.
with
  Collect_CLI,
  BST;



procedure BST_Repl is
   
   package IO renames Ada.Text_IO;
   package CL renames Collect_CLI;
   
   
   
   -- Draw_Pretty_Tree
   --
   -- I shouldn't have to explain why this exists, so I won't.
   procedure Draw_Pretty_Tree is
   begin
      IO.Put_Line ("   /\   ");
      IO.Put_Line ("   /\   ");
      IO.Put_Line ("  /^^\  ");
      IO.Put_Line ("   /\   ");
      IO.Put_Line ("  /^^\  ");
      IO.Put_Line (" /^^^^\ ");
      IO.Put_Line ("   ||   ");
   end Draw_Pretty_Tree;
   
   
   
   -- Put_Integer
   --
   -- For mapping to the tree for printing.
   procedure Put_Integer (Val: in Integer) is
   begin
      IO.Put_Line (Val'Image);
   end;
   
   
   
   -- Perform_Insert / Perform_Remove
   --
   -- Wrappers around BST.Insert/Remove for proper exception handling.
   procedure Perform_Insert 
     (Tree: aliased in out BST.Node_Ref; Element: in Integer) is
   begin
      BST.Insert (Tree, Element);
   exception
      when BST.Duplicate_Element =>
	 IO.Put_Line ("Sorry, that number is already in the tree.");
   end;
   
   procedure Perform_Remove 
     (Tree: aliased in out BST.Node_Ref; Element: in Integer) is
   begin
      BST.Remove (Tree, Element);
   exception
      when BST.Not_Found =>
	 IO.Put_Line ("Good news! That number is already not in the tree.");
   end;
   
   
   
   Goal: CL.Command;
   Integer_Tree: aliased BST.Node_Ref := null;

begin
   IO.Put_Line ("BST REPL.");
   IO.Put_Line ("  q       -> exit");
   IO.Put_Line ("  i (num) -> insert num");
   IO.Put_Line ("  r (num) -> remove num");
   IO.Put_Line ("  p       -> print");
   
   loop 
      Goal := CL.Next_Command;
      
      case Goal.Action is
	 when CL.Quit =>
	    IO.Put_Line ("Goodbye!");
	    BST.Drop (Integer_Tree);
	    return;
	    
	 when CL.Insert =>
	    Perform_Insert (Integer_Tree, Goal.Argument);	    
	    
	 when CL.Remove =>
	    Perform_Remove (Integer_Tree, Goal.Argument);
	    
	 when CL.Print =>
	    if BST."=" (Integer_Tree, null) then
	       Draw_Pretty_Tree;
	       IO.Put_Line 
		 ("The tree was empty, so I drew a (different) pretty one " &
		    "for you.");
	    else
	       BST.Map (Integer_Tree, Put_Integer'Access);
	    end if;
	    
	 when CL.Retry =>
	    goto Continue;
	    
	 when others =>
	    IO.Put_Line 
	      ("You definitely typed a command, but I don't know what it " &
		 "is.");
      end case;
      
  <<Continue>>
   end loop;
end BST_Repl;
