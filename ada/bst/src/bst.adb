--------------------------------------------------------------------------------
-- Binary Search Trees in Ada.                                                --
--                                                                            --
-- Author : Maxwell Powlison (bobdavelisafrank@protonmail.com)                --
--------------------------------------------------------------------------------
with
  Ada.Unchecked_Deallocation;



package body BST is
   
   -- Insert
   --
   -- Inserts the given key into the given BST.
   procedure Insert (Tree: aliased in out Node_Ref; Element: in Integer) is
      -- This is a pretty weird, but kind of neat scoping rule.
      type Node_Tracer is access all Node_Ref;
      
      Tracer: Node_Tracer := Tree'Access;
   begin
      -- Triple ref method for insertion.
      while Tracer.all /= null loop
	 if Tracer.all.all.Key < Element 
	 then
	    Tracer := Tracer.all.all.Left'Access;
	    
	 elsif Tracer.all.all.Key = Element 
	 then
	    raise Duplicate_Element;
	    
	 else
	    Tracer := Tracer.all.all.Right'Access;
	 end if;
      end loop;
      
      Tracer.all := new Node;
      
      Tracer.all.all.Key   := Element;
      Tracer.all.all.Left  := null;
      Tracer.all.all.Right := null;
      
   end Insert;
   
   
   
   -- Remove_Head
   --
   -- Removes the head from a given subtree.
   procedure Remove_Head (Tree: aliased in out Node_Ref) is
      
      -- Min
      --
      -- Finds the minimum node in a subtree, removes it, and returns its
      -- key.
      function Remove_Min (Subtree: aliased in out Node_Ref) return Integer is
	 type Node_Tracer is access all Node_Ref;
	 
	 Tracer: Node_Tracer := Subtree'Access;
	 Min_Key: Integer;
      begin
	 while Tracer.all.all.Left /= null loop
	    Tracer := Tracer.all.all.Left'Access;
	 end loop;
	 
	 Min_Key := Tracer.all.all.Key;
	 Free_Node (Tracer.all);
	 
	 return Min_Key;
      end Remove_Min;
      
      Left_Exists:  Boolean := Tree.all.Left  /= null;
      Right_Exists: Boolean := Tree.all.Right /= null;
      
      Successor: Node_Ref;
      
   begin
      if Left_Exists and Right_Exists then
	 Tree.Key := Remove_Min (Tree.all.Right);
	 return;
	 
      elsif Left_Exists then
	 Successor := Tree.all.Left;
	 
      elsif Right_Exists then
	 Successor := Tree.all.Right;
	 
      else
	 Free_Node (Tree);
	 return;
      end if;
      
      -- Tree head has only a single successor node.
      Tree.all := Successor.all;
      
      Successor.all.Left  := null;
      Successor.all.Right := null;
      
      Free_Node (Successor);
   end Remove_Head;
   
   
   
   -- Remove
   --
   -- Removes the given key from the given BST.
   procedure Remove 
     (Tree: aliased in out Node_Ref; 
      Element: in Integer) 
   is
      
      type Node_Tracer is access all Node_Ref;
      
      Tracer: Node_Tracer := Tree'Access;
   begin
      while Tracer.all /= null loop
	 if Tracer.all.all.Key = Element then
	    goto Break;
	    
	 elsif Tracer.all.all.Key < Element then
	    Tracer := Tracer.all.all.Left'Access;
	    
	 else
	    Tracer := Tracer.all.all.Right'Access;
	    
	 end if;
      end loop;
  <<Break>>
      
      if Tracer.all = null then
	 raise Not_Found;
      end if;
      
      Remove_Head (Tracer.all);
      
   end Remove;
   
   
   
   -- Map
   --
   -- Feeds all nodes in a tree into the given function.
   procedure Map 
     (Tree: aliased in out Node_Ref; 
      Target: access procedure (Val: Integer))
   is
   begin
      if Tree /= null then
	 Target (Tree.all.Key);
	 
	 Map (Tree.all.Left,  Target);
	 Map (Tree.all.Right, Target);
      end if;
   end;
   
   
   
   -- Drop
   --
   -- Deletes a given tree.
   procedure Drop (Tree: aliased in out Node_Ref) is
   begin
      if Tree = null then
	 return;
      else
	 Drop (Tree.all.Left);
	 Drop (Tree.all.Right);
	 Free_Node (Tree);
      end if;
   end;
   
end BST;
