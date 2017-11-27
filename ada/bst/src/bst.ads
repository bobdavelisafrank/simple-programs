--------------------------------------------------------------------------------
-- Binary Search Trees in Ada.                                                --
--                                                                            --
-- Author : Maxwell Powlison (bobdavelisafrank@protonmail.com)                --
--------------------------------------------------------------------------------
with
  Ada.Unchecked_Deallocation;



package BST is
   type Node;
   type Node_Ref is access Node;
   
   type Node is
      record
	 Key: Integer := 0;
	 Left: aliased Node_Ref;
	 Right: aliased Node_Ref;
      end record;
   
   -- Free_Node
   --
   -- Destroys a heap-allocated node.
   procedure Free_Node is new Ada.Unchecked_Deallocation (Node, Node_Ref);
   
   
   
   -- For in case of insertion of a duplicate element.
   Duplicate_Element: exception;
   
   -- Insert
   --
   -- Inserts the given key into the given BST.
   procedure Insert (Tree: aliased in out Node_Ref; Element: in Integer);
   
   
   
   -- For in the case of a node not found during removal.
   Not_Found: exception;
   
   -- Remove
   --
   -- Removes the given key from the given BST.
   procedure Remove 
     (Tree: aliased in out Node_Ref; 
      Element: in Integer);
   
   
   
   -- Map
   --
   -- Feeds all nodes in a tree into the given function.
   procedure Map 
     (Tree: aliased in out Node_Ref; 
      Target: access procedure (Val: Integer));
   
   
   
   -- Drop
   --
   -- Deletes a given tree.
   procedure Drop (Tree: aliased in out Node_Ref);
   
end BST;
