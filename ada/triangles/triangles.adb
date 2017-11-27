--------------------------------------------------------------------------------
-- Triangle-square number calculator in Ada.                                  --
--                                                                            --
-- Author : Maxwell Powlison (bobdavelisafrank@protonmail.com)                --
--------------------------------------------------------------------------------
with
  Interfaces,
  Ada.Text_IO,
  Ada.Integer_Text_IO;

use
  Interfaces,
  Ada.Text_IO,
  Ada.Integer_Text_IO;



procedure Triangles is
   Previous: Unsigned_64 := 0;
   Current: Unsigned_64 := 1;
   Next: Unsigned_64;
begin
   Put_Line (Previous'Image);
   
   loop
      Put_Line (Current'Image);
      
      -- Unsigned integers have modular arithmetic, so overflow isn't reported
      -- and must be manually checked for.
      exit when Current > (16#FFFF_FFFF_FFFF_FFFF# / 34);
      
      Next := 34 * Current - Previous + 2;
      
      Previous := Current;
      Current := Next;
   end loop;
end Triangles;
