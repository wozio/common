--  Copyright Maciej Sobczak 2008-2014.
--  This file is part of YAMI4.
--
--  YAMI4 is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  YAMI4 is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with YAMI4.  If not, see <http://www.gnu.org/licenses/>.

with IDL.Name_Utils;
with IDL.Reader;
with IDL.Structures;

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package body IDL.Parser is

   Processing_Mode : Input_Mode := Full;
   
   type Processor_State_Type is
     (Top_Level,                     -- expecting import or package
      Import_Package_Name,           -- name
      Import_Semicolon,              -- ;
      Package_Name,                  -- name
      Package_Is,                    -- is
      Type_Name,                     -- name
      Type_Is,                       -- is
      Type_Fields,                   -- name, end
      Type_Field_Colon,              -- :
      Type_Field_Type,               -- optional, field type name
      Type_Field_End,                -- ;
      Type_End_Name,                 -- name
      Type_Semicolon,                -- ;
      Interface_Name,                -- name
      Interface_Is,                  -- is
      Interface_Messages,            -- oneway, message, end
      Interface_Message_Name,        -- name
      Interface_Message_Signature,   -- (, ;
      Interface_Message_Param_Name,  -- name
      Interface_Message_Param_Colon, -- :
      Interface_Message_Param_Mode,  -- in, out
      Interface_Message_Param_Type,  -- type name
      Interface_Message_Param_End,   -- ), ;
      Interface_Message_Semicolon,   -- ;
      Interface_End_Name,            -- name
      Interface_Semicolon,           -- ;
      Package_End_Name,              -- name
      Package_Semicolon,             -- ;
      Finish);                       -- nothing is accepted in this state

   K_Import : constant String := "import";
   K_Package : constant String := "package";
   K_Type : constant String := "type";
   K_Interface : constant String := "interface";
   K_End : constant String := "end";
   K_Is : constant String := "is";
   K_In : constant String := "in";
   K_Out : constant String := "out";
   K_Message : constant String := "message";
   K_Oneway : constant String := "oneway";
   K_Optional : constant String := "optional";
   K_Colon : constant String := ":";
   K_Semicolon : constant String := ";";
   K_Open : constant String := "(";
   K_Close : constant String := ")";

   Processor_State : Processor_State_Type := Top_Level;
   In_Package : Boolean := False;
   Import_Pkg_Name : Unbounded_String;
   Current_Package_Name : Unbounded_String;
   Current_Type_Name : Unbounded_String;
   Current_Field_Name : Unbounded_String;
   Current_Field_Type : Unbounded_String;
   Current_Field_Is_Optional : Boolean;
   Current_Message_Is_Oneway : Boolean;
   Current_Interface_Name : Unbounded_String;
   Current_Message_Name : Unbounded_String;
   Param_Name : Unbounded_String;
   Param_Mode : Unbounded_String;
   In_Param_Name : Unbounded_String;
   In_Param_Type : Unbounded_String;
   Out_Param_Name : Unbounded_String;
   Out_Param_Type : Unbounded_String;
   
   procedure Ensure_Proper_Name (Token : in String) is
   begin
      if not Ada.Characters.Handling.Is_Letter (Token (Token'First)) or
         not Ada.Characters.Handling.Is_Letter (Token (Token'Last)) then
         
         raise Invalid_Input with "name should not begin or end with underscore";
      end if;
   end Ensure_Proper_Name;
   
   procedure Process_Token (Token : in String) is
   begin
      case Processor_State is
         when Top_Level =>

            if Token = K_Import then
               if not In_Package then
                  Processor_State := Import_Package_Name;
               else
                  raise Invalid_Input with
                     "imports are not allowed inside package";
               end if;

               
            elsif Token = K_Package then
               if not In_Package then
                  Processor_State := Package_Name;
               else
                  raise Invalid_Input with
                     "packages cannot be nested";
               end if;

            elsif Token = K_Type then
               if not In_Package then
                  raise Invalid_Input with
                     "cannot define types outside of package";
               end if;
               
               Processor_State := Type_Name;
               
            elsif Token = K_Interface then
               if not In_Package then
                  raise Invalid_Input with
                     "cannot define interfaces outside of package";
               end if;
               
               Processor_State := Interface_Name;
               
            elsif Token = K_End then
               if not In_Package then
                  raise Invalid_Input with
                     "nothing to 'end' here (there is no package)";
               end if;
               
               Processor_State := Package_End_Name;
            
            else
               raise Invalid_Input with "invalid token";
            end if;

         when Import_Package_Name =>
         
            Ensure_Proper_Name (Token);
            
            Import_Pkg_Name := To_Unbounded_String (Token);
            
            Processor_State := Import_Semicolon;
         
         when Import_Semicolon =>

            if Processing_Mode = Full then
               Reset;
               Reader.Import_Package (To_String (Import_Pkg_Name));
               Structures.Collect_Import
                 (Imported_Package_Name => To_String (Import_Pkg_Name), Explicit => True);
               Reset;
            end if;
         
            Processor_State := Top_Level;

         when Package_Name =>
            
            --  mark parent packages as implicitly imported
            declare
               procedure Mark_Parent_Package_As_Imported (Package_Name : in String) is
                  Parent_Package_Name : constant String :=
                     Name_Utils.Trim_Last_Component (Package_Name);
               begin
                  if Parent_Package_Name = "" then
                     return;
                  end if;
                  
                  Structures.Collect_Import
                    (Imported_Package_Name => Parent_Package_Name, Explicit => False);
                  
                  Mark_Parent_Package_As_Imported (Parent_Package_Name);
               end Mark_Parent_Package_As_Imported;
            begin
               Mark_Parent_Package_As_Imported (Token);
            end;

            Structures.Create_Package (Token);

            --  really import all parent packages if the package is process in full mode
            declare
               procedure Import_Parent_Package (Package_Name : in String) is
                  Parent_Package_Name : constant String :=
                     Name_Utils.Trim_Last_Component (Package_Name);
               begin
                  if Parent_Package_Name = "" then
                     return;
                  end if;
                  
                  Reader.Import_Package (Parent_Package_Name);
                  
                  Import_Parent_Package (Parent_Package_Name);
               end Import_Parent_Package;
            begin
               if Processing_Mode = Full then
                  Import_Parent_Package (Token);
               end if;
            end;
            
            Current_Package_Name := To_Unbounded_String (Token);
            
            Processor_State := Package_Is;
            
         when Package_Is =>

            if Token = K_Is then
               In_Package := True;
               Processor_State := Top_Level;
            else
               raise Invalid_Input with
                  "'is' expected after package name";
            end if;
            
         when Type_Name =>

            Name_Utils.Verify_Name_Is_Simple (Token);
            
            Current_Type_Name := To_Unbounded_String (Token);

            Structures.Create_Type (To_String (Current_Package_Name), Token);

            Processor_State := Type_Is;
            
         when Type_Is =>

            if Token = K_Is then
               Processor_State := Type_Fields;
            else
               raise Invalid_Input with
                  "'is' expected after type name";
            end if;
            
         when Type_Fields =>

            if Token = K_End then
               Processor_State := Type_End_Name;
            else
               Name_Utils.Verify_Name_Is_Simple (Token);
               
               Current_Field_Name := To_Unbounded_String (Token);
               
               Processor_State := Type_Field_Colon;
            end if;
            
         when Type_Field_Colon =>
            
            if Token = K_Colon then
               Current_Field_Is_Optional := False;
               Processor_State := Type_Field_Type;
            else
               raise Invalid_Input with
                  "':' expected after field name";
            end if;

         when Type_Field_Type =>
            
            if Token = K_Optional then
               Current_Field_Is_Optional := True;
            else
               Current_Field_Type := To_Unbounded_String (Token);
               Processor_State := Type_Field_End;
            end if;
            
         when Type_Field_End =>

            if Token = K_Semicolon then

               Structures.Create_Field
                 (To_String (Current_Package_Name),
                  To_String (Current_Type_Name),
                  To_String (Current_Field_Name),
                  To_String (Current_Field_Type),
                  Current_Field_Is_Optional);
                  
               Processor_State := Type_Fields;
            end if;
               
         when Type_End_Name =>

            if Token = To_String (Current_Type_Name) then
               Processor_State := Type_Semicolon;
            else
               raise Invalid_Input with
                  "'" & To_String (Current_Type_Name) & "' expected here";
            end if;
            
         when Type_Semicolon =>

            if Token = K_Semicolon then
               Processor_State := Top_Level;
            else
               raise Invalid_Input with
                  "';' expected here";
            end if;
            
         when Interface_Name =>

            Name_Utils.Verify_Name_Is_Simple (Token);
            
            Current_Interface_Name := To_Unbounded_String (Token);

            Structures.Create_Interface
              (To_String (Current_Package_Name), Token);

            Processor_State := Interface_Is;
            
         when Interface_Is =>

            if Token = K_Is then
               Current_Message_Is_Oneway := False;
               Processor_State := Interface_Messages;
            else
               raise Invalid_Input with
                  "'is' expected after interface name";
            end if;
            
         when Interface_Messages =>

            if Token = K_Oneway then
               Current_Message_Is_Oneway := True;
            elsif Token = K_Message then
               Processor_State := Interface_Message_Name;
            elsif Token = K_End then
               Processor_State := Interface_End_Name;
            else
               raise Invalid_Input with
                  "'oneway' or 'message' or 'end' expected here";
            end if;
            
         when Interface_Message_Name =>

            Name_Utils.Verify_Name_Is_Simple (Token);
            
            Current_Message_Name := To_Unbounded_String (Token);

            In_Param_Name := Null_Unbounded_String;
            Out_Param_Name := Null_Unbounded_String;
            
            Processor_State := Interface_Message_Signature;
            
         when Interface_Message_Signature =>

            if Token = K_Open then
               Processor_State := Interface_Message_Param_Name;
            elsif Token = K_Semicolon then

               Structures.Create_Message
                 (To_String (Current_Package_Name),
                  To_String (Current_Interface_Name),
                  To_String (Current_Message_Name),
                  Current_Message_Is_Oneway,
                  To_String (In_Param_Name),
                  To_String (In_Param_Type),
                  To_String (Out_Param_Name),
                  To_String (Out_Param_Type));
               
               Current_Message_Is_Oneway := False;
               Processor_State := Interface_Messages;
            else
               raise Invalid_Input with
                  "'(' for the message signature or ';' expected here";
            end if;
            
         when Interface_Message_Param_Name =>
         
            Name_Utils.Verify_Name_Is_Simple (Token);
            
            Param_Name := To_Unbounded_String (Token);
            
            Processor_State := Interface_Message_Param_Colon;
            
         when Interface_Message_Param_Colon =>
            
            if Token = K_Colon then
               Processor_State := Interface_Message_Param_Mode;
            else
               raise Invalid_Input with
                  "':' expected here.";
            end if;
            
         when Interface_Message_Param_Mode =>
         
            if Token = K_In then
               if In_Param_Name /= Null_Unbounded_String then
                  raise Invalid_Input with
                     "parameter of 'in' mode was already specified " &
                     "for this message";
               else
                  Param_Mode := To_Unbounded_String (Token);
               end if;
            elsif Token = K_Out then
               if Current_Message_Is_Oneway then
                  raise Invalid_Input with
                     "oneway messages cannot have out parameters";
                     
               elsif Out_Param_Name /= Null_Unbounded_String then
                  raise Invalid_Input with
                     "parameter of 'out' mode was already specified " &
                     "for this message";
                     
               else
                  Param_Mode := To_Unbounded_String (Token);
               end if;
            else
               raise Invalid_Input with
                  "'in' or 'out' parameter mode expected here";
            end if;
            
            Processor_State := Interface_Message_Param_Type;
            
         when Interface_Message_Param_Type =>
         
            if Processing_Mode = Full then
               Structures.Verify_Param_Type_Is_User_Defined
                 (To_String (Current_Package_Name), Token);
            end if;
            
            if Param_Mode = To_Unbounded_String (K_In) then
               In_Param_Name := Param_Name;
               In_Param_Type := To_Unbounded_String (Token);
            else --  mode = out
               Out_Param_Name := Param_Name;
               Out_Param_Type := To_Unbounded_String (Token);
            end if;
            
            Processor_State := Interface_Message_Param_End;
            
         when Interface_Message_Param_End =>
         
            if Token = K_Semicolon then
               Processor_State := Interface_Message_Param_Name;
            elsif Token = K_Close then
               Processor_State := Interface_Message_Semicolon;
            else
               raise Invalid_Input with
                  "';' or ')' expected after parameter specification";
            end if;
            
         when Interface_Message_Semicolon =>

            if Token = K_Semicolon then

               Structures.Create_Message
                 (To_String (Current_Package_Name),
                  To_String (Current_Interface_Name),
                  To_String (Current_Message_Name),
                  Current_Message_Is_Oneway,
                  To_String (In_Param_Name),
                  To_String (In_Param_Type),
                  To_String (Out_Param_Name),
                  To_String (Out_Param_Type));
               
               Current_Message_Is_Oneway := False;
               Processor_State := Interface_Messages;
            else
               raise Invalid_Input with
                  "';' expected at the end of message definition";
            end if;
            
         when Interface_End_Name =>

            if Token = To_String (Current_Interface_Name) then
               Processor_State := Type_Semicolon;
            else
               raise Invalid_Input with
                  "'" & To_String (Current_Interface_Name) &
                  "' expected here.";
            end if;
            
         when Interface_Semicolon =>

            if Token = K_Semicolon then
               Processor_State := Top_Level;
            else
               raise Invalid_Input with
                  "';' expected after interface definition";
            end if;
            
         when Package_End_Name =>

            if Token = To_String (Current_Package_Name) then
               Processor_State := Package_Semicolon;
            else
               raise Invalid_Input with
                  "'" & To_String (Current_Package_Name) &
                  "' expected here";
            end if;
            
         when Package_Semicolon =>

            if Token = K_Semicolon then
               Processor_State := Finish;
               In_Package := False;
               
               Structures.Finish_Package (To_String (Current_Package_Name));
            else
               raise Invalid_Input with
                  "';' expected after package definition";
            end if;
            
         when Finish =>

            raise Invalid_Input with
               "no text is expected after package definition";
         
      end case;
   end Process_Token;

   function Finished return Boolean is
   begin
      return Processor_State = Finish;
   end Finished;
   
   procedure Reset is
   begin
      Processor_State := Top_Level;
      In_Package := False;
   end Reset;
   
   procedure Set_Mode (Mode : in Input_Mode) is
   begin
      Processing_Mode := Mode;
   end Set_Mode;
   
end IDL.Parser;
