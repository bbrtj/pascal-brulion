unit UniqName;

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils, Classes;

procedure SetUniqName(Item: TComponent);

implementation

var
	LastId: UInt32;

procedure SetUniqName(Item: TComponent);
begin
	Inc(LastId);
	Item.Name := Item.Name + IntToStr(LastId);
end;

initialization
	LastId := 0;
end.

