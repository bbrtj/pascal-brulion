unit DragDrop;

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils;

var
	GDraggedObject: TObject;

procedure StartDragging(What: TObject);
function IsDragging(): Boolean;
function EndDragging(): TObject;

implementation

procedure StartDragging(What: TObject);
begin
	GDraggedObject := What;
end;

function IsDragging(): Boolean;
begin
	result := GDraggedObject <> nil;
end;

function EndDragging(): TObject;
begin
	result := GDraggedObject;
	GDraggedObject := nil;
end;

initialization
	GDraggedObject := nil;
end.

