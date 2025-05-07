unit BrulionContainer;

{$mode objfpc}{$H+}{$J-}
{$interfaces corba}

interface

uses SysUtils, Classes, Web;

type
	TStorage = class abstract
	protected
		function GetItem(const Name: String): String; virtual; abstract;
		procedure SetItem(const Name, Value: String); virtual; abstract;
	public
		property Items[I: String]: String read GetItem write SetItem;
	end;

	TLocalStorage = class(TStorage)
	protected
		function GetItem(const Name: String): String; override;
		procedure SetItem(const Name, Value: String); override;
	end;

	TContainerSlots = (
		csStorage
		csBoardsApi
		csLanesApi
		csState
	);

	TContainer = class(TPersistent)
	private
		FSlots = Array[TContainerSlots] of TObject;
		FSlotsOwned = Array[TContainerSlots] of Boolean;
	public
		procedure Assign(Other: TContainer);
	end;

var
	GDefaultContainer: TContainer;

function GetContainer(Container: TContainer = nil): TContainer;

implementation

function TLocalStorage.GetItem(const Name: String): String;
begin
	result := window.localStorage.getItem(Name);
end;

procedure TLocalStorage.SetItem(const Name, Value: String);
begin
	window.localStorage.setItem(Name, Value);
end;

procedure TContainer.SetStorage(AStorage: TStorage);
begin
	FOwnsStorage := True;
	FStorage := AStorage;
end;

destructor TContainer.Destroy();
begin
	if FOwnsStorage then
		FStorage.Free;

	inherited;
end;

procedure TContainer.Assign(Other: TContainer);
begin
	FOwnsStorage := False;
	FStorage := Other.Storage;
end;

function GetContainer(Container: TContainer): TContainer;
begin
	result := TContainer.Create;
	if Container = nil then
		result.Assign(GDefaultContainer)
	else
		result.Assign(Container);
end;

initialization
	GDefaultContainer := TContainer.Create;
	GDefaultContainer.Storage := TLocalStorage.Create;

{$IFNDEF PAS2JS}
finalization
	GDefaultContainer.Free;
{$ENDIF}
end.

