unit BrulionContainer;

{$mode objfpc}{$H+}{$J-}
{$interfaces corba}

interface

uses SysUtils, Classes, Web;

type
	IStorage = interface
	['{4923fe8c-3623-428e-a4f7-a90e80b48402}']
	protected
		function GetItem(const Name: String): String;
		procedure SetItem(const Name, Value: String);
	public
		function HasItem(const Name: String): Boolean;
		property Items[I: String]: String read GetItem write SetItem;
	end;

	TContainerSlots = (
		csStorage,
		csBoardsApi,
		csLanesApi,
		csState
	);

	TContainer = class(TPersistent)
	private
		FSlots: Array[TContainerSlots] of TObject;
		FSlotsOwned: Array[TContainerSlots] of Boolean;
	protected
		function GetService(Slot: TContainerSlots): TObject;
		procedure SetService(Slot: TContainerSlots; AValue: TObject);
	public
		destructor Destroy; override;
	public
		procedure ServiceOwned(Slot: TContainerSlots; AValue: Boolean = True);
		procedure Assign(Other: TContainer);
	public
		property Services[Slot: TContainerSlots]: TObject read GetService write SetService;
	end;

var
	GDefaultContainer: TContainer;

function GetContainer(Container: TContainer = nil): TContainer;

implementation

function TContainer.GetService(Slot: TContainerSlots): TObject;
begin
	result := FSlots[Slot];
end;

procedure TContainer.SetService(Slot: TContainerSlots; AValue: TObject);
begin
	FSlotsOwned[Slot] := False;
	FSlots[Slot] := AValue;
end;

destructor TContainer.Destroy;
var
	I: TContainerSlots;
begin
	for I := low(FSlots) to high(FSlots) do begin
		if (FSlots[I] <> nil) and FSlotsOwned[I] then
			FSlots[I].Free;
	end;

	inherited;
end;

procedure TContainer.Assign(Other: TContainer);
var
	I: TContainerSlots;
begin
	for I := low(FSlots) to high(FSlots) do begin
		FSlots[I] := Other.FSlots[I];
		FSlotsOwned[I] := False;
	end;
end;

procedure TContainer.ServiceOwned(Slot: TContainerSlots; AValue: Boolean);
begin
	FSlotsOwned[Slot] := AValue;
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

{$IFNDEF PAS2JS}
finalization
	GDefaultContainer.Free;
{$ENDIF}
end.

