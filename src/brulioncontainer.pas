unit BrulionContainer;

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils, Classes, Web,
	BrulionTypes;

type
	// Not all slots have their own interfaces - some must be casted by hand
	TContainerSlots = (
		csStorage, // IStorage
		csBoardsApi, // IBoardsApi
		csLanesApi, // ILanesApi
		csNotesApi, // INotesApi
		csState,
		csPipelineManager
	);

	TContainer = class(TPersistent)
	private
		FSlots: Array[TContainerSlots] of TObject;
		FSlotsOwned: Array[TContainerSlots] of Boolean;
	protected
		function GetService(Slot: TContainerSlots): TObject;
		procedure SetService(Slot: TContainerSlots; AValue: TObject);
		function GetStorage(): IStorage;
		function GetBoardsApi(): IBoardsApi;
		function GetLanesApi(): ILanesApi;
		function GetNotesApi(): INotesApi;
	public
		destructor Destroy; override;
	public
		procedure ServiceOwned(Slot: TContainerSlots; AValue: Boolean = True);
		procedure Assign(Other: TContainer);
	public
		property Services[Slot: TContainerSlots]: TObject read GetService write SetService; default;
		property Storage: IStorage read GetStorage;
		property BoardsApi: IBoardsApi read GetBoardsApi;
		property LanesApi: ILanesApi read GetLanesApi;
		property NotesApi: INotesApi read GetNotesApi;
	end;

var
	GContainer: TContainer;

function GetContainer(Container: TContainer = nil): TContainer;

implementation

function TContainer.GetService(Slot: TContainerSlots): TObject;
begin
	result := FSlots[Slot];
end;

procedure TContainer.SetService(Slot: TContainerSlots; AValue: TObject);
begin
	if (FSlots[Slot] <> nil) and FSlotsOwned[Slot] then
		FSlots[Slot].Free;

	FSlotsOwned[Slot] := False;
	FSlots[Slot] := AValue;
end;

function TContainer.GetStorage(): IStorage;
begin
	result := self.GetService(csStorage) as IStorage;
end;

function TContainer.GetBoardsApi(): IBoardsApi;
begin
	result := self.GetService(csBoardsApi) as IBoardsApi;
end;

function TContainer.GetLanesApi(): ILanesApi;
begin
	result := self.GetService(csLanesApi) as ILanesApi;
end;

function TContainer.GetNotesApi(): INotesApi;
begin
	result := self.GetService(csNotesApi) as INotesApi;
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
		result.Assign(GContainer)
	else
		result.Assign(Container);
end;

initialization
	GContainer := TContainer.Create;
{$IFNDEF PAS2JS}
finalization
	GContainer.Free;
{$ENDIF}
end.

