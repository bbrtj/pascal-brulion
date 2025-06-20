unit BrulionState;

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils,
	BrulionTypes, BrulionContainer;

type
	EBrulionState = class(Exception);

	TBrulionState = class;

	generic TBrulionStateHolder<T: TBrulionData> = class
	private type
		TArray = Array of T;
	private
		FParent: TBrulionState;
		FItems: TArray;
	private
		function GetContainer(): TContainer;
	protected
		function GetIndex(AItem: T): Integer;
	protected
		property Parent: TBrulionState read FParent;
		property Container: TContainer read GetContainer;
	public
		constructor Create(Parent: TBrulionState);
		destructor Destroy; override;
	public
		function GetItem(Ind: Integer): T;
		function GetCount(): Integer;
		procedure Init(AItems: TArray); virtual;
		procedure Add(AItems: TArray); virtual;
		procedure Remove(AItem: T); virtual;
		procedure Clear(); virtual;
	public
		property Count: Integer read GetCount;
		property Items[I: Integer]: T read GetItem;
	end;

	TBrulionBoardsState = class(specialize TBrulionStateHolder<TBoardData>)
	private
		FCurrentBoard: TBoardData;
	private
		procedure SetCurrent(Board: TBoardData);
		function GetCurrentIndex(): Integer;
	public
		function GetBoardById(const Id: TUlid): TBoardData;
		procedure Init(Boards: TBoardDataArray); override;
		procedure Remove(Board: TBoardData); override;
		procedure Clear(); override;
	public
		property Current: TBoardData read FCurrentBoard write SetCurrent;
		property CurrentIndex: Integer read GetCurrentIndex;
	end;

	TBrulionLanesState = class(specialize TBrulionStateHolder<TLaneData>)
	end;

	TBrulionState = class
	private
		FContainer: TContainer;
		FBoards: TBrulionBoardsState;
		FLanes: TBrulionLanesState;
	public
		constructor Create(Container: TContainer = nil);
		destructor Destroy; override;
	public
		property Container: TContainer read FContainer;
		property Boards: TBrulionBoardsState read FBoards;
		property Lanes: TBrulionLanesState read FLanes;
	end;

implementation

function TBrulionStateHolder.GetContainer(): TContainer;
begin
	result := FParent.Container;
end;

function TBrulionStateHolder.GetIndex(AItem: T): Integer;
var
	I: Integer;
begin
	result := -1;
	for I := 0 to High(FItems) do begin
		if FItems[I] = AItem then
			exit(I);
	end;
end;

constructor TBrulionStateHolder.Create(Parent: TBrulionState);
begin
	FParent := Parent;
end;

destructor TBrulionStateHolder.Destroy();
begin
	self.Clear;
	inherited;
end;

function TBrulionStateHolder.GetItem(Ind: Integer): T;
begin
	if (Ind < Low(FItems)) or (Ind > High(FItems)) then
		result := nil
	else
		result := FItems[Ind];
end;

function TBrulionStateHolder.GetCount(): Integer;
begin
	result := Length(FItems);
end;

procedure TBrulionStateHolder.Init(AItems: TArray);
begin
	self.Clear;
	self.Add(AItems);
end;

procedure TBrulionStateHolder.Add(AItems: TArray);
begin
	FItems := Concat(FItems, AItems);
end;

procedure TBrulionStateHolder.Remove(AItem: T);
var
	LIndex: Integer;
begin
	LIndex := self.GetIndex(AItem);
	if LIndex = -1 then exit;

	Delete(FItems, LIndex, 1);
	AItem.Free;
end;

procedure TBrulionStateHolder.Clear();
var
	I: Integer;
begin
	for I := 0 to High(FItems) do begin
		FreeAndNil(FItems[I]);
	end;

	FItems := [];
end;

procedure TBrulionBoardsState.SetCurrent(Board: TBoardData);
var
	LFound: Integer;
begin
	if Board = FCurrentBoard then exit;
	if Board <> nil then begin
		LFound := self.GetIndex(Board);

		if LFound = -1 then
			raise EBrulionState.Create('board with identifier ' + Board.Id + ' is not known');

		self.Container.Storage.Items['current_board'] := Board.Id;
	end
	else
		self.Container.Storage.Items['current_board'] := CEmptyUlid;

	FCurrentBoard := Board;
	self.Parent.Lanes.Clear;
end;

function TBrulionBoardsState.GetCurrentIndex(): Integer;
begin
	result := self.GetIndex(FCurrentBoard);
end;

function TBrulionBoardsState.GetBoardById(const Id: TUlid): TBoardData;
var
	I: Integer;
begin
	result := nil;
	for I := 0 to self.Count do begin
		if self.Items[I].Id = Id then
			exit(self.Items[I]);
	end;
end;

procedure TBrulionBoardsState.Init(Boards: TBoardDataArray);
begin
	inherited;

	if self.Container.Storage.HasItem('current_board') then
		FCurrentBoard := self.GetBoardById(self.Container.Storage.Items['current_board']);
end;

procedure TBrulionBoardsState.Remove(Board: TBoardData);
begin
	if Board = FCurrentBoard then
		FCurrentBoard := nil;

	inherited;
end;

procedure TBrulionBoardsState.Clear();
begin
	inherited;
	FCurrentBoard := nil;
end;

constructor TBrulionState.Create(Container: TContainer);
begin
	FContainer := GetContainer(Container);
	FLanes := TBrulionLanesState.Create(self);
	FBoards := TBrulionBoardsState.Create(self);
end;

destructor TBrulionState.Destroy();
begin
	FBoards.Free;
	FLanes.Free;
	FContainer.Free;
	inherited;
end;

end.

