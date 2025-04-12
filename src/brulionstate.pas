unit BrulionState;

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils,
	BrulionTypes, BrulionContainer;

type
	EBrulionState = class(Exception);

	TBrulionBoardsState = class
	private
		FContainer: TContainer;
		FCurrentBoard: TBoardData;
		FBoards: TBoardDataArray;
	private
		procedure SetCurrent(Board: TBoardData);
		function GetIndex(Board: TBoardData): Integer;
		function GetCurrentIndex(): Integer;
		function GetCount(): Integer;
	public
		constructor Create(Container: TContainer = nil);
		destructor Destroy; override;
	public
		function GetBoardById(const Id: TUlid): TBoardData;
		function GetBoardByIndex(Ind: Integer): TBoardData;
		procedure Init(Boards: TBoardDataArray);
		procedure Add(Boards: TBoardDataArray);
		procedure Remove(Board: TBoardData);
		procedure Clear();
	public
		property Current: TBoardData read FCurrentBoard write SetCurrent;
		property CurrentIndex: Integer read GetCurrentIndex;
		property Count: Integer read GetCount;
		property Items[I: Integer]: TBoardData read GetBoardByIndex;
	end;

	TBrulionState = class
	private
		FBoards: TBrulionBoardsState;
	public
		constructor Create(Container: TContainer = nil);
		destructor Destroy; override;
	public
		property Boards: TBrulionBoardsState read FBoards;
	end;

implementation

procedure TBrulionBoardsState.SetCurrent(Board: TBoardData);
var
	LFound: Boolean;
	I: Integer;
begin
	if Board <> nil then begin
		for I := 0 to High(FBoards) do begin
			LFound := FBoards[I] = Board;
			if LFound then break;
		end;

		if not LFound then
			raise EBrulionState.Create('board with identifier ' + Board.Id + ' is not known');

		FContainer.Storage.Items['current_board'] := Board.Id;
	end
	else
		FContainer.Storage.Items['current_board'] := CEmptyUlid;

	FCurrentBoard := Board;
end;

function TBrulionBoardsState.GetIndex(Board: TBoardData): Integer;
var
	I: Integer;
begin
	result := -1;
	for I := 0 to High(FBoards) do begin
		if FBoards[I] = Board then
			exit(I);
	end;
end;

function TBrulionBoardsState.GetCurrentIndex(): Integer;
begin
	result := self.GetIndex(FCurrentBoard);
end;

function TBrulionBoardsState.GetCount(): Integer;
begin
	result := Length(FBoards);
end;

constructor TBrulionBoardsState.Create(Container: TContainer);
begin
	FContainer := GetContainer(Container);
end;

destructor TBrulionBoardsState.Destroy();
begin
	self.Clear;
	FContainer.Free;
	inherited;
end;

function TBrulionBoardsState.GetBoardById(const Id: TUlid): TBoardData;
var
	I: Integer;
begin
	result := nil;
	for I := 0 to High(FBoards) do begin
		if FBoards[I].Id = Id then
			exit(FBoards[I]);
	end;
end;

function TBrulionBoardsState.GetBoardByIndex(Ind: Integer): TBoardData;
begin
	if (Ind < 0) or (Ind > High(FBoards)) then
		result := nil
	else
		result := FBoards[Ind];
end;

procedure TBrulionBoardsState.Init(Boards: TBoardDataArray);
begin
	self.Clear;
	self.Add(Boards);
	FCurrentBoard := self.GetBoardById(FContainer.Storage.Items['current_board']);
end;

procedure TBrulionBoardsState.Add(Boards: TBoardDataArray);
begin
	FBoards := Concat(FBoards, Boards);
end;

procedure TBrulionBoardsState.Remove(Board: TBoardData);
var
	LIndex: Integer;
begin
	LIndex := self.GetIndex(Board);
	if LIndex = -1 then exit;

	Delete(FBoards, LIndex, 1);
	if Board = FCurrentBoard then
		FCurrentBoard := nil;

	Board.Free;
end;

procedure TBrulionBoardsState.Clear();
var
	I: Integer;
begin
	for I := 0 to High(FBoards) do begin
		FreeAndNil(FBoards[I]);
	end;

	FBoards := [];
	FCurrentBoard := nil;
end;

constructor TBrulionState.Create(Container: TContainer);
begin
	FBoards := TBrulionBoardsState.Create(Container);
end;

destructor TBrulionState.Destroy();
begin
	FBoards.Free;
	inherited;
end;

end.

