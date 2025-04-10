unit MainWindow;

interface

uses
	JS, Classes, SysUtils, Graphics, Controls, Forms, WebCtrls, StdCtrls,
	Contnrs,
	NewBoard, LanesContainer, BrulionTypes, BrulionApiConnector;

type

	{ TMainForm }

	TMainForm = class(TWForm)
		AddLaneButton: TWButton;
		AddBoardButton: TWButton;
		BoardListCombo: TWComboBox;
		TopMenuPanel: TWPanel;
		BoardPanel: TWPanel;
		DeleteBoardButton: TWButton;
		procedure AddBoardButtonClick(Sender: TObject);
		procedure AddLaneButtonClick(Sender: TObject);
		procedure DeleteBoard(Sender: TObject);
		procedure EnterBoard(Sender: TObject);
		procedure Load(Sender: TObject);
	private
		FBoardsApi: TBoardsApi;
		FBoardData: TBoardDataArray;
		FCurrentBoardId: TUlid;
		FTemporaryObjects: TObjectList;
		procedure BoardComboReload;
		procedure CreateBoardComplete(Sender: TObject);
		procedure DeleteBoardComplete(Sender: TObject);
		procedure LoadBoard(const Id: TUlid);
		procedure LoadBoardComplete(Sender: TObject);
	private
		function TemporaryObject(AObject: TObject): TObject;
		procedure LoadBoardsComplete(Sender: TObject);
		procedure CreateBoard(Sender: TObject; ModalResult: TModalResult);
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy(); override;
		procedure ReAlign(); override;
	end;

var
	GMainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.AddLaneButtonClick(Sender: TObject);
var
	LLane: TLaneFrame;
begin
	LLane := TLaneFrame.Create(self);
	LLane.Parent := BoardPanel;
	BoardListCombo.ReAlign;
end;

procedure TMainForm.DeleteBoard(Sender: TObject);
begin
	FBoardsApi.DeleteBoard(@DeleteBoardComplete, FCurrentBoardId);
end;

procedure TMainForm.AddBoardButtonClick(Sender: TObject);
begin
	TNewBoardForm.Create(self).ShowModal(@CreateBoard);
end;

procedure TMainForm.EnterBoard(Sender: TObject);
begin

	if BoardListCombo.ItemIndex >= 0 then begin
		FCurrentBoardId := TWrappedBoardData(BoardListCombo.Items.Objects[BoardListCombo.ItemIndex]).Data.Id;
		writeln(TWrappedBoardData(BoardListCombo.Items.Objects[BoardListCombo.ItemIndex]).Data.Id)
	end
	else begin
		// TODO: clear board
	end;
end;

procedure TMainForm.Load(Sender: TObject);
begin
	FBoardsApi.LoadBoards(@self.LoadBoardsComplete);
end;

procedure TMainForm.BoardComboReload;
var
	I, WantedIndex: Integer;
begin
	WantedIndex := -1;
	BoardListCombo.Clear;

   	for I := 0 to High(FBoardData) do begin
   		BoardListCombo.AddItem(
   			FBoardData[I].Name,
   			TemporaryObject(TWrappedBoardData.Create(FBoardData[I]))
   		);

		if FBoardData[I].Id = FCurrentBoardId then
			WantedIndex := I;
   	end;

   	if WantedIndex >= 0 then
   		BoardListCombo.ItemIndex := WantedIndex;
end;

procedure TMainForm.CreateBoardComplete(Sender: TObject);
begin
	FCurrentBoardId := TGeneralSuccessApiData(Sender).Value.Id;
	LoadBoard(FCurrentBoardId);
end;

procedure TMainForm.DeleteBoardComplete(Sender: TObject);
var
	I, J: Integer;
	LNewBoardData: TBoardDataArray;
begin
	SetLength(LNewBoardData, Length(FBoardData) - 1);
	J := 0;
	for I := 0 to High(FBoardData) do begin
		if FBoardData[I].Id = FCurrentBoardId then
			continue;
		LNewBoardData[J] := FBoardData[I];
		Inc(J);
	end;

	FBoardData := LNewBoardData;
	FCurrentBoardId := '';
	BoardComboReload;
end;

procedure TMainForm.LoadBoard(const Id: TUlid);
begin
	FBoardsApi.LoadBoard(@LoadBoardComplete, Id);
end;

procedure TMainForm.LoadBoardComplete(Sender: TObject);
begin
	FBoardData := Concat([TBoardsApiData(Sender).Value], FBoardData);
	BoardComboReload;
end;

function TMainForm.TemporaryObject(AObject: TObject): TObject;
begin
	FTemporaryObjects.Add(AObject);
	result := AObject;
end;

procedure TMainForm.LoadBoardsComplete(Sender: TObject);
var
	LResponse: TBoardsApiDataList;
begin
	LResponse := Sender as TBoardsApiDataList;
	// TODO: handle pagination
   	// TODO: last used board (stored in webpage memory)
	FBoardData := LResponse.Value;
	BoardComboReload;
	EnterBoard(Sender);
end;

procedure TMainForm.CreateBoard(Sender: TObject; ModalResult: TModalResult);
var
	LModal: TNewBoardForm;
begin
	LModal := TNewBoardForm(Sender);
	if ModalResult = mrOk then begin
		FBoardsApi.CreateBoard(@CreateBoardComplete, LModal.NewBoardData);
	end;

	Self.RemoveComponent(LModal);
	LModal.Free;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	FTemporaryObjects := TObjectList.Create;
	FBoardsApi := TBoardsApi.Create;
end;

destructor TMainForm.Destroy();
begin
	FBoardsApi.Free;
	FTemporaryObjects.Free;
	inherited;
end;

procedure TMainForm.ReAlign();
var
	LCurrentOffset: Integer;
	I: Integer;
begin
	inherited ReAlign();
	if BoardPanel = nil then exit;

	LCurrentOffset := 0;
	for I := 0 to BoardPanel.ControlCount - 1 do begin
		BoardPanel.Controls[I].Left := LCurrentOffset;
		LCurrentOffset += BoardPanel.Controls[I].Width;
	end;
end;

end.

