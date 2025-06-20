unit MainWindow;

interface

uses
	JS, Web, Classes, SysUtils, Graphics, Controls, Forms, WebCtrls, StdCtrls,
	Contnrs, Dialogs,
	NewBoard, NewLane, LanesContainer, BrulionTypes, BrulionApiConnector,
	BrulionState, BrulionContainer;

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
		procedure BoardChanged(Sender: TObject);
		procedure Load(Sender: TObject);
	private
		FState: TBrulionState;
	private
		procedure BoardComboReload;
		procedure LanesReload;
		procedure EnterBoard;
		procedure CreateBoardComplete(Sender: TObject);
		procedure CreateLaneComplete(Sender: TObject);
		procedure DeleteBoardComplete(Sender: TObject);
		procedure DeleteBoardConfirmed(Sender: TObject; ModalResult: TModalResult);
		procedure LoadBoardComplete(Sender: TObject);
		procedure LoadLaneComplete(Sender: TObject);
		procedure LoadBoardsComplete(Sender: TObject);
		procedure LoadLanesComplete(Sender: TObject);
		procedure CreateBoard(Sender: TObject; ModalResult: TModalResult);
		procedure CreateLane(Sender: TObject; ModalResult: TModalResult);
	public
		constructor Create(AOwner: TComponent); override;
		procedure ReAlign(); override;
	end;

	TLocalStorage = class(IStorage)
	protected
		function GetItem(const Name: String): String;
		procedure SetItem(const Name, Value: String);
	public
		function HasItem(const Name: String): Boolean;
	end;

var
	GMainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.DeleteBoardConfirmed(Sender: TObject; ModalResult: TModalResult);
begin
	if ModalResult = mrYes then
		GDefaultContainer.BoardsApi.DeleteBoard(@DeleteBoardComplete, FState.Boards.Current.Id);
end;

procedure TMainForm.AddLaneButtonClick(Sender: TObject);
begin
	if FState.Boards.Current = nil then begin
		ShowMessage('Please create or select a board');
		exit;
	end;

	TNewLaneForm.Create(self).ShowModal(@CreateLane);
end;

procedure TMainForm.DeleteBoard(Sender: TObject);
begin
	if FState.Boards.Current = nil then exit;

	MessageDlg(
		Self,
		Format('Permanently delete board "%s"?', [FState.Boards.Current.Name]),
		mtWarning,
		mbYesNo,
		mbNo,
		@DeleteBoardConfirmed
	);
end;

procedure TMainForm.AddBoardButtonClick(Sender: TObject);
begin
	TNewBoardForm.Create(self).ShowModal(@CreateBoard);
end;

procedure TMainForm.BoardChanged(Sender: TObject);
begin
	if BoardListCombo.ItemIndex >= 0 then begin
		FState.Boards.Current := TBoardData(BoardListCombo.Items.Objects[BoardListCombo.ItemIndex]);
	end
	else begin
		FState.Boards.Current := nil;
	end;

	EnterBoard;
end;

procedure TMainForm.Load(Sender: TObject);
begin
	GDefaultContainer.BoardsApi.LoadBoards(@self.LoadBoardsComplete);
end;

procedure TMainForm.BoardComboReload;
var
	I: Integer;
begin
	BoardListCombo.Clear;

	for I := 0 to FState.Boards.Count - 1 do begin
		BoardListCombo.AddItem(
			FState.Boards.Items[I].Name,
			FState.Boards.Items[I]
		);
	end;

	if FState.Boards.Current <> nil then
		BoardListCombo.ItemIndex := FState.Boards.CurrentIndex;
end;

procedure TMainForm.LanesReload;
var
	LLaneFrame: TLaneFrame;
	I: Integer;
begin
	for I := BoardPanel.ControlCount - 1 downto 0 do begin
		LLaneFrame := BoardPanel.Controls[I] as TLaneFrame;
		LLaneFrame.Free;
	end;

	for I := 0 to FState.Lanes.Count - 1 do begin
		LLaneFrame := TLaneFrame.Create(self);
	   	LLaneFrame.Parent := BoardPanel;
		LLaneFrame.Lane := FState.Lanes.Items[I];
	end;
end;

procedure TMainForm.EnterBoard;
begin
	if FState.Boards.Current = nil then exit;
	GDefaultContainer.LanesApi.LoadLanes(@LoadLanesComplete, FState.Boards.Current.Id);
end;

procedure TMainForm.CreateBoardComplete(Sender: TObject);
begin
	GDefaultContainer.BoardsApi.LoadBoard(@LoadBoardComplete, TGeneralSuccessApiData(Sender).Value.Id);
end;

procedure TMainForm.CreateLaneComplete(Sender: TObject);
begin
	GDefaultContainer.LanesApi.LoadLane(@LoadLaneComplete, TGeneralSuccessApiData(Sender).Value.Id);
end;

procedure TMainForm.DeleteBoardComplete(Sender: TObject);
begin
	FState.Boards.Remove(FState.Boards.Current);

	if FState.Boards.Count > 0 then
		FState.Boards.Current := FState.Boards.Items[0];

	BoardComboReload;
	EnterBoard;
end;

procedure TMainForm.LoadBoardComplete(Sender: TObject);
var
	LBoard: TBoardData;
begin
	LBoard := TBoardsApiData(Sender).Snatch;
	FState.Boards.Add([LBoard]);
	FState.Boards.Current := LBoard;

	BoardComboReload;
	EnterBoard;
end;

procedure TMainForm.LoadLaneComplete(Sender: TObject);
var
	LLane: TLaneData;
begin
	LLane := TLanesApiData(Sender).Snatch;
	FState.Lanes.Add([LLane]);
	LanesReload;
end;

procedure TMainForm.LoadBoardsComplete(Sender: TObject);
var
	LResponse: TBoardsApiDataList;
begin
	LResponse := Sender as TBoardsApiDataList;
	// TODO: handle pagination
	FState.Boards.Init(LResponse.Value);
	BoardComboReload;
	EnterBoard;
end;

procedure TMainForm.LoadLanesComplete(Sender: TObject);
var
	LResponse: TLanesApiDataList;
begin
	LResponse := Sender as TLanesApiDataList;
	// TODO: handle pagination
	FState.Lanes.Init(LResponse.Value);
	LanesReload;
end;

procedure TMainForm.CreateBoard(Sender: TObject; ModalResult: TModalResult);
var
	LModal: TNewBoardForm;
begin
	LModal := TNewBoardForm(Sender);
	if ModalResult = mrOk then
		GDefaultContainer.BoardsApi.CreateBoard(@CreateBoardComplete, LModal.NewBoardData);

	Self.RemoveComponent(LModal);
	LModal.Free;
end;

procedure TMainForm.CreateLane(Sender: TObject; ModalResult: TModalResult);
var
	LModal: TNewLaneForm;
	LData: TLaneData;
begin
	LModal := TNewLaneForm(Sender);
	if ModalResult = mrOk then begin
		LData := LModal.NewLaneData;
		LData.BoardId := FState.Boards.Current.Id;
		GDefaultContainer.LanesApi.CreateLane(@CreateLaneComplete, LData);
	end;

	Self.RemoveComponent(LModal);
	LModal.Free;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	GDefaultContainer.Services[csStorage] := TLocalStorage.Create;
	GDefaultContainer.ServiceOwned(csStorage);

	GDefaultContainer.Services[csState] := TBrulionState.Create;
	GDefaultContainer.ServiceOwned(csState);
	FState := TBrulionState(GDefaultContainer.Services[csState]);

	GDefaultContainer.Services[csBoardsApi] := TBoardsApi.Create;
	GDefaultContainer.ServiceOwned(csBoardsApi);

	GDefaultContainer.Services[csLanesApi] := TLanesApi.Create;
	GDefaultContainer.ServiceOwned(csLanesApi);
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

{ TLocalStorage }

function TLocalStorage.GetItem(const Name: String): String;
begin
	result := window.localStorage.Items[Name];
end;

procedure TLocalStorage.SetItem(const Name, Value: String);
begin
	window.localStorage.Items[Name] := Value;
end;

function TLocalStorage.HasItem(const Name: String): Boolean;
var
	I: Integer;
begin
	for I := 0 to window.localStorage.length do begin
		if window.localStorage.Keys[I] = Name then
			exit(True);
	end;

	result := False;
end;

end.

