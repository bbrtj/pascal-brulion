unit MainWindow;

interface

uses
	JS, Web, Classes, SysUtils, Graphics, Controls, Forms, WebCtrls, StdCtrls,
	Contnrs, Dialogs,
	NewBoard, LanesContainer, BrulionTypes, BrulionApiConnector, BrulionState;

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
		FState: TBrulionState;
		FBoardsApi: TBoardsApi;
		procedure BoardComboReload;
		procedure CreateBoardComplete(Sender: TObject);
		procedure DeleteBoardComplete(Sender: TObject);
		procedure DeleteBoardConfirmed(Sender: TObject; ModalResult: TModalResult);
		procedure LoadBoardComplete(Sender: TObject);
	private
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

procedure TMainForm.DeleteBoardConfirmed(Sender: TObject; ModalResult: TModalResult);
begin
	if ModalResult = mrYes then
		FBoardsApi.DeleteBoard(@DeleteBoardComplete, FState.Boards.Current.Id);
end;

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

procedure TMainForm.EnterBoard(Sender: TObject);
begin
	if BoardListCombo.ItemIndex >= 0 then begin
		FState.Boards.Current := TBoardData(BoardListCombo.Items.Objects[BoardListCombo.ItemIndex]);
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

procedure TMainForm.CreateBoardComplete(Sender: TObject);
begin
	FBoardsApi.LoadBoard(@LoadBoardComplete, TGeneralSuccessApiData(Sender).Value.Id);
end;

procedure TMainForm.DeleteBoardComplete(Sender: TObject);
begin
	FState.Boards.Remove(FState.Boards.Current);

	if FState.Boards.Count > 0 then
		FState.Boards.Current := FState.Boards.Items[0];

	BoardComboReload;
end;

procedure TMainForm.LoadBoardComplete(Sender: TObject);
var
	LBoard: TBoardData;
begin
	LBoard := TBoardsApiData(Sender).Snatch;
	FState.Boards.Add([LBoard]);
	FState.Boards.Current := LBoard;

	BoardComboReload;
end;

procedure TMainForm.LoadBoardsComplete(Sender: TObject);
var
	LResponse: TBoardsApiDataList;
begin
	LResponse := Sender as TBoardsApiDataList;
	// TODO: handle pagination
	FState.Boards.Clear;
	FState.Boards.Init(LResponse.Value);
	BoardComboReload;
	// TODO: draw board
end;

procedure TMainForm.CreateBoard(Sender: TObject; ModalResult: TModalResult);
var
	LModal: TNewBoardForm;
begin
	LModal := TNewBoardForm(Sender);
	if ModalResult = mrOk then
		FBoardsApi.CreateBoard(@CreateBoardComplete, LModal.NewBoardData);

	Self.RemoveComponent(LModal);
	LModal.Free;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	FState := TBrulionState.Create;
	FBoardsApi := TBoardsApi.Create;
end;

destructor TMainForm.Destroy();
begin
	FBoardsApi.Free;
	FState.Free;
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

