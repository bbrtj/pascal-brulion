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
		procedure AddBoardButtonClick(Sender: TObject);
		procedure AddLaneButtonClick(Sender: TObject);
		procedure LoadBoard(Sender: TObject);
		procedure Load(Sender: TObject);
	private
		FBoardData: TBoardsApiDataList;
		FTemporaryObjects: TObjectList;
	private
		function TemporaryObject(AObject: TObject): TObject;
		procedure UpdateBoards(Sender: TObject);
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
end;

procedure TMainForm.AddBoardButtonClick(Sender: TObject);
begin
	TNewBoardForm.Create(self).ShowModal(@CreateBoard);
end;

procedure TMainForm.LoadBoard(Sender: TObject);
begin
	if BoardListCombo.ItemIndex >= 0 then begin
		writeln(TWrappedBoardData(BoardListCombo.Items.Objects[BoardListCombo.ItemIndex]).Data.Id)
	end
	else begin
		// TODO: clear board
	end;
end;

procedure TMainForm.Load(Sender: TObject);
var
	LBoardsApi: TBoardsApi;
begin
	LBoardsApi := TBoardsApi.Create;
	LBoardsApi.LoadBoards(@self.UpdateBoards);
end;

function TMainForm.TemporaryObject(AObject: TObject): TObject;
begin
	FTemporaryObjects.Add(AObject);
	result := AObject;
end;

procedure TMainForm.UpdateBoards(Sender: TObject);
var
	I: Integer;
begin
	FBoardData := Sender as TBoardsApiDataList;
	for I := 0 to High(FBoardData.Value) do begin
		BoardListCombo.AddItem(
			FBoardData.Value[I].Name,
			TemporaryObject(TWrappedBoardData.Create(FBoardData.Value[I]))
		);
	end;

	// TODO: last used board (stored in webpage memory)
	if length(FBoardData.Value) > 0 then
		BoardListCombo.ItemIndex := 0;

	// not called automatically by modifying ItemIndex
	LoadBoard(Sender);
end;

procedure TMainForm.CreateBoard(Sender: TObject; ModalResult: TModalResult);
var
	LModal: TNewBoardForm;
begin
	LModal := TNewBoardForm(Sender);
	writeln(ModalResult);

	Self.RemoveComponent(LModal);
	LModal.Free;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	FTemporaryObjects := TObjectList.Create;
end;

destructor TMainForm.Destroy();
begin
	FBoardData.Free;
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

