unit MainForm;

interface

uses
	JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls, StdCtrls,
	LaneWidgets, BrulionTypes, BrulionApiConnector;

type

	{ TWForm1 }

	{ TMainForm }

	TMainForm = class(TWForm)
		AddLaneButton: TWButton;
		AddBoardButton: TWButton;
		BoardListCombo: TWComboBox;
		TopMenuPanel: TWPanel;
		BoardPanel: TWPanel;
		procedure AddLaneButtonClick(Sender: TObject);
	private
		FBoardData: TBoardDataList;
		procedure Load();
	private
		procedure UpdateBoards(Sender: TObject);
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

procedure TMainForm.Load();
var
	LBoardsApi: TBoardsApi;
begin
	LBoardsApi := TBoardsApi.Create;
	LBoardsApi.LoadBoards(@self.UpdateBoards);
end;

procedure TMainForm.UpdateBoards(Sender: TObject);
var
	I: Integer;
begin
	FBoardData := Sender as TBoardDataList;
	for I := 0 to High(FBoardData.List) do begin
		BoardListCombo.AddItem(FBoardData.List[I].Name, FBoardData.List[I]);
	end;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	Load;
end;

destructor TMainForm.Destroy();
begin
	FBoardData.Free;
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

