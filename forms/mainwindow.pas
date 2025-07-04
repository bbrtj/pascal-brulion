unit MainWindow;

interface

uses
	JS, Web, Classes, SysUtils, Graphics, Controls, Forms, WebCtrls, StdCtrls,
	Contnrs, Dialogs,
	NewBoard, NewLane, LanesContainer, BrulionTypes, BrulionApiConnector,
	BrulionState, BrulionContainer, BrulionPipelines, BrulionUiPipelines,
	DragDrop;

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
		procedure DragEnd(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: NativeInt);
		procedure Load(Sender: TObject);
	private
		FState: TBrulionState;
		FPipelines: TPipelineManager;
	private
		FLastNoteLoadPipeline: Integer;
		function BuildNoteLoadPipeline(Arg: TObject): TPipeline;
	private
		procedure BoardComboReload;
		procedure LanesReload;
		procedure EnterBoard;
		procedure DeleteBoardComplete(Sender: TObject);
		procedure LoadBoardComplete(Sender: TObject);
		procedure LoadLaneComplete(Sender: TObject);
		procedure LoadBoardsComplete(Sender: TObject);
	public
		constructor Create(AOwner: TComponent); override;
		procedure ReloadDirtyLanes(Sender: TObject);
		procedure LoadLanesComplete(Sender: TObject);
		procedure ReAlign(); override;
	end;

	TLocalStorage = class(IStorage)
	public
		function GetItem(const Name: String): String;
		procedure SetItem(const Name, Value: String);
		function HasItem(const Name: String): Boolean;
	public
		property Items[I: String]: String read GetItem write SetItem;
	end;

var
	GMainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.AddBoardButtonClick(Sender: TObject);
var
	LModalPipeline: TBoardModalPipeline;
	LCreatePipeline: TCreateBoardPipeline;
	LLoadPipeline: TLoadBoardPipeline;
begin
	LModalPipeline := FPipelines.New(TBoardModalPipeline) as TBoardModalPipeline;
	LCreatePipeline := FPipelines.New(TCreateBoardPipeline) as TCreateBoardPipeline;
	LLoadPipeline := FPipelines.New(TLoadBoardPipeline) as TLoadBoardPipeline;

	LModalPipeline.Form := self;
	LModalPipeline.SetNext(LCreatePipeline);

	LCreatePipeline.SetNext(LLoadPipeline);

	LLoadPipeline.SetNext(@self.LoadBoardComplete);

	LModalPipeline.Start(nil);
end;

procedure TMainForm.AddLaneButtonClick(Sender: TObject);
var
	LModalPipeline: TLaneModalPipeline;
	LCreatePipeline: TCreateLanePipeline;
	LLoadPipeline: TLoadLanePipeline;
begin
	if FState.Boards.Current = nil then begin
		ShowMessage('Please create or select a board');
		exit;
	end;

	LModalPipeline := FPipelines.New(TLaneModalPipeline) as TLaneModalPipeline;
	LCreatePipeline := FPipelines.New(TCreateLanePipeline) as TCreateLanePipeline;
	LLoadPipeline := FPipelines.New(TLoadLanePipeline) as TLoadLanePipeline;

	LModalPipeline.Form := self;
	LModalPipeline.BoardId := FState.Boards.Current.Id;
	LModalPipeline.SetNext(LCreatePipeline);

	LCreatePipeline.SetNext(LLoadPipeline);

	LLoadPipeline.SetNext(@self.LoadLaneComplete);

	LModalPipeline.Start(nil);
end;

procedure TMainForm.DeleteBoard(Sender: TObject);
var
	LConfirmPipeline: TConfirmPipeline;
	LDeletePipeline: TDeleteBoardPipeline;
begin
	if FState.Boards.Current = nil then exit;

	LConfirmPipeline := FPipelines.New(TConfirmPipeline) as TConfirmPipeline;
	LDeletePipeline := FPipelines.New(TDeleteBoardPipeline) as TDeleteBoardPipeline;

	LConfirmPipeline.Form := self;
	LConfirmPipeline.ConfirmText := Format('Permanently delete board "%s"?', [FState.Boards.Current.Name]);
	LConfirmPipeline.SetNext(LDeletePipeline);

	LDeletePipeline.Data := FState.Boards.Current;
	LDeletePipeline.SetNext(@self.DeleteBoardComplete);

	LConfirmPipeline.Start(nil);
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

procedure TMainForm.DragEnd(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: NativeInt);
begin
	EndDragging;
end;

procedure TMainForm.Load(Sender: TObject);
var
	LLoadPipeline: TLoadSystemBoardsPipeline;
begin
	LLoadPipeline := FPipelines.New(TLoadSystemBoardsPipeline) as TLoadSystemBoardsPipeline;
	LLoadPipeline.SetNext(@self.LoadBoardsComplete);
	LLoadPipeline.Start(nil);
end;

function TMainForm.BuildNoteLoadPipeline(Arg: TObject): TPipeline;
begin
	if FLastNoteLoadPipeline > High(TLanesApiDataList(Arg).Value) then
		exit(nil);

	result := FPipelines.New(TLoadLaneNotesPipeline);
	TLoadLaneNotesPipeline(result).Data := TLanesApiDataList(Arg).Value[FLastNoteLoadPipeline];
	Inc(FLastNoteLoadPipeline);
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
		LLaneFrame.Reload;
	end;
end;

procedure TMainForm.EnterBoard;
var
	LLanesPipeline: TLoadBoardLanesPipeline;
	LForkPipeline: TForkPipeline;
begin
	FLastNoteLoadPipeline := 0;
	if FState.Boards.Current = nil then exit;

	// clean up finished pipelines
	FPipelines.Cleanup;

	LLanesPipeline := FPipelines.New(TLoadBoardLanesPipeline) as TLoadBoardLanesPipeline;
	LForkPipeline := FPipelines.New(TForkPipeline) as TForkPipeline;

	LLanesPipeline.Data := FState.Boards.Current;
	LLanesPipeline.SetNext(LForkPipeline);

	LForkPipeline.BuildPipelineProc := @self.BuildNoteLoadPipeline;
	LForkPipeline.SetNext(@self.LoadLanesComplete);

	LLanesPipeline.Start(nil);
end;

procedure TMainForm.DeleteBoardComplete(Sender: TObject);
begin
	BoardComboReload;
	EnterBoard;
end;

procedure TMainForm.LoadBoardComplete(Sender: TObject);
begin
	FState.Boards.Current := Sender as TBoardData;
	BoardComboReload;
	EnterBoard;
end;

procedure TMainForm.LoadLaneComplete(Sender: TObject);
begin
	LanesReload;
end;

procedure TMainForm.LoadBoardsComplete(Sender: TObject);
begin
	if (FState.Boards.Current = nil) and (FState.Boards.Count > 0) then
		FState.Boards.Current := FState.Boards.Items[0];

	BoardComboReload;
	EnterBoard;
end;

procedure TMainForm.ReloadDirtyLanes(Sender: TObject);
var
	LLaneFrame: TLaneFrame;
	LLaneState: TBrulionLanesState;
	I: Integer;
begin
	LLaneState := TBrulionState(GContainer[csState]).Lanes;

	for I := BoardPanel.ControlCount - 1 downto 0 do begin
		LLaneFrame := BoardPanel.Controls[I] as TLaneFrame;
		if LLaneState.Dirty[LLaneFrame.Lane.Id] then
			LLaneFrame.Reload;
	end;
end;

procedure TMainForm.LoadLanesComplete(Sender: TObject);
begin
	LanesReload;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	GContainer[csStorage] := TLocalStorage.Create;
	GContainer.ServiceOwned(csStorage);

	GContainer[csState] := TBrulionState.Create;
	GContainer.ServiceOwned(csState);
	FState := GContainer[csState] as TBrulionState;

	GContainer[csBoardsApi] := TBoardsApi.Create;
	GContainer.ServiceOwned(csBoardsApi);

	GContainer[csLanesApi] := TLanesApi.Create;
	GContainer.ServiceOwned(csLanesApi);

	GContainer[csNotesApi] := TNotesApi.Create;
	GContainer.ServiceOwned(csNotesApi);

	GContainer[csPipelineManager] := TPipelineManager.Create;
	GContainer.ServiceOwned(csPipelineManager);
	FPipelines := GContainer[csPipelineManager] as TPipelineManager;
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

