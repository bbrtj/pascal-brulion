unit LanesContainer;

interface

uses
	JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls,
	BrulionTypes, BrulionContainer, BrulionState, UniqName,
	BrulionPipelines, BrulionUiPipelines,
	NoteContainer,
	DragDrop;

type

	{ TLaneFrame }

	TLaneFrame = class(TWFrame)
		NewNoteButton: TWButton;
		LaneNameLabel: TWLabel;
		DeleteLaneButton: TWButton;
		LanePanel: TWPanel;
		procedure DeleteLane(Sender: TObject);
		procedure DragStop(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: NativeInt);
		procedure LaneFrameResize(Sender: TObject);
		procedure NewNote(Sender: TObject);
	private
		FLane: TLaneData;
		procedure SetLane(AValue: TLaneData);
		procedure SetParent(AValue: TWinControl);
		procedure NoteCreated(Sender: TObject);
	public
		constructor Create(AOwner: TComponent); override;
	public
		procedure NoteDeleted(Sender: TObject);
		procedure Reload();
		procedure ReAlign(); override;
	public
		property Parent write SetParent;
		property Lane: TLaneData read FLane write SetLane;
	end;

implementation

uses MainWindow;

{$R *.lfm}

{ TLaneFrame }

procedure TLaneFrame.LaneFrameResize(Sender: TObject);
begin
	self.LanePanel.Width := self.Width;
end;

procedure TLaneFrame.NewNote(Sender: TObject);
var
	LModalPipeline: TNoteModalPipeline;
	LCreatePipeline: TCreateNotePipeline;
	LLoadPipeline: TLoadNotePipeline;
begin
	LModalPipeline := TPipelineManager(GContainer[csPipelineManager])
		.New(TNoteModalPipeline) as TNoteModalPipeline;
	LCreatePipeline := TPipelineManager(GContainer[csPipelineManager])
		.New(TCreateNotePipeline) as TCreateNotePipeline;
	LLoadPipeline := TPipelineManager(GContainer[csPipelineManager])
		.New(TLoadNotePipeline) as TLoadNotePipeline;

	LModalPipeline.Form := TMainForm(self.Owner);
	LModalPipeline.LaneId := self.Lane.Id;
	LModalPipeline.SetNext(LCreatePipeline);

	LCreatePipeline.SetNext(LLoadPipeline);

	LLoadPipeline.SetNext(@self.NoteCreated);

	LModalPipeline.Start(nil);
end;

procedure TLaneFrame.DeleteLane(Sender: TObject);
var
	LConfirmPipeline: TConfirmPipeline;
	LDeletePipeline: TDeleteLanePipeline;
begin
	LConfirmPipeline := TPipelineManager(GContainer[csPipelineManager])
		.New(TConfirmPipeline) as TConfirmPipeline;
	LDeletePipeline := TPipelineManager(GContainer[csPipelineManager])
		.New(TDeleteLanePipeline) as TDeleteLanePipeline;

	LConfirmPipeline.Form := TMainForm(self.Owner);
	LConfirmPipeline.ConfirmText := Format('Permanently delete lane "%s"?', [self.Lane.Name]);
	LConfirmPipeline.SetNext(LDeletePipeline);

	LDeletePipeline.Data := self.Lane;
	LDeletePipeline.SetNext(@TMainForm(self.Owner).LoadLanesComplete);

	LConfirmPipeline.Start(nil);
end;

procedure TLaneFrame.DragStop(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: NativeInt);
var
	LDragged: TObject;
	LUpdatePipeline: TChangeLanePipeline;
	LNeedUpdating: Boolean;
begin
	LDragged := EndDragging();
	if (LDragged = nil) or not(LDragged is TNoteData) or (TNoteData(LDragged).LaneId = self.Lane.Id) then
		exit;

	TBrulionState(GContainer[csState]).Lanes.Dirty[self.Lane.Id] := true;
	TBrulionState(GContainer[csState]).Lanes.Dirty[TNoteData(LDragged).LaneId] := true;

	LUpdatePipeline := TPipelineManager(GContainer[csPipelineManager])
		.New(TChangeLanePipeline) as TChangeLanePipeline;
	LUpdatePipeline.Data := TNoteData(LDragged);
	LUpdatePipeline.NewLaneId := self.Lane.Id;
	LUpdatePipeline.SetNext(@TMainForm(self.Owner).ReloadDirtyLanes);

	LUpdatePipeline.Start(nil);

	{$IFDEF DEBUG}
	writeln('dragged note ', TNoteData(LDragged).Id, ' onto lane ', self.Lane.Id);
	{$ENDIF}
end;

procedure TLaneFrame.SetParent(AValue: TWinControl);
begin
	inherited Parent := AValue;
	self.Height := self.Parent.Height;
end;

procedure TLaneFrame.ReAlign();
const
	CExtraPanelHeight = 100;
var
	LCurrentOffset: Integer;
	I: Integer;
begin
	inherited ReAlign();
	if LanePanel = nil then exit;

	LCurrentOffset := 0;
	for I := 0 to LanePanel.ControlCount - 1 do begin
		LanePanel.Controls[I].Top := LCurrentOffset;
		LCurrentOffset += LanePanel.Controls[I].Height;
	end;

	// extra height for better look and some drop area
	LanePanel.Height := LCurrentOffset + CExtraPanelHeight;
end;

procedure TLaneFrame.NoteCreated(Sender: TObject);
begin
	self.Reload;
end;

procedure TLaneFrame.SetLane(AValue: TLaneData);
begin
	if FLane = AValue then Exit;
	FLane := AValue;
	LaneNameLabel.Caption := FLane.Name;
end;

constructor TLaneFrame.Create(AOwner: TComponent);
begin
	inherited;
	SetUniqName(self);
end;

procedure TLaneFrame.NoteDeleted(Sender: TObject);
begin
	self.Reload;
end;

procedure TLaneFrame.Reload;
var
	LNoteFrame: TNoteFrame;
	I: Integer;
begin
	for I := LanePanel.ControlCount - 1 downto 0 do begin
		LNoteFrame := LanePanel.Controls[I] as TNoteFrame;
		LNoteFrame.Free;
	end;

	for I := 0 to TBrulionState(GContainer[csState]).Notes[self.Lane.Id].Count - 1 do begin
		LNoteFrame := TNoteFrame.Create(self);
		LNoteFrame.Parent := LanePanel;
		LNoteFrame.Note := TBrulionState(GContainer[csState]).Notes[self.Lane.Id].Items[I];
	end;
end;

end.

