unit LanesContainer;

interface

uses
	JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls,
	BrulionTypes, BrulionContainer, BrulionState, UniqName,
	BrulionPipelines, BrulionUiPipelines;

type

	{ TLaneFrame }

	TLaneFrame = class(TWFrame)
		LaneNameLabel: TWLabel;
		DeleteLaneButton: TWButton;
		WPanel1: TWPanel;
		procedure DeleteLane(Sender: TObject);
		procedure LaneFrameResize(Sender: TObject);
	private
		FState: TBrulionState;
		FLane: TLaneData;
		procedure SetLane(AValue: TLaneData);
		procedure SetParent(AValue: TWinControl);
	public
		constructor Create(AOwner: TComponent); override;
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
	self.WPanel1.Width := self.Width;
end;

procedure TLaneFrame.DeleteLane(Sender: TObject);
var
	LPipelines: TPipelineManager;
	LConfirmPipeline: TConfirmPipeline;
	LDeletePipeline: TDeleteLanePipeline;
begin
	LPipelines := TPipelineManager(GContainer.Services[csPipelineManager]);
	LConfirmPipeline := LPipelines.New(TConfirmPipeline) as TConfirmPipeline;
	LDeletePipeline := LPipelines.New(TDeleteLanePipeline) as TDeleteLanePipeline;

	LConfirmPipeline.Form := TMainForm(self.Owner);
	LConfirmPipeline.ConfirmText := Format('Permanently delete lane "%s"?', [self.Lane.Name]);
	LConfirmPipeline.SetNext(LDeletePipeline);

	LDeletePipeline.Data := self.Lane;
	LDeletePipeline.SetNext(@TMainForm(self.Owner).LoadLanesComplete);

	LConfirmPipeline.Start(nil);
end;

procedure TLaneFrame.SetParent(AValue: TWinControl);
begin
	inherited Parent := AValue;
	self.Height := self.Parent.Height;
end;

procedure TLaneFrame.SetLane(AValue: TLaneData);
begin
	if FLane = AValue then Exit;
	FLane := AValue;
	LaneNameLabel.Caption := FLane.Name;
end;

constructor TLaneFrame.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	SetUniqName(self);

	FState := TBrulionState(GContainer.Services[csState]);
end;

end.

