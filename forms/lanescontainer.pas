unit LanesContainer;

interface

uses
	JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls,
	BrulionTypes, BrulionContainer, BrulionState, UniqName;

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
		procedure DeleteLaneConfirmed(Sender: TObject; ModalResult: TModalResult);
		procedure DeleteLaneComplete(Sender: TObject);
	public
		constructor Create(AOwner: TComponent); override;
	public
		property Parent write SetParent;
		property Lane: TLaneData read FLane write SetLane;
	end;

implementation

{$R *.lfm}

{ TLaneFrame }

procedure TLaneFrame.DeleteLaneConfirmed(Sender: TObject; ModalResult: TModalResult);
begin
	// if ModalResult = mrYes then
	// 	GContainer.LanesApi.DeleteLane(@DeleteLaneComplete, FState.Lanes.Current.Id);
end;

procedure TLaneFrame.DeleteLaneComplete(Sender: TObject);
begin
end;

procedure TLaneFrame.LaneFrameResize(Sender: TObject);
begin
	self.WPanel1.Width := self.Width;
end;

procedure TLaneFrame.DeleteLane(Sender: TObject);
begin
	// MessageDlg(
	// 	Self,
	// 	Format('Permanently delete lane "%s"?', [FLane.Name]),
	// 	mtWarning,
	// 	mbYesNo,
	// 	mbNo,
	// 	@DeleteLaneConfirmed
	// );
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

