unit LanesContainer;

interface

uses
	JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls,
	BrulionTypes, UniqName;

type

	{ TLaneFrame }

	TLaneFrame = class(TWFrame)
		LaneNameLabel: TWLabel;
		WPanel1: TWPanel;
		procedure LaneFrameResize(Sender: TObject);
	private
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

{$R *.lfm}

{ TLaneFrame }

procedure TLaneFrame.LaneFrameResize(Sender: TObject);
begin
	self.WPanel1.Height := self.Height;
	self.WPanel1.Width := self.Width;
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
end;

end.

