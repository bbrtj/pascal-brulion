unit LaneWidgets;

interface

uses
	JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls;

type

	{ TLaneFrame }

	TLaneFrame = class(TWFrame)
		WPanel1: TWPanel;
		procedure LaneFrameResize(Sender: TObject);
	private class var
		LastName: Integer;
	private
		procedure SetParent(AValue: TWinControl);
	public
		constructor Create(AOwner: TComponent); override;

		property Parent write SetParent;
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

constructor TLaneFrame.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

	TLaneFrame.LastName += 1;
	self.Name := self.Name + IntToStr(TLaneFrame.LastName);
end;

initialization
	TLaneFrame.LastName := 0;
end.

