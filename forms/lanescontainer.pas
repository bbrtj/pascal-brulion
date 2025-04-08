unit LanesContainer;

interface

uses
	JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls,
	UniqName;

type

	{ TLaneFrame }

	TLaneFrame = class(TWFrame)
		WPanel1: TWPanel;
		procedure LaneFrameResize(Sender: TObject);
	private
		procedure SetParent(AValue: TWinControl);
	public
		constructor Create(AOwner: TComponent); override;
	public
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
	SetUniqName(self);
end;

end.

