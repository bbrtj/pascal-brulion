unit NewLane;

interface

uses
	JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls,
	BrulionTypes;

type

	{ TNewLaneForm }

 	TNewLaneForm = class(TWForm)
		ConfirmButton: TWButton;
		CancelButton: TWButton;
		NameEdit: TWEdit;
		NameLabel: TWLabel;
		procedure KeyPress(Sender: TObject; var Key: char);
	private
		FLaneData: TLaneData;
		function GetNewLaneData: TLaneData;
	public
		destructor Destroy; override;
	public
		property NewLaneData: TLaneData read GetNewLaneData;
	end;

implementation

{$R *.lfm}

{ TNewLaneForm }

destructor TNewLaneForm.Destroy();
begin
	FLaneData.Free;
	inherited;
end;

procedure TNewLaneForm.KeyPress(Sender: TObject; var Key: char);
begin
	if ord(Key) = 13 then
		self.ModalResult := mrOk;
end;

function TNewLaneForm.GetNewLaneData: TLaneData;
begin
	FLaneData.Free;
	FLaneData := TLaneData.Create;
	FLaneData.Name := NameEdit.Text;

	result := FLaneData;
end;

end.

