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
end;

function TNewLaneForm.GetNewLaneData: TLaneData;
begin
	FLaneData.Free;
	FLaneData := TLaneData.Create;
	FLaneData.Name := NameEdit.Text;

	result := FLaneData;
end;

end.

