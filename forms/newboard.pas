unit NewBoard;

interface

uses
	JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls,
	BrulionTypes;

type

	{ TNewBoardForm }

 TNewBoardForm = class(TWForm)
		ConfirmButton: TWButton;
		CancelButton: TWButton;
		NameEdit: TWEdit;
		NameLabel: TWLabel;
		procedure KeyPress(Sender: TObject; var Key: char);
	private
		FBoardData: TBoardData;
		function GetNewBoardData: TBoardData;
	public
		destructor Destroy; override;
	public
		property NewBoardData: TBoardData read GetNewBoardData;
	end;

implementation

{$R *.lfm}

{ TNewBoardForm }

procedure TNewBoardForm.KeyPress(Sender: TObject; var Key: char);
begin
	if ord(Key) = 13 then
		self.ModalResult := mrOk;
end;

destructor TNewBoardForm.Destroy();
begin
	FBoardData.Free;
end;

function TNewBoardForm.GetNewBoardData: TBoardData;
begin
	FBoardData.Free;
	FBoardData := TBoardData.Create;
	FBoardData.Name := NameEdit.Text;

	result := FBoardData;
end;

end.

