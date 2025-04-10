unit NewBoard;

{$mode ObjFPC}{$H+}

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
	private
		function GetNewBoardData: TBoardData;

	public
		property NewBoardData: TBoardData read GetNewBoardData;
	end;

implementation

{$R *.lfm}

{ TNewBoardForm }

function TNewBoardForm.GetNewBoardData: TBoardData;
begin
	result.Name := NameEdit.Text;
end;

end.

