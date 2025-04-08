unit NewBoard;

{$mode ObjFPC}{$H+}

interface

uses
	JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls;

type

	{ TNewBoardForm }

 TNewBoardForm = class(TWForm)
		ConfirmButton: TWButton;
		CancelButton: TWButton;
		procedure CancelButtonClick(Sender: TObject);
		procedure ConfirmButtonClick(Sender: TObject);
	private

	public

	end;

implementation

{$R *.lfm}

{ TNewBoardForm }

procedure TNewBoardForm.CancelButtonClick(Sender: TObject);
begin
	Close;
end;

procedure TNewBoardForm.ConfirmButtonClick(Sender: TObject);
begin
	Close;
end;

end.

