unit ConfirmDialog;

{$mode ObjFPC}{$H+}

interface

uses
	JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls;

type

	{ TConfirmDialogForm }

	TConfirmDialogForm = class(TWForm)
		ConfirmButton: TWButton;
		CancelButton: TWButton;
		DialogBox: TWLabel;
	private
		procedure SetDialogText(AValue: String);
	public
		property DialogText: String write SetDialogText;
	end;

var
	ConfirmDialogForm: TConfirmDialogForm;

implementation

{$R *.lfm}

{ TConfirmDialogForm }

procedure TConfirmDialogForm.SetDialogText(AValue: String);
begin
	DialogBox.Caption := AValue;
end;

end.

