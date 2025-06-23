unit NewNote;

{$mode ObjFPC}{$H+}

interface

uses
	JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls,
	BrulionTypes;

type

	{ TNewNoteForm }

	TNewNoteForm = class(TWForm)
		CancelButton: TWButton;
		ConfirmButton: TWButton;
		ContentEdit: TWMemo;
		ContentLabel: TWLabel;
		procedure KeyPress(Sender: TObject; var Key: char);
	private
		FNoteData: TNoteData;
		function GetNewNoteData: TNoteData;
	public
		destructor Destroy; override;
	public
		property NewNoteData: TNoteData read GetNewNoteData;
	end;

var
	NewNoteForm: TNewNoteForm;

implementation

{$R *.lfm}

destructor TNewNoteForm.Destroy;
begin
	FNoteData.Free;
	inherited;
end;

procedure TNewNoteForm.KeyPress(Sender: TObject; var Key: char);
begin
	if ord(Key) = 13 then
		self.ModalResult := mrOk;
end;

function TNewNoteForm.GetNewNoteData(): TNoteData;
begin
	FNoteData.Free;
	FNoteData := TNoteData.Create;
	FNoteData.Content := ContentEdit.Lines.Text;

	result := FNoteData;
end;

end.

