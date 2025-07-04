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
	private
		FNoteData: TNoteData;
		function GetNoteData: TNoteData;
	public
		destructor Destroy; override;
	public
		property NoteData: TNoteData read GetNoteData;
	end;

implementation

{$R *.lfm}

destructor TNewNoteForm.Destroy;
begin
	FNoteData.Free;
	inherited;
end;

function TNewNoteForm.GetNoteData(): TNoteData;
begin
	if FNoteData = nil then
		FNoteData := TNoteData.Create;

	FNoteData.Content := ContentEdit.Lines.Text;
	result := FNoteData;
end;

end.

