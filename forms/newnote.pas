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
		function GetNoteData: TNoteData;
		procedure SetNoteData(AValue: TNoteData);
	public
		destructor Destroy; override;
	public
		property NoteData: TNoteData read GetNoteData write SetNoteData;
	end;

implementation

{$R *.lfm}

destructor TNewNoteForm.Destroy;
begin
	FNoteData.Free;
	inherited;
end;

procedure TNewNoteForm.KeyPress(Sender: TObject; var Key: char);
begin
end;

function TNewNoteForm.GetNoteData(): TNoteData;
begin
	if FNoteData = nil then
		FNoteData := TNoteData.Create;

	FNoteData.Content := ContentEdit.Lines.Text;
	result := FNoteData;
end;

procedure TNewNoteForm.SetNoteData(AValue: TNoteData);
begin
	FNoteData := AValue;
	ContentEdit.Lines.Text := FNoteData.Content;
end;

end.

