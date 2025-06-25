unit EditNote;

{$mode ObjFPC}{$H+}

interface

uses
	JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls,
	BrulionTypes;

type

	{ TEditNoteForm }

	TEditNoteForm = class(TWForm)
		CancelButton: TWButton;
		DeleteButton: TWButton;
		ConfirmButton: TWButton;
		ContentEdit: TWMemo;
		ContentLabel: TWLabel;
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

destructor TEditNoteForm.Destroy;
begin
	FNoteData.Free;
	inherited;
end;

function TEditNoteForm.GetNoteData(): TNoteData;
begin
	if FNoteData = nil then
		FNoteData := TNoteData.Create;

	FNoteData.Content := ContentEdit.Lines.Text;
	result := FNoteData;
end;

procedure TEditNoteForm.SetNoteData(AValue: TNoteData);
begin
	FNoteData := AValue;
	ContentEdit.Lines.Text := FNoteData.Content;
end;

end.

