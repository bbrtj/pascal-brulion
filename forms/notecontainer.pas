unit NoteContainer;

{$mode ObjFPC}{$H+}

interface

uses
	JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls,
	BrulionContainer, BrulionTypes, UniqName;

type

	{ TNoteFrame }

	TNoteFrame = class(TWFrame)
		NoteContent: TWLabel;
	private
		FNote: TNoteData;
		procedure SetNote(AValue: TNoteData);
		procedure SetParent(AValue: TWinControl);
	public
		constructor Create(AOwner: TComponent); override;
	public
		property Parent write SetParent;
		property Note: TNoteData read FNote write SetNote;

	end;

implementation

uses LanesContainer;

{$R *.lfm}

procedure TNoteFrame.SetNote(AValue: TNoteData);
begin
	FNote := AValue;
	self.NoteContent.Caption := FNote.Content;
end;

procedure TNoteFrame.SetParent(AValue: TWinControl);
begin
	inherited Parent := AValue;
end;

constructor TNoteFrame.Create(AOwner: TComponent);
begin
	inherited;
	SetUniqName(self);
end;

end.

