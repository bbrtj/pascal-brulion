unit NoteContainer;

{$mode ObjFPC}{$H+}

interface

uses
	JS, Classes, SysUtils, Math, Graphics, Controls, Forms, Dialogs, WebCtrls,
	BrulionContainer, BrulionTypes, BrulionUiPipelines, BrulionPipelines,
	UniqName;

type

	{ TNoteFrame }

	TNoteFrame = class(TWFrame)
		NoteContent: TWLabel;
		WPanel1: TWPanel;
		procedure UpdateNote(Sender: TObject);
	private
		FNote: TNoteData;
		procedure NoteUpdated(Sender: TObject);
		procedure SetNote(AValue: TNoteData);
		procedure SetParent(AValue: TWinControl);
		function GetRealHeight(): Integer;
	public
		constructor Create(AOwner: TComponent); override;
	public
		procedure ReAlign(); override;
	public
		property Parent write SetParent;
		property Note: TNoteData read FNote write SetNote;
	end;

implementation

uses LanesContainer, MainWindow;

{$R *.lfm}

procedure TNoteFrame.UpdateNote(Sender: TObject);
var
	LModalPipeline: TNoteEditModalPipeline;
	LConditionPipeline: TConditionPipeline;
	LConfirmPipeline: TConfirmPipeline;
	LDeletePipeline: TDeleteNotePipeline;
	LUpdatePipeline: TUpdateNotePipeline;
begin
	LModalPipeline := TPipelineManager(GContainer[csPipelineManager])
		.New(TNoteEditModalPipeline) as TNoteEditModalPipeline;
	LConditionPipeline := TPipelineManager(GContainer[csPipelineManager])
		.New(TConditionPipeline) as TConditionPipeline;
	LConfirmPipeline := TPipelineManager(GContainer[csPipelineManager])
		.New(TConfirmPipeline) as TConfirmPipeline;
	LDeletePipeline := TPipelineManager(GContainer[csPipelineManager])
		.New(TDeleteNotePipeline) as TDeleteNotePipeline;
	LUpdatePipeline := TPipelineManager(GContainer[csPipelineManager])
		.New(TUpdateNotePipeline) as TUpdateNotePipeline;

	LModalPipeline.Form := TMainForm(self.Owner.Owner);
	LModalPipeline.NoteData := self.Note;
	LModalPipeline.SetNext(LConditionPipeline);

	LConditionPipeline.SetNext(LUpdatePipeline, Ord(nemaEdit));
	LConditionPipeline.SetNext(LConfirmPipeline, Ord(nemaDelete));

	LConfirmPipeline.Form := TMainForm(self.Owner.Owner);
	LConfirmPipeline.ConfirmText := 'Permanently delete this note?';
	LConfirmPipeline.SetNext(LDeletePipeline);

	LDeletePipeline.SetNext(@TLaneFrame(self.Owner).NoteDeleted);

	LUpdatePipeline.SetNext(@self.NoteUpdated);

	LModalPipeline.Start(nil);
end;

procedure TNoteFrame.NoteUpdated(Sender: TObject);
begin
	// force update form
	self.Note := self.Note;
end;

procedure TNoteFrame.SetNote(AValue: TNoteData);
begin
	FNote := AValue;
	self.NoteContent.Caption := FNote.Content;
	self.ReAlign;
end;

procedure TNoteFrame.SetParent(AValue: TWinControl);
begin
	inherited Parent := AValue;
end;

function TNoteFrame.GetRealHeight(): Integer;
const
	CMinHeight = 60;
begin
	result := NoteContent.ContentElement.ScrollHeight;
	result := Max(CMinHeight, result);
end;

constructor TNoteFrame.Create(AOwner: TComponent);
begin
	inherited;
	SetUniqName(self);
end;

procedure TNoteFrame.ReAlign();
begin
	inherited;
	if NoteContent = nil then exit;

	NoteContent.Height := GetRealHeight;
	WPanel1.Height := NoteContent.Height + NoteContent.BorderSpacing.Around * 2 + NoteContent.BorderSpacing.Top + NoteContent.BorderSpacing.Bottom;
	Height := WPanel1.Height + WPanel1.BorderSpacing.Around * 2 + WPanel1.BorderSpacing.Top + WPanel1.BorderSpacing.Bottom;
end;

end.

