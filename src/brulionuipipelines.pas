unit BrulionUiPipelines;

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils, Classes, Forms, Controls, Dialogs,
	BrulionPipelines, BrulionState, BrulionContainer, BrulionTypes,
	NewBoard, NewLane, NewNote, EditNote, ConfirmDialog;

type

	TUIPipeline = class(TPipeline)
	private
		FForm: TCustomForm;
	public
		property Form: TCustomForm read FForm write FForm;
	end;

	TConfirmPipeline = class(TUIPipeline)
	private
		FConfirmText: String;
		FSavedSender: TObject;
	private
		procedure Response(Sender: TObject; ModalResult: TModalResult);
	public
		procedure Start(Sender: TObject); override;
		property ConfirmText: String read FConfirmText write FConfirmText;
	end;

	TBoardModalPipeline = class(TUIPipeline)
	private
		procedure Response(Sender: TObject; ModalResult: TModalResult);
	public
		procedure Start(Sender: TObject); override;
	end;

	TLaneModalPipeline = class(TUIPipeline)
	private
		FBoardId: TUlid;
	private
		procedure Response(Sender: TObject; ModalResult: TModalResult);
	public
		procedure Start(Sender: TObject); override;
	public
		property BoardId: TUlid read FBoardId write FBoardId;
	end;

	TNoteModalPipeline = class(TUIPipeline)
	private
		FLaneId: TUlid;
	private
		procedure Response(Sender: TObject; ModalResult: TModalResult);
	public
		procedure Start(Sender: TObject); override;
	public
		property LaneId: TUlid read FLaneId write FLaneId;
	end;

	TNoteEditModalArg = (nemaEdit, nemaDelete);
	TNoteEditModalPipeline = class(TUIPipeline)
	private
		FNoteData: TNoteData;
	private
		procedure Response(Sender: TObject; ModalResult: TModalResult);
	public
		procedure Start(Sender: TObject); override;
	public
		property NoteData: TNoteData read FNoteData write FNoteData;
	end;

implementation

procedure TConfirmPipeline.Response(Sender: TObject; ModalResult: TModalResult);
begin
	if ModalResult = mrYes then
		self.Finish(FSavedSender)
	else
		self.Fail(FSavedSender);

	self.Form.RemoveComponent(TComponent(Sender));
	Sender.Free;
end;

procedure TConfirmPipeline.Start(Sender: TObject);
var
	LDialog: TConfirmDialogForm;
begin
	inherited;
	FSavedSender := Sender;
	LDialog := TConfirmDialogForm.Create(self.Form);
	LDialog.DialogText := self.ConfirmText;
	LDialog.ShowModal(@self.Response);
end;

procedure TBoardModalPipeline.Response(Sender: TObject; ModalResult: TModalResult);
var
	LModal: TNewBoardForm;
begin
	LModal := TNewBoardForm(Sender);
	if ModalResult = mrOk then
		self.Finish(LModal.NewBoardData)
	else
		self.Fail(Sender);

	self.Form.RemoveComponent(LModal);
	LModal.Free;
end;

procedure TBoardModalPipeline.Start(Sender: TObject);
begin
	inherited;
	TNewBoardForm.Create(self.Form).ShowModal(@self.Response);
end;

procedure TLaneModalPipeline.Response(Sender: TObject; ModalResult: TModalResult);
var
	LModal: TNewLaneForm;
	LData: TLaneData;
begin
	LModal := TNewLaneForm(Sender);
	if ModalResult = mrOk then begin
		LData := LModal.NewLaneData;
		LData.BoardId := self.BoardId;
		self.Finish(LData);
	end
	else
		self.Fail(Sender);

	self.Form.RemoveComponent(LModal);
	LModal.Free;
end;

procedure TLaneModalPipeline.Start(Sender: TObject);
begin
	inherited;
	TNewLaneForm.Create(self.Form).ShowModal(@self.Response);
end;

procedure TNoteModalPipeline.Response(Sender: TObject; ModalResult: TModalResult);
var
	LModal: TNewNoteForm;
	LData: TNoteData;
begin
	LModal := TNewNoteForm(Sender);
	if ModalResult = mrOk then begin
		LData := LModal.NoteData;
		LData.LaneId := self.LaneId;
		self.Finish(LData);
	end
	else
		self.Fail(Sender);

	self.Form.RemoveComponent(LModal);
	LModal.Free;
end;

procedure TNoteModalPipeline.Start(Sender: TObject);
var
	LForm: TNewNoteForm;
begin
	inherited;

	LForm := TNewNoteForm.Create(self.Form);
	LForm.ShowModal(@self.Response);
end;

procedure TNoteEditModalPipeline.Response(Sender: TObject; ModalResult: TModalResult);
var
	LModal: TEditNoteForm;
begin
	LModal := TEditNoteForm(Sender);
	case ModalResult of
		mrOk: self.Finish(TSenderWithArg.Create(LModal.NoteData, Ord(nemaEdit)));
		mrAbort: self.Finish(TSenderWithArg.Create(LModal.NoteData, Ord(nemaDelete)));
		else self.Fail(Sender);
	end;

	self.Form.RemoveComponent(LModal);
	LModal.Free;
end;

procedure TNoteEditModalPipeline.Start(Sender: TObject);
var
	LForm: TEditNoteForm;
begin
	inherited;

	LForm := TEditNoteForm.Create(self.Form);
	LForm.NoteData := self.NoteData;
	LForm.ShowModal(@self.Response);
end;

end.

