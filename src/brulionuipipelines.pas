unit BrulionUiPipelines;

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils, Classes, Forms, Controls, Dialogs,
	BrulionPipelines, BrulionState, BrulionContainer, BrulionTypes,
	NewBoard, NewLane;

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

implementation

procedure TConfirmPipeline.Response(Sender: TObject; ModalResult: TModalResult);
begin
	if ModalResult = mrYes then
		self.Finish(Sender)
	else
		self.Fail(Sender);
end;

procedure TConfirmPipeline.Start(Sender: TObject);
begin
	inherited;
	MessageDlg(
		self.Form,
		FConfirmText,
		mtWarning,
		mbYesNo,
		mbNo,
		@self.Response
	);
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

end.

