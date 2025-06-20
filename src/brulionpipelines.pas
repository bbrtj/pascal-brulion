unit BrulionPipelines;

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils, Classes, Generics.Collections,
	BrulionState, BrulionApiConnector, BrulionContainer, BrulionTypes;

type
	TPipelineStatus = (
		psNew,
		psStarted,
		psFailed,
		psFinished
	);

	TPipeline = class abstract
	private
		FStatus: TPipelineStatus;
		FOnFinish: TNotifyEvent;
	protected
		procedure Finish(Sender: TObject); virtual;
		procedure Fail(Sender: TObject); virtual;
	public
		procedure Start(Sender: TObject); virtual;
		procedure SetNext(Target: TNotifyEvent);
		procedure SetNext(Target: TPipeline);
	public
		property Status: TPipelineStatus read FStatus write FStatus;
	end;

	TPipelineClass = class of TPipeline;

	TPipelineList = specialize TObjectList<TPipeline>;
	TPipelineManager = class
	private
		FPipelines: TPipelineList;
	public
		constructor Create();
		destructor Destroy; override;
	public
		function New(Typ: TPipelineClass): TPipeline;
		procedure Cleanup();
	end;

	{ Board pipelines }

	TBoardPipeline = class(TPipeline)
	private
		FData: TBoardData;
	public
		procedure Start(Sender: TObject); override;
	public
		property Data: TBoardData read FData write FData;
	end;

	TLoadBoardPipeline = class(TBoardPipeline)
	private
		FId: TUlid;
	protected
		procedure Finish(Sender: TObject); override;
	public
		procedure Start(Sender: TObject); override;
	public
		property Id: TUlid read FId write FId;
	end;

	TCreateBoardPipeline = class(TBoardPipeline)
	protected
		procedure Finish(Sender: TObject); override;
	public
		procedure Start(Sender: TObject); override;
	end;

	TDeleteBoardPipeline = class(TBoardPipeline)
	protected
		procedure Finish(Sender: TObject); override;
	public
		procedure Start(Sender: TObject); override;
	end;

implementation

procedure TPipeline.Finish(Sender: TObject);
begin
	if FOnFinish <> nil then
		FOnFinish(Sender);
	FStatus := psFinished;
end;

procedure TPipeline.Fail(Sender: TObject);
begin
	FStatus := psFailed;
end;

procedure TPipeline.Start(Sender: TObject);
begin
	FStatus := psStarted;
end;

procedure TPipeline.SetNext(Target: TNotifyEvent);
begin
	FOnFinish := Target;
end;

procedure TPipeline.SetNext(Target: TPipeline);
begin
	FOnFinish := @Target.Start;
end;

constructor TPipelineManager.Create();
begin
	FPipelines := TPipelineList.Create;
end;

destructor TPipelineManager.Destroy();
begin
	FPipelines.Free;
end;

function TPipelineManager.New(Typ: TPipelineClass): TPipeline;
begin
	result := Typ.Create;
	FPipelines.Add(result);
end;

{ Performs a periodic cleanup }
procedure TPipelineManager.Cleanup();
var
	I: Integer;
begin
	for I := FPipeLines.Count - 1 downto 0 do begin
		if (FPipelines[I].Status = psFailed) or (FPipelines[I].Status = psFinished) then
			FPipelines.Delete(I);
	end;
end;

procedure TBoardPipeline.Start(Sender: TObject);
begin
	inherited;
	if (self.Data = nil) and (Sender <> nil) and (Sender is TBoardData) then
		self.Data := Sender as TBoardData;
end;

procedure TLoadBoardPipeline.Finish(Sender: TObject);
begin
	self.Data := TBoardsApiData(Sender).Snatch;
	TBrulionState(GContainer.Services[csState]).Boards.Add([self.Data]);
	inherited Finish(self.Data);
end;

procedure TLoadBoardPipeline.Start(Sender: TObject);
begin
	inherited;
	if (self.Id = CEmptyUlid) and (Sender <> nil) and (Sender is TGeneralSuccessData) then
		self.Id := TGeneralSuccessData(Sender).Id;

	GContainer.BoardsApi.LoadBoard(@self.Finish, self.Id);
end;

procedure TCreateBoardPipeline.Finish(Sender: TObject);
begin
	// no need to transfer API success, just the actual data
	inherited Finish(TGeneralSuccessApiData(Sender).Value);
end;

procedure TCreateBoardPipeline.Start(Sender: TObject);
begin
	inherited;
	GContainer.BoardsApi.CreateBoard(@self.Finish, self.Data);
end;

procedure TDeleteBoardPipeline.Start(Sender: TObject);
begin
	inherited;
	GContainer.BoardsApi.DeleteBoard(@self.Finish, self.Data.Id);
end;

procedure TDeleteBoardPipeline.Finish(Sender: TObject);
var
	LState: TBrulionState;
	LCurrent: Boolean;
begin
	LState := GContainer.Services[csState] as TBrulionState;
	LCurrent := LState.Boards.Current = self.Data;
	LState.Boards.Remove(self.Data);

	if LCurrent and (LState.Boards.Count > 0) then
		LState.Boards.Current := LState.Boards.Items[0];

	inherited Finish(Sender);
end;

end.

