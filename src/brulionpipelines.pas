unit BrulionPipelines;

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils, Classes, Generics.Collections,
	BrulionState, BrulionApiConnector, BrulionContainer, BrulionTypes;

type
	EPipeline = class(Exception);

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
		FOnFail: TNotifyEvent;
	protected
		procedure Finish(Sender: TObject); virtual;
		procedure Fail(Sender: TObject); virtual;
	public
		procedure Start(Sender: TObject); virtual;
		procedure SetNext(Target: TNotifyEvent);
		procedure SetNext(Target: TPipeline);
		procedure SetFail(Target: TNotifyEvent);
		procedure SetFail(Target: TPipeline);
	public
		property Status: TPipelineStatus read FStatus write FStatus;
	end;

	TPipelineClass = class of TPipeline;

	TKnownPipeline = record
		Pipeline: TPipeline;
		LastStatus: TPipelineStatus;
		VisitedCount: Integer;
	end;

	TPipelineList = specialize TList<TKnownPipeline>;
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

	TCreatePipelineProc = function(Arg: TObject): TPipeline of object;
	TForkPipeline = class(TPipeline)
	private
		FBuildPipelineProc: TCreatePipelineProc;
		FPipelinesCount: Integer;
	protected
		function BuildPipeline(Arg: TObject): TPipeline; virtual;
		procedure Finish(Sender: TObject); override;
	public
		procedure Start(Sender: TObject); override;
	public
		property BuildPipelineProc: TCreatePipelineProc read FBuildPipelineProc write FBuildPipelineProc;
	end;

	TSenderWithArg = class
	public
		Sender: TObject;
		Arg: Byte;
		constructor Create(Sender: TObject; Arg: Byte);
	end;

	TConditionPipeline = class(TPipeline)
	private
		FOnArg: Array of TNotifyEvent;
	public
		procedure Start(Sender: TObject); override;
		procedure SetNext(Target: TNotifyEvent; Arg: Byte);
		procedure SetNext(Target: TPipeline; Arg: Byte);
	end;

	TLoadSystemBoardsPipeline = class(TPipeline)
	protected
		procedure Finish(Sender: TObject); override;
	public
		procedure Start(Sender: TObject); override;
	end;

	{ Board pipelines }

	TLoadBoardPipeline = class(TPipeline)
	private
		FId: TUlid;
	protected
		procedure Finish(Sender: TObject); override;
	public
		procedure Start(Sender: TObject); override;
	public
		property Id: TUlid read FId write FId;
	end;

	TBoardPipeline = class abstract(TPipeline)
	private
		FData: TBoardData;
	public
		procedure Start(Sender: TObject); override;
	public
		property Data: TBoardData read FData write FData;
	end;

	TLoadBoardLanesPipeline = class(TBoardPipeline)
	protected
		procedure Finish(Sender: TObject); override;
	public
		procedure Start(Sender: TObject); override;
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

	{ Lane pipelines }

	TLoadLanePipeline = class(TPipeline)
	private
		FId: TUlid;
	protected
		procedure Finish(Sender: TObject); override;
	public
		procedure Start(Sender: TObject); override;
	public
		property Id: TUlid read FId write FId;
	end;

	TLanePipeline = class abstract(TPipeline)
	private
		FData: TLaneData;
	public
		procedure Start(Sender: TObject); override;
	public
		property Data: TLaneData read FData write FData;
	end;

	TCreateLanePipeline = class(TLanePipeline)
	protected
		procedure Finish(Sender: TObject); override;
	public
		procedure Start(Sender: TObject); override;
	end;

	TDeleteLanePipeline = class(TLanePipeline)
	protected
		procedure Finish(Sender: TObject); override;
	public
		procedure Start(Sender: TObject); override;
	end;

	TLoadLaneNotesPipeline = class(TLanePipeline)
	protected
		procedure Finish(Sender: TObject); override;
	public
		procedure Start(Sender: TObject); override;
	end;

	{ Note pipelines }

	TLoadNotePipeline = class(TPipeline)
	private
		FId: TUlid;
	protected
		procedure Finish(Sender: TObject); override;
	public
		procedure Start(Sender: TObject); override;
	public
		property Id: TUlid read FId write FId;
	end;

	TNotePipeline = class abstract(TPipeline)
	private
		FData: TNoteData;
	public
		procedure Start(Sender: TObject); override;
	public
		property Data: TNoteData read FData write FData;
	end;

	TCreateNotePipeline = class(TNotePipeline)
	protected
		procedure Finish(Sender: TObject); override;
	public
		procedure Start(Sender: TObject); override;
	end;

	TUpdateNotePipeline = class(TNotePipeline)
	public
		procedure Start(Sender: TObject); override;
	end;

	TChangeLanePipeline = class(TNotePipeline)
	private
		FNewLaneId: TUlid;
		FOldLaneId: TUlid;
		FUpdatePipeline: TUpdateNotePipeline;
	protected
		procedure Finish(Sender: TObject); override;
		procedure Fail(Sender: TObject); override;
	public
		procedure Start(Sender: TObject); override;
	public
		property NewLaneId: TUlid read FNewLaneId write FNewLaneId;
	end;

	TDeleteNotePipeline = class(TNotePipeline)
	protected
		procedure Finish(Sender: TObject); override;
	public
		procedure Start(Sender: TObject); override;
	end;

	TMoveNotePipeline = class(TNotePipeline)
	private
		FSource: TNoteData;
	protected
		procedure Finish(Sender: TObject); override;
	public
		procedure Start(Sender: TObject); override;
	public
		property Source: TNoteData read FSource write FSource;
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
	if FOnFail <> nil then
		FOnFail(Sender);
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

procedure TPipeline.SetFail(Target: TNotifyEvent);
begin
	FOnFail := Target;
end;

procedure TPipeline.SetFail(Target: TPipeline);
begin
	FOnFail := @Target.Start;
end;

constructor TPipelineManager.Create();
begin
	FPipelines := TPipelineList.Create;
end;

destructor TPipelineManager.Destroy();
var
	I: Integer;
begin
	if FPipelines <> nil then begin
		for I := 0 to FPipelines.Count - 1 do
			FPipelines[I].Pipeline.Free;

		FPipelines.Free;
	end;

	inherited;
end;

function TPipelineManager.New(Typ: TPipelineClass): TPipeline;
var
	LRec: TKnownPipeline;
begin
	result := Typ.Create;

	LRec.Pipeline := result;
	LRec.VisitedCount := 0;
	LRec.LastStatus := result.Status;
	FPipelines.Add(LRec);
end;

{ Performs a periodic cleanup }
procedure TPipelineManager.Cleanup();
const
	CLostThreshold = 1;
	CStuckThreshold = 5;
var
	I: Integer;
	LRemove: Boolean;
	LRec: TKnownPipeline;
begin
	for I := FPipelines.Count - 1 downto 0 do begin
		LRec := FPipelines[I];
		LRemove :=
			(LRec.Pipeline.Status = psFailed)
			or (LRec.Pipeline.Status = psFinished)
			or (
				(LRec.Pipeline.Status = psNew)
				and (LRec.VisitedCount >= CLostThreshold)
			)
			or (
				(LRec.Pipeline.Status = psStarted)
				and (LRec.LastStatus = psStarted)
				and (LRec.VisitedCount >= CStuckThreshold)
			);

		if LRemove then begin
			{$IFDEF DEBUG}
			writeln(
				format(
					'Removing a pipeline of type %s, status %d, seen %d times',
					[
						LRec.Pipeline.ClassName,
						Ord(LRec.Pipeline.Status),
						LRec.VisitedCount
					]
				)
			);
			{$ENDIF}
			LRec.Pipeline.Free;
			FPipelines.Delete(I);
		end
		else begin
			Inc(LRec.VisitedCount);
			LRec.LastStatus := LRec.Pipeline.Status;
			FPipelines[I] := LRec;
		end;
	end;
end;

function TForkPipeline.BuildPipeline(Arg: TObject): TPipeline;
begin
	result := FBuildPipelineProc(Arg);
	if result <> nil then
		Inc(FPipelinesCount);
end;

procedure TForkPipeline.Finish(Sender: TObject);
begin
	Dec(FPipelinesCount);

	if FPipelinesCount <= 0 then
		inherited Finish(self);
end;

procedure TForkPipeline.Start(Sender: TObject);
var
	LPipeline: TPipeline;
begin
	inherited;
	FPipelinesCount := 0;

	LPipeline := self.BuildPipeline(Sender);
	while LPipeline <> nil do begin
		LPipeline.SetNext(@self.Finish);
		LPipeline.Start(nil);
		LPipeline := self.BuildPipeline(Sender);
	end;

	// if there is nothing to fork, make sure to just push the pipeline further
	if FPipelinesCount = 0 then
		self.Finish(nil);
end;


procedure TLoadSystemBoardsPipeline.Finish(Sender: TObject);
var
	LResponse: TBoardsApiDataList;
begin
	LResponse := Sender as TBoardsApiDataList;
	// TODO: handle pagination
	TBrulionState(GContainer.Services[csState]).Boards.Init(LResponse.Value);
	inherited;
end;

procedure TLoadSystemBoardsPipeline.Start(Sender: TObject);
begin
	inherited;
	GContainer.BoardsApi.LoadBoards(@self.Finish);
end;

procedure TLoadBoardPipeline.Finish(Sender: TObject);
var
	LData: TBoardData;
begin
	LData := TBoardsApiData(Sender).Snatch;
	TBrulionState(GContainer.Services[csState]).Boards.Add([LData]);
	inherited Finish(LData);
end;

procedure TLoadBoardPipeline.Start(Sender: TObject);
begin
	inherited;
	if (self.Id = CEmptyUlid) and (Sender <> nil) and (Sender is TGeneralSuccessData) then
		self.Id := TGeneralSuccessData(Sender).Id;

	GContainer.BoardsApi.LoadBoard(@self.Finish, self.Id);
end;

procedure TBoardPipeline.Start(Sender: TObject);
begin
	inherited;
	if (self.Data = nil) and (Sender <> nil) and (Sender is TBoardData) then
		self.Data := Sender as TBoardData;
end;

procedure TLoadBoardLanesPipeline.Finish(Sender: TObject);
var
	LResponse: TLanesApiDataList;
begin
	LResponse := Sender as TLanesApiDataList;
	// TODO: handle pagination
	TBrulionState(GContainer.Services[csState]).Lanes.Init(LResponse.Value);
	inherited;
end;

procedure TLoadBoardLanesPipeline.Start(Sender: TObject);
begin
	inherited;
	GContainer.LanesApi.LoadLanes(@self.Finish, self.Data.Id);
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

	inherited;
end;

procedure TDeleteBoardPipeline.Start(Sender: TObject);
begin
	inherited;
	GContainer.BoardsApi.DeleteBoard(@self.Finish, self.Data.Id);
end;

procedure TLoadLanePipeline.Finish(Sender: TObject);
var
	LData: TLaneData;
begin
	LData := TLanesApiData(Sender).Snatch;
	TBrulionState(GContainer.Services[csState]).Lanes.Add([LData]);
	inherited Finish(LData);
end;

procedure TLoadLanePipeline.Start(Sender: TObject);
begin
	inherited;
	if (self.Id = CEmptyUlid) and (Sender <> nil) and (Sender is TGeneralSuccessData) then
		self.Id := TGeneralSuccessData(Sender).Id;

	GContainer.LanesApi.LoadLane(@self.Finish, self.Id);
end;

procedure TLanePipeline.Start(Sender: TObject);
begin
	inherited;
	if (self.Data = nil) and (Sender <> nil) and (Sender is TLaneData) then
		self.Data := Sender as TLaneData;
end;

procedure TCreateLanePipeline.Finish(Sender: TObject);
begin
	// no need to transfer API success, just the actual data
	inherited Finish(TGeneralSuccessApiData(Sender).Value);
end;

procedure TCreateLanePipeline.Start(Sender: TObject);
begin
	inherited;
	GContainer.LanesApi.CreateLane(@self.Finish, self.Data);
end;

procedure TDeleteLanePipeline.Finish(Sender: TObject);
begin
	TBrulionState(GContainer.Services[csState]).Lanes.Remove(self.Data);
	inherited;
end;

procedure TDeleteLanePipeline.Start(Sender: TObject);
begin
	inherited;
	GContainer.LanesApi.DeleteLane(@self.Finish, self.Data.Id);
end;

procedure TLoadLaneNotesPipeline.Finish(Sender: TObject);
var
	LResponse: TNotesApiDataList;
begin
	LResponse := Sender as TNotesApiDataList;
	// TODO: handle pagination
	TBrulionState(GContainer.Services[csState]).Notes[self.Data.Id].Init(LResponse.Value);
	inherited;
end;

procedure TLoadLaneNotesPipeline.Start(Sender: TObject);
begin
	inherited;
	GContainer.NotesApi.LoadNotes(@self.Finish, self.Data.Id);
end;

procedure TLoadNotePipeline.Finish(Sender: TObject);
var
	LData: TNoteData;
begin
	LData := TNotesApiData(Sender).Snatch;
	TBrulionState(GContainer.Services[csState]).Notes[LData.LaneId].Add([LData]);
	inherited Finish(LData);
end;

procedure TLoadNotePipeline.Start(Sender: TObject);
begin
	inherited;
	if (self.Id = CEmptyUlid) and (Sender <> nil) and (Sender is TGeneralSuccessData) then
		self.Id := TGeneralSuccessData(Sender).Id;

	GContainer.NotesApi.LoadNote(@self.Finish, self.Id);
end;

procedure TNotePipeline.Start(Sender: TObject);
begin
	inherited;
	if (self.Data = nil) and (Sender <> nil) and (Sender is TNoteData) then
		self.Data := Sender as TNoteData;
end;

procedure TCreateNotePipeline.Finish(Sender: TObject);
begin
	// no need to transfer API success, just the actual data
	inherited Finish(TGeneralSuccessApiData(Sender).Value);
end;

procedure TCreateNotePipeline.Start(Sender: TObject);
begin
	inherited;
	GContainer.NotesApi.CreateNote(@self.Finish, self.Data);
end;

procedure TUpdateNotePipeline.Start(Sender: TObject);
begin
	inherited;
	GContainer.NotesApi.UpdateNote(@self.Finish, self.Data);
end;

procedure TChangeLanePipeline.Finish(Sender: TObject);
begin
	TBrulionState(GContainer.Services[csState])
		.Notes[FOldLaneId].Remove(self.Data);
	TBrulionState(GContainer.Services[csState])
		.Notes[FNewLaneId].Add([self.Data]);

	FUpdatePipeline.Free;
	inherited;
end;

procedure TChangeLanePipeline.Fail(Sender: TObject);
begin
	self.Data.LaneId := FOldLaneId;
	FUpdatePipeline.Free;
	inherited;
end;

procedure TChangeLanePipeline.Start(Sender: TObject);
begin
	inherited;

	FOldLaneId := self.Data.LaneId;
	self.Data.LaneId := FNewLaneId;

	FUpdatePipeline := TUpdateNotePipeline.Create;
	FUpdatePipeline.Data := self.Data;
	FUpdatePipeline.SetNext(@self.Finish);
	FUpdatePipeline.SetFail(@self.Fail);
	FUpdatePipeline.Start(nil);
end;

procedure TDeleteNotePipeline.Finish(Sender: TObject);
begin
	TBrulionState(GContainer.Services[csState]).Notes[self.Data.LaneId].Remove(self.Data);
	inherited;
end;

procedure TDeleteNotePipeline.Start(Sender: TObject);
begin
	inherited;
	GContainer.NotesApi.DeleteNote(@self.Finish, self.Data.Id);
end;

procedure TMoveNotePipeline.Finish(Sender: TObject);
var
	LIndex: Integer;
begin
	TBrulionState(GContainer.Services[csState]).Notes[self.Data.LaneId].Remove(self.Source);
	LIndex := TBrulionState(GContainer.Services[csState]).Notes[self.Data.LaneId].GetIndex(self.Data);
	TBrulionState(GContainer.Services[csState]).Notes[self.Data.LaneId].Insert(LIndex + 1, [self.Source]);
	inherited;
end;

procedure TMoveNotePipeline.Start(Sender: TObject);
begin
	inherited;
	GContainer.NotesApi.MoveNote(@self.Finish, self.Source.Id, self.Data.Id);
end;

constructor TSenderWithArg.Create(Sender: TObject; Arg: Byte);
begin
	self.Sender := Sender;
	self.Arg := Arg;
end;

procedure TConditionPipeline.Start(Sender: TObject);
var
	Arg: Byte;
	RealSender: TObject;
begin
	inherited;

	try
		if not(Sender is TSenderWithArg) then
			raise EPipeline.Create('Condition pipeline called without a proper argument');

		Arg := TSenderWithArg(Sender).Arg;
		RealSender := TSenderWithArg(Sender).Sender;
		Sender.Free;

		if Arg > High(FOnArg) then
			raise EPipeline.Create('Condition pipeline has no handler for argument ' + IntToStr(Arg));

		FOnArg[Arg](RealSender);
		self.Finish(RealSender);
	except
		on E: EPipeline do self.Fail(E);
	end;
end;

procedure TConditionPipeline.SetNext(Target: TNotifyEvent; Arg: Byte);
begin
	if Arg > High(FOnArg) then
		SetLength(FOnArg, Arg + 1);
	FOnArg[Arg] := Target;
end;

procedure TConditionPipeline.SetNext(Target: TPipeline; Arg: Byte);
begin
	if Arg > High(FOnArg) then
		SetLength(FOnArg, Arg + 1);
	FOnArg[Arg] := @Target.Start;
end;

end.

