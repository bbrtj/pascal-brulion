unit BrulionApiConnector;

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils, StrUtils, Classes, JS, Web, FPJson, FPJsonJS, Generics.Collections, Math,
	BrulionTypes;

const
	// see project options for this define
	{$macro on} ApiUrl = BrulionApiUrl; {$macro off}

type
	EBrulionApi = class(Exception);
	EBrulionApiConnection = class(EBrulionApi);
	EBrulionSerializer = class(EBrulionApi);

	TSerializationStage = (ssFull, ssInsert, ssUpdate);

	TBrulionApiDataBase = class abstract
	public
		function Pack(): String; virtual; abstract;
		procedure Unpack(Value: TJsonData); virtual; abstract;
		procedure Unpack(const Data: String); virtual;
	end;

	generic TBrulionApiDataSingle<T: TBrulionData> = class(TBrulionApiDataBase)
	private
		FValue: T;
	public
		destructor Destroy; override;
	public
		function Pack(): String; override;
		procedure Unpack(Value: TJsonData); override;
		function Snatch(): T;
	public
		property Value: T read FValue write FValue;
	end;

	generic TBrulionApiDataList<T: TBrulionData> = class(TBrulionApiDataBase)
	private type
		TThisType = specialize TBrulionApiDataList<T>;
		TArray = Array of T;
	private
		FValue: TArray;
		FBookmark: TUlid;
	public
		function Pack(): String; override;
		procedure Unpack(Value: TJsonData); override;
		procedure Merge(Other: TThisType);
		function HasBookmark(): Boolean;
	public
		property HasMoreData: Boolean read HasBookmark;
		property Bookmark: TUlid read FBookmark;
		property Value: TArray read FValue write FValue;
	end;

	TAjax = class
	private
		FXmlHttpRequest: TJSXMLHttpRequest;
		FBaseUrl: String;
		FDataResult: TBrulionApiDataBase;
		FOnLoad: TNotifyEvent;
		FFinished: Boolean;
	private
		function Loaded(Response: TEventListenerEvent): Boolean;
		function Failed(Response: TEventListenerEvent): Boolean;
		procedure InternalPost(const Action, Url: String; Data: TJsonData);
	public
		constructor Create(const BaseUrl: String);
		destructor Destroy; override;
	public
		procedure Get(const Url: String);
		procedure Delete(const Url: String);
		procedure Post(const Url: String; Data: TJsonData);
		procedure Put(const Url: String; Data: TJsonData);
	public
		property OnLoad: TNotifyEvent write FOnLoad;
		property DataResult: TBrulionApiDataBase write FDataResult;
		property Finished: Boolean read FFinished;
	end;
	TAjaxList = specialize TObjectList<TAjax>;

	TGeneralSuccessApiData = specialize TBrulionApiDataSingle<TGeneralSuccessData>;
	TGeneralErrorApiData = specialize TBrulionApiDataSingle<TGeneralErrorData>;
	TGeneralEmptyApiData = class(TBrulionApiDataBase)
	public
		procedure Unpack(const Data: String); override;
	end;

	TApi = class abstract
	private
		FBaseUrl: String;
		FApiCalls: TAjaxList;
	protected
		function GetAjax(Event: TNotifyEvent; DataResult: TBrulionApiDataBase): TAjax;
	public
		constructor Create(const BaseUrl: String);
		destructor Destroy; override;
	public
		property BaseUrl: String read FBaseUrl;
	end;

	TBoardsApiData = specialize TBrulionApiDataSingle<TBoardData>;
	TBoardsApiDataList = specialize TBrulionApiDataList<TBoardData>;
	TBoardsApi = class(TApi, IBoardsApi)
	public
		constructor Create();
		procedure LoadBoard(Event: TNotifyEvent; const Id: TUlid);
		procedure DeleteBoard(Event: TNotifyEvent; const Id: TUlid);
		procedure LoadBoards(Event: TNotifyEvent; const Args: TBrulionListArgs);
		procedure CreateBoard(Event: TNotifyEvent; const Board: TBoardData);
	end;

	TLanesApiData = specialize TBrulionApiDataSingle<TLaneData>;
	TLanesApiDataList = specialize TBrulionApiDataList<TLaneData>;
	TLanesApi = class(TApi, ILanesApi)
	public
		constructor Create();
		procedure LoadLane(Event: TNotifyEvent; const Id: TUlid);
		procedure DeleteLane(Event: TNotifyEvent; const Id: TUlid);
		procedure LoadLanes(Event: TNotifyEvent; const BoardId: TUlid; const Args: TBrulionListArgs);
		procedure CreateLane(Event: TNotifyEvent; const Lane: TLaneData);
	end;

	TNotesApiData = specialize TBrulionApiDataSingle<TNoteData>;
	TNotesApiDataList = specialize TBrulionApiDataList<TNoteData>;
	TNotesApi = class(TApi, INotesApi)
	public
		constructor Create();
		procedure LoadNote(Event: TNotifyEvent; const Id: TUlid);
		procedure DeleteNote(Event: TNotifyEvent; const Id: TUlid);
		procedure LoadNotes(Event: TNotifyEvent; const LaneId: TUlid; const Args: TBrulionListArgs);
		procedure CreateNote(Event: TNotifyEvent; const Note: TNoteData);
		procedure UpdateNote(Event: TNotifyEvent; const Note: TNoteData);
		procedure MoveNote(Event: TNotifyEvent; const Id, After: TUlid);
	end;

function DefaultListArgs(): TBrulionListArgs;
function JoinUrl(const Base, Url: String): String;

function Serialize(const Args: TBrulionListArgs): String;

function Serialize(const Value: TGeneralSuccessData; Stage: TSerializationStage = ssFull): TJsonData;
function Serialize(const Value: TGeneralErrorData; Stage: TSerializationStage = ssFull): TJsonData;
function Serialize(const Value: TBoardData; Stage: TSerializationStage = ssFull): TJsonData;
function Serialize(const Value: TLaneData; Stage: TSerializationStage = ssFull): TJsonData;
function Serialize(const Value: TNoteData; Stage: TSerializationStage = ssFull): TJsonData;

procedure DeSerialize(Value: TJsonData; Obj: TGeneralSuccessData);
procedure DeSerialize(Value: TJsonData; Obj: TGeneralErrorData);
procedure DeSerialize(Value: TJsonData; Obj: TBoardData);
procedure DeSerialize(Value: TJsonData; Obj: TLaneData);
procedure DeSerialize(Value: TJsonData; Obj: TNoteData);

implementation

procedure TBrulionApiDataBase.Unpack(const Data: String);
var
	LJsonData: TJsonData;
begin
	LJsonData := nil;

	try
		LJsonData := GetJSON(Data);
		self.Unpack(LJsonData);
	finally
		LJsonData.Free;
	end;
end;

destructor TBrulionApiDataSingle.Destroy();
begin
	FValue.Free;
end;

function TBrulionApiDataSingle.Pack(): String;
var
	LSerialized: TJsonData;
begin
	LSerialized := Serialize(FValue);
	result := LSerialized.AsJson;
	LSerialized.Free;
end;

procedure TBrulionApiDataSingle.Unpack(Value: TJsonData);
begin
	try
		FValue := T.Create;
		DeSerialize(Value, FValue);
	except
		on E: Exception do begin
			FreeAndNil(FValue);
			raise E;
		end;
	end;
end;

function TBrulionApiDataSingle.Snatch(): T;
begin
	result := FValue;
	FValue := nil;
end;

function TBrulionApiDataList.Pack(): String;
var
	LSerialized: TJsonArray;
	I: Integer;
begin
	for I := 0 to High(self.Value) do begin
		LSerialized.Add(Serialize(self.Value[I]));
	end;

	result := LSerialized.AsJson;
	LSerialized.Free;
end;

procedure TBrulionApiDataList.Unpack(Value: TJsonData);
var
	LData: TJsonArray;
	I: Integer;
begin
	if not(
		(Value is TJsonObject)
		and (TJsonObject(Value).Elements['data'] is TJsonArray)
	) then raise EBrulionSerializer.Create('invalid board list data');

	if TJsonObject(Value).Elements['bookmark'] is TJsonString then
		FBookmark := TJsonObject(Value).Elements['bookmark'].AsString;

	LData := TJsonObject(Value).Arrays['data'];

	SetLength(self.Value, LData.Count);
	for I := 0 to High(self.Value) do
		self.Value[I] := T.Create;

	try
		for I := 0 to High(self.Value) do
			DeSerialize(LData[I], self.Value[I]);
	except
		on E: Exception do begin
			for I := 0 to High(self.Value) do
				FreeAndNil(self.Value[I]);

			raise E;
		end;
	end;
end;

procedure TBrulionApiDataList.Merge(Other: TThisType);
begin
	self.Value := Concat(Other.Value, self.Value);
end;

function TBrulionApiDataList.HasBookmark(): Boolean;
begin
	result := length(FBookmark) > 0;
end;

function TAjax.Loaded(Response: TEventListenerEvent): Boolean;
var
	LError: String;
begin
	FFinished := true;

	if not InRange(FXmlHttpRequest.Status, 200, 299) then begin
		FDataResult.Free;
		FDataResult := TGeneralErrorApiData.Create;
		try
			FDataResult.Unpack(FXmlHttpRequest.ResponseText);
			LError := TGeneralErrorApiData(FDataResult).Value.Error;
		except
			on EJson do LError := FXmlHttpRequest.ResponseText;
		end;

		raise EBrulionApi.Create(LError);
	end
	else begin
		FDataResult.Unpack(FXmlHttpRequest.ResponseText);
		FOnLoad(FDataResult);
		result := true;
	end;
end;

function TAjax.Failed(Response: TEventListenerEvent): Boolean;
begin
	FFinished := true;
	raise EBrulionApiConnection.Create('Network communication has failed');
end;

procedure TAjax.InternalPost(const Action, Url: String; Data: TJsonData);
begin
	try
		FXmlHttpRequest.Open(Action, JoinUrl(FBaseUrl, Url), true);
		FXmlHttpRequest.SetRequestHeader('Content-Type', 'application/json');
		FXmlHttpRequest.Send(Data.AsJson);
	finally
		Data.Free;
	end;
end;

constructor TAjax.Create(const BaseUrl: String);
begin
	FXmlHttpRequest := TJSXMLHttpRequest.new;
	FXmlHttpRequest.AddEventListener('load', @self.Loaded);
	FXmlHttpRequest.AddEventListener('error', @self.Failed);
	FBaseUrl := BaseUrl;
end;

destructor TAjax.Destroy;
begin
	FDataResult.Free;
	// no need to free FXmlHttpRequest, it's a JS object
	// FXmlHttpRequest.Free;
	inherited;
end;

procedure TAjax.Get(const Url: String);
begin
	FXmlHttpRequest.Open('GET', JoinUrl(FBaseUrl, Url), true);
	FXmlHttpRequest.Send;
end;

procedure TAjax.Delete(const Url: String);
begin
	FXmlHttpRequest.Open('DELETE', JoinUrl(FBaseUrl, Url), true);
	FXmlHttpRequest.Send;
end;

procedure TAjax.Post(const Url: String; Data: TJsonData);
begin
	self.InternalPost('POST', Url, Data);
end;

procedure TAjax.Put(const Url: String; Data: TJsonData);
begin
	self.InternalPost('PUT', Url, Data);
end;

procedure TGeneralEmptyApiData.Unpack(const Data: String);
begin
	if Data <> '' then
		raise EBrulionSerializer.Create('invalid api empty data');
end;

function TApi.GetAjax(Event: TNotifyEvent; DataResult: TBrulionApiDataBase): TAjax;
const
	CClearThreshold = 20;
var
	I: Integer;
begin
	result := TAjax.Create(JoinUrl(ApiUrl, self.BaseUrl));
	result.DataResult := DataResult;
	result.OnLoad := Event;

	if FApiCalls.Count > CClearThreshold then begin
		for I := FApiCalls.Count - 1 downto 0 do begin
			if FApiCalls[I].Finished then
				FApiCalls.Remove(FApiCalls[I]);
		end;
	end;

	FApiCalls.Add(result);
end;

constructor TApi.Create(const BaseUrl: String);
begin
	FBaseUrl := BaseUrl;
	FApiCalls := TAjaxList.Create;
end;

destructor TApi.Destroy();
begin
	FApiCalls.Free;
end;

constructor TBoardsApi.Create();
const
	CBaseUrl = '/boards';
begin
	inherited Create(CBaseUrl);
end;

procedure TBoardsApi.LoadBoard(Event: TNotifyEvent; const Id: TUlid);
const
	CUrl = '';
begin
	self.GetAjax(Event, TBoardsApiData.Create).Get(JoinUrl(CUrl, Id));
end;

procedure TBoardsApi.DeleteBoard(Event: TNotifyEvent; const Id: TUlid);
const
	CUrl = '';
begin
	self.GetAjax(Event, TGeneralEmptyApiData.Create).Delete(JoinUrl(CUrl, Id));
end;

procedure TBoardsApi.LoadBoards(Event: TNotifyEvent; const Args: TBrulionListArgs);
const
	CUrl = '';
begin
	self.GetAjax(Event, TBoardsApiDataList.Create).Get(CUrl + Serialize(Args));
end;

procedure TBoardsApi.CreateBoard(Event: TNotifyEvent; const Board: TBoardData);
const
	CUrl = '';
begin
	self.GetAjax(Event, TGeneralSuccessApiData.Create).Post(CUrl, Serialize(Board, ssInsert));
end;

constructor TLanesApi.Create();
const
	CBaseUrl = '/lanes';
begin
	inherited Create(CBaseUrl);
end;

procedure TLanesApi.LoadLane(Event: TNotifyEvent; const Id: TUlid);
const
	CUrl = '';
begin
	self.GetAjax(Event, TLanesApiData.Create).Get(JoinUrl(CUrl, Id));
end;

procedure TLanesApi.DeleteLane(Event: TNotifyEvent; const Id: TUlid);
const
	CUrl = '';
begin
	self.GetAjax(Event, TGeneralEmptyApiData.Create).Delete(JoinUrl(CUrl, Id));
end;

procedure TLanesApi.LoadLanes(Event: TNotifyEvent; const BoardId: TUlid; const Args: TBrulionListArgs);
const
	CUrl = '/board';
begin
	self.GetAjax(Event, TLanesApiDataList.Create).Get(JoinUrl(CUrl, BoardId) + Serialize(Args));
end;

procedure TLanesApi.CreateLane(Event: TNotifyEvent; const Lane: TLaneData);
const
	CUrl = '';
begin
	self.GetAjax(Event, TGeneralSuccessApiData.Create).Post(CUrl, Serialize(Lane, ssInsert));
end;

constructor TNotesApi.Create();
const
	CBaseUrl = '/notes';
begin
	inherited Create(CBaseUrl);
end;

procedure TNotesApi.LoadNote(Event: TNotifyEvent; const Id: TUlid);
const
	CUrl = '';
begin
	self.GetAjax(Event, TNotesApiData.Create).Get(JoinUrl(CUrl, Id));
end;

procedure TNotesApi.DeleteNote(Event: TNotifyEvent; const Id: TUlid);
const
	CUrl = '';
begin
	self.GetAjax(Event, TGeneralEmptyApiData.Create).Delete(JoinUrl(CUrl, Id));
end;

procedure TNotesApi.LoadNotes(Event: TNotifyEvent; const LaneId: TUlid; const Args: TBrulionListArgs);
const
	CUrl = '/lane';
begin
	self.GetAjax(Event, TNotesApiDataList.Create).Get(JoinUrl(CUrl, LaneId) + Serialize(Args));
end;

procedure TNotesApi.CreateNote(Event: TNotifyEvent; const Note: TNoteData);
const
	CUrl = '';
begin
	self.GetAjax(Event, TGeneralSuccessApiData.Create).Post(CUrl, Serialize(Note, ssInsert));
end;

procedure TNotesApi.UpdateNote(Event: TNotifyEvent; const Note: TNoteData);
const
	CUrl = '';
begin
	self.GetAjax(Event, TGeneralEmptyApiData.Create).Put(JoinUrl(CUrl, Note.Id), Serialize(Note, ssUpdate));
end;

procedure TNotesApi.MoveNote(Event: TNotifyEvent; const Id, After: TUlid);
const
	CUrl = '/move';
var
	LJson: TJsonObject;
begin
	LJson := TJsonObject.Create;
	LJson.Add('after', After);

	self.GetAjax(Event, TGeneralEmptyApiData.Create).Put(JoinUrl(CUrl, Id), LJson);
end;

function DefaultListArgs(): TBrulionListArgs;
begin
	result.Bookmark := '';
	result.Count := 0;
	result.SortField := '';
	result.SortAsc := False;
end;

function JoinUrl(const Base, Url: String): String;
const
	CUrlSeparator = '/';

	function ClearFront(const From: String): String;
	begin
		result := From;
		if (length(result) > 0) and (result[1] = CUrlSeparator) then
			Delete(result, 1, 1);
	end;
	function ClearBack(const From: String): String;
	begin
		result := From;
		if (length(result) > 0) and (result[length(result)] = CUrlSeparator) then
			Delete(result, length(result), 1);
	end;
begin
	result := ClearBack(Base) + CUrlSeparator + ClearFront(Url);
end;

function Serialize(const Args: TBrulionListArgs): String;
var
	LResultLabels: Array [0 .. 3] of String;
	LResultParts: Array [0 .. 3] of String;
	I: Integer;
begin
	LResultLabels[0] := 'bookmark';
	LResultParts[0] := Args.Bookmark;
	LResultLabels[1] := 'count';
	LResultParts[1] := IfThen(Args.Count > 0, IntToStr(Args.Count), '');
	LResultLabels[2] := 'sort_field';
	LResultParts[2] := Args.SortField;
	LResultLabels[3] := 'sort_order';
	LResultParts[3] := IfThen(Args.SortAsc, 'asc', '');

	for I := low(LResultParts) to high(LResultParts) do begin
		if length(LResultParts[I]) > 0 then
			result += '&' + encodeURIComponent(LResultLabels[I]) + '=' + encodeURIComponent(LResultParts[I]);
	end;

	if length(result) > 0 then
		result[1] := '?';
end;

function Serialize(const Value: TGeneralSuccessData; Stage: TSerializationStage): TJsonData;
begin
	result := TJsonObject.Create;
	TJsonObject(result).Add('id', Value.Id);
end;

function Serialize(const Value: TGeneralErrorData; Stage: TSerializationStage): TJsonData;
begin
	result := TJsonObject.Create;
	TJsonObject(result).Add('error', Value.Error);
end;

function Serialize(const Value: TBoardData; Stage: TSerializationStage): TJsonData;
begin
	result := TJsonObject.Create;
	if Stage = ssFull then
		TJsonObject(result).Add('id', Value.Id);

	TJsonObject(result).Add('name', Value.Name);
end;

function Serialize(const Value: TLaneData; Stage: TSerializationStage): TJsonData;
begin
	result := TJsonObject.Create;
	if Stage = ssFull then
		TJsonObject(result).Add('id', Value.Id);
	TJsonObject(result).Add('board_id', Value.BoardId);
	TJsonObject(result).Add('name', Value.Name);
end;

function Serialize(const Value: TNoteData; Stage: TSerializationStage): TJsonData;
begin
	result := TJsonObject.Create;
	if Stage = ssFull then
		TJsonObject(result).Add('id', Value.Id);
	TJsonObject(result).Add('lane_id', Value.LaneId);
	TJsonObject(result).Add('content', Value.Content);
end;

procedure DeSerialize(Value: TJsonData; Obj: TGeneralSuccessData);
begin
	if not(
		(Value is TJsonObject)
		and (TJsonObject(Value).Elements['id'] is TJsonString)
	) then raise EBrulionSerializer.Create('invalid api data');

	Obj.Id := TJsonObject(Value).Strings['id'];
end;

procedure DeSerialize(Value: TJsonData; Obj: TGeneralErrorData);
begin
	if not(
		(Value is TJsonObject)
		and (TJsonObject(Value).Elements['error'] is TJsonString)
	) then raise EBrulionSerializer.Create('invalid api error data');

	Obj.Error := TJsonObject(Value).Strings['error'];
end;

procedure DeSerialize(Value: TJsonData; Obj: TBoardData);
begin
	if not(
		(Value is TJsonObject)
		and (TJsonObject(Value).Elements['id'] is TJsonString)
		and (TJsonObject(Value).Elements['name'] is TJsonString)
	) then raise EBrulionSerializer.Create('invalid api board data');

	Obj.Id := TJsonObject(Value).Strings['id'];
	Obj.Name := TJsonObject(Value).Strings['name'];
end;

procedure DeSerialize(Value: TJsonData; Obj: TLaneData);
begin
	if not(
		(Value is TJsonObject)
		and (TJsonObject(Value).Elements['id'] is TJsonString)
		and (TJsonObject(Value).Elements['board_id'] is TJsonString)
		and (TJsonObject(Value).Elements['name'] is TJsonString)
	) then raise EBrulionSerializer.Create('invalid api lane data');

	Obj.Id := TJsonObject(Value).Strings['id'];
	Obj.BoardId := TJsonObject(Value).Strings['board_id'];
	Obj.Name := TJsonObject(Value).Strings['name'];
end;

procedure DeSerialize(Value: TJsonData; Obj: TNoteData);
begin
	if not(
		(Value is TJsonObject)
		and (TJsonObject(Value).Elements['id'] is TJsonString)
		and (TJsonObject(Value).Elements['lane_id'] is TJsonString)
		and (TJsonObject(Value).Elements['content'] is TJsonString)
	) then raise EBrulionSerializer.Create('invalid api note data');

	Obj.Id := TJsonObject(Value).Strings['id'];
	Obj.LaneId := TJsonObject(Value).Strings['lane_id'];
	Obj.Content := TJsonObject(Value).Strings['content'];
end;

end.

