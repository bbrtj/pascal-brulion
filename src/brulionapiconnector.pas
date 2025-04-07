unit BrulionApiConnector;

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils, Classes, Web, FPJson, FPJsonJS,
	BrulionTypes;

const
	// see project options for this define
	{$macro on} ApiUrl = BrulionApiUrl; {$macro off}

type
	EBrulionApi = class(Exception);
	EBrulionSerializer = class(EBrulionApi);

	TBrulionApiDataBase = class
	public
		function Pack(): String; virtual; abstract;
		procedure Unpack(Value: TJsonData); virtual; abstract;
		procedure Unpack(Data: String);
	end;

	generic TBrulionApiDataSingle<T> = class(TBrulionApiDataBase)
	private
		FValue: T;
	public
		function Pack(): String; override;
		procedure Unpack(Value: TJsonData); override;
	public
		property Value: T read FValue write FValue;
	end;

	generic TBrulionApiDataList<T> = class(TBrulionApiDataBase)
	private type
		TArray = Array of T;
	private
		FValue: TArray;
		FBookmark: TUlid;
	public
		function Pack(): String; override;
		procedure Unpack(Value: TJsonData); override;
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
	private
		function Loaded(Response: TEventListenerEvent): Boolean;
	public
		constructor Create(const BaseUrl: String);
		destructor Destroy; override;
	public
		procedure Get(const Url: string);
	public
		property OnLoad: TNotifyEvent write FOnLoad;
		property DataResult: TBrulionApiDataBase write FDataResult;
	end;

	TApi = class abstract
	private
		FBaseUrl: String;
	protected
		function GetAjax(Event: TNotifyEvent; DataResult: TBrulionApiDataBase): TAjax;
	public
		constructor Create(const BaseUrl: String);
	public
		property BaseUrl: String read FBaseUrl;
	end;

	TBoardsApiDataList = specialize TBrulionApiDataList<TBoardData>;
	TBoardsApi = class(TApi)
	public
		constructor Create();
		procedure LoadBoards(Event: TNotifyEvent);
	end;

function Serialize(Value: TBoardData): TJsonData;
function Serialize(Value: TLaneData): TJsonData;

// Template is not used - it's a hack to force function overloading to work.
function DeSerialize(Value: TJsonData; const Template: TBoardData): TBoardData;
function DeSerialize(Value: TJsonData; const Template: TLaneData): TLaneData;

implementation

procedure TBrulionApiDataBase.Unpack(Data: String);
begin
	self.Unpack(GetJSON(Data));
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
	FValue := DeSerialize(Value, FValue);
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

	for I := 0 to LData.Count - 1 do begin
		self.Value[I] := DeSerialize(LData[I], self.Value[I]);
	end;
end;

function TBrulionApiDataList.HasBookmark(): Boolean;
begin
	result := length(FBookmark) > 0;
end;

function TAjax.Loaded(Response: TEventListenerEvent): Boolean;
begin
	FDataResult.Unpack(FXmlHttpRequest.ResponseText);
	FOnLoad(FDataResult);
	FDataResult.Free;
	result := true;
end;

constructor TAjax.Create(const BaseUrl: String);
begin
	FXmlHttpRequest := TJSXMLHttpRequest.new;
	FXmlHttpRequest.addEventListener('load', @self.Loaded);
	FBaseUrl := BaseUrl;
end;

destructor TAjax.Destroy;
begin
	// no need to free FXmlHttpRequest, it's a JS object
	// FXmlHttpRequest.Free;
	inherited;
end;

procedure TAjax.Get(const Url: String);
var
	LFullUrl: String;
begin
	LFullUrl := FBaseUrl;
	if (LFullUrl[length(LFullUrl)] <> '/') and (length(Url) > 0) then
		LFullUrl += '/';
	LFullUrl += Url;

	FXmlHttpRequest.open('GET', LFullUrl, true);
	FXmlHttpRequest.send;
end;

function TApi.GetAjax(Event: TNotifyEvent; DataResult: TBrulionApiDataBase): TAjax;
begin
	result := TAjax.Create(ApiUrl + self.BaseUrl);
	result.DataResult := DataResult;
	result.OnLoad := Event;
end;

constructor TApi.Create(const BaseUrl: String);
begin
	FBaseUrl := BaseUrl;
end;

constructor TBoardsApi.Create();
const
	CBaseUrl = '/boards';
begin
	inherited Create(CBaseUrl);
end;

procedure TBoardsApi.LoadBoards(Event: TNotifyEvent);
const
	CUrl = '';
begin
	self.GetAjax(Event, TBoardsApiDataList.Create).Get(CUrl);
end;

function Serialize(Value: TBoardData): TJsonData;
begin
	// TODO
end;

function Serialize(Value: TLaneData): TJsonData;
begin
	// TODO
end;

function DeSerialize(Value: TJsonData; const Template: TBoardData): TBoardData;
begin
	if not(
		(Value is TJsonObject)
		and (TJsonObject(Value).Elements['id'] is TJsonString)
		and (TJsonObject(Value).Elements['name'] is TJsonString)
	) then raise EBrulionSerializer.Create('invalid board data');

	result.Id := TJsonObject(Value).Strings['id'];
	result.Name := TJsonObject(Value).Strings['name'];
end;

function DeSerialize(Value: TJsonData; const Template: TLaneData): TLaneData;
begin
	if not(
		(Value is TJsonObject)
		and (TJsonObject(Value).Elements['id'] is TJsonString)
		and (TJsonObject(Value).Elements['name'] is TJsonString)
	) then raise EBrulionSerializer.Create('invalid lane data');

	result.Id := TJsonObject(Value).Strings['id'];
	result.Name := TJsonObject(Value).Strings['name'];
end;

end.

