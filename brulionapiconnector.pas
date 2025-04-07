unit BrulionApiConnector;

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils, Classes, Web, FPJson, FPJsonJS,
	BrulionTypes;

const
	// see project options for this define
	{$MACRO ON} ApiUrl = BrulionApiUrl; {$MACRO OFF}

type
	EBrulionApi = class(Exception);

	TAjax = class
	private
		FXmlHttpRequest: TJSXMLHttpRequest;
		FBaseUrl: String;
		FDataResult: TBrulionData;
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
		property DataResult: TBrulionData write FDataResult;
	end;

	TApi = class abstract
	private
		FBaseUrl: String;
	protected
		function GetAjax(Event: TNotifyEvent; DataResult: TBrulionData): TAjax;
	public
		constructor Create(const BaseUrl: String);
	public
		property BaseUrl: String read FBaseUrl;
	end;

	TBoardsApi = class(TApi)
	public
		constructor Create();
		procedure LoadBoards(Event: TNotifyEvent);
	end;

implementation

function TAjax.Loaded(Response: TEventListenerEvent): Boolean;
begin
	FDataResult.Unpack(FXmlHttpRequest.ResponseText);
	FOnLoad(FDataResult);
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

function TApi.GetAjax(Event: TNotifyEvent; DataResult: TBrulionData): TAjax;
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
	self.GetAjax(Event, TBoardDataList.Create).Get(CUrl);
end;

end.

