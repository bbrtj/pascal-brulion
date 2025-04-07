unit BrulionTypes;

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils, FPJson, FPJsonJS;

type
	TULID = type String; // not working: [26];

	TBrulionData = class abstract
	public
		function Pack(): String; virtual; abstract;
		procedure Unpack(Value: TJsonData); virtual; abstract;
		procedure Unpack(Data: String);
	end;
	TBrulionDataClass = class of TBrulionData;
	EBrulionData = class(Exception);

	generic TBrulionDataList<T: TBrulionData> = class(TBrulionData)
	public type
		TBrulionDataArray = array of T;
	private
		FBookmark: String;
	public
		List: TBrulionDataArray;
	public
		function Pack(): String; override;
		procedure Unpack(Value: TJsonData); override;
		function HasBookmark(): Boolean;
	public
		property HasMoreData: Boolean read HasBookmark;
		property Bookmark: String read FBookmark;
	end;

	TBoardData = class(TBrulionData)
	public
		Id: TULID;
		Name: String;
	public
		function Pack(): String; override;
		procedure Unpack(Value: TJsonData); override;
	end;

	TBoardDataList = specialize TBrulionDataList<TBoardData>;

implementation

procedure TBrulionData.Unpack(Data: String);
begin
	self.Unpack(GetJSON(Data));
end;

function TBrulionDataList.Pack(): String;
begin
end;

procedure TBrulionDataList.Unpack(Value: TJsonData);
var
	LData: TJsonArray;
	I: Integer;
begin
	if not(
		(Value is TJsonObject)
		and (TJsonObject(Value).Elements['data'] is TJsonArray)
	) then raise EBrulionData.Create('invalid board list data');

	if TJsonObject(Value).Elements['bookmark'] is TJsonString then
		FBookmark := TJsonObject(Value).Elements['bookmark'].AsString;

	LData := TJsonObject(Value).Arrays['data'];
	SetLength(self.List, LData.Count);

	for I := 0 to LData.Count - 1 do begin
		self.List[I] := T.Create;
		self.List[I].Unpack(LData.Items[I]);
	end;
end;

function TBrulionDataList.HasBookmark(): Boolean;
begin
	result := length(FBookmark) > 0;
end;

function TBoardData.Pack(): String;
begin
end;

procedure TBoardData.Unpack(Value: TJsonData);
begin
	if not(
		(Value is TJsonObject)
		and (TJsonObject(Value).Elements['id'] is TJsonString)
		and (TJsonObject(Value).Elements['name'] is TJsonString)
	) then raise EBrulionData.Create('invalid board data');

	self.Id := TJsonObject(Value).Strings['id'];
	self.Name := TJsonObject(Value).Strings['name'];
end;


end.

