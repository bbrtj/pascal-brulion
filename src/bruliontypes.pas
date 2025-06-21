unit BrulionTypes;

{$mode objfpc}{$H+}{$J-}
{$interfaces corba}

interface

uses SysUtils, Classes, FPJson, FPJsonJS;

const
	CEmptyUlid = '';

type
	TUlid = type String{$IFNDEF PAS2JS}[26]{$ENDIF};

	TBrulionData = class
	end;
	TBrulionDataClass = class of TBrulionData;

	TGeneralSuccessData = class(TBrulionData)
		Id: TUlid;
	end;

	TGeneralErrorData = class(TBrulionData)
		Error: String;
	end;

	TBoardData = class(TBrulionData)
		Id: TUlid;
		Name: String;
	end;
	TBoardDataArray = Array of TBoardData;

	TLaneData = class(TBrulionData)
		Id: TUlid;
		BoardId: TUlid;
		Name: String;
	end;
	TLaneDataArray = Array of TLaneData;

	TNoteData = class(TBrulionData)
		Id: TUlid;
		LaneId: TUlid;
		Content: String;
	end;
	TNoteDataArray = Array of TNoteData;

	IStorage = interface
	['{4923fe8c-3623-428e-a4f7-a90e80b48402}']
		function GetItem(const Name: String): String;
		procedure SetItem(const Name, Value: String);
		function HasItem(const Name: String): Boolean;
		property Items[I: String]: String read GetItem write SetItem;
	end;

	IBoardsApi = interface
	['{2f90c07b-b363-49eb-9cc7-a97a94de3f6d}']
		procedure LoadBoard(Event: TNotifyEvent; const Id: TUlid);
		procedure DeleteBoard(Event: TNotifyEvent; const Id: TUlid);
		procedure LoadBoards(Event: TNotifyEvent);
		procedure CreateBoard(Event: TNotifyEvent; const Board: TBoardData);
	end;

	ILanesApi = interface
	['{99ea450d-2085-42a6-8c3a-70ca9bd331bb}']
		procedure LoadLane(Event: TNotifyEvent; const Id: TUlid);
		procedure DeleteLane(Event: TNotifyEvent; const Id: TUlid);
		procedure LoadLanes(Event: TNotifyEvent; const BoardId: TUlid);
		procedure CreateLane(Event: TNotifyEvent; const Lane: TLaneData);
	end;

implementation

end.

