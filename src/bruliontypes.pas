unit BrulionTypes;

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils, FPJson, FPJsonJS;

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
		Name: String;
	end;
	TLaneDataArray = Array of TLaneData;

implementation

end.

