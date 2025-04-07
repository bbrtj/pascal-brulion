unit BrulionTypes;

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils, FPJson, FPJsonJS;

type
	TUlid = type String; // not working: [26];

	TBoardData = record
		Id: TUlid;
		Name: String;
	end;

	TBoardDataArray = Array of TBoardData;

implementation

end.

