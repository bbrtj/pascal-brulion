unit BrulionTypes;

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils, FPJson, FPJsonJS;

type
	TUlid = type String; // not working: [26];

	// for places which need a TObject
	generic TWrappedRecord<T> = class
	public
		Data: T;
	public
		constructor Create(AData: T);
	end;

	TBoardData = record
		Id: TUlid;
		Name: String;
	end;
	TWrappedBoardData = specialize TWrappedRecord<TBoardData>;

	TBoardDataArray = Array of TBoardData;

implementation

constructor TWrappedRecord.Create(AData: T);
begin
	self.Data := AData;
end;

end.

