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

	TGeneralSuccessData = record
		Id: TUlid;
	end;
	TWrappedGeneralSuccessData = specialize TWrappedRecord<TGeneralSuccessData>;

	TGeneralErrorData = record
		Error: String;
	end;
	TWrappedGeneralErrorData = specialize TWrappedRecord<TGeneralErrorData>;

	TBoardData = record
		Id: TUlid;
		Name: String;
	end;
	TWrappedBoardData = specialize TWrappedRecord<TBoardData>;
	TBoardDataArray = Array of TBoardData;

	TLaneData = record
		Id: TUlid;
		Name: String;
	end;
	TWrappedLaneData = specialize TWrappedRecord<TLaneData>;
	TLaneDataArray = Array of TLaneData;

implementation

constructor TWrappedRecord.Create(AData: T);
begin
	self.Data := AData;
end;

end.

