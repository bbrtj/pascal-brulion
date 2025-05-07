package BrulionInterface;

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils,
	BrulionApiConnector, BrulionState;

type
	TBrulionInterface = class
	public
		constructor Create();
		destructor Destroy; override;
	public
		property State: TBrulionState read FState write FState;
		property Api: TBrulionApiContainer read FApi write FApi;
	end;

implementation

end.

