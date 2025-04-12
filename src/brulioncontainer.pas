unit BrulionContainer;

{$mode objfpc}{$H+}{$J-}
{$interfaces corba}

interface

uses SysUtils, Classes, Web;

type
	TStorage = class abstract
	protected
		function GetItem(const Name: String): String; virtual; abstract;
		procedure SetItem(const Name, Value: String); virtual; abstract;
	public
		property Items[I: String]: String read GetItem write SetItem;
	end;

	TLocalStorage = class(TStorage)
	protected
		function GetItem(const Name: String): String; override;
		procedure SetItem(const Name, Value: String); override;
	end;

	TContainer = class(TPersistent)
	private
		FStorage: TStorage;
	public
		destructor Destroy; override;
	public
		procedure Assign(Other: TContainer);
	public
		property Storage: TStorage read FStorage write FStorage;
	end;

var
	GDefaultContainer: TContainer;

function GetContainer(Container: TContainer = nil): TContainer;

implementation

function TLocalStorage.GetItem(const Name: String): String;
begin
	result := window.localStorage.getItem(Name);
end;

procedure TLocalStorage.SetItem(const Name, Value: String);
begin
	window.localStorage.setItem(Name, Value);
end;

destructor TContainer.Destroy();
begin
	FStorage.Free;
	inherited;
end;

procedure TContainer.Assign(Other: TContainer);
begin
	FStorage := Other.Storage;
end;

function GetContainer(Container: TContainer): TContainer;
begin
	result := TContainer.Create;
	if Container = nil then
		result.Assign(GDefaultContainer)
	else
		result.Assign(Container);
end;

initialization
	GDefaultContainer := TContainer.Create;
	GDefaultContainer.Storage := TLocalStorage.Create;
// Not yet supported:
// finalization
// 	GDefaultContainer.Free;
end.

