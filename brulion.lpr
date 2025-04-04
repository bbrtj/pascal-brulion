program brulion;

uses
  Interfaces, Forms, MainForm;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, GMainForm);
  Application.Run;
  WriteLn('test');
end.
