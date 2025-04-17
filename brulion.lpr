program brulion;

uses
  Interfaces, Forms, MainWindow, NewBoard;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, GMainForm);
  Application.Run;
end.

