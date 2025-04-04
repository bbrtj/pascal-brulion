unit MainForm;

interface

uses
  JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls, StdCtrls,
  Grids;

type

  { TWForm1 }

  { TMainForm }

  TMainForm = class(TWForm)
    WButton1: TWButton;
    WCheckbox1: TWCheckbox;
    WFileButton1: TWFileButton;
    WMemo1: TWMemo;
    WPanel1: TWPanel;
    procedure WButton1Click(Sender: TObject);
  private

  public
	constructor Create(AOwner: TComponent); override;
  end;

var
  GMainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.WButton1Click(Sender: TObject);
begin
  writeln(WMemo1.Lines.Text);
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

end.

