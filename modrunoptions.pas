unit modRunOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, modDataTypes, modTestData;

type

  { TdlgRunOptions }

  TdlgRunOptions = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    chkbxBootStraps: TCheckBox;
    edbxTests: TEdit;
    lblTests: TLabel;
    procedure chkbxBootStrapsClick(Sender: TObject);
    function Execute(var intTests: integer): boolean;
  private
    { private declarations }
  public
    { public declarations }
  end;

const
  kReplicates = 100;

var
  dlgRunOptions: TdlgRunOptions;

implementation

{$R *.lfm}

function TdlgRunOptions.Execute(var intTests: integer): boolean;
begin
  if intTests = 0 then
    edbxTests.Text := IntToStr(kReplicates)
  else
    edbxTests.Text := IntToStr(intTests);
  if ShowModal = mrOK then
  begin
    TestTableForSpeciesData.boolBootStrap := chkbxBootStraps.Checked;
    if TestTableForSpeciesData.boolBootStrap then
      intTests := StrToInt(edbxTests.Text)
    else
      intTests := 0;
    Result := True;
  end
  else
    Result := False;
end;

procedure TdlgRunOptions.chkbxBootStrapsClick(Sender: TObject);
begin
  if chkbxBootStraps.Checked then
  begin
    lblTests.Visible := True;
    edbxTests.Visible := True;
    lblTests.Enabled := True;
    edbxTests.Enabled := True;
  end
  else
  begin
    lblTests.Visible := False;
    edbxTests.Visible := False;
    lblTests.Enabled := False;
    edbxTests.Enabled := False;
  end;
end;

end.

