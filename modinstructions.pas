unit modInstructions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, FileUtil, RichMemo, Forms, Controls,
  Graphics, Dialogs, Menus, ExtCtrls;

type

  { TfrmInstructions }

  TfrmInstructions = class(TForm)
    mnuMain: TMainMenu;
    mnuClose: TMenuItem;
    rtmInstructions: TRichMemo;
    procedure mnuCloseClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmInstructions: TfrmInstructions;

implementation

{$R *.lfm}

{ TfrmInstructions }

procedure TfrmInstructions.mnuCloseClick(Sender: TObject);
begin
  Close;
end;

end.
