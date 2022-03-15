unit modAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, modTestData, modDataTypes;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnOK: TButton;
    txtTotalUsed: TEdit;
    lblTotalUsed: TLabel;
    txtTotalFree: TEdit;
    lblTotalFree: TLabel;
    txtTotalSpace: TEdit;
    lblTotalSpace: TLabel;
    txtHeapError: TEdit;
    lblHeapError: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { private declarations }
    intTotalFree,
    intHeapError: integer;
    hsAbout: THeapStatus;
  public
    { public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.FormActivate(Sender: TObject);
begin
  intHeapError := intShowHeapStatus(hsAbout);
  intTotalFree := hsAbout.TotalFree;
  txtHeapError.Text := IntToStr(intHeapError);
  txtTotalFree.Text := IntToStr(intTotalFree);
  txtTotalSpace.Text := IntToStr(hsAbout.TotalAddrSpace);
  txtTotalUsed.Text := IntToStr(hsAbout.TotalAllocated);
end;

end.

