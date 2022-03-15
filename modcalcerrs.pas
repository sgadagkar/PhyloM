unit modCalcErrs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, modDataTypes, modTestData;

type

  { TfrmCalculatedErrors }

  TfrmCalculatedErrors = class(TForm)
    btnClose: TButton;
    btnFileSave: TButton;
    lblCalculatedErrors: TLabel;
    mmoErrorList: TMemo;
    svdlgErrorTextFile: TSaveDialog;
    procedure btnClearErrorsClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnFileSaveClick(Sender: TObject);
    procedure cbxErrorTypeChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmCalculatedErrors: TfrmCalculatedErrors;

implementation

uses
  modPhyIoM;

{$R *.lfm}

{ TfrmCalculatedErrors }

procedure TfrmCalculatedErrors.FormActivate(Sender: TObject);
begin
//  mmoErrorList.Clear;
//  case cbxErrorType.ItemIndex of
//    0:
  mmoErrorList.Lines.Assign(strlstCalcErrs);
//    1: mmoErrorList.Lines.Assign(strlstArrayErrs);
//    2: mmoErrorList.Lines.Assign(stlsRecTree);
//  end;
end;

procedure TfrmCalculatedErrors.btnClearErrorsClick(Sender: TObject);
begin
  if mrYes = MessageDlg('Clear Display', 'Do you want to clear current calcualtion errors?', mtConfirmation, [mbYes,mbNo], 0) then
  begin
    strlstCalcErrs.Clear;
//    strlstArrayErrs.Clear;
//    stlsRecTree.Clear;
    mmoErrorList.Clear;
  end;
end;

procedure TfrmCalculatedErrors.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmCalculatedErrors.btnFileSaveClick(Sender: TObject);
begin
  svdlgErrorTextFile.InitialDir := strSaveCurrentDir;
  if svdlgErrorTextFile.Execute then
  begin
    strSaveFileName := svdlgErrorTextFile.FileName;
    if FileExists(strSaveFileName) then
      if mrYes = MessageDlg('File: ' + strSaveFileName + ' already exists do you want to replace it?', mtConfirmation, [mbYes, mbNo], 0) then
        try
          mmoErrorList.Lines.SaveToFile(strSaveFileName)
        except
          on E:exception do MessageDlg(E.Message + ' for file: ' + strSaveFileName, mtWarning, [mbOK], 0);
        end
      else
        MessageDlg('Original file: ' + strSaveFileName + ' unchanged.', mtWarning, [mbOK], 0)
    else
      mmoErrorList.Lines.SaveToFile(strSaveFileName);
  end
  else
    MessageDlg('Request to save file: ' + strSaveFileName + ' cancelled.', mtWarning, [mbOK], 0);
end;

procedure TfrmCalculatedErrors.cbxErrorTypeChange(Sender: TObject);
begin
  FormActivate(Sender);
end;

end.

