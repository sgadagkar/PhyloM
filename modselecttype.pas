// Module to select measurement or binary data
// Binary - or + is default
unit modSelectType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, modDataTypes, modTestData;

type

  { TdlgSelectFileType }

  TdlgSelectFileType = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    grpbxSlectFileType: TGroupBox;
    rdgrpSequence: TRadioGroup;
    rdbtnMeasurement: TRadioButton;
    rdbtnBinary: TRadioButton;
    rdbtnSequence: TRadioButton;
    rdgrpBinary: TRadioGroup;
    function Execute(var wrdRadio: word; var strErrorMsg: string): boolean;
    procedure FormCreate(Sender: TObject);
    procedure rdbtnBinaryClick(Sender: TObject);
    procedure rdbtnMeasurementClick(Sender: TObject);
    procedure rdbtnSequenceClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  dlgSelectFileType: TdlgSelectFileType;

implementation

{$R *.lfm}

{ TdlgSelectFileType }

uses
  modPhyIoM;

function TdlgSelectFileType.Execute(var wrdRadio: word; var strErrorMsg: string): boolean;
begin
  wrdRadio := wrdNoType;
  if ShowModal = mrOK then
  begin
//    wrdRadio := rdgrpFileType.ItemIndex;
    if rdbtnMeasurement.Checked then
      wrdRadio := wrdMeasurement
    else
      if rdbtnBinary.Checked then
        if rdgrpBinary.ItemIndex <> -1 then
          wrdRadio := rdgrpBinary.ItemIndex
        else
        begin
          strErrorMsg := 'Type of Binary not selected.';
          Result := False;
          Exit;
        end
      else
        if rdbtnSequence.Checked then
          if rdgrpSequence.ItemIndex <> -1 then
            wrdRadio := rdgrpSequence.ItemIndex + wrdSequence
          else
          begin
            strErrorMsg := 'Type of Sequence not selected.';
            Result := False;
            Exit;
          end
        else
        begin
          Result := False;
          Exit;
        end;
    Result := True
  end
  else
    Result := False;
end;

procedure TdlgSelectFileType.FormCreate(Sender: TObject);
begin
  rdbtnSequence.Visible:= frmPhyIoM.prpRunMode;
  if not frmPhyIoM.prpRunMode then
  begin
    btnCancel.Top := 144;
    btnCancel.BringToFront;
    btnOK.Top := 144;
    btnOK.BringToFront;
    Height  := 208;
  end;
end;

procedure TdlgSelectFileType.rdbtnBinaryClick(Sender: TObject);
begin
  if rdbtnBinary.Checked then
  begin
    rdgrpBinary.Visible := True;
    rdgrpSequence.Visible := False;
    rdgrpSequence.ItemIndex := -1;
  end;
end;

procedure TdlgSelectFileType.rdbtnMeasurementClick(Sender: TObject);
begin
  if rdbtnMeasurement.Checked then
  begin
//    rdbtnMeasurement.Checked := True;
    rdgrpBinary.Visible := False;
    rdgrpSequence.Visible := False;
    rdgrpBinary.ItemIndex := -1;
    rdgrpSequence.ItemIndex := -1;
  end;
end;

procedure TdlgSelectFileType.rdbtnSequenceClick(Sender: TObject);
begin
  if rdbtnSequence.Checked then
  begin
//    rdbtnSequence.Checked := True;
    rdgrpSequence.Visible := True;
    rdgrpBinary.Visible := False;
    rdgrpBinary.ItemIndex := -1;
  end;
end;

end.

