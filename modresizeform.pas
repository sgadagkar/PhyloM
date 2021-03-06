unit modresizeform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TfrmResizeForm }

  TfrmResizeForm = class(TForm)
    btnExit: TButton;
    btnRange: TButton;
    Shape1: TShape;
    txtFormWidth: TEdit;
    lblWForm: TLabel;
    txtShapeWidth: TEdit;
    lblWShape: TLabel;
    txtVertRange: TEdit;
    lblVRange: TLabel;
    txtFormHeight: TEdit;
    lblHForm: TLabel;
    txtShapeHight: TEdit;
    lblHShape: TLabel;
    shpResize: TShape;
    procedure btnExitClick(Sender: TObject);
    procedure btnRangeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmResizeForm: TfrmResizeForm;

implementation

{$R *.lfm}

{ TfrmResizeForm }

procedure TfrmResizeForm.btnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmResizeForm.btnRangeClick(Sender: TObject);
begin
  frmResizeForm.VertScrollBar.Range := 880;
  frmResizeForm.HorzScrollBar.Range := 880;
  shpResize.Height := 800;
  shpResize.Width := 800;
  txtShapeHight.Text := IntToStr(shpResize.Height);
  txtFormHeight.Text := IntToStr(frmResizeForm.Height);
  txtShapeWidth.Text := IntToStr(shpResize.Width);
  txtFormWidth.Text := IntToStr(frmResizeForm.Width);
  txtVertRange.Text := IntToStr(frmResizeForm.VertScrollBar.Range);
end;

procedure TfrmResizeForm.FormCreate(Sender: TObject);
begin
  txtShapeHight.Text := IntToStr(shpResize.Height);
  txtFormHeight.Text := IntToStr(frmResizeForm.Height);
  txtShapeWidth.Text := IntToStr(shpResize.Width);
  txtFormWidth.Text := IntToStr(frmResizeForm.Width);
  txtVertRange.Text := IntToStr(frmResizeForm.VertScrollBar.Range);
end;

end.

