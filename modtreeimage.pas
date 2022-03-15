unit modTreeImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TfrmTreeImage }

  TfrmTreeImage = class(TForm)
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmTreeImage: TfrmTreeImage;

implementation

{$R *.lfm}

{ TfrmTreeImage }

procedure TfrmTreeImage.FormActivate(Sender: TObject);
begin

end;

procedure TfrmTreeImage.FormCreate(Sender: TObject);
begin
  Canvas.Brush.Color := clWhite;
  Canvas.Brush.Style := bsSolid;
  Canvas.Refresh;
  Canvas.MoveTo(50, 50);
  Canvas.LineTo(100, 50);
end;

procedure TfrmTreeImage.FormPaint(Sender: TObject);
begin
  Canvas.MoveTo(50, 50);
  Canvas.LineTo(100, 50);
end;

end.

