unit modPrintTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, PrintersDlgs, Forms, Controls, Graphics, Dialogs, Printers,
  Menus, ExtCtrls, modDataTypes, modTestData;

type

  { TfrmPrintTree }

  TfrmPrintTree = class(TForm)
    imgPrintFrame: TImage;
    mnuPrint: TMenuItem;
    mnuClose: TMenuItem;
    mnuMain: TMainMenu;
    prtdlgTree: TPrintDialog;
    scrbxPrintImage: TScrollBox;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure imgPrintFrameClick(Sender: TObject);
    procedure mnuCloseClick(Sender: TObject);
    procedure mnuPrintClick(Sender: TObject);
  private
    { private declarations }
    strPrinterType: string;
    boolPrint: boolean;
    procedure ShowBracketTree(Sender: TObject);
    procedure PrintBracketTree(const boolPrintTree: boolean; Sender: TObject);
    procedure SetPrinterType(const strPrinter: string);
    procedure SetPrintOut(const boolPrntOt: boolean);
  public
    { public declarations }
  published
    property prpPrinterType: string read strPrinterType write SetPrinterType;
    property prpDoPrintOut: boolean read boolPrint write SetPrintOut;
  end;

var
  frmPrintTree: TfrmPrintTree;

implementation

Uses
  modTreeGraph;

var
  intHeight,
  intWidth,
  intCPLngth,
  intCPWdth: integer;

{$R *.lfm}

{ TfrmPrintTree }

procedure TfrmPrintTree.mnuCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmPrintTree.SetPrinterType(const strPrinter: string);
begin
  strPrinterType := strPrinter;
end;

procedure TfrmPrintTree.SetPrintOut(const boolPrntOt: boolean);
begin
  boolPrint := boolPrntOt;
end;

procedure TfrmPrintTree.FormCreate(Sender: TObject);
begin
  SetPrinterType('');
  SetPrintOut(False);
end;

procedure TfrmPrintTree.FormActivate(Sender: TObject);
var
  intLabelWidth: integer;
  Bitmap: TBitmap;
  wsSaved: TWindowState;
begin
  if strPrinterType = '' then
    if prtdlgTree.Execute then
    begin
      intHeight := Printer.PageHeight;
      intWidth := Printer.PageWidth;
      SetPrinterType('Selected');
      SetPrintOut(True);
    end
    else
      Close;
  Bitmap := TBitmap.Create;
  imgPrintFrame.Canvas.Font.Size := 40;
  try
    intLabelWidth := imgPrintFrame.Canvas.TextWidth(TestTableForSpeciesData.Species[intMaxLabelLengthPos].SpeciesName);
    case frmTreeGraph.prpGraphType of
      kRadialTree: begin
        intCPLngth := Origin.ttoRadial.Height * 5;
        intCPWdth := Origin.ttoRadial.Width * 5;
        Bitmap.Width := intCPWdth;
        Bitmap.Height := intCPLngth;
        imgPrintFrame.Picture.Graphic := Bitmap;
        imgPrintFrame.Width := intCPWdth;
        imgPrintFrame.Height := intCPLngth;
      end;
      kBracketTree: begin
        intCPLngth := Origin.ttoBracket.Height * 5;
        intCPWdth := Origin.ttoBracket.Width * 5; //intMaxBracketWidth(40) + intLabelWidth div 3;
        Bitmap.Width := intCPWdth; //Origin.ttoBracket.Width * 5;
        Bitmap.Height := intCPLngth;
        imgPrintFrame.Picture.Graphic := Bitmap;
        imgPrintFrame.Width := intCPWdth; //Origin.ttoBracket.Width * 5;
        imgPrintFrame.Height := intCPLngth;
      end;
      kTopologyOnlyTree: begin
        intCPLngth := Origin.ttoTplgyOnly.Height * 5;
        intCPWdth := Origin.ttoTplgyOnly.Width * 5;  //intMaxTopologyOnlyWidth(40) + intLabelWidth div 2;
        Bitmap.Width := intCPWdth; //Origin.ttoTplgyOnly.Width * 4 + 100;
        Bitmap.Height := intCPLngth;
        imgPrintFrame.Picture.Graphic := Bitmap;
        imgPrintFrame.Width := intCPWdth; //Origin.ttoTplgyOnly.Width * 4 + 100;
        imgPrintFrame.Height := intCPLngth;
      end;
      kNwckBrcktTree: begin
        intCPLngth := Origin.ttoNwckBrckt.Height * 5;
        intCPWdth := Origin.ttoNwckBrckt.Width * 5; // intMaxTopologyOnlyWidth(40) + intLabelWidth div 2;
        Bitmap.Width := intCPWdth; //Origin.ttoTplgyOnly.Width * 4 + 100;
        Bitmap.Height := intCPLngth;
        imgPrintFrame.Picture.Graphic := Bitmap;
        imgPrintFrame.Width := intCPWdth; //Origin.ttoTplgyOnly.Width * 4 + 100;
        imgPrintFrame.Height := intCPLngth;
      end;
      kNewickTOTree: begin
        intCPLngth := Origin.ttoNewickTO.Height * 5;
        intCPWdth := Origin.ttoNewickTO.Width * 5; // intMaxTopologyOnlyWidth(40) + intLabelWidth div 2;
        Bitmap.Width := intCPWdth; //Origin.ttoTplgyOnly.Width * 4 + 100;
        Bitmap.Height := intCPLngth;
        imgPrintFrame.Picture.Graphic := Bitmap;
        imgPrintFrame.Width := intCPWdth; //Origin.ttoTplgyOnly.Width * 4 + 100;
        imgPrintFrame.Height := intCPLngth;
      end;
    end;
  finally
    Bitmap.Free;
  end;
  scrbxPrintImage.Height := frmPrintTree.Height - 21;
  scrbxPrintImage.Width := frmPrintTree.Width;
  if intCPWdth > scrbxPrintImage.Width then
    scrbxPrintImage.HorzScrollBar.Range := intCPWdth
  else
    scrbxPrintImage.HorzScrollBar.Range := 0;
  if intCPLngth > scrbxPrintImage.Height then
    scrbxPrintImage.VertScrollBar.Range := intCPLngth
  else
    scrbxPrintImage.VertScrollBar.Range := 0;
  scrbxPrintImage.HorzScrollBar.Position := 0;
  scrbxPrintImage.VertScrollBar.Position := 0;
  imgPrintFrame.Refresh;
  imgPrintFrame.Canvas.Pen.Color := clWhite;
  imgPrintFrame.Canvas.Pen.Style := psSolid;
  imgPrintFrame.Canvas.Pen.Width := 1;
  Canvas.Brush.Color := clWhite;
  Canvas.Brush.Style := bsSolid;
  imgPrintFrame.Canvas.Clear;
  imgPrintFrame.Canvas.Pen.Color := clBlack;
  ShowBracketTree(Sender);
end;

procedure TfrmPrintTree.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  boolPrint := True;
  SetPrinterType('');
  SetPrintOut(False);
end;

procedure TfrmPrintTree.FormResize(Sender: TObject);
begin
  scrbxPrintImage.Height := frmPrintTree.Height - 21; // - pnlTreeGraph.Height - 42;
  scrbxPrintImage.Width := frmPrintTree.Width;
  if intCPLngth > scrbxPrintImage.Height then
    scrbxPrintImage.VertScrollBar.Range := intCPLngth
  else
    scrbxPrintImage.VertScrollBar.Range := 0;
  if intCPWdth > scrbxPrintImage.Width then
    scrbxPrintImage.HorzScrollBar.Range := intCPWdth
  else
    scrbxPrintImage.HorzScrollBar.Range := 0;
  if scrbxPrintImage.VertScrollBar.Range = 0 then
    if scrbxPrintImage.VertScrollBar.Position > scrbxPrintImage.Height then
      scrbxPrintImage.VertScrollBar.Position := 0;
  if scrbxPrintImage.HorzScrollBar.Range = 0 then
    if scrbxPrintImage.HorzScrollBar.Position > scrbxPrintImage.Width then
      scrbxPrintImage.HorzScrollBar.Position := 0;
end;

procedure TfrmPrintTree.imgPrintFrameClick(Sender: TObject);
begin

end;

procedure TfrmPrintTree.mnuPrintClick(Sender: TObject);
begin
  PrintBracketTree(prpDoPrintOut, Sender);
  Close;
end;

procedure TfrmPrintTree.ShowBracketTree(Sender: TObject);
var
  intHeight,
  intWidth,
  intNdLvl,
  intLngth,
  intVPages,
  intHPages,
  intCrrLngth,
  intCrrWdth,
  iny,
  inx: integer;
  rctPrnt,
  rctSlctd: TRect;
  GrphCntr: TPoint;

procedure DrawScale(const dblScl: double);
begin
  with imgPrintFrame.Canvas do
  begin
    Pen.Width := 5;
    Font.Size := 40;
    if dblScl > dblZero then
    begin
      MoveTo(115, 105);
      LineTo(115, 155);
      MoveTo(115, 130);
      LineTo(115 + 35 * 5, 130); // + intScl, 30);
      MoveTo(115 + 35 * 5, 105);
      LineTo(115 + 35 * 5, 155);
      if dblScl < 20.0 then
        if dblScl < 5.0 then
          TextOut(120, 35, FloatToStrF(dblScl * 2.0 / 5.0, ffFixed, 8, 6))
        else
          TextOut(120, 35, FloatToStrF(dblScl * 2.0 / 5.0, ffFixed, 8, 4))
      else
        TextOut(120, 35, FloatToStrF(dblScl * 2.0 / 5.0, ffFixed, 8, 0));
//      TextOut(90, 165, 'SBL=' + FloatToStrF(dblSumLngth, ffFixed, 8, 6));
    end
    else
    begin
//      Pen.Color := clWhite;
//      Clear;
//      Pen.Color := clBlack;
    end;
  end;
end;

procedure DrawBracket(const VrtclPstn: TVrtclPosition);
begin
  with VrtclPstn do
  begin
    with imgPrintFrame.Canvas do
    begin
      MoveTo(BgnVrtcl.x * 5, BgnVrtcl.y * 5);
      LineTo(EndVrtcl.x * 5, EndVrtcl.y * 5);
      LineTo(BtmHrzntl.x * 5, BtmHrzntl.y * 5);
      MoveTo(BgnVrtcl.x * 5, BgnVrtcl.y * 5);
      LineTo(TopHrzntl.x * 5, BgnVrtcl.y * 5);
    end;
  end;
end;

procedure DrawBracketTO(const ttoTO: TTplgyOnly; const intFactor: integer);
begin
  with ttoTO do
  begin
    with imgPrintFrame.Canvas do
    begin
      if TopVrtcl.y <> BtmVrtcl.y then
      begin
        MoveTo(TopVrtcl.x * intFactor, TopVrtcl.y * intFactor);
        LineTo(BtmVrtcl.x * intFactor, BtmVrtcl.y * intFactor);
      end;
      MoveTo(BgnHrzntl.x * intFactor, BgnHrzntl.y * intFactor);
      LineTo(EndHrzntl.x * intFactor, EndHrzntl.y * intFactor);
    end;
  end;
end;

procedure DrawLabelTO(const ttoTO: TTplgyOnly; const strSpcs: string; const intFctr, intXOffst, intYOffst: integer);
begin
  imgPrintFrame.Canvas.TextOut(ttoTO.EndHrzntl.x * intFctr - intXOffst, ttoTO.EndHrzntl.y * intFctr - intYOffst, strSpcs);
end;

procedure DrawLabelNwkTO(const pntLabel: TPoint; const strSpcs: string; const intFctr, intXOffst, intYOffst: integer);
begin
  imgPrintFrame.Canvas.TextOut(pntLabel.x * intFctr - intXOffst, pntLabel.y * intFctr - intYOffst, strSpcs);
end;

procedure DrawLabel(const VrtclPstn: TVrtclPosition; const Segment: TSegment);
begin
  with TestTableForSpeciesData, VrtclPstn, Segment do
    with imgPrintFrame.Canvas do
    begin
      if Species[int1stXNd].VerticalMode = Leaf then
        TextOut(TopHrzntl.x * 5 + 5, TopHrzntl.y * 5 - 34{ - 20}, Species[Species[int1stXNd].EndSegment.int1stXNd].SpeciesName);
      if Species[int2ndXNd].VerticalMode = Leaf then
        TextOut(BtmHrzntl.x * 5 + 5, BtmHrzntl.y * 5 - 34{ - 20}, Species[Species[int2ndXNd].EndSegment.int1stXNd].SpeciesName);
    end;
end;

function intPageXYPos(const intScrBxXYPos, intFactor, intOffset: integer): integer;
begin
  Result := intScrBxXYPos * intFactor - intOffset;
end;

procedure DrawBracketLabel(const VrtclPstn: TVrtclPosition; const Segment: TSegment);
begin
  with VrtclPstn, Segment do
    if int1stXNd <> int2ndXNd then
    begin
      DrawBracket(VrtclPstn);
      DrawLabel(VrtclPstn, Segment);
    end;
end;

procedure DrawBootStrapLabelTO(const ttoTO: TTplgyOnly; const dblBS: double; const intFctr, inyXOffst, intYOffst: integer);
begin
  imgPrintFrame.Canvas.TextOut(ttoTO.EndHrzntl.x * intFctr + inyXOffst, ttoTO.EndHrzntl.y * intFctr - intYOffst, FloatToStrF(dblBS, ffFixed, 8, 0));
end;

procedure DrawBootStrapLabel(const VrtclPstn: TVrtclPosition; const dblBS: double; const intFctr, inyXOffst, intYOffst: integer);
begin
  imgPrintFrame.Canvas.TextOut(VrtclPstn.MidPoint.x * intFctr + inyXOffst, VrtclPstn.MidPoint.y * intFctr - intYOffst, FloatToStrF(dblBS, ffFixed, 8, 0));
end;

begin
  case frmTreeGraph.prpGraphType of
    kTopologyOnlyTree: with TestTableForSpeciesData, imgPrintFrame do
    begin
//      intCPLngth := Height;  //Origin.ttoTplgyOnly.Height * 5;
//      intCPWdth := Width;  //Origin.ttoTplgyOnly.Width * 5;
//      Width := intCPWdth;
//      Height := intCPLngth;
{      Canvas.Clear;
      Canvas.Pen.Color := clBlack;
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Width := 1;
      Canvas.Brush.Color := clWhite;
      Canvas.Brush.Style := bsSolid;}
      DrawScale(dblZero);
      for inx := 0 to NodeCount do
        with Species[inx].TplgyOnly do
          if Drawn then
          begin
            if inx < SpeciesCount then
              DrawLabelTO(Species[inx].TplgyOnly, Species[inx].SpeciesName, 5, -5, 34)
            else
              if boolBootStrap then
                DrawBootStrapLabelTO(Species[inx].TplgyOnly, BSPercent[inx - SpeciesCount], 5, 5, 35);
            DrawBracketTO(Species[inx].TplgyOnly, 5);
            if inx = NodeCount then
            begin
              Canvas.MoveTo(BgnHrzntl.x * 5, intPageXYPos(BgnHrzntl.y, 5, 0));
              with Species[NodeCount - 1].TplgyOnly do
              begin
                Canvas.LineTo(BgnHrzntl.x * 5, intPageXYPos(BgnHrzntl.y, 5, 0));
              end;
            end;
          end;
    end;
    kNwckBrcktTree: with TestTableForSpeciesData, imgPrintFrame do
    begin
      DrawScale(dblScale);
      for inx := 0 to Length(aRectTreePlot) - 1 do
        if (aRectTreePlot[inx].LSide.x <> -1) and (aRectTreePlot[inx].LSide.y <> -1) then
          if (aRectTreePlot[inx].RSide.x <> -1) and (aRectTreePlot[inx].RSide.y <> -1) then
          begin
            Canvas.MoveTo(aRectTreePlot[inx].LSide.x * 5, intPageXYPos(aRectTreePlot[inx].LSide.y, 5, 0));
            Canvas.LineTo(aRectTreePlot[inx].RSide.x * 5, intPageXYPos(aRectTreePlot[inx].RSide.y, 5, 0));
            if aRectTreePlot[inx].strLabel <> '' then
              DrawLabelNwkTO(aRectTreePlot[inx].RSide, aRectTreePlot[inx].strLabel, 5, -5, 34);
          end;
      for inx := 0 to Length(aRectTreePlot) - 1 do
        if (aRectTreePlot[inx].TpSide.x <> -1) and (aRectTreePlot[inx].TpSide.y <> -1) then
          if (aRectTreePlot[inx].BttmSide.x <> -1) and (aRectTreePlot[inx].BttmSide.y <> -1) then
          begin
            Canvas.MoveTo(aRectTreePlot[inx].TpSide.x * 5, intPageXYPos(aRectTreePlot[inx].TpSide.y, 5, 0));
            Canvas.LineTo(aRectTreePlot[inx].BttmSide.x * 5, intPageXYPos(aRectTreePlot[inx].BttmSide.y, 5, 0));
          end;
    end;
    kNewickTOTree: with TestTableForSpeciesData, imgPrintFrame do
    begin
      DrawScale(dblZero);
      for inx := 0 to Length(aTOTreePlot) - 1 do
        if (aTOTreePlot[inx].LSide.x <> -1) and (aTOTreePlot[inx].LSide.y <> -1) then
          if (aTOTreePlot[inx].RSide.x <> -1) and (aTOTreePlot[inx].RSide.y <> -1) then
          begin
            Canvas.MoveTo(aTOTreePlot[inx].LSide.x * 5, intPageXYPos(aTOTreePlot[inx].LSide.y, 5, 0));
            Canvas.LineTo(aTOTreePlot[inx].RSide.x * 5, intPageXYPos(aTOTreePlot[inx].RSide.y, 5, 0));
            if aTOTreePlot[inx].strLabel <> '' then
              DrawLabelNwkTO(aTOTreePlot[inx].RSide, aTOTreePlot[inx].strLabel, 5, -5, 34);
          end;
      for inx := 0 to Length(aTOTreePlot) - 1 do
        if (aTOTreePlot[inx].TpSide.x <> -1) and (aTOTreePlot[inx].TpSide.y <> -1) then
          if (aTOTreePlot[inx].BttmSide.x <> -1) and (aTOTreePlot[inx].BttmSide.y <> -1) then
          begin
            Canvas.MoveTo(aTOTreePlot[inx].TpSide.x * 5, intPageXYPos(aTOTreePlot[inx].TpSide.y, 5, 0));
            Canvas.LineTo(aTOTreePlot[inx].BttmSide.x * 5, intPageXYPos(aTOTreePlot[inx].BttmSide.y, 5, 0));
          end;
    end;
    kBracketTree: with TestTableForSpeciesData, imgPrintFrame do
    begin
//      intCPLngth := Height; // Origin.ttoBracket.Height * 5;
//      intCPWdth := Width;  //Origin.ttoBracket.Width * 5;
//      Width := intCPWdth;
//      Height := intCPLngth;
{      Canvas.Clear;
      Canvas.Pen.Color := clBlack;
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Width := 1;
      Canvas.Brush.Color := clWhite;
      Canvas.Brush.Style := bsSolid;}
      DrawScale(dblScale);
      DrawBracket(HomeVrtcl);
      DrawBracketLabel(Species[NodeCount].VrtclPosition, Species[NodeCount].EndSegment);
      DrawBracketLabel(Species[NodeCount - 1].VrtclPosition, Species[NodeCount - 1].EndSegment);
      intNdLvl := 1;
      while boolMoreNodeLevels(intNdLvl) do
      begin
        for inx := 0 to NodeCount - 2 do
          if Species[inx].NodeLevel = intNdLvl then
            DrawBracketLabel(Species[inx].VrtclPosition, Species[inx].EndSegment);
        inc(intNdLvl);
      end;
      if boolBootStrap then
        for inx := SpeciesCount to NodeCount do
          DrawBootStrapLabel(Species[inx].VrtclPosition, BSPercent[inx - SpeciesCount], 5, 5, 25);
    end;
  end;
end;

procedure TfrmPrintTree.PrintBracketTree(const boolPrintTree: boolean; Sender: TObject);
const
  intVStart = 1;
  intHStart = 1;
var
  intVPages,
  intHPages,
  intNdLvl,
  intLngth,
  intCrrLngth,
  intCrrWdth,
  intVirtualPage,
  iny,
  inx: integer;
  rctPrnt,
  rctSlctd: TRect;
  GrphCntr: TPoint;

begin
  if boolPrintTree then
//    if prtdlgTree.Execute then
      with Printer do
      begin
{        if FileExists('C:\Temp\Tree Picture.bmp') then
          if not DeleteFile('C:\Temp\Tree Picture.bmp') then
            MessageDlg('Delete file C:\Temp\Tree Picture.bmp failed.', mtWarning, [mbOK], 0);
        imgTreeGraph.Picture.SaveToFile('C:\Temp\Tree Picture.bmp');
        imgPrintFrame.Picture.LoadFromFile('C:\Temp\Tree Picture.bmp');}
//        Printer.Orientation := poPortrait;;
//        if imgTreeGraph.Width * 5 > Printer.PageWidth then
//          Printer.Orientation := poLandscape;
        Printer.BeginDoc;
        case frmTreeGraph.prpGraphType of
          kTopologyOnlyTree, kNewickTOTree: with TestTableForSpeciesData, Printer.Canvas do
          begin
            intVPages := intCPLngth div intHeight + 1;
            intHPages := intCPWdth div intWidth + 1;
            for iny := intVStart to intVPages do
            begin
              Canvas.Brush.Style := bsSolid;
              Canvas.Brush.Color := clWhite;
//              Picture.Clear;
              with rctPrnt do
              begin
                Left := 0;
                Top := 0;
                if intCPLngth > intHeight * iny then
                  Bottom := intHeight - 1
                else
                  Bottom := intCPLngth - intHeight * (iny - 1) - 1;
              end;
              with rctSlctd do
              begin
                Top := intHeight * (iny - 1);
                if intCPLngth > intHeight * iny then
                  Bottom := intHeight * iny - 1
                else
                  Bottom := intCPLngth;
              end;
              for inx := intHStart to intHPages do
              begin
                Canvas.Pen.Color := clWhite;
                Canvas.Clear;
                Canvas.Pen.Color := clBlack;
//                NewPage;
                with rctPrnt do
                  if intCPWdth > intWidth * inx then
                    Right := intWidth - 1
                  else
                    Right := intCPWdth - intWidth * (inx - 1) - 1;
                with rctSlctd do
                begin
                  Left := intWidth * (inx - 1);
                  if intCPWdth > intWidth * inx then
                    Right := intWidth * inx - 1
                  else
                    Right := intCPWdth - 1;
                end;
//                Canvas.Rectangle(0, 0, intWidth - 1, intHeight - 1);
//                Canvas.
                Canvas.CopyRect(rctPrnt, imgPrintFrame.Canvas, rctSlctd);
                Canvas.Refresh;
                if ((iny = intVPages) and (inx < intHPages)) or (iny < intVPages) then
                  NewPage;
              end;
            end;
          end;
          kBracketTree, kNwckBrcktTree: with TestTableForSpeciesData, Printer.Canvas do
          begin
//            intCPLngth := Origin.ttoBracket.Height * 5;
//            intCPWdth := Origin.ttoBracket.Width * 5;
{            imgPrintFrame.Picture.Bitmap.SetSize(intCPWdth, intCPLngth);
            imgPrintFrame.Picture.Bitmap.TransparentMode := tmFixed;
            imgPrintFrame.Picture.Bitmap.TransparentColor := clWhite;
            imgPrintFrame.Width := intCPWdth;
            imgPrintFrame.Height := intCPLngth;
            imgPrintFrame.Canvas.FillRect(0, 0, intCPWdth - 1, intCPLngth - 1);}
//            if imgPrintFrame.Canvas = nil then
//              imgPrintFrame.Canvas.Create;
//            pictPrint.Height := intCPLngth; // * 6 div 5;
//            pictPrint.Width := intCPWdth; // * 6 div 5;
//            imgPrintFrame.Canvas.BrushCopy(Rect(0, 0, pictPrint.Width, pictPrint.Height));
            intVPages := intCPLngth div intHeight + 1;
            intHPages := intCPWdth div intWidth + 1;
//            CreateBitMap(imgPrintFrame.Picture.BitMap, intCPWdth, intCPLngth);
//            imgPrintFrame.Picture.Bitmap.Canvas.StretchDraw(Rect(0, 0, intCPWdth - 1, intCPLngth - 1), imgTreeGraph.Picture.Bitmap);
//            imgTreeGraph.Picture.SaveToFile('C:\Temp\Tree Picture.bmp');
//            imgPrintFrame.Picture.LoadFromFile('C:\Temp\Tree Picture.bmp');
//            DrawScale(dblScale);
            Canvas.Refresh;
            Canvas.Pen.Color := clBlack;
            Canvas.Pen.Style := psSolid;
            Canvas.Pen.Width := 1;
//            Canvas.Brush.Color := clWhite;
//            Canvas.Brush.Style := bsSolid;
            Canvas.Clear;
{            DrawBracket(HomeVrtcl);
            DrawBracketLabel(Species[NodeCount].VrtclPosition, Species[NodeCount].EndSegment);
            DrawBracketLabel(Species[NodeCount - 1].VrtclPosition, Species[NodeCount - 1].EndSegment);
            intNdLvl := 1;
            while boolMoreNodeLevels(intNdLvl) do
            begin
              for inx := 0 to NodeCount - 2 do
                if Species[inx].NodeLevel = intNdLvl then
                  DrawBracketLabel(Species[inx].VrtclPosition, Species[inx].EndSegment);
              inc(intNdLvl);
            end;
            if boolBootStrap then
              for inx := SpeciesCount to NodeCount do
                DrawBootStrapLabel(Species[inx].VrtclPosition, BSPercent[inx - SpeciesCount], 5, 5, 55);
//            imgPrintFrame.Stretch := True;}
            for iny := intVStart to intVPages do
            begin
              Pen.Color := clBlack;
              Brush.Style := bsSolid;
              Brush.Color := clWhite;
              Clear;
              with rctPrnt do
              begin
                Left := 0;
                Top := 0;
                if intCPLngth > intHeight * iny then
                  Bottom := intHeight - 1
                else
                  Bottom := intCPLngth - intHeight * (iny - 1) - 1;
                if intCPWdth > intHeight * inx then
                  Right := intWidth - 1
                else
                  Right := intCPWdth - intWidth * (inx - 1) - 1;
              end;
              with rctSlctd do
              begin
                Left := intWidth * (inx - 1);
                Top := intHeight * (iny - 1);
                if intCPLngth > intHeight * iny then
                  Bottom := intHeight * iny - 1
                else
                  Bottom := intCPLngth;
                if intCPWdth > intWidth * inx then
                  Right := intWidth * inx - 1
                else
                  Right := intCPWdth;
              end;
              for inx := intHStart to intHPages do
              begin
//                NewPage;
                with rctPrnt do
                  if intCPWdth > intWidth * inx then
                    Right := intWidth - 1
                  else
                    Right := intCPWdth - intWidth * (inx - 1) - 1;
                with rctSlctd do
                begin
                  Left := intWidth * (inx - 1);
                  if intCPWdth > intWidth * inx then
                    Right := intWidth * inx - 1
                  else
                    Right := intCPWdth - 1;
                end;
//                Canvas.CopyMode := cmWhiteness;
//                Canvas.Pen.Color := clWhite;
//                Canvas.Rectangle(0, 0, intWidth - 1, intHeight - 1);
 //               Canvas.FloodFill( 1, 1, clWhite, fsSurface);
                Canvas.Pen.Color := clBlack;
                Canvas.CopyRect(rctPrnt, imgPrintFrame.Canvas, rctSlctd);
//                Canvas.Clear;
                Canvas.Refresh;
                If (intCPLngth - 1 > rctSlctd.BottomRight.x) and (intCPWdth - 1 > rctSlctd.BottomRight.y) then
                  Canvas.CopyMode := cmSrcCopy; { restore the copy mode }
                if ((iny = intVPages) and (inx < intHPages)) or (iny < intVPages) then
                  NewPage;
              end;
            end;
//                  TextOut(Species[inx].VrtclPosition.MidPoint.x * 5 + 5, Species[inx].VrtclPosition.MidPoint.y * 5 - 55, FloatToStrF(BSPercent[inx - SpeciesCount], ffFixed, 8, 0));
//            if (imgTreeGraph.Canvas.Height * 5 > intHeight) or (imgTreeGraph.Canvas.Width * 5 > intWidth) then
//            begin
//              NewPage;
          end;
        end;
        EndDoc;
//        if not DeleteFile('C:\Temp\Tree Picture.bmp') then
//          MessageDlg('Delete file C:\Temp\Tree Picture.bmp failed.', mtWarning, [mbOK], 0);
      end
    else
      MessageDlg('Request to print rectangular/topology only tree has been cancelled.', mtWarning, [mbOK], 0);
// end;
end;

end.

