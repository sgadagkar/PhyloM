unit modTreeGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TATools, PrintersDlgs, RTTICtrls, Forms,
  Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Printers, Menus, Buttons,
  ActnList, ComCtrls, modPrintTree, modSelectNewRoot, modDataTypes, modTestData;

type

  { TfrmTreeGraph }

  TfrmTreeGraph = class(TForm)
    mnuRootedTree: TMenuItem;
    mnuInferredTree: TMenuItem;
    mnuRectangularURt: TMenuItem;
    mnuRectangularRooted: TMenuItem;
    MenuItem3: TMenuItem;
    mnuNewickTO: TMenuItem;
    mnuRadialUnrooted: TMenuItem;
    mnuSaveAs: TMenuItem;
    txtSelectedRoot: TEdit;
    mnuNewickSave: TMenuItem;
    mnuTopologyOnlyURt: TMenuItem;
    svdlgPaintNwk: TSaveDialog;
    txtDisplay: TEdit;
    lblDisplay: TLabel;
    txtSBL: TEdit;
    imgTreeGraph: TImage;
    lblSBL: TLabel;
    lblRoot: TLabel;
    mnuRootTheTreeOn: TMenuItem;
    mnuExit: TMenuItem;
    mnuGraphType: TMenuItem;
    mnuSave: TMenuItem;
    mnuPrint: TMenuItem;
    mnuFile: TMenuItem;
    mnuTreeView: TMainMenu;
    pnlTreeGraph: TPanel;
    ilButtonIcons: TImageList;
    prtdlgTree: TPrintDialog;
    scrbxTreeGraph: TScrollBox;
    svdlgPaintJPEG: TSaveDialog;
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure mnuNewickSaveClick(Sender: TObject);
    procedure mnuNewickTOClick(Sender: TObject);
    procedure mnuTopologyOnlyURtClick(Sender: TObject);
    procedure mnuRadialUnrootedClick(Sender: TObject);
    procedure mnuRectangularRootedClick(Sender: TObject);
    procedure mnuRootTheTreeOnClick(Sender: TObject);
    procedure mnuRootTreeOnClick(Sender: TObject);
    procedure mnuSetRootClick(Sender: TObject);
    procedure pumnuCloseClick(Sender: TObject);
    procedure pumnuPrintTreeClick(Sender: TObject);
    procedure btnShowRadialTreeClick(Sender: TObject);
    procedure btpnlTreeGraphClick(Sender: TObject);
//    procedure cbxSetRootChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuPrintClick(Sender: TObject);
    procedure mnuRectangularURtClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
  private
    { private declarations }
    strRootSelected: string;
    boolFirstOpen,
    boolSavedRootState,
    boolShowTree: boolean;
    wrdDisplayTree: word;
    intRootCount: integer;
    procedure PaintToCanvas(PTCanvas: TCanvas);
    procedure SetFormExtents(PTCanvas: TCanvas);
//    procedure PrintBracketTree(const boolPrintTree: boolean; Sender: TObject);
    procedure PaintToFile(Sender: TObject; const strFileName: string);
    procedure InitialRootList;
    procedure CreateBitMap(var bmpPctr: TBitMap; const intW, intH: integer);
    procedure SetGraphTreeType(const ADisplay: word);
  public
    { public declarations }
    strOrgNewick,
    strNwckTrShow: string;
    boolOriginal,
    boolShowLabels,
    boolComplete: boolean;
//    intSplit: integer;
    intRoot: integer;
    intTreeType: integer;
    procedure RootSelected(Sender: TObject);
    procedure RootCancelled(Sender: TObject);
    procedure SetShowLabels(const boolShwLbls: boolean);
  published
    property prpGraphType: word read wrdDisplayTree write SetGraphTreeType;
    property prpShowLabels: boolean read boolShowLabels write SetShowLabels;
  end;

var
  strlsRootList: TStringList;
  frmTreeGraph: TfrmTreeGraph;
  intScale: integer;
  dblScale: double;
  HomeVrtcl: TVrtclPosition;

implementation

{$R *.lfm}

{ TfrmTreeGraph }

uses
  modPhyIoM, modTreeView, modAbout;

procedure TfrmTreeGraph.FormCreate(Sender: TObject);
begin
  boolFirstOpen := True;
  boolShowTree := True;
  prpGraphType := kBracketTree;
  boolSavedRootState := False;
  boolOriginal := False;
  boolComplete := False;
  intRootCount := 0;
  intRoot := TestTableForSpeciesData.intRoot;
  intTreeType := 1;
//  intSplit := 50;
  with Origin do
  begin
    initWidth := ttoBracket.Width;
    initHeight := ttoBracket.Height;
  end;
  strNwckTrShow := '';
  strOrgNewick := '';
  strRootSelected := '';
  SetShowLabels(False);
  strlsRootList := TStringList.Create;
  if frmPhyIoM.boolTestRun then
    mnuRadialUnrooted.Visible := True;
{  imgPrintFrame.Canvas;}
end;

procedure TfrmTreeGraph.SetGraphTreeType(const ADisplay: word);
begin
  if ADisplay <> wrdDisplayTree then
    if ADisplay in [kRadialTree, kBracketTree, kTopologyOnlyTree, kNwckBrcktTree, kNewickTOTree] then
    begin
      wrdDisplayTree := ADisplay;
      case ADisplay of
        kRadialTree: txtDisplay.Text := 'Radial Unrooted';
        kBracketTree: txtDisplay.Text := 'Rectangular Unrooted';
        kTopologyOnlyTree: txtDisplay.Text := 'Topology Only Unrooted';
        kNwckBrcktTree: begin
          if txtSelectedRoot.Text <> '' then
            if txtSelectedRoot.Text <> strRootSelected then
              strRootSelected := txtSelectedRoot.Text;
//            else
//          else
//            if cbxSetRoot.Text <> '' then
//              if cbxSetRoot.Text <> strRootSelected then
//                strRootSelected := cbxSetRoot.Text;
          txtDisplay.Text := 'Rectangular Rooted'; // - '; + strRootSelected;
        end;
        kNewickTOTree: begin
          if txtSelectedRoot.Text <> '' then
            if txtSelectedRoot.Text <> strRootSelected then
              strRootSelected := txtSelectedRoot.Text;
//            else
//          else
//            if cbxSetRoot.Text <> '' then
//              if cbxSetRoot.Text <> strRootSelected then
//                strRootSelected := cbxSetRoot.Text;
          txtDisplay.Text := 'Topology Only Rooted'; // - ' + strRootSelected;
        end;
      end;
    end
    else
      wrdDisplayTree := kUnknownDsp;
end;

procedure TfrmTreeGraph.SetShowLabels(const boolShwLbls: boolean);
begin
  boolShowLabels := boolShwLbls;
end;

procedure TfrmTreeGraph.SetFormExtents(PTCanvas: TCanvas);
var
  inx: integer;
begin
  case prpGraphType of
    kRadialTree: begin
      if Origin.ttoRadial.Width > scrbxTreeGraph.Width - {trvwTextTreeTG.Left - chtTreeGraph.Left - }20 then
      begin
        scrbxTreeGraph.HorzScrollBar.Range := Origin.ttoRadial.Width + {trvwTextTreeTG.Left + chtTreeGraph.Left + }20;
//        chtTreeGraph.Width := HorzScrollBar.Range - 20;
      end
      else
      begin
        scrbxTreeGraph.HorzScrollBar.Range := 0;
//        chtTreeGraph.Width := frmTreeGraph.Width - 20;
      end;
      if Origin.ttoRadial.Height > scrbxTreeGraph.Height - {trvwTextTreeTG.Top - chtTreeGraph.Top - }20 then
      begin
        scrbxTreeGraph.VertScrollBar.Range := Origin.ttoRadial.Height + {trvwTextTreeTG.Top + chtTreeGraph.Top + }20;
//        chtTreeGraph.Height := VertScrollBar.Range - 20;
      end
      else
      begin
        scrbxTreeGraph.VertScrollBar.Range := 0;
//        chtTreeGraph.Height := frmTreeGraph.Height - 20;
      end;
      PTCanvas.MoveTo(Origin.ttoRadial.GraphOrigin);
    end;
    kBracketTree: begin
      if Origin.ttoBracket.Width > scrbxTreeGraph.Width + {trvwTextTreeTG.Left - chtTreeGraph.Left - }20 then
      begin
        scrbxTreeGraph.HorzScrollBar.Range := Origin.ttoBracket.Width + {trvwTextTreeTG.Left + chtTreeGraph.Left + }20;
//        chtTreeGraph.Width := HorzScrollBar.Range - 20;
      end
      else
      begin
        scrbxTreeGraph.HorzScrollBar.Range := 0;
//        chtTreeGraph.Width := frmTreeGraph.Width - 20;
      end;
      if Origin.ttoBracket.Height > scrbxTreeGraph.Height + {trvwTextTreeTG.Top - chtTreeGraph.Top - }20 then
      begin
        scrbxTreeGraph.VertScrollBar.Range := Origin.ttoBracket.Height + {trvwTextTreeTG.Top + chtTreeGraph.Top + }20;
//        chtTreeGraph.Height := VertScrollBar.Range - 20;
      end
      else
      begin
        scrbxTreeGraph.VertScrollBar.Range := 0;
//        chtTreeGraph.Height := frmTreeGraph.Height - 20;
      end;
      PTCanvas.MoveTo(Origin.ttoBracket.GraphOrigin);
    end;
    kTopologyOnlyTree: begin
      if Origin.ttoTplgyOnly.Width > scrbxTreeGraph.Width + {trvwTextTreeTG.Left - chtTreeGraph.Left - }20 then
      begin
        scrbxTreeGraph.HorzScrollBar.Range := Origin.ttoTplgyOnly.Width + {trvwTextTreeTG.Left + chtTreeGraph.Left + }20;
//        chtTreeGraph.Width := HorzScrollBar.Range - 20;
      end
      else
      begin
        scrbxTreeGraph.HorzScrollBar.Range := 0;
//        chtTreeGraph.Width := frmTreeGraph.Width - 20;
      end;
      if Origin.ttoTplgyOnly.Height > scrbxTreeGraph.Height - {trvwTextTreeTG.Top - chtTreeGraph.Top - }20 then
      begin
        scrbxTreeGraph.VertScrollBar.Range := Origin.ttoTplgyOnly.Height + {trvwTextTreeTG.Top + chtTreeGraph.Top + }20;
//        chtTreeGraph.Height := VertScrollBar.Range - 20;
      end
      else
      begin
        scrbxTreeGraph.VertScrollBar.Range := 0;
//        chtTreeGraph.Height := frmTreeGraph.Height - 20;
      end;
      PTCanvas.MoveTo(Origin.ttoTplgyOnly.GraphOrigin);
    end;
    kNwckBrcktTree: begin
      if Origin.ttoNwckBrckt.Width > scrbxTreeGraph.Width - {trvwTextTreeTG.Left - chtTreeGraph.Left - }20 then
      begin
        scrbxTreeGraph.HorzScrollBar.Range := Origin.ttoNwckBrckt.Width + {trvwTextTreeTG.Left + chtTreeGraph.Left + }20;
//        chtTreeGraph.Width := HorzScrollBar.Range - 20;
      end
      else
      begin
        scrbxTreeGraph.HorzScrollBar.Range := 0;
//        chtTreeGraph.Width := frmTreeGraph.Width - 20;
      end;
      if Origin.ttoNwckBrckt.Height > scrbxTreeGraph.Height - {trvwTextTreeTG.Top - chtTreeGraph.Top - }20 then
      begin
        scrbxTreeGraph.VertScrollBar.Range := Origin.ttoNwckBrckt.Height + {trvwTextTreeTG.Top + chtTreeGraph.Top + }20;
//        chtTreeGraph.Height := VertScrollBar.Range - 20;
      end
      else
      begin
        scrbxTreeGraph.VertScrollBar.Range := 0;
//        chtTreeGraph.Height := frmTreeGraph.Height - 20;
      end;
      PTCanvas.MoveTo(Origin.ttoNwckBrckt.GraphOrigin);
    end;
    kNewickTOTree: begin
      if Origin.ttoNewickTO.Width > scrbxTreeGraph.Width + {trvwTextTreeTG.Left - chtTreeGraph.Left - }20 then
      begin
        scrbxTreeGraph.HorzScrollBar.Range := Origin.ttoNewickTO.Width + {trvwTextTreeTG.Left + chtTreeGraph.Left + }20;
//        chtTreeGraph.Width := HorzScrollBar.Range - 20;
      end
      else
      begin
        scrbxTreeGraph.HorzScrollBar.Range := 0;
//        chtTreeGraph.Width := frmTreeGraph.Width - 20;
      end;
      if Origin.ttoNewickTO.Height > scrbxTreeGraph.Height - {trvwTextTreeTG.Top - chtTreeGraph.Top - }20 then
      begin
        scrbxTreeGraph.VertScrollBar.Range := Origin.ttoNewickTO.Height + {trvwTextTreeTG.Top + chtTreeGraph.Top + }20;
//        chtTreeGraph.Height := VertScrollBar.Range - 20;
      end
      else
      begin
        scrbxTreeGraph.VertScrollBar.Range := 0;
//        chtTreeGraph.Height := frmTreeGraph.Height - 20;
      end;
      PTCanvas.MoveTo(Origin.ttoNewickTO.GraphOrigin);
    end;
  end;
//  trvwTextTreeTG.Items.Assign(frmDPhyIoM.trvwTreeGraph.Items);
  {chtTreeGraph.}PTCanvas.Refresh;
  {chtTreeGraph.}imgTreeGraph.Repaint;
end;

procedure TfrmTreeGraph.FormPaint(Sender: TObject);
begin
  PaintToCanvas(imgTreeGraph.Canvas);
end;

procedure TfrmTreeGraph.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmTreeGraph.mnuPrintClick(Sender: TObject);
begin
  frmPrintTree.Show;
  frmPrintTree.Hide;
  if frmPrintTree.prpDoPrintOut then
    frmPrintTree.mnuPrintClick(Sender);
//  frmPrintTree.Close
//  PrintBracketTree(True, Sender);
end;

procedure TfrmTreeGraph.mnuRectangularURtClick(Sender: TObject);
var
  intSaveHeight: integer;
begin
  prpGraphType := kBracketTree;
  txtSelectedRoot.Enabled := False;
  lblRoot.Enabled := False;
  imgTreeGraph.Width := Origin.ttoBracket.Width;
  imgTreeGraph.Height := Origin.ttoBracket.Height;
  if Origin.ttoBracket.Width > scrbxTreeGraph.Width - {trvwTextTreeTG.Left - chtTreeGraph.Left - }20 then
  begin
    scrbxTreeGraph.HorzScrollBar.Range := Origin.ttoBracket.Width + 20;
//    chtTreeGraph.Width := HorzScrollBar.Range - 20;
  end
  else
  begin
    scrbxTreeGraph.HorzScrollBar.Range := 0;
//    chtTreeGraph.Width := scrbxTreeGraph.Width - 20;
  end;
  if scrbxTreeGraph.HorzScrollBar.Range = 0 then
    if scrbxTreeGraph.HorzScrollBar.Position > scrbxTreeGraph.Width then
      scrbxTreeGraph.HorzScrollBar.Position := 0;
  if Origin.ttoBracket.Height > scrbxTreeGraph.Height + {chtTreeGraph.Top - }20 then
  begin
    scrbxTreeGraph.VertScrollBar.Range := Origin.ttoBracket.Height + 20;
//    chtTreeGraph.Height := VertScrollBar.Range - 20;
  end
  else
  begin
    scrbxTreeGraph.VertScrollBar.Range := 0;
//    chtTreeGraph.Height := scrbxTreeGraph.Height - 20;
  end;
  if scrbxTreeGraph.VertScrollBar.Range = 0 then
    if scrbxTreeGraph.VertScrollBar.Position > scrbxTreeGraph.Height then
      scrbxTreeGraph.VertScrollBar.Position := 0;
  mnuRectangularURt.Enabled := False;
//  btnShowRadialTree.Enabled := True;
  mnuTopologyOnlyURt.Enabled := True;
  mnuRadialUnrooted.Enabled := True;
  if txtSelectedRoot.Text <> '' then
  begin
    mnuRectangularRooted.Enabled := True;
    mnuNewickTO.Enabled := True
  end;
//  boolSavedRootState := mnuRootTreeOn.Checked;
//  intRootCount := cbxSetRoot.Items.Count;
//  mnuRootTreeOn.Checked := False;
//  mnuPrint.Enabled := True;
//  mnuSave.Enabled := True;
//  cbxSetRoot.Enabled := False;
//  mnuRootTreeOn.Enabled := False;;
//  mnuSetRoot.Enabled := False;
  boolShowTree := True;
  intSaveHeight := Height;
  Height := 400;
  Refresh;
  Height := intSaveHeight;
  Refresh;
end;

procedure TfrmTreeGraph.mnuSaveClick(Sender: TObject);
var
  strFlNm: string;
begin
  svdlgPaintJPEG.InitialDir := strCurrentDir;
  svdlgPaintJPEG.FileName := Copy(frmPhyIoM.edtActiveFile.Text, 1, Length(frmPhyIoM.edtActiveFile.Text) - 4);
  if prpGraphType in [kNwckBrcktTree, kNewickTOTree] then
    svdlgPaintJPEG.FileName := svdlgPaintJPEG.FileName + ' Rooted-' + txtSelectedRoot.Text;
  if svdlgPaintJPEG.Execute then
  begin
    strFlNm := svdlgPaintJPEG.FileName;
    if FileExists(strFlNm) then
      if mrYes = MessageDlg('File: ' + strFlNm + ' already exists do you want to replace it?', mtConfirmation, [mbYes, mbNo], 0) then
        try
          PaintToFile(Sender, strFlNm);
        except
          on E:exception do MessageDlg(E.Message + ' for file: ' + strFlNm, mtWarning, [mbOK], 0);
        end
      else
        MessageDlg('Original file: ' + strFlNm + ' unchanged.', mtWarning, [mbOK], 0)
    else
      try
        PaintToFile(Sender, strFlNm);
      except
        on E:exception do MessageDlg(E.Message + ' for file: ' + strFlNm, mtWarning, [mbOK], 0);
      end
  end
  else
    MessageDlg('Request to save file: ' + svdlgPaintJPEG.FileName + ' cancelled.', mtWarning, [mbOK], 0);
end;

procedure TfrmTreeGraph.mnuNewickTOClick(Sender: TObject);
var
  intSaveHeight: integer;
begin
  prpGraphType := kNewickTOTree;
  txtSelectedRoot.Enabled := True;
  lblRoot.Enabled := True;
//  if Origin.ttoTplgyOnly.Width < aTOTreePlot[0].RSide.x + 200 then
//    imgTreeGraph.Width := aTOTreePlot[0].RSide.x + 200
//  else
  imgTreeGraph.Width := Origin.ttoNewickTO.Width;
  imgTreeGraph.Height := Origin.ttoNewickTO.Height;
  if Origin.ttoNewickTO.Width > scrbxTreeGraph.Width + {trvwTextTreeTG.Left - chtTreeGraph.Left - }20 then
  begin
    scrbxTreeGraph.HorzScrollBar.Range := Origin.ttoNewickTO.Width + 20;
//    chtTreeGraph.Width := HorzScrollBar.Range - 20;
  end
  else
  begin
    scrbxTreeGraph.HorzScrollBar.Range := 0;
//    chtTreeGraph.Width := frmTreeGraph.Width - 20;
  end;
  if scrbxTreeGraph.HorzScrollBar.Range = 0 then
    if scrbxTreeGraph.HorzScrollBar.Position > scrbxTreeGraph.Width then
      scrbxTreeGraph.HorzScrollBar.Position := 0;
  if Origin.ttoNewickTO.Height > scrbxTreeGraph.Height + {trvwTextTreeTG.Top - chtTreeGraph.Top - }20 then
  begin
    scrbxTreeGraph.VertScrollBar.Range := Origin.ttoNewickTO.Height + 20;
//    chtTreeGraph.Height := VertScrollBar.Range - 20;
  end
  else
  begin
    scrbxTreeGraph.VertScrollBar.Range := 0;
//    chtTreeGraph.Height := frmTreeGraph.Height - 20;
  end;
  if scrbxTreeGraph.VertScrollBar.Range = 0 then
    if scrbxTreeGraph.VertScrollBar.Position > scrbxTreeGraph.Height then
      scrbxTreeGraph.VertScrollBar.Position := 0;
//  btnShowRadialTree.Enabled := True;
  mnuRectangularURt.Enabled := True;
  mnuTopologyOnlyURt.Enabled := True;
  mnuRectangularRooted.Enabled := True;
  mnuRadialUnrooted.Enabled := True;
  mnuNewickTO.Enabled := False;
//  boolSavedRootState := mnuRootTreeOn.Checked;
//  intRootCount := cbxSetRoot.Items.Count;
//  mnuRootTreeOn.Checked := False;
//  cbxSetRoot.Enabled := False;
//  mnuRootTreeOn.Enabled := False;;
//  mnuSetRoot.Enabled := False;
  boolShowTree := True;
  intSaveHeight := Height;
  Height := 400;
  Refresh;
  Height := intSaveHeight;
  Refresh;
end;

procedure TfrmTreeGraph.mnuTopologyOnlyURtClick(Sender: TObject);
var
  intSaveHeight: integer;
begin
  prpGraphType := kTopologyOnlyTree;
//  FormActivate(Sender);
  txtSelectedRoot.Enabled := False;
  lblRoot.Enabled := False;
  imgTreeGraph.Width := Origin.ttoTplgyOnly.Width;
  imgTreeGraph.Height := Origin.ttoTplgyOnly.Height;
  if Origin.ttoTplgyOnly.Width > scrbxTreeGraph.Width + {trvwTextTreeTG.Left - chtTreeGraph.Left - }20 then
  begin
    scrbxTreeGraph.HorzScrollBar.Range := Origin.ttoTplgyOnly.Width + 20;
//    chtTreeGraph.Width := HorzScrollBar.Range - 20;
  end
  else
  begin
    scrbxTreeGraph.HorzScrollBar.Range := 0;
//    chtTreeGraph.Width := frmTreeGraph.Width - 20;
  end;
  if scrbxTreeGraph.HorzScrollBar.Range = 0 then
    if scrbxTreeGraph.HorzScrollBar.Position > scrbxTreeGraph.Width then
      scrbxTreeGraph.HorzScrollBar.Position := 0;
  if Origin.ttoTplgyOnly.Height > scrbxTreeGraph.Height + {trvwTextTreeTG.Top - chtTreeGraph.Top - }20 then
  begin
    scrbxTreeGraph.VertScrollBar.Range := Origin.ttoTplgyOnly.Height + 20;
//    chtTreeGraph.Height := VertScrollBar.Range - 20;
  end
  else
  begin
    scrbxTreeGraph.VertScrollBar.Range := 0;
//    chtTreeGraph.Height := frmTreeGraph.Height - 20;
  end;
  if scrbxTreeGraph.VertScrollBar.Range = 0 then
    if scrbxTreeGraph.VertScrollBar.Position > scrbxTreeGraph.Height then
      scrbxTreeGraph.VertScrollBar.Position := 0;
//  btnShowRadialTree.Enabled := True;
  mnuRectangularURt.Enabled := True;
  mnuRadialUnrooted.Enabled := True;
  mnuTopologyOnlyURt.Enabled := False;
//  mnuPrint.Enabled := False;
//  mnuSave.Enabled := False;
//  chkbxSetRoot.Enabled := True;
//  mnuRootTreeOn.Enabled := True;
//  mnuRootTreeOn.Checked := boolSavedRootState;
//  cbxSetRoot.Text := strSavedRoot;
  if txtSelectedRoot.Text <> '' then
  begin
    mnuRectangularRooted.Enabled := True;
    mnuNewickTO.Enabled := True
  end;
{  if mnuRootTreeOn.Checked then
  begin
    cbxSetRoot.Enabled := True;
    if cbxSetRoot.Text <> '' then
    begin
      mnuSetRoot.Enabled := True;
      mnuTopologyOnlyURt.Enabled := True;
    end
    else
    begin
      mnuSetRoot.Enabled := False;
      mnuTopologyOnlyURt.Enabled := False;
    end;
  end;}
  boolShowTree := True;
  intSaveHeight := Height;
  Height := 400;
  Refresh;
  Height := intSaveHeight;
  Refresh;
end;

{procedure TfrmTreeGraph.spbtnSetRootClick(Sender: TObject);
var
  strNewickFile: string;
  intRoot: integer;
  boolFileAvailable: boolean;
begin
  intRoot := intFindShortLabel(cbxSetRoot.Text);
  if intRoot > -1 then
  begin
    frmTreeListDown.Show;
    frmTreeListDown.trvwTextDownBased.Selected := nodSelectBranch(frmTreeListDown.trvwTextDownBased.Items, intRoot);
    frmTreeListDown.btnSetRootClick(Sender);
    frmTreeListDown.svdlgRerootedNewick.InitialDir := strCurrentDir;
    frmTreeListDown.Hide;
    if frmTreeListDown.svdlgRerootedNewick.Execute then
    begin
      boolFileAvailable := True;
      strNewickFile := frmTreeListDown.svdlgRerootedNewick.FileName;
      if FileExists(strNewickFile) then
        if mrYes = MessageDlg('File: ' + strNewickFile + ' already exists do you want to replace it?', mtConfirmation, [mbYes, mbNo], 0) then
          try
            frmTreeListDown.memRerootedNwkFmt.Lines.SaveToFile(strNewickFile);
          except
            on E:exception do MessageDlg(E.Message + ' for file: ' + strNewickFile, mtWarning, [mbOK], 0);
          end
        else
          MessageDlg('Original file: ' + strNewickFile + ' unchanged.', mtWarning, [mbOK], 0)
      else
        try
          frmTreeListDown.memRerootedNwkFmt.Lines.SaveToFile(strNewickFile);
        except
          on E:exception do MessageDlg(E.Message + ' for file: ' + strNewickFile, mtWarning, [mbOK], 0);
        end;
    end
    else
      MessageDlg('Request to save file: ' + frmTreeListDown.svdlgRerootedNewick.FileName + ' cancelled.', mtWarning, [mbOK], 0);
    CreateTopologyRerootedTree;
    frmTreeListDown.Close;
  end;
end;}

procedure TfrmTreeGraph.FormResize(Sender: TObject);
begin
  scrbxTreeGraph.Height := frmTreeGraph.Height - pnlTreeGraph.Height - 21;
  scrbxTreeGraph.Width := frmTreeGraph.Width;
  pnlTreeGraph.Width := frmTreeGraph.Width;
  if scrbxTreeGraph.Height < imgTreeGraph.Height then
    scrbxTreeGraph.VertScrollBar.Range := imgTreeGraph.Height
  else
  begin
    scrbxTreeGraph.VertScrollBar.Position := 0;
    scrbxTreeGraph.VertScrollBar.Range := 0;
  end;
  if scrbxTreeGraph.Width < imgTreeGraph.Width then
    scrbxTreeGraph.HorzScrollBar.Range := imgTreeGraph.Width
  else
  begin
    scrbxTreeGraph.HorzScrollBar.Position := 0;
    scrbxTreeGraph.HorzScrollBar.Range := 0;
  end;
end;

procedure TfrmTreeGraph.FormShow(Sender: TObject);
begin
  if TestTableForSpeciesData.boolNewCalculation then
  begin
    InitialRootList;
    strRootSelected := '';
    txtSelectedRoot.Text := strRootSelected;
    txtSelectedRoot.Enabled := False;
    lblRoot.Enabled := False;
{    if strlsRootList.Count > intRoot then
    begin
      if intRoot > -1 then
        strRootSelected := strlsRootList[intRoot]
      else
        strRootSelected := '';
      txtSelectedRoot.Text := strRootSelected;
    end;}
    TestTableForSpeciesData.boolNewCalculation := False;
    intRoot := TestTableForSpeciesData.intRoot;
    SetGraphTreeType(kBracketTree);
    mnuRootedTree.Visible := False;
    mnuNewickTO.Enabled := False;
    mnuRectangularRooted.Enabled := False;
    mnuRectangularURt.Enabled := False;
    Show;
  end
  else
    if txtSelectedRoot.Text <> '' then
      if wrdDisplayTree in [kNwckBrcktTree, kNewickTOTree] then
      begin
        txtSelectedRoot.Enabled := True;
        lblRoot.Enabled := True;
      end
      else
      begin
        txtSelectedRoot.Enabled := False;
        lblRoot.Enabled := False;
      end
end;

procedure TfrmTreeGraph.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMinimized then
    if frmSelectNewRoot.Showing then
      if frmSelectNewRoot.WindowState = wsNormal then
        frmSelectNewRoot.WindowState := wsMinimized
      else
    else
  else
    if WindowState in [wsNormal, wsMaximized, wsFullScreen] then
      if frmSelectNewRoot.Showing then
        frmSelectNewRoot.WindowState := wsNormal;
end;

procedure TfrmTreeGraph.mnuNewickSaveClick(Sender: TObject);
var
  strlsNewick: TStringList;
  strFlNm: string;
begin
  svdlgPaintNwk.InitialDir := strCurrentDir;
  svdlgPaintNwk.FileName := Copy(frmPhyIoM.edtActiveFile.Text, 1, Length(frmPhyIoM.edtActiveFile.Text) - 4);
  if prpGraphType in [kNwckBrcktTree, kNewickTOTree] then
    svdlgPaintNwk.FileName := svdlgPaintNwk.FileName + ' Rooted-' + txtSelectedRoot.Text;
  if svdlgPaintNwk.Execute then
  begin
    strFlNm := svdlgPaintNwk.FileName;
    if FileExists(strFlNm) then
      if mrYes = MessageDlg('File: ' + strFlNm + ' already exists do you want to replace it?', mtConfirmation, [mbYes, mbNo], 0) then
        try
          strlsNewick := TStringList.Create;
          try
            if prpGraphType in [kRadialTree, kBracketTree, kTopologyOnlyTree] then
              if prpGraphType in [kRadialTree, kBracketTree] then
                strlsNewick.Add(strOrgNewick)
              else
                strlsNewick.Add(strStripLengthsWithBS(strOrgNewick))
            else
              if prpGraphType in [kNwckBrcktTree, kNewickTOTree] then
                if prpGraphType = kNwckBrcktTree then
                  strlsNewick.Add(strNwckTrShow)
                else
                  strlsNewick.Add(strStripLengthsWithBS(strNwckTrShow));
            strlsNewick.SaveToFile(strFlNm);
          finally
            strlsNewick.Free;
          end;
        except
          on E:exception do MessageDlg(E.Message + ' for file: ' + strFlNm, mtWarning, [mbOK], 0);
        end
      else
        MessageDlg('Original file: ' + strFlNm + ' unchanged.', mtWarning, [mbOK], 0)
    else
      try
        strlsNewick := TStringList.Create;
        try
          if prpGraphType in [kBracketTree, kTopologyOnlyTree] then
            strlsNewick.Add(strOrgNewick)
          else
            if prpGraphType in [kNwckBrcktTree, kNewickTOTree] then
              strlsNewick.Add(strNwckTrShow);
          strlsNewick.SaveToFile(strFlNm);
        finally
          strlsNewick.Free;
        end;
      except
        on E:exception do MessageDlg(E.Message + ' for file: ' + strFlNm, mtWarning, [mbOK], 0);
      end;
  end
  else
    MessageDlg('Request to save file: ' + svdlgPaintNwk.FileName + ' cancelled.', mtWarning, [mbOK], 0);
end;

{procedure TfrmTreeGraph.mnuTopologyOnlyURtClick(Sender: TObject);
begin
  prpGraphType := kNewickTOTree;
//  if Origin.ttoTplgyOnly.Width < aTOTreePlot[0].RSide.x + 200 then
//    imgTreeGraph.Width := aTOTreePlot[0].RSide.x + 200
//  else
  imgTreeGraph.Width := Origin.ttoNewickTO.Width;
  imgTreeGraph.Height := Origin.ttoNewickTO.Height;
  if Origin.ttoNewickTO.Width > scrbxTreeGraph.Width + {trvwTextTreeTG.Left - chtTreeGraph.Left - }20 then
  begin
    scrbxTreeGraph.HorzScrollBar.Range := Origin.ttoNewickTO.Width + 20;
//    chtTreeGraph.Width := HorzScrollBar.Range - 20;
  end
  else
  begin
    scrbxTreeGraph.HorzScrollBar.Range := 0;
//    chtTreeGraph.Width := frmTreeGraph.Width - 20;
  end;
  if scrbxTreeGraph.HorzScrollBar.Range = 0 then
    if scrbxTreeGraph.HorzScrollBar.Position > scrbxTreeGraph.Width then
      scrbxTreeGraph.HorzScrollBar.Position := 0;
  if Origin.ttoNewickTO.Height > scrbxTreeGraph.Height + {trvwTextTreeTG.Top - chtTreeGraph.Top - }20 then
  begin
    scrbxTreeGraph.VertScrollBar.Range := Origin.ttoNewickTO.Height + 20;
//    chtTreeGraph.Height := VertScrollBar.Range - 20;
  end
  else
  begin
    scrbxTreeGraph.VertScrollBar.Range := 0;
//    chtTreeGraph.Height := frmTreeGraph.Height - 20;
  end;
  if scrbxTreeGraph.VertScrollBar.Range = 0 then
    if scrbxTreeGraph.VertScrollBar.Position > scrbxTreeGraph.Height then
      scrbxTreeGraph.VertScrollBar.Position := 0;
//  btnShowRadialTree.Enabled := True;
  mnuRectangular.Enabled := True;
  mnuTopologyOnly.Enabled := True;
  mnuNewickTO.Enabled := True;
  mnuRadialUnrooted.Enabled := True;
  mnuTopologyOnlyURt.Enabled := False;
//  boolSavedRootState := mnuRootTreeOn.Checked;
//  intRootCount := cbxSetRoot.Items.Count;
//  mnuRootTreeOn.Checked := False;
//  cbxSetRoot.Enabled := False;
//  mnuRootTreeOn.Enabled := False;;
//  mnuSetRoot.Enabled := False;
  boolShowTree := True;
  Hide;
  Show;
end;}

procedure TfrmTreeGraph.mnuRadialUnrootedClick(Sender: TObject);
var
  intSaveHeight: integer;
begin
  prpGraphType := kRadialTree;
  txtSelectedRoot.Enabled := False;
  lblRoot.Enabled := False;
  imgTreeGraph.Width := Origin.ttoRadial.Width;
  imgTreeGraph.Height := Origin.ttoRadial.Height;
  if Origin.ttoRadial.Width > scrbxTreeGraph.Width - {trvwTextTreeTG.Left - chtTreeGraph.Left - }20 then
  begin
    scrbxTreeGraph.HorzScrollBar.Range := Origin.ttoRadial.Width + 20;
//    chtTreeGraph.Width := HorzScrollBar.Range - 20;
  end
  else
  begin
//    chtTreeGraph.Width := frmTreeGraph.Width - 20;
    scrbxTreeGraph.HorzScrollBar.Range := 0;
  end;
  if scrbxTreeGraph.HorzScrollBar.Range = 0 then
    if scrbxTreeGraph.HorzScrollBar.Position > scrbxTreeGraph.Width then
      scrbxTreeGraph.HorzScrollBar.Position := 0;
  if Origin.ttoRadial.Height > scrbxTreeGraph.Height - {trvwTextTreeTG.Top - chtTreeGraph.Top - }20 then
  begin
    scrbxTreeGraph.VertScrollBar.Range := Origin.ttoRadial.Height + 20;
//    chtTreeGraph.Height := VertScrollBar.Range - 20;
  end
  else
  begin
    scrbxTreeGraph.VertScrollBar.Range := 0;
//    chtTreeGraph.Height := frmTreeGraph.Height - 20;
  end;
  if scrbxTreeGraph.VertScrollBar.Range = 0 then
    if scrbxTreeGraph.VertScrollBar.Position > scrbxTreeGraph.Height then
      scrbxTreeGraph.VertScrollBar.Position := 0;
//  btnShowRadialTree.Enabled := False;
//  btnShowRectangularTree.Enabled := True;
//  btnShowTopologyOnly.Enabled := True;
//  chkbxSetRoot.Enabled := False;
  mnuRectangularURt.Enabled := True;
  mnuTopologyOnlyURt.Enabled := True;
  mnuRadialUnrooted.Enabled := False;
  if txtSelectedRoot.Text <> '' then
  begin
    mnuRectangularRooted.Enabled := True;
    mnuNewickTO.Enabled := True
  end;
  boolShowTree := True;
  intSaveHeight := Height;
  Height := 400;
  Refresh;
  Height := intSaveHeight;
  Refresh;
end;

procedure TfrmTreeGraph.mnuRectangularRootedClick(Sender: TObject);
var
  intSaveHeight: integer;
begin
  prpGraphType := kNwckBrcktTree;
  imgTreeGraph.Width := Origin.ttoNwckBrckt.Width;
  imgTreeGraph.Height := Origin.ttoNwckBrckt.Height;
  if Origin.ttoNwckBrckt.Width > scrbxTreeGraph.Width + {trvwTextTreeTG.Left - chtTreeGraph.Left - }20 then
    scrbxTreeGraph.HorzScrollBar.Range := Origin.ttoNwckBrckt.Width + 20
  else
    scrbxTreeGraph.HorzScrollBar.Range := 0;
  if scrbxTreeGraph.HorzScrollBar.Range = 0 then
    if scrbxTreeGraph.HorzScrollBar.Position > scrbxTreeGraph.Width then
      scrbxTreeGraph.HorzScrollBar.Position := 0;
  if Origin.ttoNwckBrckt.Height > scrbxTreeGraph.Height + {trvwTextTreeTG.Top - chtTreeGraph.Top - }20 then
    scrbxTreeGraph.VertScrollBar.Range := Origin.ttoNwckBrckt.Height + 20
  else
    scrbxTreeGraph.VertScrollBar.Range := 0;
  if scrbxTreeGraph.VertScrollBar.Range = 0 then
    if scrbxTreeGraph.VertScrollBar.Position > scrbxTreeGraph.Height then
      scrbxTreeGraph.VertScrollBar.Position := 0;
  mnuRectangularURt.Enabled := True;
  mnuTopologyOnlyURt.Enabled := True;
  mnuRadialUnrooted.Enabled := True;
  mnuRectangularRooted.Enabled := False;
  lblRoot.Enabled := True;
  txtSelectedRoot.Enabled := True;
  if txtSelectedRoot.Text <> '' then
    mnuNewickTO.Enabled := True
  else
    mnuNewickTO.Enabled := False;
//  mnuNewickTO.Enabled := False;
//  boolSavedRootState := mnuRootTreeOn.Checked;
  intRootCount := strlsRootList.Count;
//  mnuRootTreeOn.Checked := False;
//  mnuRootTreeOn.Enabled := False;;
//  mnuSetRoot.Enabled := False;
//  mnuNewickTO.Enabled := False;
  boolShowTree := True;
  intSaveHeight := Height;
  Height := 400;
  Refresh;
  Height := intSaveHeight;
  Refresh;
  Hide;
  Show;
end;

procedure TfrmTreeGraph.mnuRootTheTreeOnClick(Sender: TObject);
begin
  SetShowLabels(True);
  mnuTopologyOnlyURtClick(Sender);
//  if dlgSelectNewRoot.Execute(intRoot, intSplit) then
//  if intRoot > -1 then
  frmSelectNewRoot.Show;
{  if mnuRootTreeOn.Checked then
  begin
    cbxSetRoot.Enabled := True;
    if cbxSetRoot.Text <> '' then
    begin
      mnuSetRoot.Enabled := True;
      mnuTopologyOnlyURt.Enabled := True;
    end
    else
    begin
      mnuSetRoot.Enabled := False;
      mnuTopologyOnlyURt.Enabled := False;
    end;
  end
  else
  begin
    cbxSetRoot.Enabled := False;
    mnuSetRoot.Enabled := False;
    mnuTopologyOnlyURt.Enabled := False;
  end;
  Refresh;}
end;

procedure TfrmTreeGraph.RootSelected(Sender: TObject);
begin
  if boolComplete then
  begin
    txtSelectedRoot.Enabled := True;
    lblRoot.Enabled := True;
    if intRoot = strlsRootList.Count - 2 then
    begin
      txtSelectedRoot.Text := strlsRootList[intRoot];
      strNwckTrShow := strOrgNewick;
      ShowTextTreeView(strNwckTrShow, intRoot, 53);
    end
    else
    begin
//      if TestTableForSpeciesData.intRoot > -1 then
      txtSelectedRoot.Text := strlsRootList[intRoot];
      frmTreeListDown.Show;
      frmTreeListDown.trvwTextDownBased.Selected := nodSelectBranch(frmTreeListDown.trvwTextDownBased.Items, intRoot);
      frmTreeListDown.btnSetRootClick(Sender);
      strNwckTrShow := frmTreeListDown.strNwkRR;
      frmTreeListDown.Hide;
      frmTreeListDown.Close;
    end;
    mnuRootedTree.Visible := True;
    if intTreeType = 0 then
    begin
      mnuRectangularRooted.Enabled := True;
      mnuRectangularRootedClick(Sender);
    end
    else
    begin
      mnuNewickTO.Enabled := True;
      mnuNewickTOClick(Sender);
    end;
  end
  else
  begin
//    boolShowLabels := False;
    txtSelectedRoot.Text := '';
  end;
  if txtSelectedRoot.Text <> '' then
    if intTreeType = 1 then
      mnuRectangularRooted.Enabled := True
    else
      mnuNewickTO.Enabled := True
  else
  begin
    intRoot := -1;
    mnuRectangularRooted.Enabled := False;
    mnuNewickTO.Enabled := False;
  end;
  SetShowLabels(False);
end;

procedure TfrmTreeGraph.RootCancelled(Sender: TObject);
begin
  SetShowLabels(False);
//  boolShowTree := True;
  Hide;
  Show;
end;

procedure TfrmTreeGraph.mnuRootTreeOnClick(Sender: TObject);
begin
end;

procedure TfrmTreeGraph.mnuSetRootClick(Sender: TObject);
var
  strNewickFile: string;
//  intRoot: integer;
  boolFileAvailable: boolean;
begin
  intRoot := intFindShortLabel(txtSelectedRoot.Text);
  if intRoot > -1 then
  begin
    frmTreeListDown.Show;
    frmTreeListDown.trvwTextDownBased.Selected := nodSelectBranch(frmTreeListDown.trvwTextDownBased.Items, intRoot);
    frmTreeListDown.btnSetRootClick(Sender);
    strNwckTrShow := frmTreeListDown.strNwkRR;
//    frmTreeListDown.svdlgRerootedNewick.InitialDir := strCurrentDir;
    frmTreeListDown.Hide;
{    if frmTreeListDown.svdlgRerootedNewick.Execute then
    begin
      boolFileAvailable := True;
      strNewickFile := frmTreeListDown.svdlgRerootedNewick.FileName;
      if FileExists(strNewickFile) then
        if mrYes = MessageDlg('File: ' + strNewickFile + ' already exists do you want to replace it?', mtConfirmation, [mbYes, mbNo], 0) then
          try
            frmTreeListDown.memRerootedNwkFmt.Lines.SaveToFile(strNewickFile);
          except
            on E:exception do MessageDlg(E.Message + ' for file: ' + strNewickFile, mtWarning, [mbOK], 0);
          end
        else
          MessageDlg('Original file: ' + strNewickFile + ' unchanged.', mtWarning, [mbOK], 0)
      else
        try
          frmTreeListDown.memRerootedNwkFmt.Lines.SaveToFile(strNewickFile);
        except
          on E:exception do MessageDlg(E.Message + ' for file: ' + strNewickFile, mtWarning, [mbOK], 0);
        end;
    end
    else
      MessageDlg('Request to save file: ' + frmTreeListDown.svdlgRerootedNewick.FileName + ' cancelled.', mtWarning, [mbOK], 0);}
    frmTreeListDown.Close;
    CreateTopologyRerootedTree;
    mnuTopologyOnlyURt.Enabled := True;
    mnuNewickTOClick(Sender);
  end;
end;

procedure TfrmTreeGraph.FormDestroy(Sender: TObject);
begin
  strlsRootList.Free;
end;

procedure TfrmTreeGraph.FormDeactivate(Sender: TObject);
begin
//  if frmSelectNewRoot.Showing then
//    frmSelectNewRoot.Hide;
end;

procedure TfrmTreeGraph.FormHide(Sender: TObject);
begin

end;

procedure TfrmTreeGraph.pumnuCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmTreeGraph.pumnuPrintTreeClick(Sender: TObject);
begin
  frmPrintTree.Show;
//  PrintBracketTree(True, Sender);
end;

procedure TfrmTreeGraph.FormActivate(Sender: TObject);
var
  pntOrigin: TPoint;
begin
  if boolShowTree then
  begin
//    if strlsRootList.Count <> intRootCount then
//      InitialRootList;
    if not(boolOriginal) then
      if intRoot + 1 > intRootCount then
        intRoot := -1;
    if intRoot > -1 then
//      txtSelectedRoot.Text := strlsRootList[intRoot];
      strRootSelected := strlsRootList[intRoot];
    txtSelectedRoot.Text := strRootSelected;
    if strNwckFrmt <> strOrgNewick then
    begin
      strOrgNewick := strNwckFrmt;
      strNwckTrShow := strNwckFrmt;
      mnuRectangularURt.Enabled := True;
      mnuRectangularURtClick(Sender);
//      mnuTopologyOnlyURt.Enabled := False;
      mnuNewickTO.Enabled := False;
    end;
    scrbxTreeGraph.HorzScrollBar.Position := 0;
    scrbxTreeGraph.VertScrollBar.Position := 0;
    txtSBL.Text := FloatToStrF(dblSumLngth, ffFixed, 12, 6);
    with Origin do
    begin
      case prpGraphType of
        kRadialTree: begin
//          pntOrigin.x := initWidth div 2;
//          pntOrigin.y := initHeight div 2;
//          txtDisplay.Text := 'Radial';
//          AssignRadialRange(pntOrigin, initWidth, initHeight);
//          imgTreeGraph.Width := Origin.ttoRadial.Width;
//          imgTreeGraph.Height := Origin.ttoRadial.Height;
        end;
        kBracketTree: begin
          pntOrigin.x := 100;
          AssignBracketRange(pntOrigin, initWidth, initHeight);
//          txtDisplay.Text := 'Rectangular';
//          imgTreeGraph.Width := Origin.ttoBracket.Width;
//          imgTreeGraph.Height := Origin.ttoBracket.Height;
        end;
        kTopologyOnlyTree: begin
          pntOrigin.x := kStrtBrcktTOX;
          pntOrigin.y := kStrtBrcktTOY;
//          txtDisplay.Text := 'Topology Only';
          AssignTplgyOnlyRange(pntOrigin, initWidth, initHeight);
//          imgTreeGraph.Width := Origin.ttoTplgyOnly.Width;
//          imgTreeGraph.Height := Origin.ttoTplgyOnly.Height;
        end;
        kNwckBrcktTree:begin
          pntOrigin.x := 100;
          AssignNwckBrcktRange(pntOrigin, initWidth, initHeight);
        end;
        kNewickTOTree: begin
          pntOrigin.x := kStrtBrcktTOX;
          pntOrigin.y := kStrtBrcktTOY;
          AssignNewickTORange(pntOrigin, initWidth, initHeight);
        end;
      end;
      intPixelFactor := intMaxPixelFactor(prpGraphType, dblScale);
    end;
    with TestTableForSpeciesData do
    begin
      sngIntrrOffset := kFullCircle / (NodeCount - SpeciesCount + 1);
      sngExtrrOffset := kFullCircle / SpeciesCount;
      sngHalfAngle := sngExtrrOffset / 2.0;
      sngHalfInterior := sngIntrrOffset / 2.0;
    end;
    CreateTopologyOnlyTree;
    CreateRadialGraph;
    CreateBracketTree(HomeVrtcl, intScale, dblScale);
    ShowTextTreeView(strNwckTrShow, intRoot, TestTableForSpeciesData.intSplit);
//    frmAbout.Show;
//      frmAbout.FormActivate(nil);
{    with frmDPhyIoM, TestTableForSpeciesData do
    begin
      txtZeroLength.Text := IntToStr(Species[NodeCount].VrtclPosition.intVrtclLngth);
      txtZeroSpace.Text := IntToStr(Species[NodeCount].VrtclPosition.intVrtclSpc);
      txtHomeLength.Text := IntToStr(HomeVrtcl.intVrtclLngth);
      txtHomeSpace.Text := IntToStr(HomeVrtcl.intVrtclSpc);
    end;}
//    Bitmap := nil;
    if frmSelectNewRoot.Showing then
      frmSelectNewRoot.Show;
  end;
end;

procedure TfrmTreeGraph.btnShowRadialTreeClick(Sender: TObject);
begin
  prpGraphType := kRadialTree;
  imgTreeGraph.Width := Origin.ttoRadial.Width + 200;
  imgTreeGraph.Height := Origin.ttoRadial.Height + 200;
  if Origin.ttoRadial.Width > scrbxTreeGraph.Width - {trvwTextTreeTG.Left - chtTreeGraph.Left - }20 then
  begin
    scrbxTreeGraph.HorzScrollBar.Range := Origin.ttoRadial.Width + 20;
//    chtTreeGraph.Width := HorzScrollBar.Range - 20;
  end
  else
  begin
//    chtTreeGraph.Width := frmTreeGraph.Width - 20;
    scrbxTreeGraph.HorzScrollBar.Range := 0;
  end;
  if scrbxTreeGraph.HorzScrollBar.Range = 0 then
    if scrbxTreeGraph.HorzScrollBar.Position > scrbxTreeGraph.Width then
      scrbxTreeGraph.HorzScrollBar.Position := 0;
  if Origin.ttoRadial.Height > scrbxTreeGraph.Height - {trvwTextTreeTG.Top - chtTreeGraph.Top - }20 then
  begin
    scrbxTreeGraph.VertScrollBar.Range := Origin.ttoRadial.Height + 20;
//    chtTreeGraph.Height := VertScrollBar.Range - 20;
  end
  else
  begin
    scrbxTreeGraph.VertScrollBar.Range := 0;
//    chtTreeGraph.Height := frmTreeGraph.Height - 20;
  end;
  if scrbxTreeGraph.VertScrollBar.Range = 0 then
    if scrbxTreeGraph.VertScrollBar.Position > scrbxTreeGraph.Height then
      scrbxTreeGraph.VertScrollBar.Position := 0;
  boolShowTree := True;
  Hide;
//  Paint;
  Show;
end;

procedure TfrmTreeGraph.btpnlTreeGraphClick(Sender: TObject);
begin

end;

{procedure TfrmTreeGraph.cbxSetRootChange(Sender: TObject);
begin
  if cbxSetRoot.Text = '' then
    mnuSetRoot.Enabled := False
  else
    mnuSetRoot.Enabled := True;
end;}

{procedure TfrmTreeGraph.chkbxSetRootChange(Sender: TObject);
begin
  if chkbxSetRoot.Checked then
  begin
    cbxSetRoot.Enabled := True;
    if cbxSetRoot.Text <> '' then
      spbtnSetRoot.Enabled := True
    else
      spbtnSetRoot.Enabled := False;
  end
  else
  begin
    cbxSetRoot.Enabled := False;
    spbtnSetRoot.Enabled := False;
  end;
  Refresh;
end;}

procedure TfrmTreeGraph.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
//  strlsRootList.Clear;
  txtSelectedRoot.Clear;
  {cbxSetRoot.Clear;}
  TestTableForSpeciesData.intRoot := intRoot;
  boolShowTree := True;
  if frmSelectNewRoot.Showing then
    frmSelectNewRoot.Close;
//  boolFirstOpen := True;
end;

procedure TfrmTreeGraph.InitialRootList;
var
  inx: integer;
begin
  strlsRootList.Clear;
  with TestTableForSpeciesData do
    for inx := 0 to NodeCount do
      strlsRootList.Add(Species[inx].ShortLabel);
  intRootCount := strlsRootList.Count;
  intRoot := -1;
 // cbxSetRoot.Items.Assign(strlsRootList);
end;

procedure TfrmTreeGraph.PaintToCanvas(PTCanvas: TCanvas);
var
  intNdLvl,
  intLngth,
  intStartX,
  intStarty,
  intTxtWdth,
  inx: integer;
  GrphCntr: TPoint;
  Bitmap: TBitmap;

procedure DrawScale(const dblScl: double);
//var
//  intTextWidth: integer;
begin
  with PTCanvas do
  begin
    if dblScl > dblZero then
    begin
//      Font.Size := 20;
//      TextOut(400, 40, 'Tree: ' + TestTableForSpeciesData.SpeciesTitle);
      Font.Size := 10;
//      TextOut(20, 60, 'Scale:');
//      intTextWidth := TextWidth('Scale:');
      MoveTo(120, 65);
      LineTo(120, 75);
      MoveTo(120, 70);
      LineTo(155, 70); // + intScl, 30);
      MoveTo(155, 65);
      LineTo(155, 75);
//      MoveTo(intTextWidth + 20 + intScl div 2, 12);
//      TextOut(intTextWidth + 50, 52, FloatToStrF(dblScl, ffFixed, 8, 4)); //intScl div 2 + 20, 11, '5');
      if dblScl < 20.0 then
        if dblScl < 5.0 then
          TextOut(120, 50, FloatToStrF(dblScl * 2.0 / 5.0, ffFixed, 8, 6))
        else
          TextOut(120, 50, FloatToStrF(dblScl * 2.0 / 5.0, ffFixed, 8, 4))
      else
        TextOut(130, 50, FloatToStrF(dblScl * 2.0 / 5.0, ffFixed, 8, 0));
//      TextOut(90, 30, 'SBL=' + FloatToStrF(dblSumLngth, ffFixed, 8, 6));
    end
    else
    begin
//      Font.Size := 20;
//      TextOut(300, 40, 'Tree: ' + TestTableForSpeciesData.SpeciesTitle);
      Font.Size := 10;
    end;
  end;
end;

procedure DrawBracket(const VrtclPstn: TVrtclPosition);
begin
  with VrtclPstn, PTCanvas do
  begin
    MoveTo(BgnVrtcl);
    LineTo(EndVrtcl);
    LineTo(BtmHrzntl);
    MoveTo(BgnVrtcl);
    LineTo(TopHrzntl);
  end;
end;

procedure DrawLabel(const VrtclPstn: TVrtclPosition; const Segment: TSegment);
begin
  with TestTableForSpeciesData, VrtclPstn, Segment, PTCanvas do
  begin
    if Species[int1stXNd].VerticalMode = Leaf then
      TextOut(TopHrzntl.x + 1, TopHrzntl.y - 9, Species[Species[int1stXNd].EndSegment.int1stXNd].SpeciesName);
    if Species[int2ndXNd].VerticalMode = Leaf then
      TextOut(BtmHrzntl.x + 1, BtmHrzntl.y - 9, Species[Species[int2ndXNd].EndSegment.int1stXNd].SpeciesName);
  end;
end;

procedure DrawBracketLabel(const VrtclPstn: TVrtclPosition; const Segment: TSegment);
begin
  with VrtclPstn, Segment, PTCanvas do
    if int1stXNd <> int2ndXNd then
    begin
      DrawBracket(VrtclPstn);
      DrawLabel(VrtclPstn, Segment);
    end;
end;

begin
  Bitmap := TBitmap.Create;
  try
    case prpGraphType of
      kRadialTree: begin
        Bitmap.Width := Origin.ttoRadial.Width + 200;
        Bitmap.Height := Origin.ttoRadial.Height + 200;
        imgTreeGraph.Picture.Graphic := Bitmap;
        imgTreeGraph.Width := Origin.ttoRadial.Width;
        imgTreeGraph.Height := Origin.ttoRadial.Height;
        imgTreeGraph.Canvas.MoveTo(Origin.ttoRadial.GraphOrigin);
      end;
      kBracketTree: begin
        Bitmap.Width := Origin.ttoBracket.Width;
        Bitmap.Height := Origin.ttoBracket.Height;
        imgTreeGraph.Picture.Graphic := Bitmap;
        imgTreeGraph.Width := Origin.ttoBracket.Width;
        imgTreeGraph.Height := Origin.ttoBracket.Height;
        imgTreeGraph.Canvas.MoveTo(Origin.ttoBracket.GraphOrigin);
      end;
      kTopologyOnlyTree: begin
        Bitmap.Width := Origin.ttoTplgyOnly.Width;
        Bitmap.Height := Origin.ttoTplgyOnly.Height;
        imgTreeGraph.Picture.Graphic := Bitmap;
        imgTreeGraph.Width := Origin.ttoTplgyOnly.Width;
        imgTreeGraph.Height := Origin.ttoTplgyOnly.Height;
//        imgTreeGraph.Canvas.MoveTo(Origin.ttoTplgyOnly.GraphOrigin);
      end;
      kNwckBrcktTree: begin
        Bitmap.Width := Origin.ttoNwckBrckt.Width;
        Bitmap.Height := Origin.ttoNwckBrckt.Height;
        imgTreeGraph.Picture.Graphic := Bitmap;
        imgTreeGraph.Width := Origin.ttoNwckBrckt.Width;
        imgTreeGraph.Height := Origin.ttoNwckBrckt.Height;
//        imgTreeGraph.Canvas.MoveTo(Origin.ttoNwckBrckt.GraphOrigin);
      end;
      kNewickTOTree: begin
        Bitmap.Width := Origin.ttoNewickTO.Width;
        Bitmap.Height := Origin.ttoNewickTO.Height;
        imgTreeGraph.Picture.Graphic := Bitmap;
        imgTreeGraph.Width := Origin.ttoNewickTO.Width;
        imgTreeGraph.Height := Origin.ttoNewickTO.Height;
//        imgTreeGraph.Canvas.MoveTo(Origin.ttoNewickTO.GraphOrigin);
      end;
    end;
  finally
    Bitmap.Free;
  end;
  SetFormExtents(imgTreeGraph.Canvas);
  boolShowTree := False;
//  PTCanvas.Refresh;
  PTCanvas.Clear;
  PTCanvas.Pen.Color := clBlack;
  PTCanvas.Pen.Style := psSolid;
  PTCanvas.Pen.Width := 1;
  case wrdDisplayTree of
  //  if boolDisplayRadialTree then
    kRadialTree: with TestTableForSpeciesData, PTCanvas do
    begin
      DrawScale(dblScale);
      intStartX := Origin.ttoRadial.GraphOrigin.x;
      intStartY := Origin.ttoRadial.GraphOrigin.y;
      for inx := 0 to {SpeciesCount - 1}NodeCount do
        with Species[inx] do
        begin
          MoveTo(BeginSegment.pntRadial.x + intStartX, intStartY + BeginSegment.pntRadial.y);
          LineTo(BeginSegment.pntRadial.x + EndSegment.pntRadial.x + intStartX, intStartY + BeginSegment.pntRadial.y + EndSegment.pntRadial.y);
          if EndSegment.int1stXNd = EndSegment.int2ndXNd then
            if (dblAngle >= 270.0) and (dblAngle < 360.0) then
              TextOut(BeginSegment.pntRadial.x + EndSegment.pntRadial.x + intStartX + 3, intStartY + BeginSegment.pntRadial.y + EndSegment.pntRadial.y - 9, Species[inx].SpeciesName)
            else
              if (dblAngle >= 0.0) and (dblAngle < 90.0) then
                TextOut(BeginSegment.pntRadial.x + EndSegment.pntRadial.x + intStartX + 3, intStartY + BeginSegment.pntRadial.y + EndSegment.pntRadial.y - 9, Species[inx].SpeciesName)
              else
              begin
                intTxtWdth := TextWidth(Species[inx].SpeciesName);
                if (dblAngle >= 90.0) and (dblAngle < 180.0) then
                  TextOut(BeginSegment.pntRadial.x + EndSegment.pntRadial.x + intStartX - 3 - intTxtWdth {div 2}, intStartY + BeginSegment.pntRadial.y + EndSegment.pntRadial.y - 9, Species[inx].SpeciesName)
                else
                  TextOut(BeginSegment.pntRadial.x + EndSegment.pntRadial.x + intStartX - 3 - intTxtWdth {div 2}, intStartY + BeginSegment.pntRadial.y + EndSegment.pntRadial.y - 9, Species[inx].SpeciesName);
              end;
        end;
    end;
    kTopologyOnlyTree: with TestTableForSpeciesData, PTCanvas do
    begin
      DrawScale(dblZero);
      for inx := 0 to NodeCount do
        with Species[inx].TplgyOnly do
          if Drawn then
          begin
            if inx < SpeciesCount then
              TextOut(EndHrzntl.x + 1, EndHrzntl.y - 9, Species[inx].SpeciesName);
            if TopVrtcl.y <> BtmVrtcl.y then
            begin
              MoveTo(TopVrtcl.x, TopVrtcl.y);
              LineTo(BtmVrtcl.x, BtmVrtcl.y);
            end;
            MoveTo(BgnHrzntl.x, BgnHrzntl.y);
            LineTo(EndHrzntl.x, EndHrzntl.y);
            if boolBootStrap then
              if inx > SpeciesCount - 1 then
                TextOut(EndHrzntl.x + 3, EndHrzntl.y - 9, FloatToStrF(BSPercent[inx - SpeciesCount], ffFixed, 5, 0));
            if boolShowLabels and (inx < NodeCount) then
            begin
              {PTCanvas.}Font.Color := clRed;
              TextOut((BgnHrzntl.x + EndHrzntl.x) div 2, EndHrzntl.y - 17, Species[inx].ShortLabel);
              {PTCanvas.}Font.Color := clBlack;
            end;
            if inx = NodeCount then
            begin
              MoveTo(BgnHrzntl.x, BgnHrzntl.y);
              with Species[NodeCount - 1].TplgyOnly do
                LineTo(BgnHrzntl.x, BgnHrzntl.y);
            end;
          end;
    end;
//  else
    kBracketTree: with TestTableForSpeciesData, PTCanvas do
    begin
      DrawScale(dblScale);
      DrawBracket(HomeVrtcl);
      DrawBracketLabel(Species[NodeCount].VrtclPosition, Species[NodeCount].EndSegment);
      if boolBootStrap {and (not frmDPhyIoM.mnuBootStrap.Enabled)} then
        TextOut(Species[NodeCount].VrtclPosition.MidPoint.x + 3, Species[NodeCount].VrtclPosition.MidPoint.y - 9, FloatToStrF(BSPercent[NodeCount - SpeciesCount], ffFixed, 5, 0));
      DrawBracketLabel(Species[NodeCount - 1].VrtclPosition, Species[NodeCount - 1].EndSegment);
      if boolBootStrap {and (not frmDPhyIoM.mnuBootStrap.Enabled)} then
        TextOut(Species[NodeCount - 1].VrtclPosition.MidPoint.x + 3, Species[NodeCount - 1].VrtclPosition.MidPoint.y - 9, FloatToStrF(BSPercent[NodeCount - SpeciesCount - 1], ffFixed, 5, 0));
{      with ZeroVrtcl do
      begin
        int1stBegin := Species[NodeCount - 1].BeginSegment.int1stXNd;
        int2ndBegin := Species[NodeCount - 1].BeginSegment.int2ndXNd;
        MoveTo(BgnVrtcl.x, BgnVrtcl.y);
        LineTo(EndVrtcl.x, EndVrtcl.y);
        intLngth := Trunc(Species[int2ndBegin].BranchLength * intPixelFactor * kShortLengthFactor);
        LineTo(EndVrtcl.x + intLngth, EndVrtcl.y);
        if Species[int2ndBegin].EndSegment.int1stXNd = Species[int2ndBegin].EndSegment.int2ndXNd then
          TextOut(EndVrtcl.x + intLngth + 1, EndVrtcl.y - 5, Species[int2ndBegin].SpeciesName);
        MoveTo(BgnVrtcl.x, BgnVrtcl.y);
        intLngth := Trunc(Species[int1stBegin].BranchLength * intPixelFactor * kShortLengthFactor);
        LineTo(BgnVrtcl.x + intLngth, BgnVrtcl.y);
        if Species[int1stBegin].EndSegment.int1stXNd = Species[int1stBegin].EndSegment.int2ndXNd then
          TextOut(BgnVrtcl.x + intLngth + 1, BgnVrtcl.y - 5, Species[int1stBegin].SpeciesName);
      end;
      with ZeroVrtcl do
      begin
        MoveTo(BgnVrtcl.x, BgnVrtcl.y);
        LineTo(EndVrtcl.x, EndVrtcl.y);
        intLngth := Trunc(Species[int2ndBegin].BranchLength * intPixelFactor * kShortLengthFactor);
        LineTo(EndVrtcl.x + intLngth, EndVrtcl.y);
        if Species[int2ndBegin].EndSegment.int1stXNd = Species[int2ndBegin].EndSegment.int2ndXNd then
          TextOut(EndVrtcl.x + intLngth + 1, EndVrtcl.y - 5, Species[int2ndBegin].SpeciesName);
        MoveTo(BgnVrtcl.x, BgnVrtcl.y);
        intLngth := Trunc(Species[int1stBegin].BranchLength * intPixelFactor * kShortLengthFactor);
        LineTo(BgnVrtcl.x + intLngth, BgnVrtcl.y);
        if Species[int1stBegin].EndSegment.int1stXNd = Species[int1stBegin].EndSegment.int2ndXNd then
          TextOut(BgnVrtcl.x + intLngth + 1, BgnVrtcl.y - 5, Species[int1stBegin].SpeciesName);
      end;
      with Species[NodeCount - 1].VrtclPosition do
      begin
        MoveTo(BgnVrtcl.x, BgnVrtcl.y);
        LineTo(EndVrtcl.x, EndVrtcl.y);
        intLngth := Trunc(Species[Species[NodeCount - 1].EndSegment.int2ndXNd].BranchLength * intPixelFactor * kShortLengthFactor);
        LineTo(EndVrtcl.x + intLngth, EndVrtcl.y);
        if Species[Species[NodeCount - 1].EndSegment.int2ndXNd].EndSegment.int1stXNd = Species[Species[NodeCount - 1].EndSegment.int2ndXNd].EndSegment.int2ndXNd then
          TextOut(EndVrtcl.x + intLngth + 1, EndVrtcl.y - 5, Species[Species[NodeCount - 1].EndSegment.int2ndXNd].SpeciesName);
        MoveTo(BgnVrtcl.x, BgnVrtcl.y);
        intLngth := Trunc(Species[Species[NodeCount - 1].EndSegment.int1stXNd].BranchLength * intPixelFactor * kShortLengthFactor);
        LineTo(BgnVrtcl.x + intLngth, BgnVrtcl.y);
        if Species[Species[NodeCount - 1].EndSegment.int1stXNd].EndSegment.int1stXNd = Species[Species[NodeCount - 1].EndSegment.int1stXNd].EndSegment.int2ndXNd then
          TextOut(BgnVrtcl.x + intLngth + 1, BgnVrtcl.y - 5, Species[Species[NodeCount - 1].EndSegment.int1stXNd].SpeciesName);
      end;}
      intNdLvl := 1;
      while boolMoreNodeLevels(intNdLvl) do
      begin
        for inx := 0 to NodeCount - 2 do
          if Species[inx].NodeLevel = intNdLvl then
          begin
            DrawBracketLabel(Species[inx].VrtclPosition, Species[inx].EndSegment);
            if boolBootStrap {and (not frmDPhyIoM.mnuBootStrap.Enabled)} then
              if inx > SpeciesCount - 1 then
                TextOut(Species[inx].VrtclPosition.MidPoint.x + 3, Species[inx].VrtclPosition.MidPoint.y - 9, FloatToStrF(BSPercent[inx - SpeciesCount], ffFixed, 8, 0));
          end;
{            with Species[inx] do
              with VrtclPosition do
              begin
                MoveTo(BgnVrtcl.x, BgnVrtcl.y);
                LineTo(EndVrtcl.x, EndVrtcl.y);
                intLngth := Trunc(Species[Species[inx].EndSegment.int2ndXNd].BranchLength * intPixelFactor * kShortLengthFactor);
                LineTo(EndVrtcl.x + intLngth, EndVrtcl.y);
                if Species[Species[inx].EndSegment.int2ndXNd].EndSegment.int1stXNd = Species[Species[inx].EndSegment.int2ndXNd].EndSegment.int2ndXNd then
                  TextOut(EndVrtcl.x + intLngth + 1, EndVrtcl.y - 5, Species[Species[inx].EndSegment.int2ndXNd].SpeciesName);
                MoveTo(BgnVrtcl.x, BgnVrtcl.y);
                intLngth := Trunc(Species[Species[inx].EndSegment.int1stXNd].BranchLength * intPixelFactor * kShortLengthFactor);
                LineTo(BgnVrtcl.x + intLngth, BgnVrtcl.y);
                if Species[Species[inx].EndSegment.int1stXNd].EndSegment.int1stXNd = Species[Species[inx].EndSegment.int1stXNd].EndSegment.int2ndXNd then
                  TextOut(BgnVrtcl.x + intLngth + 1, BgnVrtcl.y - 5, Species[Species[inx].EndSegment.int1stXNd].SpeciesName);
              end;}
        inc(intNdLvl);
      end;
//      with chtTreeGraph do
//      begin
//        Canvas.Pen.Width:=3;
//        Canvas.Pen.Style:=psSolid;
//        Canvas.Pen.Color:=clRed;
//        Canvas.MoveTo(Left + 50, Top - 100);
//        Canvas.LineTo(Left + 250, Top - 100);
//      end;
    end;
    kNwckBrcktTree:  with PTCanvas do
    begin
      inx := Length(aRectTreePlot) - 1;
      DrawScale(dblScale);
//      while aTOTreePlot[inx].RSide.x < 50 do
//        dec(inx);
//      intLngth := Trunc(TestTableForSpeciesData.Species[intRoot].BranchLength * 4 * 1); //aRectTreePlot[aRectTreePlot[inx].intLC].intBrnchLngth * 175 div 100;
      while inx > -1 do
      begin
        MoveTo(aRectTreePlot[inx].LSide);
        LineTo(aRectTreePlot[inx].RSide);
        if aRectTreePlot[inx].strLabel <> '' then
          TextOut(aRectTreePlot[inx].RSide.x + 3, aRectTreePlot[inx].RSide.y - 9, aRectTreePlot[inx].strLabel); // + '->Inx:' + IntToStr(inx)) // + ', Prnt:' + IntToStr(aRectTreePlot[inx].intPrnt) + ', FC:' + IntToStr(aRectTreePlot[inx].intFC) + ', LC:' + IntToStr(aRectTreePlot[inx].intLC) + ', L:' + IntToStr(aRectTreePlot[inx].intBrnchLngth) + ', RS:(' + IntToStr(aRectTreePlot[inx].RSide.x) + ',' +IntToStr(aRectTreePlot[inx].RSide.y) + ')');
//        else
//          TextOut(aRectTreePlot[inx].RSide.x + 3, aRectTreePlot[inx].RSide.y - 9, 'Inx:' + IntToStr(inx) + ', Prnt:' + IntToStr(aRectTreePlot[inx].intPrnt) + ', FC:' + IntToStr(aRectTreePlot[inx].intFC) + ', LC:' + IntToStr(aRectTreePlot[inx].intLC) + ', L:' + IntToStr(aRectTreePlot[inx].intBrnchLngth) + ', RS:(' + IntToStr(aRectTreePlot[inx].RSide.x) + ',' +IntToStr(aRectTreePlot[inx].RSide.y) + ')');
      if aRectTreePlot[inx].intFC <> aRectTreePlot[inx].intLC then //(aRectTreePlot[inx].TpSide.x <> -1) and (aRectTreePlot[inx].BttmSide.x <> -1) then
        begin
          MoveTo(aRectTreePlot[inx].TpSide);
          LineTo(aRectTreePlot[inx].BttmSide);
        end;
        dec(inx);
      end;
//      LineTo(aRectTreePlot[aRectTreePlot[inx].intLC].RSide);
//      MoveTo(aRectTreePlot[aRectTreePlot[inx].intFC].LSide);
//      LineTo(aRectTreePlot[aRectTreePlot[inx].intFC].RSide);
//      intLngth := aRectTreePlot[inx].intLC;
//      intLngth := aRectTreePlot[inx].intFC;  //.intBrnchLngth;
//      intLngth := aRectTreePlot[aRectTreePlot[inx].intLC].intBrnchLngth;
//      intLngth := aRectTreePlot[inx - 2].intBrnchLngth;
//      intLngth := aRectTreePlot[4].intBrnchLngth;
    end;
    kNewickTOTree: with PTCanvas do
    begin
      DrawScale(dblZero);
      for inx := 0 to Length(aTOTreePlot) - 1 do
        if (aTOTreePlot[inx].LSide.x <> -1) and (aTOTreePlot[inx].LSide.y <> -1) then
          if (aTOTreePlot[inx].RSide.x <> -1) and (aTOTreePlot[inx].RSide.y <> -1) then
          begin
            MoveTo(aTOTreePlot[inx].LSide);
            LineTo(aTOTreePlot[inx].RSide);
            if aTOTreePlot[inx].strLabel <> '' then
              TextOut(aTOTreePlot[inx].RSide.x + 3, aTOTreePlot[inx].RSide.y - 9, aTOTreePlot[inx].strLabel);
          end;

      for inx := 0 to Length(aTOTreePlot) - 1 do
        if (aTOTreePlot[inx].TpSide.x <> -1) and (aTOTreePlot[inx].TpSide.y <> -1) then
          if (aTOTreePlot[inx].BttmSide.x <> -1) and (aTOTreePlot[inx].BttmSide.y <> -1) then
          begin
            MoveTo(aTOTreePlot[inx].TpSide);
            LineTo(aTOTreePlot[inx].BttmSide);
          end;
    end;
  end;
end;

procedure TfrmTreeGraph.PaintToFile(Sender: TObject; const strFileName: string);
begin
  try
    imgTreeGraph.Picture.Jpeg.SaveToFile(strFileName);
  except
    on E:exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);       // Send message to dialog box
  end;
  FormActivate(Sender);
end;

procedure TfrmTreeGraph.CreateBitMap(var bmpPctr: TBitMap; const intW, intH: integer);
var
  bmpCBM: TBitMap;
begin
  bmpCBM := TBitmap.Create;	{ construct the bitmap object }
  try
    bmpCBM.Width := intW;	{ assign the initial width... }
    bmpCBM.Height := intH;	{ ...and the initial height }
    bmpPctr := bmpCBM;   	{ assign the bitmap to the image control }
  finally
    bmpCBM.Free;                {We are done with the bitmap, so free it }
  end;
end;

end.

