// Main form module for PhyloM application.
// Author: Karl E. Terpstra
// Last Update: 09/20/2018
unit modphyiom;

{$mode objfpc}{$H+}

{
Program executable PhyloM.exe for published version.  Same excecutable with test version add
test parameter PhyloM.exe /Test in short cut or run command.  [Test] will appear in application
title at the end in test mode and just application title in published mode.

This program computes a pairwise distance matrix from a matrix of measurements or binary
data (0/1 or +/-) using the corresponding algorithm described in the paper. The user may
then infer the phylogeny of the taxa in the input file using the Neighbor-joining method
(Saitou and Nei, 1987), with the option of obtaining bootstrap support and rooting the tree
on an outgroup. The tree can be output as an image or text (Newick format) file. If the user
is interested in using the distance matrix itself (e.g., for use in another phylogenetics
program), it can also be saved as a .csv, .meg (for use with the software MEGA) or .nex (for
use with the software PAUP, Mesquite, etc.) file.
}
interface

uses
  Classes, SysUtils, FileUtil, RichMemo, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Menus, ComCtrls, Grids, ExtCtrls, Printers,
  modDataTypes, modTestData, modSelectType, modRunOptions, modTreeGraph,
  modTreeView, modCalcErrs, modAbout, modPrintTree, modSelectNewRoot,
  modInstructions, modFormBckgrd;

type

  { TfrmPhyIoM }

  TfrmPhyIoM = class(TForm)
    btnExit: TButton;
    btnSaveReplicateData: TButton;
    btnInteriorNodes: TButton;
    btnShowTreeView: TButton;
    btnSaveDMReplicate: TButton;
    cbxBootStrapPage: TComboBox;
    chkbxBootStrapPage: TCheckBox;
    cbxDMBootStrap: TComboBox;
    imgInstructions: TImage;
    lblBackground: TLabel;
    lblCiting: TLabel;
    memTreeTextView: TMemo;
    mnuCiting: TMenuItem;
    mnuDPhyIoM: TMainMenu;
    mnuNexusFormat: TMenuItem;
    mnuCSVFormat: TMenuItem;
    mnuAbout: TMenuItem;
    mnuSaveFastaFormat: TMenuItem;
    mnuSaveMegaFormat: TMenuItem;
    mnuShowCalcErrs: TMenuItem;
    pgctlDPhyIoM: TPageControl;
    svdlgInstructions: TSaveDialog;
    strgrdResult: TStringGrid;
    svdlgNexusFormat: TSaveDialog;
    svdlgFastaFormat: TSaveDialog;
    svdlgMegaFormat: TSaveDialog;
    tabDistanceMatrixGrid: TTabSheet;
    trvwTreeGraph: TTreeView;
    txtDataRows: TEdit;
    lblDataRows: TLabel;
    txtDataColumns: TEdit;
    lblDataColumns: TLabel;
    txtHiddenMatches: TEdit;
    lbHiddenMatches: TLabel;
    lblHiddenNodeName: TLabel;
    txtHiddenNodeName: TEdit;
    lblEndNode: TLabel;
    lblBeginNode: TLabel;
    txtNodeEnd: TEdit;
    txtBeginNode: TEdit;
    lblBootStrapPage: TLabel;
    mnuShowTreeGraph: TMenuItem;
    svdlgReplicateData: TSaveDialog;
    strgrdRplctRwDt: TStringGrid;
    tabReplicatesRawData: TTabSheet;
    txtSumBranchLengths: TEdit;
    lblBranchLengths: TLabel;
    lblSumBL: TLabel;
    lblHomeSpace: TLabel;
    lblHomeLength: TLabel;
    txtHomeSpace: TEdit;
    txtHomeLength: TEdit;
    lblZeroVetricalNode: TLabel;
    lblHomeVerticalNode: TLabel;
    lblHeapSpace: TLabel;
    txtUnusedHeapSpace: TEdit;
    lblHeapError: TLabel;
    txtHeapErrorCode: TEdit;
    lblReplicateEntries: TLabel;
    lblNewickFormat: TLabel;
    lblInteriorNodes: TLabel;
    mnuSaveNwckFrmt: TMenuItem;
    mmoNewickFormat: TMemo;
    stgrdReplicateEntries: TStringGrid;
    svdlgNwckFrmt: TSaveDialog;
    tabReplicates: TTabSheet;
    tabTreeTextView: TTabSheet;
    txtInteriorNodes: TEdit;
    lblDataTableProp: TLabel;
    lblNodeName: TLabel;
    txtNodeName: TEdit;
    txtSecondSpeciesLength: TEdit;
    txtFirstSpeciesLength: TEdit;
    lblDistance: TLabel;
    txtSecondSpeciesOfPairName: TEdit;
    lblSecondSpeciesOfPair: TLabel;
    lblFirstSpeciesOfPair: TLabel;
    mnuMakeTree: TMenuItem;
    txtFirstSpeciesOfPairName: TEdit;
    lblValue: TLabel;
    txtLowestValue: TEdit;
    edtFileAction: TEdit;
    edtActiveFile: TEdit;
    lblLowestPair: TLabel;
    lblFileAction: TLabel;
    lblCurrentFile: TLabel;
    mmoTestData: TMemo;
    mnuRun: TMenuItem;
    mnuAnalysis: TMenuItem;
    mnuTestTable: TMenuItem;
    mmoResults: TMemo;
    mnuClearDisplay: TMenuItem;
    mnuDisplayDstncMtrx: TMenuItem;
    mnuDisplayInstructions: TMenuItem;
    mnuDisplayData: TMenuItem;
    mnuDisplay: TMenuItem;
    mnuSaveDstncMtrx: TMenuItem;
    mnuSaveInstructions: TMenuItem;
    mnuExit: TMenuItem;
    mnuFile: TMenuItem;
    mnuOpen: TMenuItem;
    mnumodDPhyIoM: TMainMenu;
    opndlgTestData: TOpenDialog;
    prgbrFileAction: TProgressBar;
    strgrdDataTableProp: TStringGrid;
    strgrdDstncMatrix: TStringGrid;
    strgrdQMarix: TStringGrid;
    svdlgResultData: TSaveDialog;
    strgrdTestData: TStringGrid;
    tabResults: TTabSheet;
    tabInstructions: TTabSheet;
    tabCiteProgram: TTabSheet;
    tabDstncMatrix: TTabSheet;
    tabQMatrix: TTabSheet;
    tabMakeTree: TTabSheet;
    tabTestDataTable: TTabSheet;
    tabTestData: TTabSheet;
    procedure btnExitClick(Sender: TObject);
    procedure btnInteriorNodesClick(Sender: TObject);
    procedure btnSaveDMReplicateClick(Sender: TObject);
    procedure btnSaveReplicateDataClick(Sender: TObject);
    procedure btnShowTreeViewClick(Sender: TObject);
    procedure cbxBootStrapPageChange(Sender: TObject);
    procedure cbxDMBootStrapChange(Sender: TObject);
    procedure chkbxBootStrapPageChange(Sender: TObject);
    procedure ClearData(const boolNtExt: boolean);
    procedure ClearTextBoxes;
    procedure edtFileActionChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuCitingClick(Sender: TObject);
    procedure mnuNexusFormatClick(Sender: TObject);
    procedure mnuSaveFastaFormatClick(Sender: TObject);
    procedure mnuShowTreeGraphClick(Sender: TObject);
    procedure mnuClearDisplayClick(Sender: TObject);
    procedure mnuDisplayDataClick(Sender: TObject);
    procedure mnuDisplayInstructionsClick(Sender: TObject);
    procedure mnuDisplayDstncMtrxClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuMakeTreeClick(Sender: TObject);
    procedure mnuOpenClick(Sender: TObject);
    procedure mnuRunClick(Sender: TObject);
    procedure mnuSaveInstructionsClick(Sender: TObject);
    procedure mnuSaveMegaFormatClick(Sender: TObject);
    procedure mnuSaveNwckFrmtClick(Sender: TObject);
    procedure mnuCSVFormatClick(Sender: TObject);
    procedure mnuShowCalcErrsClick(Sender: TObject);
    procedure mnuTestTableClick(Sender: TObject);
    procedure pgctlDPhyIoMChange(Sender: TObject);
    procedure RichMemo1Change(Sender: TObject);
    procedure tabResultsHide(Sender: TObject);
    procedure tabResultsShow(Sender: TObject);
    procedure trvwTreeGraphClick(Sender: TObject);
  private
    { private declarations }
    intTop1stLevel,
    intTop2ndLevel,
    intBttmTxtBxWidth,
    intTopDstMtrxLbl,
    intBtStrpPgTop,
    intNwckFrmtLblTop,
    intlblNwckFrmtTop,
    intmmNwckFrmtTop,
    intTtlLblsLeft,
    intBoxLblsLeft,
    intTextBoxLeft,
    intIntrrNdsLblLeft,
    intIntrrNdsTxtLeft,
    intExitBtnLeft,
    intFlActnPrgBrLeft,
    intmmTrTxtVwWidth,
    intmmTrTxtVwHeight,
    intmmNwckFrmtWidth,
    intTrTxtVwTabWidth,
    intTrTxtVwTabHeight,
    intsgDtTblPrpWidth,
    intsgDtTblPrpHeight,
    intMakeTreeTabWidth,
    intMakeTreeTabHeight,
    intsgRplctRwDtWidth,
    intsgRplctRwDtHeight,
    intRplctsRwDtTabWidth,
    intRplctsRwDtTabHeight,
    intsgReplicatesWidth,
    intsgReplicatesHeight,
    intReplicatesTabWidth,
    intReplicatesTabHeight,
    intsgDstncMtrxWidth,
    intsgDstncMtrxHeight,
    intDstncMtrxTabWidth,
    intDstncMtrxTabHeight,
    intsgQMatrixWidth,
    intsgQMatrixHeight,
    intQMatrixTabWidth,
    intQMatrixTabHeight,
    intsgResultWidth,
    intsgResultHeight,
    intDstncMtrxWidth,
    intDstncMtrxHeight,
    intsgDataWidth,
    intsgDataHeight,
    intDataTabWidth,
    intDataTabHeight,
    intFrameWidth,
    intFrameTop,
    intPageWidth,
    intPageHeight,
    intFormWidth,
    intFormHeight: integer;
    hsApplication: THeapStatus;
    procedure InitialDataTableProp;
    procedure SetRunMode(const boolRunState: boolean);
  public
    { public declarations }
    boolPublished,
    boolTestRun: boolean;
  published
    property prpRunMode: boolean read boolTestRun write SetRunMode;
  end;

var
  frmPhyIoM: TfrmPhyIoM;
  strSaveCurrentDir,
  strSaveFileName,
  strFileName: string;

implementation

{$R *.lfm}

{ TfrmPhyIoM }

var
  inxarDstcn: TDistanceIndex;
  strDataType,
  strErrMssg: string;
  intReplicates: integer;

{Secondary exit}
procedure TfrmPhyIoM.btnExitClick(Sender: TObject);
begin
  Close;
end;

{
If boolRunState is true then the application is test mode.  If boolRunState is false
the application is in production or published mode.
}
procedure TfrmPhyIoM.SetRunMode(const boolRunState: boolean);
begin
  boolTestRun := boolRunState;
end;

procedure TfrmPhyIoM.btnInteriorNodesClick(Sender: TObject);
begin
  svdlgResultData.InitialDir := strSaveCurrentDir;
  svdlgResultData.FileName := 'NodeList.csv';
  if svdlgResultData.Execute then
  begin
    strSaveFileName := svdlgResultData.FileName;
    if FileExists(strSaveFileName) then
      if mrYes = MessageDlg('File: ' + strSaveFileName + ' already exists do you want to replace it?', mtConfirmation, [mbYes, mbNo], 0) then
        MakeInteriorNodeFile(strSaveFileName)
      else
        MessageDlg('Request to save file: ' + strSaveFileName + ' cancelled.', mtWarning, [mbOK], 0)
    else
      MakeInteriorNodeFile(strSaveFileName);
  end;
end;

procedure TfrmPhyIoM.btnSaveDMReplicateClick(Sender: TObject);
var
  strSvFlNm: string;
begin
  strSvFlNm := '';
  svdlgReplicateData.InitialDir := strSaveCurrentDir;
  if svdlgReplicateData.Execute then
  begin
    strSvFlNm := svdlgReplicateData.FileName;
    if FileExists(strSvFlNm) then
      if mrYes = MessageDlg('File: ' + strSvFlNm + ' already exists do you want to replace it?', mtConfirmation, [mbYes, mbNo], 0) then
        SaveDMReplicateFile(strSvFlNm, cbxDMBootStrap.ItemIndex)
      else
        MessageDlg('Request to save distance matrix file: ' + strSvFlNm + ' cancelled.', mtWarning, [mbOK], 0)
    else
      SaveDMReplicateFile(strSvFlNm, cbxDMBootStrap.ItemIndex);
  end;
end;

procedure TfrmPhyIoM.btnSaveReplicateDataClick(Sender: TObject);
var
  strSvFlNm: string;
begin
  strSvFlNm := '';
  svdlgReplicateData.InitialDir := strSaveCurrentDir;
  if svdlgReplicateData.Execute then
  begin
    strSvFlNm := svdlgReplicateData.FileName;
    if FileExists(strSvFlNm) then
      if mrYes = MessageDlg('File: ' + strSvFlNm + ' already exists do you want to replace it?', mtConfirmation, [mbYes, mbNo], 0) then
        SaveReplicateDataFile(strSvFlNm, cbxBootStrapPage.ItemIndex)
      else
        MessageDlg('Request to save file: ' + strSvFlNm + ' cancelled.', mtWarning, [mbOK], 0)
    else
      SaveReplicateDataFile(strSvFlNm, cbxBootStrapPage.ItemIndex);
  end;
end;

procedure TfrmPhyIoM.btnShowTreeViewClick(Sender: TObject);
begin
  frmTreeListDown.Show;
end;

procedure TfrmPhyIoM.cbxBootStrapPageChange(Sender: TObject);
begin
  case TestTableForSpeciesData.wrdDataType of
    0..1: DisplayBootStrapPageBnry(cbxBootStrapPage.ItemIndex);
    2: DisplayBootStrapPageMsrmnt(cbxBootStrapPage.ItemIndex);
    3..4: DisplayBootStrapPageSqnc(cbxBootStrapPage.ItemIndex);
  end;
end;

procedure TfrmPhyIoM.cbxDMBootStrapChange(Sender: TObject);
begin
  BuildBSFullDMatrix(cbxDMBootStrap.ItemIndex);
end;

procedure TfrmPhyIoM.chkbxBootStrapPageChange(Sender: TObject);
begin
  if chkbxBootStrapPage.Checked then
  begin
    cbxDMBootStrap.Enabled := True;
    btnSaveDMReplicate.Enabled := True;       // Enable save data form Replicate Distance matrix page
    if cbxDMBootStrap.ItemIndex < 0 then
      cbxDMBootStrap.ItemIndex := 0;
    BuildBSFullDMatrix(cbxDMBootStrap.ItemIndex);
  end
  else
  begin
    cbxDMBootStrap.Enabled := False;
    btnSaveDMReplicate.Enabled := False;      // Disable save data form Replicate Distance matrix page
    BuildFullDMatrix;
  end;
end;

procedure TfrmPhyIoM.FormCreate(Sender: TObject);
begin
//  strCurrentDir := 'C:\';  // Start out in root directory
  boolPublished := False;
  strCurrentDir := Application.Location;  // Start out in application directory
  if Application.ParamCount > 0 then
    if UpperCase(Application.Params[1]) = '/TEST' then
      SetRunMode(True)
    else
      SetRunMode(False)
  else
    SetRunMode(False);
  if boolTestRun or boolPublished then
  begin
    if boolTestRun then
      Caption := Caption + ' [Test]';
  end;
  tabCiteProgram.TabVisible := True;
  lblCiting.Visible := True;
  strSaveCurrentDir := strCurrentDir;
  intReplicates := kReplicates;
  boolGraphInitial := True;
  TestTableForSpeciesData.intRoot := -1; // Unselect root
  TestTableForSpeciesData.intSplit := 50; // Initial root at 50% split branch lengths
  TestTableForSpeciesData.boolNewCalculation := True;
  TestTableForSpeciesData.wrdDataType := wrdNoType;
  strlstCalcErrs := TStringList.Create;
  stlsRecTree := TStringList.Create;
  strlstArrayErrs := TStringList.Create;
  strlstDstncMtrx := TStringList.Create;
  memTreeTextView.Font.Name := 'Terminal';
  memTreeTextView.Font.Bold := True;
  memTreeTextView.Font.Size := 9;
  astrTreePlot[Undrawn] := strRepeat(' ', 4);
  astrTreePlot[LeafEnd] := strRepeat(kHorzDash, 4);
  astrTreePlot[UpprCrnr] := astrTreePlot[Undrawn] + kAsterick;
  astrTreePlot[LwrCrnr] := astrTreePlot[Undrawn] + kAsterick;
  astrTreePlot[LftTee] := astrTreePlot[LeafEnd] + kVertDash;
  astrTreePlot[CvrLine] := astrTreePlot[Undrawn] + kVertDash;
  astrTreePlot[ExtLine] := strRepeat(kHorzDash, 5);
  astrTreePlot[Spaces] := strRepeat(' ', 5);
  astrTreePlot[RootTee] := strRepeat(' ', 2) + strRepeat(kHorzDash, 2) + kVertDash;
  intTop1stLevel := lblCurrentFile.Top;
  intTop2ndLevel := lblFileAction.Top;
  intBttmTxtBxWidth := edtFileAction.Width;
  intIntrrNdsLblLeft := lblInteriorNodes.Left;
  intIntrrNdsTxtLeft := txtInteriorNodes.Left;
  intExitBtnLeft := btnExit.Left;
  intFlActnPrgBrLeft := prgbrFileAction.Left;
  intDataTabWidth := tabTestData.Width;
  intDataTabHeight := tabTestData.Height;
  intFormWidth :=  frmPhyIoM.Width;
  intFormHeight :=  frmPhyIoM.Height;
  intPageWidth := pgctlDPhyIoM.Width;
  intPageHeight := pgctlDPhyIoM.Height;
  intsgDataWidth := strgrdTestData.Width;
  intsgDataHeight := strgrdTestData.Height;
  intDstncMtrxWidth := tabDistanceMatrixGrid.Width;
  intDstncMtrxHeight := tabDistanceMatrixGrid.Height;
  intsgResultWidth := strgrdResult.Width;
  intsgResultHeight := strgrdResult.Height;
  intQMatrixTabWidth := tabQMatrix.Width;
  intQMatrixTabHeight := tabQMatrix.Height;
  intsgQMatrixWidth := strgrdQMarix.Width;
  intsgQMatrixHeight := strgrdQMarix.Height;
  intDstncMtrxTabWidth := tabDstncMatrix.Width;
  intDstncMtrxTabHeight := tabDstncMatrix.Height;
  intTopDstMtrxLbl := lblDataColumns.Top;
  intsgDstncMtrxWidth := strgrdDstncMatrix.Width;
  intsgDstncMtrxHeight := strgrdDstncMatrix.Height;
  intReplicatesTabWidth := tabReplicates.Width;
  intReplicatesTabHeight := tabReplicates.Height;
  intsgReplicatesWidth := stgrdReplicateEntries.Width;
  intsgReplicatesHeight := stgrdReplicateEntries.Height;
  intRplctsRwDtTabWidth := tabReplicatesRawData.Width;
  intRplctsRwDtTabHeight := tabReplicatesRawData.Height;
  intBtStrpPgTop := lblBootStrapPage.Top;
  intsgRplctRwDtWidth := strgrdRplctRwDt.Width;
  intsgRplctRwDtHeight := strgrdRplctRwDt.Height;
  intMakeTreeTabWidth := tabMakeTree.Width;
  intMakeTreeTabHeight := tabMakeTree.Height;
  intsgDtTblPrpWidth := strgrdDataTableProp.Width;
  intsgDtTblPrpHeight := strgrdDataTableProp.Height;
  intTrTxtVwTabWidth := tabTreeTextView.Width;
  intTrTxtVwTabHeight := tabTreeTextView.Height;
  intNwckFrmtLblTop := lblNewickFormat.Top;
  intmmNwckFrmtTop := mmoNewickFormat.Top;
  intmmNwckFrmtWidth := mmoNewickFormat.Width;
  intTtlLblsLeft := lblZeroVetricalNode.Left;
  intBoxLblsLeft := lblHeapError.Left;
  intTextBoxLeft := txtHeapErrorCode.Left;
  intmmTrTxtVwWidth := memTreeTextView.Width;
  intmmTrTxtVwHeight := memTreeTextView.Height;
  intFrameWidth := lblBackground.Left;
  intFrameTop := lblBackground.Top;
end;

procedure TfrmPhyIoM.FormResize(Sender: TObject);
var
  intOffsetX,
  intOffsetY: integer;
begin
  if frmPhyIoM.Width > intFormWidth then
  begin
    intOffsetX := frmPhyIoM.Width - intFormWidth;
    lblBackground.Left := lblBackground.Left + intOffsetX div 2;
    edtActiveFile.Width := edtActiveFile.Width + intOffsetX;
    edtFileAction.Width := edtFileAction.Width + intOffsetX;
    lblInteriorNodes.Left := lblInteriorNodes.Left + intOffsetX;
    txtInteriorNodes.Left := txtInteriorNodes.Left + intOffsetX;
    prgbrFileAction.Left := prgbrFileAction.Left + intOffsetX;
    btnExit.Left := btnExit.Left + intOffsetX;
    pgctlDPhyIoM.Width := pgctlDPhyIoM.Width + intOffsetX;
    tabTestData.Width := tabTestData.Width + intOffsetX;
    strgrdTestData.Width := strgrdTestData.Width + intOffsetX;
    tabDistanceMatrixGrid.Width := tabDistanceMatrixGrid.Width + intOffsetX;
    strgrdResult.Width := strgrdResult.Width + intOffsetX;
    tabQMatrix.Width := tabQMatrix.Width + intOffsetX;
    strgrdQMarix.Width := strgrdQMarix.Width + intOffsetX;
    tabDstncMatrix.Width := tabDstncMatrix.Width + intOffsetX;
    strgrdDstncMatrix.Width := strgrdDstncMatrix.Width + intOffsetX;
    tabReplicates.Width := tabReplicates.Width + intOffsetX;
    stgrdReplicateEntries.Width := stgrdReplicateEntries.Width + intOffsetX;
    tabReplicatesRawData.Width := tabReplicatesRawData.Width + intOffsetX;
    strgrdRplctRwDt.Width := strgrdRplctRwDt.Width + intOffsetX;
    tabMakeTree.Width := tabMakeTree.Width + intOffsetX;
    strgrdDataTableProp.Width := strgrdDataTableProp.Width + intOffsetX;
    tabTreeTextView.Width := tabTreeTextView.Width + intOffsetX;
    mmoNewickFormat.Width := mmoNewickFormat.Width + intOffsetX;
    lblZeroVetricalNode.Left := lblZeroVetricalNode.Left + intOffsetX;
    lblHomeVerticalNode.Left := lblHomeVerticalNode.Left + intOffsetX;
    lblBranchLengths.Left := lblBranchLengths.Left + intOffsetX;
    lblHeapError.Left := lblHeapError.Left + intOffsetX;
    lblHeapSpace.Left := lblHeapSpace.Left + intOffsetX;
    lblHomeLength.Left := lblHomeLength.Left + intOffsetX;
    lblHomeSpace.Left := lblHomeSpace.Left + intOffsetX;
    lblBeginNode.Left := lblBeginNode.Left + intOffsetX;
    lblEndNode.Left := lblEndNode.Left + intOffsetX;
    lblSumBL.Left := lblSumBL.Left + intOffsetX;
    txtHeapErrorCode.Left := txtHeapErrorCode.Left + intOffsetX;
    txtUnusedHeapSpace.Left := txtUnusedHeapSpace.Left + intOffsetX;
    txtHomeLength.Left := txtHomeLength.Left + intOffsetX;
    txtHomeSpace.Left := txtHomeSpace.Left + intOffsetX;
    txtBeginNode.Left := txtBeginNode.Left + intOffsetX;
    txtNodeEnd.Left := txtNodeEnd.Left + intOffsetX;
    txtSumBranchLengths.Left := txtSumBranchLengths.Left + intOffsetX;
    memTreeTextView.Width := memTreeTextView.Width + intOffsetX;
  end
  else
  begin
    lblBackground.Left := intFrameWidth;
    edtActiveFile.Width := intBttmTxtBxWidth;
    edtFileAction.Width := intBttmTxtBxWidth;
    lblInteriorNodes.Left := intIntrrNdsLblLeft;
    txtInteriorNodes.Left := intIntrrNdsTxtLeft;
    btnExit.Left := intExitBtnLeft;
    prgbrFileAction.Left := intFlActnPrgBrLeft;
    pgctlDPhyIoM.Width := intPageWidth;
    tabTestData.Width := intDataTabWidth;
    strgrdTestData.Width := intsgDataWidth;
    tabDistanceMatrixGrid.Width := intDstncMtrxWidth;
    strgrdResult.Width := intsgResultWidth;
    tabQMatrix.Width := intQMatrixTabWidth;
    strgrdQMarix.Width := intsgQMatrixWidth;
    tabDstncMatrix.Width := intDstncMtrxTabWidth;
    strgrdDstncMatrix.Width := intsgDstncMtrxWidth;
    tabReplicates.Width := intReplicatesTabWidth;
    stgrdReplicateEntries.Width := intsgReplicatesWidth;
    tabReplicatesRawData.Width := intRplctsRwDtTabWidth;
    strgrdRplctRwDt.Width := intsgRplctRwDtWidth;
    tabMakeTree.Width := intMakeTreeTabWidth;
    strgrdDataTableProp.Width := intsgDtTblPrpWidth;
    tabTreeTextView.Width := intTrTxtVwTabWidth;
    mmoNewickFormat.Width := intmmNwckFrmtWidth;
    lblZeroVetricalNode.Left := intTtlLblsLeft;
    lblHomeVerticalNode.Left := intTtlLblsLeft;
    lblBranchLengths.Left := intTtlLblsLeft;
    lblHeapError.Left := intBoxLblsLeft;
    lblHeapSpace.Left := intBoxLblsLeft;
    lblHomeLength.Left := intBoxLblsLeft;
    lblHomeSpace.Left := intBoxLblsLeft;
    lblBeginNode.Left := intBoxLblsLeft;
    lblEndNode.Left := intBoxLblsLeft;
    lblSumBL.Left := intBoxLblsLeft;
    txtHeapErrorCode.Left := intTextBoxLeft;
    txtUnusedHeapSpace.Left := intTextBoxLeft;
    txtHomeLength.Left := intTextBoxLeft;
    txtHomeSpace.Left := intTextBoxLeft;
    txtBeginNode.Left := intTextBoxLeft;
    txtNodeEnd.Left := intTextBoxLeft;
    txtSumBranchLengths.Left := intTextBoxLeft;
    memTreeTextView.Width := intmmTrTxtVwWidth;
  end;
  if frmPhyIoM.Height > intFormHeight then
  begin
    intOffsetY := frmPhyIoM.Height - intFormHeight;
    lblCurrentFile.Top := lblCurrentFile.Top + intOffsetY;
    edtActiveFile.Top := edtActiveFile.Top + intOffsetY;
    lblFileAction.Top := lblFileAction.Top + intOffsetY;
    edtFileAction.Top := edtFileAction.Top + intOffsetY;
    btnExit.Top := btnExit.Top + intOffsetY;
    lblInteriorNodes.Top := lblInteriorNodes.Top + intOffsetY;
    txtInteriorNodes.Top := txtInteriorNodes.Top + intOffsetY;
    prgbrFileAction.Top := prgbrFileAction.Top + intOffsetY;
    pgctlDPhyIoM.Height := pgctlDPhyIoM.Height + intOffsetY;
    tabTestData.Height := tabTestData.Height + intOffsetY;
    strgrdTestData.Height :=  strgrdTestData.Height + intOffsetY;
    tabDistanceMatrixGrid.Height := tabDistanceMatrixGrid.Height + intOffsetY;
    strgrdResult.Height := strgrdResult.Height + intOffsetY;
    tabQMatrix.Height := tabQMatrix.Height + intOffsetY;
    strgrdQMarix.Height := strgrdQMarix.Height + intOffsetY;
    tabDstncMatrix.Height := tabDstncMatrix.Height + intOffsetY;
    lblDataColumns.Top := lblDataColumns.Top + intOffsetY;
    txtDataColumns.Top := txtDataColumns.Top + intOffsetY;
    lblDataRows.Top := lblDataRows.Top + intOffsetY;
    txtDataRows.Top := txtDataRows.Top + intOffsetY;
    cbxDMBootStrap.Top := cbxDMBootStrap.Top + intOffsetY;
    chkbxBootStrapPage.Top := chkbxBootStrapPage.Top + intOffsetY;
    btnSaveDMReplicate.Top := btnSaveDMReplicate.Top + intOffsetY;
    strgrdDstncMatrix.Height := strgrdDstncMatrix.Height + intOffsetY;
    tabReplicates.Height := tabReplicates.Height + intOffsetY;
    stgrdReplicateEntries.Height := stgrdReplicateEntries.Height + intOffsetY;
    tabReplicatesRawData.Height := tabReplicatesRawData.Height + intOffsetY;
    lblBootStrapPage.Top := lblBootStrapPage.Top + intOffsetY;
    cbxBootStrapPage.Top := cbxBootStrapPage.Top + intOffsetY;
    btnSaveReplicateData.Top := btnSaveReplicateData.Top + intOffsetY;
    strgrdRplctRwDt.Height := strgrdRplctRwDt.Height + intOffsetY;
    tabMakeTree.Height := tabMakeTree.Height + intOffsetY;
    strgrdDataTableProp.Height := strgrdDataTableProp.Height + intOffsetY;
    tabTreeTextView.Height := tabTreeTextView.Height + intOffsetY;
    lblNewickFormat.Top := lblNewickFormat.Top + intOffsetY;
    mmoNewickFormat.Top := mmoNewickFormat.Top + intOffsetY;
    memTreeTextView.Height := memTreeTextView.Height + intOffsetY;
    lblBackground.Top := lblBackground.Top + intOffsetY div 2;
  end
  else
  begin
    lblCurrentFile.Top := intTop1stLevel;
    edtActiveFile.Top := intTop1stLevel;
    lblFileAction.Top := intTop2ndLevel;
    edtFileAction.Top := intTop2ndLevel;
    btnExit.Top :=  intTop1stLevel;
    lblInteriorNodes.Top :=  intTop1stLevel;
    txtInteriorNodes.Top :=  intTop1stLevel;
    prgbrFileAction.Top := intTop2ndLevel;
    pgctlDPhyIoM.Height := intPageHeight;
    tabTestData.Height := intDataTabHeight;
    strgrdTestData.Height := strgrdTestData.Height;
    tabDistanceMatrixGrid.Height := intDstncMtrxHeight;
    strgrdResult.Height := intsgResultHeight;
    tabQMatrix.Height := intQMatrixTabHeight;
    strgrdQMarix.Height := intsgQMatrixHeight;
    tabDstncMatrix.Height := intDstncMtrxTabHeight;
    lblDataColumns.Top := intTopDstMtrxLbl;
    txtDataColumns.Top := intTopDstMtrxLbl;
    lblDataRows.Top := intTopDstMtrxLbl;
    txtDataRows.Top := intTopDstMtrxLbl;
    cbxDMBootStrap.Top := intTopDstMtrxLbl;
    chkbxBootStrapPage.Top := intTopDstMtrxLbl;
    btnSaveDMReplicate.Top := intTopDstMtrxLbl;
    strgrdDstncMatrix.Height := intsgDstncMtrxHeight;
    tabReplicates.Height := intReplicatesTabHeight;
    stgrdReplicateEntries.Height := intsgReplicatesHeight;
    tabReplicatesRawData.Height := intRplctsRwDtTabHeight;
    lblBootStrapPage.Top := intBtStrpPgTop;
    cbxBootStrapPage.Top := intBtStrpPgTop;
    btnSaveReplicateData.Top := intBtStrpPgTop;
    strgrdRplctRwDt.Height := intsgRplctRwDtHeight;
    tabMakeTree.Height := intMakeTreeTabHeight;
    strgrdDataTableProp.Height := intsgDtTblPrpHeight;
    tabTreeTextView.Height := intTrTxtVwTabHeight;
    lblNewickFormat.Top := intNwckFrmtLblTop;
    mmoNewickFormat.Top := intmmNwckFrmtTop;
    memTreeTextView.Height := intmmTrTxtVwHeight;
    lblBackground.Top := intFrameTop;
  end;
end;

procedure TfrmPhyIoM.mnuAboutClick(Sender: TObject);
begin
  frmAbout.Show;
end;

procedure TfrmPhyIoM.mnuCitingClick(Sender: TObject);
begin
  if not pgctlDPhyIoM.Visible then
    pgctlDPhyIoM.Visible := True
  else
    if not tabTestDataTable.TabVisible then
      pgctlDPhyIoM.Visible := False;
  pgctlDPhyIoM.PageIndex := 11;
end;

procedure TfrmPhyIoM.mnuNexusFormatClick(Sender: TObject);
begin
  svdlgNexusFormat.InitialDir := strSaveCurrentDir;
  if svdlgNexusFormat.Execute then
  begin
    strSaveFileName := svdlgNexusFormat.FileName;
    if FileExists(strSaveFileName) then
      if mrYes = MessageDlg('File: ' + strSaveFileName + ' already exists do you want to replace it?', mtConfirmation, [mbYes, mbNo], 0) then
        try
          SaveNexusFormatFile(strSaveFileName);
        except
          on E:exception do MessageDlg(E.Message + ' for file: ' + strSaveFileName, mtWarning, [mbOK], 0);
        end
      else
        MessageDlg('Original file: ' + strSaveFileName + ' unchanged.', mtWarning, [mbOK], 0)
    else
      try
        SaveNexusFormatFile(strSaveFileName);
      except
        on E:exception do MessageDlg(E.Message + ' for file: ' + strSaveFileName, mtWarning, [mbOK], 0);
      end;
  end
  else
    MessageDlg('Request to save Nexus format file cancelled.', mtWarning, [mbOK], 0);
end;

procedure TfrmPhyIoM.mnuSaveFastaFormatClick(Sender: TObject);
begin
  svdlgFastaFormat.InitialDir := strSaveCurrentDir;
  if svdlgFastaFormat.Execute then
  begin
    strSaveFileName := svdlgFastaFormat.FileName;
    if FileExists(strSaveFileName) then
      if mrYes = MessageDlg('File: ' + strSaveFileName + ' already exists do you want to replace it?', mtConfirmation, [mbYes, mbNo], 0) then
        try
          SaveFastaFormatFile(strSaveFileName);
        except
          on E:exception do MessageDlg(E.Message + ' for file: ' + strSaveFileName, mtWarning, [mbOK], 0);
        end
      else
        MessageDlg('Original file: ' + strSaveFileName + ' unchanged.', mtWarning, [mbOK], 0)
    else
      try
        SaveFastaFormatFile(strSaveFileName);
      except
        on E:exception do MessageDlg(E.Message + ' for file: ' + strSaveFileName, mtWarning, [mbOK], 0);
      end;
  end
  else
    MessageDlg('Request to save Fasta format file cancelled.', mtWarning, [mbOK], 0);
end;

procedure TfrmPhyIoM.mnuShowTreeGraphClick(Sender: TObject);
//var
//  boolHideTree: boolean;
begin
  frmTreeGraph.Show;
{  boolHideTree := frmTreeGraph.Showing;
  if boolHideTree then
    frmTreeGraph.Hide;
  FindBranches(intReplicates, strErrMssg);
  if strErrMssg = '' then
  begin
    mnuShowTreeGraph.Enabled := False;       // Disable bootstrap calculation
    chkbxBootStrapPage.Enabled := True;
    txtHiddenMatches.Text := '[' + FloatToStrF(TestTableForSpeciesData.HiddenPercent, ffFixed, 6, 0) + ']';
    MakeNewickFormat(mmoNewickFormat);                          // Put Newick format in memo field
    edtFileAction.Text := edtFileAction.Text + '  ' + IntToStr(intReplicates) + ' Replicates.';
    MessageDlg('Bootstrap calculation...completed.', mtWarning, [mbOK], 0);
  end
  else
  begin
    edtFileAction.Text := edtFileAction.Text + '  ' + 'Bootstrap failed.';
    MessageDlg('Bootstrap calculation...failed.  ' + strErrMssg, mtWarning, [mbOK], 0);
  end;
  if boolHideTree then
    frmTreeGraph.Show;}
end;

procedure TfrmPhyIoM.mnuClearDisplayClick(Sender: TObject);
begin
  if mrYes = MessageDlg('Close Data/Analysis', 'Do you want to close current data/analysis?', mtConfirmation, [mbYes,mbNo], 0) then
    ClearData(kboolClear);
end;

procedure TfrmPhyIoM.mnuDisplayDataClick(Sender: TObject);
begin
  pgctlDPhyIoM.PageIndex := 0;
end;

procedure TfrmPhyIoM.mnuDisplayInstructionsClick(Sender: TObject);
begin
//  pgctlDPhyIoM.PageIndex := 10;
  frmInstructions.Show;
end;

procedure TfrmPhyIoM.mnuDisplayDstncMtrxClick(Sender: TObject);
begin
  pgctlDPhyIoM.PageIndex := 2;
end;

procedure TfrmPhyIoM.ClearData(const boolNtExt: boolean);
begin
  if mmoTestData.Lines.Count > 0 then
  begin
    mmoTestData.Clear;
    with strgrdTestData do
    begin
      Clear;
      RowCount := 2;
      ColCount := 2;
      FixedRows := 1;
      FixedCols := 1;
      Cells[0, 0] := '';
      Cells[0, 1] := '';
      Cells[1, 0] := '';
      Cells[1, 1] := '';
    end;
    with strgrdResult do
    begin
      Clear;
      RowCount := 2;
      FixedRows := 1;
      ColCount := 2;
      FixedCols := 1;
      Cells[0, 0] := '';
      Cells[0, 1] := '';
      Cells[1, 0] := '';
      Cells[1, 1] := '';
    end;
    with strgrdDstncMatrix do
    begin
      Clear;
      RowCount := 2;
      FixedRows := 1;
      ColCount := 2;
      FixedCols := 1;
      Cells[0, 0] := '';
      Cells[0, 1] := '';
      Cells[1, 0] := '';
      Cells[1, 1] := '';
    end;
    with strgrdQMarix do
    begin
      Clear;
      RowCount := 2;
      ColCount := 2;
      FixedRows := 1;
      FixedCols := 1;
      Cells[0, 0] := '';
      Cells[0, 1] := '';
      Cells[1, 0] := '';
      Cells[1, 1] := '';
    end;
    with stgrdReplicateEntries do
    begin
      Clear;
      RowCount := 2;
      ColCount := 2;
      FixedRows := 1;
      FixedCols := 1;
      Cells[0, 0] := '';
      Cells[0, 1] := '';
      Cells[1, 0] := '';
      Cells[1, 1] := '';
    end;
    with strgrdRplctRwDt do
    begin
      Clear;
      RowCount := 2;
      ColCount := 2;
      FixedRows := 1;
      FixedCols := 1;
      Cells[0, 0] := '';
      Cells[0, 1] := '';
      Cells[1, 0] := '';
      Cells[1, 1] := '';
    end;
    trvwTreeGraph.Items.Clear;
    mmoResults.Clear;
    mmoNewickFormat.Clear;
    with cbxBootStrapPage do
    begin
      Clear;
      Text := '0';
      Enabled := False;
    end;
    with chkbxBootStrapPage do
    begin
      Checked := False;
      Enabled := False;
    end;
    with cbxDMBootStrap do
    begin
      Clear;
      Text := '0';
      Enabled := False;
    end;
    tabTestData.TabVisible := False;           // Hide Test Data tab
    tabTestDataTable.TabVisible := False;      // Hide Test Data Table tab
    tabResults.TabVisible := False;            // Hide Results tab
    tabDistanceMatrixGrid.TabVisible := False; // Hide Results Text Grid tab
    tabDstncMatrix.TabVisible := False;        // Hide Distance Matrix tab
    tabQMatrix.TabVisible := False;            // Hide Q matrix tab
    tabReplicates.TabVisible := False;         // Hide Replicate Entry data tab
    tabReplicatesRawData.TabVisible := False;  // Hide Replicates Raw tab
    tabMakeTree.TabVisible := False;           // Hide Make Tree tab
    tabTreeTextView.TabVisible := False;       // Hide Tree Text View tab
    prgbrFileAction.Position := 0;             // Clear progress bar
    mnuOpen.Enabled := True;                   // Enable file open
    mnuDisplayData.Enabled := False;           // Disable display data
    mnuRun.Enabled := False;                   // Disable run analysis
    mnuTestTable.Enabled := False;             // Disable Test Data tab selection
    mnuDisplayDstncMtrx.Enabled := False;      // Disable Distance Matrix tab selection
    mnuSaveDstncMtrx.Enabled := False;         // Disable save Distance Matrix results for output
    mnuSaveMegaFormat.Enabled := False;        // Disable save Mega File format
    mnuSaveFastaFormat.Enabled := False;       // Disable save Fasta File format
    mnuMakeTree.Enabled := False;              // Disable make tree
    mnuShowTreeGraph.Enabled := False;         // Disable Show Tree Graph
    btnShowTreeView.Enabled := False;          // Disable Tree Graph
    lblInteriorNodes.Enabled := False;         // Disable interior nodes label
    txtInteriorNodes.Enabled := False;         // Disable interior nodes text box
    btnInteriorNodes.Enabled := False;         // Disable make interior node file
    btnSaveReplicateData.Enabled := False;     // Disable save data form Replicate page
    btnSaveDMReplicate.Enabled := False;       // Disable save data form Replicate Distance matrix page
    intReplicates := 100;                      // Rnadom replicates data tests
    mnuShowCalcErrs.Enabled := False;          // Disable Show Calc Erros menu item
    strlstCalcErrs.Clear;
    strlstArrayErrs.Clear;
    ClearBinariesMeasurements(boolNtExt);      // Clear all data items
    if boolNtExt then
      InitialDataTableProp;
  end;
  ClearTextBoxes;
  frmTreeGraph.boolComplete := False;
  if frmTreeGraph.Showing then
    frmTreeGraph.Close;
  if frmTreeListDown.Showing then
    frmTreeListDown.Close;
  txtHeapErrorCode.Text := IntToStr(intShowHeapStatus(hsApplication));
  txtUnusedHeapSpace.Text := IntToStr(hsApplication.Unused);
  pgctlDPhyIoM.Visible := False;
end;

procedure TfrmPhyIoM.ClearTextBoxes;
begin
  edtActiveFile.Clear;                         // Clear text boxes
  edtFileAction.Clear;
  txtLowestValue.Clear;
  txtFirstSpeciesOfPairName.Clear;
  txtFirstSpeciesLength.Clear;
  txtSecondSpeciesOfPairName.Clear;
  txtSecondSpeciesLength.Clear;
  txtNodeName.Clear;
  txtHeapErrorCode.Clear;
  txtUnusedHeapSpace.Clear;
  txtHomeLength.Clear;
  txtHomeSpace.Clear;
  txtSumBranchLengths.Clear;
  txtInteriorNodes.Clear;
  txtBeginNode.Clear;
  txtNodeEnd.Clear;
  txtHiddenNodeName.Clear;
  txtHiddenMatches.Clear;
  txtDataColumns.Clear;
  txtDataRows.Clear;
  mnuSaveNwckFrmt.Enabled := False;            // Disable Newick format file save menu item
  mnuClearDisplay.Enabled := False;            // Disable clear since all data is empty/
end;

procedure TfrmPhyIoM.edtFileActionChange(Sender: TObject);
begin

end;

procedure TfrmPhyIoM.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ClearData(kboolExit);
end;

procedure TfrmPhyIoM.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmPhyIoM.mnuMakeTreeClick(Sender: TObject);
var
  dblLV: double;
  intFirstSpecies,
  intSecondSpecies: integer;
begin
  dblLV := dblZero;
  txtInteriorNodes.Enabled := True;
  lblInteriorNodes.Enabled := True;
  MakeABranch(dblLV, dblSumLngth, inxarDstcn, intFirstSpecies, intSecondSpecies, strErrMssg, prgbrFileAction, txtInteriorNodes);
  if strErrMssg = '' then
    with TestTableForSpeciesData do
    begin
      tabMakeTree.TabVisible := True;
      tabTreeTextView.TabVisible := True;
      if dlgRunOptions.Execute(intReplicates) then
        if wrdDataType in [wrdBPM, wrdB01, wrdMeasurement, wrdSequence, wrdSequenceJC] then
          if TestTableForSpeciesData.boolBootStrap then
          begin
            AssignReplicates(intReplicates, stgrdReplicateEntries);
            CheckForReplicates(intReplicates, cbxBootStrapPage, cbxDMBootStrap);
            BuildBSDistanceMatrix(prgbrFileAction, intReplicates);
            if boolTestRun then
            begin
              btnSaveReplicateData.Enabled := True;      // Enable save data form Replicate page
              tabReplicates.TabVisible := True;
              tabReplicatesRawData.TabVisible := True;
            end;
            FindBranches(intReplicates, strErrMssg);
            if strErrMssg = '' then
            begin
              chkbxBootStrapPage.Enabled := True;
              txtHiddenMatches.Text := '[' + FloatToStrF(TestTableForSpeciesData.HiddenPercent, ffFixed, 6, 0) + ']';
              edtFileAction.Text := edtFileAction.Text + '  ' + IntToStr(intReplicates) + ' Replicates.';
//              MessageDlg('Bootstrap calculation...completed.', mtWarning, [mbOK], 0);
            end
            else
            begin
              edtFileAction.Text := edtFileAction.Text + '  ' + 'Bootstrap failed.';
              MessageDlg('Bootstrap calculation...failed.  ' + strErrMssg, mtWarning, [mbOK], 0);
            end;
//            Clear3DDoubleArray(darBtStrpRwDtMsrmnt);
//            Clear3DCharArray(carBinary);
//            Clear3DCharArray(carSequence);
          end
          else
        else
          MessageDlg('Invalid option selected by user.', mtWarning, [mbOK], 0)
      else
         MessageDlg('Bootstrap options selection has been cancelled by user.', mtWarning, [mbOK], 0);
      MakeNewickFormat(mmoNewickFormat);       // Put Newick format in memo field
      txtFirstSpeciesOfPairName.Text := Species[intFirstSpecies].SpeciesName;
      txtSecondSpeciesOfPairName.Text := Species[intSecondSpecies].SpeciesName;
      txtFirstSpeciesLength.Text := FloatToStrF(Species[intFirstSpecies].BranchLength, ffFixed, 10, 4);
      txtSecondSpeciesLength.Text := FloatToStrF(Species[intSecondSpecies].BranchLength, ffFixed, 10, 4);
      txtNodeName.Text := Species[NodeCount - 1].SpeciesName;
      txtBeginNode.Text := FloatToStrF(Species[NodeCount - 1].BranchLength, ffFixed, 12, 8);
      txtNodeEnd.Text := FloatToStrF(Species[NodeCount].BranchLength, ffFixed, 12, 8);
      txtSumBranchLengths.Text := FloatToStrF(dblSumLngth, ffFixed, 12, 8);
      txtHiddenNodeName.Text := Species[NodeCount].SpeciesName;  //HiddenNodeName; //
      ShowViewTreeViewValues;    // Display text grid tree
      ShowTextTreeView(strNwckFrmt, -1, 60);   // Show line text tree
      frmTreeGraph.Show;
      if not boolTestRun then
      begin
        tabMakeTree.TabVisible := False;
        tabTreeTextView.TabVisible := False;
      end;
    end
  else
  begin
    txtFirstSpeciesOfPairName.Text := 'Error';
    txtSecondSpeciesOfPairName.Text := strErrMssg;
    txtFirstSpeciesLength.Text := 'Error';
    txtSecondSpeciesLength.Text := 'Error';
  end;
  txtLowestValue.Text := FloatToStrF(dblLV, ffFixed, 9, 3);
  with TestTableForSpeciesData do
//  begin
//    lblInteriorNodes.Enabled := True;
//    txtInteriorNodes.Enabled := True;
    txtInteriorNodes.Text := IntToStr(NodeCount - SpeciesCount + 1);
//  end;
  mnuMakeTree.Enabled := False;                               // Disable make tree
  mnuSaveNwckFrmt.Enabled := True;                            // Enable Save Newick format
  btnShowTreeView.Enabled := True;                           // Enable Show Tree Graph button
  mnuShowTreeGraph.Enabled := True;                           // Enable Show Tree Graph menu item
  btnInteriorNodes.Enabled := True;                           // Enable make interior node file
  txtHeapErrorCode.Text := IntToStr(intShowHeapStatus(hsApplication));
  txtUnusedHeapSpace.Text := IntToStr(hsApplication.TotalFree);
end;

procedure TfrmPhyIoM.mnuOpenClick(Sender: TObject);
var
  strErrorMssg,
  strExtension: string;
  intDataErr,
  intTestCount,
  intSpeciesCount: integer;
  boolExecute,
  boolFileAvailable: boolean;
begin
  opndlgTestData.InitialDir := strCurrentDir;  // Select file to open for processing
  repeat
    strErrorMssg := strBlank;
    boolExecute := dlgSelectFileType.Execute(TestTableForSpeciesData.wrdDataType, strErrorMssg);
    if strErrorMssg <> strBlank then
      MessageDlg(strErrorMssg, mtWarning, [mbOK], 0);
  until boolExecute or (strErrorMssg = strBlank);
  if boolExecute then
  begin
    ClearData(True);                           // Clear all data items
    if TestTableForSpeciesData.wrdDataType in [wrdSequence, wrdSequenceJC] then
    begin
      opndlgTestData.DefaultExt := '.fas';
      opndlgTestData.FilterIndex := 2;
    end
    else
    begin
      opndlgTestData.DefaultExt := '.csv';
      opndlgTestData.FilterIndex := 1;
    end;
    if opndlgTestData.Execute then
    begin
      strFileName := opndlgTestData.FileName;
      edtActiveFile.Text := ExtractFileName(strFileName);
      if FileExists(strFileName) then
      begin
        pgctlDPhyIoM.Visible := True;
        boolFileAvailable := True;
        try
          mmoTestData.Lines.LoadFromFile(strFileName);
        except
          on E:exception do
          begin
            MessageDlg(E.Message + ' for file ' + strFileName + '.', mtWarning, [mbOK], 0);
            edtFileAction.Text := 'File not available to open.';
            boolFileAvailable := False;
          end;
        end;
        if boolFileAvailable then
        begin
          case TestTableForSpeciesData.wrdDataType of  // Select data type processing
            0: begin
              strDataType := 'Binary - or + data';
              LoadBinaryData(strFileName, prgbrFileAction, strgrdTestData, karryMinusPlus);
              intTestCount := TestTableForSpeciesData.TestCount;
              intDataErr := TestTableForSpeciesData.ErrorCount;
              intSpeciesCount := TestTableForSpeciesData.SpeciesCount;
              if intDataErr > 0 then
                MessageDlg('Invalid Data', IntToStr(intDataErr) + ' Data error(s) encountered.', mtWarning, [mbOK], 0);
            end;
            1: begin
              strDataType := 'Binary 0 or 1 data';
              LoadBinaryData(strFileName, prgbrFileAction, strgrdTestData, karryZeroOne);
              intTestCount := TestTableForSpeciesData.TestCount;
              intDataErr := TestTableForSpeciesData.ErrorCount;
              intSpeciesCount := TestTableForSpeciesData.SpeciesCount;
              if intDataErr > 0 then
                MessageDlg('Invalid Data', IntToStr(intDataErr) + ' Data error(s) encountered.', mtWarning, [mbOK], 0);
            end;
            2: begin
              strDataType := 'Measurement data';
              LoadMeasuremntData(strFileName, prgbrFileAction, strgrdTestData);
              intTestCount := TestTableForSpeciesData.TestCount;
              intDataErr := TestTableForSpeciesData.ErrorCount;
              intSpeciesCount := TestTableForSpeciesData.SpeciesCount;
              if intDataErr > 0 then
                MessageDlg('Invalid Data', IntToStr(intDataErr) + ' Non-numerical data error(s) encountered.', mtWarning, [mbOK], 0);
            end;
            3..4: begin
              strDataType := 'Sequence data';
              if TestTableForSpeciesData.wrdDataType = wrdSequenceJC  then
                strDataType := strDataType + ' JC';
              strExtension := ExtractFileExt(strFileName);
              if strExtension = '.csv' then
                LoadSequenceData(strFileName, prgbrFileAction, strgrdTestData)
              else
                if strExtension = '.fas' then
                  LoadFastaSequenceData(strFileName, prgbrFileAction, strgrdTestData)
                else
                begin
                  edtFileAction.Text := 'Invalid file extension.';
                  MessageDlg('Invalid file extension ', strExtension + ' encountered.', mtWarning, [mbOK], 0);
                  exit;
                end;
              intTestCount := TestTableForSpeciesData.TestCount;
              intDataErr := TestTableForSpeciesData.ErrorCount;
              intSpeciesCount := TestTableForSpeciesData.SpeciesCount;
              if intDataErr > 0 then
                MessageDlg('Invalid Data', IntToStr(intDataErr) + ' Non-numerical data error(s) encountered.', mtWarning, [mbOK], 0)
              else
                if strExtension <> '.fas' then
                  mnuSaveFastaFormat.Enabled := True;    // Enable save Fasta format
            end;
          else
            edtFileAction.Text := 'Unknown';
            MessageDlg('Invalid Type', 'Unknown type.', mtWarning, [mbOK], 0);
            exit;
          end;
          if intDataErr > 0 then
          begin
            pgctlDPhyIoM.PageIndex := 1;
            if (rctSelectedCell.Left = 1) and (rctSelectedCell.Top = 1) then
            begin
              rctSelectedCell.Right := 2;
              rctSelectedCell.Bottom := 2;
              strgrdTestData.Selection := rctSelectedCell;
              rctSelectedCell.Right := 1;
              rctSelectedCell.Bottom := 1;
            end;
            strgrdTestData.Selection := rctSelectedCell;
          end
          else
          begin
            if TestTableForSpeciesData.SpeciesCount < 4 then
            begin
              edtFileAction.Text := 'Invalid data file selected.  ' + IntToStr(intTestCount) + ' Tests.  ' + IntToStr(intSpeciesCount) + ' records to process.  Insufficient taxa to calculate 4 or more.';
              MessageDlg('There must be at least four taxa to create the distance matrix.', mtWarning, [mbOK], 0);
            end
            else
            begin
              pgctlDPhyIoM.PageIndex := 1;
              strgrdDataTableProp.ColCount := 2 * TestTableForSpeciesData.SpeciesCount - 2;
              mnuRun.Enabled := True;                  // Enable run analysis
              mnuOpen.Enabled := False;                // Disable file open
            end;
          end;
//          mnuDisplayData.Enabled := True;
//          mnuTestTable.Enabled := True;
//          tabTestData.TabVisible := True;
          mnuTestTable.Enabled := False;
          tabTestDataTable.TabVisible := True;
          mnuClearDisplay.Enabled := True;
          strCurrentDir := opndlgTestData.GetNamePath;
          if edtFileAction.Text = '' then
            edtFileAction.Text := 'File selected.  ' + IntToStr(intTestCount) + ' Tests.  ' + IntToStr(intSpeciesCount) + ' records to process.  ' + strDataType + '.';
        end
        else
          strDataType := 'File open by another apllication or user.'
      end
      else
      begin
        MessageDlg('File: ' + strFileName + ' does not exist.', mtWarning, [mbOK], 0);
        edtFileAction.Text := 'Requested file does not exist';
        intTestCount := 0;
        intSpeciesCount := 0;
      end;
    end
    else
      edtFileAction.Text := 'File open selection cancelled';
  end
  else
  begin
    strDataType := 'Data type select cancelled by user';
    MessageDlg(strDataType, mtWarning, [mbOK], 0);
    edtFileAction.Text := strDataType;
    intTestCount := 0;
    intSpeciesCount := 0;
    mmoTestData.Clear;
  end;
end;

procedure TfrmPhyIoM.mnuRunClick(Sender: TObject);
begin
  try
    case TestTableForSpeciesData.wrdDataType of
      0..1: begin
        BuildMatchingMatrix(prgbrFileAction, TestTableForSpeciesData.Binary, MatchingMatrix, True);
        BuildBinaryDistanceMatrix(prgbrFileAction, mmoResults);
        BuildFullDMatrix;
        BuildQMatrix;
      end;
      2: if TestTableForSpeciesData.ErrorCount > 0 then
        MessageDlg('Measurement data has ' + IntToStr(TestTableForSpeciesData.ErrorCount) + ' Non-numeric data error(s).', mtWarning, [mbOK], 0)
      else
      begin
        BuildDifferenceMatrix(prgbrFileAction, TestTableForSpeciesData.Measurement, DifferenceMatrix, True);
        BuildMeasurementDistanceMatrix(prgbrFileAction, mmoResults);
        BuildFullDMatrix;
        BuildQMatrix;
      end;
      3..4: if TestTableForSpeciesData.ErrorCount > 0 then
        MessageDlg('Sequence data has ' + IntToStr(TestTableForSpeciesData.ErrorCount) + ' invalid length error(s).', mtWarning, [mbOK], 0)
      else
      begin
        BuildDffrncSqncMatrix(prgbrFileAction, TestTableForSpeciesData.Sequence, DffrncSqncMatrix, True);
        BuildSequenceDistanceMatrix(prgbrFileAction, mmoResults);
        BuildFullDMatrix;
        BuildQMatrix;
      end;
    else
      MessageDlg('Invalid run option encountered.', mtWarning, [mbOK], 0);
      Exit;
    end;
    InitialDataTableProp;
//    tabResults.TabVisible := True;             // Make Results tab visible
    tabDistanceMatrixGrid.TabVisible := True;  // Make Text Grid Results tab visible
    if boolTestRun then
    begin
      tabDstncMatrix.TabVisible := True;       // Make Distance Matrix tab visible
      tabQMatrix.TabVisible := True;           // Make Q Matrix tab visible
    end;
    mnuDisplayDstncMtrx.Enabled := True;   // Enable results display
    mnuSaveDstncMtrx.Enabled := True;      // Enable save Distance Matrix results for output
    mnuSaveMegaFormat.Enabled := True;     // Enable save Mega format
    mnuMakeTree.Enabled := True;           // Enable make tree
    mnuRun.Enabled := False;               // Disable run analysis
  except
    on E: exception do begin
      MessageDlg(E.Message + ' on run option', mtError, [mbOK], 0);
      Exit;
    end;
  end;
  MessageDlg('Make distance matrix option...Done', mtInformation, [mbOK], 0);
end;

procedure TfrmPhyIoM.mnuSaveInstructionsClick(Sender: TObject);
begin
  svdlgInstructions.InitialDir := strSaveCurrentDir;
  if svdlgInstructions.Execute then
  begin
    strSaveFileName := svdlgInstructions.FileName;
    if FileExists(strSaveFileName) then
      if mrYes = MessageDlg('File: ' + strSaveFileName + ' already exists do you want to replace it?', mtConfirmation, [mbYes, mbNo], 0) then
        try
          frmInstructions.rtmInstructions.Lines.SaveToFile(strSaveFileName);
        except
          on E:exception do MessageDlg(E.Message + ' for file: ' + strSaveFileName, mtWarning, [mbOK], 0);
        end
      else
        MessageDlg('Original file: ' + strSaveFileName + ' unchanged.', mtWarning, [mbOK], 0)
    else
      try
        frmInstructions.rtmInstructions.Lines.SaveToFile(strSaveFileName);
      except
        on E:exception do MessageDlg(E.Message + ' for file: ' + strSaveFileName, mtWarning, [mbOK], 0);
      end;
  end
  else
    MessageDlg('Request to save instructions: ' + svdlgInstructions.FileName + ' cancelled.', mtWarning, [mbOK], 0);
end;

procedure TfrmPhyIoM.mnuSaveMegaFormatClick(Sender: TObject);
begin
  svdlgMegaFormat.InitialDir := strSaveCurrentDir;
  if svdlgMegaFormat.Execute then
  begin
    strSaveFileName := svdlgMegaFormat.FileName;
    if FileExists(strSaveFileName) then
      if mrYes = MessageDlg('File: ' + strSaveFileName + ' already exists do you want to replace it?', mtConfirmation, [mbYes, mbNo], 0) then
        try
          SaveMegaFormatFile(strSaveFileName);
        except
          on E:exception do MessageDlg(E.Message + ' for file: ' + strSaveFileName, mtWarning, [mbOK], 0);
        end
      else
        MessageDlg('Original file: ' + strSaveFileName + ' unchanged.', mtWarning, [mbOK], 0)
    else
      try
        SaveMegaFormatFile(strSaveFileName);
      except
        on E:exception do MessageDlg(E.Message + ' for file: ' + strSaveFileName, mtWarning, [mbOK], 0);
      end;
  end
  else
    MessageDlg('Request to save Mega file: ' + svdlgMegaFormat.FileName + ' cancelled.', mtWarning, [mbOK], 0);
end;

procedure TfrmPhyIoM.mnuSaveNwckFrmtClick(Sender: TObject);
begin
  svdlgNwckFrmt.InitialDir := strSaveCurrentDir;
  if svdlgNwckFrmt.Execute then
  begin
    strSaveFileName := svdlgNwckFrmt.FileName;
    if FileExists(strSaveFileName) then
      if mrYes = MessageDlg('File: ' + strSaveFileName + ' already exists do you want to replace it?', mtConfirmation, [mbYes, mbNo], 0) then
        try
          mmoNewickFormat.Lines.SaveToFile(strSaveFileName)
        except
          on E:exception do MessageDlg(E.Message + ' for file: ' + strSaveFileName, mtWarning, [mbOK], 0);
        end
      else
        MessageDlg('Original file: ' + strSaveFileName + ' unchanged.', mtWarning, [mbOK], 0)
    else
      try
        mmoNewickFormat.Lines.SaveToFile(strSaveFileName);
      except
        on E:exception do MessageDlg(E.Message + ' for file: ' + strSaveFileName, mtWarning, [mbOK], 0);
      end;
  end
  else
    MessageDlg('Request to save Newick file format cancelled.', mtWarning, [mbOK], 0);
end;

procedure TfrmPhyIoM.mnuCSVFormatClick(Sender: TObject);
begin
  svdlgResultData.InitialDir := strSaveCurrentDir;
  if svdlgResultData.Execute then
  begin
    strSaveFileName := svdlgResultData.FileName;
    if FileExists(strSaveFileName) then
      if mrYes = MessageDlg('File: ' + strSaveFileName + ' already exists do you want to replace it?', mtConfirmation, [mbYes, mbNo], 0) then
        try
          strlstDstncMtrx.SaveToFile(strSaveFileName);
//          mmoResults.Lines.SaveToFile(strSaveFileName)
        except
          on E:exception do MessageDlg(E.Message + ' for file: ' + strSaveFileName, mtWarning, [mbOK], 0);
        end
      else
        MessageDlg('Original file: ' + strSaveFileName + ' unchanged.', mtWarning, [mbOK], 0)
    else
      strlstDstncMtrx.SaveToFile(strSaveFileName);
//      mmoResults.Lines.SaveToFile(strSaveFileName);
  end
  else
    MessageDlg('Request to save CSV file: ' + svdlgResultData.FileName + ' cancelled.', mtWarning, [mbOK], 0);
end;

procedure TfrmPhyIoM.mnuShowCalcErrsClick(Sender: TObject);
begin
  frmCalculatedErrors.Show;
end;

procedure TfrmPhyIoM.mnuTestTableClick(Sender: TObject);
begin
  pgctlDPhyIoM.PageIndex := 1;
end;

procedure TfrmPhyIoM.pgctlDPhyIoMChange(Sender: TObject);
begin
  if tabTestDataTable.TabVisible then
    if tabTestDataTable.Visible then
      mnuTestTable.Enabled := False
    else
      mnuTestTable.Enabled := True;
  if tabDistanceMatrixGrid.TabVisible then
    if tabDistanceMatrixGrid.Visible then
      mnuDisplayDstncMtrx.Enabled := False
    else
      mnuDisplayDstncMtrx.Enabled := True;
  if tabTestDataTable.TabVisible then
    if tabCiteProgram.TabVisible then
      if tabCiteProgram.Visible then
        mnuCiting.Enabled := False
      else
        mnuCiting.Enabled := True;
end;

procedure TfrmPhyIoM.RichMemo1Change(Sender: TObject);
begin

end;

procedure TfrmPhyIoM.tabResultsHide(Sender: TObject);
begin
  mnuDisplayDstncMtrx.Enabled := True;
end;

procedure TfrmPhyIoM.tabResultsShow(Sender: TObject);
begin
  mnuDisplayDstncMtrx.Enabled := False;
end;

procedure TfrmPhyIoM.trvwTreeGraphClick(Sender: TObject);
begin

end;

procedure TfrmPhyIoM.InitialDataTableProp;
var
  inx: integer;
begin
  with strgrdDataTableProp do
  begin
   if (RowCount > 0) or (ColCount > 0) then
     Clear;
    RowCount := 47;
    FixedRows := 2;
    if TestTableForSpeciesData.SpeciesCount > 2 then
      ColCount := 2 * TestTableForSpeciesData.SpeciesCount
    else
      ColCount := 2;
    FixedCols := 1;
    for inx := 0 to RowCount - 1 do
      Cells[0, inx] := kDataTableTitle[inx];
  end;
end;

end.
