unit modSelectNewRoot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, modDataTypes, modTestData;

type

  { TfrmSelectNewRoot }

  TfrmSelectNewRoot = class(TForm)
    btnRoot: TButton;
    btnCancel: TButton;
    lblSplitLength: TLabel;
    lblRootList: TLabel;
    lsbxBranchList: TListBox;
    rdgrpRootedTreeType: TRadioGroup;
    trkbrSplitLengthRoot: TTrackBar;
    procedure btnCancelClick(Sender: TObject);
    procedure btnRootClick(Sender: TObject);
//    procedure chkbxOriginalTreeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    function Execute(var intRt, intSplt, intTrTyp: integer): boolean;
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure lsbxBranchListClick(Sender: TObject);
    procedure rdgrpRootedTreeTypeClick(Sender: TObject);
  private
    intRoot,
//    intSplit,
    intTreeType: integer;
    { private declarations }
  public
    { public declarations }
  end;

var
  frmSelectNewRoot: TfrmSelectNewRoot;

implementation

{$R *.lfm}

{ TfrmSelectNewRoot }

uses
  modPhyIoM, modTreeGraph, modTreeView, modAbout;

function TfrmSelectNewRoot.Execute(var intRt, intSplt, intTrTyp: integer): boolean;
begin
  lsbxBranchList.Items.Assign(strlsRootList);
  intRoot := intRt;
//  intSplit := intSplt;
  intTreeType := intTrTyp;
//  lsbxBranchList.ItemIndex := intRoot;
  trkbrSplitLengthRoot.Position := TestTableForSpeciesData.intSplit;
  rdgrpRootedTreeType.ItemIndex := intTreeType;
  if ShowModal = mrOK then
  begin
    intSplt := trkbrSplitLengthRoot.Position;
    TestTableForSpeciesData.dblSpltFctr := 100.0 / intSplt;
    if lsbxBranchList.ItemIndex = -1 then
    begin
      MessageDlg('No branch selected for root.', mtWarning, [mbOK], 0);
      Result := False
    end
    else
    begin
      intRoot := lsbxBranchList.ItemIndex;
      Result := True;
    end;
    intTreeType := rdgrpRootedTreeType.ItemIndex;
  end
  else
    Result := False;
end;

procedure TfrmSelectNewRoot.FormShow(Sender: TObject);
var
  inx: integer;
begin
  rdgrpRootedTreeType.ItemIndex := intTreeType;
  lsbxBranchList.Clear;
  for inx := 0 to strlsRootList.Count - 2 do
    lsbxBranchList.Items.AddText(strlsRootList[inx]);
  if intRoot + 1 > lsbxBranchList.Items.Count then
  begin
    lsbxBranchList.ItemIndex := -1;
    intRoot := lsbxBranchList.ItemIndex;
  end;
//  if chkbxOriginalTree.Checked then
//    lsbxBranchList.Enabled := False
//  else
//    lsbxBranchList.Enabled := True;
  lsbxBranchList.ItemIndex := intRoot;
{  if lsbxBranchList.Enabled then
    lsbxBranchList.ItemIndex := frmTreeGraph.intRoot
  else
    lsbxBranchList.ItemIndex := -1;}
  trkbrSplitLengthRoot.Position := TestTableForSpeciesData.intSplit;
  if lsbxBranchList.ItemIndex = -1  {and not chkbxOriginalTree.Checked} then
    btnRoot.Enabled := False
  else
    btnRoot.Enabled := True;
{  if ShowModal = mrOK then
  begin
  end
  else}
end;

procedure TfrmSelectNewRoot.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsNormal then
    if frmTreeGraph.WindowState = wsMinimized then
      frmTreeGraph.WindowState := wsNormal;
end;

procedure TfrmSelectNewRoot.lsbxBranchListClick(Sender: TObject);
begin
  btnRoot.Enabled := True;
end;

procedure TfrmSelectNewRoot.rdgrpRootedTreeTypeClick(Sender: TObject);
begin
  intTreeType := rdgrpRootedTreeType.ItemIndex;
end;

procedure TfrmSelectNewRoot.FormCreate(Sender: TObject);
begin
  trkbrSplitLengthRoot.Position := TestTableForSpeciesData.intSplit;
//  intSplit := trkbrSplitLengthRoot.Position;
  intTreeType := rdgrpRootedTreeType.ItemIndex;
end;

procedure TfrmSelectNewRoot.btnRootClick(Sender: TObject);
begin
  lsbxBranchList.Enabled := True;
  TestTableForSpeciesData.intSplit := trkbrSplitLengthRoot.Position;
  TestTableForSpeciesData.dblSpltFctr := 100.0 / TestTableForSpeciesData.intSplit;
  frmTreeGraph.intTreeType := intTreeType;
{  if chkbxOriginalTree.Checked then
  begin
    frmTreeGraph.intRoot := lsbxBranchList.Count;
    frmTreeGraph.boolComplete := True;
    frmTreeGraph.boolOriginal := True;
  end
  else}
    if lsbxBranchList.ItemIndex = -1 then
    begin
      MessageDlg('No branch selected for root.', mtWarning, [mbOK], 0);
      frmTreeGraph.boolComplete := False;
      frmTreeGraph.boolOriginal := False;
      exit;
    end
    else
    begin
      frmTreeGraph.boolComplete := True;
      frmTreeGraph.boolOriginal := False;
      intRoot := lsbxBranchList.ItemIndex;
      frmTreeGraph.intRoot := intRoot;
    end;
  frmTreeGraph.RootSelected(Sender);
  Close;
end;

{procedure TfrmSelectNewRoot.chkbxOriginalTreeClick(Sender: TObject);
begin
  if chkbxOriginalTree.Checked then
  begin
    lsbxBranchList.Enabled := False;
    btnRoot.Enabled := True;
  end
  else
  begin
    lsbxBranchList.Enabled := True;
    if lsbxBranchList.ItemIndex = -1 then
      btnRoot.Enabled := False
    else
      btnRoot.Enabled := True;
  end;
end;}

procedure TfrmSelectNewRoot.btnCancelClick(Sender: TObject);
begin
//  frmTreeGraph.boolComplete := False;
  frmTreeGraph.RootCancelled(Sender);
  Close;
end;

end.

