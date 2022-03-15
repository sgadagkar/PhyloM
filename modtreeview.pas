unit modTreeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, modDataTypes, modTestData;

type

  { TfrmTreeListDown }

  TfrmTreeListDown = class(TForm)
    btnClose: TButton;
    btnSetRoot: TButton;
    chkbxViewTreeMap: TCheckBox;
    lblNewickFormat: TLabel;
    lblRerootedTree: TLabel;
    lblTreeMap: TLabel;
    lblTreeDownView: TLabel;
    memRerootedNwkFmt: TMemo;
    memTreeView: TMemo;
    svdlgRerootedNewick: TSaveDialog;
    trvwRerootedTree: TTreeView;
    trvwTextDownBased: TTreeView;
    procedure btnCloseClick(Sender: TObject);
    procedure btnSetRootClick(Sender: TObject);
    procedure chkbxViewTreeMapChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    function CreateTVMNode(var ndItem: TTreeNode; const boolIsFirst: boolean; const intNP: integer): PTreeViewMember;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure InitialRootList;
    procedure CreateChildrenNodes(const Node, NodeOrg: TTreeNode);
    procedure CreateParentNodes(const Node, NodeOrg, ndOrgRoot: TTreeNode; const iaOrgRt: TOldRootIndex);
    function NewNewickBranch(const ndOrgTr: TTreeNode; const intInRoot, intNBRoot: integer; const RtMd: TRootMode; const ORtMd: TOrgRootMode): PRerootedViewMember;
    function strCreateNewickTree: string;
  public
    { public declarations }
    ndNewRootOT: TTreeNode;
    strNwkRR,
    strOrgRoot,
    strTopLabel: string;
  end;

var
  frmTreeListDown: TfrmTreeListDown;

implementation

{$R *.lfm}

{ TfrmTreeListDown }

uses
  modPhyIoM;

const
  kNotInRoot = -1;
  kByRoot = -2;
var
  intOR,
  intSNP: integer;

procedure TfrmTreeListDown.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  TreeMember: PTreeViewMember;
  pntRRVM: PRerootedViewMember;
  inx: integer;
begin
  for inx := 0 to trvwTextDownBased.Items.Count - 1 do
    if trvwTextDownBased.Items[inx].Data <> nil then
    begin
      TreeMember := PTreeViewMember(trvwTextDownBased.Items[inx].Data);
      Dispose(TreeMember);
      trvwTextDownBased.Items[inx].Data := nil;
    end;
  trvwTextDownBased.Items.Clear;
  for inx := 0 to trvwRerootedTree.Items.Count - 1 do
    if trvwRerootedTree.Items[inx].Data <> nil then
    begin
      pntRRVM := PRerootedViewMember(trvwRerootedTree.Items[inx].Data);
      Dispose(pntRRVM);
      trvwRerootedTree.Items[inx].Data := nil;
    end;
  trvwRerootedTree.Items.Clear;
  memTreeView.Lines.Clear;
  memRerootedNwkFmt.Clear;
end;

procedure TfrmTreeListDown.FormActivate(Sender: TObject);
var
  TreeMember: PTreeViewMember;
  inx: integer;
begin
  trvwTextDownBased.Items.Assign(frmPhyIoM.trvwTreeGraph.Items);
  if frmPhyIoM.trvwTreeGraph.Items.Count > 0 then
    if (trvwTextDownBased.Items[0].Data = nil) then
      for inx := 0 to frmPhyIoM.trvwTreeGraph.Items.Count - 1 do
      begin
        New(TreeMember);
        TreeMember^ := PTreeViewMember(frmPhyIoM.trvwTreeGraph.Items[inx].Data)^;
        trvwTextDownBased.Items[inx].Data := TreeMember;
        with TestTableForSpeciesData, TreeMember^ do
        begin
          trvwTextDownBased.Items[inx].Text := trvwTextDownBased.Items[inx].Text + ':' + Species[intNodePos].ShortLabel;
        end;
      end;
  memTreeView.Lines.Clear;
  for inx := 0 to frmPhyIoM.trvwTreeGraph.Items.Count - 1 do
  with PTreeViewMember(trvwTextDownBased.Items[inx].Data)^ do
    begin
      memTreeView.Lines[inx] := 'Pos:' + IntToStr(intNodePos) + ', Parent:' + IntToStr(intNodeParent) + ', Sibling:' + IntToStr(intNodeSibling) + ', Top Child:' + IntToStr(intChildren[0]) + ', Bottom Child:' + IntToStr(intChildren[1]);
    end;
  chkbxViewTreeMap.Checked := False;
  strTopLabel := trvwTextDownBased.Items[0].Text;
end;

procedure TfrmTreeListDown.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmTreeListDown.btnSetRootClick(Sender: TObject);
var
  pntRRVM: PRerootedViewMember;
  RootTreeMember,
  SelectedTreeMember,
  TreeMember: PTreeViewMember;
  dblBrnchLngth,
  dblLngth: double;
  intNP: integer;
  strSlctText,
  strNwk,
  strNewRootParent: string;
  ndSiblingNext,                // Member of original tree
  ndSiblingPrev,                // Member of original tree
  ndOrgSibling,                 // Member of original tree
  ndSibling,                    // Member of rerooted tree
  ndOldParent,                  // Member of rerooted tree now sibling of original sibling
  ndOrgParent,
  ndParent,
  ndOrgRoot,
  NdRoot,
  NCT,
  NCB,
  NPT,
  ndONPT,
  NSB,
  NdTree,
  NdTreeBottom,
  NdTreeTop: TTreeNode;

begin
  ndNewRootOT := trvwTextDownBased.Selected;
  if ndNewRootOT = nil then
    exit
  else
    if ndNewRootOT.Text = strTopLabel then
      exit;
  SelectedTreeMember := PTreeViewMember(ndNewRootOT.Data);
  memRerootedNwkFmt.Lines.Clear;
  ndOrgRoot := trvwTextDownBased.Items[0];
  strOrgRoot := '*Old Root* for ' + ndOrgRoot.Text;
  RootTreeMember := PTreeViewMember(ndOrgRoot.Data);
  intSNP := SelectedTreeMember^.intNodePos;
  trvwRerootedTree.Items.Clear;
  NdRoot := nil;
  pntRRVM := nil;
  intOR := RootTreeMember^.intNodePos;
  strSlctText := ndNewRootOT.Text;
  strNewRootParent := ndNewRootOT.Parent.Text;
  new(TreeMember);
  with TestTableForSpeciesData do
  begin
    iaOldRoot[0] := NodeCount - 1;
    iaOldRoot[1] := NodeCount;
    iaOldRoot[2] := NodeCount + 1;
    dblBrnchLngth := Species[intSNP].BranchLength;
    dblLngth := dblSplitLength(dblBrnchLngth);
    if trvwTextDownBased.Selected.Parent.Text = strTopLabel then
    begin
      pntRRVM := NewNewickBranch(nil, intOR, intOR, AtRoot, OrgRoot);
      NdTree := trvwRerootedTree.Items.AddObjectFirst(NdRoot, strOrgRoot, pntRRVM);
      pntRRVM := NewNewickBranch(nil {NdTree}, intSNP, intOR, RootB, ORootT);
      NdTreeBottom := trvwRerootedTree.Items.AddChildObject(NdTree, strSlctText, pntRRVM);
      if trvwTextDownBased.Selected.GetNextSibling = nil then
        ndOrgSibling := trvwTextDownBased.Selected.GetPrevSibling
      else
        ndOrgSibling := trvwTextDownBased.Selected.GetNextSibling;
      pntRRVM := NewNewickBranch(ndOrgSibling, kNotInRoot, intOR, RootT, ORootB);
      NdTreeTop := trvwRerootedTree.Items.AddChildObjectFirst(NdTree, ndOrgSibling.Text + ' Sibling', pntRRVM); //'Parent & Sibling: ' + strBranchLabel(-1, dblBrnchLngth - dblLngth)); //trvwTextDownBased.Selected.Parent.Text);
      ndSibling := NdTreeTop;
    end
    else
    begin
      pntRRVM := NewNewickBranch(nil, intSNP, intSNP, AtRoot, ortmGetMode(intSNP));
      NdTree := trvwRerootedTree.Items.AddObjectFirst(NdRoot, '*New Root* for ' + trvwTextDownBased.Selected.Text, pntRRVM);
      pntRRVM := NewNewickBranch(NdTree, intSNP, intSNP, RootB, ortmGetMode(intSNP));
      NdTreeBottom := trvwRerootedTree.Items.AddChildObject(NdTree, 'Branch for "' + strBranchLabel(intSNP, dblLngth) + '"', pntRRVM);
      pntRRVM := NewNewickBranch(NdTree, intSNP, intSNP, RootT, ortmGetMode(intSNP));
      NdTreeTop := trvwRerootedTree.Items.AddChildObjectFirst(NdTree, 'Parent & Sibling: "' + strBranchLabel(intSNP, dblBrnchLngth - dblLngth) + '"', pntRRVM); //trvwTextDownBased.Selected.Parent.Text);
      if trvwTextDownBased.Selected.GetNextSibling = nil then
        ndOrgSibling := trvwTextDownBased.Selected.GetPrevSibling
      else
        ndOrgSibling := trvwTextDownBased.Selected.GetNextSibling;
      ndOrgParent := trvwTextDownBased.Selected.Parent;
      pntRRVM := NewNewickBranch(ndOrgSibling, kNotInRoot, intOR, NonRoot, FindOrgRt);
      ndSibling := trvwRerootedTree.Items.AddChildObject(NdTreeTop, ndOrgSibling.Text, pntRRVM);
      CreateParentNodes(NdTreeTop, ndOrgParent, ndOrgRoot, iaOldRoot);
    end;
    CreateChildrenNodes(ndSibling, ndOrgSibling{ndOrgParent});
    CreateChildrenNodes(NdTreeBottom, trvwTextDownBased.Selected);
    trvwRerootedTree.FullExpand;
    if trvwTextDownBased.Items.Count <> trvwRerootedTree.Items.Count then
      MessageDlg('Original tree count:' + IntToStr(trvwTextDownBased.Items.Count) + ' is not equal to Rerooted tree count:' + IntToStr(trvwRerootedTree.Items.Count), mtError, [mbOK], 0);       // Send difference message to dialog box
    strNwk := strCreateNewickTree;
//    memRerootedNwkFmt.Lines[0] := strNwk;
    strNwkRR := strRerootedNewickFrmt(strNwk);
    memRerootedNwkFmt.Lines[0] := strNwkRR;
    CreateTopologyRerootedTree;
    ShowTextTreeView(strNwkRR, intSNP, 53);
//    ShowTextTreeView(strNwk);
  end;
end;

procedure TfrmTreeListDown.chkbxViewTreeMapChange(Sender: TObject);
var
  pntRRVM: PRerootedViewMember;
  inx: integer;
begin
  if chkbxViewTreeMap.Checked then
  begin
    memTreeView.Visible := True;
    lblTreeMap.Visible := True;
    trvwRerootedTree.Visible := False;
    lblRerootedTree.Visible := False;
    if trvwRerootedTree.Items.Count > 0 then
      with memTreeView do
      begin
        Lines.Clear;
        for inx := 0 to trvwRerootedTree.Items.Count - 1 do
          with trvwRerootedTree.Items[inx] do
          begin
            pntRRVM := Data;
            Lines.Add('Label:' + pntRRVM^.strBranchLabel + ' Newick:' + pntRRVM^.strNewick + ' Brnch String:' + pntRRVM^.strBranchLabel);
          end;
      end;
  end
  else
  begin
    memTreeView.Visible := False;
    lblTreeMap.Visible := false;
    trvwRerootedTree.Visible := True;
    lblRerootedTree.Visible := True;
  end;
end;

function TfrmTreeListDown.CreateTVMNode(var ndItem: TTreeNode; const boolIsFirst: boolean; const intNP: integer): PTreeViewMember;
var
  pntTVM: PTreeViewMember;
  ndCNode: TTreeNode;
begin
  with trvwTextDownBased, TestTableForSpeciesData do
  begin
    New(pntTVM);
    with pntTVM^ do
    begin
      intNodePos := intNP;
      Species[intNodePos].AbsIndex := Items.Count;  //0;  //
      intNodeparent := Species[intNodePos].NodeParent;
      intNodeSibling := Species[intNodePos].BeginSegment.int2ndXNd;
      intChildren[0] := Species[intNodePos].EndSegment.int1stXNd;
      intChildren[1] := Species[intNodePos].EndSegment.int2ndXNd;
      if (ndItem = nil) or (ndItem = Items[0]) then
        ndCNode := Items.AddObjectFirst(ndItem, Species[intNodePos].SpeciesName + '(' + IntToStr(Species[intNodePos].AbsIndex) + ')(' + IntToStr(intNodePos) + '):' + FloatToStrF(Species[intNodePos].BranchLength, ffFixed, 12, 4), pntTVM)
      else
        if boolIsFirst then
          ndCNode := Items.AddChildObjectFirst(ndItem, Species[intNodePos].SpeciesName + '(' + IntToStr(Species[intNodePos].AbsIndex) + ')(' + IntToStr(intNodePos) + '):' + FloatToStrF(Species[intNodePos].BranchLength, ffFixed, 12, 4), pntTVM)
        else
          ndCNode := Items.AddChildObject(ndItem, Species[intNodePos].SpeciesName + '(' + IntToStr(Species[intNodePos].AbsIndex) + ')(' + IntToStr(intNodePos) + '):' + FloatToStrF(Species[intNodePos].BranchLength, ffFixed, 12, 4), pntTVM);
      if intChildren[0] <> intChildren[1] then
      begin
        pntTVMLftChld := CreateTVMNode(ndCNode, kboolFirst, intChildren[0]);
        pntTVMRghtChld := CreateTVMNode(ndCNode, kboolSecond, intChildren[1]);
      end
      else
      begin
        pntTVMLftChld := nil;
        pntTVMRghtChld := nil;
      end;
      Result := pntTVM;
    end;
  end;
end;

procedure TfrmTreeListDown.CreateParentNodes(const Node, NodeOrg, ndOrgRoot: TTreeNode; const iaOrgRt: TOldRootIndex);
var
  pntRRVM: PRerootedViewMember;
  TreeMember,
  PrntTreeMember: PTreeViewMember;
  intOPC,
  intPP: integer;
  ndParent,
  ndSibling,
  ndOrgChildren,
  ndChild1,
  ndChild2,
  ndOrgSibling: TTreeNode;
  strOrg,
  strOrgFC,
  strOrgLC,
  strOrgChildren,
  strChildren1,
  strChildren2: string;
begin
  pntRRVM := nil;
  TreeMember := PTreeViewMember(NodeOrg.Data);
  intPP := TreeMember^.intNodePos;
  if intPP > TestTableForSpeciesData.NodeCount then
    MessageDlg('Original tree root encountered an original part.:' + IntToStr(intPP), mtError, [mbOK], 0);       // Send difference message to dialog box
  if (intPP = iaOrgRt[0]) or (intPP = iaOrgRt[1]) then
  begin
    strOrg := NodeOrg.Text;
    pntRRVM := NewNewickBranch(NodeOrg, intOR, intOR, NonRoot, FindOrgRt);
    ndParent := trvwRerootedTree.Items.AddChildObjectFirst(Node, strOrgRoot, pntRRVM);
    strOrgFC := ndOrgRoot.GetFirstChild.Text;
    strOrgLC := ndOrgRoot.GetLastChild.Text;
    if strOrgFC = strOrg then
      ndOrgChildren := ndOrgRoot.GetLastChild
    else
      ndOrgChildren := ndOrgRoot.GetFirstChild;
    strOrgChildren := ndOrgChildren.Text;
//    ndOrgChildren := ndSibling.Parent;
//    ndSibling := trvwRerootedTree.Items.AddChildFirst(ndParent, ndNewRootOT.Parent.Text);
    pntRRVM := NewNewickBranch(ndOrgChildren.GetFirstChild, kNotInRoot, intOR, NonRoot, FindOrgRt);
    ndChild1 := trvwRerootedTree.Items.AddChildObjectFirst(ndParent, ndOrgChildren.GetFirstChild.Text, pntRRVM);
    strChildren1 := ndChild1.Text;
    CreateChildrenNodes(ndChild1, ndOrgChildren.GetFirstChild);
    pntRRVM := NewNewickBranch(ndOrgChildren.GetLastChild, kNotInRoot, intOR, NonRoot, FindOrgRt);
    ndChild2 := trvwRerootedTree.Items.AddChildObject(ndParent, ndOrgChildren.GetLastChild.Text, pntRRVM);
    strChildren2 := ndChild2.Text;
    CreateChildrenNodes(ndChild2, ndOrgChildren.GetLastChild);
  end
  else
  begin
    PrntTreeMember := PTreeViewMember(NodeOrg.Parent.Data);
    intOPC := PrntTreeMember^.intNodePos;
    if (intOPC = iaOrgRt[0]) or (intOPC = iaOrgRt[1]) then
    begin
      pntRRVM := NewNewickBranch(ndNewRootOT.Parent, kNotInRoot, intOR, NonRoot, FindOrgRt);
      ndParent := trvwRerootedTree.Items.AddChildObjectFirst(Node, ndNewRootOT.Parent.Text, pntRRVM);
    end
    else
    begin
      pntRRVM := NewNewickBranch(NodeOrg.Parent, kNotInRoot, intOR, NonRoot, FindOrgRt);
      ndParent := trvwRerootedTree.Items.AddChildObjectFirst(Node, NodeOrg.Parent.Text, pntRRVM);
    end;
    CreateParentNodes(ndParent, NodeOrg.Parent, ndOrgRoot, iaOrgRt);
    if NodeOrg.GetNextSibling = nil then
      ndOrgSibling := NodeOrg.GetPrevSibling
    else
      ndOrgSibling := NodeOrg.GetNextSibling;
    pntRRVM := NewNewickBranch(ndOrgSibling, kNotInRoot, intOR, NonRoot, FindOrgRt);
    ndSibling := trvwRerootedTree.Items.AddChildObject(ndParent, ndOrgSibling.Text, pntRRVM);
    CreateChildrenNodes(ndSibling, ndOrgSibling);
  end;
end;

function TfrmTreeListDown.NewNewickBranch(const ndOrgTr: TTreeNode; const intInRoot, intNBRoot: integer; const RtMd: TRootMode; const ORtMd: TOrgRootMode): PRerootedViewMember;
var
  TreeMember: PTreeViewMember;
  strBLbl: string;
  intNP: integer;
begin
  Result := new(PRerootedViewMember);
  with Result^, TestTableForSpeciesData do
  begin
    if ndOrgTr <> nil then
      If ORtMd = FindOrgRt then
      begin
        TreeMember := PTreeViewMember(ndOrgTr.Data);
        intNP := TreeMember^.intNodePos;
      end
      else
      begin
        intNP := intInRoot;
        ortmMmbr := ORtMd;
      end
    else
    begin
      intNP := intInRoot;
      ortmMmbr := ORtMd;
    end;
    strSpcsNm := Species[intNP].SpeciesName;
    ortmMmbr := ortmGetMode(intNP);
    rtmMmbr := RtMd;
    with Species[intNP] do
    begin
      if ortmMmbr = OrgRoot then
        dblBL := BranchLength
      else
        if rtmMmbr = AtRoot then
          dblBL := Species[intNBRoot].BranchLength
        else
          if rtmMmbr = RootB then
            dblBL := dblSplitLength(Species[intNBRoot].BranchLength)
          else
            if rtmMmbr = RootT then
              dblBL := Species[intNBRoot].BranchLength - dblSplitLength(Species[intNBRoot].BranchLength)
            else
              if not (ortmMmbr in [ORootT, ORootB]) then
                dblBL := BranchLength
              else
                dblBL := Species[NodeCount + 1].BranchLength;
      if rtmMmbr = AtRoot then
      begin
        strNewick := '(,);';
        strBranchLabel := '*New Root*';
      end
      else
        if rtmMmbr = RootT then
        begin
          strBranchLabel := 'Branch for ' + Species[intNP].SpeciesName;
          strNewick := '(,):' + FloatToStrF(dblBL, ffFixed, 10, 4) + strBSBrackets(Species[intNP].BeginSegment.int1stXNd);
        end
        else
          if rtmMmbr <> RootB then
            if EndSegment.int1stXNd = EndSegment.int2ndXNd then
            begin
              strBranchLabel := Species[intNP].SpeciesName;
              strNewick := strBranchLabel + ':' + FloatToStrF(dblBL, ffFixed, 10, 4);
            end
            else
            begin
              strBranchLabel := Species[intNP].SpeciesName;
              strNewick := '(,):' + FloatToStrF(dblBL, ffFixed, 10, 4) + strBSBrackets(intNP);
            end
          else
            if EndSegment.int1stXNd = EndSegment.int2ndXNd then
            begin
              strBranchLabel := Species[intNBRoot].SpeciesName;
              strNewick := strBranchLabel + ':' + FloatToStrF(dblBL, ffFixed, 10, 4);
            end
            else
            begin
              strBranchLabel := Species[intNBRoot].SpeciesName;
              strNewick := '(,):' + FloatToStrF(dblBL, ffFixed, 10, 4) + strBSBrackets(intNBRoot);
            end;
{        begin
          strBranchLabel := SpeciesName;
          strNewick := strBranchLabel + ':' + FloatToStrF(dblBL, ffFixed, 10, 4);
        end
        else
        begin
          strBranchLabel := '*Branch*';
          if intNodePos = NodeCount + 1 then
            strNewick := '(,):' + FloatToStrF(dblBL, ffFixed, 10, 4) + strBSBrackets(intNodePos - 1)
          else
            if EndSegment.int1stXNd <> EndSegment.int2ndXNd then
              strNewick := '(,):' + FloatToStrF(dblBL, ffFixed, 10, 4) + strBSBrackets(intNodePos)
            else
            begin
              strBranchLabel := SpeciesName;
              strNewick := strBranchLabel + ':' + FloatToStrF(dblBL, ffFixed, 10, 4);
            end
        end;}
    end;
  end;
end;

procedure TfrmTreeListDown.CreateChildrenNodes(const Node, NodeOrg: TTreeNode);
var
  pntRRVM: PRerootedViewMember;
  ndCT,
  ndCB: TTreeNode;
begin
  pntRRVM := nil;
  if NodeOrg.HasChildren then
  begin
    pntRRVM := NewNewickBranch(NodeOrg.GetFirstChild, kNotInRoot, intOR, NonRoot, FindOrgRt);
    ndCT := trvwRerootedTree.Items.AddChildObjectFirst(Node, NodeOrg.GetFirstChild.Text, pntRRVM);
    CreateChildrenNodes(ndCT, NodeOrg.GetFirstChild);
    pntRRVM := NewNewickBranch(NodeOrg.GetLastChild, kNotInRoot, intOR, NonRoot, FindOrgRt);
    ndCB := trvwRerootedTree.Items.AddChildObject(Node, NodeOrg.GetLastChild.Text, pntRRVM);
    CreateChildrenNodes(ndCB, NodeOrg.GetLastChild);
  end;
end;

function TfrmTreeListDown.strCreateNewickTree: string;
var
  pntRRVM: PRerootedViewMember;
  inx,
  intPs: integer;
begin
  pntRRVM := nil;
  Result := '';
  with TestTableForSpeciesData, trvwRerootedTree do
  begin
    for inx := 0 to Items.Count - 1 do
    begin
      pntRRVM := Items[inx].Data;
      if inx <> 0 then
        Result := Result + '|';
//      if pntRRVM <> nil then
      Result := Result + pntRRVM^.strNewick;
//      else
//        Result := Result + 'nil';
    end;
  end;
end;

procedure TfrmTreeListDown.FormCreate(Sender: TObject);
begin
  trvwTextDownBased.Items.Clear;
  trvwRerootedTree.Items.Clear;
end;

procedure TfrmTreeListDown.InitialRootList;
const
  kLetters = 26;
var
  inx: integer;
  bytC,
  bytC2,
  bytA: byte;
  strItem: string;
begin
{  cbxSetRoot.Clear;
  bytA := ord('A');
  with TestTableForSpeciesData do
    for inx := 0 to NodeCount - 1 do
    begin
      bytC := inx div kLetters;
      if bytC > 0 then
        strItem := Chr(bytA + bytC - 1)
      else
        strItem := '';
      bytC2 := inx mod kLetters;
      strItem := strItem + Chr(bytA + bytC2);
      cbxSetRoot.AddItem(strItem, nil);
    end;}
end;


end.

