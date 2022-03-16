// Module to load measurement and binary data to grid and record structures
// Test commit comment
unit modTestData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, Grids, Dialogs, Graphics, modDataTypes;

function intShowHeapStatus(var SHS: THeapStatus): integer;
procedure AssignBracketRange(const StrtPnt: TPoint; const W, H: integer);
procedure AssignTplgyOnlyRange(const StrtPnt: TPoint; const W, H: integer);
procedure AssignNewickTORange(const StrtPnt: TPoint; const W, H: integer);
procedure AssignNwckBrcktRange(const StrtPnt: TPoint; const W, H: integer);
procedure AssignRadialRange(const StrtPnt: TPoint; const W, H: integer);
procedure ClearBinariesMeasurements(const boolNotExit: boolean);
procedure Clear3DDoubleArray(var aryDbl: T3DDoubleArray);
procedure Clear3DCharArray(var arrayChar: TBSBinaryMatix);
procedure LoadMeasuremntData(const strFileName: string; var prgBar: TProgressBar; var strgrdTD: TStringGrid);
procedure LoadBinaryData(const strFileName: string; var prgBar: TProgressBar; var strgrdTD: TStringGrid; const BinaryType: TBinaryDataType);
procedure LoadSequenceData(const strFileName: string; var prgBar: TProgressBar; var strgrdTD: TStringGrid);
procedure LoadFastaSequenceData(const strFileName: string; var prgBar: TProgressBar; var strgrdTD: TStringGrid);
procedure BuildDifferenceMatrix(var prgBar: TProgressBar; var MM: TMeasurementMatrix; var DM: T3DDoubleArray; const boolShowBar: boolean);
procedure BuildMeasurementDistanceMatrix(var prgBar: TProgressBar; var mmoRslts: TMemo);
procedure BuildMatchingMatrix(var prgBar: TProgressBar; var BM: TBinaryMatix; var MM: TMatchingMatrix; const boolShowBar: boolean);
procedure BuildBinaryDistanceMatrix(var prgBar: TProgressBar; var mmoRslts: TMemo);
procedure BuildDffrncSqncMatrix(var prgBar: TProgressBar; var SM: TSequenceMatix; var UM: TUnmatchingMatrix; const boolShowBar: boolean);
procedure BuildSequenceDistanceMatrix(var prgBar: TProgressBar; var mmoRslts: TMemo);
procedure BuildBSDistanceMatrix(var prgBar: TProgressBar; const intRplcts: integer);
//procedure InitialDataTableProp;
procedure BuildFullDMatrix;
procedure BuildBSFullDMatrix(const intBootStrapPage: integer);
procedure BuildQMatrix;
procedure MakeABranch(var dblLowestValue, dblSum: double; var inxarDstcn: TDistanceIndex; var ColX, RowY: integer; var strError: string; var prgBar: TProgressBar; var txtIN: TEdit);
procedure FindBranches(const intRplcts: Integer; var strError: string);
function DegToRad(const extDegrees: Extended): Extended;
function intAllNdSgmntRslvd(const intNd: integer): integer;
function dblFindMaxLengths: double;
function dblFindMinLengths: double;
function intMaxPixelFactor(const wrdDsplyTr: word; var dblScale: double): integer;
function intMaxTextLength: integer;
function FindQudrant(sngAngle: single): CompassPnt;
procedure CreateRadialGraph;
function strBSBrackets(const intNP: integer): string;
procedure ShowViewTreeViewValues;
function strStripLengths(const strNwck: string): string;
function strStripLengthsWithBS(const strNwck: string): string;
procedure ShowTextTreeView(const strNewickFmt: string; const intSlctRt, intSplt: integer);
procedure CreateRectangularRerootedTree(const intRt, intSplt, intPF: integer);
function  intAssignParentNodes(const intNL: integer): integer;
procedure MakeNewickNode(const intNL, intOffset: integer; var strNewick: string);
procedure MakeNewickFormat(var mmoNwckFrmt: TMemo);
procedure AssignReplicates(const intBootStraps: integer; var strgrdRplcts: TStringGrid);
function dblSplitLength(const dblFctrd: double): double;
procedure MakeInteriorNodeFile(const strFileName: string);
procedure DisplayBootStrapPageSqnc(const intIndex: integer);
procedure DisplayBootStrapPageBnry(const intIndex: integer);
procedure DisplayBootStrapPageMsrmnt(const intIndex: integer);
procedure CheckForReplicates(const inRplcts: integer; var cbxItems, cbxFDMDsply: TComboBox);
function boolMoreNodeLevels(const intLevel: integer): boolean;
function intMaxBracketPosX: integer;
function intMaxBracketPosY: integer;
procedure CreateBracketTree(var HomeVrtclPosition: TVrtclPosition; var intScaleTree: integer; var dblScaleValue: double);
procedure CreateTopologyRerootedTree;
function intMaxLabelLengthPos: integer ;
function intMaxTopologyOnlyWidth(const intFontSize: integer): integer;
function intMaxBracketWidth(const intFontSize: integer): integer;
procedure CreateTopologyOnlyTree;
procedure SaveReplicateDataFile(const strFlNm: string; const intRplct: integer);
procedure SaveDMReplicateFile(const strFlNm: string; const intRplct: integer);
procedure SaveMegaFormatFile(const strFlNm: string);
procedure SaveFastaFormatFile(const strFlNm: string);
procedure SaveNexusFormatFile(const strFlNm: string);
function strBranchLabel(const Pos: integer; const dblLblLngth: double): string;
function strRerootedNewickFrmt(const strBarNewick: string): string;
function intFindShortLabel(const strShrtLbl: string): integer;
function nodSelectBranch(const nodItems: TTreeNodes; const intRt: integer): TTreeNode;
function ortmGetMode(const intOrgNP: integer): TOrgRootMode;

implementation

uses
  modPhyIoM, modCalcErrs, modTreeGraph, modTreeView; //, modAbout;

var
  dblarDstcn: array[0..2] of double;
  dblarBrnchLngths: T1DDoubleArray;
  arinxSpcsQM: T1DArrayInteger;
  iarQIndex: TBSQIndex;
  CurrentHeapStatus: THeapStatus;

// Show status of heap to help with memory management of the application
function intShowHeapStatus(var SHS: THeapStatus): integer;
begin
  CurrentHeapStatus := GetHeapStatus;
  SHS := CurrentHeapStatus;
  Result := CurrentHeapStatus.HeapErrorCode;
end;

// Sets the starting point, width, and height of the Rectangular Tree graph
procedure AssignBracketRange(const StrtPnt: TPoint; const W, H: integer);
begin
  with Origin.ttoBracket do
  begin
    GraphOrigin := StrtPnt;
    Width := W;
    Height := H;
  end;
end;

// Sets the starting point, width, and height of the Topology Only Tree graph
procedure AssignTplgyOnlyRange(const StrtPnt: TPoint; const W, H: integer);
begin
 with Origin.ttoTplgyOnly do
 begin
   GraphOrigin := StrtPnt;
   Width := W;
   Height := H;
 end;
end;

// Sets the starting point, width, and height of the Topology Only Tree graph formed by the rerooted Newick tree
procedure AssignNewickTORange(const StrtPnt: TPoint; const W, H: integer);
begin
 with Origin.ttoNewickTO do
 begin
   GraphOrigin := StrtPnt;
   Width := W;
   Height := H;
 end;
end;

// Sets the starting point, width, and height of the Rectangular Tree graph formed by the rerooted Newick tree
procedure AssignNwckBrcktRange(const StrtPnt: TPoint; const W, H: integer);
begin
  with Origin.ttoNwckBrckt do
  begin
    GraphOrigin := StrtPnt;
    Width := W;
    Height := H;
  end;
end;

// Sets the starting point, width, and height of the Radial Tree graph
procedure AssignRadialRange(const StrtPnt: TPoint; const W, H: integer);
begin
  with Origin.ttoRadial do
  begin
    GraphOrigin := StrtPnt;
    Width := W;
    Height := H;
  end;
end;

{
  Function returns a header or footer title string to mark errors from certain calculation processes.

Parameters:
  strPrcssTitle -> Title of the current calculating process being used
  boolBegin -> If true it is the header tilte otherwise it is the footer
  Result -> string for Header or Footer of error report data
}
function strErrorHdrFtr(const strTitle, strPrcssTitle: string; const boolBegin: boolean): string;
var
  intLength: integer;
begin
  intLength := (100 - Length(TestTableForSpeciesData.SpeciesTitle) - Length(strTitle)) div 5;
  Result := strRepeat('-', intLength);
  if boolBegin then
    Result := Result + 'Begin Title: '
  else
    Result := Result + 'End Title: ';
  if strTitle = '' then
    Result := Result + TestTableForSpeciesData.SpeciesTitle + ', '
  else
    Result := Result + strTitle + ', ';
  Result := Result + strPrcssTitle + ' Errors' + strRepeat('-', intLength);
end;

// Clears a single dimesion double value array by setting length to zero length.
procedure ClearDoubleArray(var arrayDbl: T1DDoubleArray);
begin
  try
    if Length(arrayDbl) > 0 then
      SetLength(arrayDbl, 0);
  except
    on E:exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);                   //
  end;
end;

// Clears a two dimesional double value array by setting each single dimmension column length to zero
// and finally setting the double row dimension to zero length.
procedure Clear2DDoubleArray(var arryDbl: T2DDoubleArray);
var
  iny: integer;
begin
  try
    if Length(arryDbl) > 0 then
    begin
      for iny := High(arryDbl) downto 0 do
        ClearDoubleArray(arryDbl[iny]);
      SetLength(arryDbl, 0);
    end;
  except
    on E:exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);                   //
  end;
end;

// Clears a three dimesional double value array by setting calling Clear2DDoubleArray to clear each two
// dimmensional array and finally setting the top dimension or page to zero length.
procedure Clear3DDoubleArray(var aryDbl: T3DDoubleArray);
var
  inx: integer;
begin
  try
    if Length(aryDbl) > 0 then
    begin
      for inx := High(aryDbl) downto 0 do
        Clear2DDoubleArray(aryDbl[inx]);
      SetLength(aryDbl, 0);
    end;
  except
    on E:exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);                   //
  end;
end;

// Clears a integer single dimesional value array by setting length to zero length.
procedure ClearIntegerArray(var arrayInt: T1DArrayInteger);
begin
  if Length(arrayInt) > 0 then
    SetLength(arrayInt, 0);
end;

// Clears a two dimesional integer value array by setting each one dimmensional column length to zero
// and finally setting the second indice row dimension to zero length.
procedure Clear2DIntegerArray(var arryInt: T2DArrayInteger);
var
  inx: integer;
begin
  if Length(arryInt) > 0 then
  begin
    for inx := High(arryInt) downto 0 do
      ClearIntegerArray(arryInt[inx]);
    SetLength(arryInt, 0);
  end;
end;

// Clears a three dimesional integer value array by setting calling Clear2DIntegerArray to clear each two
// dimmensional array and finally setting the top dimension or page to zero length.
procedure Clear3DIntegerArray(var arryInt: T3DArrayInteger);
var
  inx: integer;
begin
  if Length(arryInt) > 0 then
  begin
    for inx := High(arryInt) downto 0 do
      Clear2DIntegerArray(arryInt[inx]);
    SetLength(arryInt, 0);
  end;
end;

// Clears a single dimesion char value array by setting length to zero length.
procedure ClearCharArray(var arrayChar: T1DCharArray);
begin
  if Length(arrayChar) > 0 then
    SetLength(arrayChar, 0);
end;

// Clears a two dimesional char value array by setting each one dimmensional column length to zero
// and finally setting the second indice row dimension to zero length.
procedure Clear2DCharArray(var arrayChar: TBinaryMatix);
var
  intUprInd,
  inx: integer;
begin
  if Length(arrayChar) > 0 then
  begin
    intUprInd := High(arrayChar);
    for inx := intUprInd downto 0 do
      ClearCharArray(arrayChar[inx]);
    SetLength(arrayChar, 0);
  end;
end;

// Clears a three dimesional char value array by setting calling Clear2DCharArray to clear each two
// dimmensional array and finally setting the top dimension or page to zero length.
procedure Clear3DCharArray(var arrayChar: TBSBinaryMatix);
var
  intUprInd,
  inx: integer;
begin
  try
    if Length(arrayChar) > 0 then
    begin
      intUprInd := High(arrayChar);
      for inx := intUprInd downto 0 do
        Clear2DCharArray(arrayChar[inx]);
      SetLength(arrayChar, 0);
    end;
  except
    on E:exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);                   //
  end;
end;

// Clear data from species binary/measurement records and arrays for a new
// calculation.
procedure ClearBinariesMeasurements(const boolNotExit: boolean);
var
//  D1X,
//  D2Y,
//  D3Z,
  inx: integer;
begin
  try
    Clear3DDoubleArray(DifferenceMatrix);
    Clear2DDoubleArray(DistanceMatrix);
    Clear3DDoubleArray(darBSDistanceMatrix);
    Clear3DDoubleArray(QMatrix);
    Clear2DIntegerArray(iarReplicates);
//    D1X := Length(darBtStrpRwDtMsrmnt);
//    if Length(darBtStrpRwDtMsrmnt) > 0 then
//    begin
//      D2Y := Length(darBtStrpRwDtMsrmnt[0]);
//      D3Z := Length(darBtStrpRwDtMsrmnt[0][0]);
//    end;
    Clear3DDoubleArray(darBtStrpRwDtMsrmnt);
//    if Length(darBtStrpRwDtMsrmnt) > 0 then
//    begin
//      darBtStrpRwDtMsrmnt.Free;
//      SetLength(darBtStrpRwDtMsrmnt, 1);
//      SetLength(darBtStrpRwDtMsrmnt, 0);
//    end;
    Clear3DCharArray(carBinary);
    Clear3DCharArray(carSequence);
    if Length(arstrNodeNames) > 0 then
      SetLength(arstrNodeNames, 0);
    if Length(arryinxBracket) > 0 then
      SetLength(arryinxBracket, 0);
    if Length(arstrConnected) > 0 then
      SetLength(arstrConnected, 0);
    if Length(arTplgyBrnch) > 0 then
      SetLength(arTplgyBrnch, 0);
    if Length(aTOTreePlot) > 0 then
      SetLength(aTOTreePlot, 0);
    if Length(aRectTreePlot) > 0 then
      SetLength(aRectTreePlot, 0);
    if Length(arTreeToken) > 0 then
      SetLength(arTreeToken, 0);
    if Length(arTreePlot) > 0 then
      SetLength(arTreePlot, 0);
    if Length(ariLengthPos) > 0 then
      SetLength(ariLengthPos, 0);
    if Length(astrBtStrp) > 0 then
      SetLength(astrBtStrp, 0);
    if Length(astrLengths) > 0 then
      SetLength(astrLengths, 0);
    with TestTableForSpeciesData do
    begin
      SpeciesTitle := '';
      SpeciesCount := 0;
      TestCount := 0;
      TierCount := 0;
      ErrorCount := 0;
      NodeCount := 0;
      LastNode := 0;
      inxQ := 0;
      dblSpltFctr := kSpltFctr;
      HiddenNodeName := '';
      HiddenPercent := dblZero;
      HiddenMatches := 0;
      if Length(TestTitle) > 0 then
        SetLength(TestTitle, 0);
      Clear2DDoubleArray(Measurement);
      Clear2DCharArray(Binary);
      Clear2DCharArray(Sequence);
      if Length(inxSpcsQM) > 0 then
        SetLength(inxSpcsQM, 0);
      if Length(IsActive) > 0 then
        SetLength(IsActive, 0);
      if Length(QIndex) > 0 then
        SetLength(QIndex, 0);
      if Length(BSPercent) > 0 then
        SetLength(BSPercent, 0);
      if Length(iarRplctMtchs) > 0 then
        SetLength(iarRplctMtchs, 0);
      if frmPhyIoM.trvwTreeGraph.Items.Count > 0 then
      begin
        for inx := 0 to frmPhyIoM.trvwTreeGraph.Items.Count - 1 do
          if frmPhyIoM.trvwTreeGraph.Items[inx].Data <> nil then
            Dispose(PTreeViewMember(frmPhyIoM.trvwTreeGraph.Items[inx].Data));
        frmPhyIoM.trvwTreeGraph.Items.Clear;
      end;
      if frmTreeListDown.trvwTextDownBased.Items.Count > 0 then
      begin
        for inx := 0 to frmTreeListDown.trvwTextDownBased.Items.Count - 1 do
          if frmTreeListDown.trvwTextDownBased.Items[inx].Data <> nil then
            Dispose(PTreeViewMember(frmTreeListDown.trvwTextDownBased.Items[inx].Data));
        frmTreeListDown.trvwTextDownBased.Items.Clear;
      end;
      strlsRootList.Clear;
      if boolNotExit then
//        if Species <> nil then
        if Length(Species) > 0 then
            SetLength(Species, 0)
//          else
        else
      else
      begin
        strlstCalcErrs.Free;
        strlstCalcErrs := nil;
        strlstArrayErrs.Free;
        strlstArrayErrs := nil;
        strlstDstncMtrx.Free;
        strlstDstncMtrx := nil;
        stlsRecTree.Free;
        stlsRecTree := nil;
        strlsRootList.Free;
        strlsRootList := nil;
      end;
      intRoot := -1;
      boolNewCalculation := True;
    end;
    dblSumLngth := dblZero;
    boolGraphInitial := True;
  except
    on E:exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
//  if boolNotExit then
//    MessageDlg('Clearing of data succesful.', mtInformation, [mbOK], 0);
end;

// Function returns integer value which is the highest NodeLevel value.  This
// represents the farthest Node from the root.
function intHighestLevel: integer;
var
  inx: integer;
begin
  Result := 0;
  with TestTableForSpeciesData do
    for inx := 0 to NodeCount - 2 do
      With Species[inx] do
        if NodeLevel > Result then
          Result := Nodelevel;
end;

{
Function replaces characters in a string with another set of characters to prevent
the string from being parsed incorrectly.  The first character is character that needs
to be covertered and the second character is what it will be converted to.  The odd characters
require conversion and the even character is what it is converted to.  This functions is used
to convert thing such as parantheses, colons, and semicolon that Newick to be read incorrectly.

Parameters:
  strSpcsNm -> string that requires conversion typically the species name
  strCnvrsnLst -> odd characters to be converted and even characters is the conversion result
  Result -> string that is converted from conversion string list
}
function strConversion(const strSpcsNm, strCnvrsnLst: string): string;
var
  inx,
  iny: integer;
begin
  Result := strSpcsNm;
  for inx := 1 to Length(strSpcsNm) do
    for iny := 1 to Length(strCnvrsnLst) div 2 do
      if Result[inx] = strCnvrsnLst[iny * 2 - 1] then
        Result[inx] := strCnvrsnLst[iny * 2];
end;

{
Function replaces characters to return it to its previous state before the conversion.
The even characters will be replacced with the odd character.  This is reverse of the
previous function.

Parameters:
  strSpcsNm -> string that requires conversion typically the species name
  strCnvrsnLst -> odd characters to be unconverted and even characters is the conversion result
  Result -> string that is unconverted from conversion string list
}
function strUndoConversion(const strSpcsNm, strCnvrsnLst: string): string;
var
  inx,
  iny: integer;
begin
  Result := strSpcsNm;
  for inx := 1 to Length(strSpcsNm) do
    for iny := 1 to Length(strCnvrsnLst) div 2 do
      if Result[inx] = strCnvrsnLst[iny * 2] then
        Result[inx] := strCnvrsnLst[iny * 2 - 1];
end;

{
Place data into the measrement records and data grid that was read into the memo field.  Place error
values in the grid with "<< >>" around the data and minus and number to obtain offending first digit.

Parameters:
   strFileName -> Name of data file
   prgBar -> Progress bar to show percentge of completion
   strgrdTD -> String grid to show data results for errors
}
procedure LoadMeasuremntData(const strFileName: string; var prgBar: TProgressBar; var strgrdTD: TStringGrid);
var
  MD: TStrings;           // Measurment data string list
  intComma,               // Position of first comma in string
  intCommaPos,            // Position of later commas in original full string
  int1stChar,             // Position of first character in data field
  intColErr,              // Column of first error in grid
  intRowErr,              // Row of first error in grid
  intLength,              // Length of string line data
  inxSpecies,             // Indices of species
  inxTests: integer;      // Indices of tests
  wrdError: word;         // Error code reported by Val procedure which is the invalid digit position
  strLineData: string;    // String for the cuurent line data read from memeo field
  boolShowError: boolean; // Boolean to show error when true
begin
  boolShowError := False;
  with TestTableForSpeciesData do
  try
    MD := TStringList.Create;
    try
      MD.LoadFromFile(strFileName);
      inxQ := 0;                         // Indices for QMatrix
      strLineData := MD[0];              // Read memo string into string variable
      intLength := Length(strLineData);  // Assign string length
      SpeciesCount := MD.Count - 1;      // Count of memo with title line removed
      NodeCount := SpeciesCount;
      LastNode := 0;
      TestCount := 1;
      inxTests := 0;                     // Used with zero base array
      ErrorCount := 0;                   // Clear error counter
      prgBar.Max := 100;
      prgBar.Position := 0;
      with strgrdTD do
      begin
        RowCount := SpeciesCount + 1;    // Set rows to number of species + 1
        ColCount := TestCount + 1;       // Set columns to 2 prior to column counting
      end;
      SetLength(TestTitle, TestCount);
      SetLength(Species, 2 * SpeciesCount - 1);
      SetLength(IsActive, 2 * SpeciesCount - 2);
      SetLength(QIndex, 2 * SpeciesCount - 2);
      for inxSpecies := 0 to 2 * SpeciesCount - 3 do                                              // For next through species
      begin
        Species[inxSpecies] := EmptySpecies;
        IsActive[inxSpecies] := True;
        QIndex[inxSpecies] := 0;
      end;
      intComma := Pos(',', strLineData); // Find first comma
      intCommaPos := intComma;           // Assign comma accumulator
      int1stChar := 1;                   // Point to first field
      if intComma > 0 then               // Test to see if it has any commas
                                         //         v int1stCharPos=8
                                         //               v intCommaPos=14
      begin                              //  xxxxxx,yyyyyy,zzzzzzz,aaaaaaaa  <--- strLineData
                                         //  Copy--|      ^ intComma=7      | intCommaPos-int1stChar=6
                                         // |-------------------------------| End of string intLength=27
        SpeciesTitle := Copy(strLineData, int1stChar, intCommaPos - int1stChar);  // Assign species title field
        strgrdTD.Cells[0, 0] := SpeciesTitle;                                     // Put title in grid
        int1stChar := intComma + 1;                                               // Find start of next string field
        intComma := Pos(',', Copy(strLineData, int1stChar, intLength - intCommaPos)); // Find next comma
        intCommaPos := intCommaPos + intComma;                                        // Accumulate comma position to work with full string
        while intComma > 1 do                                                         // Test for commas left in string
        begin
          if TestTitle[inxTests] <> '' then
            TestTitle[inxTests] := TestTitle[inxTests] + Copy(strLineData, int1stChar, intCommaPos - int1stChar) // Assign copy of string and keep previous test to Test title
          else
            TestTitle[inxTests] := Copy(strLineData, int1stChar, intCommaPos - int1stChar); // Assign copy of string to Test title
          strgrdTD.Cells[inxTests + 1, 0] := TestTitle[inxTests];                         // Assign copy of string to grid
          int1stChar := intCommaPos + 1;                                                  // Prepare to search for next comma
          intComma := Pos(',', Copy(strLineData, int1stChar, intLength - intCommaPos));   // Find comma
          intCommaPos := intCommaPos + intComma;                                          // Accumulate comma position for copy extraction
          inxTests := inxTests + 1;                                                       // Increment test
          TestCount := inxTests + 1;                                                      // Increment test count
          SetLength(TestTitle, TestCount);                                                // Extend length of Test titles
          strgrdTD.ColCount := TestCount + 1;                                              // Extend grid one column
        end;
        if int1stChar <= intLength then                                                   // Test if int1stChar before end of sring
        begin
          TestTitle[inxTests] := Copy(strLineData, int1stChar, intLength - int1stChar + 1);  // Add last Test title in line
          strgrdTD.Cells[inxTests + 1, 0] := TestTitle[inxTests];                            // Add title to grid
        end;
        prgBar.Max := strgrdTD.ColCount * strgrdTD.RowCount;
        prgBar.Position := strgrdTD.ColCount;
      end;
      SetLength(Measurement, SpeciesCount, TestCount);
      SetLength(arryinxBracket, 1);
      SetLength(arryinxBracket[0], SpeciesCount);
      for inxSpecies := 0 to SpeciesCount - 1 do                                              // For next through species
        with Species[inxSpecies] do                                                           // Use Species array qualifier
        begin
          Leaves := 1;
          inxBracket := kintInvalidBracket;
          arryinxBracket[0, inxSpecies] := kintInvalidBracket;
          strLineData := MD[inxSpecies + 1];                                // Read memo string into string variable
          intLength := Length(strLineData);                                 // Assign string length
          intComma := Pos(',', strLineData);                                // Find first comma
          intCommaPos := intComma;                                          // Assign comma accumulator
          int1stChar := 1;                                                  // Point to first field
          SpeciesName := strConversion(Copy(strLineData, 1, intCommaPos - int1stChar), kLabelCoversion);    // Assign species name field
          strgrdTD.Cells[0, inxSpecies + 1] := SpeciesName;                 // Assign species name to grid
          inxTests := 0;                                                    // Set Test index to zero
          prgBar.Position := prgBar.Position + 1;
          int1stChar := intCommaPos + 1;                                    // Point to first field
          intComma := Pos(',', Copy(strLineData, int1stChar, intLength - intCommaPos));  // Find firsr comma
          intCommaPos := intCommaPos + intComma;
          while intComma > 0 do                                                          // Test for commas left in string
          begin
            if inxTests + 1 > TestCount then
            begin
              ErrorCount := ErrorCount + 1;                                 // Increment error counter
              if ErrorCount = 1 then
              begin
                boolShowError := True;
                strlstCalcErrs.Add(strErrorHdrFtr('', 'Measurement', True));
              end;
              strlstCalcErrs.Add(IntToStr(TestCount) + ' Tests were expected and ' + IntToStr(inxTests + 1) + ' tests were processed for species ' + SpeciesName + '.');
              break;
            end;
            if strLineData[int1stChar] = kMissingData then
            begin
              Measurement[inxSpecies, inxTests] := kInvalidData;
              strgrdTD.Cells[inxTests + 1, inxSpecies + 1] := strLineData[int1stChar];   // Add missing measurement to grid.
            end
            else
            begin
              Val(Copy(strLineData, int1stChar, intCommaPos - int1stChar), Measurement[inxSpecies, inxTests], wrdError);  // Convert measurement from string to double
              if wrdError = 0 then
                strgrdTD.Cells[inxTests + 1, inxSpecies + 1] := Copy(strLineData, int1stChar, intCommaPos - int1stChar)   // Add measurement to grid.
              else
              begin
                ErrorCount := ErrorCount + 1;                                                                                                                        // Increment error counter
                strgrdTD.Cells[inxTests + 1, inxSpecies + 1] := '**' + Copy(strLineData, int1stChar, intCommaPos - int1stChar) + '**@' + IntToStr(wrdError);       // Add measurment to grid with error data.
                if ErrorCount = 1 then
                begin
                  boolShowError := True;
                  intColErr := inxTests + 1;
                  intRowErr := inxSpecies + 1;
                  rctSelectedCell.Left := intColErr;
                  rctSelectedCell.Top := intRowErr;
                  rctSelectedCell.Right := intColErr;
                  rctSelectedCell.Bottom := intRowErr;
                  strlstCalcErrs.Add(strErrorHdrFtr('', 'Measurement', True));
                end;
                strlstCalcErrs.Add('Test: ' + TestTitle[inxTests] + ', Species: ' + Species[inxSpecies].SpeciesName + ', and Data: ' + Copy(strLineData, int1stChar, intCommaPos - int1stChar));
                Measurement[inxSpecies, inxTests] := kInvalidData;                                                                                                             // Place invalid data in measurement data.
              end;
            end;
            int1stChar := intCommaPos + 1;                                                                                                                       // Point to start of next field
            intComma := Pos(',', Copy(strLineData, int1stChar, intLength - intCommaPos));                                                                        // Find next comma
            intCommaPos := intCommaPos + intComma;                                                                                                               // Assign comma accumulator
            prgBar.Position := prgBar.Position + 1;
            inxTests := inxTests + 1;                                                                                                                            // Increment tests
          end;
          if int1stChar <= intLength then                                                                                                                        // Add last measurement in line
          begin
            if inxTests + 1 > TestCount then
            begin
              ErrorCount := ErrorCount + 1;                                                                                                                      // Increment error counter
              if ErrorCount = 1 then
              begin
                strlstCalcErrs.Add(strErrorHdrFtr('', 'Measurement', True));
                boolShowError := True;
              end;
              strlstCalcErrs.Add(IntToStr(TestCount) + ' Tests were expected and ' + IntToStr(inxTests + 1) + ' tests were processed for species ' + SpeciesName + '.');
            end
            else
            begin
              if strLineData[int1stChar] = kMissingData then
              begin
                Measurement[inxSpecies, inxTests] := kInvalidData;
                strgrdTD.Cells[inxTests + 1, inxSpecies + 1] := strLineData[int1stChar];   // Add missing measurement to grid.
              end
              else
              begin
                Val(Copy(strLineData,  int1stChar, intLength - int1stChar + 1), Measurement[inxSpecies, inxTests], wrdError);                                        // Convert measurement from string to double
                if wrdError = 0 then                                                                                                                                 // Test for invalid data
                  strgrdTD.Cells[inxTests + 1, inxSpecies + 1] := Copy(strLineData, int1stChar, intLength - int1stChar + 1)
                else                                                                                                                                                 // Add measurment to grid
                begin
                  ErrorCount := ErrorCount + 1;                                                                                                                      // Increment error counter
                  strgrdTD.Cells[inxTests + 1, inxSpecies + 1] := '**' + Copy(strLineData, int1stChar, intLength - int1stChar + 1) + '**@' + IntToStr(wrdError);     // Add measurment to grid with error data.
                  if ErrorCount = 1 then
                  begin
                    boolShowError := True;
                    intColErr := inxTests + 1;
                    intRowErr := inxSpecies + 1;
                    rctSelectedCell.Left := intColErr;
                    rctSelectedCell.Top := intRowErr;
                    rctSelectedCell.Right := intColErr;
                    rctSelectedCell.Bottom := intRowErr;
                    strlstCalcErrs.Add(strErrorHdrFtr('', 'Measurement', True));
                  end;
                  strlstCalcErrs.Add('Test: ' + TestTitle[inxTests] + ', Species: ' + Species[inxSpecies].SpeciesName + ', and Data: ' + Copy(strLineData, int1stChar, intLength - int1stChar + 1));
                  Measurement[inxSpecies, inxTests] := kInvalidData;  // Place invalid data in measurement data.
                end;
              end;
            end;
            prgBar.Position := prgBar.Position + 1;
          end;
          if inxTests + 1 < TestCount then
          begin
            ErrorCount := ErrorCount + 1;                           // Increment error counter
            if ErrorCount = 1 then
            begin
              strlstCalcErrs.Add(strErrorHdrFtr('', 'Measurement', True));
              boolShowError := True;
            end;
            strlstCalcErrs.Add(IntToStr(TestCount) + ' Tests were expected and ' + IntToStr(inxTests + 1) + ' tests were processed for species ' + SpeciesName + '.');
          end;
        end;
    finally
      MD.Free;
      strgrdTD.AutoSizeColumns;
    end;
  except
    on E:exception do
    begin
      ErrorCount := ErrorCount + 1;                                // Increment error counter
      if ErrorCount = 1 then
      begin
        strlstCalcErrs.Add(strErrorHdrFtr('', 'Measurement', True));
        boolShowError := True;
      end;
      strlstCalcErrs.Add(E.Message);
      MessageDlg(E.Message, mtError, [mbOK], 0);                   //
    end;
  end;
  if boolShowError then
  begin
    strlstCalcErrs.Add(strErrorHdrFtr('', 'Measurement', False));
    modCalcErrs.frmCalculatedErrors.Show;
    frmPhyIoM.mnuShowCalcErrs.Enabled := True;
  end;
end;

{
Place data into the binary data records and data grid that was read into the memo field.  Place error
values in the grid with "** **" around the data.

Parameters:
   strFileName -> Name of data file
   strgrdTD -> String to show data results for errors
   BinaryType -> Binary data type array
   prgBar -> Progress bar to show percentge of completion
}
procedure LoadBinaryData(const strFileName: string; var prgBar: TProgressBar; var strgrdTD: TStringGrid; const BinaryType: TBinaryDataType);
var
  BD: TStrings;           // Binary -/+ or 0/1 data string list
  intComma,               // Position of first comma in string
  intCommaPos,            // Position of later commas in original full string
  int1stChar,             // Position of first character in data field
  intColErr,              // Column of first error in grid
  intRowErr,              // Row of first error in grid
  intLength,              // Length of string line data
  inxSpecies,             // Indices of species
  inxTests: integer;      // Indices of tests
  strBinaryType,
  strLineData: string;    // String for the cuurent line data read from memo field
  boolShowError: boolean; // Boolean to show error when true
begin
  boolShowError := False;
  with TestTableForSpeciesData do
  try
    BD := TStringList.Create;
    try
      BD.LoadFromFile(strFileName);
      inxQ := 0;                         // Indices for QMatrix
      strLineData := BD[0];              // Read memo string into string variable
      intLength := Length(strLineData);  // Assign string length
      SpeciesCount := BD.Count - 1;      // Count of memo with title line removed
      if '-' in BinaryType then
        strBinaryType := '-/+'
      else
        strBinaryType := '0/1';
      NodeCount := SpeciesCount;
      LastNode := 0;
      TestCount := 1;
      inxTests := 0;                  // Used with zero base array
      ErrorCount := 0;                // Clear error counter
      prgBar.Max := 100;
      prgBar.Position := 0;
      with strgrdTD do
      begin
        RowCount := SpeciesCount + 1;       // Set rows to number of species + 1
        ColCount := TestCount + 1;          // Set columns to 2 prior to column counting
      end;
      SetLength(TestTitle, TestCount);
      SetLength(Species, 2 * SpeciesCount - 1);
      SetLength(IsActive, 2 * SpeciesCount - 2);
      SetLength(QIndex, 2 * SpeciesCount - 2);
      for inxSpecies := 0 to 2 * SpeciesCount - 3 do // For next through species
      begin
        Species[inxSpecies] := EmptySpecies;
        IsActive[inxSpecies] := True;
        QIndex[inxSpecies] := 0;
      end;
      strLineData := BD[0];                 // Read memo string into string variable
      intLength := Length(strLineData);     // Set columns to 2 prior to column counting
      intComma := Pos(',', strLineData);    // Find first comma
      intCommaPos := intComma;              // Assign comma accumulator
      int1stChar := 1;                      // Point to first field
      if intComma > 0 then                  // Test to see if it has any commas
      begin
        SpeciesTitle := Copy(strLineData, int1stChar, intCommaPos - int1stChar);          // Assign species title field
        strgrdTD.Cells[0, 0] := SpeciesTitle;                                             // Put title in grid
        int1stChar := intComma + 1;                                                       // Find start of next field
        intComma := Pos(',', Copy(strLineData, int1stChar, intLength - intCommaPos));     // Find next comma
        intCommaPos := intCommaPos + intComma;                                            // Accumulate position to work with full string
        while intComma > 0 do                                                             // Test for commas left in string
        begin
          TestTitle[inxTests] := Copy(strLineData, int1stChar, intCommaPos - int1stChar); // Assign copy of string to Test title
          strgrdTD.Cells[inxTests + 1, 0] := TestTitle[inxTests];                         // Assign copy of string to grid
          int1stChar := intCommaPos + 1;                                                  // Prepare to search next comma
          intComma := Pos(',', Copy(strLineData, int1stChar, intLength - intCommaPos));   // Find comma
          intCommaPos := intCommaPos + intComma;                                          // Accumulate comma position for copy extraction
          inxTests := inxTests + 1;                                                       // Increment tests
          TestCount := inxTests + 1;                                                      // Increment test count
          SetLength(TestTitle, TestCount);                                                // Extend length of Test titles
          strgrdTD.ColCount := TestCount + 1;                                             // Extend grid column
        end;
        if int1stChar <= intLength then                                                   // Test if int1stChar before end of string
        begin
          TestTitle[inxTests] := Copy(strLineData, int1stChar, intLength - int1stChar + 1);   // Add last test tile in line
          strgrdTD.Cells[inxTests + 1, 0] := TestTitle[inxTests];                             // Add title to grid
        end;
        prgBar.Max := strgrdTD.ColCount * strgrdTD.RowCount;
        prgBar.Position := strgrdTD.ColCount;
      end;
      SetLength(Binary, SpeciesCount, TestCount);
      SetLength(arryinxBracket, 1);
      SetLength(arryinxBracket[0], SpeciesCount);
      for inxSpecies := 0 to SpeciesCount - 1 do                                              // For next through species
        with Species[inxSpecies] do
        begin
          Leaves := 1;
          inxBracket := kintInvalidBracket;
          arryinxBracket[0, inxSpecies] := kintInvalidBracket;
          strLineData := BD[inxSpecies + 1];                                                  // Read memo string intostring variable
          intLength := Length(strLineData);
          intComma := Pos(',', strLineData);                                                  // Find first comma
          intCommaPos := intComma;                                                            // Assign comma accumulator
          int1stChar := 1;                                                                    // Point to first field position
          SpeciesName := strConversion(Copy(strLineData, 1, intCommaPos - int1stChar), kLabelCoversion);  // Assign species name field
          strgrdTD.Cells[0, inxSpecies + 1] := SpeciesName;                                   // Assign species name to grid
          inxTests := 0;                                                                      // Set Test index to zero
          prgBar.Position := prgBar.Position + 1;
          int1stChar := intCommaPos + 1;                                                      // Point to first field
          intComma := Pos(',', Copy(strLineData, int1stChar, intLength - intCommaPos));       // Find next comma
          intCommaPos := intCommaPos + intComma;                                              // Assign comma accumulator
          while intComma > 0 do                                                               // Test for a comma left in string
          begin
            if inxTests + 1 > TestCount then
            begin
              ErrorCount := ErrorCount + 1;                                                                                                                      // Increment error counter
              if ErrorCount = 1 then
              begin
                strlstCalcErrs.Add(strErrorHdrFtr('', 'Binary ' + strBinaryType, True));
                boolShowError := True;
              end;
              strlstCalcErrs.Add(IntToStr(TestCount) + ' Tests were expected and ' + IntToStr(inxTests + 1) + ' tests were processed for species ' + SpeciesName + '.');
              break;
            end;
            if strLineData[int1stChar] in (BinaryType + [kMissingData]) then                           // Test if data is valid
            begin
              Binary[inxSpecies, inxTests] := strLineData[int1stChar];                        // Assign data to binary result
              strgrdTD.Cells[inxTests + 1, inxSpecies + 1] := Binary[inxSpecies, inxTests];   // Assign data to grid
            end
            else
            begin
              Binary[inxSpecies, inxTests] := '*';                                                        // Assign error data to binary result
              strgrdTD.Cells[inxTests + 1, inxSpecies + 1] := kInvalidBinary + strLineData[int1stChar] + kInvalidBinary;  // Assign error data to grid
              ErrorCount := ErrorCount + 1;                                                     // Increment error counter
              if ErrorCount = 1 then
              begin
                intColErr := inxTests + 1;
                intRowErr := inxSpecies + 1;
                rctSelectedCell.Left := intColErr;
                rctSelectedCell.Top := intRowErr;
                rctSelectedCell.Right := intColErr;
                rctSelectedCell.Bottom := intRowErr;
                strlstCalcErrs.Add(strErrorHdrFtr('', 'Binary ' + strBinaryType, True));
                boolShowError := True;
              end;
              strlstCalcErrs.Add('Test: ' + TestTitle[inxTests] + ', Species: ' + Species[inxSpecies].SpeciesName + ', and Data: ' + strLineData[int1stChar]);
            end;
            int1stChar := intCommaPos + 1;                                                    // Point to first field
            intComma := Pos(',', Copy(strLineData, int1stChar, intLength - intCommaPos));     // Find next comma
            intCommaPos := intCommaPos + intComma;                                            // Assign comma accumulator
            inxTests := inxTests + 1;                                                         // Increment inxTest
            prgBar.Position := prgBar.Position + 1;
          end;
          if int1stChar <= intLength then                                                     // Test if int1stChar before end of string
            if inxTests + 1 > TestCount then
            begin
              ErrorCount := ErrorCount + 1;                                                                                                                      // Increment error counter
              if ErrorCount = 1 then
              begin
                strlstCalcErrs.Add(strErrorHdrFtr('', 'Binary ' + strBinaryType, True));
                boolShowError := True;
              end;
              strlstCalcErrs.Add(IntToStr(TestCount) + ' Tests were expected and ' + IntToStr(inxTests + 1) + ' tests were processed for species ' + SpeciesName + '.');
            end
            else
            begin
              if strLineData[intLength] in (BinaryType + [kMissingData]) then                    // Test if data is valid
              begin
                Binary[inxSpecies, inxTests] := strLineData[intLength];                          // Assign data to binary result
                strgrdTD.Cells[inxTests + 1, inxSpecies + 1] := Binary[inxSpecies, inxTests];    // Assign data to grid
              end
              else
              begin
                Binary[inxSpecies, inxTests] := '*';                                                        // Assign error data to binary result
                strgrdTD.Cells[inxTests + 1, inxSpecies + 1] := kInvalidBinary + strLineData[intLength] + kInvalidBinary;  // Assign error data to grid
                ErrorCount := ErrorCount + 1;                                                     // Increment error counter
                if ErrorCount = 1 then
                begin
                  intColErr := inxTests + 1;
                  intRowErr := inxSpecies + 1;
                  rctSelectedCell.Left := intColErr;
                  rctSelectedCell.Top := intRowErr;
                  rctSelectedCell.Right := intColErr;
                  rctSelectedCell.Bottom := intRowErr;
                  strlstCalcErrs.Add(strErrorHdrFtr('', 'Binary ' + strBinaryType, True));
                  boolShowError := True;
                end;
                strlstCalcErrs.Add('Test: ' + TestTitle[inxTests] + ', Species: ' + Species[inxSpecies].SpeciesName + ', and Data: ' + strLineData[int1stChar]);
              end;
            end;
          prgBar.Position := prgBar.Position + 1;
          if inxTests + 1 < TestCount then
          begin
            ErrorCount := ErrorCount + 1;                                   // Increment error counter
            if ErrorCount = 1 then
            begin
              strlstCalcErrs.Add(strErrorHdrFtr('', 'Binary ' + strBinaryType, True));
              boolShowError := True;
            end;
            strlstCalcErrs.Add(IntToStr(TestCount) + ' Tests were expected and ' + IntToStr(inxTests + 1) + ' tests were processed for species ' + SpeciesName + '.');
          end;
        end;
    finally
      BD.Free;
      strgrdTD.AutoSizeColumns;
    end;
  except
    on E:exception do
    begin
      ErrorCount := ErrorCount + 1;                    // Increment error counter
      if ErrorCount = 1 then
      begin
        strlstCalcErrs.Add(strErrorHdrFtr('', 'Binary ' + strBinaryType, True));
        boolShowError := True;
      end;
      strlstCalcErrs.Add(E.Message);
      MessageDlg(E.Message, mtError, [mbOK], 0);       //
    end;
  end;
  if boolShowError then
  begin
    frmPhyIoM.mnuShowCalcErrs.Enabled := True;
    strlstCalcErrs.Add(strErrorHdrFtr('', 'Binary ' + strBinaryType, False));
    modCalcErrs.frmCalculatedErrors.Show;
  end;
end;

{
Place data into the sequence records and data grid that was read into the memo field.  Place error
messages when data lengths are different from first record.  This is comma delimited character values.

Parameters:
   strFileName -> Name of data file
   prgBar -> Progress bar to show percentge of completion
   strgrdTD -> String grid to show data results for errors
}
procedure LoadSequenceData(const strFileName: string; var prgBar: TProgressBar; var strgrdTD: TStringGrid);
var
  SD: TStrings;           // Sequece data string list in A,G,T,C or letter of protein
//  intComma,               // Position of first comma in string
//  intCommaPos,            // Position of later commas in original full string
//  int1stChar,             // Position of first character in data field
//  intColErr,              // Column of first error in grid
//  intRowErr,              // Row of first error in grid
  intLength,              // Length of string line data
  inxMmmLns,              // Indices of memo lines
  inxSpecies,             // Indices of species
  inxTests: integer;      // Indices of tests
  strSpeciesName,         // String for Species Name
  strLineData: string;    // String for the cuurent line data read from memo field
  boolShowError: boolean; // Boolean to show error when true
begin
  boolShowError := False;
  with TestTableForSpeciesData do
  try
    SD := TStringList.Create;
    try
      SpeciesTitle := 'Sequence Data';       // Assign species title field
      strgrdTD.Cells[0, 0] := SpeciesTitle;  // Put title in grid
      SD.LoadFromFile(strFileName);
      inxQ := 0;                             // Indices for QMatrix
      SpeciesCount := SD.Count div 2;        // Count of memo with titles line removed
      NodeCount := SpeciesCount;
      strSpeciesName := SD[0];               // Read species name string into string variable
      strLineData := SD[1];                  // Read sequence data line string
      intLength := Length(strLineData);      // Assign string length
      TestCount := intLength div 2 + 1;
      ErrorCount := 0;                // Clear error counter
      prgBar.Max := 100;
      prgBar.Position := 0;
      with strgrdTD do
      begin
        RowCount := SpeciesCount + 1;       // Set rows to number of species + 1
        ColCount := TestCount + 1;          // Set columns to 2 prior to column counting
      end;
      prgBar.Max := TestCount * (SpeciesCount * 1);
      SetLength(TestTitle, TestCount);
      SetLength(Species, 2 * SpeciesCount - 1);
      SetLength(IsActive, 2 * SpeciesCount - 2);
      SetLength(QIndex, 2 * SpeciesCount - 2);
      Species[0] := EmptySpecies;
      Species[0].SpeciesName := strSpeciesName;
      IsActive[0] := True;
      QIndex[0] := 0;
      SetLength(Sequence, SpeciesCount, TestCount);
      SetLength(arryinxBracket, 1);
      SetLength(arryinxBracket[0], SpeciesCount);
      strgrdTD.Cells[0, 1] := Species[0].SpeciesName;
      for inxTests := 0 to TestCount - 1 do
      begin
        TestTitle[inxTests] := 'S_' + IntToStr(inxTests + 1);
        strgrdTD.Cells[inxTests + 1, 0] := TestTitle[inxTests];         // Assign test title to grid
        Sequence[0, inxTests] := strLineData[inxTests * 2 + 1];
        strgrdTD.Cells[inxTests + 1, 1] := Sequence[0, inxTests];      // Assign data to grid
        prgBar.Position := prgBar.Position + 1;
      end;
      prgBar.Position := prgBar.Position + 1;
      for inxSpecies := 1 to 2 * SpeciesCount - 3 do                  // For next through species
      begin
        Species[inxSpecies] := EmptySpecies;
        IsActive[inxSpecies] := True;
        QIndex[inxSpecies] := 0;
      end;
      for inxSpecies := 1 to SpeciesCount - 1 do
      begin
        inxMmmLns := inxSpecies * 2;
        Species[inxSpecies].SpeciesName := strConversion(SD[inxMmmLns], kLabelCoversion);  // Read species name string into string variable
        strgrdTD.Cells[0, inxSpecies + 1] := Species[inxSpecies].SpeciesName;
        strLineData := SD[inxMmmLns + 1];                           // Read sequence data line string
        if Length(strLineData) <> intLength then
        begin
          ErrorCount := ErrorCount + 1;                                                     // Increment error counter
          if ErrorCount = 1 then
          begin
            strlstCalcErrs.Add(strErrorHdrFtr('', 'Sequence .csv', True)); // Place header at beginning of CalcErrs string list.
            boolShowError := True;
          end;
          if Length(strLineData) < intLength then
            strlstCalcErrs.Add('Test data length shorter than expected for ' + Species[inxSpecies].SpeciesName + '.')
          else
            strlstCalcErrs.Add('Test data length longer than expected for ' + Species[inxSpecies].SpeciesName + '.');
        end
        else
          for inxTests := 0 to TestCount - 1 do
          begin
            Sequence[inxSpecies, inxTests] := strLineData[inxTests * 2 + 1];
            strgrdTD.Cells[inxTests + 1, inxSpecies + 1] := Sequence[inxSpecies, inxTests];    // Assign data to grid
            prgBar.Position := prgBar.Position + 1;
          end;
        prgBar.Position := prgBar.Position + 1;
      end;
    finally
      SD.Free;
      strgrdTD.AutoSizeColumns;
    end;
  except
    on E:exception do
    begin
      ErrorCount := ErrorCount + 1;                    // Increment error counter
      if ErrorCount = 1 then
      begin
        strlstCalcErrs.Add(strErrorHdrFtr('', 'Sequence', True)); // Place header at beginning of CalcErrs string list.
        boolShowError := True;
      end;
      strlstCalcErrs.Add(E.Message);                   // Add error message to CalcErrs string list
      MessageDlg(E.Message, mtError, [mbOK], 0);       // Display error message in a Dialog box.
    end;
  end;
  if boolShowError then
  begin
    frmPhyIoM.mnuShowCalcErrs.Enabled := True;
    strlstCalcErrs.Add(strErrorHdrFtr('', 'Sequence .csv', False)); // Place footer at beginning of CalcErrs string list.
    modCalcErrs.frmCalculatedErrors.Show;
  end;
end;

{
Place data into the sequence records and data grid that was read into the memo field.  Place error
messages when data lengths are different from first record.  This is not comma delimited but adjacent charactera.

Parameters:
   strFileName -> Name of data file
   prgBar -> Progress bar to show percentge of completion
   strgrdTD -> String grid to show data results for errors
}
procedure LoadFastaSequenceData(const strFileName: string; var prgBar: TProgressBar; var strgrdTD: TStringGrid);
var
  SD: TStrings;           // Sequece data string list in A,G,T,C or letter of protein
//  intComma,               // Position of first comma in string
//  intCommaPos,            // Position of later commas in original full string
//  int1stChar,             // Position of first character in data field
//  intColErr,              // Column of first error in grid
//  intRowErr,              // Row of first error in grid
  intLength,              // Length of string line data
  inxMmmLns,              // Indices of memo lines
  inxSpecies,             // Indices of species
  inxTests: integer;      // Indices of tests
  strSpeciesName,         // String for Species Name
  strLineData: string;    // String for the cuurent line data read from memo field
  boolShowError: boolean; // Boolean to show error when true
begin
  boolShowError := False;
  with TestTableForSpeciesData do
  try
    SD := TStringList.Create;
    try
      SpeciesTitle := 'Sequence Data';       // Assign species title field
      strgrdTD.Cells[0, 0] := SpeciesTitle;  // Put title in grid
      SD.LoadFromFile(strFileName);
      inxQ := 0;                             // Indices for QMatrix
      SpeciesCount := SD.Count div 2;        // Count of memo with titles line removed
      NodeCount := SpeciesCount;
      strSpeciesName := Copy(SD[0], 2, Length(SD[0]) - 1) ;// Read species name string into string variable
      strLineData := SD[1];                   // Read sequence data line string
      intLength := Length(strLineData);      // Assign string length
      TestCount := intLength;
      ErrorCount := 0;                // Clear error counter
      prgBar.Max := 100;
      prgBar.Position := 0;
      with strgrdTD do
      begin
        RowCount := SpeciesCount + 1;       // Set rows to number of species + 1
        ColCount := TestCount + 1;          // Set columns to 2 prior to column counting
      end;
      prgBar.Max := TestCount * (SpeciesCount * 1);
      SetLength(TestTitle, TestCount);
      SetLength(Species, 2 * SpeciesCount - 1);
      SetLength(IsActive, 2 * SpeciesCount - 2);
      SetLength(QIndex, 2 * SpeciesCount - 2);
      Species[0] := EmptySpecies;
      Species[0].SpeciesName := strSpeciesName;
      IsActive[0] := True;
      QIndex[0] := 0;
      SetLength(Sequence, SpeciesCount, TestCount);
      SetLength(arryinxBracket, 1);
      SetLength(arryinxBracket[0], SpeciesCount);
      strgrdTD.Cells[0, 1] := Species[0].SpeciesName;
      for inxTests := 0 to TestCount - 1 do
      begin
        TestTitle[inxTests] := 'S_' + IntToStr(inxTests + 1);
        strgrdTD.Cells[inxTests + 1, 0] := TestTitle[inxTests];         // Assign test title to grid
        Sequence[0, inxTests] := strLineData[inxTests + 1];
        strgrdTD.Cells[inxTests + 1, 1] := Sequence[0, inxTests];      // Assign data to grid
        prgBar.Position := prgBar.Position + 1;
      end;
      prgBar.Position := prgBar.Position + 1;
      for inxSpecies := 1 to 2 * SpeciesCount - 3 do                  // For next through species
      begin
        Species[inxSpecies] := EmptySpecies;
        IsActive[inxSpecies] := True;
        QIndex[inxSpecies] := 0;
      end;
      for inxSpecies := 1 to SpeciesCount - 1 do
      begin
        inxMmmLns := inxSpecies * 2;
        Species[inxSpecies].SpeciesName := strConversion(Copy(SD[inxMmmLns], 2, Length(SD[inxMmmLns]) - 1), kLabelCoversion); // Read species name string into string variable
        strgrdTD.Cells[0, inxSpecies + 1] := Species[inxSpecies].SpeciesName;
        strLineData := SD[inxMmmLns + 1];                           // Read sequence data line string
        if Length(strLineData) <> intLength then
        begin
          ErrorCount := ErrorCount + 1;                                                     // Increment error counter
          if ErrorCount = 1 then
          begin
            strlstCalcErrs.Add(strErrorHdrFtr('', 'Sequence Fasta', True));
            boolShowError := True;
          end;
          if Length(strLineData) < intLength then
            strlstCalcErrs.Add('Test data length shorter than expected for ' + Species[inxSpecies].SpeciesName + '.')
          else
            strlstCalcErrs.Add('Test data length longer than expected for ' + Species[inxSpecies].SpeciesName + '.');
        end
        else
          for inxTests := 0 to TestCount - 1 do
          begin
            Sequence[inxSpecies, inxTests] := strLineData[inxTests + 1];
            strgrdTD.Cells[inxTests + 1, inxSpecies + 1] := Sequence[inxSpecies, inxTests];    // Assign data to grid
            prgBar.Position := prgBar.Position + 1;
          end;
        prgBar.Position := prgBar.Position + 1;
      end;
    finally
      SD.Free;
      strgrdTD.AutoSizeColumns;
    end;
  except
    on E:exception do
    begin
      ErrorCount := ErrorCount + 1;                    // Increment error counter
      if ErrorCount = 1 then
      begin
        strlstCalcErrs.Add(strErrorHdrFtr('', 'Sequence Fasta', True));
        boolShowError := True;
      end;
      strlstCalcErrs.Add(E.Message);
      MessageDlg(E.Message, mtError, [mbOK], 0);       //
    end;
  end;
  if boolShowError then
  begin
    frmPhyIoM.mnuShowCalcErrs.Enabled := True;
    strlstCalcErrs.Add(strErrorHdrFtr('', 'Sequence', False));
    modCalcErrs.frmCalculatedErrors.Show;
  end;
end;

{
Build difference matrix for species matching remaining species in test column.
Matrix has the form top species in books, remaining species in rows, and tests in columns.
The difference is first species with deifference against other remaining species.
Arrays: MM = Matching Matrix and DM = Difference Matrix

Parameters:
  prgBar -> Progress bar to show progress of procedure execution of building distance matrix
  mmoRslts -> Memo field to show result of left bottom distance matrix after calculation is completed.
  MM -> 2D array of double Measuremwnt Data values taxa by tests
  DX -> 3D array of double for Difference data values taxa page by taxa row by tests column
  boolShowBar -> boolean to tell if progress bar values should be updated
}
procedure BuildDifferenceMatrix(var prgBar: TProgressBar; var MM: TMeasurementMatrix; var DM: T3DDoubleArray; const boolShowBar: boolean);
var
  inxSpecies,             // Indices of species
  inxNextSpecies,         // Indices of different species
  inxTests: integer;      // Indices of tests

begin
  with TestTableForSpeciesData do
  begin
    prgBar.Position := 0;
    prgBar.Max := (SpeciesCount - 1) * (SpeciesCount - 1) * (TestCount - 1) + 1;
    SetLength(DM, SpeciesCount - 1, SpeciesCount - 1, TestCount);
    for inxSpecies := 0 to SpeciesCount - 2 do
      for inxNextSpecies := inxSpecies + 1 to SpeciesCount - 1 do
        for inxTests := 0 to TestCount - 1 do
        begin
          if (MM[inxSpecies ,inxTests] <> kInvalidData) and (MM[inxNextSpecies, inxTests] <> kInvalidData) then
            DM[inxSpecies, inxNextSpecies - 1, inxTests] := Abs(MM[inxSpecies ,inxTests] - MM[inxNextSpecies, inxTests])
          else
            DM[inxSpecies, inxNextSpecies - 1, inxTests] := kInvalidData;
          if boolShowBar then
            prgBar.Position := prgBar.Position + 1;
        end;
  end;
end;

{
Build distance matrix from averaging difference matrix values. The difference matrix is based on differnt taxa
having rhe same test being subtrcted with an absolute value.  Then difference vallues are averaged for all tests
for one taxa against another taxa.

Parameters:
  prgBar -> Progress bar to show progress of procedure execution of building distance matrix
  mmoRslts -> Memo field to show result of left bottom distance matrix after calculation is completed.
  DM -> 3D array of double Difference Data values taxa by taxa by tests
  DX -> 2D array of double for Distance data values taxa row by taxa column
  boolShowBar -> boolean to tell if progress bar values should be updated
}
procedure AssignMeasurementDistanceMatrix(var prgBar: TProgressBar; var DM: T3DDoubleArray; var DX: T2DDoubleArray; const boolShowBar: boolean);
var
  inxSpecies,             // Indices of species
  inxNextSpecies,         // Indices of remaining species
  inxTests: integer;      // Indices of tests
  dblCount,               // Test count double precision
  dblSum: double;         // Sum of differences in a double number
  inx,
  iny: integer;
begin
  with TestTableForSpeciesData do
  begin
    for inxSpecies := 0 to SpeciesCount - 2 do
      for inxNextSpecies := inxSpecies + 1 to SpeciesCount - 1 do
      begin
        dblCount := dblZero;
        dblSum := dblZero;
        for inxTests := 0 to TestCount - 1 do
        begin
          if DM[inxSpecies, inxNextSpecies - 1, inxTests] <> kInvalidData then
          begin
            dblSum :=  dblSum + DM[inxSpecies, inxNextSpecies - 1, inxTests];
            dblCount := dblCount + 1.0;
          end;
          if boolShowBar then
            prgBar.Position := prgBar.Position + 1;
        end;
        if dblCount > 0 then
          DX[inxSpecies, inxNextSpecies] := dblSum / dblCount
        else
          DX[inxSpecies, inxNextSpecies] := dblZero;
      end;
    for inx := 0 to SpeciesCount - 1 do
      DX[inx, inx] := dblZero;
    for iny := 0 to SpeciesCount - 2 do
      for inx := iny + 1 to SpeciesCount - 1 do
        DX[inx, iny] := DX[iny, inx];
  end;
end;

{
Build distance matrix from averaging diffference matrix values. Procedure calls AssignMeasurementDistanceMatrix which actually
calculates the data and this procedure places data in the string list to make distance matrix result to show in a meem field
mmoResults.  This result is stored in left bottom form.

Parameters:
  prgBar -> Progress bar to show progress of procedure execution of building distance matrix
  mmoRslts -> Memo field to show result of left bottom distance matrix after calculation is completed.
}
procedure BuildMeasurementDistanceMatrix(var prgBar: TProgressBar; var mmoRslts: TMemo);
var
  inxSpecies,              // Indices of species
  inxNextSpecies: integer; // Indices of different species
  strWork: string;         // Working string to build memo line
begin
  with TestTableForSpeciesData do
  begin
    SetLength(DistanceMatrix, SpeciesCount, SpeciesCount);
    AssignMeasurementDistanceMatrix(prgBar, DifferenceMatrix, DistanceMatrix, True);
    strlstDstncMtrx.Clear;
    strWork := Speciestitle + ',';
    for inxSpecies := 0 to SpeciesCount - 1 do
    begin
      Species[inxSpecies].VrtclPosition.intVrtclLngth := 0;
      Species[inxSpecies].VrtclPosition.intVrtclSpc := 0;
      Species[inxSpecies].VerticalMode := Leaf;
      Species[inxSpecies].NodeTier := -1;
      strWork := strWork + Species[inxSpecies].SpeciesName;
      if inxSpecies <= SpeciesCount - 2 then
        strWork := strWork + ',';
      prgBar.Position := prgBar.Position + 1;
    end;
    strlstDstncMtrx.Add(strWork);
    strWork := Species[0].SpeciesName;
    strlstDstncMtrx.Add(strWork);
    for inxSpecies := 1 to SpeciesCount - 1 do
    begin
      strWork := Species[inxSpecies].SpeciesName + ',';
      for inxNextSpecies := 0 to inxSpecies - 1 do
      begin
        strWork := strWork + FloatToStrF(DistanceMatrix[inxNextSpecies, inxSpecies], ffFixed, 12, 6);
        if inxNextSpecies < inxSpecies - 1 then
          strWork := strWork + ',';
        prgBar.Position := prgBar.Position + 1;
       end;
      strlstDstncMtrx.Add(strWork);
    end;
    mmoRslts.Lines.Assign(strlstDstncMtrx);
  end;
end;

{
Build unmatching matrix for species matching remaining species in test column.
Matrix has the form top species in pages, remaining species in rows, and tests in columns.
Arrays: BM = Binary Matrix and MM = Unmatching Matrix

Parameters:
  prgBar -> Progress bar to show progress of procedure execution of building distance matrix
  mmoRslts -> Memo field to show result of left bottom distance matrix after calculation is completed.
  BM -> 2D array of double for binary data values taxa by tests
  MM -> 3D array of double Unmatching Data values taxa row by taxa column by tests
  boolShowBar -> boolean to tell if progress bar values should be updated
}
procedure BuildMatchingMatrix(var prgBar: TProgressBar; var BM: TBinaryMatix; var MM: TMatchingMatrix; const boolShowBar: boolean);
var
  inxSpecies,             // Indices of species
  inxNextSpecies,         // Indices of different species
  inxTests: integer;      // Indices of tests

begin
  with TestTableForSpeciesData do
  begin
    prgBar.Position := 0;
    prgBar.Max := (SpeciesCount - 1) * (SpeciesCount - 1) * (TestCount - 1) + 1;
    SetLength(MM, SpeciesCount - 1, SpeciesCount - 1, TestCount);
    for inxSpecies := 0 to SpeciesCount - 2 do
      for inxNextSpecies := inxSpecies + 1 to SpeciesCount - 1 do
        for inxTests := 0 to TestCount - 1 do
        begin
          if (BM[inxSpecies, inxTests] <> kMissingData) and (BM[inxNextSpecies, inxTests] <> kMissingData) then
            if BM[inxSpecies, inxTests] <> BM[inxNextSpecies, inxTests] then
              MM[inxSpecies, inxNextSpecies - 1, inxTests] := kUnmatching
            else
              MM[inxSpecies, inxNextSpecies - 1, inxTests] := kMatching
          else
            MM[inxSpecies, inxNextSpecies - 1, inxTests] := kMissingData;
          if boolShowBar then
            prgBar.Position := prgBar.Position + 1;
        end;
  end;
end;

{
Build distance matrix for species not matching remaining species in test column.
Matrix has the form top species in pages, remaining species in rows, and tests in columns.
The difference is first species with deifference against other remaining species.
Arrays: MM = Unmatching Matrix and DM = Distance Matrix

Parameters:
  prgBar -> Progress bar to show progress of procedure execution of building distance matrix
  mmoRslts -> Memo field to show result of left bottom distance matrix after calculation is completed.
  MM -> 3D array of double Unmatching Data values taxa by taxa by tests
  DM -> 2D array of double for Distance data values taxa row by taxa column
  boolShowBar -> boolean to tell if progress bar values should be updated
}
procedure AssignBinaryDistanceMatrix(var prgBar: TProgressBar; var MM: TMatchingMatrix; var DM: T2DDoubleArray; const boolShowBar: boolean);
var
  inxSpecies,             // Indices of species
  inxNextSpecies,         // Indices of remaining species
  inxTests: integer;      // Indices of tests
  dblCount,               // Test count double precision
  dblSum: double;         // Sum of matches in a double number
  inx,
  iny: integer;
begin
  with TestTableForSpeciesData do
  begin
//    dblCount := TestCount;
    for inxSpecies := 0 to SpeciesCount - 2 do
      for inxNextSpecies := inxSpecies + 1 to SpeciesCount - 1 do
      begin
        dblSum := dblZero;
        dblCount := dblZero;
        for inxTests := 0 to TestCount - 1 do
        begin
          if MM[inxSpecies, inxNextSpecies - 1, inxTests] <> kMissingData then
          begin
            if MM[inxSpecies, inxNextSpecies - 1, inxTests] = kUnmatching then
              dblSum :=  dblSum + 1.0;
            dblCount := dblCount + 1.0;
          end;
          if boolShowBar then
            prgBar.Position := prgBar.Position + 1;
        end;
        if dblCount > dblZero then
          DM[inxSpecies, inxNextSpecies] := dblSum / dblCount
        else
          DM[inxSpecies, inxNextSpecies] := dblZero;
      end;
    for inx := 0 to SpeciesCount - 1 do
      DM[inx, inx] := dblZero;
    for iny := 0 to SpeciesCount - 2 do
      for inx := iny + 1 to SpeciesCount - 1 do
        DM[inx, iny] := DM[iny, inx];
  end;
end;

{
  Build binary distant matrix from dividing unmatching matrix values by count of tests
  when comparing 2 different taxa.  Calls AssignBinaryDistanceMatrix to calculate matrix.

Parameters:
  prgBar -> Progress bar to show progress of procedure execution of building distance matrix
  mmoRslts -> Memo field to show result of left bottom distance matrix after calculation is completed.
}
procedure BuildBinaryDistanceMatrix(var prgBar: TProgressBar; var mmoRslts: TMemo);
var
  inxSpecies,              // Indices of species
  inxNextSpecies: integer; // Indices of remaining species
  strWork: string;         // Working string to build memo line
begin
  with TestTableForSpeciesData do
  begin
    SetLength(DistanceMatrix, SpeciesCount, SpeciesCount);
    AssignBinaryDistanceMatrix(prgBar, MatchingMatrix, DistanceMatrix, True);
    strlstDstncMtrx.Clear;
    strWork := Speciestitle + ',';
    for inxSpecies := 0 to SpeciesCount - 1 do
    begin
      Species[inxSpecies].VerticalMode := Leaf;
      Species[inxSpecies].VrtclPosition.intVrtclLngth := 0;
      Species[inxSpecies].VrtclPosition.intVrtclSpc := 0;
      Species[inxSpecies].NodeTier := -1;
      strWork := strWork + Species[inxSpecies].SpeciesName;
      if inxSpecies <= SpeciesCount - 2 then
        strWork := strWork + ',';
      prgBar.Position := prgBar.Position + 1;
    end;
    strlstDstncMtrx.Add(strWork);
    strWork := Species[0].SpeciesName;
    strlstDstncMtrx.Add(strWork);
    for inxSpecies := 1 to SpeciesCount - 1 do
    begin
      strWork := Species[inxSpecies].SpeciesName + ',';
      for inxNextSpecies := 0 to inxSpecies - 1 do
      begin
        strWork := strWork + FloatToStrF(DistanceMatrix[inxNextSpecies, inxSpecies], ffFixed, 12, 6);
        if inxNextSpecies < inxSpecies - 1 then
          strWork := strWork + ',';
         prgBar.Position := prgBar.Position + 1;
      end;
      strlstDstncMtrx.Add(strWork);
    end;
    mmoRslts.Lines.Assign(strlstDstncMtrx);
  end;
end;

{
Build difference matrix for species not matching remaining species in test column.
Matrix has the form top species in pages, remaining species in rows, and tests in columns.
The difference is first species with deifference against other remaining species.
Arrays: SM = Sequence Matrix and DM = Difference Matrix

Parameters:
  prgBar -> Progress bar to show progress of procedure execution of building distance matrix
  mmoRslts -> Memo field to show result of left bottom distance matrix after calculation is completed.
  SM -> 2D array of double Sequence Data values tests by taxa
  DM -> 3D array of double for Difference data values taxa row by taxa column by tests
  boolShowBar -> boolean to tell if progress bar values should be updated
}
procedure BuildDffrncSqncMatrix(var prgBar: TProgressBar; var SM: TSequenceMatix; var UM: TUnmatchingMatrix; const boolShowBar: boolean);
var
  inxSpecies,             // Indices of species
  inxNextSpecies,         // Indices of different species
  inxTests: integer;      // Indices of tests

begin
  with TestTableForSpeciesData do
  begin
    prgBar.Position := 0;
    prgBar.Max := (SpeciesCount - 1) * (SpeciesCount - 1) * (TestCount - 1) + 1;
    SetLength(UM, SpeciesCount - 1, SpeciesCount - 1, TestCount);
    for inxSpecies := 0 to SpeciesCount - 2 do
      for inxNextSpecies := inxSpecies + 1 to SpeciesCount - 1 do
        for inxTests := 0 to TestCount - 1 do
        begin
          if (SM[inxSpecies, inxTests] <> kMissingData) and (SM[inxNextSpecies, inxTests] <> kMissingData) then
            if SM[inxSpecies, inxTests] <> SM[inxNextSpecies, inxTests] then
              UM[inxSpecies, inxNextSpecies - 1, inxTests] := kUnmatching
            else
              UM[inxSpecies, inxNextSpecies - 1, inxTests] := kMatching
          else
            UM[inxSpecies, inxNextSpecies - 1, inxTests] := kMissingData;
          if boolShowBar then
            prgBar.Position := prgBar.Position + 1;
        end;
  end;
end;

{
Assign distance matrix for species not matching remaining species in test column.
Matrix has the form top species in pages, remaining species in rows, and tests in columns.
The difference is first species with difference against other remaining species.
Arrays: MM = Unmatching Matrix and DM = Distance Matrix

Parameters:
  prgBar -> Progress bar to show progress of procedure execution of building distance matrix
  mmoRslts -> Memo field to show result of left bottom distance matrix after calculation is completed.
  MM -> 3D array of double Unmatching Sequence Data values taxa by taxa by tests
  DM -> 2D array of double for Distance data values taxa row by taxa column
  boolShowBar -> boolean to tell if progress bar values should be updated
}
procedure AssignSequenceDistanceMatrix(var prgBar: TProgressBar; var UM: TUnmatchingMatrix; var DM: T2DDoubleArray; const boolShowBar: boolean);
var
  inxSpecies,             // Indices of species
  inxNextSpecies,         // Indices of remaining species
  inxTests: integer;      // Indices of tests
  dblCount,               // Test count double precision
  dblSum: double;         // Sum of matches in a double number
  boolIsJC: boolean;
  inx,
  iny: integer;
begin
  with TestTableForSpeciesData do
  begin
    boolIsJC := wrdDataType = wrdSequenceJC;
    for inxSpecies := 0 to SpeciesCount - 2 do
      for inxNextSpecies := inxSpecies + 1 to SpeciesCount - 1 do
      begin
        dblSum := dblZero;
        dblCount := dblZero;
        for inxTests := 0 to TestCount - 1 do
        begin
          if UM[inxSpecies, inxNextSpecies - 1, inxTests] <> kMissingData then
          begin
            if UM[inxSpecies, inxNextSpecies - 1, inxTests] = kUnmatching then
              dblSum :=  dblSum + 1.0;
            dblCount := dblCount + 1.0;
          end;
          if boolShowBar then
            prgBar.Position := prgBar.Position + 1;
        end;
        if dblCount > dblZero then
          DM[inxSpecies, inxNextSpecies] := dblSum / dblCount
        else
          DM[inxSpecies, inxNextSpecies] := dblZero;
        if boolIsJC then
          DM[inxSpecies, inxNextSpecies] := -0.75 * ln(1.0 - 4.0 / 3.0 * DM[inxSpecies, inxNextSpecies]);
      end;
    for inx := 0 to SpeciesCount - 1 do
      DM[inx, inx] := dblZero;
    for iny := 0 to SpeciesCount - 2 do
      for inx := iny + 1 to SpeciesCount - 1 do
        DM[inx, iny] := DM[iny, inx];
  end;
end;

{
  Build sequence distant matrix from dividing unmatching matrix values by count of tests
  when comparing 2 different taxa.  Calls AssignSequenceDistanceMatrix to calculate matrix.

Parameters:
  prgBar -> Progress bar to show progress of procedure execution of building distance matrix
  mmoRslts -> Memo field to show result of left bottom distance matrix after calculation is completed.
}
procedure  BuildSequenceDistanceMatrix(var prgBar: TProgressBar; var mmoRslts: TMemo);
var
  inxSpecies,              // Indices of species
  inxNextSpecies: integer; // Indices of remaining species
  strWork: string;         // Working string to build memo line
begin
  with TestTableForSpeciesData do
  begin
    SetLength(DistanceMatrix, SpeciesCount, SpeciesCount);
    AssignSequenceDistanceMatrix(prgBar, DffrncSqncMatrix, DistanceMatrix, True);
    strlstDstncMtrx.Clear;
    strWork := Speciestitle + ',';
    for inxSpecies := 0 to SpeciesCount - 1 do
    begin
      Species[inxSpecies].VerticalMode := Leaf;
      Species[inxSpecies].VrtclPosition.intVrtclLngth := 0;
      Species[inxSpecies].VrtclPosition.intVrtclSpc := 0;
      Species[inxSpecies].NodeTier := -1;
      strWork := strWork + Species[inxSpecies].SpeciesName;
      if inxSpecies <= SpeciesCount - 2 then
        strWork := strWork + ',';
      prgBar.Position := prgBar.Position + 1;
    end;
    strlstDstncMtrx.Add(strWork);
    strWork := Species[0].SpeciesName;
    strlstDstncMtrx.Add(strWork);
    for inxSpecies := 1 to SpeciesCount - 1 do
    begin
      strWork := Species[inxSpecies].SpeciesName + ',';
      for inxNextSpecies := 0 to inxSpecies - 1 do
      begin
        strWork := strWork + FloatToStrF(DistanceMatrix[inxNextSpecies, inxSpecies], ffFixed, 12, 6);
        if inxNextSpecies < inxSpecies - 1 then
          strWork := strWork + ',';
         prgBar.Position := prgBar.Position + 1;
      end;
      strlstDstncMtrx.Add(strWork);
    end;
    mmoRslts.Lines.Assign(strlstDstncMtrx);
  end;
end;

{
Builds a distance matrix for each matching replicate test data array where the replicate
number is the page and rows and columns for each distance matrix.  It first set the
lengths distance matrix page which starts out as the species count on both.  Next, it
calculates the differnce or unmatching matrix.  It calculates the corresponding distance
matrix.

Parameters:
  prgBar -> Progress bar to show progress of procedure execution of building bootstrap distance matrices
  intRplcts -> Number of replicates to process each distance matrix
}
procedure BuildBSDistanceMatrix(var prgBar: TProgressBar; const intRplcts: integer);
var
  inx: integer;
begin
  with TestTableForSpeciesData do
  begin
    SetLength(darBSDistanceMatrix, intRplcts);
    case wrdDataType of
      0..1: begin  // Binary +/- or 0/1
        for inx := 0 to intRplcts - 1 do
        begin
          SetLength(darBSDistanceMatrix[inx], SpeciesCount, SpeciesCount);
          BuildMatchingMatrix(prgBar, carBinary[inx], MatchingMatrix, False);
          AssignBinaryDistanceMatrix(prgBar, MatchingMatrix, darBSDistanceMatrix[inx], False);
        end;
        SetLength(MatchingMatrix, 0, 0, 0);
      end;
      2: begin  // Measuremwnt type
        for inx := 0 to intRplcts - 1 do
        begin
          SetLength(darBSDistanceMatrix[inx], SpeciesCount, SpeciesCount);
          BuildDifferenceMatrix(prgBar, darBtStrpRwDtMsrmnt[inx], DifferenceMatrix, False);
          AssignMeasurementDistanceMatrix(prgBar, DifferenceMatrix, darBSDistanceMatrix[inx], False);
        end;
        SetLength(DifferenceMatrix, 0, 0, 0);
      end;
      3..4: begin // Sequence Data p-Distance or JC-Distance
        for inx := 0 to intRplcts - 1 do
        begin
          SetLength(darBSDistanceMatrix[inx], SpeciesCount, SpeciesCount);
          BuildDffrncSqncMatrix(prgBar, carSequence[inx], DffrncSqncMatrix, False);
          AssignSequenceDistanceMatrix(prgBar, DffrncSqncMatrix, darBSDistanceMatrix[inx], False);
        end;
        SetLength(DifferenceMatrix, 0, 0, 0);
      end;
    end;
  end;
end;

{
Populates string grid for distance matrix for the original data both lower left and upper
right halves of distance matrix.  Added string grid for bottom-left triangle of the
distance matrix for user instead memo tab display.
}
procedure BuildFullDMatrix;
var
  inx,
  iny: integer;
begin
  try
    with TestTableForSpeciesData, frmPhyIoM do
    begin
      strgrdDstncMatrix.Clear;
      strgrdDstncMatrix.RowCount := NodeCount + 1;
      strgrdDstncMatrix.ColCount := NodeCount + 1;
      strgrdDstncMatrix.FixedRows := 1;
      strgrdDstncMatrix.FixedCols := 1;
      strgrdDstncMatrix.Cells[0, 0] := SpeciesTitle;
      for inx := 1 to NodeCount do
        strgrdDstncMatrix.Cells[inx, 0] := Species[inx - 1].SpeciesName;
      for iny := 1 to NodeCount do
        strgrdDstncMatrix.Cells[0, iny] := Species[iny - 1].SpeciesName;
      for iny := 1 to NodeCount do
        for inx := 1 to NodeCount do
          if (inx <= SpeciesCount) and (iny <= SpeciesCount) then
            strgrdDstncMatrix.Cells[inx, iny] := FloatToStrF(DistanceMatrix[inx - 1, iny - 1], ffFixed, 10, 4)
          else
            if DistanceMatrix[inx - 1, iny - 1] <> 0 then
              strgrdDstncMatrix.Cells[inx, iny] := FloatToStrF(DistanceMatrix[inx - 1, iny - 1], ffFixed, 10, 4)
            else
              strgrdDstncMatrix.Cells[inx, iny] := '';
      txtDataColumns.Text := IntToStr(strgrdDstncMatrix.ColCount - strgrdDstncMatrix.FixedCols);
      txtDataRows.Text := IntToStr(strgrdDstncMatrix.RowCount - strgrdDstncMatrix.FixedRows);
      strgrdResult.Clear;
      strgrdResult.RowCount := NodeCount + 1;
      strgrdResult.ColCount := NodeCount + 1;
      strgrdResult.FixedRows := 1;
      strgrdResult.FixedCols := 1;
      strgrdResult.Cells[0, 0] := SpeciesTitle;
      for inx := 1 to SpeciesCount do
        strgrdResult.Cells[inx, 0] := Species[inx - 1].SpeciesName;
      for iny := 1 to SpeciesCount do
        strgrdResult.Cells[0, iny] := Species[iny - 1].SpeciesName;
      for iny := 1 to SpeciesCount do
        for inx := 1 to iny - 1 do
          strgrdResult.Cells[inx, iny] := FloatToStrF(DistanceMatrix[inx - 1, iny - 1], ffFixed, 10, 4)
    end;
  except
    on E:exception do
    begin
      with frmPhyIoM.strgrdDstncMatrix do
      begin
        Cells[0, 1] := IntToStr(E.HelpContext);
        Cells[1, 0] := 'Error';
        Cells[1, 1] := E.Message;
        RowCount := 2;
        ColCount := 2;
      end;
      with frmPhyIoM.strgrdResult do
      begin
        Cells[0, 1] := IntToStr(E.HelpContext);
        Cells[1, 0] := 'Error';
        Cells[1, 1] := E.Message;
        RowCount := 2;
        ColCount := 2;
      end;
    end;
  end;
  frmPhyIoM.strgrdDstncMatrix.AutoSizeColumns;
  frmPhyIoM.strgrdResult.AutoSizeColumns;
  with TestTableForSpeciesData, frmPhyIoM.strgrdResult do
    if ColCount = SpeciesCount + 1 then
      if ColWidths[SpeciesCount] < 64 then
        ColWidths[SpeciesCount] := 64;
end;
{
Populates string grid for distance matrix for each replicate both lower left and upper
right halves of distance matrix.

Parameters:
  intBootStrapPage -> number of current replicates to be displayed
}
procedure BuildBSFullDMatrix(const intBootStrapPage: integer);
var
  intNC,
  inx,
  iny: integer;
begin
  try
    if intBootStrapPage < 0 then
      Exit;
    with TestTableForSpeciesData, frmPhyIoM do
    begin
      intNC := High(darBSDistanceMatrix[intBootStrapPage]);
      strgrdDstncMatrix.Clear;
      strgrdDstncMatrix.RowCount := intNC + 2;
      strgrdDstncMatrix.ColCount := intNC + 2;
      strgrdDstncMatrix.FixedRows := 1;
      strgrdDstncMatrix.FixedCols := 1;
      strgrdDstncMatrix.Cells[0, 0] := 'Replicate:' + IntToStr(intBootStrapPage);
      for inx := 1 to NodeCount do
        strgrdDstncMatrix.Cells[inx, 0] := arstrNodeNames[intBootStrapPage][inx - 1];
      for iny := 1 to NodeCount do
        strgrdDstncMatrix.Cells[0, iny] := arstrNodeNames[intBootStrapPage][iny - 1];
      for iny := 1 to intNC + 1 do
        for inx := 1 to intNC + 1 do
          if (inx <= SpeciesCount) and (iny <= SpeciesCount) then
            strgrdDstncMatrix.Cells[inx, iny] := FloatToStrF(darBSDistanceMatrix[intBootStrapPage][inx - 1, iny - 1], ffFixed, 10, 4)
          else
            if darBSDistanceMatrix[intBootStrapPage][inx - 1, iny - 1] <> 0 then
              strgrdDstncMatrix.Cells[inx, iny] := FloatToStrF(darBSDistanceMatrix[intBootStrapPage][inx - 1, iny - 1], ffFixed, 10, 4)
            else
              strgrdDstncMatrix.Cells[inx, iny] := '';
      txtDataColumns.Text := IntToStr(strgrdDstncMatrix.ColCount - strgrdDstncMatrix.FixedCols);
      txtDataRows.Text := IntToStr(strgrdDstncMatrix.RowCount - strgrdDstncMatrix.FixedRows);
    end;
  except
    on E:exception do
      with frmPhyIoM.strgrdDstncMatrix do
      begin
        Cells[0, 1] := IntToStr(E.HelpContext);
        Cells[1, 0] := 'Error';
        Cells[1, 1] := E.Message;
        RowCount := 2;
        ColCount := 2;
      end;
  end;
  frmPhyIoM.strgrdDstncMatrix.AutoSizeColumns;
end;

{
The procedure calculates the distance between two points from a distance matrix to build the Q Matrix.
These points must be distinct not same point on the different axises.  The same points on the other axis
will have s distance of zero.  Calculate the Q sum by summing the negative for the active distnce
points and not the selected point for column and row of the selected point.  The Q Value is
calculated by multiplying (the selected point by number of species minus 2 minus index of the
Q matrix).  The Q Value is placed in the darQM 3D array inxQM by ColX by RowY.  This procedure
will be used by MakeABranch, FindBranches, and BuildQMatrix.

Parameters:
darQM - is a three dimensional data matrix of double.  The array was 3D to accomodate replicate Q Matrices.
boolarIsActive - is a one dimesional array of boolean.  Which taxa or node is active in the calculation.
iarQI - Array of QIdexes for species.  Holds ColX or RowY in the Q Matrix corresponding to Species array index.
darDM - is a two dimensional array of the distance matrix.  For primary data or replicates.
intIDM - High value index of 1st and 2nd dimension of distance matrix.
intIQL - Length of 2nd and 3rd dimension of Q matrix.
inxQM - Index of Q Matrix replicate or zero for original data.
intQM - Replicate number that is the original data.
}
procedure AssignQMatrix(var darQM: TFullQMatrix; const boolarIsActive: TIsActive; const iarQI: TBSQIndex; const darDM: T2DDoubleArray; const intIDM, intIQL, inxQM, intQM: integer);
var
  ColX,              // Column of lowest Q Matrix value
  RowY,              // Row of lowest Q Matrix value
  inx,
  iny,
  ink: integer;
  dblQV,             // Single Q Matrix values
  dblDstcn,
  dblSum: double;
begin
  try
    with TestTableForSpeciesData do
    begin
      SetLength(darQM[inxQM], intIQL, intIQL);
      for inx := 0 to intIDM do
        if boolarIsActive[inx] then
          for iny := 0 to intIDM do
            if inx = iny then
              continue
            else
              if boolarIsActive[iny] then
              begin
                dblSum := dblZero;
                for ink := 0 to intIDM do
                  if boolarIsActive[ink] then
                    dblSum := dblSum - darDM[inx, ink];
                for ink := 0 to intIDM do
                  if boolarIsActive[ink] then
                    dblSum := dblSum - darDM[iny, ink];
                dblDstcn := darDM[inx, iny];
                ColX := iarQI[inx];
                RowY := iarQI[iny];
                if (ColX >= intIQL) or (RowY >= intIQL) then
                  strlstCalcErrs.Add('High intAQMCC = ' + IntToStr(intIQL) + ', Column X = ' + IntToStr(ColX) + ', Row Y = ' + IntToStr(RowY) + ', index Q = ' + IntToStr(inxQM) + ', inx = ' + IntToStr(inx) + ', iny = ' + IntToStr(iny) + ', and inr = ' + IntToStr(intQM))
                else
                begin
                  dblQV := (SpeciesCount - 2 - inxQM) * dblDstcn + dblSum;
                  darQM[inxQM][ColX, RowY] := dblQV;
                end;
              end;
    end;
  except
    on E:exception do
    begin
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

{
Procedure popluates string grid for displaying Q Matrix after calling AssignQMatrix.
This can be for any data type since it is after the distance matrix is created.
}
procedure BuildQMatrix;
var
  intADMCC,     // Current Distance Matrix size
  intADMGridC,  // Current Distance Matrix grid size
  intAQMCC,     // Current Q Matrix size
  intAQMGridC,  // Current Q Matrix grid size
  inx,
  iny,
  ink: integer;
begin
  try
    with TestTableForSpeciesData, frmPhyIoM do
    begin
      intADMGridC := SpeciesCount + inxQ;
      intADMCC := intADMGridC - 1;
      intAQMCC := SpeciesCount - inxQ;
      intAQMGridC := intAQMCC + 1;
      SetLength(inxSpcsQM, intAQMCC);
      if inxQ = 0 then
      begin
        SetLength(QMatrix, SpeciesCount - 3);
        for inx := SpeciesCount to 2 * SpeciesCount - 4 do
          IsActive[inx] := False;
      end
      else
        strgrdQMarix.Clear;
      with strgrdQMarix do
      begin
        RowCount := intAQMGridC;
        ColCount := intAQMGridC;
        FixedRows := 1;
        FixedCols := 1;
        Cells[0, 0] := SpeciesTitle;
      end;
      ink := 0;
      for inx := 1 to intADMGridC do
        if IsActive[inx - 1] then
        begin
          QIndex[inx - 1] := ink;
          inxSpcsQM[ink] := inx - 1;
          if ink > intAQMCC - 1 then
          begin
            strgrdQMarix.RowCount := ink + 2;
            strgrdQMarix.ColCount := ink + 2;
          end;
          strgrdQMarix.Cells[ink + 1, 0] := Species[inx - 1].SpeciesName;
          strgrdQMarix.Cells[0, ink + 1] := Species[inx - 1].SpeciesName;
          inc(ink);
        end
        else
          QIndex[inx - 1] := kintDisabledSpcs;
      AssignQMatrix(QMatrix, IsActive, QIndex, DistanceMatrix, intADMCC, intAQMCC, inxQ, 0);
      for iny := 1 to intAQMCC do
        for inx := 1 to intAQMCC do
        begin
          if inx <> iny then
            strgrdQMarix.Cells[inx, iny] := FloatToStrF(QMatrix[inxQ][inx - 1, iny - 1], ffFixed, 12, 6);
        end;
      ink := 0;
    end;
  except
    on E:exception do
      with frmPhyIoM.strgrdQMarix do
      begin
        Clear;
        RowCount := 2;
        ColCount := 2;
        FixedRows := 1;
        FixedCols := 1;
        Cells[0, 0] := TestTableForSpeciesData.SpeciesTitle;
        Cells[0, 1] := IntToStr(E.HelpContext);
        Cells[1, 0] := 'Error';
        Cells[1, 1] := E.Message;
      end;
  end;
  frmPhyIoM.strgrdQMarix.AutoSizeColumns;
end;

{
Function returns boolean of true or false if there are any unresolved nodes left.
An unresolved node means it has not been assigned a node level.  Which it position
on the tree has not been determined.

Parameters:
  Result -> boolean
}
function boolUnresolvedNodes: boolean;
var
  inx: integer;
begin
  Result := False;
  for inx := 0 to TestTableForSpeciesData.NodeCount - 2 do
    if TestTableForSpeciesData.Species[inx].NodeLevel = -1 then
    begin
      Result := True;
      break;
    end;
end;

{
Function returns booelean value of true if Col is lower than Row other returns
false if Row greater than or equal to Col.

Parameters:
  strColX -> String of ColX of the angle bracket form which is a psuedo form of Newick.
  strRowY -> String of RowY of the angle bracket form which is a psuedo form of Newick.
  Result -> boolean
}
function boolFindLeadLowElement(const strColX, strRowY: string): boolean;
var
  strCX,
  strRY: string;
  intPos,
  intCX,
  intRY,
  inx,
  iny: integer;
begin
  inx := 1;
  while strColX[inx] = '<' do
    inc(inx);
  iny := 1;
  while strRowY[iny] = '<' do
    inc(iny);
  if inx < iny then
    Result := True
  else
    if inx > iny then
      Result := False
    else
    begin
      strCX := Copy(strColX, inx, Length(strColX) - inx + 1);
      intPos := Pos(',', strCX);
      intCX := StrToInt(Copy(strCX, 1, intPos - 1));
      strRY := Copy(strRowY, iny, Length(strRowY) - iny + 1);
      intPos := Pos(',', strRY);
      intRY := StrToInt(Copy(strRY, 1, intPos - 1));
      if intCX < intRY then
        Result := True
      else
        Result := False;
    end;
end;

{
This procedure adds branches to the leaves by finding pair with the lowest distance.
Performing one iteration on the distance matrix and Q matrix on each cycle.  The
lowest pair is eliminated in the Q matrix and made inactive in the distance matrix
and it adds a branch.  As pairs are eliminated and a branch added the Q Matrix must
be recalculated.  This will be repeated until there are only three elements active.
At this point the last 3 distance must be calculated by three line elimination method.
This will create all distance for the tree to be displayed.  The procedure will calculate
Newick tokens for each leaf and node as it iterates the distance matrix.  Populates
the string grid for Make Tree tab.

Parameters:
  dblLowestValue -> Last lowest value calculated in the Q Matrix
  dblSum -> Sum of the branc lengths in double precision
  inxarDstcn -> array of integer Indices of final 3 distances in distance matrix
  ColX -> Column selected with lowest pair value
  RowY -> Row selected with lowest pair value
  strError -> string for errors in tree calculation such as negative lengths
}
procedure MakeABranch(var dblLowestValue, dblSum: double; var inxarDstcn: TDistanceIndex; var ColX, RowY: integer; var strError: string; var prgBar: TProgressBar; var txtIN: TEdit);
const
  kFirstCol = 1;
  kFirstRow = 0;
  kLetters = 26;
  k2Letters = kLetters * kLetters;
  k3Letters = k2Letters * kLetters;
var
  bytC,
  bytC2,
  bytC3,
  bytC4,
  bytA: byte;
  csrSave: TCursor;
  intErrs,
  int1stNdix,
  int2ndNdix,
  intFrstNd,
  intSpcs,
  intC,
  intC2,
  intNC,
  ink,
  inx,
  iny: integer;
  dblX,
  dblY,
  dblZ,
  dblSpltLngth,
  dblFirst: double;
  boolFirstNode,
  boolSplitter,
  boolEndRun: boolean;
  strItem,
  strDigits,
  strWork: string;
begin
 csrSave := Screen.Cursor;
 Screen.Cursor := crHourglass;
 try
  try
    prgBar.Min := 0;
    prgBar.Max := TestTableForSpeciesData.SpeciesCount - 2;
    intNC := 0;
    prgBar.Position := intNC;
    txtIN.Caption := '0';
    txtIN.Refresh;
    intErrs := strlstCalcErrs.Count;
    boolEndRun := False;
    repeat
      with TestTableForSpeciesData, frmPhyIoM do
      begin;
        ColX := inxSpcsQM[kFirstCol];
        RowY := inxSpcsQM[kFirstRow];
        dblLowestValue := QMatrix[inxQ][kFirstCol, kFirstRow];
        for iny := 0 to SpeciesCount - inxQ - 1 do
          for inx := SpeciesCount - inxQ - 1 downto iny + 1 do
            if QMatrix[inxQ][inx, iny] < dblLowestValue then
            begin
              dblLowestValue := QMatrix[inxQ][inx, iny];
              ColX := inxSpcsQM[inx];
              RowY := inxSpcsQM[iny];
            end;
        dblFirst := dblZero;
        for iny := 0 to NodeCount - 1 do
          if IsActive[iny] then
            dblFirst := dblFirst + DistanceMatrix[ColX, iny];
        for iny := 0 to NodeCount - 1 do
          if IsActive[iny] then
            dblFirst := dblFirst - DistanceMatrix[RowY, iny];
        Species[ColX].BranchLength := 0.5 * DistanceMatrix[ColX, RowY] + 0.5 / (SpeciesCount - 2 - inxQ) * dblFirst;
        Species[RowY].BranchLength := DistanceMatrix[ColX, RowY] - Species[ColX].BranchLength;
        if Species[RowY].BranchLength <= dblZero then
        begin
          if strlstCalcErrs.Count = intErrs then
            strlstCalcErrs.Add(strErrorHdrFtr('', 'Calculate Make a Branch.  Negative branch length set to zero.', True));
          strlstCalcErrs.Add('Warning Zero/Negative Branch length = ' + FloatToStrF(Species[RowY].BranchLength, ffFixed, 12, 8) + ' @ Species Name = ' + Species[RowY].SpeciesName);
          if Species[RowY].BranchLength < dblZero then
          begin
            Species[ColX].BranchLength := Species[ColX].BranchLength - Species[RowY].BranchLength;
            Species[RowY].BranchLength := dblZero;
          end;
        end;
        if Species[ColX].BranchLength <= dblZero then
        begin
          if strlstCalcErrs.Count = intErrs then
            strlstCalcErrs.Add(strErrorHdrFtr('', 'Calculate Make a Branch.  Negative branch length set to zero.', True));
          strlstCalcErrs.Add('Warning Zero/Negative Branch length = ' + FloatToStrF(Species[ColX].BranchLength, ffFixed, 12, 8) + ' @ Species Name = ' + Species[ColX].SpeciesName);
          if Species[ColX].BranchLength < dblZero then
          begin
            Species[RowY].BranchLength := Species[RowY].BranchLength - Species[ColX].BranchLength;
            Species[ColX].BranchLength := dblZero;
          end;
        end;
        IsActive[ColX] := False;
        IsActive[RowY] := False;
        with Species[ColX] do
        begin
          NodeParent := NodeCount;
          BeginSegment.int1stXNd := NodeCount;
          BeginSegment.int2ndXNd := RowY;
          Species[NodeCount].Leaves := Leaves;
        end;
        with Species[RowY] do
        begin
          NodeParent := NodeCount;
          BeginSegment.int1stXNd := NodeCount;
          BeginSegment.int2ndXNd := ColX;
          Species[NodeCount].Leaves := Species[NodeCount].Leaves + Leaves;
        end;
        Inc(intNC);
        prgBar.Position := intNC;
        txtIN.Caption := IntToStr(intNC - 1);
        txtIN.Refresh;
        Inc(LastNode);
        strWork := '<';
        if (ColX < SpeciesCount) and (RowY < SpeciesCount) then
          if RowY > ColX then  // original
            strWork := strWork + IntToStr(ColX) + ',' + IntToStr(RowY)
          else
            strWork := strWork + IntToStr(RowY) + ',' + IntToStr(ColX)
        else
          if ColX < SpeciesCount then
             strWork := strWork + IntToStr(ColX) + ',' + Species[RowY].SpeciesName
          else
            if RowY < SpeciesCount then
              strWork := strWork + IntToStr(RowY) + ',' + Species[ColX].SpeciesName
            else
              if Length(Species[ColX].SpeciesName) > Length(Species[RowY].SpeciesName) then
                strWork := strWork + Species[RowY].SpeciesName + ',' + Species[ColX].SpeciesName
              else
                if Length(Species[ColX].SpeciesName) < Length(Species[RowY].SpeciesName) then
                  strWork := strWork + Species[ColX].SpeciesName + ',' + Species[RowY].SpeciesName
                else
                  if boolFindLeadLowElement(Species[ColX].SpeciesName, Species[RowY].SpeciesName) then
                    strWork := strWork + Species[ColX].SpeciesName + ',' + Species[RowY].SpeciesName
                  else
                    strWork := strWork + Species[RowY].SpeciesName + ',' + Species[ColX].SpeciesName;
        Species[NodeCount].SpeciesName := strWork + '>';
        with Species[NodeCount] do
        begin
          EndSegment.int1stXNd := ColX;
          EndSegment.int2ndXNd := RowY;
        end;
        IsActive[NodeCount] := True;
        Inc(NodeCount);
        SetLength(DistanceMatrix, NodeCount, NodeCount);
        strgrdDstncMatrix.ColCount := NodeCount + 1;
        strgrdDstncMatrix.RowCount := NodeCount + 1;
        strgrdDstncMatrix.Cells[NodeCount, 0] := Species[NodeCount - 1].SpeciesName;
        strgrdDstncMatrix.Cells[0, NodeCount] := Species[NodeCount - 1].SpeciesName;
        for iny := 0 to NodeCount - 2 do
          if ColX <> iny then
            if iny <> RowY then
              if IsActive[iny] then
              begin
                dblFirst := 0.5 * (DistanceMatrix[iny, RowY] + DistanceMatrix[iny, ColX] - DistanceMatrix[ColX, RowY]);
                DistanceMatrix[NodeCount - 1, iny] := dblFirst;
                strgrdDstncMatrix.Cells[NodeCount, iny + 1] := FloatToStrF(dblFirst, ffFixed, 10, 4);
                DistanceMatrix[iny, NodeCount - 1] := dblFirst;
                strgrdDstncMatrix.Cells[iny + 1, NodeCount] := FloatToStrF(dblFirst, ffFixed, 10, 4);
              end;
        txtDataColumns.Text := IntToStr(strgrdDstncMatrix.ColCount - strgrdDstncMatrix.FixedCols);
        txtDataRows.Text := IntToStr(strgrdDstncMatrix.RowCount - strgrdDstncMatrix.FixedRows);
        with strgrdDataTableProp do
          for inx := 1 to 2 * SpeciesCount - 3 do
            with Species[inx - 1] do
            begin
              if SpeciesName <> '' then
                Cells[inx, 0] := SpeciesName
              else
                Cells[inx, 0] := IntToStr(inx - 1);
              Cells[inx, 1] := IntToStr(inx - 1);
              Cells[inx, 2] := IntToStr(QIndex[inx - 1]);
              Cells[inx, 3] := strToF(IsActive[inx - 1], 'Active', 'Not Active');
              Cells[inx, 4] := FloatToStrF(BranchLength, ffFixed, 12, 8);
              Cells[inx, 5] := IntToStr(NodeLevel);
              Cells[inx, 6] := IntToStr(NodeParent);
            end;
        inc(inxQ);
        strgrdDstncMatrix.AutoSizeColumns;
      end;
      if TestTableForSpeciesData.SpeciesCount - TestTableForSpeciesData.inxQ > 3 then
        BuildQMatrix
      else
        boolEndRun := True;
      strError := '';
    until boolEndRun;
    ink := 0;
    for iny := 0 to TestTableForSpeciesData.NodeCount - 1 do
      if TestTableForSpeciesData.IsActive[iny] then  // Find remaining active points
      begin
        inxarDstcn[ink] := iny;    // Active point found
        inc(ink);
      end;
    dblarDstcn[0] := DistanceMatrix[inxarDstcn[0], inxarDstcn[2]]; // X + Y
    dblarDstcn[1] := DistanceMatrix[inxarDstcn[1], inxarDstcn[2]]; // Y + Z
    dblarDstcn[2] := DistanceMatrix[inxarDstcn[0], inxarDstcn[1]]; // X + Z
    dblX := (dblarDstcn[0] + dblarDstcn[1] - dblarDstcn[2]) / 2.0;   // Calculate X
    dblY := dblarDstcn[1] - dblX;                                  // Calculate Y
    dblZ := dblarDstcn[0] - dblX;                                  // Calculate Z
    with TestTableForSpeciesData do
    begin
      Species[inxarDstcn[0]].BranchLength := dblZ;              // Place Z in first active node
      Species[inxarDstcn[1]].BranchLength := dblY;              // Place Y in second active node
      Species[inxarDstcn[2]].BranchLength := dblX;              // Place Z in third active node
      Species[NodeCount - 1].NodeLevel := 0;
      Species[NodeCount].NodeLevel := 0;
      Species[NodeCount + 1].NodeLevel := -1;
      Species[NodeCount - 1].VerticalMode := AParent;
      Species[NodeCount].VerticalMode := AParent;
      Species[NodeCount + 1].VerticalMode := AParent;
      boolFirstNode := True;
      for inx := 0 to 2 do
        if inxarDstcn[inx] = NodeCount - 1 then
        begin
          Species[inxarDstcn[inx]].BeginSegment.int1stXNd := NodeCount + 1;
          Species[inxarDstcn[inx]].BeginSegment.int2ndXNd := NodeCount;
          Species[inxarDstcn[inx]].NodeParent := NodeCount + 1;
          Species[NodeCount].BeginSegment.int1stXNd := NodeCount + 1;
          Species[NodeCount].BeginSegment.int2ndXNd := NodeCount - 1;
        end
        else
        begin
          Species[inxarDstcn[inx]].BeginSegment.int1stXNd := NodeCount;
          if boolFirstNode then
          begin
            Species[NodeCount].EndSegment.int1stXNd := inxarDstcn[inx];
            Species[inxarDstcn[inx]].NodeParent := NodeCount;
            intFrstNd := inxarDstcn[inx];
            boolFirstNode := False;
          end
          else
          begin
            Species[NodeCount].EndSegment.int2ndXNd := inxarDstcn[inx];
            Species[inxarDstcn[inx]].NodeParent := NodeCount;
            Species[inxarDstcn[inx]].BeginSegment.int2ndXNd := intFrstNd;
            Species[intFrstNd].BeginSegment.int2ndXNd := inxarDstcn[inx];
          end;
        end;
      while boolUnresolvedNodes do
      begin
        for inx := 0 to NodeCount - 2 do
          if Species[Species[inx].NodeParent].NodeLevel <> kUnknownNode then
            if Species[inx].NodeLevel = kUnknownNode then
              Species[inx].NodeLevel := Species[Species[inx].NodeParent].NodeLevel + 1;
      end;
      if boolUnresolvedNodes then
      begin
        strWork := '';
        for inx := 0 to NodeCount - 2 do
          if Species[inx].NodeLevel = kUnknownNode then
          begin
            if Length(strWork) > 0 then
              strWork := strWork + ', ';
            strWork := strWork + IntToStr(inx);
          end;
        MessageDlg('Unresolved Nodes Encountered - ' + strWork, mtWarning, [mbOK], 0);
      end;
    end;
    dblSum := dblZero;
    with TestTableForSpeciesData, frmPhyIoM.strgrdDataTableProp do
    begin
      for inx := 0 to 2 do         // Place X, Y, & Z in Species Properties table grid
        Cells[inxarDstcn[inx] + 1, 4] := FloatToStrF(Species[inxarDstcn[inx]].BranchLength, ffFixed, 12, 8);
      Species[NodeCount].SpeciesName := 'Node';
      boolSplitter := True;
      strWork := '<';
      for inx := 0 to NodeCount - 2 do
        if IsActive[inx] then
          if boolSplitter then
          begin
            int1stNdix := inx;
            boolSplitter := False;
          end
          else
          begin
            int2ndNdix := inx;
            break;
          end;
//          Species[NodeCount].SpeciesName := Species[NodeCount].SpeciesName + IntToStr(inx);
//          begin
//            Species[NodeCount].SpeciesName := Species[NodeCount].SpeciesName + '<|>';
//            boolSplitter := False;
//          end
//          else
//            break;
//        end;
      if (int1stNdix < SpeciesCount) and (int2ndNdix < SpeciesCount) then
        if int2ndNdix > int1stNdix then  // original
          strWork := strWork + IntToStr(int1stNdix) + ',' + IntToStr(int2ndNdix)
        else
          strWork := strWork + IntToStr(int2ndNdix) + ',' + IntToStr(int1stNdix)
      else
        if int1stNdix < SpeciesCount then
           strWork := strWork + IntToStr(int1stNdix) + ',' + Species[int2ndNdix].SpeciesName
        else
          if int2ndNdix < SpeciesCount then
            strWork := strWork + IntToStr(int2ndNdix) + ',' + Species[int1stNdix].SpeciesName
          else
            if Length(Species[int1stNdix].SpeciesName) > Length(Species[int2ndNdix].SpeciesName) then
              strWork := strWork + Species[int2ndNdix].SpeciesName + ',' + Species[int1stNdix].SpeciesName
            else
              if Length(Species[int1stNdix].SpeciesName) < Length(Species[int2ndNdix].SpeciesName) then
                strWork := strWork + Species[int1stNdix].SpeciesName + ',' + Species[int2ndNdix].SpeciesName
              else
                if boolFindLeadLowElement(Species[int1stNdix].SpeciesName, Species[RowY].SpeciesName) then
                  strWork := strWork + Species[int1stNdix].SpeciesName + ',' + Species[int2ndNdix].SpeciesName
                else
                  strWork := strWork + Species[int2ndNdix].SpeciesName + ',' + Species[int1stNdix].SpeciesName;
      Species[NodeCount].SpeciesName := strWork + '>';
      Species[NodeCount + 1].SpeciesName := '*Root*';
      for inx := 0 to NodeCount - 1 do
      begin
        if inx < SpeciesCount then
          with Species[inx] do
          begin
            EndSegment.int1stXNd := inx;
            EndSegment.int2ndXNd := inx;
            Cells[inx + 1, 6] := IntToStr(NodeParent);
            Cells[inx + 1, 8] := IntToStr(BeginSegment.int1stXNd);
            Cells[inx + 1, 9] := IntToStr(BeginSegment.int2ndXNd);
            Cells[inx + 1, 10] := IntToStr(EndSegment.int1stXNd);
            Cells[inx + 1, 11] := IntToStr(EndSegment.int2ndXNd);
            Cells[inx + 1, 16] := Cells[inx + 1, 0] + ':' + Cells[inx + 1, 4];
            Cells[inx + 1, 33] := IntToStr(Leaves);
          end
        else
          if inx < NodeCount - 1 then
          begin
            Cells[inx + 1, 6] := IntToStr(Species[inx].NodeParent);
            Cells[inx + 1, 8] := IntToStr(Species[inx].BeginSegment.int1stXNd);
            Cells[inx + 1, 9] := IntToStr(Species[inx].BeginSegment.int2ndXNd);
            Cells[inx + 1, 10] := IntToStr(Species[inx].EndSegment.int1stXNd);
            Cells[inx + 1, 11] := IntToStr(Species[inx].EndSegment.int2ndXNd);
            Cells[inx + 1, 16] := '()' + ':' + Cells[inx + 1, 4];
            Cells[inx + 1, 33] := IntToStr(Species[inx].Leaves);
          end
          else
          begin
            Species[inx + 2].BranchLength := Species[inx].BranchLength;
            dblSpltLngth := dblSplitLength(Species[inx + 2].BranchLength);
            Species[inx + 1].BranchLength := Species[inx].BranchLength - dblSpltLngth;
            Species[inx].BranchLength := dblSpltLngth;
            Species[inx].Leaves := Species[Species[inx].EndSegment.int1stXNd].Leaves + Species[Species[inx].EndSegment.int2ndXNd].Leaves;
            Species[inx + 1].Leaves := Species[Species[inx + 1].EndSegment.int1stXNd].Leaves + Species[Species[inx + 1].EndSegment.int2ndXNd].Leaves;
            Species[inx + 2].Leaves := Species[inx + 1].Leaves + Species[inx].Leaves;
            Species[inx + 2].NodeParent := NodeCount + 1;
            Species[inx + 2].BeginSegment.int1stXNd := NodeCount + 1;
            Species[inx + 2].BeginSegment.int2ndXNd := NodeCount + 1;
            Species[inx + 2].EndSegment.int1stXNd := NodeCount - 1;
            Species[inx + 2].EndSegment.int2ndXNd := NodeCount;
            Species[inx].NodeParent := NodeCount + 1;
            Species[inx + 1].NodeParent := NodeCount + 1;
            Cells[inx + 2, 0] := Species[NodeCount].SpeciesName;
            Cells[inx + 3, 0] := Species[NodeCount + 1].SpeciesName;
            Cells[inx + 3, 1] := IntToStr(inx + 2);
            Cells[inx + 2, 1] := IntToStr(NodeCount);
            Cells[inx + 2, 2] := IntToStr(QIndex[inx + 1]);
            Cells[inx + 2, 3] := strToF(IsActive[inx + 1], 'Active', 'Not Active');
            Cells[inx + 1, 4] := FloatToStrF(dblSpltLngth, ffFixed, 12, 8);
            Cells[inx + 2, 4] := FloatToStrF(Species[inx + 1].BranchLength, ffFixed, 12, 8);
            Cells[inx + 3, 4] := FloatToStrF(Species[inx + 2].BranchLength, ffFixed, 12, 8);
            Cells[inx + 1, 6] := IntToStr(Species[inx].NodeParent);
            Cells[inx + 2, 6] := IntToStr(Species[inx + 1].NodeParent);
            Cells[inx + 3, 6] := IntToStr(Species[inx + 2].NodeParent);
            Cells[inx + 1, 16] := ')' + ':' + FloatToStrF(dblSpltLngth, ffFixed, 12, 8); // + ',' +
            Cells[inx + 2, 16] := ')' + ':' + FloatToStrF(Species[inx + 1].BranchLength, ffFixed, 12, 8);
            Cells[inx + 1, 8] := IntToStr(Species[inx].BeginSegment.int1stXNd);
            Cells[inx + 1, 9] := IntToStr(Species[inx].BeginSegment.int2ndXNd);
            Cells[inx + 1, 10] := IntToStr(Species[inx].EndSegment.int1stXNd);
            Cells[inx + 1, 11] := IntToStr(Species[inx].EndSegment.int2ndXNd);
            Cells[inx + 2, 8] := IntToStr(Species[inx + 1].BeginSegment.int1stXNd);
            Cells[inx + 2, 9] := IntToStr(Species[inx + 1].BeginSegment.int2ndXNd);
            Cells[inx + 3, 8] := IntToStr(Species[inx + 2].BeginSegment.int1stXNd);
            Cells[inx + 3, 9] := IntToStr(Species[inx + 2].BeginSegment.int2ndXNd);
            Cells[inx + 2, 10] := IntToStr(Species[inx + 1].EndSegment.int1stXNd);
            Cells[inx + 2, 11] := IntToStr(Species[inx + 1].EndSegment.int2ndXNd);
            Cells[inx + 3, 10] := IntToStr(Species[inx + 2].EndSegment.int1stXNd);
            Cells[inx + 3, 11] := IntToStr(Species[inx + 2].EndSegment.int2ndXNd);
            Cells[inx + 1, 33] := IntToStr(Species[inx].Leaves);
            Cells[inx + 2, 33] := IntToStr(Species[inx + 1].Leaves);
            Cells[inx + 3, 33] := IntToStr(Species[inx + 2].Leaves);
            Species[inx + 1].NewickToken := Cells[inx + 2, 16];
          end;
        Species[inx].NewickToken := Cells[inx + 1, 16];
      end;
      for inx := SpeciesCount to NodeCount - 2 do
      begin
        strWork := '';
        for iny := inx - 1 downto 0 do
          if Species[iny].NodeParent = inx then
            strWork := strWork + Species[iny].NewickToken + ',';
        if Length(strWork) > 0 then
          Delete(strWork, Length(strWork), 1);
        Species[inx].NewickToken := Species[inx].NewickToken[1] + strWork + Copy(Species[inx].NewickToken, 2, Length(Species[inx].NewickToken) - 1);
        Cells[inx + 1, 16] := Species[inx].NewickToken;
        Species[inx].VerticalMode := Child;
      end;
      bytA := ord('A');
      for inx := 1 to NodeCount + 2 do
      begin
        Cells[inx, 5] := IntToStr(Species[inx - 1].NodeLevel);
        if inx < NodeCount + 2 then
          dblSum := dblSum + Species[inx - 1].BranchLength;
        bytC := (inx - 1) div k3Letters;
        if bytC > 0 then
        begin
          strItem := Chr(bytA + bytC - 1);
          intC := (inx - 1) mod k3Letters;
        end
        else
        begin
          strItem := '';
          intC := inx;
        end;
        bytC2 := intC div k2Letters;
        if bytC2 > 0 then
        begin
          strItem := strItem + Chr(bytA + bytC2 - 1);
          intC2 := (intC - 1) mod k2Letters;
        end
        else
          intC2 := intC;
        bytC3 := (intC2 - 1) div kLetters;
        if bytC3 > 0 then
          strItem := strItem + Chr(bytA + bytC3 - 1);
//        else
//          strItem := '';
        bytC4 := (inx - 1) mod kLetters;
        Species[inx - 1].ShortLabel := strItem + Chr(bytA + bytC4);
      end;
      strWork := Species[NodeCount].SpeciesName + Species[NodeCount - 1].SpeciesName + ';';
      inx := 1;
      iny := 0;
      strDigits := '';
      while strWork[inx] <> ';' do
      begin
        if strWork[inx] in ['0'..'9'] then
          strDigits := strDigits + strWork[inx]
        else
          if Length(strDigits) > 0 then
          begin
            intSpcs := StrToInt(strDigits);
            arryinxBracket[0][iny] := intSpcs;
            Species[intSpcs].inxBracket := iny;
            strDigits := '';
            inc(iny);
          end;
        inc(inx);
      end;
      for inx :=  0 to NodeCount do
        Cells[inx + 1, 34] := IntToStr(Species[inx].inxBracket);
      with Species[NodeCount + 1] do
        NewickToken := '----';
      Cells[NodeCount + 2, 16] := Species[NodeCount + 1].NewickToken;
    end;
    Inc(intNC, 2);
    prgBar.Position := intNC;
    txtIN.Caption := IntToStr(intNC - 1);
    txtIN.Refresh;
    MessageDlg('Make Tree...Done', mtInformation, [mbOK], 0);
  except
    on E:exception do begin
      dblLowestValue := kInvalidData;
      ColX := 0;
      RowY := 0;
      strError := E.Message;
      if strlstCalcErrs.Count = intErrs then
        strlstCalcErrs.Add(strErrorHdrFtr('', 'Calculate Make a Branch.  Negative branch length set to zero.', True));
      strlstCalcErrs.Add(strError);
      MessageDlg(strError, mtError, [mbOK], 0);
    end;
  end;
 finally
   frmPhyIoM.strgrdDataTableProp.AutoSizeColumns;
   if strlstCalcErrs.Count > intErrs then
   begin
     strlstCalcErrs.Add(strErrorHdrFtr('', 'Calculate Make a Branch.  Negative branch lengths set to zero.', False));
     frmPhyIoM.mnuShowCalcErrs.Enabled := True;
     modCalcErrs.frmCalculatedErrors.Show;
   end;
   Screen.Cursor := csrSave;
 end;
end;

{
  Function builds an array of integers from string for a branch.  The number are
  stripped out the string and each number adde to the string.

Parameters:
  strBranch -> string representing the indices branch with <, >, and , delimeters
  Result -> array of integers for all numbers in the string
}
function ariFindElements(const strBranch: string): T1DArrayInteger;
var
  strN,
  strWork: string;
  inx,
  iny,
  inz,
  inxf,
  inxl: integer;
  boolFirstDigit: boolean;
begin
  SetLength(Result, 0);
  if strBranch = strBlank then
    exit
  else
  begin
    strWork := strBranch;
    inx := 1;
    inz := 0;
    while Length(strWork) > inx do
    begin
      boolFirstDigit := True;
      strN := strBlank;
      if strWork[inx] in ['0'..'9'] then
      begin
        if boolFirstDigit then
        begin
          inxf := inx;
          boolFirstDigit := False;
        end;
        inxl := inx;
        inc(inx);
      end
      else
      begin
        for iny := inxf to inxl do
          strN := strN + strBranch[iny];
        SetLength(Result, inz + 1);
        Result[inz] := StrToInt(StrN);
        inc(inz)
      end;
      inc(inx);
    end;
  end;
end;

{
Not used yet.
  Function determines if two strings are the same branch described differently.
  Returns boolean true if describing the same branch false if not.

Parameters:
  strOriginal -> string of a branch from original tree
  strBootStrap -> string of a branch from a replicate tree
  Result -> boolean value whether strings are matching
}
function boolMatchElements(const strOriginal, strBootStrap: string): boolean;
begin

end;

{
  Name: FindBranches
  Performs housekeeping creates or clear arrays for bootstraps matches.  Calculates
  tree for each replicate.  Creates branches by finding lowest leaf and/or node pair
  to make a distance line.  This is contined for each replicate calculation until
  there are 3 nodes or leaves left.  At this point the last 3 distances must be
  calculated by three line elimination method for each replicate calculation.  This
  fundamemtally the same as the original tree calculation but done for each replicate.
  The node names for branches are a collection of indices, commas, greater than brackets,
  and less than brackets.  Where <0,1> forms the branch for first and second leaves.
  The node names follow the rule loewst number first and then shortest bracket pair <>
  next.  This forms new nodes in terms of leaves and other child nodes.  When node
  are compared they must be an exact match to count as a matched replicate to the
  original tree.  The nodes <0,<2,<5,8>>> and <1,<2,<5,8>>> don't match but child
  nodes <2,<5,8>> on both nodes would count as a match.  The indices are in a zero
  based array.  This procedures populates the matches for bootstraps in species and
  nodes record array.

Parameters:
  intRplcts -> number of replicate datasets to be compared to original tree
  strError -> string contains error messages from a failed calculation
}
procedure FindBranches(const intRplcts: Integer; var strError: string);
var
  crsSave: TCursor;
  strNdNm,
  strWork,
  strHiddenNode: string;
  intErrs,
  int1stNdix,
  int2ndNdix,
  intNodeCount,
  intClcltdNd,
  intADMCC,                           // Current Distance Matrix size
  intAQMCC,                           // Current Q Matrix size
  ColX,
  RowY,
  ink,
  inx,
  inxQI,
  inr,
  iny: integer;
  dblX,
  dblY,
  dblZ,
  dblLowestValue,
  dblAdd,
  dblMinus,
  dblFirst: double;
  boolSplitter,
  boolEndRun: boolean;
const
  kFirstCol = 1;
  kFirstRow = 0;

begin
  crsSave := Screen.Cursor;
  Screen.Cursor := crHourglass;
  with TestTableForSpeciesData, frmPhyIoM do
  try
    try
      intErrs := strlstCalcErrs.Count;
      prgbrFileAction.Max := intRplcts;
      prgbrFileAction.Position := 0;
      SetLength(arstrNodeNames, intRplcts);
      SetLength(iarRplctMtchs, NodeCount - SpeciesCount + 1);
      SetLength(BSPercent, NodeCount - SpeciesCount + 1);
      for inx := 0 to NodeCount - SpeciesCount do
        iarRplctMtchs[inx] := 0;
      for inr := 0 to intRplcts - 1 do
      begin
        intNodeCount := SpeciesCount;
        intClcltdNd := 2 * SpeciesCount - 3;
        inxQI := 0;
        SetLength(iarQIndex, intClcltdNd);
        SetLength(dblarBrnchLngths, intClcltdNd + 1);
        SetLength(arstrNodeNames[inr], intClcltdNd + 1);
        SetLength(boolarIsActive, intClcltdNd);
        intADMCC := SpeciesCount + inxQI - 1;
        intAQMCC := SpeciesCount - inxQI;
        SetLength(darBSQMatrix, intAQMCC - 3);
        SetLength(arinxSpcsQM, intAQMCC);
        ink := 0;
        for inx := 0 to intClcltdNd - 1 do
          if inx >= SpeciesCount then
          begin
            boolarIsActive[inx] := False;
            iarQIndex[inx] := kintDisabledSpcs;
          end
          else
          begin
            boolarIsActive[inx] := True;
            arstrNodeNames[inr][inx] := Species[inx].SpeciesName;
            iarQIndex[inx] := ink;
            arinxSpcsQM[ink] := inx;
            inc(ink);
          end;
        boolEndRun := False;
        repeat
          AssignQMatrix(darBSQMatrix, boolarIsActive, iarQIndex, darBSDistanceMatrix[inr], intADMCC, intAQMCC, inxQI, inr);
          ColX := arinxSpcsQM[kFirstCol];
          RowY := arinxSpcsQM[kFirstRow];
          dblLowestValue := darBSQMatrix[inxQI][kFirstCol, kFirstRow]; //[ColX, RowY];
          for iny := 0 to SpeciesCount - inxQI - 1 do
            for inx := SpeciesCount - inxQI - 1 downto iny + 1 do
              if darBSQMatrix[inxQI][inx, iny] < dblLowestValue then
              begin
                dblLowestValue := darBSQMatrix[inxQI][inx, iny];
                ColX := arinxSpcsQM[inx];
                RowY := arinxSpcsQM[iny];
              end;
          dblAdd := dblZero;
          dblMinus := dblZero;
          for iny := 0 to intNodeCount - 1 do
            if boolarIsActive[iny] then
              dblAdd := dblAdd + darBSDistanceMatrix[inr][ColX, iny];
          for iny := 0 to intNodeCount - 1 do
            if boolarIsActive[iny] then
              dblMinus := dblMinus - darBSDistanceMatrix[inr][RowY, iny];
          dblFirst := dblAdd + dblMinus;
          dblarBrnchLngths[ColX] := 0.5 * darBSDistanceMatrix[inr][ColX, RowY] + 0.5 / (SpeciesCount - 2 - inxQI) * dblFirst;
          dblarBrnchLngths[RowY] := darBSDistanceMatrix[inr][ColX, RowY] - dblarBrnchLngths[ColX];
          boolarIsActive[ColX] := False;
          boolarIsActive[RowY] := False;
          boolarIsActive[intNodeCount] := True;
          strNdNm := '<';
          if (ColX < SpeciesCount) and (RowY < SpeciesCount) then
            if RowY > ColX then  // original
              strNdNm := strNdNm + IntToStr(ColX) + ',' + IntToStr(RowY)
            else
              strNdNm := strNdNm + IntToStr(RowY) + ',' + IntToStr(ColX)
          else
            if ColX < SpeciesCount then
               strNdNm := strNdNm + IntToStr(ColX) + ',' + arstrNodeNames[inr][RowY]
            else
              if RowY < SpeciesCount then
                strNdNm := strNdNm + IntToStr(RowY) + ',' + arstrNodeNames[inr][ColX]
              else
                if Length(arstrNodeNames[inr][ColX]) > Length(arstrNodeNames[inr][RowY]) then
                  strNdNm := strNdNm + arstrNodeNames[inr][RowY] + ',' + arstrNodeNames[inr][ColX]
                else
                  if Length(arstrNodeNames[inr][ColX]) < Length(arstrNodeNames[inr][RowY]) then
                    strNdNm := strNdNm + arstrNodeNames[inr][ColX] + ',' + arstrNodeNames[inr][RowY]
                  else
                    if boolFindLeadLowElement(arstrNodeNames[inr][ColX], arstrNodeNames[inr][RowY]) then
                      strNdNm := strNdNm + arstrNodeNames[inr][ColX] + ',' + arstrNodeNames[inr][RowY]
                    else
                      strNdNm := strNdNm + arstrNodeNames[inr][RowY] + ',' + arstrNodeNames[inr][ColX];
          arstrNodeNames[inr][intNodeCount] := strNdNm + '>';
          Inc(intNodeCount);
          SetLength(darBSDistanceMatrix[inr], intNodeCount, intNodeCount);
          for iny := 0 to intNodeCount - 2 do
            if ColX <> iny then
              if iny <> RowY then
                if boolarIsActive[iny] then
                begin
                  dblFirst := 0.5 * (darBSDistanceMatrix[inr][iny, RowY] + darBSDistanceMatrix[inr][iny, ColX] - darBSDistanceMatrix[inr][ColX, RowY]);
                  darBSDistanceMatrix[inr][intNodeCount - 1, iny] := dblFirst;
                  darBSDistanceMatrix[inr][iny, intNodeCount - 1] := dblFirst;
                end;
          inc(inxQI);
          intADMCC := SpeciesCount + inxQI - 1;
          intAQMCC := SpeciesCount - inxQI;
          if SpeciesCount - inxQI > 3 then
          begin
            ink := 0;
            SetLength(arinxSpcsQM, ink);
            for iny := 0 to intADMCC do
              if boolarIsActive[iny] then  // Find remaining active points
              begin
                SetLength(arinxSpcsQM, ink + 1);
                arinxSpcsQM[ink] := iny;   // Active point found
                iarQIndex[iny] := ink;
                inc(ink);
              end
              else
                iarQIndex[iny] := kintDisabledSpcs;
          end
          else
             boolEndRun := True;
          strError := '';
        until boolEndRun;
        ink := 0;
        SetLength(arinxSpcsQM, ink);
        for iny := 0 to intADMCC do
          if boolarIsActive[iny] then  // Find remaining active points
          begin
            SetLength(arinxSpcsQM, ink + 1);
            arinxSpcsQM[ink] := iny;    // Active point found
            iarQIndex[iny] := ink;
            inc(ink);
          end
          else
            iarQIndex[iny] := kintDisabledSpcs;
        dblarDstcn[0] := darBSDistanceMatrix[inr][arinxSpcsQM[0], arinxSpcsQM[2]]; // X + Y
        dblarDstcn[1] := darBSDistanceMatrix[inr][arinxSpcsQM[1], arinxSpcsQM[2]]; // Y + Z
        dblarDstcn[2] := darBSDistanceMatrix[inr][arinxSpcsQM[0], arinxSpcsQM[1]]; // X + Z
        dblX := (dblarDstcn[0] + dblarDstcn[1] - dblarDstcn[2]) / 2.0; // Calculate X
        dblY := dblarDstcn[1] - dblX;                                  // Calculate Y
        dblZ := dblarDstcn[0] - dblX;                                  // Calculate Z
        with TestTableForSpeciesData do
        begin
          dblarBrnchLngths[arinxSpcsQM[0]] := dblZ;              // Place Z in first active node
          dblarBrnchLngths[arinxSpcsQM[1]] := dblY;              // Place Y in second active node
          dblarBrnchLngths[arinxSpcsQM[2]] := dblX;              // Place Z in third active node
        end;
        SetLength(arinxSpcsQM, 0);
        SetLength(iarQIndex, 0);
        SetLength(dblarBrnchLngths, 0);
        SetLength(darBSQMatrix, 0);
//        strHiddenNode := 'Node';
        boolSplitter := True;
        strNdNm := '<';
        for inx := 0 to NodeCount - 2 do
          if boolarIsActive[inx] then
            if boolSplitter then
            begin
              int1stNdix := inx;
              boolSplitter := False;
            end
            else
            begin
              int2ndNdix := inx;
              break;
            end;
        if (int1stNdix < SpeciesCount) and (int2ndNdix < SpeciesCount) then
          if int2ndNdix > int1stNdix then  // original
            strNdNm := strNdNm + IntToStr(int1stNdix) + ',' + IntToStr(int2ndNdix)
          else
            strNdNm := strNdNm + IntToStr(int2ndNdix) + ',' + IntToStr(int1stNdix)
        else
          if int1stNdix < SpeciesCount then
            strNdNm := strNdNm + IntToStr(int1stNdix) + ',' + arstrNodeNames[inr][int2ndNdix]
          else
            if int2ndNdix < SpeciesCount then
              strNdNm := strNdNm + IntToStr(int2ndNdix) + ',' + arstrNodeNames[inr][int1stNdix]
            else
              if Length(arstrNodeNames[inr][int2ndNdix]) > Length(arstrNodeNames[inr][int1stNdix]) then
                strNdNm := strNdNm + arstrNodeNames[inr][int2ndNdix] + ',' + arstrNodeNames[inr][int1stNdix]
              else
                if Length(arstrNodeNames[inr][int2ndNdix]) < Length(arstrNodeNames[inr][int1stNdix]) then
                  strNdNm := strNdNm + arstrNodeNames[inr][int1stNdix] + ',' + arstrNodeNames[inr][int2ndNdix]
                else
                  if boolFindLeadLowElement(arstrNodeNames[inr][int1stNdix], arstrNodeNames[inr][int2ndNdix]) then
                    strNdNm := strNdNm + arstrNodeNames[inr][int1stNdix] + ',' + arstrNodeNames[inr][int2ndNdix]
                  else
                    strNdNm := strNdNm + arstrNodeNames[inr][int2ndNdix] + ',' + arstrNodeNames[inr][int1stNdix];
        arstrNodeNames[inr][intNodeCount] := strNdNm + '>';
        with Species[NodeCount + 1] do
          NewickToken := '----';
//        for inx := 0 to intNodeCount - 2 do
//          if boolarIsActive[inx] then
//          begin
//            strHiddenNode := strHiddenNode + IntToStr(inx);
//            if boolSplitter then
//            begin
//              strHiddenNode := strHiddenNode + '<|>';
//              boolSplitter := False;
//            end
//            else
//              break;
//          end;
        for inx := SpeciesCount to intNodeCount do   // - i
        begin
          for iny := SpeciesCount to intNodeCount do   // - i
            if arstrNodeNames[inr][iny] = Species[inx].SpeciesName then
              inc(iarRplctMtchs[inx - SpeciesCount]);
//          if strHiddenNode = Species[inx].SpeciesName then
//            inc(iarRplctMtchs[inx - SpeciesCount]);
        end;
//        for iny := SpeciesCount to intNodeCount do   // - 1
//          if arstrNodeNames[inr][iny] = HiddenNodeName then
//            inc(HiddenMatches);
//        if strHiddenNode = HiddenNodeName then
//          inc(HiddenMatches);
        prgbrFileAction.Position :=  inr + 1;
      end;
      with strgrdDataTableProp do
      begin
        for inx := 1 to NodeCount do
          if inx < SpeciesCount + 1 then
            Cells[inx, 32] := '     ----'
          else
          begin
            BSPercent[inx - SpeciesCount - 1] := iarRplctMtchs[inx - SpeciesCount - 1]/intRplcts * 100.0;
            Cells[inx, 32] := '[' + FloatToStrF(BSPercent[inx - SpeciesCount - 1], ffFixed, 8, 0) + ']';
            if inx < NodeCount then
              Cells[inx, 16] := '()' + ':' + Cells[inx, 4] + Cells[inx, 32]
            else
            begin
              HiddenPercent := iarRplctMtchs[inx - SpeciesCount - 1] / intRplcts * 100.0;
              BSPercent[inx - SpeciesCount] := BSPercent[inx - SpeciesCount - 1];
              Cells[inx, 16] := ')' + ':' + FloatToStrF(Species[inx - 1].BranchLength, ffFixed, 12, 8) + Cells[inx, 32]; // + ',' +
              Cells[inx + 1, 32] := '[' + FloatToStrF(HiddenPercent, ffFixed, 8, 0) + ']';
              Cells[inx + 1, 16] := ')' + ':' + FloatToStrF(Species[inx].BranchLength, ffFixed, 12, 8) + Cells[inx + 1, 32];
              Species[inx].NewickToken := Cells[inx + 1, 16];
            end;
            Species[inx - 1].NewickToken := Cells[inx, 16];
          end;
        for inx := SpeciesCount to NodeCount - 2 do
        begin
          strWork := '';
          for iny := inx - 1 downto 0 do
            if Species[iny].NodeParent = inx then
              strWork := strWork + Species[iny].NewickToken + ',';
            if Length(strWork) > 0 then
              Delete(strWork, Length(strWork), 1);
            Species[inx].NewickToken := Species[inx].NewickToken[1] + strWork + Copy(Species[inx].NewickToken, 2, Length(Species[inx].NewickToken) - 1);
            Cells[inx + 1, 16] := Species[inx].NewickToken;
        end;
      end;
      SetLength(boolarIsActive, 0);
    except
      on E:exception do begin
        dblLowestValue := kInvalidData;
        ColX := 0;
        RowY := 0;
        strError := E.Message;
        if strlstCalcErrs.Count = intErrs then
          strlstCalcErrs.Add(strErrorHdrFtr('', 'Calculate Newick format and bootstrapping', True));
        strlstCalcErrs.Add(strError);
        MessageDlg(strError, mtError, [mbOK], 0);
      end;
    end;
  finally
    strgrdDataTableProp.AutoSizeColumns;
    if strlstCalcErrs.Count > intErrs then
    begin
      strlstCalcErrs.Add(strErrorHdrFtr('', 'Calculate Newick format and bootstrapping', False));
      mnuShowCalcErrs.Enabled := True;
      modCalcErrs.frmCalculatedErrors.Show;
    end;
    Screen.Cursor := crsSave;
  end;
end;

{
  Function converts degrees to radians.  Returning the readians in an extended variable.

Parameters:
  extDegrees -> Degrees value to be converted in extended format
  Result -> Returns radians in extended format
}
function DegToRad(const extDegrees: Extended): Extended;
begin
  Result := extDegrees * Pi / kSemiCircle;
end;

{
  Function returns true if parent at int1stXNd is not -1 and sibling at int2ndXNd
  is not -1 otherwise it returns false.

Parameters:
  intNd -> the node will be interrogated to determine if it is resolved
  Result -> Returns a boolean value for node
}
function IsBeginNodeResolved(const intNd: integer): boolean;
begin
  with TestTableForSpeciesData.Species[intNd].BeginSegment do
    if (int1stXNd <> kUnknownNode) and (int2ndXNd <> kUnknownNode) then
      Result := True
    else
      Result := False;
end;

{
  Function returns true if first child at int1stXNd is not -1 and second chld at
  int2ndXNd is not -1 otherwise it returns false.

Parameters:
  intNd -> the node will be interrogated to determine if it is resolved
  Result -> Returns a boolean value for node
}
function IsEndNodeResolved(const intNd: integer): boolean;
begin
  with TestTableForSpeciesData.Species[intNd].EndSegment do
    if (int1stXNd <> kUnknownNode) and (int2ndXNd <> kUnknownNode) then
      Result := True
    else
      Result := False;
end;

{
  Function returns true if begin and end segments are resolved calling IsBeginNodeResolved
  and IsEndNodeResolved.

Parameters:
  intNd -> the node will be interrogated to determine if it is resolved
  Result -> Returns a boolean value for node
}
function IsSegmentResolved(const intNd: integer): boolean;
begin
  if IsBeginNodeResolved(intNd) then
    if IsEndNodeResolved(intNd) then
      Result := True
    else
      Result := False
  else
    Result := False;
end;

{
  Function returns an integer value for node that is unresolved.  If no nodes are
  unresolved it the unknown node or node could not be found.  It tests this by testing
  all node points associated with this node by checking end segment int1stXNd and
  int2ndXNd, begin segment int1stXNd and int2ndXNd,  Node parent, and parent of node
  parent.

Parameters:
  intNd -> the node will be interrogated to determine if it is resolved
  Result -> Returns an intger value for the unresolved node
}
function intAllNdSgmntRslvd(const intNd: integer): integer;
begin
  if IsSegmentResolved(TestTableForSpeciesData.Species[intNd].BeginSegment.int1stXNd) then
    if IsSegmentResolved(TestTableForSpeciesData.Species[intNd].BeginSegment.int2ndXNd) then
      if IsSegmentResolved(TestTableForSpeciesData.Species[intNd].EndSegment.int1stXNd) then
        if IsSegmentResolved(TestTableForSpeciesData.Species[intNd].EndSegment.int2ndXNd) then
          if IsSegmentResolved(TestTableForSpeciesData.Species[intNd].NodeParent) then
            if IsSegmentResolved(TestTableForSpeciesData.Species[TestTableForSpeciesData.Species[intNd].NodeParent].NodeParent) then
              Result := kUnknownNode
            else
              Result := TestTableForSpeciesData.Species[TestTableForSpeciesData.Species[intNd].NodeParent].NodeParent
          else
            Result := TestTableForSpeciesData.Species[intNd].NodeParent
        else
          Result := TestTableForSpeciesData.Species[intNd].EndSegment.int2ndXNd
      else
        Result := TestTableForSpeciesData.Species[intNd].EndSegment.int1stXNd
    else
      Result := TestTableForSpeciesData.Species[intNd].BeginSegment.int2ndXNd
  else
    Result := TestTableForSpeciesData.Species[intNd].BeginSegment.int1stXNd;
end;

{
  Function to assign maximum length of branches which is largest double value.

Parameters:
  Result -> returns an double of highest branch length
}
function dblFindMaxLengths: double;
var
  inx: integer;
begin
  with TestTableForSpeciesData do
  begin
    Result := Species[0].BranchLength;
    for inx := 1 to NodeCount - 1 do
      if Species[inx].BranchLength > Result then
       Result := Species[inx].BranchLength;
  end;
end;

{
  Function to assign minimum length of branches which is lowest double value.

Parameters:
  Result -> returns an double of shortest branch length
}
function dblFindMinLengths: double;
var
  inx: integer;
begin
  with TestTableForSpeciesData do
  begin
    Result := Species[0].BranchLength;
    for inx := 1 to NodeCount - 1 do
      if Species[inx].BranchLength > dblApproxZero then
        if Species[inx].BranchLength < Result then
          Result := Species[inx].BranchLength;
  end;
end;

{
  Functions returns value of pixels shown to be seen branches on each type of graph.

Parameters:
  Result -> an integer value so all branches can be seen
}
function intMaxPixelFactor(const wrdDsplyTr: word; var dblScale: double): integer;
var
  intMinPixels,
  intPixels: integer;
  dblFactor,
  dblMin,
  dblMax: double;

begin
  case wrdDsplyTr of
    kRadialTree:
      if Origin.ttoRadial.Width > Origin.ttoRadial.Height then
        intPixels := Origin.ttoRadial.Height
      else
        intPixels := Origin.ttoRadial.Width;
    kBracketTree:
      if Origin.ttoBracket.Width > Origin.ttoBracket.Height then
        intPixels := Origin.ttoBracket.Height
      else
        intPixels := Origin.ttoBracket.Width;
    kTopologyOnlyTree:
      if Origin.ttoTplgyOnly.Width > Origin.ttoTplgyOnly.Height then
        intPixels := Origin.ttoTplgyOnly.Height
      else
        intPixels := Origin.ttoTplgyOnly.Width;
    kNwckBrcktTree:
      if Origin.ttoNwckBrckt.Width > Origin.ttoNwckBrckt.Height then
        intPixels := Origin.ttoNwckBrckt.Height
      else
        intPixels := Origin.ttoNwckBrckt.Width;
    kNewickTOTree:
      if Origin.ttoNewickTO.Width > Origin.ttoNewickTO.Height then
        intPixels := Origin.ttoNewickTO.Height
      else
        intPixels := Origin.ttoNewickTO.Width;
  end;
  dblMin := dblFindMinLengths;
  if dblMin = dblZero then
    dblMin := dblApproxZero;
  dblMax := dblFindMaxLengths;
//  if dblMin = dblZero then
//    dblFactor := 100000 //1600.0
//  else
  dblFactor := dblMax / dblMin;
{  if dblMax < 10.0 then
    if dblMin < 1.0 then
    begin
      intMinPixels := Trunc(1.0 / dblMin);
      if intMinPixels > 100 then
        Result := 100
      else
        Result := intMinPixels;
    end
    else
      Result := 10
  else}
  if dblFactor > 1500.0 then
    Result := 1
  else
    Result := Trunc(500.0 / dblMax);
//    if dblFactor > 100.0 then
//      Result := 10.0
//    else
//      if dblFactor > 10.0 then
//        Result := 100.0
//      else
//        if dblFactor > 1.0 then
//          Result := 1000.0;
//    if dblMin < 1.0 then
//      if dblMax > 100.0 then
//        Result := 10
//      else
//        if dblMax > 10.0 then
//          Result := 100
//        else
//          if dblMin < 0.0625 then
//            Result := 16000
//          else
//            if dblMin < 0.125 then
//              Result := 8000
//            else
//              if dblMin < 0.25 then
//                Result := 4000
//              else
//                if dblMin < 0.5 then
//                  Result := 2000
//                else
//                  if dblMin < 2.5 then
//                    Result := 400
//                  else
//                    Result := 250
//    else
//      Result := (intPixels + intPixels div 2) div Trunc(dblMax * 1.5);
  dblScale := 100.0 / Result;
{  intSF := 1;
  while (intPF > 0) and (intPF < 10) do
  begin
    dblMin := dblMin * 10.0;
    intPF := Trunc(dblMin);
    intSF := intSF * 10;
  end;
  intPF := Trunc(dblMax);
  while (intPF > 0) and (intPF < 10) do
  begin
    dblMax := dblMax / 10.0;
    intPF := Trunc(dblMax);
    intSF := intSF * 10;
  end;
  Result := intSF;}
end;

{
  Function to assign quadrant that angle is pointing to 0-90 SE, 90-180 SW, 180-270
  NW, and 270-360 or 0 NE.  Function returns SE, SW, NW, or NE.

Parameters:
  sngAngle -> value of angle for branch on radial graph
  Result -> Returns SE, SW, NW, or NE for a compass point.
}
function FindQudrant(sngAngle: single): CompassPnt;
var
  intAngle: integer;
begin
  intAngle := Trunc(sngAngle * 1000);
  if intAngle mod 90000 = 0 then
    result := CompassPnt(intAngle div 90000 * 2)
  else
    result := CompassPnt(intAngle div 90000 * 2 + 1);
end;

{
  Function to assign minimum X position on radial graph which includes left based
  text length.

Parameters:
  Result -> returns an integer of X lowest position store in tree less longest text leaf
}
function intMinRadialPosX: integer;
var
  intTxtLngth,
  inx: integer;
begin
  intTxtLngth := 0;
  Result := 0;
  with TestTableForSpeciesData do
    for inx := 0 to NodeCount - 2 do
      With Species[inx] do
      begin
        if BeginSegment.pntRadial.x + EndSegment.pntRadial.x < Result then
          Result := BeginSegment.pntRadial.x + EndSegment.pntRadial.x;
        if inx < SpeciesCount then
          if Length(SpeciesName) > intTxtLngth then
            intTxtLngth := Length(SpeciesName);
      end;
  Result := Result - intTxtLngth * 8;
end;

{
  Function to assign minimum or top Y position on radial graph.

Parameters:
  Result -> returns an integer of Y lowest or top position store in tree
}
function intMinRadialPosY: integer;
var
  inx: integer;
begin
  Result := 0;
  with TestTableForSpeciesData do
    for inx := 0 to NodeCount - 2 do
    begin
      With Species[inx] do
        if BeginSegment.pntRadial.y + EndSegment.pntRadial.y < Result then
          Result := BeginSegment.pntRadial.y + EndSegment.pntRadial.y;
    end;
end;

{
  Function to assign maximum X position on radial graph which includes right based
  text length.

Parameters:
  Result -> returns an integer of highest X position stored in tree plus longest text leaf
}
function intMaxRadialPosX: integer;
var
  intTxtLngth,
  inx: integer;
begin
  intTxtLngth := 0;
  Result := 0;
  with TestTableForSpeciesData do
  begin
    for inx := 0 to NodeCount - 2 do
      With Species[inx] do
      begin
        if BeginSegment.pntRadial.x + EndSegment.pntRadial.x > Result then
          Result := BeginSegment.pntRadial.x + EndSegment.pntRadial.x;
        if inx < SpeciesCount then
          if Length(SpeciesName) > intTxtLngth then
            intTxtLngth := Length(SpeciesName);
      end;
    Result := Result + intTxtLngth * 8;
  end;
end;

{
Name: intSclDrwngFctr -> shortened for scale drawing factor
  Function to assign integer length for drawing radial branch times pixel factor and
  adding minimum visible length.

Parameters:
  dblLngth -> value of length for branch in a double variable
  dblSinCosVl -> value of sine or cosine result in a double variable
  intPF -> Pixel factor
  intMnLngth -> minimum length for branch to be seen on graph
  Result -> returns a positive or negative number depending on sine or cosine
}
function intSclDrwngFctr(const dblLngth, dblSinCosVl: double; const intPF, intMnLngth: integer): integer;
var
  intSclFctr: integer;
begin
  intSclFctr := Trunc(dblSinCosVl * dblLngth * intPF);
  if dblSinCosVl < dblZero then
    Result := intSclFctr - intMnLngth
  else
    Result := intSclFctr + intMnLngth;
end;

{
  Function to assign maximum Y position on radial graph.

Parameters:
  Result -> returns an integer of X highest position store in tree
}
function intMaxRadialPosY: integer;
var
  inx: integer;
begin
  Result := 0;
  with TestTableForSpeciesData do
  begin
    for inx := 0 to NodeCount - 2 do
      With Species[inx] do
        if BeginSegment.pntRadial.y + EndSegment.pntRadial.y > Result then
          Result := BeginSegment.pntRadial.y + EndSegment.pntRadial.y;
  end;
end;

{
  Procedure to create an unrooted radial graph populating end and begin segments in
  Species records.  Forming angles, begin segment containing location on chart for
  branch, and end segment containing length integers converted with sine and cosine.
  Sets angle step by dividing 360 by number of leaves.  First leaf angle is set to
  double of leaf step angle and divided by Species count plus one and each remaining
  leaf angle adds leaf step angle.  The position 200,200 is default position for starting
  the tree which iniitially starts as a star tree and pushing leaf branches as interior
  branches are inserted in the tree.  The order of the leaves is determined by the
  topology tree.  The begin segment holds the starting position of the branch and
  the end segment holds the length converting cosine for x and sine for y.  The length
  is converted to sine or cosine by angle branch times branch length converted to
  integer times pixel factor plus minimum visible length.  After all branches have
  been calculated with angles and magnitudes the branches are pushed out from root
  branch.  This continues until it reaches all the leaves.  Data is stored in the
  radial section of the SpeciesAndNode type as a dynamic array.  Finally populates
  the Make Tree grid setting angles, begin segments, and end segments into the grid.
}
procedure CreateRadialGraph;
const
  kMnmLngth = 4;
var
  inx,
  iny,
  inz,
  intMaxX,
  intMaxY,
  intMinX,
  intMinY,
  int1stPnt,
  int2ndPnt: integer;
  GrphPnt: TPoint;
  ariFrstSgmntPnt: TSegmentPoints;
//  ariNextOpenNds,
//  ariOpenNodes: array of integer;
  dblDegree,
  dblLeafAngleStep: double;
begin
  if boolGraphInitial then
  begin
    with TestTableForSpeciesData do
    begin
      intPixelFactor := intMaxPixelFactor(kRadialTree, dblScale); // * 10;
      dblLeafAngleStep := kFullCircle / SpeciesCount;
      dblDegree := dblLeafAngleStep * 2 / (SpeciesCount + 1){ + kRghtAnglBsctd + kSemiCircle};
//      if dblDegree >= kFullCircle then
//        dblDegree := dblDegree - kFullCircle;
      for iny := 0 to SpeciesCount - 1 do
        for inx := 0 to SpeciesCount - 1 do
          if iny = Species[inx].inxBracket then
          begin
            Species[inx].dblAngle := dblDegree;
            Species[inx].BeginSegment.pntRadial.x := 200;
            Species[inx].BeginSegment.pntRadial.y := 200;
            Species[inx].EndSegment.pntRadial.x := intSclDrwngFctr(Species[inx].BranchLength, cos(DegToRad(dblDegree)), intPixelFactor, kMnmLngth);
            Species[inx].EndSegment.pntRadial.y := intSclDrwngFctr(Species[inx].BranchLength, sin(DegToRad(dblDegree)), intPixelFactor, kMnmLngth);
            dblDegree := dblDegree + dblLeafAngleStep;
//            if dblDegree < dblZero then
//              dblDegree := kFullCircle + dblDegree
//            else
//              if dblDegree >= kFullCircle then
//                dblDegree := dblDegree - kFullCircle;
            break;
          end;
      for iny := SpeciesCount to NodeCount - 2 do
        for inx := SpeciesCount to NodeCount - 2 do
          if Species[inx].dblAngle = -1.0 then
            if (Species[Species[inx].EndSegment.int1stXNd].dblAngle <> -1.0) and (Species[Species[inx].EndSegment.int2ndXNd].dblAngle <> -1.0) then
            begin
              Species[inx].dblAngle := (Species[Species[inx].EndSegment.int1stXNd].dblAngle + Species[Species[inx].EndSegment.int2ndXNd].dblAngle) / 2.0;
              Species[inx].BeginSegment.pntRadial.x := 200;
              Species[inx].BeginSegment.pntRadial.y := 200;
              Species[inx].EndSegment.pntRadial.x := intSclDrwngFctr(Species[inx].BranchLength, cos(DegToRad(Species[inx].dblAngle)), intPixelFactor, kMnmLngth);
              Species[inx].EndSegment.pntRadial.y := intSclDrwngFctr(Species[inx].BranchLength, sin(DegToRad(Species[inx].dblAngle)), intPixelFactor, kMnmLngth);
              Species[Species[inx].EndSegment.int1stXNd].BeginSegment.pntRadial.x := Species[inx].EndSegment.pntRadial.x + Species[inx].BeginSegment.pntRadial.x;
              Species[Species[inx].EndSegment.int1stXNd].BeginSegment.pntRadial.y := Species[inx].BeginSegment.pntRadial.y + Species[inx].EndSegment.pntRadial.y;
              Species[Species[inx].EndSegment.int2ndXNd].BeginSegment.pntRadial.x := Species[inx].EndSegment.pntRadial.x + Species[inx].BeginSegment.pntRadial.x;
              Species[Species[inx].EndSegment.int2ndXNd].BeginSegment.pntRadial.y := Species[inx].BeginSegment.pntRadial.y + Species[inx].EndSegment.pntRadial.y;
            end;
       Species[NodeCount - 1].dblAngle := kSemiCircle;
       Species[NodeCount - 1].BeginSegment.pntRadial.x := 200;
       Species[NodeCount - 1].BeginSegment.pntRadial.y := 200;
       Species[NodeCount - 1].EndSegment.pntRadial.x := -Trunc((Species[NodeCount - 1].BranchLength) {* cos(DegToRad(Species[NodeCount - 1].dblAngle))}) * intPixelFactor - kMnmLngth;
       Species[NodeCount - 1].EndSegment.pntRadial.y := 0; //Trunc((Species[NodeCount - 1].BranchLength) * sin(DegToRad(Species[NodeCount - 1].dblAngle))) * intPixelFactor + 1;
       Species[Species[NodeCount - 1].EndSegment.int1stXNd].BeginSegment.pntRadial.x := Species[NodeCount - 1].EndSegment.pntRadial.x + Species[NodeCount - 1].BeginSegment.pntRadial.x;
       Species[Species[NodeCount - 1].EndSegment.int1stXNd].BeginSegment.pntRadial.y := Species[NodeCount - 1].BeginSegment.pntRadial.y + Species[NodeCount - 1].EndSegment.pntRadial.y;
       Species[Species[NodeCount - 1].EndSegment.int2ndXNd].BeginSegment.pntRadial.x := Species[NodeCount - 1].EndSegment.pntRadial.x + Species[NodeCount - 1].BeginSegment.pntRadial.x;
       Species[Species[NodeCount - 1].EndSegment.int2ndXNd].BeginSegment.pntRadial.y := Species[NodeCount - 1].BeginSegment.pntRadial.y + Species[NodeCount - 1].EndSegment.pntRadial.y;
       Species[NodeCount].dblAngle := dblZero;
       Species[NodeCount].BeginSegment.pntRadial.x := 200;
       Species[NodeCount].BeginSegment.pntRadial.y := 200;
       Species[NodeCount].EndSegment.pntRadial.x := Trunc((Species[NodeCount].BranchLength) {* cos(DegToRad(Species[NodeCount].dblAngle))}) * intPixelFactor + kMnmLngth;
       Species[NodeCount].EndSegment.pntRadial.y := 0; //Trunc((Species[NodeCount].BranchLength) * sin(DegToRad(Species[NodeCount].dblAngle))) * intPixelFactor + 1;
       Species[Species[NodeCount].EndSegment.int1stXNd].BeginSegment.pntRadial.x := Species[NodeCount].EndSegment.pntRadial.x + Species[NodeCount].BeginSegment.pntRadial.x;
       Species[Species[NodeCount].EndSegment.int1stXNd].BeginSegment.pntRadial.y := Species[NodeCount].BeginSegment.pntRadial.y + Species[NodeCount].EndSegment.pntRadial.y;
       Species[Species[NodeCount].EndSegment.int2ndXNd].BeginSegment.pntRadial.x := Species[NodeCount].EndSegment.pntRadial.x + Species[NodeCount].BeginSegment.pntRadial.x;
       Species[Species[NodeCount].EndSegment.int2ndXNd].BeginSegment.pntRadial.y := Species[NodeCount].BeginSegment.pntRadial.y + Species[NodeCount].EndSegment.pntRadial.y;
       Species[Species[NodeCount - 1].EndSegment.int1stXNd].boolPushed := True;
       Species[Species[NodeCount - 1].EndSegment.int2ndXNd].boolPushed := True;
       Species[Species[NodeCount].EndSegment.int1stXNd].boolPushed := True;
       Species[Species[NodeCount].EndSegment.int2ndXNd].boolPushed := True;
       for iny := SpeciesCount to NodeCount - 2 do
         for inx := SpeciesCount to NodeCount - 2 do
           if Species[inx].boolPushed then
           begin
             if Species[inx].EndSegment.int1stXNd <> Species[inx].EndSegment.int2ndXNd then
             begin
               Species[Species[inx].EndSegment.int1stXNd].BeginSegment.pntRadial.x := Species[inx].EndSegment.pntRadial.x + Species[inx].BeginSegment.pntRadial.x;
               Species[Species[inx].EndSegment.int1stXNd].BeginSegment.pntRadial.y := Species[inx].BeginSegment.pntRadial.y + Species[inx].EndSegment.pntRadial.y;
               Species[Species[inx].EndSegment.int2ndXNd].BeginSegment.pntRadial.x := Species[inx].EndSegment.pntRadial.x + Species[inx].BeginSegment.pntRadial.x;
               Species[Species[inx].EndSegment.int2ndXNd].BeginSegment.pntRadial.y := Species[inx].BeginSegment.pntRadial.y + Species[inx].EndSegment.pntRadial.y;
               Species[Species[inx].EndSegment.int1stXNd].boolPushed := True;
               Species[Species[inx].EndSegment.int2ndXNd].boolPushed := True;
             end;
             Species[inx].boolPushed := False;
           end;
    end;
    intMinX := intMinRadialPosX;
    intMinY := intMinRadialPosY;
    intMaxX := intMaxRadialPosX;
    intMaxY := intMaxRadialPosY;
    GrphPnt.x := 50 - intMinX;
    GrphPnt.y := 50 - intMinY;
    AssignRadialRange(GrphPnt, 200 - intMinX + intMaxX, 200 - intMinY + intMaxY);
    with TestTableForSpeciesData do
      with frmPhyIoM.strgrdDataTableProp do
        for inx := 1 to NodeCount + 2 do
        begin
          Cells[inx, 7] := FloatToStrF(Species[inx - 1].dblAngle, ffFixed, 8, 2);
          Cells[inx, 8] := IntToStr(Species[inx - 1].BeginSegment.int1stXNd);
          Cells[inx, 9] := IntToStr(Species[inx - 1].BeginSegment.int2ndXNd);
          Cells[inx, 10] := IntToStr(Species[inx - 1].EndSegment.int1stXNd);
          Cells[inx, 11] := IntToStr(Species[inx - 1].EndSegment.int2ndXNd);
          Cells[inx, 12] := IntToStr(Species[inx - 1].BeginSegment.pntRadial.x);
          Cells[inx, 13] := IntToStr(Species[inx - 1].BeginSegment.pntRadial.y);
          Cells[inx, 14] := IntToStr(Species[inx - 1].EndSegment.pntRadial.x);
          Cells[inx, 15] := IntToStr(Species[inx - 1].EndSegment.pntRadial.y);
        end;
  end;
end;

{
  Function determines highest node level fir the tree which the number nodes in relation
  to thr root.  The root has value 0 and first out grouping is one and next is two.

Parameters:
  Result -> returns highest node levels in an integer value
}
function intHighestNodeLevel: integer;
var
  inx: integer;
begin
  Result := 0;
  with TestTableForSpeciesData do
    for inx := 0 to NodeCount - 1 do
      if Species[inx].NodeLevel > Result then
        Result := Species[inx].NodeLevel;
end;

{
  Function returns true if successful freeing of the treeview nodes and false if not.
  PTreeViewMember holds a pointer TTreeViewMember which contains NodePos, NodeParent,
  NodeSibling, and right or left children.

Parameters:
  Result -> returns a true boolean successful freeing of treeview otherwise returns false
}
function FreeTVMNodes(var pntTVM: PTreeViewMember): boolean;
begin
  Result := False;
  if pntTVM <> nil then
  begin
    if pntTVM^.pntTVMLftChld <> nil then
      if not FreeTVMNodes(pntTVM^.pntTVMLftChld) then
        exit;
    if pntTVM^.pntTVMRghtChld <> nil then
      if not FreeTVMNodes(pntTVM^.pntTVMRghtChld) then
        exit;
    Dispose(pntTVM);
    Result := True;
  end;
end;

{
  Functions places bootstrap value in between brackets in a string value.

Parameters:
  intNP -> integer value of current node position
  Result - > returns string value of bootstrap example: [50] for 50%
}
function strBSBrackets(const intNP: integer): string;
begin
  with TestTableForSpeciesData do
    if boolBootStrap then
      if intNP > SpeciesCount - 1 then
        Result := '[' + FloatToStrF(BSPercent[intNP - SpeciesCount], ffFixed, 6, 0) + ']'
      else
        Result := ''
    else
      Result := '';
end;

{
  Creates node in treeview format to help with selecting a root by giving one item         P
  in the node list.  It takes the position from Specfies array and add a node to the      / \
  end of the node list.  Places the values sibling and parent from begin segment,        S   NP
  and two children from begin segment in pntTVM^.  pntTVM pointer to the branch or          /  \
  leaf data for this node position in the tree.                                            C1   C2

Parameters:
  ndItem -> current TTreeNode parent pointer
  boolIsFirst -> boolean is true this node is first child of parent
  intNP -> Position in the Species Record array
  result -> current view member data stored in created node
}
function CreateTVMNode(var ndItem: TTreeNode; const boolIsFirst: boolean; const intNP: integer): PTreeViewMember;
var
  pntTVM: PTreeViewMember;
  ndCNode: TTreeNode;
begin
  with frmPhyIoM.trvwTreeGraph, TestTableForSpeciesData do
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
      if ndItem = nil then
        ndCNode := Items.AddObjectFirst(ndItem, Species[intNodePos].SpeciesName + '(' + IntToStr(Species[intNodePos].AbsIndex) + ')(' + IntToStr(intNodePos) + '):' + FloatToStrF(Species[intNodePos].BranchLength, ffFixed, 12, 4), pntTVM)
      else
        if boolIsFirst then
          ndCNode := Items.AddChildObjectFirst(ndItem, Species[intNodePos].SpeciesName + '(' + IntToStr(Species[intNodePos].AbsIndex) + ')(' + IntToStr(intNodePos) + '):' + FloatToStrF(Species[intNodePos].BranchLength, ffFixed, 12, 4) + strBSBrackets(intNodePos), pntTVM)
        else
          ndCNode := Items.AddChildObject(ndItem, Species[intNodePos].SpeciesName + '(' + IntToStr(Species[intNodePos].AbsIndex) + ')(' + IntToStr(intNodePos) + '):' + FloatToStrF(Species[intNodePos].BranchLength, ffFixed, 12, 4) + strBSBrackets(intNodePos), pntTVM);
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

{
  First creates tree view node from root from original data.  Places index from topology
  tree from top going down of leaf order for displaying and calculating graphs
}
procedure ShowViewTreeViewValues;
var
  TreeMember: PTreeViewMember;
  inx: integer;
  ndRoot: TTreeNode;
begin
  with frmPhyIoM.trvwTreeGraph, TestTableForSpeciesData do
  try
    try
      ndRoot := nil;
      TreeMember := CreateTVMNode(ndRoot, kboolSecond, NodeCount + 1);
      for inx := 1 to NodeCount + 2 do
        with frmPhyIoM.strgrdDataTableProp do
          Cells[inx, 46] := IntToStr(Species[inx - 1].AbsIndex);
    finally
      frmPhyIoM.trvwTreeGraph.FullExpand;
    end;
  except
    on E:exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);       // Show error message
  end;
end;

function strStripLengthsWithBS(const strNwck: string): string;
var
  strScanned: string;
  inx,
  intCmm,
  intLB,
  intRPn,
  intCln: integer;
begin
  intCln := Pos(':', strNwck);
  if intCln = 0 then
    Result := strNwck
  else
  begin
    Result := '';
    strScanned := strNwck;
    while intCln > 0 do
    begin
      Result := Result + Copy(strScanned, 1, intCln - 1);
      Delete(strScanned, 1, intCln);
      intLB := Pos('[', strScanned);
      intCmm := Pos(',', strScanned);
      intRPn := Pos(')', strScanned);
      if (intLB > 0) or (intCmm > 0) or  (intRPn > 0) then
        if (intLB > 0) and ((intLB < intCmm) or (intCmm = 0)) and ((intLB < intRPn) or (intRPn = 0)) then
          Delete(strScanned, 1, intLB - 1)
        else
          if (intCmm > 0) and ((intCmm < intRPn) or (intRPn = 0)) then
            Delete(strScanned, 1, intCmm - 1)
          else
            if intRPn > 0 then
              Delete(strScanned, 1, intRPn - 1);
      intCln := Pos(':', strScanned);
    end;
    Result := Result + strScanned;
  end;
end;

function strStripLengths(const strNwck: string): string;
var
  strFrontKept,
  strLngthBS,
  strScanned: string;
  inx,
  intCmm,
  intRPn,
  intCln: integer;

function strFindBS(const strNwkBS: string): string;
var
  intLB,
  intRB: integer;
begin
  Result := '';
  intLB := Pos('[', strNwkBS);
  intRB := Pos(']', strNwkBS);
  if (intRB > intLB) and (intLB > 0) then
    Result := Copy(strNwkBS, intLB + 1, intRB - intLB - 1);
end;

function strFindLngths(const strNwkL: string): string;
var
  intLB,
  intRB: integer;
begin
  intLB := Pos('[', strNwkL);
  intRB := Pos(']', strNwkL);
  if (intLB = 0) and (intRB = 0) then
    Result := strNwkL
  else
    if (intLB = 1) and (intRB = Length(strNwkL)) then
      Result := ''
    else
      if intLB = 1 then
        Result := Copy(strNwkL, intRB + 1, Length(strNwkL) - intRB)
      else
        if intRB = Length(strNwkL) then
          Result := Copy(strNwkL, 1, intLB - 1)
        else
          Result := 'Error';
end;

begin
  strScanned := strNwck;
  Result := '';
  SetLength(astrLengths, 0);
  Setlength(ariLengthPos, 0);
  Setlength(astrBtStrp, 0);
  intCln := Pos(':', strScanned);
  inx := 0;
  if intCln > 0 then
  begin
    while intCln > 0 do
    begin
      strFrontKept := Copy(strScanned, 1, intCln - 1);
      Result := Result + strFrontKept;
      strScanned := Copy(strScanned, intCln + 1, Length(strScanned) - intCln); // - 2);
      intCmm := Pos(',', strScanned);
      intRPn := Pos(')', strScanned);
      SetLength(astrLengths, inx + 1);
      SetLength(astrBtStrp, inx + 1);
      SetLength(ariLengthPos, inx + 1);
      if (intCmm > 0) and (intCmm < intRPn) then
      begin
        ariLengthPos[inx] := Length(Result);
        strLngthBS := Copy(strScanned, 1, intCmm - 1);
        astrBtStrp[inx] := strFindBS(strLngthBS);
        astrLengths[inx] := strFindLngths(strLngthBS);
        strScanned :=  Copy(strScanned, intCmm, Length(strScanned) - intCmm + 1) // - 1)
      end
      else
      begin
        ariLengthPos[inx] := Length(Result);
        strLngthBS := Copy(strScanned, 1, intRPn - 1);
        astrBtStrp[inx] := strFindBS(strLngthBS);
        astrLengths[inx] := strFindLngths(strLngthBS);
        strScanned :=  Copy(strScanned, intRPn, Length(strScanned) - intRPn + 1); // - 1);
      end;
      intCln := Pos(':', strScanned);
      inc(inx);
    end;
    Result := Result + strScanned;
  end
  else
    Result := strScanned;
end;

{
}
procedure ShowTextTreeView(const strNewickFmt: string; const intSlctRt, intSplt: integer);
var
  intVrtBrckHghtY,
  intStrtngPntX,
  intHeightPos,
  intMostLeft,
  intMaxLvl,
//  intLngth,
  intUppr,
  intoy,
  inx,
  iny,
  inz,
  indy,
  intPF,
  intPos: integer;
  GrphPnt: TPoint;
  strFullNewick,
  strNewick: string;
  dblScl: double;
  stlsNewick: TStringList;

function strFindLength(const intNwkPos: integer): string;
var
  inx: integer;
begin
  Result := '';
  for inx := 0 to Length(ariLengthPos) - 1 do
    if ariLengthPos[inx] = intNwkPos then
    begin
      Result := astrLengths[inx];
      exit;
    end;
end;

function strFindBootStrap(const intNwkPos: integer): string;
var
  inx: integer;
begin
  Result := '';
  for inx := 0 to Length(ariLengthPos) - 1 do
    if ariLengthPos[inx] = intNwkPos then
    begin
      Result := astrBtStrp[inx];
      exit;
    end;
end;

function IsStaging: boolean;
var
  inx: integer;
begin
  inx := 0;
  Result := False;
  while not (arTreeToken[inx].boolStaging) do
    if inx < Length(arTreeToken) - 1 then
      inc(inx)
    else
      exit;
  Result := True;
end;

function intParenBalancing(strNwk: string): integer;
var
  inx,
  intPosLP,
  intPosRP,
  intLPs: integer;
begin
  intLPs := 0;
  Result := 0;
  intPosLP := Pos(kLP, strNwk);
  intPosRP := Pos(kRP, strNwk);
  if intPosLP = 0 then
    dec(Result);
  if intPosRP = 0 then
    dec(Result, 2);
  if Result < 0 then
    exit;
  if intPosLP > 0 then
    inc(intLPs);
  if intPosRP > intPosLP then
    inx := intPosLP
  else
    inx := intPosRP;
  while ((intPosRP > 0) or (intPosLP > 0)) and (strNwk <> '') and (strNwk <> ';') do
  begin
    if (intPosLP < intPosRP) and (intPosLP > 0) then
      strNwk := Copy(strNwk, intPosLP + 1, Length(strNwk) - intPosLP)
    else
      strNwk := Copy(strNwk, intPosRP + 1, Length(strNwk) - intPosRP);
    if (strNwk <> '') and (strNwk <> ';') then
    begin
      intPosLP := Pos(kLP, strNwk);
      intPosRP := Pos(kRP, strNwk);
      if (intPosRP > intPosLP) and (intPosLP > 0) then
        inc(intLPs)
      else
        dec(intLPs);
      if (intPosRP > intPosLP) and (intPosLP > 0) then
        inx := intPosLP
      else
        inx := intPosRP;
    end;
  end;
  if strNwk = '' then
    Result := intLPs
  else
    if strNwk = ';' then
      Result := intLPs
    else
      Result := -1;
end;

procedure FindNextElement(const LeadDlmtr: setNwkDlmtr; var NextDlmtr: setNwkDlmtr; var strLeaf: string; const intLNL: integer; var intNNL: integer);
var
  intFindPos: integer;
begin
  case LeadDlmtr of
    LP: if strNewick[intPos] = kLP then
      begin
        intNNL := intLNL + 1;
        NextDlmtr := LP;
        strLeaf := '';
        inc(intPos);
      end
      else
      begin
        intNNL := intLNL + 1;
        NextDlmtr := Lbl;
        intFindPos := Pos(kComma, Copy(strNewick, intPos, Length(strNewick) - intPos + 1));
        strLeaf := strUndoConversion(Copy(strNewick, intPos, intFindPos - 1), kLabelCoversion);
        intPos := intPos + intFindPos - 1;
      end;
    Lbl: if strNewick[intPos] = kComma then
      begin
        intNNL := intLNL;
        NextDlmtr := Comma;
        strLeaf := '';
        inc(intPos);
      end
      else
      begin
        intNNL := intLNL - 1;
        NextDlmtr := RP;
        strLeaf := '';
        inc(intPos);
      end;
    Comma: if strNewick[intPos] = kLP then
      begin
        intNNL := intLNL;
        NextDlmtr := LP;
        strLeaf := '';
        inc(intPos);
      end
      else
      begin
        intNNL := intLNL;
        NextDlmtr := Lbl;
        intFindPos := Pos(kRP, Copy(strNewick, intPos, Length(strNewick) - intPos + 1));
        strLeaf := strUndoConversion(Copy(strNewick, intPos, intFindPos - 1), kLabelCoversion);
        intPos := intPos + intFindPos - 1;
      end;
    RP: if strNewick[intPos] = kRP then
      begin
        intNNL := intLNL - 1;
        NextDlmtr := RP;
        strLeaf := '';
        inc(intPos);
      end
      else
      begin
        intNNL := intLNL;
        NextDlmtr := Comma;
        strLeaf := '';
        inc(intPos);
      end;
  end;
end;

begin
  try
    intPF := intMaxPixelFactor(kBracketTree, dblScl);
    stlsNewick := TStringList.Create;
    with TestTableForSpeciesData, frmPhyIoM do
    try
      intMaxLvl := intHighestLevel;
      memTreeTextView.Lines.Clear;
      strFullNewick := strNewickFmt; //strNwckFrmt;
      strNewick := strStripLengths(strFullNewick);
      AssignNewickTORange(GrphPnt, 0, 0);
      if intParenBalancing(strNewick) <> 0 then
      begin
        MessageDlg('Unbalanced parentheses encountered.', mtInformation, [mbOK], 0);
        exit;
      end;
      if (strNewick[1] = kLP) and (Length(strNewick) > 1) then
      begin
        inx := 0;
        iny := 0;
//        inz := 0;
        SetLength(arTreeToken, inx + 1);
        with arTreeToken[inx] do
        begin
          nwkDlmtr := LP;
          tpToken := Trunk;
          intLevel := 0;
          intFC := -1;
          intLC := -1;
          intLine := -1;
          intTOPos := -1;
          intOffset := -1;
          intPrnt := -1;
          strLeaf := '';
          boolStaging := False;
          boolDrawn := False;
          boolEnded := False;
          boolCovered := False;
        end;
        intPos := 2;
        intoy := 0;
        TierCount := intHighestLevel + 1;
        if boolBootStrap then
          intVrtBrckHghtY := kVrtclBrcktTOBS
        else
          intVrtBrckHghtY := kVrtclBrcktTO;
        intStrtngPntX := kStartBracketX + kHrzntlBrcktTO * TierCount;
        intHeightPos := kStrtBrcktTOY;
        while Length(strNewick) > intPos do
        begin
          inc(inx);
          SetLength(arTreeToken, inx + 1);
          arTreeToken[inx].boolCovered := False;
          FindNextElement(arTreeToken[inx - 1].nwkDlmtr, arTreeToken[inx].nwkDlmtr, arTreeToken[inx].strLeaf, arTreeToken[inx - 1].intLevel, arTreeToken[inx].intLevel);
          if arTreeToken[inx].nwkDlmtr = Lbl then
          begin
            SetLength(arTreePlot, iny + 2);
            SetLength(aTOTreePlot, intoy + 1);
            SetLength(aRectTreePlot, intoy + 1);
            aTOTreePlot[intoy].RSide.x := intStrtngPntX;
            aTOTreePlot[intoy].RSide.y := intHeightPos;
            aTOTreePlot[intoy].LSide.x := intStrtngPntX - kHrzntlBrcktTO;
            aTOTreePlot[intoy].LSide.y := aTOTreePlot[intoy].RSide.y;
            aRectTreePlot[intoy].LSide.y := aTOTreePlot[intoy].RSide.y;
            aTOTreePlot[intoy].TpSide := kNotViewable;
            aTOTreePlot[intoy].BttmSide := kNotViewable;
            aRectTreePlot[intoy].TpSide := kNotViewable;
            aRectTreePlot[intoy].BttmSide := kNotViewable;
//            aRectTreePlot[intoy].intFC := -1;
//            aRectTreePlot[intoy].intLC := -1;
            arTreeToken[inx].intFC := -1;
            arTreeToken[inx].intLC := -1;
            aRectTreePlot[intoy].intPrnt := arTreeToken[arTreeToken[inx].intPrnt].intTOPos;
            aTOTreePlot[intoy].strLabel := arTreeToken[inx].strLeaf;
            aRectTreePlot[intoy].strLabel := arTreeToken[inx].strLeaf;
            arTreePlot[iny].intLevel := 0;
            aTOTreePlot[intoy].intLevel := arTreePlot[iny].intLevel;
            aRectTreePlot[intoy].intBrcktLvl := aTOTreePlot[intoy].intLevel;
//            arTreePlot[iny].RSide.x := intStrtngPntX;
//            arTreePlot[iny].RSide.y := intHeightPos;
//            arTreePlot[iny].LSide.x := intStrtngPntX - kHrzntlBrcktTO;
//            arTreePlot[iny].LSide.y := arTreePlot[iny].RSide.y;
//            arTreePlot[iny].TpSide := kNotViewable;
//            arTreePlot[iny].BttmSide := kNotViewable;
            arTreePlot[iny].intOwner := inx;
            arTreePlot[iny].tpTreePlot := LeafEnd;
            aRectTreePlot[intoy].tpTreePlot := LeafEnd;
            arTreePlot[iny].strLabel := arTreeToken[inx].strLeaf;
            arTreePlot[iny].strPlot := astrTreePlot[arTreePlot[iny].tpTreePlot] + arTreePlot[iny].strLabel;
            arTreePlot[iny + 1].intLevel := 0;
            arTreePlot[iny + 1].intOwner := -1;
            arTreePlot[iny + 1].tpTreePlot := Undrawn;
            arTreePlot[iny + 1].strLabel := '';
            arTreePlot[iny + 1].strPlot := astrTreePlot[arTreePlot[iny + 1].tpTreePlot];
            //arTreePlot[iny + 1].dblBrnchLngth := dblWhiteSpace;
            //arTreePlot[iny + 1].TpSide := kNotViewable;
            //arTreePlot[iny + 1].LSide := kNotViewable;
            //arTreePlot[iny + 1].RSide := kNotViewable;
            //arTreePlot[iny + 1].BttmSide := kNotViewable;
            arTreePlot[iny + 1].intBrnchLngth := -1;
            arTreeToken[inx].intLPos := intPos - 1;
            arTreeToken[inx].strLength := strFindLength(arTreeToken[inx].intLPos);
            arTreePlot[iny].dblBrnchLngth := StrToFloat(arTreeToken[inx].strLength);
            arTreePlot[iny].intBrnchLngth := Trunc(arTreePlot[iny].dblBrnchLngth) * intPF * kShortLengthFactor;
            aRectTreePlot[intoy].dblBrnchLngth := arTreePlot[iny].dblBrnchLngth;
            aRectTreePlot[intoy].intBrnchLngth := Trunc(arTreePlot[iny].dblBrnchLngth * intPF * kShortLengthFactor);
            arTreeToken[inx].strBootStrap := strFindBootStrap(arTreeToken[inx].intLPos);
            arTreeToken[inx].intOffset := 5;
            arTreeToken[inx].boolDrawn := True;
            arTreeToken[inx].boolStaging := True;
            arTreeToken[inx].boolEnded := False;
            arTreeToken[inx].strEdge := astrTreePlot[LeafEnd] + arTreeToken[inx].strLeaf;
            stlsNewick.Add(arTreePlot[iny].strPlot); //arTreeToken[inx].strEdge);
            arTreeToken[inx].intLine := iny;
            arTreeToken[inx].intTOPos:= intoy;
            stlsNewick.Add(arTreePlot[iny + 1].strPlot);  // astrTreePlot[Undrawn]);
            inc(iny, 2);
            inc(intoy);
            arTreeToken[inx].intFC := inx;
            arTreeToken[inx].intLC := inx;
            if arTreeToken[inx - 1].nwkDlmtr = LP then
            begin
              arTreeToken[inx].intPrnt := inx - 1;
              arTreeToken[inx - 1].intFC := inx;
            end;
            if arTreeToken[inx - 1].nwkDlmtr = Comma then
            begin
              arTreeToken[inx].intPrnt := arTreeToken[inx - 1].intPrnt;
              arTreeToken[arTreeToken[inx].intPrnt].intLC := inx;
            end;
            inc(intHeightPos, intVrtBrckHghtY);
          end
          else
          begin
            arTreeToken[inx].strEdge := '';
            arTreeToken[inx].boolStaging := False;
            arTreeToken[inx].intLine := -1;
            case arTreeToken[inx].nwkDlmtr of
              LP: begin
                arTreeToken[inx].boolDrawn := False;
                arTreeToken[inx].boolEnded := False;
                arTreeToken[inx].intLine := -1;
                arTreeToken[inx].intTOPos := -1;
                if arTreeToken[inx - 1].nwkDlmtr = LP then
                begin
                  arTreeToken[inx].intPrnt := inx - 1;
                  arTreeToken[inx - 1].intFC := inx;
                end;
                if arTreeToken[inx - 1].nwkDlmtr = Comma then
                begin
                  arTreeToken[inx].intPrnt := arTreeToken[inx - 1].intPrnt;
                  arTreeToken[arTreeToken[inx].intPrnt].intLC := inx;
                end;
              end;
              Comma: begin
                arTreeToken[inx].boolDrawn := True;
                arTreeToken[inx].boolEnded := True;
                arTreeToken[inx].intLine := -1;
                arTreeToken[inx].intTOPos := -1;
                arTreeToken[inx].intPrnt := arTreeToken[inx - 1].intPrnt;
                arTreeToken[inx].intFC := -1;
                arTreeToken[inx].intLC := -1;
              end;
              RP: begin
                arTreeToken[inx].boolDrawn := True;
                arTreeToken[inx].boolEnded := True;
                arTreeToken[inx].intLine := -1;
                arTreeToken[inx].intTOPos := -1;
                arTreeToken[arTreeToken[inx - 1].intPrnt].intLPos := intPos - 1;
                arTreeToken[arTreeToken[inx - 1].intPrnt].strLength := strFindLength(arTreeToken[arTreeToken[inx - 1].intPrnt].intLPos);
                arTreeToken[arTreeToken[inx - 1].intPrnt].strBootStrap := strFindBootStrap(arTreeToken[arTreeToken[inx - 1].intPrnt].intLPos);
                arTreeToken[inx].intPrnt := arTreeToken[arTreeToken[inx - 1].intPrnt].intPrnt;
                arTreeToken[inx].intFC := arTreeToken[arTreeToken[inx - 1].intPrnt].intFC;
                arTreeToken[inx].intLC := arTreeToken[arTreeToken[inx - 1].intPrnt].intLC;
              end;
            end;
          end;
          if strNewick[intPos - 1] = ';' then
          begin
            MessageDlg('Semi colon encountered before string was resolved.', mtInformation, [mbOK], 0);
            Exit;
          end;
        end; //  end while
        SetLength(arTreePlot, Length(arTreePlot) - 1);
//        exit;
      end;
      inx := Length(arTreeToken);
      SetLength(arTreeToken, inx + 1);
      with arTreeToken[inx] do
      begin
        nwkDlmtr := RP;
        tpToken := Trunk;
        intLevel := arTreeToken[inx - 1].intLevel - 1;
//        aRectTreePlot[intoy].intBrcktLvl := intLevel;
        intFC := arTreeToken[0].intFC;
        intLC := arTreeToken[0].intLC;
        strLeaf := '';
      end;
      inz := 0;
      while IsStaging do
      begin
//        MessageDlg('Next Step:' + IntToStr(inx) + ', Process:' + IntToStr(inz), mtInformation, [mbOK], 0);
        inc(inz);
        inx := 0;
        indy := 0;
        while inx < Length(arTreeToken) do
        begin
          if arTreeToken[inx].nwkDlmtr = LP then
            if not arTreeToken[inx].boolDrawn then
              if arTreeToken[arTreeToken[inx].intFC].boolStaging and arTreeToken[arTreeToken[inx].intLC].boolStaging then
              begin
                inc(indy, 3);
                arTreeToken[inx].intLine := (arTreeToken[arTreeToken[inx].intFC].intLine + arTreeToken[arTreeToken[inx].intLC].intLine) div 2;
                SetLength(aTOTreePlot, intoy + 1);
                SetLength(aRectTreePlot, intoy + 1);
//                intLngth := Length(aTOTreePlot);
                intUppr := High(aTOTreePlot);
                arTreeToken[inx].intTOPos := intoy;
                arTreeToken[arTreeToken[inx].intFC].strEdge :=  astrTreePlot[UpprCrnr] + arTreeToken[arTreeToken[inx].intFC].strEdge;
                arTreePlot[arTreeToken[arTreeToken[inx].intFC].intLine].intLevel := inz;
                arTreePlot[arTreeToken[arTreeToken[inx].intFC].intLine].intOwner := inx;
                arTreePlot[arTreeToken[arTreeToken[inx].intFC].intLine].tpTreePlot := UpprCrnr;
//                arTreePlot[iny].TpSide := arTreePlot[arTreeToken[arTreeToken[inx].intFC].intLine].LSide;
                arTreePlot[arTreeToken[arTreeToken[inx].intFC].intLine].strPlot := astrTreePlot[arTreePlot[arTreeToken[arTreeToken[inx].intFC].intLine].tpTreePlot] + arTreePlot[arTreeToken[arTreeToken[inx].intFC].intLine].strPlot;
                stlsNewick[arTreeToken[arTreeToken[inx].intFC].intLine] := arTreePlot[arTreeToken[arTreeToken[inx].intFC].intLine].strPlot; // arTreeToken[arTreeToken[inx].intFC].strEdge;
                for iny := arTreeToken[arTreeToken[inx].intFC].intLine + 1 to arTreeToken[inx].intLine - 1 do
                begin
                  arTreePlot[iny].intLevel := inz;
                  arTreePlot[iny].intOwner := inx;
                  arTreePlot[iny].tpTreePlot := CvrLine;
                  arTreePlot[iny].strPlot := astrTreePlot[arTreePlot[iny].tpTreePlot] + arTreePlot[iny].strPlot;
                  stlsNewick[iny] := arTreePlot[iny].strPlot;  // astrTreePlot[CvrLine] + memTreeView.stlsNewick[iny];
                end;
                arTreePlot[arTreeToken[inx].intLine].intLevel := inz;
                aTOTreePlot[intoy].intLevel := inz;
                aRectTreePlot[intoy].intBrcktLvl := inz;
                arTreePlot[arTreeToken[inx].intLine].intOwner := inx;
                if inx > 0 then
                  arTreePlot[arTreeToken[inx].intLine].tpTreePlot := LftTee
                else
                  arTreePlot[arTreeToken[inx].intLine].tpTreePlot := RootTee;
                arTreeToken[inx].strEdge := arTreePlot[arTreeToken[inx].intLine].strPlot + arTreeToken[inx].strBootStrap;
                if Length(arTreeToken[inx].strBootStrap) > 0 then
                begin
                  arTreePlot[arTreeToken[inx].intLine].strLabel := arTreeToken[inx].strBootStrap;
                  Delete(arTreePlot[arTreeToken[inx].intLine].strPlot, 1, Length(arTreeToken[inx].strBootStrap));
                  aTOTreePlot[intoy].strLabel := arTreeToken[inx].strBootStrap;
                  aRectTreePlot[intoy].strLabel := arTreeToken[inx].strBootStrap;
                end
                else
                  arTreePlot[arTreeToken[inx].intLine].strLabel := '';
                aRectTreePlot[intoy].dblBrnchLngth := arTreePlot[iny].dblBrnchLngth;
                aRectTreePlot[intoy].intBrnchLngth := Trunc(arTreePlot[iny].dblBrnchLngth * intPF * kShortLengthFactor);
                arTreePlot[arTreeToken[inx].intLine].strPlot := astrTreePlot[arTreePlot[arTreeToken[inx].intLine].tpTreePlot] + arTreeToken[inx].strBootStrap + arTreePlot[arTreeToken[inx].intLine].strPlot;
                if inx > 0 then
                begin
                  arTreePlot[arTreeToken[inx].intLine].dblBrnchLngth := StrToFloat(arTreeToken[inx].strLength);
                  arTreePlot[arTreeToken[inx].intLine].intBrnchLngth := Trunc(arTreePlot[arTreeToken[inx].intLine].dblBrnchLngth * intPF * kShortLengthFactor);
                end
                else
                begin
                  arTreePlot[arTreeToken[inx].intLine].dblBrnchLngth := dblWhiteSpace;
                  arTreePlot[arTreeToken[inx].intLine].intBrnchLngth := -1;
                end;
                stlsNewick[arTreeToken[inx].intLine] := arTreePlot[arTreeToken[inx].intLine].strPlot;  // arTreeToken[inx].strEdge; //strRepeatChar(' ', 4) + Char(kVertLine) + memTreeView.stlsNewick[arTreeToken[inx].intLine] + strRepeatChar(' ', 5);
                for iny := arTreeToken[inx].intLine + 1 to arTreeToken[arTreeToken[inx].intLC].intLine - 1 do
                begin
                  arTreePlot[iny].intLevel := inz;
                  arTreePlot[iny].intOwner := inx;
                  arTreePlot[iny].tpTreePlot := CvrLine;
                  arTreePlot[iny].strPlot := astrTreePlot[arTreePlot[iny].tpTreePlot] + arTreePlot[iny].strPlot;
                  stlsNewick[iny] := arTreePlot[iny].strPlot;  // astrTreePlot[CvrLine] + memTreeView.stlsNewick[iny];
                end;
                arTreeToken[arTreeToken[inx].intLC].strEdge := strRepeat(' ', 4) + Char(kLwrBrnch) + arTreeToken[arTreeToken[inx].intLC].strEdge;
                arTreePlot[arTreeToken[arTreeToken[inx].intLC].intLine].intLevel := inz;
                arTreePlot[arTreeToken[arTreeToken[inx].intLC].intLine].intOwner := inx;
                arTreePlot[arTreeToken[arTreeToken[inx].intLC].intLine].tpTreePlot := LwrCrnr;
                arTreePlot[arTreeToken[arTreeToken[inx].intLC].intLine].strPlot := astrTreePlot[arTreePlot[arTreeToken[arTreeToken[inx].intLC].intLine].tpTreePlot] + arTreePlot[arTreeToken[arTreeToken[inx].intLC].intLine].strPlot;
                stlsNewick[arTreeToken[arTreeToken[inx].intLC].intLine] := arTreePlot[arTreeToken[arTreeToken[inx].intLC].intLine].strPlot;  // arTreeToken[arTreeToken[inx].intLC].strEdge;
                aTOTreePlot[intoy].TpSide := aTOTreePlot[arTreeToken[arTreeToken[inx].intFC].intTOPos].LSide;
                aRectTreePlot[intoy].TpSide.y := aTOTreePlot[intoy].TpSide.y; //aRectTreePlot[arTreeToken[arTreeToken[inx].intFC].intTOPos].LSide.y;
                aTOTreePlot[intoy].BttmSide := aTOTreePlot[arTreeToken[arTreeToken[inx].intLC].intTOPos].LSide;
                aRectTreePlot[intoy].BttmSide.y := aTOTreePlot[intoy].BttmSide.y; //aRectTreePlot[arTreeToken[arTreeToken[inx].intLC].intTOPos].LSide.y;
                aTOTreePlot[intoy].RSide.x := aTOTreePlot[intoy].BttmSide.x;
                aTOTreePlot[intoy].RSide.y := (aTOTreePlot[intoy].TpSide.y + aTOTreePlot[intoy].BttmSide.y) div 2;
                aRectTreePlot[intoy].RSide.y := aTOTreePlot[intoy].RSide.y;  //(aRectTreePlot[intoy].TpSide.y + aRectTreePlot[intoy].BttmSide.y) div 2;
                if inx > 0 then
                  aTOTreePlot[intoy].LSide.x := aTOTreePlot[intoy].RSide.x - kHrzntlBrcktTO
                else
                begin
                  aTOTreePlot[intoy].LSide.x := aTOTreePlot[intoy].RSide.x - kHrzntlBrcktTO div 3;
                  aRectTreePlot[intoy].RSide.x := aTOTreePlot[intoy].RSide.x;
                  aRectTreePlot[intoy].LSide.x := aRectTreePlot[intoy].RSide.x - kHrzntlBrcktTO div 3;
                  aRectTreePlot[intoy].TpSide.x := aRectTreePlot[intoy].RSide.x;
                  aRectTreePlot[intoy].BttmSide.x := aRectTreePlot[intoy].RSide.x;
                end;
                aTOTreePlot[intoy].LSide.y := aTOTreePlot[intoy].RSide.y;
                aRectTreePlot[intoy].RSide.y := aTOTreePlot[intoy].LSide.y;
                aRectTreePlot[intoy].LSide.y := aRectTreePlot[intoy].RSide.y;
                aTOTreePlot[intoy].intLevel := inz;
                aRectTreePlot[intoy].intBrcktLvl := inz;
                aRectTreePlot[intoy].intFC := arTreeToken[arTreeToken[inx].intFC].intTOPos;
                aRectTreePlot[intoy].intLC := arTreeToken[arTreeToken[inx].intLC].intTOPos;
                if arTreeToken[inx].intPrnt > -1 then
                  aRectTreePlot[intoy].intPrnt := arTreeToken[arTreeToken[inx].intPrnt].intTOPos
                else
                  aRectTreePlot[intoy].intPrnt := arTreeToken[inx].intPrnt;
                aTOTreePlot[arTreeToken[arTreeToken[inx].intFC].intTOPos].intLevel := TestTableForSpeciesData.SpeciesCount - 1;
                aTOTreePlot[arTreeToken[arTreeToken[inx].intLC].intTOPos].intLevel := TestTableForSpeciesData.SpeciesCount - 1;
                arTreeToken[arTreeToken[inx].intFC].boolStaging := False;
                arTreeToken[arTreeToken[inx].intLC].boolStaging := False;
                arTreeToken[arTreeToken[inx].intFC].boolEnded := True;
                arTreeToken[arTreeToken[inx].intLC].boolEnded := True;
                arTreeToken[inx].boolDrawn := True;
                inc(intoy);
                if inx > 0 then
                  arTreeToken[inx].boolStaging := True;
              end
              else
            else
          else
          begin
          end;
          inc(inx);
        end;
//        MessageDlg('Next Draw:' + IntToStr(inx) + ', Process:' + IntToStr(inz), mtInformation, [mbOK], 0);
        for inx := 0 to Length(arTreePlot) - 1 do // arTreeToken) - 1 do
        begin
          if arTreePlot[inx].intLevel < inz then
          begin
            arTreePlot[inx].intLevel := inz;
            if arTreePlot[inx].tpTreePlot in [LwrCrnr, UpprCrnr, CvrLine, Spaces, Undrawn] then
              arTreePlot[inx].tpTreePlot := Spaces
            else
            begin
              arTreePlot[inx].tpTreePlot := ExtLine;
            end;
            arTreePlot[inx].strPlot := astrTreePlot[arTreePlot[inx].tpTreePlot] + arTreePlot[inx].strPlot;
            stlsNewick[inx] := arTreePlot[inx].strPlot;
          end;
        end;
        for inx := 0 to intUppr do // arTreeToken) - 1 do
          if aTOTreePlot[inx].intLevel < inz then
          begin
            aTOTreePlot[inx].intLevel := inz;
//            aRectTreePlot[intoy].intLevel := inz;
            aTOTreePlot[inx].LSide.x := aTOTreePlot[inx].LSide.x - kHrzntlBrcktTO;
          end;
      end;
      for inx := 0 to stlsNewick.Count - 1 do
        memTreeTextView.Lines.Insert(inx, stlsNewick[inx]);
      intMostLeft := 0;
      for inx := Length(aTOTreePlot) - 3 to Length(aTOTreePlot) - 1 do
        if aTOTreePlot[inx].LSide.x < intMostLeft then
          intMostLeft := aTOTreePlot[inx].LSide.x;
      intMostLeft := -intMostLeft + 25;
      if intMostLeft > 0 then
        for inx := 0 to Length(aTOTreePlot) - 1 do
        begin
          aTOTreePlot[inx].LSide.x := aTOTreePlot[inx].LSide.x + intMostLeft;
          aTOTreePlot[inx].RSide.x := aTOTreePlot[inx].RSide.x + intMostLeft;
          if aTOTreePlot[inx].TpSide.x <> -1 then
            aTOTreePlot[inx].TpSide.x := aTOTreePlot[inx].TpSide.x + intMostLeft;
          if aTOTreePlot[inx].BttmSide.x <> -1 then
            aTOTreePlot[inx].BttmSide.x := aTOTreePlot[inx].BttmSide.x + intMostLeft;
        end;
      inx := Length(aTOTreePlot) - 1;
      aRectTreePlot[inx].RSide := aTOTreePlot[inx].RSide;
      aRectTreePlot[inx].LSide := aTOTreePlot[inx].LSide;
      aRectTreePlot[inx].TpSide := aTOTreePlot[inx].TpSide;
      aRectTreePlot[inx].BttmSide := aTOTreePlot[inx].BttmSide;
      AssignNewickTORange(GrphPnt, aTOTreePlot[0].RSide.x + intMaxTextLength + 50, Origin.ttoTplgyOnly.Height);
      if frmTreeGraph.boolComplete then
        if intSlctRt > -1 then
          CreateRectangularRerootedTree(intSlctRt, intSplt, intPF);
    finally
      stlsNewick.Free;
    end;
//    finally
//    end;
  except
    on E:exception do
     MessageDlg(E.Message + ' in ShowTextTreeView', mtError, [mbOK], 0);       // Show error message
  end;
end;

{
Function returns the farthest right x-axis rerooted racket label position of tree using the
aRectTreePlot record array.

Parameters:
  Result -> an integer value of the farthest right label on the graph
}
function intMaxRRBrcktPosX: integer;
var
  inx: integer;
begin
  Result := 0;
  for inx := 0 to Length(aRectTreePlot) - 1 do
    if aRectTreePlot[inx].RSide.x > Result then
      Result := aRectTreePlot[inx].RSide.x;
end;

procedure CreateRectangularRerootedTree(const intRt, intSplt, intPF: integer);
var
  GrphPnt: TPoint;
  dblLngth: double;
  intLngth,
  iny,
  inx: integer;
  strWork: string;

function dblFindLength(const intTOP: integer): double;
var
  inx: integer;
begin
  Result := -1.0;
  for inx := 0 to Length(arTreeToken) - 1 do
    with arTreeToken[inx] do
      if intTOP = intTOPos then
      begin
        Result := StrToFloat(strLength);
        Exit;
      end;
end;

begin
 AssignNwckBrcktRange(GrphPnt, 0, 0);
 if Length(aRectTreePlot) = 0 then
   exit;
 try
  for inx := 0 to Length(aRectTreePlot) - 1 do
  begin
    if (inx < Length(astrLengths)) and (inx >= TestTableForSpeciesData.SpeciesCount) and  (inx < TestTableForSpeciesData.NodeCount) then
//      if aRectTreePlot[inx].dblBrnchLngth <= dblZero then
      begin
        aRectTreePlot[inx].dblBrnchLngth := dblFindLength(inx);
        aRectTreePlot[inx].intBrnchLngth := Trunc(aRectTreePlot[inx].dblBrnchLngth * intPF * kShortLengthFactor);
      end;
    if aRectTreePlot[inx].LSide.y = 0 then
      aRectTreePlot[inx].LSide.y := aTOTreePlot[inx].LSide.y;
    if aRectTreePlot[inx].RSide.y = 0 then
      aRectTreePlot[inx].RSide.y := aTOTreePlot[inx].RSide.y;
    if aRectTreePlot[inx].BttmSide.y = 0 then
      aRectTreePlot[inx].BttmSide.y := aTOTreePlot[inx].BttmSide.y;
    if aRectTreePlot[inx].TpSide.y = 0 then
      aRectTreePlot[inx].TpSide.y := aTOTreePlot[inx].TpSide.y;
  end;
  for inx := Length(arTreeToken) - 1 downto 0 do
  begin
    if arTreeToken[inx].intTOPos > -1 then
    begin
      if arTreeToken[inx].intPrnt > -1 then
        aRectTreePlot[arTreeToken[inx].intTOPos].intPrnt := arTreeToken[arTreeToken[inx].intPrnt].intTOPos
      else
        aRectTreePlot[arTreeToken[inx].intTOPos].intPrnt := -1;
{      if arTreeToken[inx].intFC = arTreeToken[inx].intLC then
      begin
        aRectTreePlot[arTreeToken[inx].intTOPos].intFC := -1;
        aRectTreePlot[arTreeToken[inx].intTOPos].intLC := -1;
      end
      else
      begin}
      aRectTreePlot[arTreeToken[inx].intTOPos].intFC := arTreeToken[arTreeToken[inx].intFC].intTOPos;
//      if arTreeToken[inx].intLC = 0 then
//      else
      aRectTreePlot[arTreeToken[inx].intTOPos].intLC := arTreeToken[arTreeToken[inx].intLC].intTOPos;
//      end;
    end;
    iny := inx;
  end;
  inx := Length(aRectTreePlot) - 1;
  iny := aRectTreePlot[inx].intBrcktLvl;
  aRectTreePlot[inx].intPrnt := -1;
  if intRt = TestTableForSpeciesData.NodeCount - 1 then
  begin
    dblLngth := (TestTableForSpeciesData.Species[intRt].BranchLength + TestTableForSpeciesData.Species[intRt + 1].BranchLength) * TestTableForSpeciesData.intSplit / 100.0;
    intLngth := Trunc((TestTableForSpeciesData.Species[intRt].BranchLength + TestTableForSpeciesData.Species[intRt + 1].BranchLength) * intPF * kShortLengthFactor); //aRectTreePlot[aRectTreePlot[inx].intLC].intBrnchLngth * 175 div 100;
  end
  else
  begin
    dblLngth := TestTableForSpeciesData.Species[intRt].BranchLength * TestTableForSpeciesData.intSplit / 100.0;
    intLngth := Trunc(TestTableForSpeciesData.Species[intRt].BranchLength * intPF * kShortLengthFactor); //aRectTreePlot[aRectTreePlot[inx].intLC].intBrnchLngth * 175 div 100;
  end;
  aRectTreePlot[aRectTreePlot[inx].intLC].intBrnchLngth := intLngth * (100 - TestTableForSpeciesData.intSplit) div 100;
  aRectTreePlot[aRectTreePlot[inx].intLC].dblBrnchLngth := TestTableForSpeciesData.Species[intRt].BranchLength - dblLngth;
  aRectTreePlot[aRectTreePlot[inx].intFC].intBrnchLngth := intLngth * TestTableForSpeciesData.intSplit div 100;
  aRectTreePlot[aRectTreePlot[inx].intFC].dblBrnchLngth := dblLngth;
  aRectTreePlot[aRectTreePlot[inx].intLC].LSide.x := aRectTreePlot[inx].BttmSide.x;
  aRectTreePlot[aRectTreePlot[inx].intFC].LSide.x := aRectTreePlot[inx].TpSide.x;
  aRectTreePlot[aRectTreePlot[inx].intLC].RSide.x := aRectTreePlot[inx].BttmSide.x + aRectTreePlot[aRectTreePlot[inx].intLC].intBrnchLngth;
  aRectTreePlot[aRectTreePlot[inx].intFC].RSide.x := aRectTreePlot[inx].TpSide.x + aRectTreePlot[aRectTreePlot[inx].intFC].intBrnchLngth;
  aRectTreePlot[0].RSide.x := 0;
//  aRectTreePlot[aRectTreePlot[inx].intLC].RSide.y := aRectTreePlot[aRectTreePlot[inx].intLC].LSide.y;
  while iny > -1 do
  begin
    while inx > -1 do //< Length(aRectTreePlot) - 1 do
    begin
      if aRectTreePlot[inx].intBrcktLvl = iny then
      begin
        if aRectTreePlot[inx].LSide.x > 0 then
        begin
          if aRectTreePlot[inx].RSide.x = 0 then
//          begin
            aRectTreePlot[inx].RSide.x := aRectTreePlot[inx].LSide.x + aRectTreePlot[inx].intBrnchLngth;
//            aRectTreePlot[inx].TpSide.x := aRectTreePlot[inx].RSide.x;
//            aRectTreePlot[inx].BttmSide.x := aRectTreePlot[inx].RSide.x;
//          end
//          else
//          begin
            if aRectTreePlot[inx].TpSide.x <> -1 then
              if aRectTreePlot[inx].TpSide.x = 0 then
                aRectTreePlot[inx].TpSide.x := aRectTreePlot[inx].RSide.x;
            if aRectTreePlot[inx].BttmSide.x <> -1 then
              if aRectTreePlot[inx].BttmSide.x = 0 then
                aRectTreePlot[inx].BttmSide.x := aRectTreePlot[inx].RSide.x;
//          end;
          if aRectTreePlot[aRectTreePlot[inx].intFC].LSide.x <> -1 then
            if aRectTreePlot[inx].intFC <> inx then
            begin
              aRectTreePlot[aRectTreePlot[inx].intFC].LSide.x := aRectTreePlot[inx].TpSide.x;
              aRectTreePlot[aRectTreePlot[inx].intFC].LSide.y := aRectTreePlot[inx].TpSide.y;
            end;
          if aRectTreePlot[aRectTreePlot[inx].intLC].LSide.x <> -1 then
            if aRectTreePlot[inx].intLC <> inx then
            begin
              aRectTreePlot[aRectTreePlot[inx].intLC].LSide.x := aRectTreePlot[inx].BttmSide.x;
              aRectTreePlot[aRectTreePlot[inx].intLC].LSide.y := aRectTreePlot[inx].BttmSide.y;
            end;
        end;
      end;
      dec(inx);
    end;
    inx := Length(aRectTreePlot) - 1;
    dec(iny);
  end;
  stlsRecTree.Clear;
  strWork := '---------aRectTreePlot String List---------------';
  stlsRecTree.AddText(strWork);
  for inx := 0 to Length(aRectTreePlot) - 1 do
  begin
    strWork := 'Inx:' + IntToStr(inx) + ',Prnt:' + IntToStr(aRectTreePlot[inx].intPrnt) + ',FC:' + IntToStr(aRectTreePlot[inx].intFC) + ',LC:' + IntToStr(aRectTreePlot[inx].intLC) + ',Lvl:' + IntToStr(aRectTreePlot[inx].intBrcktLvl) + ',L:[' + aRectTreePlot[inx].strLabel + '],LS.x:' + IntToStr(aRectTreePlot[inx].LSide.x)+ ',RS.x:' + IntToStr(aRectTreePlot[inx].RSide.x) +
               ',Lngth:' + IntToStr(aRectTreePlot[inx].intBrnchLngth) + ',FPL:' + FloatToStr(aRectTreePlot[inx].dblBrnchLngth);
    stlsRecTree.AddText(strWork);
  end;
  strWork := '----------arTreeToken String List----------------';
  stlsRecTree.AddText(strWork);
  for inx := 0 to Length(arTreeToken) - 1 do
    with arTreeToken[inx] do
    begin
      strWork := 'Inx:' + IntToStr(inx) + ',FPL:' + strLength + ',L:[' + strLeaf + '],Edge:[' + strEdge + '],Line:' + IntToStr(intLine) + ',BS:[' + strBootStrap + '],LTOPos:' + IntToStr(intTOPos) + ',intLPos:' + IntToStr(intLPos);
      stlsRecTree.AddText(strWork);
    end;
  strWork := '--------String length List--------------';
  stlsRecTree.AddText(strWork);
  strWork := '';
  for inx := 0 to Length(astrLengths) - 1 do
  begin
    strWork := strWork + 'Inx:' + IntToStr(inx) + ',Length:' + astrLengths[inx];
    if inx < Length(astrLengths) - 1 then
      strWork := strWork + ',';
  end;
  stlsRecTree.AddText(strWork);
  AssignNwckBrcktRange(GrphPnt, intMaxRRBrcktPosX + intMaxTextLength + 50, Origin.ttoTplgyOnly.Height);
 except
   on E:exception do
     MessageDlg(E.Message + ' in CreateRectangularRerootedTree', mtError, [mbOK], 0);       // Show error message
 end;
end;

function intAssignParentNodes(const intNL: integer): integer;
var
  inz,
  iny,
  inx: integer;
begin
  inz := 0;
  Result := -1;
  for inx := 0 to 2 do
    ariNodeParent[inx] := -1;
  with TestTableForSpeciesData do
    for inx := NodeCount - 1 downto 0 do
      if intNL <> -1 then
        if intNL = Species[inx].NodeLevel then
          for iny := 0 to inz do
            if Species[inx].NodeParent = ariNodeParent[iny] then
              break
            else
              if (ariNodeParent[iny] = -1) and ((Species[inx].NodeParent > SpeciesCount - 1) or ((inx = nodeCount - 1) and (Species[inx].NodeParent = 0))) then
              begin
                ariNodeParent[iny] := Species[inx].NodeParent;
                Result := inz;
                inc(inz);
                break;
              end;
end;

procedure MakeNewickNode(const intNL, intOffset: integer; var strNewick: string);
var
  intSgmntNd,
  intPos,
  inz,
  iny,
  inx: integer;
  strNode: string;
begin
  strNewick := '';
  intPos := 0;
  intSgmntNd := intAssignParentNodes(intNL);
  iny := 0;
  with TestTableForSpeciesData do
    for inx := NodeCount - 1 downto 0 do
      if Species[inx].NodeLevel = intNL then
      begin
        for inz := 0 to intSgmntNd do
        begin
          if iny + 1 = inz then
          begin
            inc(iny);
            SetLength(iarInsrtnPnt[intNL], iny + 1);
          end;
          if (ariNodeParent[inz] = Species[inx].NodeParent) or (Species[inx].NodeLevel = 0) then
            if inx > (SpeciesCount - 1) then
            begin
              intPos := Pos('()', Copy(strNewick, intPos + 1, Length(strNewick) - intPos)) + 2;
              iarInsrtnPnt[intNL][inz] := intOffset + intPos + Length(strNewick);
              strNode := '():' + FloatToStrF(Species[inx].BranchLength, ffFixed, 10, 4) + ',';
              strNewick := strNewick + strNode;
            end
            else
              strNewick := strNewick + Species[inx].SpeciesName + ':' + FloatToStrF(Species[inx].BranchLength,  ffFixed, 10, 4) + ',';
          end;
        end;
  if strNewick <> '' then
    Delete(strNewick, Length(strNewick), 1);
  if (strNewick <> '') and (iny = 0) then
  begin
    SetLength(iarInsrtnPnt[intNL], iny + 1);
    iarInsrtnPnt[intNL][iny] := intOffset + 2;
  end;
end;

{
Procedure calculates insertions points in Newick string by finding "()".  Saving positions
of parantheses for Newick tokens to be later inserted into the Newick string using the
number of insertion nodes to iterate through.  The insertion position is fron the
beginning of the string.

Parameters:
  strNwck -> Newick string being examined to find insertions position.
}
procedure FindNewickPstn(const strNwck: string);
var
  intPos,
  iny,
  inx: integer;
begin
  SetLength(ariInsrtnPnt, 0);
  intPos := 0;
  iny := 0;
  for inx := 0 to High(ariInsrtnNd) do
  begin
    intPos := Pos('()', Copy(strNwck, intPos + 1, Length(strNwck) - intPos)) + intPos + 1;
    if intPos > 0 then
    begin
      if iny > 0 then
        inc(iny);
      SetLength(ariInsrtnPnt, iny + 1);
      ariInsrtnPnt[iny] := intPos;
    end;
  end;
end;

{
Procedure to insert Newick tokens into the mode interioir nodes forming larger Newick
tokens.  Build string from right to left inserting Newick tokens which are leaves or
other interior nodes making up a token.
}
procedure InsertNewickNodes(var strNwck: string);
var
  strWork: string;
  inz,
  iny,
  inx: integer;
begin
  SetLength(ariNextNd, 0);
  inz := 0;
  with TestTableForSpeciesData do
  begin
    for inx := High(ariInsrtnNd) downto 0 do
    begin
      strWork := '';
      for iny := NodeCount - 1 downto 0 do
        if ariInsrtnNd[inx] = Species[iny].NodeParent then
        begin
          strWork := strWork + Species[iny].NewickToken + ',';
          if iny > SpeciesCount - 1 then
          begin
            If inz > 0 then
              inc(inz);
            SetLength(ariNextNd, inz + 1);
            ariNextNd[inz] := iny;
          end;
        end;
      if Length(strWork) > 0 then
        if strWork[Length(strWork)] = ',' then
          Delete(strWork, Length(strWork), 1);
      strNwck := Copy(strNwck, 1, ariInsrtnPnt[inx] - 1) + strWork + Copy(strNwck, ariInsrtnPnt[inx], Length(strNwck) - ariInsrtnPnt[inx] + 1);
    end;
  end;
  ariInsrtnNd := ariNextNd;
end;

{
Procedure concatenates the last two string in Species Data Table in NewickToken field
which includes concatenating the two children of each as well.  This creates the value
that goes in the Newick Format memo field.

Parameters:
  mmoNwckFrmt -> Memo field holding the Newick Format result
}
procedure MakeNewickFormat(var mmoNwckFrmt: TMemo);
begin
  mmoNwckFrmt.Clear;
  with TestTableForSpeciesData do
  begin
    strNwckFrmt := ',(' + Species[Species[NodeCount - 1].EndSegment.int1stXNd].NewickToken + ',' +
                 Species[Species[NodeCount - 1].EndSegment.int2ndXNd].NewickToken +
                 Species[NodeCount - 1].NewickToken + ');';
    strNwckFrmt := '((' + Species[Species[NodeCount].EndSegment.int1stXNd].NewickToken + ',' +
                 Species[Species[NodeCount].EndSegment.int2ndXNd].NewickToken +
                 Species[NodeCount].NewickToken + strNwckFrmt;
  end;
  mmoNwckFrmt.Lines[0] := strNwckFrmt;
end;

{
Procedure to assign replicates array and replicates to string grid. A test chosen
is replaced the current.  It does this for all test positions.

Parameters:
  intBootStraps -> Number of bootstrap relicates to be created
  strgrdRplcts -> string grid of replicate tests by bootstrap replicates
}
procedure AssignReplicates(const intBootStraps: integer; var strgrdRplcts: TStringGrid);
var
  intSeed,
  intRplctLngth,
  intCellCol,
  inx,
  iny: integer;
begin
  intRplctLngth := TestTableForSpeciesData.TestCount;
  SetLength(iarReplicates, intBootStraps, intRplctLngth);
  intSeed := intRplctLngth - 1;
  intCellCol := intRplctLngth + 1;
  //RandSeed := 1039; // 115;
  Randomize;
  for inx := 0 to intBootStraps - 1 do
  begin
    for iny := 0 to intSeed do
    begin
      iarReplicates[inx, iny] := Random(intSeed);
    end;
  end;
  with strgrdRplcts, TestTableForSpeciesData do
  begin
    RowCount := intBootStraps + 1;
    ColCount := intCellCol;
    Cells[0, 0] := 'Replicates';
    for inx := 1 to intRplctLngth do
      Cells[inx, 0] := 'Rplct Test:' + IntToStr(inx - 1);
    for iny := 1 to intBootStraps do
      Cells[0, iny] := IntToStr(iny - 1);
    for iny := 1 to intBootStraps do
      for inx := 1 to intRplctLngth do
        Cells[inx, iny] := IntToStr(iarReplicates[iny - 1, inx - 1]);
    AutoSizeColumns;
  end;
end;

{
Function to split length of rooted branch.  Will return zero if factor parameter is
negative.

Parameters:
  dblFctrd -> value of double precision to be factored or split in for a root branch
  Result -> value of double precision to be factored returned value divided by 1.75
}
function dblSplitLength(const dblFctrd: double): double;
const
  kSpltFctr = 1.75;
begin
  if dblFctrd < dblZero then
    Result := dblZero
  else
    Result := dblFctrd / kSpltFctr;
end;

{
Procedure list all interior nodes other than leaves.  Lists parent name with parent
length, 1st child name and length, 2nd child name and length, and adds bootstrap
if it is calculated to end of tne record containing these items on each node.

Paramters:
  strFileName -> File name to be created as a csv file
}
procedure MakeInteriorNodeFile(const strFileName: string);
var
  inx: integer;
  strWork: string;
  flNodeText: Text;
  boolBSCalculated: boolean;
begin
  with TestTableForSpeciesData do
  begin
    if Length(frmPhyIoM.strgrdDataTableProp.Cells[SpeciesCount + 1, 32]) > 0 then
      boolBSCalculated := True
    else
      boolBSCalculated := False;
    Assign(flNodeText, strFileName);
    Rewrite(flNodeText);
    strWork := '"Parent Name","Parent Length","1st Child Name","1st Child Length","2nd Child Name","2nd Child Length"';
    if boolBSCalculated then
      strWork := strWork + ',"Boot Strap"';
    WriteLn(flNodeText, strWork);
    for inx := SpeciesCount to NodeCount do
    begin
      strWork := '"' + Species[inx].SpeciesName + '",';
      Write(flNodeText, strWork);
{      if inx = NodeCount - 1 then
      begin
        dblFactor := dblSplitLength(Species[inx].BranchLength);
        strWork := FloatToStrF(dblFactor, ffFixed, 10, 4) + ',';
      end
      else}
        strWork := FloatToStrF(Species[inx].BranchLength, ffFixed, 10, 4) + ',';
      Write(flNodeText, strWork);
      strWork := '"' + Species[Species[inx].EndSegment.int1stXNd].SpeciesName + '",';
      Write(flNodeText, strWork);
      strWork := FloatToStrF(Species[Species[inx].EndSegment.int1stXNd].BranchLength, ffFixed, 10, 4) + ',';
      Write(flNodeText, strWork);
      strWork := '"' + Species[Species[inx].EndSegment.int2ndXNd].SpeciesName + '",';
      Write(flNodeText, strWork);
      strWork := FloatToStrF(Species[Species[inx].EndSegment.int2ndXNd].BranchLength, ffFixed, 10, 4);
      Write(flNodeText, strWork);
      if boolBSCalculated then
      begin
        strWork := ',"' + frmPhyIoM.strgrdDataTableProp.Cells[inx + 1, 32] + '"';
        Write(flNodeText, strWork);
      end;
      WriteLn(flNodeText);
    end;
  end;
  Close(flNodeText);
end;

{
Displays replicate raw Sequence data for a page of the replicate set.

Parameters:
  intIndex -> replicate set to use
  strgrdRRD -> string grid to display raw replicate dataset with replicates being the random column selections.
}
procedure DisplayBootStrapPageSqnc(const intIndex: integer);
var
  iny,
  inx: integer;
begin
  with frmPhyIoM.strgrdRplctRwDt, TestTableForSpeciesData do
  begin
    RowCount := SpeciesCount + 1;
    ColCount := TestCount + 1;
    Cells[0, 0] := 'Replicate:' + IntToStr(intIndex);
    for iny := 1 to TestCount do
      Cells[iny, 0] := TestTitle[iarReplicates[intIndex][iny - 1]];
    for inx := 1 to SpeciesCount do
    begin
      Cells[0, inx] := Species[inx - 1].SpeciesName;
      for iny := 1 to TestCount do
        Cells[iny, inx] := carSequence[intIndex][inx - 1][iny - 1];
    end;
    AutoSizeColumns;
  end;
end;

{
Displays replicate raw Bimary data for a page of the replicate set.

Parameters:
  intIndex -> replicate set to use
  strgrdRRD -> string grid to display raw replicate dataset with replicates being the random column selections.
}
procedure DisplayBootStrapPageBnry(const intIndex: integer);
var
  iny,
  inx: integer;
begin
  with frmPhyIoM.strgrdRplctRwDt, TestTableForSpeciesData do
  begin
    RowCount := SpeciesCount + 1;
    ColCount := TestCount + 1;
    Cells[0, 0] := 'Replicate:' + IntToStr(intIndex);
    for iny := 1 to TestCount do
      Cells[iny, 0] := TestTitle[iarReplicates[intIndex][iny - 1]];
    for inx := 1 to SpeciesCount do
    begin
      Cells[0, inx] := Species[inx - 1].SpeciesName;
      for iny := 1 to TestCount do
        Cells[iny, inx] := carBinary[intIndex][inx - 1][iny - 1];
    end;
    AutoSizeColumns;
  end;
end;

{
Displays replicate raw Measurment data for a page of the replicate set.

Parameters:
  intIndex -> replicate set to use
  strgrdRRD -> string grid to display raw replicate dataset with replicates being the random column selections.
}
procedure DisplayBootStrapPageMsrmnt(const intIndex: integer);
var
  iny,
  inx: integer;
begin
  with frmPhyIoM.strgrdRplctRwDt, TestTableForSpeciesData do
  begin
    RowCount := SpeciesCount + 1;
    ColCount := TestCount + 1;
    Cells[0, 0] := 'Replicate:' + IntToStr(intIndex);
    for iny := 1 to TestCount do
      Cells[iny, 0] := TestTitle[iarReplicates[intIndex][iny - 1]];
    for inx := 1 to SpeciesCount do
    begin
      Cells[0, inx] := Species[inx - 1].SpeciesName;
      for iny := 1 to TestCount do
        if darBtStrpRwDtMsrmnt[intIndex][inx - 1][iny - 1] <> kInvalidData then
          Cells[iny, inx] := FloatToStrF(darBtStrpRwDtMsrmnt[intIndex][inx - 1][iny - 1], ffFixed, 10, 3)
        else
          Cells[iny, inx] := kMissingData;
    end;
    AutoSizeColumns;
  end;
end;

procedure CheckForReplicates(const inRplcts: integer; var cbxItems, cbxFDMDsply: TComboBox);
var
  inz,
  iny,
  inx: integer;
begin
  with TestTableForSpeciesData do
  begin
    SetLength(DuplicateNodes, NodeCount - SpeciesCount);
    case wrdDataType of
      0..1: begin
        SetLength(carBinary, inRplcts);
        for inx := 0 to inRplcts - 1 do
        begin
          cbxItems.AddItem(IntToStr(inx), nil);
          cbxFDMDsply.AddItem(IntToStr(inx), nil);
          SetLength(carBinary[inx], SpeciesCount);
          for iny := 0 to SpeciesCount - 1 do
          begin
            SetLength(carBinary[inx][iny], TestCount);
            for inz := 0 to TestCount - 1 do
              carBinary[inx][iny][inz] := Binary[iny][iarReplicates[inx][inz]];
          end;
        end;
        cbxItems.Enabled := True;
        cbxItems.ItemIndex := 0;
        DisplayBootStrapPageBnry(cbxItems.ItemIndex);
      end;
      2: begin
        SetLength(darBtStrpRwDtMsrmnt, inRplcts);
        for inx := 0 to inRplcts - 1 do
        begin
          cbxItems.AddItem(IntToStr(inx), nil);
          cbxFDMDsply.AddItem(IntToStr(inx), nil);
          SetLength(darBtStrpRwDtMsrmnt[inx], SpeciesCount);
          for iny := 0 to SpeciesCount - 1 do
          begin
            SetLength(darBtStrpRwDtMsrmnt[inx][iny], TestCount);
            for inz := 0 to TestCount - 1 do
              darBtStrpRwDtMsrmnt[inx][iny][inz] := Measurement[iny][iarReplicates[inx][inz]];
          end;
        end;
        cbxItems.Enabled := True;
        cbxItems.ItemIndex := 0;
        DisplayBootStrapPageMsrmnt(cbxItems.ItemIndex);
      end;
      3..4: begin
        SetLength(carSequence, inRplcts);
        for inx := 0 to inRplcts - 1 do
        begin
          cbxItems.AddItem(IntToStr(inx), nil);
          cbxFDMDsply.AddItem(IntToStr(inx), nil);
          SetLength(carSequence[inx], SpeciesCount);
          for iny := 0 to SpeciesCount - 1 do
          begin
            SetLength(carSequence[inx][iny], TestCount);
            for inz := 0 to TestCount - 1 do
              carSequence[inx][iny][inz] := Sequence[iny][iarReplicates[inx][inz]];
          end;
        end;
        cbxItems.Enabled := True;
        cbxItems.ItemIndex := 0;
        DisplayBootStrapPageSqnc(cbxItems.ItemIndex);
      end;
    end;
  end;
end;

function boolMoreNodeLevels(const intLevel: integer): boolean;
var
  inx: integer;
begin
  Result := False;
  with TestTableForSpeciesData do
    for inx := 0 to NodeCount - 2 do
      if Species[inx].NodeLevel = intLevel then
      begin
        Result := True;
        break;
      end;
end;

function intMaxTextLength: integer;
var
  intFontSize,
  intLength,
  intMaxLength,
  intMaxInx,
  inx: integer;
begin
  intMaxLength := 0;
  with TestTableForSpeciesData do
  begin
    for inx := 0 to SpeciesCount - 1 do
    begin
      intLength := Length(Species[inx].SpeciesName);
      if intLength > intMaxLength then
      begin
        intMaxLength := intLength;
        intMaxInx := inx;
      end;
    end;
    with frmTreeGraph.Canvas do
    begin
      intFontSize := Font.Size;
      Font.Size := 25;
      Result := TextWidth(Species[inx].SpeciesName);
      Font.Size := intFontSize;
    end;
  end;
end;

function intMaxBracketPosX: integer;
var
  inx: integer;
begin
  Result := 0;
  with TestTableForSpeciesData do
  begin
    for inx := 0 to SpeciesCount - 1 do
      With Species[inx].VrtclPosition do
        if MidPoint.x > Result then
          Result := MidPoint.x;
  end;
end;

function intMaxBracketPosY: integer;
var
  inx: integer;
begin
  Result := 0;
  with TestTableForSpeciesData do
  begin
    for inx := 0 to SpeciesCount - 1 do
      With Species[inx].VrtclPosition do
        if MidPoint.y > Result then
          Result := MidPoint.y;
  end;
  Result := Result + 20;
end;

procedure CreateBracketTree(var HomeVrtclPosition: TVrtclPosition; var intScaleTree: integer; var dblScaleValue: double);
var
  GrphPnt: TPoint;
  intLeaves,
  intHmVrtclSpc,
  inx: integer;
begin
  with TestTableForSpeciesData do
  begin
    AssignBracketRange(GrphPnt, 0, 0);
    GrphPnt.x := kStartBracketX;
    intLeaves := Species[NodeCount - 1].Leaves + Species[NodeCount].Leaves;
    intHmVrtclSpc := intLeaves * intLeaves * kVerticalBracket div 4 + 300;
    intScaleTree := 5 * intPixelFactor * kShortLengthFactor;
    GrphPnt.y := intHmVrtclSpc div 2;
    HomeVrtclPosition.BgnVrtcl.y := Species[NodeCount].TplgyOnly.BgnHrzntl.y;  //intHmVrtclSpc div 3;
    HomeVrtclPosition.BgnVrtcl.x := GrphPnt.x;
    HomeVrtclPosition.TopHrzntl.y := HomeVrtclPosition.BgnVrtcl.y;
    HomeVrtclPosition.TopHrzntl.x := HomeVrtclPosition.BgnVrtcl.x + Trunc(Species[NodeCount].BranchLength * intPixelFactor * kShortLengthFactor);
    HomeVrtclPosition.MidPoint.y := (Species[NodeCount - 1].TplgyOnly.BgnHrzntl.y + Species[NodeCount].TplgyOnly.BgnHrzntl.y) div 2;   //GrphPnt.y;
    HomeVrtclPosition.MidPoint.x := GrphPnt.x;
    HomeVrtclPosition.EndVrtcl.y := Species[NodeCount - 1].TplgyOnly.BgnHrzntl.y;
    HomeVrtclPosition.EndVrtcl.x := GrphPnt.x;
    HomeVrtclPosition.BtmHrzntl.y := HomeVrtclPosition.EndVrtcl.y;
    HomeVrtclPosition.BtmHrzntl.x := HomeVrtclPosition.EndVrtcl.x + Trunc(Species[NodeCount - 1].BranchLength * intPixelFactor * kShortLengthFactor);
    for inx := NodeCount downto SpeciesCount do
    begin
      if inx = NodeCount then
      begin
        Species[inx].VrtclPosition.MidPoint.x := HomeVrtclPosition.TopHrzntl.x;
        Species[inx].VrtclPosition.MidPoint.y := HomeVrtclPosition.TopHrzntl.y;
        Species[inx].VrtclPosition.BgnVrtcl.x := HomeVrtclPosition.TopHrzntl.x;
        Species[inx].VrtclPosition.EndVrtcl.x := HomeVrtclPosition.TopHrzntl.x;
      end
      else
        if inx = NodeCount - 1 then
        begin
          Species[inx].VrtclPosition.MidPoint.x := HomeVrtclPosition.BtmHrzntl.x;
          Species[inx].VrtclPosition.MidPoint.y := HomeVrtclPosition.BtmHrzntl.y;
          Species[inx].VrtclPosition.BgnVrtcl.x := HomeVrtclPosition.BtmHrzntl.x;
          Species[inx].VrtclPosition.EndVrtcl.x := HomeVrtclPosition.BtmHrzntl.x;
        end
        else
          if inx = Species[Species[inx].NodeParent].EndSegment.int1stXNd then
          begin
            Species[inx].VrtclPosition.MidPoint.x := Species[Species[inx].NodeParent].VrtclPosition.TopHrzntl.x;
            Species[inx].VrtclPosition.MidPoint.y := Species[Species[inx].NodeParent].VrtclPosition.TopHrzntl.y;  //.TplgyOnly.EndHrzntl.y;  //
            Species[inx].VrtclPosition.BgnVrtcl.x := Species[Species[inx].NodeParent].VrtclPosition.TopHrzntl.x;
            Species[inx].VrtclPosition.EndVrtcl.x := Species[Species[inx].NodeParent].VrtclPosition.TopHrzntl.x;
          end
          else
          begin
            Species[inx].VrtclPosition.MidPoint.x := Species[Species[inx].NodeParent].VrtclPosition.BtmHrzntl.x;
            Species[inx].VrtclPosition.MidPoint.y := Species[Species[inx].NodeParent].VrtclPosition.BtmHrzntl.y;  //.TplgyOnly.EndHrzntl.y;  //
            Species[inx].VrtclPosition.BgnVrtcl.x := Species[Species[inx].NodeParent].VrtclPosition.BtmHrzntl.x;
            Species[inx].VrtclPosition.EndVrtcl.x := Species[Species[inx].NodeParent].VrtclPosition.BtmHrzntl.x;
          end;
      Species[inx].VrtclPosition.BgnVrtcl.y := Species[Species[inx].EndSegment.int1stXNd].TplgyOnly.BgnHrzntl.y;  // Species[inx].Leaves * kVerticalBracket; //div 2;
      Species[inx].VrtclPosition.EndVrtcl.y := Species[Species[inx].EndSegment.int2ndXNd].TplgyOnly.BgnHrzntl.y;  // Species[inx].Leaves * kVerticalBracket; //div 2;
      Species[inx].VrtclPosition.TopHrzntl.x := Species[inx].VrtclPosition.BgnVrtcl.x + Trunc(Species[Species[inx].EndSegment.int1stXNd].BranchLength * intPixelFactor * kShortLengthFactor);
      Species[inx].VrtclPosition.TopHrzntl.y := Species[inx].VrtclPosition.BgnVrtcl.y;
      Species[inx].VrtclPosition.BtmHrzntl.x := Species[inx].VrtclPosition.EndVrtcl.x + Trunc(Species[Species[inx].EndSegment.int2ndXNd].BranchLength * intPixelFactor * kShortLengthFactor);
      Species[inx].VrtclPosition.BtmHrzntl.y := Species[inx].VrtclPosition.EndVrtcl.y;
      if Species[Species[inx].EndSegment.int1stXNd].EndSegment.int1stXNd = Species[Species[inx].EndSegment.int1stXNd].EndSegment.int2ndXNd then
      begin
        Species[Species[inx].EndSegment.int1stXNd].VrtclPosition.MidPoint.x := Species[inx].VrtclPosition.TopHrzntl.x;
        Species[Species[inx].EndSegment.int1stXNd].VrtclPosition.MidPoint.y := Species[inx].VrtclPosition.TopHrzntl.y;
      end;
      if Species[Species[inx].EndSegment.int2ndXNd].EndSegment.int1stXNd = Species[Species[inx].EndSegment.int2ndXNd].EndSegment.int2ndXNd then
      begin
        Species[Species[inx].EndSegment.int2ndXNd].VrtclPosition.MidPoint.x := Species[inx].VrtclPosition.BtmHrzntl.x;
        Species[Species[inx].EndSegment.int2ndXNd].VrtclPosition.MidPoint.y := Species[inx].VrtclPosition.BtmHrzntl.y;
      end;
    end;
    for inx := 1 to NodeCount + 1 do
      with frmPhyIoM.strgrdDataTableProp, Species[inx - 1].VrtclPosition do
      begin
        Cells[inx, 17] := IntToStr(BgnVrtcl.x);
        Cells[inx, 18] := IntToStr(BgnVrtcl.Y);
        Cells[inx, 19] := IntToStr(TopHrzntl.x);
        Cells[inx, 20] := IntToStr(TopHrzntl.Y);
        Cells[inx, 21] := IntToStr(MidPoint.x);
        Cells[inx, 22] := IntToStr(MidPoint.Y);
        Cells[inx, 23] := IntToStr(EndVrtcl.x);
        Cells[inx, 24] := IntToStr(EndVrtcl.Y);
        Cells[inx, 25] := IntToStr(BtmHrzntl.x);
        Cells[inx, 26] := IntToStr(BtmHrzntl.Y);
        Cells[inx, 27] := IntToStr(intVrtclLngth);
        Cells[inx, 28] := IntToStr(intVrtclSpc);
        Cells[inx, 29] := IntToStr(VrtclTopLngth);
        Cells[inx, 30] := IntToStr(VrtclBttmLngth);
        Cells[inx, 31] := IntToStr(Species[inx - 1].NodeTier);
      end;
    AssignBracketRange(GrphPnt, intMaxBracketPosX + intMaxTextLength + 20, intMaxBracketPosY + 20);
  end;
end;

function boolConnected: boolean;
var
  iny: integer;
begin
  Result := True;
  with TestTableForSpeciesData do
    for iny := SpeciesCount to NodeCount do
      Result := Result and Species[iny].TplgyOnly.Connected;
end;

function intMaxTplgyPosY: integer;
var
  inx: integer;
begin
  Result := 0;
  with TestTableForSpeciesData do
  begin
    for inx := 0 to SpeciesCount - 1 do
      With Species[inx].TplgyOnly do
        if EndHrzntl.y > Result then
          Result := EndHrzntl.y;
  end;
  Result := Result + 5;
end;

function intMaxTplgyPosX: integer;
begin
  with TestTableForSpeciesData do
    Result := Species[0].TplgyOnly.EndHrzntl.x;
  Result := Result + 5;
end;

procedure CreateTopologyRerootedTree;
var
  intLeaf,
  inx: integer;
//  iny: integer;
//  inz: integer;
  strArrayLn,
  strArrayPrnt: string;

function strGetSpeciesName(const intAI: integer): string;
var
  pntRRVM: PRerootedViewMember;
begin
  if frmTreeListDown.trvwRerootedTree.Items[0] = nil then
    Result := 'Error'
  else
  begin
    pntRRVM := frmTreeListDown.trvwRerootedTree.Items[intAI].Data;
    Result := pntRRVM^.strSpcsNm;
  end;
end;

begin
  with TestTableForSpeciesData, frmTreeListDown.trvwRerootedTree do
  begin
    strlstArrayErrs.Add('Begin - Title Rerooted Topology Only');
    intLeaf := 0;
    SetLength(sarBranchName, NodeCount + 2);
    SetLength(iarBranchIndex, 2, NodeCount + 2);
    strArrayLn := '';
    strArrayPrnt := '';
    for inx := 0 to NodeCount + 1 do
      if not Items[inx].HasChildren then
      begin
        sarBranchName[intLeaf] := strGetSpeciesName(inx);
        strArrayLn := strArrayLn + '(' + sarBranchName[intLeaf] + ',' + IntToStr(inx) + ')';
        iarBranchIndex[0, intLeaf] := inx;
        iarBranchIndex[1, intLeaf] := Items[Inx].Parent.AbsoluteIndex;
        strArrayPrnt := strArrayPrnt + '(' + IntToStr(iarBranchIndex[1, intLeaf]) + ')';
        inc(intLeaf);
      end;
{    SetLength(arTplgyBrnch, NodeCount + 2);
    for inx := 0 to NodeCount + 1 do
      with Species[inx] do
      begin
        arTplgyBrnch[inx].intPos := intGetSpeciesIndex(inx);
        arTplgyBrnch[inx].intParent := NodeParent;
        arTplgyBrnch[inx].boolHasChildren := Items[inx].HasChildren;
        if not arTplgyBrnch[inx].boolHasChildren then
        begin
          arTplgyBrnch[inx].strBranchName := SpeciesName;
          if inx > 0 then
            if arTplgyBrnch[inx - 1].boolTop then
              arTplgyBrnch[inx].boolTop := False
            else
              arTplgyBrnch[inx].boolTop := True
          else
            arTplgyBrnch[inx].boolTop := True;
        end
        else
          arTplgyBrnch[inx].strBranchName := strBlank;
      end;

  end;}
    strlstArrayErrs.Add(strArrayLn);
    strlstArrayErrs.Add(strArrayPrnt);
    sarBranchName[NodeCount + 1] := 'Root';
    strlstArrayErrs.Add('End - Title Rerooted Topology Only');
  end;
end;

function intMaxLabelLengthPos: integer ;
var
  intLblLngth,
  inx: integer;
begin
  with TestTableForSpeciesData do
  begin
    intLblLngth := 0;
    Result := 0;
    for inx := 0 to SpeciesCount - 1 do
      if intLblLngth < Length(Species[inx].SpeciesName) then
      begin
        intLblLngth := Length(Species[inx].SpeciesName);;
        Result := inx;
      end;
  end;
end;

function intMaxTopologyOnlyWidth(const intFontSize: integer): integer;
var
//  intLblWdth,
  intDsplyWdth: integer;
begin
  intDsplyWdth := intMaxTplgyPosX;
  //intLblWdth := intMaxLabelLength;
  //Result := intMaxTplgyPosX * 5 + intMaxLabelLength * intFontSize; // - 200;  // + 800;
  Result := intDsplyWdth * 5; //+ intDsplyWdth div 2 + intLblWdth * intFontSize + 800; // - 200;  // + 800;
end;

function intMaxBracketWidth(const intFontSize: integer): integer;
begin
  Result := intMaxBracketPosX * 5; // + intMaxLabelLength * intFontSize;  // + 50; // - 900;  // + 800;
  //Result := intMaxBracketPosX * 4 + intMaxBracketPosX div 2 + intMaxLabelLength * intFontSize + 400; // - 900;  // + 800;
end;

procedure CreateTopologyOnlyTree;
var
  GrphPnt: TPoint;
  strWork: string;
  intStrtngPntX,
  intVrtBrckHghtY,
  intParent,
  intHeightPos,
  intNext,
  intHigh,
  inx,
  iny,
  inz: integer;
const
  kintAllocated = -1;
  kstrAllocated = 'Allocated';
  kstrUnallocated = 'Unallocated';

procedure AllocateReservation(const inxBrckt, inxNode: integer; var inxNext: integer);
begin
//  if inxNode = Length(arryinxBracket[inxBrckt]) then
//  begin
    SetLength(arryinxBracket[inxBrckt], inxNode + 1);
    SetLength(arstrConnected[inxBrckt], inxNode + 1);
    arryinxBracket[inxBrckt, inxNode] := kintAllocated;
    arstrConnected[inxBrckt, inxNode] := kstrAllocated;
    inxNext := inxNode + 1;
//  end;
end;

begin
 try
  with TestTableForSpeciesData, frmPhyIoM do
  try
    TierCount := intHighestLevel + 1;
    intStrtngPntX := kStartBracketX + kHrzntlBrcktTO * TierCount;
    intHeightPos := kStrtBrcktTOY;
    inz := 0;
    SetLength(arryinxBracket, inz + 1);
    SetLength(arstrConnected, inz + 1);
    SetLength(arstrConnected[inz], SpeciesCount);
    if boolBootStrap then
      intVrtBrckHghtY := kVrtclBrcktTOBS
    else
      intVrtBrckHghtY := kVrtclBrcktTO;
    for iny := 0 to NodeCount do
      with Species[iny].TplgyOnly do
      begin
        Completed := False;
        Connected := False;
        Attached := False;
        Drawn := False;
        if iny < SpeciesCount then
          arstrConnected[inz, iny] := kstrUnallocated;
      end;
    for iny := 0 to SpeciesCount - 1 do
      with Species[arryinxBracket[inz, iny]] do
        with TplgyOnly do
        begin
          Attached := True;
          BgnHrzntl.y := intHeightPos;
          BgnHrzntl.x := intStrtngPntX - kHrzntlBrcktTO;
          EndHrzntl.y := intHeightPos;
          EndHrzntl.x := intStrtngPntX;
          TopVrtcl.y := intHeightPos;
          TopVrtcl.x := intStrtngPntX;
          BtmVrtcl.y := intHeightPos;
          BtmVrtcl.x := intStrtngPntX;
          Drawn := True;
          inc(intHeightPos, intVrtBrckHghtY);
        end;
    for iny := 0 to SpeciesCount - 1 do
      with Species[arryinxBracket[inz][iny]] do
        with TplgyOnly do
          arstrConnected[inz, iny] := strToF(Connected, 'Connected-0', 'Unconnected-0');
    repeat
      inc(inz);
      inx := 0;
      intNext := 0;
      SetLength(arryinxBracket, inz + 1);
      SetLength(arstrConnected, inz + 1);
      intParent := kintAllocated;
      intHigh := High(arryinxBracket[inz - 1]);
      for iny := 0 to intHigh + 1 do
        if iny < intHigh + 1 then
          with Species[arryinxBracket[inz - 1, iny]] do
            with TplgyOnly do
            begin
              AllocateReservation(inz, inx, intNext);
              if intParent = kintAllocated then
                intParent := NodeParent
              else
                if intParent = NodeParent then
                begin
                  arryinxBracket[inz, inx] := NodeParent;
                  arstrConnected[inz, inx] := 'Unconnected';
                  Connected := True;
//                  Species[arryinxBracket[inz - 1, iny - 1]].TplgyOnly.Connected := True;
                  Species[arryinxBracket[inz - 1, iny - 1]].TplgyOnly.Connected := True;
//                  Species[arryinxBracket[inz, inx]].TplgyOnly.Attached := True;
                  Species[arryinxBracket[inz, inx]].TplgyOnly.Attached := True;
                  arstrConnected[inz - 1, iny] := 'Connected';
                  arstrConnected[inz - 1, iny - 1] := 'Connected';
                  intParent := kintAllocated;
//                  Species[arryinxBracket[inz, inx]].TplgyOnly.Drawn := True;
                  inx := intNext;
                end
                else
                begin
                  arryinxBracket[inz, inx] := arryinxBracket[inz - 1, iny - 1];
                  arstrConnected[inz, inx] := 'Unconnected';
                  Species[arryinxBracket[inz, inx]].TplgyOnly.Attached := True;
                  intParent := NodeParent;
                  Species[arryinxBracket[inz, inx]].TplgyOnly.Drawn := True;
                  inx := intNext;
                end;
            end
        else
//        begin
//          AllocateReservation(inz, inx, intNext);
          if inx < Length(arryinxBracket[inz]) then
            if arryinxBracket[inz, inx] = kintAllocated then
            begin
              arryinxBracket[inz, inx] := arryinxBracket[inz - 1, intHigh];
              Species[arryinxBracket[inz, inx]].TplgyOnly.Attached := True;
              arstrConnected[inz, inx] := 'Unconnected';
              Species[arryinxBracket[inz, inx]].TplgyOnly.Drawn := True;
            end;
//        end;
      if intParent = NodeCount + 1 then
      begin
        SetLength(arryinxBracket[inz], inx + 1);
        arryinxBracket[inz, inx] := NodeCount - 1;
        SetLength(arstrConnected[inz], inx + 1);
        arstrConnected[inz, inx] := 'Unconnected';
      end;
      for iny := 0 to High(arryinxBracket[inz]) do
        with Species[arryinxBracket[inz, iny]] do
          if (TplgyOnly.Drawn) and (not TplgyOnly.Completed) then  // then //and (not TplgyOnly.Connected) then
            TplgyOnly.BgnHrzntl.x := TplgyOnly.BgnHrzntl.x - kHrzntlBrcktTO  // Unconnected lines extended
          else
            if (Species[EndSegment.int1stXNd].TplgyOnly.Connected) and  (Species[EndSegment.int2ndXNd].TplgyOnly.Connected) then
            begin
              Species[EndSegment.int1stXNd].TplgyOnly.Completed := True;
              Species[EndSegment.int2ndXNd].TplgyOnly.Completed := True;
//              if Species[EndSegment.int1stXNd].TplgyOnly.BgnHrzntl.y > Species[EndSegment.int2ndXNd].TplgyOnly.BgnHrzntl.y then
//              begin
                TplgyOnly.TopVrtcl.y := Species[EndSegment.int1stXNd].TplgyOnly.BgnHrzntl.y;
                TplgyOnly.BtmVrtcl.y := Species[EndSegment.int2ndXNd].TplgyOnly.BgnHrzntl.y;
                TplgyOnly.TopVrtcl.x := Species[EndSegment.int1stXNd].TplgyOnly.BgnHrzntl.x;
                TplgyOnly.BtmVrtcl.x := Species[EndSegment.int2ndXNd].TplgyOnly.BgnHrzntl.x;
//              end
//              else
//              begin
//                TplgyOnly.TopVrtcl.y := Species[EndSegment.int2ndXNd].TplgyOnly.BgnHrzntl.y;
//                TplgyOnly.BtmVrtcl.y := Species[EndSegment.int1stXNd].TplgyOnly.BgnHrzntl.y;
//                TplgyOnly.TopVrtcl.x := Species[EndSegment.int2ndXNd].TplgyOnly.BgnHrzntl.x;
//                TplgyOnly.BtmVrtcl.x := Species[EndSegment.int1stXNd].TplgyOnly.BgnHrzntl.x;
//              end;
              TplgyOnly.EndHrzntl.y := TplgyOnly.TopVrtcl.y - (TplgyOnly.TopVrtcl.y - TplgyOnly.BtmVrtcl.y) div 2; // kVrtclBrcktTO div 2;
              TplgyOnly.BgnHrzntl.y := TplgyOnly.TopVrtcl.y - (TplgyOnly.TopVrtcl.y - TplgyOnly.BtmVrtcl.y) div 2 ;
              TplgyOnly.EndHrzntl.x := TplgyOnly.TopVrtcl.x;
              TplgyOnly.BgnHrzntl.x := TplgyOnly.EndHrzntl.x - kHrzntlBrcktTO;
              TplgyOnly.Drawn := True;
            end;
//            else
//            begin
//              TplgyOnly.BgnHrzntl.x := TplgyOnly.BgnHrzntl.x - kHrzntlBrcktTO;  // Unconnected lines extended
//              if Species[EndSegment.int1stXNd].TplgyOnly.Drawn then
//                Species[EndSegment.int1stXNd].TplgyOnly.BgnHrzntl.x := Species[EndSegment.int1stXNd].TplgyOnly.BgnHrzntl.x - kHrzntlBrcktTO;
//              if Species[EndSegment.int2ndXNd].TplgyOnly.Drawn then
//                Species[EndSegment.int2ndXNd].TplgyOnly.BgnHrzntl.x := Species[EndSegment.int2ndXNd].TplgyOnly.BgnHrzntl.x - kHrzntlBrcktTO;
//            end;}
    until Length(arryinxBracket[inz]) = 2; // inz = 4; //
    TierCount := inz;
//    inx := 0;
//    for iny := 0 to SpeciesCount - 1 do
//      with Species[arryinxBracket[0][iny]].TplgyOnly do
//        if not Connected then
//        begin
//          BgnHrzntl.x := BgnHrzntl.x - kHrzntlBrcktTO;  // Unconnected lines extended
//          SetLength(arryinxSpecies, inx + 1);
//          arryinxSpecies[inx] := arryinxBracket[0][iny];
//          inc(inx);
//        end;
//    while not boolConnected do
//    begin
{      inx := 0;
      SetLength(arryinxBracket, 2);
      SetLength(arryinxBracket[1], inx);
      boolParent := False;
      boolPending := False;
      for iny := 0 to High(arryinxBracket[1]) do
        if boolPending then
          if NodeParent = intParent then
          begin
            Connected := True;
            Species[arryinxBracket[1][iny - 1]].TplgyOnly.Connected := True;
            boolPending := False;
          end
          else
            intParent := NodeParent
        else
        begin
          intParent := NodeParent;
          boolPending := True;
        end;
        if not Species[arryinxBracket[0][iny]].TplgyOnly.Connected then
        begin
          SetLength(arryinxBracket[1], inx + 1);
          arryinxBracket[1][inx] := arryinxBracket[0][iny];
//          Species[arryinxBracket[1][inx]].TplgyOnly.Connected := True;
          inc(inx);
        end
        else
          if not boolUsed then
          begin
            SetLength(arryinxBracket[1], inx + 1);
            arryinxBracket[1][inx] := Species[arryinxBracket[0][iny]].NodeParent;
            Species[Species[arryinxBracket[1][inx]].NodeParent].TplgyOnly.TopVrtcl.y := Species[arryinxBracket[0][iny]].TplgyOnly.BgnHrzntl.y;
            Species[Species[arryinxBracket[1][inx]].NodeParent].TplgyOnly.TopVrtcl.x := Species[arryinxBracket[0][iny]].TplgyOnly.BgnHrzntl.x;
            boolUsed := True;
          end
          else
          begin
            Species[Species[arryinxBracket[1][inx]].NodeParent].TplgyOnly.BtmVrtcl.y := Species[arryinxBracket[0][iny]].TplgyOnly.BgnHrzntl.y;
            Species[Species[arryinxBracket[1][inx]].NodeParent].TplgyOnly.BtmVrtcl.x := Species[arryinxBracket[0][iny]].TplgyOnly.BgnHrzntl.x;
            boolUsed := False;
            inc(inx);
          end;
    end;}
//      for iny := 0 to inx - 1 do
//         with Species[arryinxBracket[1][iny]].TplgyOnly do
//           BgnHrzntl.x := BgnHrzntl.x - kHrzntlBrcktTO;}
//    end;
    for inx := 1 to NodeCount + 1 do
      with Species[inx - 1].TplgyOnly, frmPhyIoM.strgrdDataTableProp do
      begin
        Cells[inx, 35] := IntToStr(TopVrtcl.x);
        Cells[inx, 36] := IntToStr(TopVrtcl.y);
        Cells[inx, 37] := IntToStr(BtmVrtcl.x);
        Cells[inx, 38] := IntToStr(BtmVrtcl.y);
        Cells[inx, 39] := IntToStr(BgnHrzntl.x);
        Cells[inx, 40] := IntToStr(BgnHrzntl.y);
        Cells[inx, 41] := IntToStr(EndHrzntl.x);
        Cells[inx, 42] := IntToStr(EndHrzntl.y);
        Cells[inx, 43] := strToF(Completed, 'Completed', 'Uncompleted');
        Cells[inx, 44] := strToF(Connected, 'Connected', 'Unconnected');
        Cells[inx, 45] := strToF(Drawn, 'Drawn', 'UnDrawn');
      end;
    strlstArrayErrs.Clear;
    for inx := 0 to High(arryinxBracket) do
    begin
      strWork := '[';
      for iny := 0 to High(arryinxBracket[inx]) do
      begin
        strWork := StrWork + IntToStr(arryinxBracket[inx, iny]);
        if arryinxBracket[inx, iny] >= SpeciesCount then
          strWork := strWork + '=' + Species[arryinxBracket[inx, iny]].SpeciesName;
        if iny < High(arryinxBracket[inx]) then
          strWork := StrWork + ', ';
      end;
      strWork := StrWork + ']';
      strlstArrayErrs.Append(strWork);
//      strWork := '[';
//      for iny := 0 to High(arryinxBracket[inx]) do
//      begin
//        strWork := StrWork + strToF(Species[arryinxBracket[inx, iny]].TplgyOnly.Connected, 'Connected', 'Unconnected');
//        if iny < High(arryinxBracket[inx]) then
//          strWork := StrWork + ', ';
//      end;
//      strWork := StrWork + ']';
//      strlstArrayErrs.Append(strWork);
    end;
    for inx := 0 to High(arstrConnected) do
    begin
      strWork := '[';
      for iny := 0 to High(arstrConnected[inx]) do
      begin
        strWork := StrWork + arstrConnected[inx, iny];
        if iny < High(arstrConnected[inx]) then
          strWork := StrWork + ', ';
      end;
      strWork := StrWork + ']';
      strlstArrayErrs.Append(strWork);
    end;
    AssignTplgyOnlyRange(GrphPnt, intMaxTplgyPosX + intMaxTextLength + 50, intMaxTplgyPosY + 20);
    mnuShowCalcErrs.Enabled := True;
  finally
    frmPhyIoM.strgrdDataTableProp.AutoSizeColumns;
  end;
 except
   on E:exception do
     MessageDlg(E.Message, mtError, [mbOK], 0);       // Send message to dialog box
 end;
end;

{
Procedure saves the data on Replicates Raw Data page.  The data comes the from current
raw data for this replicate.

Parameter:
  strFlNm -> String of file name to be saved
  intRplct -> integer of replicate data page to be saved
}
procedure SaveReplicateDataFile(const strFlNm: string; const intRplct: integer);
var
  inx,
  iny: integer;
  strtypRD: TStrings;
  strWork: string;
begin
  with TestTableForSpeciesData do
    try
      strtypRD := TStringList.Create;
      try
        strtypRD.Clear;
        if wrdDataType in [3..4] then
          if ExtractFileExt(strFlNm) = '.fas' then
            for iny := 0 to SpeciesCount - 1 do
            begin
              strWork := '>' + Species[iny].SpeciesName;
              strtypRD.Append(strWork);
              strWork := strBlank;
              for inx := 0 to TestCount - 1 do
                strWork := strWork + carSequence[intRplct][iny, inx];
              strtypRD.Append(strWork);
            end
          else
            for iny := 0 to SpeciesCount - 1 do
            begin
              strWork := Species[iny].SpeciesName;
              strtypRD.Append(strWork);
              strWork := strBlank;
              for inx := 0 to TestCount - 1 do
                strWork := strWork + carSequence[intRplct][iny, inx] + ',';
              Delete(strWork, Length(strWork), 1);
              strtypRD.Append(strWork);
            end
        else
        begin
          strWork := SpeciesTitle + '-Rplct:' + IntToStr(intRplct) + ',';
          for inx := 0 to TestCount - 1 do
            strWork := strWork + TestTitle[iarReplicates[intRplct, inx]] + ',';
          Delete(strWork, Length(strWork), 1);
          strtypRD.Append(strWork);
          for iny := 0 to SpeciesCount - 1 do
          begin
            strWork := Species[iny].SpeciesName + ',';
            for inx := 0 to TestCount - 1 do
              if wrdDataType in [0..1] then
                strWork := strWork + carBinary[intRplct][iny, inx] + ','
              else
                strWork := strWork + FloatToStrF(darBtStrpRwDtMsrmnt[intRplct][iny, inx], ffFixed, 10, 4) + ',';
            Delete(strWork, Length(strWork), 1);
            strtypRD.Append(strWork);
          end;
        end;
        strtypRD.SaveToFile(strFlNm);
      finally
        strtypRD.Free;
      end;
    except
      on E:exception do
        MessageDlg(E.Message, mtError, [mbOK], 0);       // Send message to dialog box
    end;
end;

{
Procedure saves distance matrix in the shown page on Distance Matrix tab in a text
file.

Parameter:
  strFlNm -> String of file name to be saved
  intRplct -> integer of replicate data page to be saved
}
procedure SaveDMReplicateFile(const strFlNm: string; const intRplct: integer);
var
  inx,
  iny: integer;
  strtypRD: TStrings;
  strWork: string;
begin
  with TestTableForSpeciesData do
    try
      strtypRD := TStringList.Create;
      try
        strtypRD.Clear;
        strWork := 'Replicate:' + IntToStr(intRplct);
        for inx := 0 to SpeciesCount - 1 do
          strWork := strWork + ',' + Species[inx].SpeciesName;
        strtypRD.Append(strWork);
        for iny := 0 to SpeciesCount - 1 do
        begin
          strWork := Species[iny].SpeciesName;
          for inx := 1 to iny do
            strWork := strWork + ',' + FloatToStrF(darBSDistanceMatrix[intRplct][inx - 1][iny], ffFixed, 10, 6);
          strtypRD.Append(strWork);
        end;
        strtypRD.SaveToFile(strFlNm);
      finally
        strtypRD.Free;
      end;
    except
      on E:exception do
        MessageDlg(E.Message, mtError, [mbOK], 0);       // Send message to dialog box
    end;
end;

{
Procedure to save file in MEGA format for use in the Mega application for a distance
matrix output as text.

Parameter:
  strFlNm -> String of file name to be saved
}
procedure SaveMegaFormatFile(const strFlNm: string);
var
  inx,
  iny: integer;
  strtypMF: TStrings;
  strWork: string;
begin
  with TestTableForSpeciesData do
  try
    strtypMF := TStringList.Create;
    try
      strtypMF.Clear;
      strWork := '#mega';
      strtypMF.Append(strWork);
      strWork := '!Title: ' + SpeciesTitle + ';';
      strtypMF.Append(strWork);
      strWork := '!Format DataType=Distance DataFormat=LowerLeft NTaxa=' + IntToStr(SpeciesCount) + ';';
      strtypMF.Append(strWork);
      strtypMF.Append(strBlank);
      for inx := 1 to SpeciesCount do
      begin
        strWork := '[' + IntToStr(inx) + '] #' + strSpcToUndrscr(Species[inx - 1].SpeciesName);
        strtypMF.Append(strWork);
      end;
      strtypMF.Append(strBlank);
      strWork := '[   ';
      for inx := 1 to SpeciesCount do
        strWork := strWork + IntToStr(inx) + '    ';
      strWork := strWork + ']';
      strtypMF.Append(strWork);
      strtypMF.Append(strBlank);
      for inx := 1 to SpeciesCount do
      begin
        strWork := '[' + IntToStr(inx) + ']';
        for iny := 1 to inx - 1 do
          strWork := strWork + ' ' + FloatToStrF(DistanceMatrix[iny - 1, inx - 1], ffFixed, 10, 6);
        strtypMF.Append(strWork);
      end;
      strtypMF.SaveToFile(strFlNm);
    finally
      strtypMF.Free;
    end;
  except
    on E:exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);       // Send message to dialog box
  end;
end;

{
Procedure to save file in Fasta format with original data coming from the Main
Data String List and converted to Fasta String List and then saved to a text file.

Parameter:
  strFlNm -> String of file name to be saved
}
procedure SaveFastaFormatFile(const strFlNm: string);
var
  inx,
  iny: integer;
  strtypFF: TStrings;
  strWork: string;
begin
  with TestTableForSpeciesData do
  try
    strtypFF := TStringList.Create;
    try
      for inx := 0 to SpeciesCount - 1 do
      begin
        strWork := '>' + Species[inx].SpeciesName;
        strtypFF.Append(strWork);
        strWork := strBlank;
        for iny := 0 to TestCount - 1 do
          strWork := strWork + Sequence[inx, iny];
        strtypFF.Append(strWork);
      end;
      strtypFF.SaveToFile(strFlNm);
    finally
      strtypFF.Free;
    end;
  except
    on E:exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);       // Send message to dialog box
  end;
end;

{
Procedure to save file in Nexus format for use in the PAUP application for a distance
matrix output as text.

Parameter:
  strFlNm -> String of file name to be saved
}
procedure SaveNexusFormatFile(const strFlNm: string);
var
  inx,
  iny: integer;
  strtypNxs: TStrings;
  strWork: string;
begin
 with TestTableForSpeciesData do
 try
   strtypNxs := TStringList.Create;
   try
     strWork := '#nexus';
     strtypNxs.Append(strWork);
     strtypNxs.Append(strBlank);
     strWork := 'begin taxa;';
     strtypNxs.Append(strWork);
     strWork := '  dimensions ntax=' + IntToStr(SpeciesCount) + ';';
     strtypNxs.Append(strWork);
     strWork := '  taxlabels';
     for inx := 0 to SpeciesCount - 1 do
       strWork := strWork + ' ' + strSpcToUndrscr(Species[inx].SpeciesName);
     strWork := strWork + ';';
     strtypNxs.Append(strWork);
     strWork := 'end;';
     strtypNxs.Append(strWork);
     strtypNxs.Append(strBlank);
     strWork := 'begin distances;';
     strtypNxs.Append(strWork);
     strWork := '  dimensions ntax=' + IntToStr(SpeciesCount) + ';';
     strtypNxs.Append(strWork);
     strWork := '  format diagonal labels triangle=lower;';
     strtypNxs.Append(strWork);
     strWork := '  matrix';
     strtypNxs.Append(strWork);
     for inx := 0 to SpeciesCount - 1 do
     begin
       strWork := '    ' + strConversion(Species[inx].SpeciesName, ' _');
       for iny := 0 to inx do
         strWork := strWork + ' ' + FloatToStrF(DistanceMatrix[inx, iny], ffFixed, 12, 6);
       strtypNxs.Append(strWork);
     end;
     strWork := '  ;';
     strtypNxs.Append(strWork);
     strWork := 'end;';
     strtypNxs.Append(strWork);
     strtypNxs.SaveToFile(strFlNm);
   finally
      strtypNxs.Free;
    end;
 except
   on E:exception do
     MessageDlg(E.Message, mtError, [mbOK], 0);       // Send message to dialog box
 end;
end;

{
Function to returns a string to a Newick Format for a label and branch length for a
leaf or branch length for an interior node.  If Pos is negative it must use the length
parameter.  If the branch length is zero it used the branch length at Pos of the record.

Parameters:
  Pos -> Position of indice in the Test Data record array using an integer value
  dblLblLngth -> Length of branch as a double number
}
function strBranchLabel(const Pos: integer; const dblLblLngth: double): string;
var
  dblBL: double;
begin
  if Pos > -1 then
    with TestTableForSpeciesData.Species[Pos] do
    begin
      if dblLblLngth > 0 then
        dblBL := dblLblLngth
      else
        dblBL := BranchLength;
      Result := SpeciesName + ':' + FloatToStrF(dblBL, ffFixed, 10, 4);
    end
  else
    Result := ':' + FloatToStrF(dblLblLngth, ffFixed, 10, 4);
end;

function strRerootedNewickFrmt(const strBarNewick: string): string;
var
  strToken,
  strNwkWork,
  strWork: string;
//  intLngth,
//  intStart,
  intPos: integer;
const
  kBar = '|';
  kLParen = '(';
  kRParen = ')';
  kComma = ',';
  kParenComma = kLParen + kComma;
  kCommaParen = kComma + kRParen;

function strGetToken(var strUnTokened: string): string;
var
  intLngth,
//  intStart,
  intPos: integer;
begin
  intLngth := Length(strUnTokened);
  intPos := Pos(kBar, strUnTokened);
  if intPos > 0 then
    Result := Copy(strUnTokened, 1, intPos - 1)
  else
    Result := strUnTokened;
  Delete(strUnTokened, 1, Length(Result) + 1);
end;

function intTokenInsertion(const strNwk: string): integer;
var
  intPosRP,
  intPosLP: integer;
begin
  intPosLP := Pos(kParenComma, strNwk);
  intPosRP := Pos(kCommaParen, strNwk);
  if (intPosRP > intPosLP) and (intPosLP > 0) then
    Result := intPosLP + 1
  else
    Result := intPosRP + 1;
end;

begin
  strWork := strBlank;
  strNwkWork := strBarNewick;
//  Delete(strNwkWork, 5, 1);
  while Length(strNwkWork) > 0 do
  begin
    strToken := strGetToken(strNwkWork);
    if strWork = strBlank then
      strWork := strToken
    else
      if strToken <> strBlank then
      begin
        intPos := intTokenInsertion(strWork);
        Insert(strToken, strWork, intPos);
      end;
  end;
{  strWork := Copy(strNwkWork, 1, intPos - 1);
  intStart := intPos + 1;
  intPos := Pos(kBar, Copy(strNwkWork, intStart, intLngth - intStart));
  Insert(Copy(strNwkWork, intStart, intPos - intStart - 1), strWork, 2);}
  Result := strWork
end;

function intFindShortLabel(const strShrtLbl: string): integer;
var
  inx: integer;
begin
  with TestTableForSpeciesData do
    for inx := 0 to NodeCount do
      with Species[Inx] do
        if ShortLabel = strShrtLbl then
        begin
          Result := inx;
          exit;
        end;
  Result := -1;
end;

function nodSelectBranch(const nodItems: TTreeNodes; const intRt: integer): TTreeNode;
var
  TreeMember: PTreeViewMember;
  inx: integer;
begin
  for inx := 0 to nodItems.Count - 1 do
  begin
    TreeMember := PTreeViewMember(nodItems.Item[inx].Data);
    if TreeMember^.intNodePos = intRt then
    begin
      Result := nodItems.Item[inx];
      exit;
    end;
  end;
  Result := nil;
end;

function ortmGetMode(const intOrgNP: integer): TOrgRootMode;
begin
  if intOrgNP = iaOldRoot[0] then
    Result := ORootT
  else
    if intOrgNP = iaOldRoot[1] then
      Result := ORootB
    else
      if intOrgNP = iaOldRoot[2] then
        Result := OrgRoot
      else
        Result := NnORoot;
end;

end.
