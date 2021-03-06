// Module for records, types, arrays, and variables that are common throughout
// the application.
// Last update: 03/24/2018 21:02  KET
unit modDataTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, ComCtrls, Grids, Dialogs, Graphics;

type
  TBinaryDataType = set of char;
  TSequenceDataType = set of char;
  CompassPnt = (N, NE, E, SE, S, SW, W, NW);
  TVerticalMode = (Open, AParent, Child, Leaf);
  TRootMode = (NonRoot, AtRoot, RootT, RootB);
  TOrgRootMode = (NnORoot, OrgRoot, ORootT, ORootB, FindOrgRt);
  QuadrantSigns = record
    A: single;
    X, Y: integer;
  end;
  QudrrantFactor = Array[CompassPnt] of QuadrantSigns;

const
  karryMinusPlus: TBinaryDataType = ['-','+']; // Binary data type - or +
  karryZeroOne: TBinaryDataType = ['0','1'];   // Binary data type 0 or 1
  karrayNucleotide = ['A','C','G','T'];        // Nucleotide data type A, C, G, or T
  karrayMissingDataSeq = ['?','_'];
  kLabelCoversion = ';|({)}';
  kNotViewable: TPoint = (x:-1;y:-1);
  kLP = '(';
  kComma = ',';
  kRP = ')';
  kSmCln = ';';
  kColon = ':';
  kAsterick = '*';
  kVertDash = '|';
  kHorzDash = '-';
  kMissingData = '?';
  kUnmatching = 'U';
  kMatching = 'M';
  kHorzLine = 196;  // ---
  kVertLine = 179;  // |||
  kBrnchSplt = 180; // -| -| -|
  kUpprBrnch = 218; // +++
  kLwrBrnch = 192;  // +++
  dblZero: double = 0.0;
  dblApproxZero = 0.00000001;
  dblWhiteSpace: double = -1.0;
  strZero = '0.000';
  strBlank = '';
  kRightangle = 90.0;
  kboolTest = False;
  kboolExit = False;
  kboolClear = True;
  kboolFirst = True;
  kboolSecond = False;
  kUnknown = -1;
  kSpltFctr: double = 1.75;
  kRghtAnglBsctd = kRightangle / 2.0;
  kSemiCircle = kRightangle * 2.0;
  kThrQtrAngle = kRightangle * 3.0;
  kFullCircle = kSemiCircle * 2.0;
  kStartAngle = 90.0;
  EmptyBinary = '*';
  intZeroTests = 0;
  kintDisabledSpcs = -1;
  kintInvalidBracket = -1;
  kStartBracketX = 20;
  kStartBracketY = 400;
  kStrtBrcktTOX = 50;
  kStrtBrcktTOY = 90;
  kUnknownNode = -1;
  wrdNoType = 65535;
  wrdBPM = 0;                     // Binary +/- type data
  wrdB01 = 1;                     // Binary 0/1 type data
  wrdMeasurement = 2;             // Measurement type data
  wrdSequence = 3;                // Sequence type data for DNA or protein p-Distance
  wrdSequenceJC = 4;              // Sequence type data for DNA or protein JC-Distance
  MinSpecies = 1;
  MinTests = 1;
  kVerticalBracket = 15;
  kVrtclBrcktTO = 26;
  kVrtclBrcktTOBS = 32;
  kHrzntlBrcktTO = 50;
  kShortLengthFactor = 1;
  kVrtDblLfSpc = 20;
  kRadialTree = 0;                // Radial tree graph
  kBracketTree = 1;               // Rectangular tree graph
  kTopologyOnlyTree = 2;          // Topology only rectangular tree graph
  kNwckBrcktTree = 3;             // Newick format Rectangular tree graph
  kNewickTOTree = 4;              // Newick format Topology only tree graph
  kUnknownDsp = 5;                // Unknown tree format
  kInvalidBinary = '**';          // Invalid binary bracket for invalid data
  kInvalidData: double = -9.999999E+203; // Invalid data constant
  kFullTitleLength = 1024;        // Single line string limit in memeo control
  kDataTableTitle: array[0..46] of string = ('Node Name', 'Index', 'QIndex', 'IsActive', 'Branch Length', 'Node Level', 'Node Parent', 'Angle', 'B-S-1', 'B-S-2', 'E-S-1', 'E-S-2', 'B-R-X', 'B-R-Y', 'E-R-X', 'E-R-Y', 'Newick Token',
                                             'Begin Vertical.x', 'Begin Vertical.y', 'Top Horizontal.x', 'Top Horizontal.y', 'Midpoint.x', 'Midpoint.y', 'End Vertical.x', 'End Vertical.y', 'Bttm Horizontal.x', 'Bttm Horizontal.y',
                                             'Vertical Length', 'Vertical Space', 'Vertical Top', 'Vertical Bottom', 'Node Tier', 'Bootstrap', 'Leaves', 'Bracket Index', 'Top Vertical.x', 'Top Vertical.y', 'Bttm Vertical.x', 'Bttm Vertical.y',
                                             'Begin Horizontal.x', 'Begin Horizontal.y', 'End Horizontal.x', 'End Horizontal.y', 'Completed', 'Connected', 'Drawn', 'Absolute Index');
  karryQF: QudrrantFactor = ((A:0.0; X:0; Y:1), (A:45.0; X:1; Y:1), (A:90.0; X:1; Y:0), (A:135.0; X:1; Y:-1), (A:180.0; X:0; Y:-1),
                             (A:225.0; X:-1; Y:-1), (A:270.0; X:-1; Y:0), (A: 315.0; X:-1; Y:1));

type
  PTreeViewMember = ^TTreeViewMember;
  TTreeViewMember = record
    intNodePos,
    intNodeParent,
    intNodeSibling: integer;
    intChildren: array[0..1] of integer;
    pntTVMLftChld,
    pntTVMRghtChld: PTreeViewMember;
  end;
  PRerootedViewMember = ^TRerootedViewMember;
  TRerootedViewMember = record
    dblBL: double;
    strBranchLabel,
    strSpcsNm,
    strNewick: string;
    rtmMmbr: TRootMode;
    ortmMmbr: TOrgRootMode;
  end;
  setNwkDlmtr = (LP,Lbl,Comma,RP);
  setTreePart = (Trunk, Branch, LeafT);
  setTreePlot = (Undrawn, // Unowned line on the topology tree 4 spaces
                 LeafEnd, // Leaves with 4 horz. lines
                 UpprCrnr, // Terminate line with corner and 4 spaces
                 LwrCrnr, // Terminate line with corner and 4 spaces
                 LftTee, // Begin line with left tee and 4 horizontal lines
                 CvrLine, // Extend vertical line with 4 spaces with line above or below
                 ExtLine, // Extend horizontal line with 5 horizontal lines
                 Spaces, // Extend with 5 spaces for covered or unowned lines
                 RootTee); // Begin line with left tee and 2 horizontal lines and 2 spaces
  asTreePlot = array[setTreePlot] of string;
  rcdRectTreePlot = record
    intPrnt,
    intFC,                        //
    intLC,                        //                 TpSide
    intBrcktLvl,                  //                  (x,y)
    intBrnchLngth: integer;       //                    *
    dblBrnchLngth: double;        // LSide              |
    tpTreePlot: setTreePlot;      // (x,y)              |RSide
    TpSide,                       //   *----------------*(x,y) <strLabel
    LSide,                        //          ^         |
    RSide,                        //     dblBrnchLngth  |
    BttmSide: TPoint;             //                    *
    strLabel: string;             //                  (x,y)
  end;                            //                 BttmSide
  rcdTOTreePlot = record          //                 TpSide
    intBrnchLngth,                //                  (x,y)
    intLevel: integer;            //                    *
    dblBrnchLngth: double;        // LSide              |
    tpTreePlot: setTreePlot;      // (x,y)              |RSide
    TpSide,                       //   *----------------*(x,y) <strLabel
    LSide,                        //                    |
    RSide,                        //                    |
    BttmSide: TPoint;             //                    *
    strLabel: string;             //                  (x,y)
  end;                            //                 BttmSide
  rcdTreePlot = record            //
    intBrnchLngth,                //                 TpSide
    intLevel,                     //                  (x,y)
    intOwner: integer;            //                    *
    dblBrnchLngth: double;        // LSide              |
    tpTreePlot: setTreePlot;      // (x,y)              |RSide
//    TpSide,                     //   *----------------*(x,y)
//    LSide,                      //                    |
//    RSide,                      //                    |
//    BttmSide: TPoint;           //                    *
    strLabel,                     //                  (x,y)
    strPlot: string;              //                 BttmSide
  end;                            //
  rcdTreeToken = record
    tpToken: setTreePart;
    nwkDlmtr: setNwkDlmtr;
    boolStaging,
    boolDrawn,
    boolEnded,
    boolCovered: boolean;
    intLPos,
    intTOPos,
    intLine,
    intOffset,
    intLevel,
    intPrnt,
    intFC,
    intLC: integer;
    strLength,
    strBootStrap,
    strEdge,
    strLeaf: string;
  end;
  TTplgyBrnch = record            // Topology branch record for rerooted branches
    intPos,                       // Position in Species Table record
    intParent: integer;           // Parent in the Species Table record                       TopVrtcl
    strBranchName: string;        // Branch label or bootstrap label                            (x,y)
    TopVrtcl,                     // Top end of vertical line                                     +
    BtmVrtcl,                     // Bottom end of vertical line                                  |
    BgnHrzntl,                    // Begin of horizontal line            BgnHrzntl                |EndHrzntl
    EndHrzntl: TPoint;            // End of horizontal line                (x,y)+-----------------+(x,y)<-strBranchName
    Completed,                    // Node doesn't need further drawing                            |
    Connected,                    // Node is connected to children                                |
    Attached,                     // Node is attached ready for drawing and connecting            +
    Drawn,                        // Node is drawn                                             BtmVrtcl
    boolTop,                      // Is top branch or bottom                                    (x,y)
    boolHasChildren: boolean;     // Branch has children
  end;
  TTreeOrigin = record
    GraphOrigin: TPoint;
    Width,
    Height: integer;              //                                                    Segment      Segment
  end;                            //                                                     (x,y)        (x,y)
  TOrigin = record                //                                              Begin    +------------+    End
    initWidth,                    //                                                    [Parent]    [Child 1]  int1stXNd
    initHeight: integer;          //                                                    [Sibling]   [Child 2]  int2ndXNd
    ttoBracket,                   //
    ttoTplgyOnly,                 //
    ttoNwckBrckt,                 //
    ttoNewickTO,                  //
    ttoRadial: TTReeOrigin;       //
  end;                            //                                                          BgnVrtcl         TopHrzntl
  TSegment = record               //                                                           (x,y)             (x,y)
    pntRadial: TPoint;            // Point(x,y) on radial tree                                   +-----------------+
    int1stXNd,                    // Indexes to child branches                                   |
    int2ndXNd: integer;           //                                                             |
  end;                            //                                                     MidPoint+
  TVrtclPosition = record         //                                                       (x,y) |
    BgnVrtcl,                     // Top vertical of vertical line                               |
    TopHrzntl,                    // Top end of horizontal                                       +-----------------+
    MidPoint,                     // Midpoint of vertical                                     EndVrtcl         BtmHrzntl
    EndVrtcl,                     // Bottom vertical of vertical line                          (x,y)             (x,y)
    BtmHrzntl: TPoint;            // Bottom end of horizontal
    intVrtclLngth,                // Full length of vertical line
    intVrtclSpc,                  // Vertical space taken to form most parent node
    VrtclTopLngth,                // Vertical top height for bracket graph
    VrtclBttmLngth,               // Vertical bottom height for bracket graph
    VrtclFctr: integer            //
  end;                            //                                                           TopVrtcl
  TTplgyOnly = record             //                                                            (x,y)
    TopVrtcl,                     // Top end of vertical line                                     +
    BtmVrtcl,                     // Bottom end of vertical line                                  |
    BgnHrzntl,                    // Begin of horizontal line            BgnHrzntl                |EndHrzntl
    EndHrzntl: TPoint;            // End of horizontal line                (x,y)+-----------------+(x,y)->SpeciesName
    Completed,                    // Node doesn't need further drawing                            |
    Connected,                    // Node is connected to children                                |
    Attached,                     // Node is attached ready for drawing and connecting            +
    Drawn: boolean;               // Node is drawn                                             BtmVrtcl
  end;                            //                                                            (x,y)
  TSpeciesAndNodes = record       // Species and Interioir nodes record
    ShortLabel,                   // Short label for rerooting
    NewickToken,                  // Newick name for each species or node.         EndSegment(x,y)
    SpeciesName: string;          // Name of species for measurement records                   +
    dblAngle,                     // Divertion angle from parent node                         /|
    BranchLength: double;         // Distance for square bracket tree                        / |
    BeginSegment,                 // Begin point of branch                                  /  |
    EndSegment: TSegment;         // End point of branch                                   /   |
    AbsIndex,                     // Absolute Index of tree view                         H/    |O
    Leaves,                       // Number of leaves or ended children for this node    /     |
    inxBracket,                   // Bracket position on rectangular tree               /      |
    NodeLevel,                    // Bracket level for tree                            /       |
    NodeTier,                     // For tier in same bracket levels                  /        |
    NodeParent: integer;          // Parent of this species                          /dblAngle |
    VerticalMode: TVerticalMode;  // Open, Parent, child, or leaf vertical line     +----------+
    VrtclPosition: TVrtclPosition;// Position end and begin of vertical bracket     ^     A
    TplgyOnly: TTplgyOnly;        // Topology only graph points                     |-----------BeginSegment(x,y)
    boolPushed: boolean;          // Parent segments causing recalculation
  end;                            //
  T1DDoubleArray = array of double;                       // One dimesional array for double
  T2DDoubleArray = array of T1DDoubleArray;               // Two dimesional array for species vs. species for distance
  T3DDoubleArray = array of T2DDoubleArray;               // Three dimesional array for species vs. species for test numerical
  T1DArrayBoolean = array of boolean;                     // One dimesional array for boolean
  T2DArrayBoolean = array of T1DArrayBoolean;             // Two dimesional array for boolean
  T1DCharArray = array of char;                           // One dimesional array for characters
  TMeasurementMatrix = T2DDoubleArray;                    // Two dimesional array for species vs. tests measurements
  TBinaryMatix = array of T1DCharArray;                   // Two dimesional array for species vs. tests binary
  TSequenceMatix = array of T1DCharArray;                 // Two dimesional array for species vs. tests sequence
  TMatchingMatrix = array of array of array of char;      // Two dimesional array for species vs. species for test binary
  TUnmatchingMatrix = array of array of array of char;    // Two dimesional array for species vs. species for test sequence
  TQMatrix = T2DDoubleArray;                              // Two dimesional array for species vs. species for Q Matrix
  TFullQMatrix = array of TQMatrix;                       // Three dimesion array for species vs. species for Q Matrix
  TBSMeasurementMatrix = array of TMeasurementMatrix;     // Boot Strap Three dimesional array for species vs. tests measurements
  TBSBinaryMatix = array of TBinaryMatix;                 // Boot Strap Three dimesional array for species vs. tests binary
  TBSSequenceMatrix = array of TSequenceMatix;            // Boot Strap Three dimesional array for species vs. tests sequence
  TIsActive = T1DArrayBoolean;                            // Array of active boolean species
  T1DArrayInteger = array of integer;                     // One dimesional array for integer
  T2DArrayInteger = array of T1DArrayInteger;             // Two dimesional array for integer
  T3DArrayInteger = array of T2DArrayInteger;             // Three dimesional array for integer
  TBSQIndex = T1DArrayInteger;                            // Array of QIdexes for species
  TDistanceIndex = array[0..2] of integer;                // Array index of final 3 distances of Distance Matrix
  TOldRootIndex = array[0..2] of integer;                 // Array index of 3 nodes making up the original root
  TBSDistanceMatrix = array of T2DDoubleArray;           // Array of distance matrices holding bootstrap values
  TBSSpeciesNodes = array of TSpeciesAndNodes;            // Array of Species and Nodes holding different bootstrap labels
  TTestTableForSpecies = record                     // Main record structure for Species and tree
    wrdDataType,                                    // Type of data measurement
    SpeciesCount,                                   // Number of species
    NodeCount,                                      // Number of nodes or species
    LastNode,
    TestCount,                                      // Number of tests
    TierCount,
    ErrorCount: word;                               // Number of errors
    intRoot,                                        // Current Root node index
    inxQ: integer;                                  // Indice of Last QMatrix created
    boolNewCalculation,                             // If a data reset has occurred
    boolBootStrap: boolean;                         // Bootstrap is being calculated
    SpeciesTitle: string;                           // Title over species name for species group
    TestTitle: array of string;                     // Title for each test
    Species: TBSSpeciesNodes;                       // Array of species measurement records.  Zero based array.
    IsActive: TIsActive;                            // Whether Species is available to calculate distance matrix or Q Matrix
    QIndex: TBSQIndex;                              // Array index to hold QMatrix working indices
    Measurement: TMeasurementMatrix;                // Measurement for tests for species.  Zero based array.  Species by Tests
    Binary: TBinaryMatix;                           // Binary data for tests for species.  Zero based array.  Species by Tests
    Sequence: TSequenceMatix;                       // Sequence data for tests for species.  Zero based array.  Species by Tests
    iarRplctMtchs: T1DArrayInteger;                 // Array of matching replicates
    inxSpcsQM: array of integer;                    // Reverse of QIndex holds position of QMatrix in Distance matrix.
    HiddenNodeName: string;
    dblSpltFctr,
    HiddenPercent: double;
    BSPercent: T1DDoubleArray;
    intSplit,
    HiddenMatches: integer;
    sngHalfInterior,
    sngHalfAngle,
    sngInterior,
    sngExterior,
    sngIntrrOffset,
    sngExtrrOffset: single;
  end;
  TNodePoint = array[0..1] of integer;
  TSegmentPoints = array[0..1] of TNodePoint;
  TDuplicateNodes = array of integer;

const
  EmptySegment: TSegment = (pntRadial:(x:0; y:0); int1stXNd:0; int2ndXNd:0);
  EmptySpecies: TSpeciesAndNodes =(ShortLabel:''; NewickToken:''; SpeciesName:''; dblAngle:-1.0; BranchLength:0.0; BeginSegment:(pntRadial:(x:0; y:0); int1stXNd:-1; int2ndXNd:-1);
                                   EndSegment:(pntRadial:(x:0; y:0); int1stXNd:-1; int2ndXNd:-1); AbsIndex:-1; Leaves:0; inxBracket:-1; NodeLevel:-1; NodeTier:0; NodeParent:0; VerticalMode:Open;
                                   VrtclPosition:(BgnVrtcl:(x:0; y:0); TopHrzntl:(x:0; y:0); MidPoint:(x:0; y:0); EndVrtcl:(x:0; y:0); BtmHrzntl:(x:0; y:0); intVrtclLngth:0; intVrtclSpc:0; VrtclTopLngth:0; VrtclBttmLngth:0; VrtclFctr:0);
                                   TplgyOnly:(TopVrtcl:(x:0; y:0); BtmVrtcl:(x:0; y:0); BgnHrzntl:(x:0; y:0); EndHrzntl:(x:0; y:0); Completed:False; Connected:False; Attached:False; Drawn:False); boolPushed:False);
  DefineOrigin: TOrigin = (initWidth:0; initHeight:0; ttoBracket:(GraphOrigin:(x:0; y:0); Width:0; Height:0); ttoTplgyOnly:(GraphOrigin:(x:0; y:0); Width:0; Height:0); ttoNwckBrckt:(GraphOrigin:(x:0; y:0); Width:0; Height:0); ttoNewickTO:(GraphOrigin:(x:0; y:0); Width:0; Height:0); ttoRadial:(GraphOrigin:(x:0; y:0); Width:0; Height:0));

var
  TestTableForSpeciesData: TTestTableForSpecies; {Test table for measurement and binary data}
  rctSelectedCell: TGridRect;                    {Cell location holding error value}
  DifferenceMatrix: T3DDoubleArray;              {Difference table for measerement data}
  MatchingMatrix: TMatchingMatrix;               {Matching table for binary data}
  DffrncSqncMatrix: TUnmatchingMatrix;           {Unmatching table for sequence data}
  QMatrix: TFullQMatrix;
  DistanceMatrix: T2DDoubleArray;
  carBinary: TBSBinaryMatix;
  carSequence: TBSSequenceMatrix;
  darBtStrpRwDtMsrmnt: TBSMeasurementMatrix;
  darBSDistanceMatrix: TBSDistanceMatrix;
  darBSQMatrix: TFullQMatrix;
  sarBranchName: array of string;
  iarBranchIndex: array of array of integer;
  boolarIsActive: TIsActive;
  intPixelFactor: integer;
  Origin: TOrigin;
  boolGraphInitial: boolean;
  arryinxBracket: array of array of integer;
  dblSumLngth: double;
  ariNextNd,
  ariInsrtnPnt,
  ariInsrtnNd: array of integer;
  iarReplicates,
  iarInsrtnPnt: array of array of integer;
  arstrConnected,
  arstrNodeNames: array of array of string;
  arsNewickBanch: array of array of string;
  arTplgyBrnch: array of TTplgyBrnch;
  NullSegment: TSegment;
  iaOldRoot: TOldRootIndex;
  ariNodeParent: array[0..2] of integer;
  DuplicateNodes: TDuplicateNodes;
  strNwckFrmt,                                    // string for Newick format
  strCurrentDir: string;
  strlstDstncMtrx,
  stlsRecTree,
  strlstArrayErrs,
  strlstCalcErrs: TStringList;
  aTOTreePlot: array of rcdTOTreePlot;
  aRectTreePlot: array of rcdRectTreePlot;
  arTreeToken: array of rcdTreeToken;
  arTreePlot: array of rcdTreePlot;
  astrTreePlot: asTreePlot;
  ariLengthPos: array of integer;
  astrBtStrp,
  astrLengths: array of string;

function intBnryPwr(const intExp: integer): integer;
function strRepeat(const C:char; const intN: integer): string;
function strToF(const boolToF: boolean; const strTrue, strFalse: string): string;
function strSpcToUndrscr(const strSpaced: string): string;

implementation

{
Returns a integer number 2 raised to the power of the paramter.  Negative numbers
return zero

Parameter:
  intExp -> an integer value used as power of 2
}
function intBnryPwr(const intExp: integer): integer;
var
  inx: integer;
begin
  if intExp < 0 then
    Result := 0
  else
  begin
    Result := 1;
    for inx := 1 to intExp do
      Result := Result * 2;
  end;
end;

{
Returns a string with specified number of characters.

Parameters:
  C -> character to repeat
  intN -> integer for number of times to repeat character
}
function strRepeat(const C:char; const intN: integer): string;
var
  inx: integer;
begin
  Result := strBlank;
  for inx := 1 to intN do
    Result := Result + C;
end;

{
Function to return a string with one for a true result and one for a false result.

Parameters:
  boolToF -> boolean value to be tested for true or false
  strTrue -> string parameter to return if boolean vslue is true
  strFalse -> string parameter to return if boolean vslue is false
}
function strToF(const boolToF: boolean; const strTrue, strFalse: string): string;
begin
  if boolToF then
    Result := strTrue
  else
    Result := strFalse;
end;

{
Function to return a string with spaces converted to underscores.

Parameter:
  strSpaced -> string to be converted
}
function strSpcToUndrscr(const strSpaced: string): string;
var
  inx: integer;
begin
  Result := strSpaced;
  for inx := 1 to Length(strSpaced) do
    if strSpaced[inx] = ' ' then
      Result[inx] := '_';
end;

end.

