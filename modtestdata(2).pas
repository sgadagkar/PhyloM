// Module to load measurement and binary data to grid and record structures
unit modTestData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ComCtrls, Grids, Dialogs;

type
  TBinaryDataType = set of char;

const
  karryMinusPlus: TBinaryDataType = ['-','+']; // Binary data type - or +
  karryZeroOne: TBinaryDataType = ['0','1'];   // Binary data type 0 or 1
  sngZero: Single = 0.0;
  EmptyBinary = '*';
  intZeroTests = 0;
  MinSpecies = 1;
  MinTests = 1;
  kInvalidBinary = '**';          // Invalid binary bracket for invalid data
  kInvalidData: Single = -9999.0; // Invalid data constant
  kFullTitleLength = 1024;

type
  TSpeciesMeasurement = record
    SpeciesName: string;          // Name of species for measurement records
    Measurement: array of single; // Measurement for tests for species.  Zero based array.
  end;
  TTestTableMeasurement = record
    SpeciesCount,                         // Number of species
    TestCount,                            // Number of tests
    ErrorCount: word;                  // Number of errors
    SpeciesTitle: string;                 // Title over species name for species group
    TestTitle: array of string;           // Title for each test
    Species: array of TSpeciesMeasurement; // Array of species measurement records.  Zero based array.
  end;
  TSpeciesBinary = record
    SpeciesName: string;          // Name of species for binary data records
    Binary: array of Char;        // Binary data for tests for species.  Zero based array.
  end;
  TTestTableBinary = record
    SpeciesCount,                         // Number of species
    TestCount,                            // Number of tests
    ErrorCount: word;                  // Number of errors
    SpeciesTitle: string;                 // Title over species name for species group
    TestTitle:  array of string;          // Title for each test
    Species: array of TSpeciesBinary;     // Array of species binary data records.  Zero based array.
  end;
  TDifferenceMatrix = array of array of array of single;  // Three dimesional array for species vs. species for test numerical
  TDistanceMatrix = array of array of single;  // Two dimesional array for species vs. species for distancel
  TMatchingMatrix = array of array of array of boolean;   // Two dimesional array for species vs. species for test binary

var
  TestTableMeasurement: TTestTableMeasurement; {Test table for measurements}
  TestTableBinary: TTestTableBinary;           {Test table for binary data}
  rctSelectedCell: TGridRect;                  {Cell location holding error value}
  DifferenceMatrix: TDifferenceMatrix;         {Difference table for measerement data}
  MatchingMatrix: TMatchingMatrix;             {Matchind table for binary data}
  DistanceMatrix: TDistanceMatrix;

procedure ClearMeasurements;
procedure ClearBinaries;
procedure LoadMeasuremwntData(const MD: TStrings; var prgBar: TProgressBar; var strgrdTD: TStringGrid);
procedure LoadBinaryData(const BD: TStrings; var prgBar: TProgressBar; var strgrdTD: TStringGrid; const BinaryType: TBinaryDataType);
procedure BuildDifferenceMatrix(var prgBar: TProgressBar);
procedure BuildMeasurementDistanceMatrix(var prgBar: TProgressBar; var mmoRslts: TMemo);
procedure BuildMatchingMatrix(var prgBar: TProgressBar);
procedure BuildBinaryDistanceMatrix(var prgBar: TProgressBar; var mmoRslts: TMemo);
procedure BuildQMatrix(var strgrdQM: TStringGrid; const wrdDT: word);

implementation

// Clear data from measurement records
procedure ClearMeasurements;
begin
  with TestTableMeasurement do
  begin
    SpeciesTitle := '';
    SpeciesCount := 0;
    TestCount := 0;
    ErrorCount := 0;
    SetLength(TestTitle, 0);
    SetLength(Species, 0);
    SetLength(DifferenceMatrix, 0, 0, 0);
    SetLength(DistanceMatrix, 0, 0);
  end;
end;

// Clear data from binary data records
procedure ClearBinaries;
begin
  with TestTableBinary do
  begin
    SpeciesTitle := '';
    SpeciesCount := 0;
    TestCount := 0;
    ErrorCount := 0;
    SetLength(TestTitle, 0);
    SetLength(Species, 0);
    SetLength(MatchingMatrix, 0, 0, 0);
    SetLength(DistanceMatrix, 0, 0);
  end;
end;

{
Place data into the measrement records and data grid that was read into the memo field.  Place error
values in the grid with "<< >>" around the data and minus and number to obtain offending first digit.

Parameters:
   MD -> list of strings typically from a data file
   prgBar -> Progress bar to show percentge of completion
   strgrdTD -> String to show data results for errors
}
procedure LoadMeasuremwntData(const MD: TStrings; var prgBar: TProgressBar; var strgrdTD: TStringGrid);
var
  inxTitleLines,          // Index of title lines
  intTitlesCount,         // Count of title lines
  intComma,               // Position of first comma in string
  intCommaPos,            // Position of later commas in original full string
  int1stChar,             // Position of first character in data field
  intColErr,              // Column of first error in grid
  intRowErr,              // Row of first error in grid
  intLength,              // Length of string line data
  inxSpecies,             // Indices of species
  inxTests: integer;      // Indices of tests
  wrdError: word;         // Erroe code reported by Val procedure which is the invalid digit position
  strLineData: string;    // String for the cuurent line data read from memeo field
begin
  with TestTableMeasurement do
  begin
    strLineData := MD[0];              // Read memo string into string variable
    intLength := Length(strLineData);  // Assign string length
    if intLength < kFullTitleLength then
    begin
      SpeciesCount := MD.Count - 1;    // Count of memo with title line removed
      inxTitleLines := 1;              // Count of title line
      intTitlesCount := 1;
    end
    else
    begin
      SpeciesCount := MD.Count - 2;    // Count of memo with 2 title lines removed
      inxTitleLines := 2;              // Count of title lines
      intTitlesCount := 2;
    end;
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
    SetLength(Species, SpeciesCount);
    intComma := Pos(',', strLineData); // Find first comma
    intCommaPos := intComma;           // Assign comma accumulator
    int1stChar := 1;                   // Point to first field
    if intComma > 1 then               // Test to see if it has any commas
                                       //        v it1stCharPos=8
                                       //              v intCommaPos=14
    begin                              //  xxxxxx,yyyyyy,zzzzzzz,aaaaaaaa  <--- strLineData
                                       //  Copy--|      ^ intComma=7      | intCommaPos-int1stChar=6
                                       // |-------------------------------| End of string intLength=27
      SpeciesTitle := Copy(strLineData, int1stChar, intCommaPos - int1stChar);  // Assign species title field
      strgrdTD.Cells[0, 0] := SpeciesTitle;                                     // Put title in grid
      int1stChar := intComma + 1;                                               // Find start of next string field
      intComma := Pos(',', Copy(strLineData, int1stChar, intLength - intCommaPos)); // Find next comma
      intCommaPos := intCommaPos + intComma;                                        // Accumulate comma position to work with full string
      while inxTitleLines > 0 do
      begin
        while intComma > 0 do                                                         // Test for commas left in string
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
          strgrdTD.ColCount := TestCount + 1                                              // Extend grid one column
        end;
        if int1stChar <= intLength then                                                   // Test if int1stChar before end of sring
        begin
          TestTitle[inxTests] := Copy(strLineData, int1stChar, intLength - int1stChar + 1);  // Add last Test title in line
          strgrdTD.Cells[inxTests + 1, 0] := TestTitle[inxTests];                            // Add title to grid
        end;
        inxTitleLines := inxTitleLines - 1;
        if inxTitleLines > 0 then
        begin
          strLineData := MD[1];                                                             // Read 2nd memo string into string variable
          intLength := Length(strLineData);                                                 // Assign string length
          intComma := Pos(',', strLineData);                                                // Find first comma
          int1stChar := 1;                                                                  // Point to first field
          intCommaPos := intComma;                                                          // Assign comma accumulator
        end;
      end;
      prgBar.Max := strgrdTD.ColCount * strgrdTD.RowCount;
      prgBar.Position := strgrdTD.ColCount;
    end;
    try
      for inxSpecies := 0 to SpeciesCount - 1 do                                              // For next through species
        with Species[inxSpecies] do                                                           // Use Species array qualifier
        begin
          strLineData := MD[inxSpecies + intTitlesCount];                   // Read memo string into string variable
          intLength := Length(strLineData);                                 // Assign string length
          intComma := Pos(',', strLineData);                                // Find first comma
          intCommaPos := intComma;                                          // Assign comma accumulator
          int1stChar := 1;                                                  // Point to first field
          SpeciesName := Copy(strLineData, 1, intCommaPos - int1stChar);    // Assign species name field
          strgrdTD.Cells[0, inxSpecies + 1] := SpeciesName;                 // Assign species name to grid
          SetLength(Measurement, TestCount);
          inxTests := 0;                                                    // Set Test index to zero
          prgBar.Position := prgBar.Position + 1;
          int1stChar := intCommaPos + 1;                                    // Point to first field
          intComma := Pos(',', Copy(strLineData, int1stChar, intLength - intCommaPos));  // Find firsr comma
          intCommaPos := intCommaPos + intComma;
          while intComma > 0 do                                                          // Test for commas left in string
          begin
            if inxTests + 1 > TestCount then
            begin
              ErrorCount := ErrorCount + 1;                                                                                                                      // Increment error counter
              MessageDlg(IntToStr(TestCount) + ' Tests were expected and ' + IntToStr(inxTests + 1) + ' tests were processed for species ' + SpeciesName + '.', mtError, [mbOK], 0);
              break;
            end;
            Val(Copy(strLineData, int1stChar, intCommaPos - int1stChar), Measurement[inxTests], wrdError);  // Convert measurement from string to single
            if wrdError = 0 then
              strgrdTD.Cells[inxTests + 1, inxSpecies + 1] := Copy(strLineData, int1stChar, intCommaPos - int1stChar)  // Add measurement to grid.
            else
            begin
              ErrorCount := ErrorCount + 1;                                                                                                                        // Increment error counter
              strgrdTD.Cells[inxTests + 1, inxSpecies + 1] := '<<' + Copy(strLineData, int1stChar, intCommaPos - int1stChar) + '>>@' + IntToStr(wrdError);       // Add measurment to grid with error data.
              if ErrorCount = 1 then
              begin
                intColErr := inxTests + 1;
                intRowErr := inxSpecies + 1;
                rctSelectedCell.Left := intColErr;
                rctSelectedCell.Top := intRowErr;
                rctSelectedCell.Right := intColErr;
                rctSelectedCell.Bottom := intRowErr;
              end;
              Measurement[inxTests] := kInvalidData;                                                                                                             // Place invalid data in measurement data.
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
              MessageDlg(IntToStr(TestCount) + ' Tests were expected and ' + IntToStr(inxTests + 1) + ' tests were processed for species ' + SpeciesName + '.', mtError, [mbOK], 0);
            end
            else
            begin
              Val(Copy(strLineData,  int1stChar, intLength - int1stChar + 1), Measurement[inxTests], wrdError);                                                    // Convert measurement from string to single
              if wrdError = 0 then                                                                                                                                 // Test for invalid data
                strgrdTD.Cells[inxTests + 1, inxSpecies + 1] := Copy(strLineData, int1stChar, intLength - int1stChar + 1)
              else                                                                                                                                                 // Add measurment to grid
              begin
                ErrorCount := ErrorCount + 1;                                                                                                                      // Increment error counter
                strgrdTD.Cells[inxTests + 1, inxSpecies + 1] := '<<' + Copy(strLineData, int1stChar, intLength - int1stChar + 1) + '>>@' + IntToStr(wrdError);     // Add measurment to grid with error data.
                if ErrorCount = 1 then
                begin
                  intColErr := inxTests + 1;
                  intRowErr := inxSpecies + 1;
                  rctSelectedCell.Left := intColErr;
                  rctSelectedCell.Top := intRowErr;
                  rctSelectedCell.Right := intColErr;
                  rctSelectedCell.Bottom := intRowErr;
                end;
                Measurement[inxTests] := kInvalidData;                                                                                                             // Place invalid data in measurement data.
              end;
            end;
            prgBar.Position := prgBar.Position + 1;
          end;
          if inxTests + 1 < TestCount then
          begin
            ErrorCount := ErrorCount + 1;                                                                                                                      // Increment error counter
            MessageDlg(IntToStr(TestCount) + ' Tests were expected and ' + IntToStr(inxTests + 1) + ' tests were processed for species ' + SpeciesName + '.', mtError, [mbOK], 0);
          end;
        end;
    except
      on E:exception do
      begin
        ErrorCount := ErrorCount + 1;                    // Increment error counter
        MessageDlg(E.Message, mtError, [mbOK], 0);       //
      end;
    end;
    strgrdTD.AutoSizeColumns;
  end;
end;

{
Place data into the binary data records and data grid that was read into the memo field.  Place error
values in the grid with "** **" around the data.

Parameters:
   BD -> list of strings typically from a data file
   strgrdTD -> String to show data results for errors
   BinaryType -> Binary data type array
   prgBar -> Progress bar to show percentge of completion
}
procedure LoadBinaryData(const BD: TStrings; var prgBar: TProgressBar; var strgrdTD: TStringGrid; const BinaryType: TBinaryDataType);
var
  inxTitleLines,          // Index of title lines
  intTitlesCount,         // Count of title lines
  intComma,               // Position of first comma in string
  intCommaPos,            // Position of later commas in original full string
  int1stChar,             // Position of first character in data field
  intColErr,              // Column of first error in grid
  intRowErr,              // Row of first error in grid
  intLength,              // Length of string line data
  inxSpecies,             // Indices of species
  inxTests: integer;      // Indices of tests
  strLineData: string;    // String for the cuurent line data read from memo field
begin
  with TestTableBinary do
  begin
    strLineData := BD[0];              // Read memo string into string variable
    intLength := Length(strLineData);  // Assign string length
    if intLength < kFullTitleLength then
    begin
      SpeciesCount := BD.Count - 1;    // Count of memo with title line removed
      inxTitleLines := 1;              // Count of title line
      intTitlesCount := 1;
    end
    else
    begin
      SpeciesCount := BD.Count - 2;    // Count of memo with 2 title lines removed
      inxTitleLines := 2;              // Count of title lines
      intTitlesCount := 2;
    end;
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
    SetLength(Species, SpeciesCount);
    strLineData := BD[0];                 // Read memo string into string variable
    intLength := Length(strLineData);     // Set columns to 2 prior to column counting
    intComma := Pos(',', strLineData);    // Find first comma
    intCommaPos := intComma;              // Assign comma accumulator
    int1stChar := 1;                      // Point to first field
    while inxTitleLines > 0 do
    begin
      if intComma > 1 then                  // Test to see if it has any commas
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
        inxTitleLines := inxTitleLines - 1;
        if inxTitleLines > 0 then
        begin
          strLineData := BD[1];                                                             // Read 2nd memo string into string variable
          intLength := Length(strLineData);                                                 // Assign string length
          intComma := Pos(',', strLineData);                                                // Find first comma
          int1stChar := 1;                                                                  // Point to first field
          intCommaPos := intComma;                                                          // Assign comma accumulator
        end;
      end;
      prgBar.Max := strgrdTD.ColCount * strgrdTD.RowCount;
      prgBar.Position := strgrdTD.ColCount;
    end;
    for inxSpecies := 0 to SpeciesCount - 1 do                                              // For next through species
      with Species[inxSpecies] do
      begin
        strLineData := BD[inxSpecies + intTitlesCount];                                                  // Read memo string intostring variable
        intLength := Length(strLineData);
        intComma := Pos(',', strLineData);                                                  // Find first comma
        intCommaPos := intComma;                                                            // Assign comma accumulator
        int1stChar := 1;                                                                    // Point to first field position
        SpeciesName := Copy(strLineData, 1, intCommaPos - int1stChar);                      // Assign species name field
        strgrdTD.Cells[0, inxSpecies + 1] := SpeciesName;                                   // Assign species name to grid
        SetLength(Binary, TestCount);
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
            MessageDlg(IntToStr(TestCount) + ' Tests were expected and ' + IntToStr(inxTests + 1) + ' tests were processed for species ' + SpeciesName + '.', mtError, [mbOK], 0);
            break;
          end;
          if strLineData[int1stChar] in BinaryType then                                     // Test if data is valid
          begin
            Binary[inxTests] := strLineData[int1stChar];                                    // Assign data to binary result
            strgrdTD.Cells[inxTests + 1, inxSpecies + 1] := Binary[inxTests];               // Assign data to grid
          end
          else
          begin
            Binary[inxTests] := '*';                                                        // Assign error data to binary result
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
            end;
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
            MessageDlg(IntToStr(TestCount) + ' Tests were expected and ' + IntToStr(inxTests + 1) + ' tests were processed for species ' + SpeciesName + '.', mtError, [mbOK], 0);
          end
          else
          begin
            if strLineData[intLength] in BinaryType then                                       // Test if data is valid
            begin
              Binary[inxTests] := strLineData[intLength];                                     // Assign data to binary result
              strgrdTD.Cells[inxTests + 1, inxSpecies + 1] := Binary[inxTests];               // Assign data to grid
            end
            else
            begin
              Binary[inxTests] := '*';                                                        // Assign error data to binary result
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
              end;
            end;
          end;
        prgBar.Position := prgBar.Position + 1;
        if inxTests + 1 < TestCount then
        begin
          ErrorCount := ErrorCount + 1;                                   // Increment error counter
          MessageDlg(IntToStr(TestCount) + ' Tests were expected and ' + IntToStr(inxTests + 1) + ' tests were processed for species ' + SpeciesName + '.', mtError, [mbOK], 0);
        end;
      end;
    strgrdTD.AutoSizeColumns;
  end;
end;

{
Builds records from measurement table to create a difference table of speices vs. species
for each test.
}
procedure BuildDifferenceMatrix(var prgBar: TProgressBar);
var
  inxSpecies,             // Indices of species
  inxNextSpecies,         // Indices of different species
  inxTests: integer;      // Indices of tests

begin
  with TestTableMeasurement do
  begin
    prgBar.Position := 0;
    prgBar.Max := (SpeciesCount - 1) * (SpeciesCount - 1) * (TestCount - 1);
    SetLength(DifferenceMatrix, SpeciesCount - 1, SpeciesCount - 1, TestCount);
    for inxSpecies := 0 to SpeciesCount - 2 do
      for inxNextSpecies := inxSpecies + 1 to SpeciesCount - 1 do
        for inxTests := 0 to TestCount - 1 do
        begin
          DifferenceMatrix[inxSpecies, inxNextSpecies - 1, inxTests] := Abs(Species[inxSpecies].Measurement[inxTests] -
                                                                            Species[inxNextSpecies].Measurement[inxTests]);
          prgBar.Position := prgBar.Position + 1;
        end;
  end;
end;

{
Build distant matrix from averaging diffference matrix values.
}
procedure BuildMeasurementDistanceMatrix(var prgBar: TProgressBar; var mmoRslts: TMemo);
var
  inxSpecies,             // Indices of species
  inxNextSpecies,         // Indices of different species
  inxTests: integer;      // Indices of tests
  sngCount,               // Test count single precision
  sngSum: single;         // Sum of differences in a single number
  strWork: string;        // Working string to build memo line
begin
  with TestTableMeasurement do
  begin
    sngCount := TestCount;
    SetLength(DistanceMatrix, SpeciesCount - 1, SpeciesCount - 1);
    for inxSpecies := 0 to SpeciesCount - 2 do
      for inxNextSpecies := inxSpecies + 1 to SpeciesCount - 1 do
      begin
        sngSum := sngZero;
        for inxTests := 0 to TestCount - 1 do
        begin
          sngSum :=  sngSum + DifferenceMatrix[inxSpecies, inxNextSpecies - 1, inxTests];
          prgBar.Position := prgBar.Position + 1;
        end;
        DistanceMatrix[inxSpecies, inxNextSpecies - 1] := sngSum / sngCount;
      end;
    mmoRslts.Clear;
    for inxSpecies := 0 to SpeciesCount - 2 do
    begin
      strWork := Species[inxSpecies+ 1].SpeciesName + ',';
      for inxNextSpecies := 0 to inxSpecies do
      begin
        strWork := strWork + FloatToStrF(DistanceMatrix[inxNextSpecies, inxSpecies], ffFixed, 9, 3);
        if inxNextSpecies <= inxSpecies - 1 then
          strWork := strWork + ',';
         prgBar.Position := prgBar.Position + 1;
      end;
      mmoRslts.Lines.Add(strWork);
    end;
    strWork := Speciestitle + ',';
    for inxSpecies := 0 to SpeciesCount - 2 do
    begin
      strWork := strWork + Species[inxSpecies].SpeciesName;
      if inxSpecies < SpeciesCount - 2 then
        strWork := strWork + ',';
      prgBar.Position := prgBar.Position + 1;
    end;
    mmoRslts.Lines.Add(strWork);
  end;
end;

procedure BuildMatchingMatrix(var prgBar: TProgressBar);
var
  inxSpecies,             // Indices of species
  inxNextSpecies,         // Indices of different species
  inxTests: integer;      // Indices of tests

begin
  with TestTableBinary do
  begin
    prgBar.Position := 0;
    prgBar.Max := (SpeciesCount - 1) * (SpeciesCount - 1) * (TestCount - 1);
    SetLength(MatchingMatrix, SpeciesCount - 1, SpeciesCount - 1, TestCount);
    for inxSpecies := 0 to SpeciesCount - 2 do
      for inxNextSpecies := inxSpecies + 1 to SpeciesCount - 1 do
        for inxTests := 0 to TestCount - 1 do
        begin
          if Species[inxSpecies].Binary[inxTests] = Species[inxNextSpecies].Binary[inxTests] then
            MatchingMatrix[inxSpecies, inxNextSpecies - 1, inxTests] := True
          else
            MatchingMatrix[inxSpecies, inxNextSpecies - 1, inxTests] := False;
          prgBar.Position := prgBar.Position + 1;
        end;
  end;
end;

procedure BuildBinaryDistanceMatrix(var prgBar: TProgressBar; var mmoRslts: TMemo);
var
  inxSpecies,             // Indices of species
  inxNextSpecies,         // Indices of different species
  inxTests: integer;      // Indices of tests
  sngCount,               // Test count single precision
  sngSum: single;         // Sum of differences in a single number
  strWork: string;        // Working string to build memo line
begin
  with TestTableBinary do
  begin
    sngCount := TestCount;
    SetLength(DistanceMatrix, SpeciesCount, SpeciesCount);
    for inxSpecies := 1 to SpeciesCount - 1 do
      for inxNextSpecies := inxSpecies + 1 to SpeciesCount do
      begin
        sngSum := sngZero;
        for inxTests := 0 to TestCount - 1 do
        begin
          if MatchingMatrix[inxSpecies, inxNextSpecies - 1, inxTests] then
            sngSum :=  sngSum + 1.0;
          prgBar.Position := prgBar.Position + 1;
        end;
        DistanceMatrix[inxSpecies, inxNextSpecies - 1] := sngSum / sngCount;
      end;
    mmoRslts.Clear;
    for inxSpecies := 1 to SpeciesCount - 1 do
    begin
      strWork := Species[inxSpecies + 1].SpeciesName + ',';
      for inxNextSpecies := 0 to inxSpecies do
      begin
        strWork := strWork + FloatToStrF(DistanceMatrix[inxNextSpecies, inxSpecies], ffFixed, 9, 3);
        if inxNextSpecies <= inxSpecies - 1 then
          strWork := strWork + ',';
         prgBar.Position := prgBar.Position + 1;
      end;
      mmoRslts.Lines.Add(strWork);
    end;
    strWork := Speciestitle + ',';
    for inxSpecies := 0 to SpeciesCount - 2 do
    begin
      strWork := strWork + Species[inxSpecies].SpeciesName;
      if inxSpecies < SpeciesCount - 2 then
        strWork := strWork + ',';
      prgBar.Position := prgBar.Position + 1;
    end;
    mmoRslts.Lines.Add(strWork);
  end;
end;

procedure BuildQMatrix(var strgrdQM: TStringGrid; const wrdDT: word);
var
  inx,
  iny: integer;
begin
  case wrdDT of
    0..1: with TestTableBinary do
      begin
        strgrdQM.RowCount := SpeciesCount + 1;
        strgrdQM.ColCount := SpeciesCount + 1;
        strgrdQM.Cells[0, 0] := SpeciesTitle;
        for inx := 1 to SpeciesCount do
          strgrdQM.Cells[inx, 0] := Species[inx - 1].SpeciesName;
        for iny := 1 to SpeciesCount do
          strgrdQM.Cells[0, iny] := Species[iny - 1].SpeciesName;
      end;
    2: with TestTableMeasurement do
      begin
        strgrdQM.RowCount := SpeciesCount + 1;
        strgrdQM.ColCount := SpeciesCount + 1;
        strgrdQM.Cells[0, 0] :=  TestTableMeasurement.SpeciesTitle;
        for inx := 1 to SpeciesCount do
          strgrdQM.Cells[inx, 0] := Species[inx - 1].SpeciesName;
        for iny := 1 to SpeciesCount do
          strgrdQM.Cells[0, iny] := Species[iny - 1].SpeciesName;
      end;
  else
    strgrdQM.Cells[0, 0] :=  'Error';
    strgrdQM.RowCount := 2;
    strgrdQM.ColCount := 2;
  end;
  strgrdQM.AutoSizeColumns;
end;

end.
