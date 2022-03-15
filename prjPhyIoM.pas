program prjPhyIoM;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, tachartlazaruspkg, lazcontrols,
  printer4lazarus, modPhyIoM, modDataTypes, modTestData, modSelectType, modRunOptions,
  modTreeGraph, modCalcErrs, modTreeView, modAbout, modPrintTree, 
  modSelectNewRoot, modInstructions, modFormBckgrd;

{$R *.res}

begin
  Application.Title:='PhyloM Application';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmPhyIoM, frmPhyIoM);
  Application.CreateForm(TdlgSelectFileType, dlgSelectFileType);
  Application.CreateForm(TdlgRunOptions, dlgRunOptions);
  Application.CreateForm(TfrmTreeGraph, frmTreeGraph);
  Application.CreateForm(TfrmCalculatedErrors, frmCalculatedErrors);
  Application.CreateForm(TfrmTreeListDown, frmTreeListDown);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.CreateForm(TfrmPrintTree, frmPrintTree);
  Application.CreateForm(TfrmSelectNewRoot, frmSelectNewRoot);
  Application.CreateForm(TfrmInstructions, frmInstructions);
  Application.Run;
end.

