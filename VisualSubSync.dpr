program VisualSubSync;

uses
  Windows,
  Forms,
  TntSystem,
  main in 'main.pas' {MainForm},
  WAVDisplayerUnit in 'WAVDisplayerUnit.pas',
  WAVFileUnit in 'WAVFileUnit.pas',
  Renderer in 'Renderer.pas',
  MiniScrollBarUnit in 'MiniScrollBarUnit.pas',
  MiscToolsUnit in 'MiscToolsUnit.pas',
  ProjectUnit in 'ProjectUnit.pas' {ProjectForm},
  WAVExtractFormUnit in 'WAVExtractFormUnit.pas' {ExtractWAVForm},
  FindFormUnit in 'FindFormUnit.pas' {FindForm},
  PeakCreationProgressFormUnit in 'PeakCreationProgressFormUnit.pas' {PeakCreationProgressForm},
  AboutFormUnit in 'AboutFormUnit.pas' {AboutForm},
  ServerUnit in 'ServerUnit.pas',
  DynamicPageProcessorUnit in 'DynamicPageProcessorUnit.pas',
  ErrorReportFormUnit in 'ErrorReportFormUnit.pas' {ErrorReportForm},
  DelayFormUnit in 'DelayFormUnit.pas' {DelayForm},
  PageProcessorUnit in 'PageProcessorUnit.pas',
  SubStructUnit in 'SubStructUnit.pas',
  SuggestionFormUnit in 'SuggestionFormUnit.pas' {SuggestionForm},
  GotoFormUnit in 'GotoFormUnit.pas' {GotoForm},
  PreferencesFormUnit in 'PreferencesFormUnit.pas' {PreferencesForm},
  MRUListUnit in 'MRUListUnit.pas',
  VerticalScalingFormUnit in 'VerticalScalingFormUnit.pas' {VerticalScalingForm},
  DetachedVideoFormUnit in 'DetachedVideoFormUnit.pas' {DetachedVideoForm},
  JavaScriptPluginUnit in 'JavaScriptPluginUnit.pas',
  LogWindowFormUnit in 'LogWindowFormUnit.pas' {LogForm},
  CursorManager in 'CursorManager.pas',
  tom_TLB in 'tom_TLB.pas',
  StyleFormUnit in 'StyleFormUnit.pas' {StyleForm},
  SSAParserUnit in 'SSAParserUnit.pas',
  DirectVobsubInterface in 'DirectVobsubInterface.pas',
  TntRichEditCustomUndoUnit in 'TntRichEditCustomUndoUnit.pas',
  UndoableTaskUnit in 'UndoableTaskUnit.pas',
  UndoableSubTaskUnit in 'UndoableSubTaskUnit.pas',
  MatroskaHelper in 'MatroskaHelper.pas',
  SceneChangeUnit in 'SceneChangeUnit.pas',
  GlobalUnit in 'GlobalUnit.pas',
  SRTParserUnit in 'SRTParserUnit.pas',
  SilentZoneFormUnit in 'SilentZoneFormUnit.pas' {SilentZoneForm},
  LibHunspellUnit in 'LibHunspellUnit.pas',
  BgThreadTaskUnit in 'BgThreadTaskUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  // Fix ANSI handling (using the thread code page doesn't work correctly on some system)
  // e.g.: Tnt is broken when loading an ANSI file through TTntStringList.LoadFromFile is doesn't use the righ code page
  TntSystem.InstallTntSystemUpdates([tsFixImplicitCodePage]);
  Application.Title := 'VisualSubSync';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TProjectForm, ProjectForm);
  Application.CreateForm(TFindForm, FindForm);
  Application.CreateForm(TErrorReportForm, ErrorReportForm);
  Application.CreateForm(TSuggestionForm, SuggestionForm);
  Application.CreateForm(TDetachedVideoForm, DetachedVideoForm);
  MainForm.InitGeneralJSPlugin;
  MainForm.LoadSettings;
  MainForm.FinishLoadSettings;
  Application.ProcessMessages;
  MainForm.ProcessParams;
  Application.Run;
end.
