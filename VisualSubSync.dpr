program VisualSubSync;

{%File 'GlobalUnit.pas'}

uses
  //FastMM4,
  Forms,
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
  RGBHSLColorUnit in 'RGBHSLColorUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'VisualSubSync';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TProjectForm, ProjectForm);
  Application.CreateForm(TFindForm, FindForm);
  Application.CreateForm(TErrorReportForm, ErrorReportForm);
  Application.CreateForm(TSuggestionForm, SuggestionForm);
  Application.CreateForm(TPreferencesForm, PreferencesForm);
  Application.CreateForm(TDetachedVideoForm, DetachedVideoForm);
  Application.CreateForm(TStyleForm, StyleForm);
  MainForm.LoadSettings;
  Application.ProcessMessages;  
  MainForm.FinishLoadSettings;
  if (ParamCount = 1) then
  begin
    MainForm.LoadProject(ParamStr(1));
  end;
  Application.Run;
end.
