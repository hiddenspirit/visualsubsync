program VisualSubSync;

{%File 'GlobalUnit.pas'}

uses
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
  DetachedVideoFormUnit in 'DetachedVideoFormUnit.pas' {DetachedVideoForm};

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
  MainForm.LoadSettings;
  Application.ProcessMessages;  
  MainForm.FinishLoadSettings;
  if (ParamCount = 1) then
  begin
    MainForm.LoadProject(ParamStr(1));
  end;
  Application.Run;
end.
