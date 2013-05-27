unit GeneticController;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, TAGraph, TAFuncSeries, TATools, TASources, TASeries,
  Forms, Graphics, StdCtrls, ExtCtrls, ComCtrls, PopupNotifier,
  GeneticBrain, TACustomSource, Classes, Controls, ActnList, types;

type

  { TFormGenetic }

  TFormGenetic = class(TForm)
    ActionGeneratePopulation: TAction;
    ActionIterate: TAction;
    ActionStart: TAction;
    ActionUpdateUI: TAction;
    ActionAddToLog: TAction;
    ActionList: TActionList;
    ButtonAddToLog: TButton;
    ButtonExport: TButton;
    ButtonClearLog: TButton;
    ButtonTimerIterations: TButton;
    ButtonStart: TButton;
    ButtonGenerateStartPopulation: TButton;
    Chart:     TChart;
    ChartFuncSeriesAimFunction: TFuncSeries;
    ChartLineSeriesFocusPoint: TLineSeries;
    ChartLineSeriesChromosomes: TLineSeries;
    ChartToolset: TChartToolset;
    ChartToolsetDataPointClickTool: TDataPointClickTool;
    ChartToolsetPanDragTool1: TPanDragTool;
    ChartToolsetZoomMouseWheelTool1: TZoomMouseWheelTool;
    ComboBoxFindingType: TComboBox;
    ComboBoxSampling: TComboBox;
    ComboBoxMutation: TComboBox;
    ComboBoxCrossingover: TComboBox;
    ComboBoxSelection: TComboBox;
    ComboBoxPopulationStrategy: TComboBox;
    GroupBoxTools: TGroupBox;
    GroupBoxLogOptions: TGroupBox;
    GroupBoxOperators: TGroupBox;
    GroupBoxStartPopulation: TGroupBox;
    GroupBoxOptions: TGroupBox;
    GroupBoxVisualisation: TGroupBox;
    LabelCrossingover: TLabel;
    LabelMutation: TLabel;
    LabelDNKLength: TLabel;
    LabelStartPopulationSize: TLabel;
    LabelPerformNIterations: TLabel;
    PageControl: TPageControl;
    PopupNotifierFocus: TPopupNotifier;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    TabSheetMain: TTabSheet;
    TabSheetLog: TTabSheet;
    TimerIterations: TTimer;
    TrackBarDNKLength: TTrackBar;
    TrackBarCrossingoverRate: TTrackBar;
    TrackBarMutationRate: TTrackBar;
    TrackBarPopulationSize: TTrackBar;
    TrackBarIterations: TTrackBar;
    SourceChromosomes: TUserDefinedChartSource;
    SourceFocusPoint: TUserDefinedChartSource;
    TreeViewLog: TTreeView;
    procedure ActionAddToLogExecute(Sender: TObject);
    procedure ActionGeneratePopulationExecute(Sender: TObject);
    procedure ActionIterateExecute(Sender: TObject);
    procedure ActionStartExecute(Sender: TObject);
    procedure ActionUpdateUIExecute(Sender: TObject);
    procedure ButtonClearLogClick(Sender: TObject);
    procedure ChartClick(Sender: TObject);
    procedure ChartFuncSeriesAimFunctionCalculate(const AX: double; out AY: double);
    procedure ChartToolsetDataPointClickToolPointClick(ATool: TChartTool;
      APoint: TPoint);
    procedure ComboBoxFindingTypeChange(Sender: TObject);
    procedure ComboBoxCrossingoverChange(Sender: TObject);
    procedure ComboBoxMutationChange(Sender: TObject);
    procedure ComboBoxPopulationStrategyChange(Sender: TObject);
    procedure ComboBoxSamplingChange(Sender: TObject);
    procedure ComboBoxSelectionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SourceFocusPointGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: integer; var AItem: TChartDataItem);
    procedure TimerIterationsTimer(Sender: TObject);
    procedure TrackBarDNKLengthChange(Sender: TObject);
    procedure TrackBarCrossingoverRateChange(Sender: TObject);
    procedure TrackBarIterationsChange(Sender: TObject);
    procedure TrackBarMutationRateChange(Sender: TObject);
    procedure TrackBarPopulationSizeChange(Sender: TObject);
    procedure SourceChromosomesGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: integer; var AItem: TChartDataItem);
  private
    { private declarations }
    Organysm: COrganysm;
    _CanAddFocusPoint: boolean;
    procedure SetCanAddFocusPoint(a: boolean);
    property CanAddFocusPoint: boolean read _CanAddFocusPoint write SetCanAddFocusPoint;
  public
    { public declarations }
  end;

var
  FormGenetic: TFormGenetic;

implementation

uses Math;

{$R *.lfm}

function AimFunction(x: real): real;
begin
  Result := x;
end;

{ TFormGenetic }

procedure TFormGenetic.FormCreate(Sender: TObject);
var
  TempInterval: TInterval;
begin
  Organysm := COrganysm.Create;
  Organysm.BestResultType := BRT_MAX;
  Organysm.PopulationCount := 10;
  Organysm.SamplingType := SAT_PROPORTIONAL;
  Organysm.SelectionType := ST_RANDOM;
  Organysm.MutationType := MT_CHANGING_GOLDEN_SEPARATION;
  Organysm.StartPopulationStrategy := SPS_SHOTGUN;
  Organysm.CrossingoverType := CT_STANDARD_ONE_POINT;
  Organysm.CrossingoverRate := 0.7;
  Organysm.MutationRate := 0.2;
  Organysm.DnkLength := 8;
  TempInterval.IStart := 0;
  TempInterval.IEnd := 3;
  Organysm.Interval := TempInterval;
  Organysm.AimFunction := @AimFunction;
  Chart.AxisList.BottomAxis.Range.Min := Organysm.Interval.IStart;
  Chart.AxisList.BottomAxis.Range.Max := Organysm.Interval.IEnd;
  CanAddFocusPoint := False;
  ActionIterate.Enabled := False;
  ActionStart.Enabled := False;
end;

procedure TFormGenetic.SourceFocusPointGetChartDataItem(
  ASource: TUserDefinedChartSource;
  AIndex: integer; var AItem: TChartDataItem);
begin
  AItem.Text := 'Точка фокусировки';
  AItem.Color := clRed;
  AItem.X := Organysm.FocusPoint;
  AItem.Y := AimFunction(AItem.X);
end;

procedure TFormGenetic.TimerIterationsTimer(Sender: TObject);
begin
  ActionStart.Execute;
end;

procedure TFormGenetic.TrackBarDNKLengthChange(Sender: TObject);
begin
  Organysm.DnkLength := TrackBarDNKLength.Position;
  LabelDNKLength.Caption := 'Длина хромосомы: ' +
    IntToStr(Organysm.DnkLength);
end;

procedure TFormGenetic.TrackBarCrossingoverRateChange(Sender: TObject);
begin
  Organysm.CrossingoverRate := TrackBarCrossingoverRate.Position / 100;
  LabelCrossingover.Caption :=
    'Вероятность кроссинговера: ' +
    IntToStr(TrackBarCrossingoverRate.Position) + '%';
end;

procedure TFormGenetic.TrackBarIterationsChange(Sender: TObject);
begin
  LabelPerformNIterations.Caption :=
    'Выполнить ' + IntToStr(TrackBarIterations.Position) + ' итераций';
end;

procedure TFormGenetic.TrackBarMutationRateChange(Sender: TObject);
begin
  Organysm.MutationRate := TrackBarMutationRate.Position / 100;
  LabelMutation.Caption :=
    'Вероятность мутации: ' +
    IntToStr(TrackBarMutationRate.Position) + '%';
end;

procedure TFormGenetic.TrackBarPopulationSizeChange(Sender: TObject);
begin
  LabelStartPopulationSize.Caption :=
    'Размер стартовой популяции: ' +
    IntToStr(TrackBarPopulationSize.Position);
  Organysm.PopulationCount := TrackBarPopulationSize.Position;
  SourceChromosomes.PointsNumber := TrackBarPopulationSize.Position;
end;

procedure TFormGenetic.SourceChromosomesGetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: integer; var AItem: TChartDataItem);
begin
  if High(Organysm.Population) >= AIndex then
  begin
    AItem.X := (Organysm.Interval.IStart + Organysm.Population[AIndex].GetReal *
      Organysm.Interval.Width);
    AItem.Y := Organysm.Population[AIndex].AimFunctionResult;
    AItem.Color := clBlue;
  end;
end;

procedure TFormGenetic.SetCanAddFocusPoint(a: boolean);
begin
  _CanAddFocusPoint := a;
end;



procedure TFormGenetic.ChartClick(Sender: TObject);
begin
  if Organysm.StartPopulationStrategy = SPS_FOCUS then
    CanAddFocusPoint := not CanAddFocusPoint;
end;

procedure TFormGenetic.ChartFuncSeriesAimFunctionCalculate(const AX: double;
  out AY: double);
begin
  AY := AimFunction(AX);
end;

procedure TFormGenetic.ChartToolsetDataPointClickToolPointClick(ATool: TChartTool;
  APoint: TPoint);
begin
  if CanAddFocusPoint then
    with ATool as TDataPointClickTool do
    begin
      SourceFocusPoint.BeginUpdate;
      Organysm.FocusPoint := (APoint.x / Chart.Width) * Organysm.Interval.Width;
      StatusBar.Panels.Items[1].Text := FloatToStr(Organysm.FocusPoint);
      ChartLineSeriesFocusPoint.Active := True;
      SourceFocusPoint.EndUpdate;
    end;
end;

procedure TFormGenetic.ComboBoxFindingTypeChange(Sender: TObject);
begin
  case ComboBoxFindingType.ItemIndex of
    0: Organysm.BestResultType := BRT_MAX;
    1: Organysm.BestResultType := BRT_MIN;
  end;
end;

procedure TFormGenetic.ComboBoxCrossingoverChange(Sender: TObject);
begin
  case ComboBoxCrossingover.ItemIndex of
    0: Organysm.CrossingoverType := CT_STANDARD_ONE_POINT;
    1: Organysm.CrossingoverType := CT_PARTLY_APROPRIATE_ONE_POINT;
    2: Organysm.CrossingoverType := CT_GOLDEN_SEPARATION;
  end;
end;

procedure TFormGenetic.ComboBoxMutationChange(Sender: TObject);
begin
  case ComboBoxMutation.ItemIndex of
    0: Organysm.MutationType := MT_CHANGING_GOLDEN_SEPARATION;
    1: Organysm.MutationType := MT_INVERSION;
  end;
end;

procedure TFormGenetic.ComboBoxPopulationStrategyChange(Sender: TObject);
begin
  CanAddFocusPoint := False;
  case ComboBoxPopulationStrategy.ItemIndex of
    0: Organysm.StartPopulationStrategy := SPS_SHOTGUN;
    1:
    begin
      Organysm.StartPopulationStrategy := SPS_FOCUS;
      CanAddFocusPoint := True;
    end;
  end;
end;

procedure TFormGenetic.ComboBoxSamplingChange(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: Organysm.SamplingType := SAT_PROPORTIONAL;
  end;
end;

procedure TFormGenetic.ComboBoxSelectionChange(Sender: TObject);
begin
  case ComboBoxSelection.ItemIndex of
    0: Organysm.SelectionType := ST_RANDOM;
    1: Organysm.SelectionType := ST_ELITE;
  end;
end;

procedure TFormGenetic.ButtonClearLogClick(Sender: TObject);
begin
  TreeViewLog.Items.Clear;
end;

procedure TFormGenetic.ActionAddToLogExecute(Sender: TObject);
var
  Root, CurrNode: TTreeNode;
  i:    integer;
  Temp: string;
begin
  Root := TreeViewLog.Items.Add(nil, 'Поколение ' +
    IntToStr(Organysm.GenerationNumber));
  for i := 0 to High(Organysm.Population) do
    with Organysm.Population[i] do
    begin
      Temp := FloatToStr(Organysm.Interval.IStart + Organysm.Interval.Width * GetReal);
      CurrNode := TreeViewLog.Items.AddChild(Root, 'DNK = ' +
        IntToStr(Organysm.Population[i].GetLongword));
      TreeViewLog.Items.AddChild(CurrNode, ' x = ' + Temp);
      TreeViewLog.Items.AddChild(CurrNode, ' y = ' +
        FloatToStr(Organysm.Population[i].AimFunctionResult));
    end;
end;

procedure TFormGenetic.ActionGeneratePopulationExecute(Sender: TObject);
begin
  TreeViewLog.Items.Clear;
  Organysm.GeneratePopulation;
  ActionUpdateUI.Execute;
  ActionAddToLog.Execute;
  SourceChromosomes.PointsNumber := TrackBarPopulationSize.Position;
  ActionStart.Enabled := True;
  ActionIterate.Enabled := True;
end;

procedure TFormGenetic.ActionIterateExecute(Sender: TObject);
begin
  TimerIterations.Enabled := not TimerIterations.Enabled;
  if TimerIterations.Enabled then
    ActionIterate.Caption :=
      'Остановка непрерывных итераций'
  else
    ActionIterate.Caption :=
      'Запуск непрерывных итераций';
end;

procedure TFormGenetic.ActionStartExecute(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to TrackBarIterations.Position do
  begin
    Organysm.NextIteration;
    ActionAddToLog.Execute;
  end;
  ActionUpdateUI.Execute;
end;

procedure TFormGenetic.ActionUpdateUIExecute(Sender: TObject);
begin
  SourceChromosomes.BeginUpdate;
  with StatusBar.Panels.Items[0] do
    Text := 'Лучший результат: ' + FloatToStr(Organysm.GetBest);
  SourceChromosomes.EndUpdate;
end;

end.
