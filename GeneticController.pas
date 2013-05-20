unit GeneticController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TAFuncSeries,
  TATransformations, TATools, TASources, TAMultiSeries, TASeries, Forms,
  Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ActnList, ComCtrls,
  GeneticBrain, types, TACustomSource;

type

  { TFormGenetic }

  TFormGenetic = class(TForm)
    ButtonTimerIterations: TButton;
    ButtonStart: TButton;
    ButtonGenerateStartPopulation: TButton;
    Chart:     TChart;
    ChartFuncSeriesAimFunction: TFuncSeries;
    ChartLineSeriesChromosomes: TLineSeries;
    ChartToolset: TChartToolset;
    ChartToolsetPanDragTool1: TPanDragTool;
    ChartToolsetZoomMouseWheelTool1: TZoomMouseWheelTool;
    ComboBoxFindingType: TComboBox;
    ComboBoxSampling: TComboBox;
    ComboBoxMutation: TComboBox;
    ComboBoxCrossingover: TComboBox;
    ComboBoxSelection: TComboBox;
    ComboBoxPopulationStrategy: TComboBox;
    GroupBoxOperators: TGroupBox;
    GroupBoxStartPopulation: TGroupBox;
    GroupBoxOptions: TGroupBox;
    GroupBoxVisualisation: TGroupBox;
    LabelDNKLength: TLabel;
    LabelResult: TLabel;
    LabelStartPopulationSize: TLabel;
    LabelPerformNIterations: TLabel;
    Splitter1: TSplitter;
    TimerIterations: TTimer;
    TrackBarDNKLength: TTrackBar;
    TrackBarCrossingoverRate: TTrackBar;
    TrackBarMutationRate: TTrackBar;
    TrackBarPopulationSize: TTrackBar;
    TrackBarIterations: TTrackBar;
    SourceChromosomes: TUserDefinedChartSource;
    procedure ButtonTimerIterationsClick(Sender: TObject);
    procedure ButtonGenerateStartPopulationClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ChartFuncSeriesAimFunctionCalculate(const AX: double; out AY: double);
    procedure ComboBoxFindingTypeChange(Sender: TObject);
    procedure ComboBoxCrossingoverChange(Sender: TObject);
    procedure ComboBoxMutationChange(Sender: TObject);
    procedure ComboBoxPopulationStrategyChange(Sender: TObject);
    procedure ComboBoxSamplingChange(Sender: TObject);
    procedure ComboBoxSelectionChange(Sender: TObject);
    procedure EditIterationsCountEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
    GraphicScale: real;
    GraphicOrigin: TPoint;
    StartMousePoint: TPoint;
    CanDrag: boolean;
    procedure updateUI;
  public
    { public declarations }
  end;

var
  FormGenetic: TFormGenetic;

implementation

uses Math;

{$R *.lfm}

function rAimFunction(x: real): real;
begin
  Result := x ** 2;
end;

function AimFunction(x: longint): longint;
begin
  Result := round(rAimFunction(x));
end;

{ TFormGenetic }

procedure TFormGenetic.FormCreate(Sender: TObject);
var
  TempInterval: TInterval;
begin
  Organysm := COrganysm.Create;
  Organysm.BestResultType := BRT_MAX;
  Organysm.IterationCount := 10;
  Organysm.PopulationCount := 10;
  Organysm.SamplingType := SAT_PROPORTIONAL;
  Organysm.SelectionType := ST_RANDOM;
  Organysm.MutationType := MT_CHANGING_GOLDEN_SEPARATION;
  Organysm.StartPopulationStrategy := SPS_DROBOVIK;
  Organysm.CrossingoverType := CT_STANDARD_ONE_POINT;
  Organysm.CrossingoverRate := 0.7;
  Organysm.MutationRate := 0.2;
  Organysm.DnkLength := 8;
  TempInterval.IStart := 0;
  TempInterval.IEnd := 3;
  Organysm.Interval := TempInterval;
  Organysm.AimFunction := @rAimFunction;
  Chart.AxisList.BottomAxis.Range.Min := Organysm.Interval.IStart;
  Chart.AxisList.BottomAxis.Range.Max := Organysm.Interval.IEnd;
end;

procedure TFormGenetic.FormShow(Sender: TObject);
begin
  updateUI;
end;

procedure TFormGenetic.TimerIterationsTimer(Sender: TObject);
begin
  Organysm.NextIteration;
  updateUI;
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
end;

procedure TFormGenetic.TrackBarIterationsChange(Sender: TObject);
begin
  LabelPerformNIterations.Caption :=
    'Выполнить ' + IntToStr(TrackBarIterations.Position) + ' итераций';
end;

procedure TFormGenetic.TrackBarMutationRateChange(Sender: TObject);
begin
  Organysm.MutationRate := TrackBarMutationRate.Position / 100;
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

procedure TFormGenetic.updateUI;
var
  i: integer;
begin
  SourceChromosomes.BeginUpdate;
  LabelResult.Caption := 'Лучший результат: ' +
    FloatToStr(Organysm.GetBest);
  SourceChromosomes.EndUpdate;
end;

procedure TFormGenetic.EditIterationsCountEditingDone(Sender: TObject);
begin
  Organysm.IterationCount := StrToInt(TEdit(Sender).Text);
end;

procedure TFormGenetic.ButtonStartClick(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to TrackBarIterations.Position do
    Organysm.NextIteration;
  updateUI;
end;

procedure TFormGenetic.ChartFuncSeriesAimFunctionCalculate(const AX: double;
  out AY: double);
begin
  AY := rAimFunction(AX);
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
  case ComboBoxPopulationStrategy.ItemIndex of
    0: Organysm.StartPopulationStrategy := SPS_DROBOVIK;
    1: Organysm.StartPopulationStrategy := SPS_FOCUS;
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

procedure TFormGenetic.ButtonGenerateStartPopulationClick(Sender: TObject);
begin
  Organysm.GeneratePopulation;
  updateUI;
  SourceChromosomes.PointsNumber := TrackBarPopulationSize.Position;
  ButtonStart.Enabled := True;
  ButtonTimerIterations.Enabled := True;
end;

procedure TFormGenetic.ButtonTimerIterationsClick(Sender: TObject);
begin
  TimerIterations.Enabled := not TimerIterations.Enabled;
  if TimerIterations.Enabled then
    ButtonTimerIterations.Caption :=
      'Остановка непрерывных итераций'
  else
    ButtonTimerIterations.Caption :=
      'Запуск непрерывных итераций';
end;

end.
