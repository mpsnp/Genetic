unit GeneticController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ActnList, ComCtrls, GeneticBrain, types;

type

  { TFormGenetic }

  TFormGenetic = class(TForm)
    ButtonTimerIterations: TButton;
    ButtonStart: TButton;
    ButtonGenerateStartPopulation: TButton;
    ComboBoxSampling: TComboBox;
    ComboBoxMutation: TComboBox;
    ComboBoxCrossingover: TComboBox;
    ComboBoxSelection: TComboBox;
    ComboBoxPopulationStrategy: TComboBox;
    GroupBoxOperators: TGroupBox;
    GroupBoxStartPopulation: TGroupBox;
    GroupBoxOptions: TGroupBox;
    GroupBoxVisualisation: TGroupBox;
    ImageGraphic: TImage;
    LabelResult: TLabel;
    LabelStartPopulationSize: TLabel;
    LabelPerformNIterations: TLabel;
    MemoNewGeneration: TMemo;
    MemoChromosomes: TMemo;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Timer1:    TTimer;
    TrackBarCrossingoverRate: TTrackBar;
    TrackBarMutationRate: TTrackBar;
    TrackBarPopulationSize: TTrackBar;
    TrackBarIterations: TTrackBar;
    procedure ButtonTimerIterationsClick(Sender: TObject);
    procedure ButtonGenerateStartPopulationClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ComboBoxCrossingoverChange(Sender: TObject);
    procedure ComboBoxMutationChange(Sender: TObject);
    procedure ComboBoxPopulationStrategyChange(Sender: TObject);
    procedure ComboBoxSamplingChange(Sender: TObject);
    procedure ComboBoxSelectionChange(Sender: TObject);
    procedure EditIterationsCountEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImageGraphicChangeBounds(Sender: TObject);
    procedure ImageGraphicMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ImageGraphicMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure ImageGraphicMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ImageGraphicMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBarCrossingoverRateChange(Sender: TObject);
    procedure TrackBarIterationsChange(Sender: TObject);
    procedure TrackBarMutationRateChange(Sender: TObject);
    procedure TrackBarPopulationSizeChange(Sender: TObject);
  private
    { private declarations }
    Organysm: COrganysm;
    GraphicScale: real;
    GraphicOrigin: TPoint;
    StartMousePoint: TPoint;
    CanDrag: boolean;
    procedure repaintGraph;
    procedure updateUI;
  public
    { public declarations }
  end;

var
  FormGenetic: TFormGenetic;

implementation

{$R *.lfm}

function rAimFunction(x: real): real;
begin
  Result := -x * x + 4 * x;
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
  TempInterval.IStart := 1;
  TempInterval.IEnd := 12;
  Organysm.Interval := TempInterval;
  Organysm.AimFunction := @AimFunction;
  repaintGraph;
  GraphicScale := 10;
  GraphicOrigin.x := 20;
  GraphicOrigin.y := 20;
  ImageGraphic.Picture.Bitmap.Width := ImageGraphic.Width;
  ImageGraphic.Picture.Bitmap.Height := ImageGraphic.Height;
end;

procedure TFormGenetic.FormShow(Sender: TObject);
begin
  updateUI;
end;

procedure TFormGenetic.ImageGraphicChangeBounds(Sender: TObject);
begin
  ImageGraphic.Picture.Bitmap.Width := ImageGraphic.Width;
  ImageGraphic.Picture.Bitmap.Height := ImageGraphic.Height;
  repaintGraph;
end;

procedure TFormGenetic.ImageGraphicMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    StartMousePoint.x := x;
    StartMousePoint.y := y;
    CanDrag := True;
  end;
end;

procedure TFormGenetic.ImageGraphicMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
  if CanDrag then
  begin
    GraphicOrigin.x -= StartMousePoint.x - x;
    GraphicOrigin.y += StartMousePoint.y - y;
    StartMousePoint.x := x;
    StartMousePoint.y := y;
    repaintGraph;
  end;
end;

procedure TFormGenetic.ImageGraphicMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  CanDrag := False;
end;

procedure TFormGenetic.ImageGraphicMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  GraphicScale += WheelDelta / 120 * 0.5;
  repaintGraph;
end;

procedure TFormGenetic.Timer1Timer(Sender: TObject);
begin
  Organysm.NextIteration;
  updateUI;
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
end;

procedure TFormGenetic.repaintGraph;
var
  i: real;
  ii, aWidth, aHeight: integer;
  TempPoint: TPoint;

  function GetLocalCoords(x, y: real): TPoint;
  begin
    Result.x := round(GraphicScale * x + GraphicOrigin.x);
    Result.y := round(GraphicScale * y + GraphicOrigin.y);
    Result.x += aWidth div 2;
    Result.y += aHeight div 2;
    Result.y := aHeight - Result.y;
  end;

begin
  aWidth := ImageGraphic.Picture.Bitmap.Width;
  aHeight := ImageGraphic.Picture.Bitmap.Height;
  with ImageGraphic.Picture.Bitmap do
  begin
    Canvas.Brush.Color := clWindow;
    Canvas.FillRect(0, 0, Width, Height);
    Canvas.Pen.Color := clRed;
    Canvas.Line(GetLocalCoords(0, 0), GetLocalCoords(0, 10));
    Canvas.Pen.Color := clGreen;
    Canvas.Line(GetLocalCoords(0, 0), GetLocalCoords(10, 0));
    Canvas.Pen.Color := clBlack;
    i := Organysm.Interval.IStart;
    Canvas.MoveTo(GetLocalCoords(i, rAimFunction(i)));
    while i < Organysm.Interval.IEnd do
    begin
      i += 0.05;
      Canvas.LineTo(GetLocalCoords(i, rAimFunction(i)));
    end;
    for ii := 0 to High(Organysm.Population) do
    begin
      TempPoint := GetLocalCoords(Organysm.Population[ii].GetLongint,
        Organysm.Population[ii].AimFunctionResult);
      Canvas.Ellipse(TempPoint.x - 2, TempPoint.y - 2, TempPoint.x + 2, TempPoint.y + 2);
    end;
  end;
end;

procedure TFormGenetic.updateUI;
var
  i: integer;
begin
  repaintGraph;
  LabelResult.Caption := 'Лучший результат: ' +
    IntToStr(Organysm.GetBest);
  MemoChromosomes.Lines.Clear;
  for i := 0 to High(Organysm.Population) do
    MemoChromosomes.Lines.Add(IntToStr(Organysm.Population[i].GetLongint) +
      ' ' + IntToStr(Organysm.Population[i].AimFunctionResult));
  MemoNewGeneration.Lines.Clear;
  for i := 0 to High(Organysm.NewGeneration) do
    MemoNewGeneration.Lines.Add(IntToStr(Organysm.NewGeneration[i].GetLongint) +
      ' ' + IntToStr(Organysm.NewGeneration[i].AimFunctionResult));
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
  ButtonStart.Enabled := True;
  ButtonTimerIterations.Enabled := True;
end;

procedure TFormGenetic.ButtonTimerIterationsClick(Sender: TObject);
begin
  Timer1.Enabled := not Timer1.Enabled;
  if Timer1.Enabled then
    ButtonTimerIterations.Caption :=
      'Остановка непрерывных итераций'
  else
    ButtonTimerIterations.Caption :=
      'Запуск непрерывных итераций';
end;

end.
