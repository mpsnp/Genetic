unit GeneticController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ActnList, ComCtrls, GeneticBrain, types;

type

  { TFormGenetic }

  TFormGenetic = class(TForm)
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
    TrackBarPopulationSize: TTrackBar;
    TrackBarIterations: TTrackBar;
    procedure ButtonGenerateStartPopulationClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
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
    procedure RadioGroupCrossingoverSelectionChanged(Sender: TObject);
    procedure RadioGroupMutationAndInvertionSelectionChanged(Sender: TObject);
    procedure RadioGroupSamplingTypeSelectionChanged(Sender: TObject);
    procedure RadioGroupSelectionTypesSelectionChanged(Sender: TObject);
    procedure RadioGroupStartPopulationStrategySelectionChanged(Sender: TObject);
    procedure TrackBarIterationsChange(Sender: TObject);
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
  Result := x;
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
  repaintGraph;
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
    GraphicOrigin.y -= StartMousePoint.y - y;
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

procedure TFormGenetic.RadioGroupCrossingoverSelectionChanged(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: Organysm.CrossingoverType := CT_STANDARD_ONE_POINT;
    1: Organysm.CrossingoverType := CT_PARTLY_APROPRIATE_ONE_POINT;
    2: Organysm.CrossingoverType := CT_GOLDEN_SEPARATION;
  end;
end;

procedure TFormGenetic.RadioGroupMutationAndInvertionSelectionChanged(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: Organysm.MutationType := MT_CHANGING_GOLDEN_SEPARATION;
    1: Organysm.MutationType := MT_INVERSION;
  end;
end;

procedure TFormGenetic.RadioGroupSamplingTypeSelectionChanged(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: Organysm.SamplingType := SAT_PROPORTIONAL;
  end;
end;

procedure TFormGenetic.RadioGroupSelectionTypesSelectionChanged(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: Organysm.SelectionType := ST_RANDOM;
    1: Organysm.SelectionType := ST_ELITE;
  end;
end;

procedure TFormGenetic.RadioGroupStartPopulationStrategySelectionChanged(
  Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: Organysm.StartPopulationStrategy := SPS_DROBOVIK;
    1: Organysm.StartPopulationStrategy := SPS_FOCUS;
  end;
end;

procedure TFormGenetic.TrackBarIterationsChange(Sender: TObject);
begin
  LabelPerformNIterations.Caption :=
    'Выполнить ' + IntToStr(TrackBarIterations.Position) + ' итераций';
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

  function GetLocalCoords(x, y: integer): TPoint;
  begin
    Result.x := round(GraphicScale * x + GraphicOrigin.x);
    Result.y := round(GraphicScale * y + GraphicOrigin.y);
    Result.x += aWidth div 2;
    Result.y += aHeight div 2;
  end;

  function GetLocalCoords(x, y: real): TPoint;
  begin
    Result.x := round(GraphicScale * x + GraphicOrigin.x);
    Result.y := round(GraphicScale * y + GraphicOrigin.y);
    Result.x += aWidth div 2;
    Result.y += aHeight div 2;
  end;

begin
  aWidth := ImageGraphic.Picture.Bitmap.Width;
  aHeight := ImageGraphic.Picture.Bitmap.Height;
  with ImageGraphic.Picture.Bitmap do
  begin
    Canvas.Brush.Color := clWindow;
    Canvas.FillRect(0, 0, Width, Height);
    Canvas.Pen.Color := clBlack;
    //Canvas.AntialiasingMode := amOn;
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
      Canvas.Ellipse(TempPoint.x, TempPoint.y, TempPoint.x + 5, TempPoint.y + 5);
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

procedure TFormGenetic.ButtonGenerateStartPopulationClick(Sender: TObject);
begin
  Organysm.GeneratePopulation;
  updateUI;
end;

end.
