unit GeneticController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ActnList, GeneticBrain, types;

type

  { TFormGenetic }

  TFormGenetic = class(TForm)
    ButtonStart: TButton;
    ButtonGenerateStartPopulation: TButton;
    EditStartPopulationSize: TEdit;
    EditIterationsCount: TEdit;
    GroupBoxStartPopulation: TGroupBox;
    GroupBoxOptions: TGroupBox;
    GroupBoxVisualisation: TGroupBox;
    ImageGraphic: TImage;
    LabelResult: TLabel;
    LabelStartPopulationSize: TLabel;
    LabelIterations: TLabel;
    LabelPerform: TLabel;
    MemoChromosomes: TMemo;
    RadioGroupSamplingType: TRadioGroup;
    RadioGroupMutationAndInvertion: TRadioGroup;
    RadioGroupCrossingover: TRadioGroup;
    RadioGroupStartPopulationStrategy: TRadioGroup;
    RadioGroupSelectionTypes: TRadioGroup;
    Splitter1: TSplitter;
    procedure ButtonGenerateStartPopulationClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure EditIterationsCountEditingDone(Sender: TObject);
    procedure EditStartPopulationSizeEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImageGraphicChangeBounds(Sender: TObject);
    procedure ImageGraphicMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure RadioGroupCrossingoverSelectionChanged(Sender: TObject);
    procedure RadioGroupMutationAndInvertionSelectionChanged(Sender: TObject);
    procedure RadioGroupSamplingTypeSelectionChanged(Sender: TObject);
    procedure RadioGroupSelectionTypesSelectionChanged(Sender: TObject);
    procedure RadioGroupStartPopulationStrategySelectionChanged(Sender: TObject);
  private
    { private declarations }
    Organysm:      COrganysm;
    GraphicScale:  real;
    GraphicOrigin: TPoint;
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
  Result := x * x + 20 * x - 34;
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
  GraphicScale := 0.5;
  GraphicOrigin.x := 20;
  GraphicOrigin.y := 20;
end;

procedure TFormGenetic.ImageGraphicChangeBounds(Sender: TObject);
begin
  ImageGraphic.Picture.Bitmap.Width := ImageGraphic.Width;
  ImageGraphic.Picture.Bitmap.Height := ImageGraphic.Height;
  repaintGraph;
end;

procedure TFormGenetic.ImageGraphicMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  GraphicScale += WheelDelta / 120 * 0.05;
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

procedure TFormGenetic.repaintGraph;
var
  i: real;
  ii, x, y: integer;
begin
  with ImageGraphic do
  begin
    Canvas.Brush.Color := clWhite;
    Canvas.FillRect(0, 0, Width, Height);
    Canvas.Pen.Color := clBlack;
    Canvas.AntialiasingMode := amOn;
    i := Organysm.Interval.IStart;
    x := round(GraphicScale * i + GraphicOrigin.x);
    y := round(GraphicScale * rAimFunction(i) + GraphicOrigin.y);
    x += Width div 2;
    y += Height div 2;
    Canvas.MoveTo(x, y);
    while i < Organysm.Interval.IEnd do
    begin
      i += 0.05;
      x := round(GraphicScale * i + GraphicOrigin.x);
      y := round(GraphicScale * rAimFunction(i) + GraphicOrigin.y);
      x += Width div 2;
      y += Height div 2;
      Canvas.LineTo(x, y);
    end;
    for ii := 0 to High(Organysm.Population) do
    begin
      x := round(GraphicScale * Organysm.Population[ii].GetLongint + GraphicOrigin.x);
      y := round(GraphicScale * Organysm.Population[ii].AimFunctionResult +
        GraphicOrigin.y);
      x += Width div 2;
      y += Height div 2;
      Canvas.Ellipse(x, y, x + 5, y + 5);
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
  for i := 0 to Organysm.PopulationCount - 1 do
    MemoChromosomes.Lines.Add(IntToStr(Organysm.Population[i].GetLongint) +
      ' ' + IntToStr(Organysm.Population[i].AimFunctionResult));
end;

procedure TFormGenetic.EditStartPopulationSizeEditingDone(Sender: TObject);
begin
  Organysm.PopulationCount := StrToInt(TEdit(Sender).Text);
end;

procedure TFormGenetic.EditIterationsCountEditingDone(Sender: TObject);
begin
  Organysm.IterationCount := StrToInt(TEdit(Sender).Text);
end;

procedure TFormGenetic.ButtonStartClick(Sender: TObject);
var
  i: integer;
begin
  Organysm.DoAllIterations;
  updateUI;
end;

procedure TFormGenetic.ButtonGenerateStartPopulationClick(Sender: TObject);
begin
  Organysm.GeneratePopulation;
  updateUI;
end;

end.
