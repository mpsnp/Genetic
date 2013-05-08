unit GeneticController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ActnList, GeneticBrain;

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
    LabelStartPopulationSize: TLabel;
    LabelIterations: TLabel;
    LabelPerform: TLabel;
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
    procedure RadioGroupCrossingoverSelectionChanged(Sender: TObject);
    procedure RadioGroupMutationAndInvertionSelectionChanged(Sender: TObject);
    procedure RadioGroupSamplingTypeSelectionChanged(Sender: TObject);
    procedure RadioGroupSelectionTypesSelectionChanged(Sender: TObject);
    procedure RadioGroupStartPopulationStrategySelectionChanged(Sender: TObject);
  private
    { private declarations }
    Organysm: COrganysm;
  public
    { public declarations }
  end;

var
  FormGenetic: TFormGenetic;

implementation

{$R *.lfm}

{ TFormGenetic }

procedure TFormGenetic.FormCreate(Sender: TObject);
begin
  Organysm := COrganysm.Create;
  Organysm.IterationCount:=10;
  Organysm.PopulationCount:=10;
  Organysm.SamplingType:=SAT_PROPORTIONAL;
  Organysm.SelectionType:=ST_RANDOM;
  Organysm.MutationType:=MT_CHANGING_GOLDEN_SEPARATION;
  Organysm.StartPopulationStrategy:=SPS_DROBOVIK;
  Organysm.CrossingoverType:=CT_STANDARD_ONE_POINT;
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

procedure TFormGenetic.EditStartPopulationSizeEditingDone(Sender: TObject);
begin
  Organysm.PopulationCount := StrToInt(TEdit(Sender).Text);
end;

procedure TFormGenetic.EditIterationsCountEditingDone(Sender: TObject);
begin
  Organysm.IterationCount := StrToInt(TEdit(Sender).Text);
end;

procedure TFormGenetic.ButtonStartClick(Sender: TObject);
begin
  Organysm.DoAllIterations;
end;

procedure TFormGenetic.ButtonGenerateStartPopulationClick(Sender: TObject);
begin
  Organysm.GeneratePopulation;
end;

end.




