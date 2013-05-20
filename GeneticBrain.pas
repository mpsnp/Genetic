unit GeneticBrain;

{$mode objfpc}
interface

type
  TStartPopulationStrategy = (SPS_BLANKET, SPS_DROBOVIK, SPS_FOCUS);

const
  //Start population strategies


  //Selection types
  ST_RANDOM     = 1;
  ST_SPECIFIED_GRAPH = 2;
  ST_ELITE      = 4;
  ST_TOURNIR    = 8;
  ST_INBREADING = 16;
  ST_GIBRIDISATION = 32;

  //Crossingover types
  CT_STANDARD_ONE_POINT = 1;
  CT_STANDARD_TWO_POINT = 2;
  CT_STANDARD_SEVERAL_POINT = 4;
  CT_UNIVERSAL = 8;
  CT_SORTING_ONE_POINT = 16;
  CT_SORTING_TWO_POINT = 32;
  CT_PARTLY_APROPRIATE_ONE_POINT = 64;
  CT_PARTLY_APROPRIATE_TWO_POINT = 128;
  CT_CYCLE     = 256;
  CT_COMPLEX   = 512;
  CT_GREEDY    = 1024;
  CT_GOLDEN_SEPARATION = 2048;
  CT_FIBONACHI = 4096;

  //Mutation types
  MT_SIMPLE    = 1;
  MT_POINT     = 2;
  MT_CHANGING  = 4;
  MT_CHANGING_GOLDEN_SEPARATION = 8;
  MT_CHANGING_FIBONACHI = 16;
  MT_INVERSION = 32;
  MT_DUPLICATION = 64;
  MT_TRANSLOCATION = 128;
  MT_TRANSPOSITION = 256;

  //Sampling type
  SAT_PROPORTIONAL = 1;
  SAT_ELITE = 2;
  SAT_EQUAL = 4;

  //Best result type
  BRT_MIN = 1;
  BRT_MAX = 2;

type
  TGenes = array of byte;
  TAimFunction = function(x: real): real;

  { TChromosom }

  TChromosom = object
    DNK: TGenes;
    AimFunctionResult: real;
    function GetLongword: longword;
    function GetReal: real;
    procedure Invert();
  end;
  TChromosomArray = array of TChromosom;

  { TInterval }

  TInterval = object
    IStart, IEnd: integer;
    function IsValueInInterval(x: integer): boolean;
    function Width: real;
  end;

  { COrganysm }

  COrganysm = class
  public
    constructor Create;
    destructor Destroy; override;
  private
    _Population:     TChromosomArray;
    _SumAimFunction: real;
    procedure updateAimFunctionInChromosomes;
    procedure reproduction;
    function compare(a, b: TChromosom): boolean;
    function getChromosomeRatio(a, min, max: TChromosom): real;
    function select: TChromosomArray;
    function crossOver(const Chromosomes: TChromosomArray): TChromosomArray;
    function mutate(const Chromosomes: TChromosomArray): TChromosomArray;
    procedure sortChromosomes;
    procedure addNewGenerationToOld(Chromosomes: TChromosomArray);
    procedure sample;
    procedure calculateSumAimFunction;
  public
    AimFunction: TAimFunction;
    DnkLength: integer;
    CrossingoverRate: real;
    MutationRate: real;
    BestResultType: integer;
    PopulationCount: integer;
    IterationCount: integer;
    StartPopulationStrategy: TStartPopulationStrategy;
    SelectionType: longint;
    CrossingoverType: longint;
    MutationType: longint;
    SamplingType: longint;
    NewGeneration: TChromosomArray;
    Interval: TInterval;
    property Population: TChromosomArray read _Population;
    procedure NextIteration;
    procedure GeneratePopulation;
    function GetBest: real;
  end;

implementation

uses
  Utils, Math;

function CopyChromosome(AChromosome: TChromosom): TChromosom;
var
  i: integer;
begin
  SetLength(Result.DNK, Length(AChromosome.DNK));
  for i := 0 to High(Result.DNK) do
    Result.DNK[i] := AChromosome.DNK[i];
  Result.AimFunctionResult := 0;
end;

function COrganysm.compare(a, b: TChromosom): boolean;
begin
  if BestResultType = BRT_MIN then
    Result := a.AimFunctionResult > b.AimFunctionResult
  else
    Result := a.AimFunctionResult < b.AimFunctionResult;
end;

function COrganysm.getChromosomeRatio(a, min, max: TChromosom): real;
begin
  if max.AimFunctionResult <> min.AimFunctionResult then
    Result := a.AimFunctionResult / (max.AimFunctionResult - min.AimFunctionResult)
  else
    Result := 0.5;
  if BestResultType = BRT_MIN then
    Result := 1 - Result;
end;

procedure COrganysm.sortChromosomes;
var
  temp:  TChromosom;
  i, ii: integer;
begin
  updateAimFunctionInChromosomes;
  for i := 0 to High(_Population) do
    for ii := 0 to High(_Population) - 1 do
      if compare(_Population[ii], _Population[ii + 1]) then
      begin
        temp := _Population[ii];
        _Population[ii] := _Population[ii + 1];
        _Population[ii + 1] := temp;
      end;
end;

procedure COrganysm.addNewGenerationToOld(Chromosomes: TChromosomArray);
var
  i: integer;
begin
  SetLength(_Population, Length(_Population) + Length(Chromosomes));
  for i := 0 to High(Chromosomes) do
    _Population[High(_Population) - i] := Chromosomes[i];
end;

procedure COrganysm.sample;
var
  i, ii: longint;
  NewPopulation: TChromosomArray;
  ratio, randomNumber, ratioSum: real;
  MiddleChromosome, min, max: TChromosom;

  procedure AddChromosomeToNewPopulation(Chromosome: TChromosom);
  begin
    SetLength(NewPopulation, Length(NewPopulation) + 1);
    NewPopulation[High(NewPopulation)] := Chromosome;
  end;

  procedure FindMinMax;
  var
    i: integer;
  begin
    min := _Population[0];
    max := _Population[0];
    for i := 0 to High(_Population) do
    begin
      if _Population[i].AimFunctionResult > max.AimFunctionResult then
        max := _Population[i];
      if _Population[i].AimFunctionResult < min.AimFunctionResult then
        min := _Population[i];
    end;
  end;

  procedure CalculateRatioSum;
  var
    i: integer;
  begin
    ratioSum := 0;
    for i := 0 to High(_Population) do
      ratioSum += getChromosomeRatio(_Population[i], min, max);
  end;

begin
  updateAimFunctionInChromosomes;
  calculateSumAimFunction;
  FindMinMax;
  CalculateRatioSum;
  for i := 1 to PopulationCount do
  begin
    randomNumber := random();
    ii := 0;
    ratio := getChromosomeRatio(_Population[ii], min, max) / ratioSum;
    while randomNumber > ratio do
    begin
      randomNumber -= ratio;
      ii += 1;
      ratio := getChromosomeRatio(_Population[ii], min, max) / ratioSum;
    end;
    AddChromosomeToNewPopulation(_Population[ii]);
  end;
  _Population := NewPopulation;
end;

procedure COrganysm.calculateSumAimFunction;
var
  i: integer;
begin
  updateAimFunctionInChromosomes;
  _SumAimFunction := 0;
  for i := 0 to High(_Population) do
    _SumAimFunction += _Population[i].AimFunctionResult;
end;

function TChromosom.GetLongword: longword;
var
  i, t: longword;
begin
  Result := 0;
  t := 1;
  for i := High(DNK) downto 0 do
  begin
    if (DNK[i] = 1) then
      Result += t;
    t *= 2;
  end;
end;

function TChromosom.GetReal: real;
begin
  Result := GetLongword / (2 ** Length(DNK) - 1);
end;

procedure TChromosom.Invert;
var
  i, s, e: integer;
  temp:    byte;
begin
  s := random(Length(DNK));
  e := s + random(Length(DNK) - s);
  for i := s to (e - s) div 2 do
  begin
    temp := DNK[i];
    DNK[i] := DNK[e - i];
    DNK[e - i] := temp;
  end;
end;

operator = (a, b: TChromosom): boolean;
begin
  Result := a.DNK = b.DNK;
end;

procedure COrganysm.reproduction;
begin

end;

function TInterval.IsValueInInterval(x: integer): boolean;
begin
  Result := (IStart <= x) and (x <= IEnd);
end;

function TInterval.Width: real;
begin
  Result := IEnd - IStart;
end;

constructor COrganysm.Create();
begin

end;

destructor COrganysm.Destroy();
begin

end;

function COrganysm.getBest: real;
var
  i: integer;
begin
  if High(_Population) >= 0 then
    Result := _Population[0].AimFunctionResult
  else
    Result := 0;
  for i := 0 to High(_Population) do
    if (BestResultType = BRT_MIN) and (Result > _Population[i].AimFunctionResult) then
      Result := _Population[i].AimFunctionResult
    else
    if (BestResultType = BRT_MAX) and (Result < _Population[i].AimFunctionResult) then
      Result := _Population[i].AimFunctionResult;
end;

function COrganysm.select: TChromosomArray;
var
  i: integer;
begin
  i := round(CrossingoverRate * Length(_Population));
  i := i - i mod 2;
  SetLength(Result, i);
  case SelectionType of
    ST_RANDOM:
      for i := 0 to High(Result) do
        Result[i] := CopyChromosome(_Population[random(Length(_Population))]);
    ST_ELITE:
    begin
      sortChromosomes;
      for i := 0 to High(Result) do
        Result[i] := CopyChromosome(_Population[i]);
    end;
  end;
end;

function COrganysm.crossOver(const Chromosomes: TChromosomArray): TChromosomArray;
var
  i, ii, cut: integer;
  temp: byte;
begin
  Result := Chromosomes;
  for i := 0 to High(Result) - 1 do
    case CrossingoverType of
      CT_STANDARD_ONE_POINT:
      begin
        cut := random(DnkLength);
        for ii := cut to High(Result[i].DNK) do
        begin
          temp := Result[i].DNK[ii];
          Result[i].DNK[ii] := Result[i + 1].DNK[ii];
          Result[i + 1].DNK[ii] := temp;
        end;
      end;
    end;
end;

function COrganysm.mutate(const Chromosomes: TChromosomArray): TChromosomArray;
var
  i: integer;
begin
  Result := Chromosomes;
  for i := 1 to round(MutationRate * length(Result)) do
    case MutationType of
      MT_CHANGING_GOLDEN_SEPARATION: ;
      MT_INVERSION: Result[random(Length(Result))].Invert;
    end;
end;

procedure COrganysm.nextIteration;
begin
  NewGeneration := select;
  NewGeneration := crossOver(NewGeneration);
  NewGeneration := mutate(NewGeneration);
  addNewGenerationToOld(NewGeneration);
  sample;
end;

procedure COrganysm.updateAimFunctionInChromosomes();
var
  i:   integer;
  max: int64;
begin
  max := 2 ** DnkLength - 1;
  for i := 0 to High(_Population) do
    with _Population[i] do
      AimFunctionResult := AimFunction(Interval.IStart + GetLongword /
        max * Interval.Width);
end;

procedure COrganysm.GeneratePopulation();
var
  i, ii, temp: integer;
begin
  SetLength(_Population, PopulationCount);
  for i := 0 to High(_Population) do
    case StartPopulationStrategy of
      SPS_DROBOVIK:
      begin
        SetLength(_Population[i].DNK, DnkLength);
        for ii := 0 to High(_Population[i].DNK) do
          _Population[i].DNK[ii] := Random(2);
      end;
      SPS_FOCUS: ;//TODO: Закончить
    end;
  updateAimFunctionInChromosomes();
end;

begin
  randomize;
end.
