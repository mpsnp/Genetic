unit GeneticBrain;

{$mode objfpc}
interface

type
  TStartPopulationStrategy = (
    SPS_BLANKET,
    SPS_SHOTGUN,
    SPS_FOCUS
    );
  TSelectionTypes    = (
    ST_RANDOM,
    ST_SPECIFIED_GRAPH,
    ST_ELITE,
    ST_TOURNIR,
    ST_INBREADING,
    ST_GIBRIDISATION
    );
  TCrossingoverTypes = (
    CT_STANDARD_ONE_POINT,
    CT_STANDARD_TWO_POINT,
    CT_STANDARD_SEVERAL_POINT,
    CT_UNIVERSAL,
    CT_SORTING_ONE_POINT,
    CT_SORTING_TWO_POINT,
    CT_PARTLY_APROPRIATE_ONE_POINT,
    CT_PARTLY_APROPRIATE_TWO_POINT,
    CT_CYCLE,
    CT_COMPLEX,
    CT_GREEDY,
    CT_GOLDEN_SEPARATION,
    CT_FIBONACHI
    );
  TMutationTypes     = (
    MT_SIMPLE,
    MT_POINT,
    MT_CHANGING,
    MT_CHANGING_GOLDEN_SEPARATION,
    MT_CHANGING_FIBONACHI,
    MT_INVERSION,
    MT_DUPLICATION,
    MT_TRANSLOCATION,
    MT_TRANSPOSITION
    );
  TSamplingTypes     = (
    SAT_PROPORTIONAL,
    SAT_ELITE,
    SAT_EQUAL
    );
  TBestResultTypes   = (
    BRT_MIN,
    BRT_MAX
    );

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

    //Start population generation
    procedure shotgunPopulationGeneration;

    //Selection
    function randomSelect: TChromosomArray;
    function eliteSelect: TChromosomArray;

    //Crossingover
    function standartOnePointCrossover
      (const Chromosomes: TChromosomArray): TChromosomArray;

    function compare(a, b: TChromosom): boolean;
    function getChromosomeRatio(a, min, max: TChromosom): real;
    function select: TChromosomArray;
    function crossOver(const Chromosomes: TChromosomArray): TChromosomArray;
    function mutate(const Chromosomes: TChromosomArray): TChromosomArray;
    procedure updateAimFunctionInChromosomes;
    procedure sortChromosomes;
    procedure addNewGenerationToOld(const Chromosomes: TChromosomArray);
    procedure reproduction;
    procedure calculateSumAimFunction;
  public
    AimFunction: TAimFunction;
    DnkLength: integer;
    CrossingoverRate: real;
    MutationRate: real;
    BestResultType: TBestResultTypes;
    PopulationCount: integer;
    StartPopulationStrategy: TStartPopulationStrategy;
    SelectionType: TSelectionTypes;
    CrossingoverType: TCrossingoverTypes;
    MutationType: TMutationTypes;
    SamplingType: TSamplingTypes;
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

procedure COrganysm.addNewGenerationToOld(const Chromosomes: TChromosomArray);
var
  i: integer;
begin
  SetLength(_Population, Length(_Population) + Length(Chromosomes));
  for i := 0 to High(Chromosomes) do
    _Population[High(_Population) - i] := Chromosomes[i];
end;

procedure COrganysm.reproduction;
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

procedure COrganysm.shotgunPopulationGeneration;
var
  i, ii: integer;
begin
  SetLength(_Population, PopulationCount);
  for i := 0 to High(_Population) do
  begin
    SetLength(_Population[i].DNK, DnkLength);
    for ii := 0 to High(_Population[i].DNK) do
      _Population[i].DNK[ii] := Random(2);
  end;
end;

function COrganysm.randomSelect: TChromosomArray;
var
  i: integer;
begin
  i := round(CrossingoverRate * Length(_Population));
  i := i - i mod 2;
  SetLength(Result, i);
  for i := 0 to High(Result) do
    Result[i] := CopyChromosome(_Population[random(Length(_Population))]);
end;

function COrganysm.eliteSelect: TChromosomArray;
var
  i: integer;
begin
  i := round(CrossingoverRate * Length(_Population));
  i := i - i mod 2;
  SetLength(Result, i);
  sortChromosomes;
  for i := 0 to High(Result) do
    Result[i] := CopyChromosome(_Population[i]);
end;

function COrganysm.standartOnePointCrossover
  (const Chromosomes: TChromosomArray): TChromosomArray;
var
  i, ii, cut: integer;
  temp: byte;
begin
  Result := Chromosomes;
  for i := 0 to High(Result) - 1 do
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
begin
  case SelectionType of
    ST_RANDOM:
      Result := randomSelect;
    ST_ELITE:
      Result := eliteSelect;
  end;
end;

function COrganysm.crossOver(const Chromosomes: TChromosomArray): TChromosomArray;
begin
  case CrossingoverType of
    CT_STANDARD_ONE_POINT: Result := standartOnePointCrossover(Chromosomes);
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
  reproduction;
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
begin
  case StartPopulationStrategy of
    SPS_SHOTGUN: shotgunPopulationGeneration;
    SPS_FOCUS: ;//TODO: Закончить
  end;
  updateAimFunctionInChromosomes();
end;

begin
  randomize;
end.
