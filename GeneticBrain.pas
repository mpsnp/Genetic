unit GeneticBrain;

{$mode objfpc}
interface

const
  //Start population strategies
  SPS_BLANKET  = 1;
  SPS_DROBOVIK = 2;
  SPS_FOCUS    = 4;

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
  TAimFunction = function(x: integer): integer;

  { TChromosom }

  TChromosom = object
    DNK: TGenes;
    AimFunctionResult: longint;
    function GetLongint: longint;
    procedure SetFromLongint(ADNK: longint);
    procedure Invert();
  end;
  TChromosomArray = array of TChromosom;

  TInterval = object
    IStart, IEnd: integer;
    function IsValueInInterval(x: integer): boolean;
  end;

  { COrganysm }

  COrganysm = class
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetInterval(a: TInterval);
  private
    _Population: TChromosomArray;
    _t: integer;
    _SumAimFunction: longint;
    _Interval: TInterval;
    procedure updateAimFunctionInChromosomes;
    procedure reproduction;
    procedure nextIteration;
    function compare(a, b: TChromosom): boolean;
    function select: TChromosomArray;
    function crossOver(const Chromosomes: TChromosomArray): TChromosomArray;
    function mutate(const Chromosomes: TChromosomArray): TChromosomArray;
    procedure sortChromosomes;
    procedure addNewGenerationToOld(Chromosomes: TChromosomArray);
    procedure sample;
    procedure calculateSumAimFunction;
  public
    AimFunction:      TAimFunction;
    CrossingoverRate: real;
    MutationRate:     real;
    BestResultType:   integer;
    PopulationCount:  integer;
    IterationCount:   integer;
    StartPopulationStrategy: longint;
    SelectionType:    longint;
    CrossingoverType: longint;
    MutationType:     longint;
    SamplingType:     longint;
    property Interval: TInterval read _Interval write SetInterval;
    property Population: TChromosomArray read _Population;
    function GetResult(): integer;
    procedure DoAllIterations;
    procedure GeneratePopulation;
    function GetBest: longint;
  end;

implementation

uses
  Utils;

var
  DnkLength: integer;

function COrganysm.compare(a, b: TChromosom): boolean;
begin
  if BestResultType = BRT_MIN then
    Result := a.AimFunctionResult > b.AimFunctionResult
  else
    Result := a.AimFunctionResult < b.AimFunctionResult;
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
  ratio, middleFunction: real;

  procedure AddChromosomeToNewPopulation(Chromosome: TChromosom);
  begin
    SetLength(NewPopulation, Length(NewPopulation) + 1);
    NewPopulation[High(NewPopulation)] := Chromosome;
  end;

  procedure DeleteBad();
  var
    i: integer;

    procedure DeleteThis;
    var
      ii: integer;
    begin
      for ii := i to High(_Population) - 1 do
        _Population[i] := _Population[i + 1];
      SetLength(_Population, Length(_Population) - 1);
    end;

  begin
    i := 0;
    while i <= high(_Population) do
      if not Interval.IsValueInInterval(_Population[i].GetLongint) then
        DeleteThis
      else
        i += 1;
  end;

begin
  sortChromosomes;
  DeleteBad();
  if Length(_Population) > PopulationCount then
    SetLength(_Population, PopulationCount);
{  middleFunction := _SumAimFunction / Length(_Population);
  for i := 0 to High(_Population) do
  begin
    ratio := _Population[i].AimFunctionResult / middleFunction;
    for ii := 1 to trunc(ratio) do
    begin
      AddChromosomeToNewPopulation(_Population[i]);
      ratio -= 1.0;
    end;
    //if random() < ratio then
    //AddChromosomeToNewPopulation(_Population[i]);
  end;
  i := Length(NewPopulation);
  _Population := NewPopulation;            }
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

procedure COrganysm.SetInterval(a: TInterval);
begin
  _Interval := a;
  DnkLength := round(ln(max(_Interval.IStart, _Interval.IEnd)) / ln(2)) + 1;
end;

function TChromosom.GetLongint: longint;
var
  i, t: integer;
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

procedure TChromosom.SetFromLongint(ADNK: longint);
var
  i: integer;
begin
  SetLength(DNK, DnkLength);
  i := 0;
  while ADNK <> 0 do
  begin
    DNK[High(DNK) - i] := ADNK mod 2;
    ADNK := ADNK div 2;
    i += 1;
  end;
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

constructor COrganysm.Create();
begin

end;

destructor COrganysm.Destroy();
begin

end;

function COrganysm.getBest: longint;
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
      for i := 1 to High(Result) do
      begin
        Result[i - 1] := _Population[random(Length(_Population))];
        Result[i] := _Population[random(Length(_Population))];
      end;
    ST_ELITE:
    begin
      sortChromosomes;
      for i := 0 to High(Result) do
        Result[i] := _Population[i];
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
  for i := 0 to High(Result) do
    if random() < MutationRate then
      case MutationType of
        MT_CHANGING_GOLDEN_SEPARATION: ;
        MT_INVERSION: Result[i].Invert;
      end;
end;

procedure COrganysm.nextIteration;
begin
  addNewGenerationToOld(mutate(crossOver(select)));
  sample;
end;

function COrganysm.GetResult(): integer;
begin
  generatePopulation();
  _t := 0;
  while _t < IterationCount do
  begin
    nextIteration;
    _t += 1;
  end;
  Result := getBest;
end;

procedure COrganysm.DoAllIterations;
var
  i: integer;
begin
  for i := 1 to IterationCount do
    nextIteration;
end;

procedure COrganysm.updateAimFunctionInChromosomes();
var
  i: integer;
begin
  for i := 0 to High(_Population) do
    with _Population[i] do
      AimFunctionResult := AimFunction(GetLongint);
end;

procedure COrganysm.GeneratePopulation();
var
  i: integer;
begin
  SetLength(_Population, PopulationCount);
  for i := 0 to High(_Population) do
    case StartPopulationStrategy of
      SPS_DROBOVIK:
        with Interval do
          _Population[i].SetFromLongint(IStart + Random(IEnd - IStart));
      SPS_FOCUS: ;//TODO: Закончить
    end;
  updateAimFunctionInChromosomes();
end;

begin
  randomize;
end.
