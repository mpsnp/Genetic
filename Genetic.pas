unit Genetic;
{$mode objfpc}
interface

const
	//Start population strategies
	SPS_BLANKET = 1;
	SPS_DROBOVIK = 2;
	SPS_FOCUS = 4;
	
	//Selection types
	ST_RANDOM = 1;
	ST_SPECIFIED_GRAPH = 2;
	ST_ELITE = 4;
	ST_TOURNIR = 8;
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
	CT_CYCLE = 256;
	CT_COMPLEX = 512;
	CT_GREEDY = 1024;
	CT_GOLDEN_SEPARATION = 2048;
	CT_FIBONACHI = 4096;
	
	//Mutation types
	MT_SIMPLE = 1;						//A. Простая.
	MT_POINT = 2;						//B. Точечная.
	MT_CHANGING = 4;					//C. Обмена.
	MT_CHANGING_GOLDEN_SEPARATION = 8;	//D. Обмена на основе «Золотого сечения».
	MT_CHANGING_FIBONACHI = 16;			//E. Обмена на основе чисел Фибоначчи.
	MT_INVERSION = 32;					//F. Инверсия.
	MT_DUPLICATION = 64;				//G. Дупликация.
	MT_TRANSLOCATION = 128;				//H. Транслокация.
	MT_TRANSPOSITION = 256;				//I. Транспозиция.
	
	//Getting type
	GT_PROPORTIONAL = 1;	//A. Пропорциональный.
	GT_ELITE = 2;			//B. Элитный.
	GT_EQUAL = 4;			//C. Равновероятный.
	
	//Best result type
	BRT_MIN = 1;
	BRT_MAX = 2;
	
type
	TAimFunction = function (x:integer):integer;
	TChromosom = record
		DNK:integer;
		AimFunctionResult:longint;
	end;
	TChromosomArray = array of TChromosom;
	TInterval = object
		IStart,IEnd:integer;
		function IsValueInInterval(x:integer):boolean;
	end;
	
	COrganism = class
		public
			constructor Create;
			destructor Destroy; override;
		private
			_Population:TChromosomArray;
			_AimFunction:TAimFunction;
			_t:integer;
			_SumAimFunction:longint;
			procedure generatePopulation;
			procedure updateAimFunctionInChromosomes;
			procedure reproduction;
			procedure nextIteration;
			function getBest:longint;
		public
			BestResultType:integer;
			Interval:TInterval;
			PopulationCount:integer;
			IterationCount:integer;
			StartPopulationStrategy:longint;
			SelectionType:longint;
			CrossingoverType:longint;
			MutationType:longint;
			GettingType:longint;
			function GetResult():integer;
	end;

implementation

procedure COrganism.reproduction;
begin

end;

function TInterval.IsValueInInterval(x:integer):boolean;
begin
	result:=(IStart < x) and (x < IEnd);
end;

constructor COrganism.Create();
begin

end;

destructor COrganism.Destroy();
begin

end;

function COrganism.getBest:longint;
var
	i:integer;
begin
	result:=_Population[0].AimFunctionResult;
	for i:=0 to High(_Population) do
		if (BestResultType = BRT_MIN) and (result > _Population[i].AimFunctionResult) then
			result:=_Population[i].AimFunctionResult
		else
			if (BestResultType = BRT_MAX) and (result <_Population[i].AimFunctionResult) then
				result:=_Population[i].AimFunctionResult;
			
end;

procedure COrganism.nextIteration;
begin

end;

function COrganism.GetResult():integer;
begin
	generatePopulation();
	_t:=0;
	while _t < IterationCount do
	begin
		nextIteration;
		_t+=1;
	end;
	result:=getBest;
end;

procedure COrganism.updateAimFunctionInChromosomes();
var
	i:integer;
begin
	for i:=0 to High(_Population) do
	begin
		with _Population[i] do
			AimFunctionResult:=_AimFunction(DNK);
	end;
end;

procedure COrganism.generatePopulation();
var
	i:integer;
begin
	SetLength(_Population, PopulationCount);
	for i:=0 to High(_Population) do
		case StartPopulationStrategy of
			SPS_DROBOVIK:
				with Interval do
					_Population[i].DNK:=IStart + Random(IEnd - IStart);
			SPS_FOCUS:
				begin
					//TODO: Закончить
				end;
		end;
	updateAimFunctionInChromosomes();
end;

begin
	randomize;
end.
