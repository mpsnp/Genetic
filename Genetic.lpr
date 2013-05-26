program Genetic;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, GeneticController, GeneticBrain, tachartlazaruspkg;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormGenetic, FormGenetic);
  Application.Run;
end.

