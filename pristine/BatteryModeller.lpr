program BatteryModeller;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  Objects,
  CommonTypes,
  SparceMatrices,
  BatteryCell2D;

{$I tests.inc}

begin
  WriteLn('Alkaline !!!');

  ///////////////////////////////////////////////
  // TESTS
  ///////////////////////////////////////////////
  TestSparceMatrix;

  Write('Finished. Press Enter...');
  ReadLn;
end.

