{------------------------------------------------------------------------------}
{ Unit Meshes                                                                  }
{ Etot modul' dolzhen vklyuchat' vse neobhodimyje struktury i metody dlja      }
{ opisanija N-mernyh setok raznyh tipov                                        }
{                                                                              }
{------------------------------------------------------------------------------}

unit Meshes;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Objects,
  SysUtils,
  CommonTypes,
  GeometryPrimitives;

type

  { TMesh }
  { Abstraktnyj shablon dlja setok ljuboj formy }

  TMesh = class
  public
    Area : TRectFloat;
    procedure CreateMeshes; virtual; abstract;
  end;

  { TRectangularMesh2D }
  { Ravnomernaja prjamougol'naja setka }

  TRectangularMesh2D = class (TMesh)
  protected
    fArea : TRectFloat;
    fNX, fNY : Integer;
    fStepX, fStepY : Float;
    fMinSteps, fMaxSteps : TPoint2DFloat;
    fMeshX, fMeshY : TArrayFloat;
    procedure SelectStepForAxis(AreaLen, StepMin, StepMax : Float;
      var Step : Float;
      var N : Integer);
    procedure PrepareAxisDistribution(AxisPointArray : PArrayFloat;
      Start, Stop, Step : Float;
      NumSteps : Integer);
  public
    // Chislo intervalov setki (ne tochek!)
    property NX : Integer read fNX;
    property NY : Integer read fNY;
    // Shag setki (fakticheskij)
    property StepX : Float read fStepX;
    property StepY : Float read fStepY;
    // Ogranchenija na shag setki (nuzhny tol'ko pered jejo postrojenijem)
    property MinSteps : TPoint2DFloat read fMinSteps write fMinSteps;
    property MaxSteps : TPoint2DFloat read fMaxSteps write fMaxSteps;
    // Sama setka: massivy tochek po X ([0..NX]) i Y ([0..NY])
    property MeshX : TArrayFloat read fMeshX;
    property MeshY : TArrayFloat read fMeshX;
    constructor Create(
      AreaRectangle : TRectFloat;
      MinSteps_, MaxSteps_ : TPoint2DFloat);
    procedure CreateMeshes; override;
  end;

  { TColouredRectangularMesh2D }
  { Ravnomernaja setka, v kotoroj uzlam prisvaivajetsja "cvet" }

  TColouredRectangularMesh2D = class (TRectangularMesh2D)
  protected
    fColourArray : TTableInteger;
    procedure InitializeColourArray;
  public
    property Colour : TTableInteger read fColourArray;
    // Zadajot shahmatnuju raskrasku iz 1 i 0
    procedure CreateCheckerColouring;
  end;

  { TUnevenRectangularMesh2D }
  { An Neravnomernaja setka, sostavljaemaja iz neskol'kih oblastej }

  TUnevenRectangularMesh2D = class (TColouredRectangularMesh2D)
  public
    // TODO: This class definition is incomplete
    procedure CreateMeshes; override;
    procedure AddArea(NewArea : TRectangularMesh2D);
  end;

  { TElectrodeSystemMesh2D }

  TElectrodeSystemMesh2D = class (TUnevenRectangularMesh2D)
  public
    // TODO: Create class that adds areas filled with some TSpecie,
    // and specie from any point can be rapidly accessed
  end;

implementation

{ class TRectangularMesh2D }

procedure TRectangularMesh2D.SelectStepForAxis(
  AreaLen, StepMin, StepMax : Float;
  var Step : Float;
  var N : integer);
const
  epsilon = 0.005;
var
  NumParts : Float;
begin
  if StepMin > 0 then
  begin
    NumParts := AreaLen / StepMin;
    N := Trunc(NumParts);
    if Frac(NumParts) / NumParts > epsilon then inc(N);
  end
  else begin
    NumParts := AreaLen / StepMax;
    N := Trunc(NumParts);
    if (Frac(NumParts) / NumParts < 1 - epsilon) and (N > 0) then dec(N);
  end;

  Step := AreaLen / N;
  if (Step < StepMin) or (Step > StepMax) then
    raise Exception.Create('Could not calculate mesh step within given bounds');
end;

procedure TRectangularMesh2D.PrepareAxisDistribution(
  AxisPointArray : PArrayFloat;
  Start, Stop, Step : Float;
  NumSteps : integer);
var
  i : integer;
  CurrentPoint : Float;
begin
  SetLength(AxisPointArray^, NumSteps + 1);
  CurrentPoint := Start;
  for i := 0 to NumSteps - 1 do
  begin
    AxisPointArray^[i] := CurrentPoint;
    CurrentPoint := CurrentPoint + Step;
  end;
  AxisPointArray^[NumSteps] := Stop;
end;

constructor TRectangularMesh2D.Create(
  AreaRectangle : TRectFloat;
  MinSteps_, MaxSteps_ : TPoint2DFloat);
begin
  fArea := AreaRectangle.Clone;
  fMinSteps := MinSteps_.Clone;
  fMaxSteps := MaxSteps_.Clone;
  CreateMeshes;
end;

procedure TRectangularMesh2D.CreateMeshes;
begin
  try
    if (fArea.Width < fMinSteps.X) or
       (fArea.Height < fMinSteps.Y) then
      raise Exception.Create('Area is too small');

    SelectStepForAxis(fArea.Width, fMinSteps.X, fMaxSteps.X, fStepX, fNX);
    PrepareAxisDistribution(@fMeshX, fArea.X, fArea.X2, fStepX, fNX);

    SelectStepForAxis(fArea.Height, fMinSteps.Y, fMaxSteps.Y, fStepY, fNY);
    PrepareAxisDistribution(@fMeshY, fArea.Y, fArea.Y2, fStepY, fNY);
  except
    on E : Exception do
    begin
      raise Exception.Create('Exception ' + E.ClassName +
        ' was raised in TRectangularMeshArea2D.CreateMeshes: ' + E.Message);
    end;
  end;
end;

{ TColouredRectangularMesh2D }

procedure TColouredRectangularMesh2D.InitializeColourArray;
var
  i : integer;
begin
  if (fNY > 0) and (fNX > 0) then
  begin
    SetLength(fColourArray, fNX + 1);
    for i := 0 to fNX do
      SetLength(fColourArray[i], fNY + 1);
  end
  else begin
    raise Exception.Create('Unable to initialize Colour array to zero dimensions');
  end;
end;

procedure TColouredRectangularMesh2D.CreateCheckerColouring;
var
  i, j : integer;
begin
  if fColourArray = nil then
    InitializeColourArray;
  for i := 0 to fNX do
    for j := 0 to fNY do
      Colour[i][j] := (i + j) mod 2;
end;

{ TUnevenRectangularMesh2D }
// Nichego ne delajem, jesli ne zadany oblasti.
// Metod dolzhen rabotat' pravil'no v 2-uh sluchajah:
// 1. Vyzov iz konstruktora - nichego ne delajet.
// 2. Vyzov, kogda oblasti zadany cherez AddArea() - razmechajet setku.
procedure TUnevenRectangularMesh2D.CreateMeshes;
begin
  // TODO
end;

procedure TUnevenRectangularMesh2D.AddArea(NewArea: TRectangularMesh2D);
begin
  // TODO
end;

end.

