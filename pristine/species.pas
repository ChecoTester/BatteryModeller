{------------------------------------------------------------------------------}
{ Unit Species                                                                 }
{ Opredeljajet klassy, opisyvajuschie sostav i svoistva veschestva v raznyh    }
{ tochkah ili jachejkah setki                                                  }
{                                                                              }
{------------------------------------------------------------------------------}

unit Species;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Objects,
  CommonTypes,
  Porosity;

type
	TAggregateState = ( AGG_SOLID, AGG_LIQUID, AGG_GAS );

  TSpecieType = (
                 { Abstract types }
                 SP_COMPOUND,
                 SP_POROUS,
                 { Liquids }
                 SP_WATER,
                 { Solids }
                 SP_NICKEL,
                 SP_CADMIUM);

  { TSpecie }

  TSpecie = class
  private
    fSpecieType : TSpecieType;
  public
    property SpecieType : TSpecieType read fSpecieType;
    constructor Create(Type_ : TSpecieType); virtual;
  end;

  { TCompound }

  TCompound = class (TSpecie)
  private
    fComponents : array of TSpecieType;
    constructor Create(Type_ : TSpecieType); override;
    function getComponent(index : Integer) : TSpecieType;
  public
    property Components[index : Integer] : TSpecieType read getComponent;
    constructor Create(Components_ : array of TSpecieType);
  end;

  { TPorous }

  TPorous = class (TSpecie)
  private
    constructor Create(Type_ : TSpecieType); override;
  public
    SolidPhase : TSpecie;
    LiquidPhase : TSpecie;
    PorosityModel : TPorosityModelType;
    constructor Create(PorosityModel_ : TPorosityModelType);
  end;

implementation

{ TSpecie }

constructor TSpecie.Create(Type_ : TSpecieType);
begin
  fSpecieType := Type_;
end;

{ TCompound }

constructor TCompound.Create(Type_ : TSpecieType);
begin
  inherited Create(Type_);
end;

constructor TCompound.Create(Components_ : array of TSpecieType);
var
  i, n : Integer;
begin
  inherited Create(SP_COMPOUND);

  n := Length(Components_);
  SetLength(fComponents, n);
  for i := 0 to n - 1 do
    fComponents[i] := Components_[i];
end;

function TCompound.getComponent(index : Integer) : TSpecieType;
begin
  Result := fComponents[index];
end;

{ TPorous }

constructor TPorous.Create(Type_ : TSpecieType);
begin
  inherited Create(Type_);
end;

constructor TPorous.Create(PorosityModel_ : TPorosityModelType);
begin
  inherited Create(SP_POROUS);
  PorosityModel := PorosityModel_;
end;

end.
