{------------------------------------------------------------------------------}
{ Unit Porosity                                                                }
{ Vkljuchajet metody rascheta raznyh svojstv dlja raznyh modelej poristosti    }
{                                                                              }
{ TPorosityModelType - perechislenije dostuphyh vidov modelej                  }
{                                                                              }
{ TPorosityModel - abstraktnyj klass, v kotorom opisany vsje metody, kotoryje  }
{                  dolzhny byt' dostupny vo vseh modeljah poristosti           }
{                                                                              }
{ TCylindricalPoreModel - opredeljaet raschjot svojstv dlja 1D modeli          }
{                         cilindricheskoj pory                                 }
{                                                                              }
{------------------------------------------------------------------------------}

unit Porosity;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Objects,
  CommonTypes;

type

  TPorosityModelType = ( POR_CYLINDRICAL );

  TPorosityModel = class
    public
      class function GetInnerDiffusionCoeff(
        DiffusionCoeff : Float;
        Porosity : Float;
        Tortuosity : Float) : Float; virtual; abstract;
      class function GetSolidDiffusionCoeff(
        DiffusionCoeff : Float;
        Porosity : Float;
        Tortuosity : Float) : Float; virtual; abstract;
      class function GetInnerConductivity(
        Conductivity : Float;
        Porosity : Float;
        Tortuosity : Float) : Float; virtual; abstract;
      class function GetSolidConductivity(
        Conductivity : Float;
        Porosity : Float;
        Tortuosity : Float) : Float; virtual; abstract;
  end;

  { TCylindricalPoreModel }

  TCylindricalPoreModel = class (TPorosityModel)
    protected
      class function GetInnerThroughputValue(
        Value : Float;
        Porosity : Float;
        Tortuosity : Float) : Float;
      class function GetSolidThroughputValue(
        Value : Float;
        Porosity : Float;
        Tortuosity : Float) : Float;
    public
      class function GetInnerDiffusionCoeff(
        DiffusionCoeff : Float;
        Porosity : Float;
        Tortuosity : Float) : Float; override;
      class function GetSolidDiffusionCoeff(
        DiffusionCoeff : Float;
        Porosity : Float;
        Tortuosity : Float) : Float; override;
      class function GetInnerConductivity(
        Conductivity : Float;
        Porosity : Float;
        Tortuosity : Float) : Float; override;
      class function GetSolidConductivity(
        Conductivity : Float;
        Porosity : Float;
        Tortuosity : Float) : Float; override;
  end;

implementation

{ TCylindricalPoreModel }

class function TCylindricalPoreModel.GetInnerThroughputValue(
  Value: Float; Porosity: Float; Tortuosity: Float): Float;
begin
  result := Value * Porosity / (Tortuosity * Tortuosity);
end;

class function TCylindricalPoreModel.GetSolidThroughputValue(
  Value: Float; Porosity: Float; Tortuosity: Float): Float;
begin
  result := Value * (1 - Porosity / Tortuosity) / Tortuosity;
end;

class function TCylindricalPoreModel.GetInnerDiffusionCoeff(
  DiffusionCoeff: Float; Porosity: Float; Tortuosity: Float): Float;
begin
  result := GetInnerThroughputValue(DiffusionCoeff, Porosity, Tortuosity);
end;

class function TCylindricalPoreModel.GetSolidDiffusionCoeff(
  DiffusionCoeff: Float; Porosity: Float; Tortuosity: Float): Float;
begin
  result := GetSolidThroughputValue(DiffusionCoeff, Porosity, Tortuosity);
end;

class function TCylindricalPoreModel.GetInnerConductivity(Conductivity: Float;
  Porosity: Float; Tortuosity: Float): Float;
begin
  result := GetInnerThroughputValue(Conductivity, Porosity, Tortuosity);
end;

class function TCylindricalPoreModel.GetSolidConductivity(Conductivity: Float;
  Porosity: Float; Tortuosity: Float): Float;
begin
  result := GetSolidThroughputValue(Conductivity, Porosity, Tortuosity);
end;

end.
