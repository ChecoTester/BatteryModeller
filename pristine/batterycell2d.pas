{------------------------------------------------------------------------------}
{ Unit BatteryCell2D                                                           }
{ Opisyvaet obobschjonnuju strukturu 2D modeli akkumuljatornoj jachejki        }
{                                                                              }
{------------------------------------------------------------------------------}

unit BatteryCell2D;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Objects,
  CommonTypes,
  GeometryPrimitives,
  Meshes,
  Species,
  Porosity;

type
  TBatteryCell2D = class
    public
      CellDimensions : TRectFloat;
      PositiveElectrodeDimensions : array of TRectFloat;
      NegativeElectrodeDimensions : array of TRectFloat;
      SeparatorDimensions : array of TRectFloat;
  end;

implementation

end.

