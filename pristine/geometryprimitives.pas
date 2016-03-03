{------------------------------------------------------------------------------}
{ Unit GeometryPrimitives                                                      }
{ Opisyvaet osnovnye geometricheskie dlja opredelenija geometrii sistemy       }
{                                                                              }
{------------------------------------------------------------------------------}

unit GeometryPrimitives;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Objects,
  CommonTypes;

type

  { TPoint2DFloat }

  TPoint2DFloat = class
  public
    X, Y : Float;
    constructor Create(X_, Y_ : Float);
    function Clone : TPoint2DFloat;
  end;

  { TRectFloat }

  TRectFloat = class
  private
    function getX2 : Float;
    function getY2 : Float ;
  public
    X, Y, Width, Height : Float ;
    property X2 : Float read getX2;
    property Y2 : Float read getY2;
    constructor Create(X_, Y_, Width_, Height_ : Float);
    function Clone : TRectFloat;
  end;

implementation

{ TPoint2DFloat }

constructor TPoint2DFloat.Create(X_, Y_ : Float);
begin
  X := X_;
  Y := Y_;
end;

function TPoint2DFloat.Clone : TPoint2DFloat ;
begin
  Result := TPoint2DFloat.Create(X, Y);
end;

{ TRectFloat }

constructor TRectFloat.Create(X_, Y_, Width_, Height_ : Float);
begin
  X := X_;
  Y := Y_;
  Width := Width_;
  Height := Height_;
end;

function TRectFloat.getX2: Float;
begin
  Result := X + Width;
end;

function TRectFloat.getY2: Float;
begin
  Result := Y + Height;
end;

function TRectFloat.Clone : TRectFloat;
begin
  Result := TRectFloat.Create(X, Y, Width, Height);
end;

end.

