{------------------------------------------------------------------------------}
{ Unit CommonTypes                                                             }
{ Opisyvaet prostejshie tipy peremennyh, obschie dlja vsego proekta            }
{                                                                              }
{------------------------------------------------------------------------------}

unit CommonTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Objects,
  SysUtils;

type
	// Vse veschestvennye chisla v programme opredeleny kak Float
	// Chyoby izmenit' tochnost', nado pomenyat' opredelenie etogo tipa
	Float = Double;

  TArrayInteger = array of Integer;
  PArrayInteger = ^TArrayInteger;

  TTableInteger = array of array of Integer;
  PTablrInteger = ^TTableInteger;

  TArrayFloat = array of Float;
  PArrayFloat = ^TArrayFloat;

  { TVectorFloat }

  PVectorFloat = ^TVectorFloat;
  TvectorFloat = class
  private
    fLength : Integer;
  public
    Values : array of Float;
    property Length : Integer read fLength;
    constructor Create(length_ : Integer);
    procedure Print;
  end;

implementation

{ TVectorFloat }

constructor TVectorFloat.Create(length_ : Integer);
begin
  fLength := length_;
  SetLength(Values, length_);
end;

procedure TVectorFloat.Print;
var
  i : Integer;
begin
  for i := 0 to fLength - 1 do
    WriteLn(Values[i]:10:4);
end;

end.

