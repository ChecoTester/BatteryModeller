{------------------------------------------------------------------------------}
{ Unit SparceMatrices                                                          }
{ Soderzhit metody dlja raboty s razrezhennymi matricami.                      }
{                                                                              }
{------------------------------------------------------------------------------}

unit SparceMatrices;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  CommonTypes;

type

  { TLIMatrixCell }

  PLIMatrixCell = ^TLIMatrixCell;
  TLIMatrixCell = record
    ColumnIndex : Integer;
    Value : Float;
    NextCell : PLIMatrixCell;
  end;

  { TLIMatrixRow }

  PLIMatrixRow = ^TLIMatrixRow;
  TLIMatrixRow = record
    RowIndex : Integer;
    PrevRow, NextRow : PLIMatrixRow;
    FirstCell : PLIMatrixCell;
  end;

  { TLIMatrix }
  { List-indexed matrix }

  PLIMatrix = ^TLIMatrix;
  TLIMatrix = class
  private
    fM, fN : Integer;
    fFirstRow, fCurrentRow : PLIMatrixRow;
    fCurrentCell, fPreviousCell : PLIMatrixCell;
    fPermutation : array of Integer;
  public
    property NumRows : Integer read fM;
    property NumColumns : Integer read fN;
    constructor Create(N : Integer);
    constructor Create(M, N : Integer);
    function FindValue(Row, Column : Integer) : Boolean;
    function GetValue(Row, Column : Integer) : Float;
    function CurrentValue : Float;
    procedure SetValue(Row, Column : Integer; Value : Float);
    procedure SwapRows(Row1, Row2 : Integer);
    procedure Print;
    procedure PrintWithZeroes;

    function LUFactor : Boolean;
    function LUSolve(B : TVectorFloat) : TVectorFloat;
  end;

implementation

  { TLIMatrix }

  constructor TLIMatrix.Create(N : Integer);
  begin
    Create(N, N);
  end;

  constructor TLIMatrix.Create(M, N : Integer);
  var
    LastRow, NextRow : PLIMatrixRow;
    i : Integer;
  begin
    fN := N;
    fM := M;
    // Inicializirujem spisok strok
    LastRow := nil;
    for i := 0 to M - 1 do
    begin
      New(NextRow);
      if LastRow = nil then
        fFirstRow := NextRow
      else
        LastRow^.NextRow := NextRow;
      NextRow^.PrevRow := LastRow;
      NextRow^.RowIndex := i;
      NextRow^.FirstCell := nil;
      NextRow^.NextRow := nil;
      LastRow := NextRow;
    end;
    fCurrentRow := nil;
    fCurrentCell := nil;
  end;

  function TLIMatrix.FindValue(Row, Column : Integer) : Boolean;
  var
    ThisRow : PLIMatrixRow;
    NextCell, LastCell : PLIMatrixCell;
    NextColumn : Integer;
  begin
    if (fCurrentRow = nil) or (fCurrentRow^.RowIndex <> Row) then
    begin
      // Najdem stroku
      ThisRow := fFirstRow;
      while (ThisRow <> nil) and (ThisRow^.RowIndex < Row) do
        ThisRow := ThisRow^.NextRow;
      fCurrentRow := ThisRow;
    end;
    // Najdem kolonku
    LastCell := nil;
    NextCell := fCurrentRow^.FirstCell;
    NextColumn := -1;
    while NextCell <> nil do
    begin
      NextColumn := NextCell^.ColumnIndex;
      if NextColumn >= Column then
        break;
      LastCell := NextCell;
      NextCell := NextCell^.NextCell;
    end;
    fPreviousCell := LastCell;
    // Proverim, jest' li takaja jachejka
    if NextColumn = Column then
    begin
      fCurrentCell := NextCell;
      Result := True;
    end
    else begin
      FCurrentCell := nil;
      Result := False;
    end;
  end;

  function TLIMatrix.GetValue(Row, Column : Integer) : Float;
  begin
    if FindValue(Row, Column) then
      Result := FCurrentCell^.Value
    else
      Result := 0.0;
  end;

  function TLIMatrix.CurrentValue : Float;
  begin
    if fCurrentCell <> nil then
      Result := FCurrentCell^.Value
    else
      Result := 0.0;
  end;

  procedure TLIMatrix.SetValue(Row, Column : Integer; Value : Float);
  var
    NewCell : PLIMatrixCell;
  begin
    if not FindValue(Row, Column) then
    begin
      // Vstavit' znachenije mezhdu fPreviousCell i fCurrentCell
      New(NewCell);
      if fPreviousCell <> nil then
        fPreviousCell^.NextCell := NewCell
      else
        fCurrentRow^.FirstCell := NewCell;
      NewCell^.NextCell := fCurrentCell;
      fCurrentCell := NewCell;
    end;
    // Ustanavlivajem znachenije
    fCurrentCell^.Value := Value;
    fCurrentCell^.ColumnIndex := Column;
  end;

  procedure TLIMatrix.Print;
  var
    ScanRow : PLIMatrixRow;
    ScanCell : PLIMatrixCell;
  begin
    ScanRow := fFirstRow;
    repeat
      Write('L', ScanRow^.RowIndex + 1, ': ');
      ScanCell := ScanRow^.FirstCell;
      while ScanCell <> nil do
      begin
        Write('(', ScanCell^.ColumnIndex + 1, ') ', ScanCell^.Value);
        ScanCell := ScanCell^.NextCell;
        if ScanCell <> nil then
          Write(', ');
      end;
      WriteLn();
      ScanRow := ScanRow^.NextRow;
    until ScanRow = nil;
  end;

  procedure TLIMatrix.PrintWithZeroes;
  var
    ScanRow : PLIMatrixRow;
    ScanCell : PLIMatrixCell;
    i : integer;
  begin
    ScanRow := fFirstRow;
    repeat
      Write('L', ScanRow^.RowIndex + 1, ': ');
      ScanCell := ScanRow^.FirstCell;
      i := 0;
      while ScanCell <> nil do
      begin
        while i < ScanCell^.ColumnIndex do
        begin
          Write(0.0:10:4);
          inc(i);
        end;
        Write(ScanCell^.Value:10:4);
        inc(i);
        ScanCell := ScanCell^.NextCell;
      end;
      while i < fN do
      begin
        Write(0.0:10:4);
        inc(i);
      end;
      WriteLn();
      ScanRow := ScanRow^.NextRow;
    until ScanRow = nil;
  end;

  procedure TLIMatrix.SwapRows(Row1, Row2 : Integer);
  var
    MinRow, MaxRow : Integer;
    FirstRow, SecondRow : PLIMatrixRow;
    XchgCell : PLIMatrixCell;
  begin
    if (Row1 >= 0) and (Row1 < FM) and
       (Row2 >= 0) and (Row2 < FM) and (Row1 <> Row2) then
    begin
      if Row1 < Row2 then
      begin
        MinRow := Row1;
        MaxRow := Row2;
      end
      else
      begin
        MinRow := Row2;
        MaxRow := Row1;
      end;
      // Najti stroki
      FirstRow := fFirstRow;
      while FirstRow^.RowIndex <> MinRow do
        FirstRow := FirstRow^.NextRow;
      SecondRow := FirstRow^.NextRow;
      while SecondRow^.RowIndex <> MaxRow do
        SecondRow := SecondRow^.NextRow;
      // Perestavit'
      XchgCell := FirstRow^.FirstCell;
      FirstRow^.FirstCell := SecondRow^.FirstCell;
      SecondRow^.FirstCell := XchgCell;
    end;
  end;

  function TLIMatrix.LUFactor : Boolean;
  var
    i, j, k, PivotIndex : Integer;
    PivotValue, ScanValue, BaseColumnValue : Float;
    CurrentRow, ScanRow : PLIMatrixRow;
    CurrentPivotCell, PivotRowScanCell, ScanCell, PrevCell, NewCell : PLIMatrixCell;
  begin
    try
      // Zadadim vektor perestanovok
      SetLength(fPermutation, fM);
      for i := 0 to fM - 1 do
        fPermutation[i] := i;
      // Nachnem preobrazovanije
      CurrentRow := fFirstRow;
      for i := 0 to fM - 1 do
      begin
        // Poisk opornogo elementa
        PivotIndex := -1;
        PivotValue := 0;
        ScanRow := CurrentRow;
        repeat
          ScanCell := ScanRow^.FirstCell;
          while (ScanCell <> nil) and (ScanCell^.ColumnIndex < i) do
            ScanCell := ScanCell^.NextCell;
          if (ScanCell <> nil) and (ScanCell^.ColumnIndex = i) then
          begin
            ScanValue := Abs(ScanCell^.Value);
            if ScanValue > PivotValue then
            begin
              PivotIndex := ScanRow^.RowIndex;
              PivotValue := ScanValue;
              CurrentPivotCell := ScanCell;
            end;
          end;
          //WriteLn('Opornoje znachenije ', PivotValue); {DEBUG}
          ScanRow := ScanRow^.NextRow;
        until ScanRow = nil;

        if PivotIndex = -1 then
        begin
          WriteLn('Matrica vyrozhdena');
          Result := False;
          exit;
        end;

        // Osushestvim perestanovku, jesli neobhodimo
        if PivotIndex <> i then
        begin
          //WriteLn('Opornaja stroka ', PivotIndex + 1, ', tekuschaja stroka ', i + 1, ' - perestavljajem'); {DEBUG}
          SwapRows(i, PivotIndex);
          j := fPermutation[PivotIndex];
          fPermutation[PivotIndex] := fPermutation[i];
          fPermutation[i] := j;
        end;

        // Vychislim LU-razlozhenije postrochno
        PivotValue := CurrentPivotCell^.Value; // prisvoim zanovo s uchetom znaka
        //WriteLn('Opornoje znachenije obnovleno: ', PivotValue); {DEBUG}
        ScanRow := CurrentRow^.NextRow; // pereschjot strok nizhe
        while ScanRow <> nil do
        begin

          // Naidem element i-go stolbca
          ScanCell := ScanRow^.FirstCell;
          while (ScanCell <> nil) and (ScanCell^.ColumnIndex < i) do
            ScanCell := ScanCell^.NextCell;

          // Jesli takoj element jest'
          if (ScanCell <> nil) and (ScanCell^.ColumnIndex = i) then
          begin
            BaseColumnValue := ScanCell^.Value;
            if BaseColumnValue <> 0 then
            begin
              // Razdelim na opornoje znachenije
              BaseColumnValue := BaseColumnValue / PivotValue;
              ScanCell^.Value := BaseColumnValue;
              // Prodolzhim obrabatyvat' elementy stroki
              PrevCell := ScanCell;
              ScanCell := ScanCell^.NextCell;
              PivotRowScanCell := CurrentPivotCell^.NextCell;
              // Prohodimsja po opornoj stroke
              while PivotRowScanCell <> nil do
              begin
                // Ishem sovpadajushij element v tekushej stroke
                j := PivotRowScanCell^.ColumnIndex;
                while (ScanCell <> nil) and (ScanCell^.ColumnIndex < j) do
                begin
                  PrevCell := ScanCell;
                  ScanCell := ScanCell^.NextCell;
                end;
                if (ScanCell <> nil) and (ScanCell^.ColumnIndex = j) then
                  // Jachejka najdena, zapomnim tekushee znachenije
                  ScanValue := ScanCell^.Value
                else
                begin
                  // Dobavim novuju jachejku
                  New(NewCell);
                  PrevCell^.NextCell := NewCell;
                  NewCell^.ColumnIndex := j;
                  NewCell^.NextCell := ScanCell;
                  ScanCell := NewCell;
                  ScanValue := 0.0;
                end;
                // Ustanovim preobrazovannoje znachenije
                ScanCell^.Value := ScanValue - BaseColumnValue * PivotRowScanCell^.Value;
                // Idem dal'she
                PivotRowScanCell := PivotRowScanCell^.NextCell;
              end;
            end;
          end;
          // Konec preobrazovanija podmatricy
          ScanRow := ScanRow^.NextRow;
        end;
        CurrentRow := CurrentRow^.NextRow;
        // Konec razlozhenija
        //WriteLn('Itog shaga ', i, ':'); {DEBUG}
        //self.PrintWithZeroes; {DEBUG}
      end;
      Result := True;
      //WriteLn('Permutation : '); {DEBUG}
      //for i := 0 to fM - 1 do WriteLn(fPermutation[i]); {DEBUG}
    except
      Result := False;
    end;
  end;

  function TLIMatrix.LUSolve(B : TVectorFloat) : TVectorFloat;
  var
    y : array of Float;
    CalculateValue, DiagonalValue : Float;
    CurrentRow : PLIMatrixRow;
    ScanCell : PLIMatrixCell;
    i, j : Integer;
    x : TVectorFloat;
  begin
    // Reshaem Ly = PB
    SetLength(y, FM);
    for i := 0 to FM - 1 do
      y[i] := B.Values[fPermutation[i]];
    // 1-j element y[0] raven B[P[0]]
    // So 2-j stroki - pereschityvajem
    CurrentRow := fFirstRow^.NextRow;
    while CurrentRow <> nil do
    begin
      i := CurrentRow^.RowIndex;
      CalculateValue := y[i];
      ScanCell := CurrentRow^.FirstCell;
      while ScanCell <> nil do
      begin
        j := ScanCell^.ColumnIndex;
        if j >= i then
          break;
        CalculateValue := CalculateValue - ScanCell^.Value * y[j];
        ScanCell := ScanCell^.NextCell;
      end;
      y[i] := CalculateValue;
      CurrentRow := CurrentRow^.NextRow;
    end;
    //WriteLn('Reshenije Ly = PB:'); {DEBUG}
    //for i := 0 to fM - 1 do WriteLn(y[i]); {DEBUG}
    // Reshaem Ux = y
    CurrentRow := fFirstRow^.NextRow;
    while CurrentRow^.RowIndex < FM - 1 do
      CurrentRow := CurrentRow^.NextRow;
    repeat
      i := CurrentRow^.RowIndex;
      // Nahodim diagonal'nyj element
      ScanCell := CurrentRow^.FirstCell;
      while (ScanCell <> nil) and (ScanCell^.ColumnIndex < i) do
        ScanCell := ScanCell^.NextCell;
      if ScanCell^.ColumnIndex = i then
      begin
        DiagonalValue := ScanCell^.Value;
        ScanCell := ScanCell^.NextCell;
      end
      else
        DiagonalValue := 0.0; // error
      // Vychisljajem znachenije peremennoj
      CalculateValue := y[i];
      while ScanCell <> nil do
      begin
        CalculateValue := CalculateValue - ScanCell^.Value * y[ScanCell^.ColumnIndex];
        ScanCell := ScanCell^.NextCell;
      end;
      y[i] := CalculateValue / DiagonalValue;
      CurrentRow := CurrentRow^.PrevRow;
    until CurrentRow = nil;
    // Gotovim vozvraschajemyj vektor
    x := TVectorFloat.Create(fM);
    for i := 0 to FM - 1 do
      x.Values[i] := y[i];
    Result := x;
  end;

end.
