procedure TestSparceMatrix;
var
  Matrix : TLIMatrix;
  B, X : TVectorFloat;
const
  MyN = 4;
begin
  Matrix := TLIMatrix.Create(MyN);
  Matrix.SetValue(0, 0, 6.78);
  Matrix.SetValue(0, 1, 0.19);
  Matrix.SetValue(0, 3, 0.738);
  Matrix.SetValue(1, 1, 68.4);
  Matrix.SetValue(1, 2, 417.0);
  Matrix.SetValue(2, 0, 9.456);
  Matrix.SetValue(2, 1, 6.2);
  Matrix.SetValue(2, 3, 82.35);
  Matrix.SetValue(3, 2, 55.31);
  Matrix.SetValue(3, 3, 88.29);

  WriteLn('Matrica zapolnena');
  Matrix.PrintWithZeroes;
  // 6.78    0.19    0      0.738
  // 0      68.4   417      0
  // 9.456   6.2     0     82.35
  // 0       0      55.31  88.29

  B := TVectorFloat.Create(MyN);
  B.Values[0] := 645.14507;
  B.Values[1] := 7485.78;
  B.Values[2] := 70968.05266;
  B.Values[3] := 76143.6761;

  WriteLn('Vektor zapolnen');
  B.Print;

  if Matrix.LUFactor then
  begin
    WriteLn('LUP-razlozhenije zakoncheno');
    Matrix.PrintWithZeroes;

    X := Matrix.LUSolve(B);
    WriteLn('Vektor reshenij najden');
    X.Print;
    // Vernyj otvet:
    // X = [ 0.1725, 59.45, 8.2, 857.29 ]
  end;
end;

