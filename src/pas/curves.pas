unit Curves;
interface 
uses Math, Graphicsmatrix, Lines;

procedure DrawCircle(e : matrix; cx, cy, cz, r: LongInt);
procedure HermiteCurve(e : matrix; x0,y0,x1,y1,rx0,ry0,rx1,ry1 : Real);
procedure BezierCurve(e : matrix; x0,y0,x1,y1,x2,y2,x3,y3 : Real);
procedure GenCurve(e : matrix; curveGen : matr; x0,y0,x1,y1,x2,y2,x3,y3 : Real);
procedure DrawCurve(e : matrix; ax,bx,cx,dx : Real; ay,by,cy,dy : Real);

implementation
const   
    CURVESTEP = 0.05;
    CIRCLEBASESTEP = 0.05;
var
    BezierMatr : matr;
    HermiteMatr : matr;
    Curve1Matr : matr;

procedure HermiteCurve(e : matrix; x0,y0,x1,y1,rx0,ry0,rx1,ry1 : Real);
begin 
    GenCurve(e, HermiteMatr, x0, y0, x1, y1, rx0, ry0, rx1, ry1);
end;
procedure BezierCurve(e : matrix; x0,y0,x1,y1,x2,y2,x3,y3 : Real);
begin 
    GenCurve(e, BezierMatr, x0, y0, x1, y1, x2, y2, x3, y3);
end;

procedure GenCurve(e : matrix; curveGen : matr; x0,y0,x1,y1,x2,y2,x3,y3 : Real);
var
    ax : Real;
    bx : Real;
    cx : Real;
    dx : Real;
begin 

    Curve1Matr.m[0][0] := x0;
    Curve1Matr.m[0][1] := x1;
    Curve1Matr.m[0][2] := x2;
    Curve1Matr.m[0][3] := x3;
    multiplyMatrix(@curveGen, @Curve1Matr);
    ax := Curve1Matr.m[0][0];
    bx := Curve1Matr.m[0][1];
    cx := Curve1Matr.m[0][2];
    dx := Curve1Matr.m[0][3];

    Curve1Matr.m[0][0] := y0;
    Curve1Matr.m[0][1] := y1;
    Curve1Matr.m[0][2] := y2;
    Curve1Matr.m[0][3] := y3;
    multiplyMatrix(@curveGen, @Curve1Matr);
    DrawCurve(e, ax, bx, cx, dx, Curve1Matr.m[0][0], Curve1Matr.m[0][1], Curve1Matr.m[0][2], Curve1Matr.m[0][3]);
end;

procedure DrawCurve(e : matrix; ax,bx,cx,dx : Real; ay,by,cy,dy : Real);
var 
    tx0 : Real;
    ty0 : Real;
    tx1 : Real;
    ty1 : Real;
    tmax : Real;
    t : Real;
begin 
    tmax := 1 + CURVESTEP;
    t := CURVESTEP;

    tx0 := dx;
    ty0 := dy;

    while t < tmax do 
    begin 
        tx1 := ax * Power(t,3) + bx * Power(t,2) + cx * t + dx; 
        ty1 := ay * Power(t,3) + by * Power(t,2) + cy * t + dy; 

        AddEdge(e, tx0, ty0, 0, tx1, ty1, 0);

        tx0 := tx1;
        ty0 := ty1;
        t += CURVESTEP;
    end;
end;

procedure DrawCircle(e : matrix; cx, cy, cz, r: LongInt);
var 
    tx0 : Real;
    ty0 : Real;
    tx1 : Real;
    ty1 : Real;
    tmax : Real;
    usedstep : Real;
    t : Real;
begin
    usedstep := CIRCLEBASESTEP;
    if r >= 100 then 
        usedstep *= 0.5;

    tmax := 1 + usedstep;
    t := usedstep;

    tx0 := cx + r;
    ty0 := cy;

    while t < tmax do 
    begin 
        tx1 := cx + r * Cos(2 * Pi * t);
        ty1 := cy + r * Sin(2 * Pi * t);

        AddEdge(e, tx0, ty0, cz, tx1, ty1, cz);

        tx0 := tx1;
        ty0 := ty1;
        t += usedstep;
    end;
end;

initialization
begin 
    setMatrixLength(@Curve1Matr, 1);
    setMatrixLength(@HermiteMatr, 4);
    HermiteMatr.m[0][0] := 2;
    HermiteMatr.m[1][0] := -2;
    HermiteMatr.m[2][0] := 1;
    HermiteMatr.m[3][0] := 1;

    HermiteMatr.m[0][1] := -3;
    HermiteMatr.m[1][1] := 3;
    HermiteMatr.m[2][1] := -2;
    HermiteMatr.m[3][1] := -1;

    HermiteMatr.m[0][2] := 0;
    HermiteMatr.m[1][2] := 0;
    HermiteMatr.m[2][2] := 1;
    HermiteMatr.m[3][2] := 0;

    HermiteMatr.m[0][3] := 1;
    HermiteMatr.m[1][3] := 0;
    HermiteMatr.m[2][3] := 0;
    HermiteMatr.m[3][3] := 0;

    setMatrixLength(@BezierMatr, 4);
    BezierMatr.m[0][0] := -1;
    BezierMatr.m[1][0] := 3;
    BezierMatr.m[2][0] := -3;
    BezierMatr.m[3][0] := 1;
    
    BezierMatr.m[0][1] := 3;
    BezierMatr.m[1][1] := -6;
    BezierMatr.m[2][1] := 3;
    BezierMatr.m[3][1] := 0;

    BezierMatr.m[0][2] := -3;
    BezierMatr.m[1][2] := 3;
    BezierMatr.m[2][2] := 0;
    BezierMatr.m[3][2] := 0;

    BezierMatr.m[0][3] := 1;
    BezierMatr.m[1][3] := 0;
    BezierMatr.m[2][3] := 0;
    BezierMatr.m[3][3] := 0;
end;
end.

