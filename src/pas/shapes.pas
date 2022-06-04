unit Shapes;
interface
uses Math, GraphicsMatrix, Lines;

function GenSpherePoints(x, y, z, r : Real) : matrix;
function GenTorusPoints(x, y, z, SmallR, BigR : Real) : matrix;
procedure DrawPoints(e : matrix; points : matrix; startindex, endindex : LongInt);
procedure DrawBox(p : matrix; left, top, front, x, y, z : Real);
procedure DrawSphere(p : matrix; x, y, z, r : Real);
procedure DrawTorus(p : matrix; x, y, z, SmallR, BigR : Real);

implementation
const
    POINTS_PER_CIRCLE = 75;
    CIRCLES_PER_SPHERE = 75;

procedure DrawBox(p : matrix; left, top, front, x, y, z : Real);
begin 
    { Front }
    AddPolygon(p, left, top, front, left, top - y, front, left + x, top - y, front);
    AddPolygon(p, left, top, front, left + x, top - y, front, left + x, top, front);
    { Top }
    AddPolygon(p, left, top, front, left + x, top, front - z, left, top, front - z);
    AddPolygon(p, left, top, front, left + x, top, front, left + x, top, front - z);
    { Back }
    AddPolygon(p, left, top, front - z, left + x, top - y, front - z, left, top - y, front - z);
    AddPolygon(p, left, top, front - z, left + x, top, front - z, left + x, top - y, front - z);
    { Bottom }
    AddPolygon(p, left, top - y, front, left, top - y, front - z, left + x, top - y, front - z);
    AddPolygon(p, left, top - y, front, left + x, top - y, front - z, left + x, top - y, front);
    { Right }
    AddPolygon(p, left + x, top, front, left + x, top - y, front, left + x, top - y, front - z);
    AddPolygon(p, left + x, top, front, left + x, top - y, front - z, left + x, top, front - z);
    { Left }
    AddPolygon(p, left, top, front, left, top - y, front - z, left, top - y, front);
    AddPolygon(p, left, top, front, left, top, front - z, left, top - y, front - z);
    
end;

function GenSpherePoints(x, y, z, r : Real) : matrix;
var 
    m : matrix;

    ThetaAngle : Real;
    ThetaIterations : LongInt;
    PhiAngle : Real;
    PhiIterations : LongInt;

    PhiStep : Real;
    ThetaStep : Real;
begin 
    New(m);
    initMatrix(m);

    PhiIterations := 0;
    PhiAngle := 0;

    PhiStep := 1 / CIRCLES_PER_SPHERE * 2 * Pi;
    ThetaStep := 1 / POINTS_PER_CIRCLE * Pi;
    while PhiIterations <= CIRCLES_PER_SPHERE do 
    begin 
        ThetaAngle := 0;
        ThetaIterations := 0;
        while ThetaIterations <= POINTS_PER_CIRCLE do  
        begin
            ThetaAngle := ThetaIterations / POINTS_PER_CIRCLE * Pi;
            AddPoint(m, x + r * cos(ThetaAngle), 
                        y + r * sin(ThetaAngle) * cos(PhiAngle), 
                        z + r * sin(ThetaAngle) * sin(PhiAngle));
            Inc(ThetaIterations);
            ThetaAngle += ThetaStep;
        end;
        Inc(PhiIterations);
        PhiAngle += PhiStep;
    end;

    GenSpherePoints := m;
end;

procedure DrawSphere(p : matrix; x, y, z, r : Real);
var 
    m : matrix;
    numpoints : LongInt;
    i : LongInt;
    j : Longint;
    ind : LongInt;
begin 
    m := GenSpherePoints(x, y, z, r);
    numpoints := CIRCLES_PER_SPHERE * (POINTS_PER_CIRCLE + 1);

    for j := 0 to CIRCLES_PER_SPHERE - 1 do
    begin 
        for i := 0 to POINTS_PER_CIRCLE - 1 do 
        begin 
            ind := j * (POINTS_PER_CIRCLE + 1) + i;
            if i <> POINTS_PER_CIRCLE - 1 then
                AddPolygon(p, 
                    m ^.m[ind], 
                    m ^.m[(ind + 1) mod numpoints], 
                    m ^.m[(ind + POINTS_PER_CIRCLE + 2) mod numpoints]);
            if i <> 0 then 
                AddPolygon(p, 
                    m ^.m[ind], 
                    m ^.m[(ind + POINTS_PER_CIRCLE + 2) mod numpoints], 
                    m ^.m[(ind + POINTS_PER_CIRCLE + 1) mod numpoints]);
        end;
    end;
    
    Dispose(m);
end;

function GenTorusPoints(x, y, z, SmallR, BigR : Real) : matrix;
var 
    m : matrix;
    ThetaAngle : Real;
    ThetaIterations : LongInt;
    PhiAngle : Real;
    PhiIterations : LongInt;
begin 
    New(m);
    initMatrix(m);

    PhiIterations := 0;
    while PhiIterations < CIRCLES_PER_SPHERE do 
    begin 
        PhiAngle := PhiIterations / CIRCLES_PER_SPHERE * 2 * Pi;
        ThetaIterations := 0;
        while ThetaIterations < POINTS_PER_CIRCLE do  
        begin
            ThetaAngle := ThetaIterations / POINTS_PER_CIRCLE * 2 * Pi;
            AddPoint(m, x + Cos(PhiAngle) * (SmallR * Cos(ThetaAngle) + BigR), 
                        y + SmallR * sin(ThetaAngle), 
                        z + Sin(PhiAngle) * (SmallR * Cos(ThetaAngle) + BigR));
            Inc(ThetaIterations);
        end;
        Inc(PhiIterations);
    end;

    GenTorusPoints := m;
end;

procedure DrawTorus(p : matrix; x, y, z, SmallR, BigR : Real);
var 
    m : matrix;
    numpoints : LongInt;
    i : LongInt;
    j : Longint;
    ind : LongInt;
begin 
    m := GenTorusPoints(x, y, z, SmallR, BigR);
    numpoints := POINTS_PER_CIRCLE * CIRCLES_PER_SPHERE;

    for j := 0 to CIRCLES_PER_SPHERE - 1 do 
    begin 
        for i := 0 to POINTS_PER_CIRCLE - 1 do
        begin
            ind := j * POINTS_PER_CIRCLE + i;
            if i <> POINTS_PER_CIRCLE - 1 then
            begin 
            { A }
            AddPolygon(p, 
                m ^.m[ind], 
                m ^.m[(ind + POINTS_PER_CIRCLE + 1) mod numpoints], 
                m ^.m[(ind + POINTS_PER_CIRCLE) mod numpoints]);
            { B } 
            AddPolygon(p, 
                m ^.m[ind], 
                m ^.m[(ind + 1) mod numpoints],
                m ^.m[(ind + POINTS_PER_CIRCLE + 1) mod numpoints]);
            end 
            else 
            begin 
            { A }
            AddPolygon(p, 
                m ^.m[ind], 
                m ^.m[(ind + 1) mod numpoints],
                m ^.m[(ind + POINTS_PER_CIRCLE) mod numpoints]);
            { B } 
            AddPolygon(p, 
                m ^.m[ind], 
                m ^.m[ind + 1 - POINTS_PER_CIRCLE], 
                m ^.m[(ind + 1) mod numpoints]);
            end;
        end; 
        
    end;

    Dispose(m);
end;

procedure DrawPoints(e : matrix; points : matrix; startindex, endindex : LongInt);
var 
    i : LongInt;
begin 
    for i := startindex to Min(endindex, points ^.length - 1) do
    begin 
        AddPoint(e, points ^.m[i][0], points ^.m[i][1], points ^.m[i][2]);
        AddPoint(e, points ^.m[i][0] + 1, points ^.m[i][1] - 1, points ^.m[i][2]);
        AddPoint(e, points ^.m[i][0] + 1, points ^.m[i][1] + 1, points ^.m[i][2]);
    end;
end;

end.