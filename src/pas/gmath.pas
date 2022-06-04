unit GMath;
interface
uses Math, Sysutils, Graphicsmatrix;

procedure Normalize(v : pcoord);
procedure SurfaceNormal(polygons : matrix; ind : LongInt; normal : pcoord);
procedure ScaleVector(v : pcoord; f : Real);
function DotProduct(a, b: pcoord): Real;
function CheckPolygonFacing(polygons : matrix; ind : LongInt): Boolean;
function GetPolygonColor(polygons : matrix; ind : LongInt): color;

var
    ViewVector : coord = (0, 0, 1, 1);

    PointLightVector : coord = (1.0, 1.0, 1.0, 1);
    PointLightColor : coord = (255, 255, 255, 1);
    AmbientLightColor : coord = (50, 50, 50, 1);

    ObjAmbientRefl : coord = (0.1, 0.1, 0.1, 1);
    ObjDiffuseRefl : coord = (0.5, 0.5, 0.5, 1);
    ObjSpecularRefl : coord = (0.5, 0.5, 0.5, 1);

implementation
var 
    normaltemp : coord;

{ Github copilot did this }
// Normalize a vector
procedure Normalize(v : pcoord);
var
    i : Integer;
    s : Real;
begin
    s := 0;
    for i := 0 to 2 do
        s += v ^[i] * v ^[i];
    s := Sqrt(s);
    for i := 0 to 2 do
        v ^[i] := v ^[i] / s;
end;

function DotProduct(a, b : pcoord): Real;
var
	i: LongInt;
	sum: Real;
begin
	sum := 0;
	for i := 0 to 2 do
		sum := sum + a ^[i] * b ^[i];
	dotProduct := sum;
end;

// Calculate a surface normal
procedure SurfaceNormal(polygons : matrix; ind : LongInt; normal : pcoord);
var
    a : coord;
    b : coord;
    i : LongInt;
begin
    for i := 0 to 2 do 
    begin 
        a[i] := polygons ^.m[ind + 1][i] - polygons ^.m[ind][i];
        b[i] := polygons ^.m[ind + 2][i] - polygons ^.m[ind][i];
    end;
    normal ^[0] := a[1] * b[2] - a[2] * b[1];
    normal ^[1] := a[2] * b[0] - a[0] * b[2];
    normal ^[2] := a[0] * b[1] - a[1] * b[0];

end;

procedure CopyVector(src , dest : pcoord);
var
    i : LongInt;
begin
    for i := 0 to 2 do
        dest ^[i] := src ^[i];
end;

procedure ScaleVector(v : pcoord; f : Real);
var 
    i : LongInt;
begin
    for i := 0 to 2 do
        v ^[i] := v ^[i] * f;
end;

function CheckPolygonFacing(polygons : matrix; ind : LongInt): Boolean;
var
    dot : Real;
begin
    SurfaceNormal(polygons, ind, @normaltemp);
    dot := DotProduct(@normaltemp, @ViewVector);
    CheckPolygonFacing := dot > 0;

end;

function GetPolygonColor(polygons : matrix; ind : LongInt): color;
var
    returnval : color;
    dot_diffuse, dot_specular : Real;
    tempvector : coord;

    i : LongInt;
begin
    returnval.red := $20 + Random($e0);
    returnval.green := $20 + Random($e0);
    returnval.blue := $20 + Random($e0);

    Normalize(@normaltemp);
    //Normalize(@PointLightVector);
    dot_diffuse := Max(0, DotProduct(@normaltemp, @PointLightVector));

    for i := 0 to 2 do
        tempvector[i] := 2 * dot_diffuse * normaltemp[i] - PointLightVector[i];
    dot_specular := Max(0, Power(DotProduct(@tempvector, @ViewVector), 4));

    returnval.red := Min(255, 
        Trunc(AmbientLightColor[0] * ObjAmbientRefl[0] + 
        PointLightColor[0] * ObjDiffuseRefl[0] * dot_diffuse +
        PointLightColor[0] * ObjSpecularRefl[0] * dot_specular
        ));
    //returnval.red := Min(255, Trunc(ObjDiffuseRefl[0] * 255));
    returnval.green := Min(255, 
        Trunc(AmbientLightColor[1] * ObjAmbientRefl[1] + 
        PointLightColor[1] * ObjDiffuseRefl[1] * dot_diffuse +
        PointLightColor[1] * ObjSpecularRefl[1] * dot_specular
        ));
    //returnval.green := Min(255, Trunc(ObjDiffuseRefl[1] * 255));
    returnval.blue := Min(255, 
        Trunc(AmbientLightColor[2] * ObjDiffuseRefl[2] + 
        PointLightColor[2] * ObjDiffuseRefl[2] * dot_diffuse +
        PointLightColor[2] * ObjSpecularRefl[2] * dot_specular
        ));
    //returnval.blue := Min(255, Trunc(ObjDiffuseRefl[2] * 255));
    //WriteLn('red: ', returnval.red, ' green: ', returnval.green, ' blue: ', returnval.blue);

    GetPolygonColor := returnval;
end;


initialization
begin
    Normalize(@ViewVector);
    Normalize(@PointLightVector);
end;

end.