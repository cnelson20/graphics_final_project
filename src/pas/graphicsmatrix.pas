unit Graphicsmatrix;
interface
uses Sysutils, Math;

const
    IMAGE_WIDTH = 500;
	MAXX = IMAGE_WIDTH - 1;
	IMAGE_HEIGHT = 500;
	MAXY = IMAGE_HEIGHT - 1;
	IMAGE_SIZE = IMAGE_WIDTH * IMAGE_HEIGHT;
    
type
    coord = Array[0..3] of Real;
    pcoord = ^ coord;
    matr = record
        length : longInt;
        m : Array of coord;
    end;
    matrix = ^ matr;

    colorarray = array[1..IMAGE_WIDTH*IMAGE_HEIGHT] of byte;
	color = record
		red : byte;
		green : byte;
		blue : byte;
	end;
	screen = record 
		redarray : colorarray;
		greenarray : colorarray;
		bluearray : colorarray;
		zbuffer : array[1..IMAGE_WIDTH*IMAGE_HEIGHT] of Real;
	end;

procedure printMatrix(m : matrix);
procedure MatrixInfo(m : matrix);
procedure multiplyMatrix(a,b : matrix);
procedure identMatrix(m : matrix);
procedure moveMatrix(x,y,z : Real; t : matrix);
procedure scaleMatrix(x,y,z : Real; t : matrix);
procedure rotateMatrixZ(theta : Real; t : matrix);
procedure rotateMatrixX(theta : Real; t : matrix);
procedure rotateMatrixY(theta : Real; t : matrix);
procedure copyMatrix(src,dest: matrix);
procedure initMatrix(m : matrix);
procedure clearMatrix(m : matrix);
procedure setMatrixLength(m : matrix; size : longInt);

implementation
var 
    n : matrix;
    temp : matr;

procedure printMatrix(m : matrix);
var 
    j : longInt;
    i : longInt;
begin 
    if m ^.length = 0 then 
    begin
        writeln('[ ]');
        exit;
    end;

    for j := 0 to 3 do 
    begin 
        i := 0;
        while i < m ^.length do 
        begin
            write(FloatToStrF(m ^.m[i][j], ffFixed, 8, 3), ' ');
            inc(i);
        end;
        Writeln();
    end;
end;

procedure MatrixInfo(m : matrix);
begin 
    writeln(' -- MatrixInfo() -- ');
    writeln('Matrix Length: ', m ^.length);
    writeln(' Matrix Contents: ');
    printMatrix(m);
    writeln(' ---- ');
end;

function getMultValue(a,b : matrix; x,y : longInt) : Real;
var 
    i : longInt;
    sum : Real;
begin 
    sum := 0.0;
    for i := 0 to 3 do
    begin 
        sum += a ^.m[i][y] * b ^.m[x][i];
    end;
    getMultValue := sum;
end;

procedure multiplyMatrix(a, b : matrix);
var 
    j : longInt;
    i : longInt;
begin 
    if (Length(n ^.m) < b ^.length) then 
        setLength(n ^.m, b ^.length);
    n ^.length := b ^.length;
    for j := 0 to (b ^.length - 1) do   
    begin
        for i := 0 to 3 do    
            n ^.m[j][i] := getMultValue(a, b, j, i); 
    end;
    copyMatrix(n, b);
end;

procedure moveMatrix(x,y,z : Real; t : matrix);
begin 
    identMatrix(@temp);
    temp.m[3][0] := x;
    temp.m[3][1] := y;
    temp.m[3][2] := z;
    
    multiplyMatrix(t, @temp);
    copyMatrix(@temp, t);
end;

procedure scaleMatrix(x,y,z : Real; t : matrix);
begin 
    identMatrix(@temp);
    temp.m[0][0] := x;
    temp.m[1][1] := y;
    temp.m[2][2] := z;
    
    multiplyMatrix(t, @temp);
    copyMatrix(@temp, t);
end;

{
    rotation around z
    cos  -sin 0 0 
    sin  cos  0 0
    0    0    1 0
    0    0    0 1

    rotation around x 
    1 0   0    0
    0 cos -sin 0
    0 sin cos  0
    0 0   0    1

    rotation around y 
    cos  0 sin 0
    0    1 0   0
    -sin 0 cos 0
    0    0 0   1    
}
procedure rotateMatrixZ(theta : Real; t : matrix);
var 
    s : Real;
    c : Real;
begin 
    identMatrix(@temp);
    s := Sin(Pi * theta / 180);
    c := Cos(Pi * theta / 180);
    temp.m[0][0] := c;
    temp.m[1][0] := -1 * s;

    temp.m[0][1] := s;
    temp.m[1][1] := c;

    multiplyMatrix(t, @temp);
    copyMatrix(@temp, t);
end;

procedure rotateMatrixX(theta : Real; t : matrix);
var 
    s : Real;
    c : Real;
begin 
    identMatrix(@temp);
    s := Sin(Pi * theta / 180);
    c := Cos(Pi * theta / 180);
    temp.m[1][1] := c;
    temp.m[2][1] := -1 * s;

    temp.m[1][2] := s;
    temp.m[2][2] := c;

    multiplyMatrix(t, @temp);
    copyMatrix(@temp, t);
end;

procedure rotateMatrixY(theta : Real; t : matrix);
var 
    s : Real;
    c : Real;
begin 
    identMatrix(@temp);
    s := Sin(Pi * theta / 180);
    c := Cos(Pi * theta / 180);
    temp.m[0][0] := c;
    temp.m[2][0] := s;

    temp.m[0][2] := -1 * s;
    temp.m[2][2] := c;

    multiplyMatrix(t, @temp);
    copyMatrix(@temp, t);
end;

procedure identMatrix(m : matrix);
var
    j : byte;
    i : byte;
begin 
    for j := 0 to 3 do
    begin 
        for i := 0 to 3 do 
        begin 
            if i = j then
            m ^.m[j][i] := 1.0
        else 
            m ^.m[j][i] := 0.0;
        end;
    end;
end;

procedure copyMatrix(src, dest : matrix);
var 
    j : longInt;
    i : longInt;
begin 
    j := 0;
    while j < dest ^.length do
    begin 
        for i := 0 to 3 do 
        begin 
            dest ^.m[j][i] := src ^.m[j][i];
        end;
        inc(j);
    end;
end;

procedure clearMatrix(m : matrix);
begin 
    initMatrix(m);
end;

procedure initMatrix(m : matrix);
begin 
    setLength(m ^.m, 10);
    m ^.length := 0;
end;

procedure setMatrixLength(m : matrix; size : longInt);
begin 
    setLength(m ^.m, size);
    m ^.length := size;
end;

initialization
begin 
    setMatrixLength(@temp, 4);
    new(n);
end;

finalization
begin 
    Dispose(n);
end;

end.