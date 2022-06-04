unit ObjReader;
interface
uses Lines, Graphicsmatrix, Curves, Shapes, Gmath, StringHelper;

procedure readObj(filename : String; PolygonList, Transform : Matrix);

implementation
uses Sysutils, Math, Process;

var
    PolygonNormalList, VertexList, NormalList : matr;
    objFile : TextFile;
    InstT : Char;

function BoolToInt( aValue : Boolean) : LongInt;
begin
  if aValue then BoolToInt := 1 else BoolToInt := 0;
end; 

function Hex(num : LongInt): String;
const
	arrayChars : Array[0..15] of Char = '0123456789ABCDEF';
var
	s : String;
begin
	setLength(s, 4);
	s[1] := arrayChars[(num mod 65536) >> 12];
	s[2] := arrayChars[(num mod 4096) >> 8];
	s[3] := arrayChars[(num mod 256) >> 4];
	s[4] := arrayChars[(num mod 16)];
	
	Hex := s;
end;

procedure readObj(filename : String; PolygonList, Transform : Matrix);
var
    FArgs : ArrayFloat;
    IArgs : Array[1..4] of LongInt;
    TextureArgs, NormalArgs : Array[1..4] of LongInt;

    comm : String;
    i : LongInt;
    j : LongInt;
begin 
    initMatrix(@VertexList);
    initMatrix(@NormalList);
    initMatrix(@PolygonNormalList);
    clearMatrix(PolygonLIst);

    WriteLn('filename = ', filename);

    Assign(objFile, filename);
    Reset(objFile);
    while not Eof(objFile) do begin
        ReadLn(objFile, comm);
        if (Length(comm) >= 4) and (comm[1] = '#') then begin
            if Copy(comm,1,strchr(comm,' ', 1)-1) = '#!!c' then begin
                //WriteLn('Special color comment!');
                multiplyMatrix(Transform, PolygonList);
                DrawPolygons(PolygonList);
                clearMatrix(PolygonList);
                j := getFloatsFromString(Copy(comm, 6), @FArgs);
                if j >= 3 then
                    for j := 0 to 2 do 
                        ObjAmbientRefl[j] := FArgs[j+1];
                if j >= 6 then
                    for j := 0 to 2 do 
                        ObjDiffuseRefl[j] := FArgs[j+4];
                if j >= 9 then
                    for j := 0 to 2 do 
                        ObjSpecularRefl[j] := FArgs[j+7];

                //WriteLn(ObjAmbientRefl[i], ObjDiffuseRefl[i], ObjSpecularRefl[i]);
            end;
            Continue;
        end else if not (Length(comm) = 0) then begin
            //WriteLn('comm: ', comm);
            InstT := comm[1];
            if (InstT = 'v') and (comm[2] <> 't') then begin 
                if (comm[2] = 'n') then begin
                    j := getFloatsFromString(Copy(comm,4), @FArgs);
                    {WriteLn('vn ', FloatToStrF(FArgs[1], ffFixed, 8, 4),
                            ' ', FloatToStrF(FArgs[2], ffFixed, 8, 4),
                            ' ', FloatToStrF(FArgs[3], ffFixed, 8, 4));}
                    AddPoint(@NormalList, FArgs[1], FArgs[2], FArgs[3]);
                end else begin
                    j := getFloatsFromString(Copy(comm,3), @FArgs);
                    {WriteLn('v ', FloatToStrF(FArgs[1], ffFixed, 8, 4),
                            ' ', FloatToStrF(FArgs[2], ffFixed, 8, 4),
                            ' ', FloatToStrF(FArgs[3], ffFixed, 8, 4));}
                    AddPoint(@VertexList, FArgs[1], FArgs[2], FArgs[3]);
                end;

            end else if InstT = 'f' then begin 
                j := getIntsFromString(Copy(comm,3), IArgs, TextureArgs, NormalArgs);
                for i := 1 to j do begin 
                    if IArgs[i] < 0 then
                        IArgs[i] := IArgs[i] + VertexList.length + 1;
                    if (NormalArgs[i] < 0) and (NormalArgs[i] > ARBITARILY_SMALL_NUM) then
                        NormalArgs[i] := NormalArgs[i] + VertexList.length + 1;
                end;
                if j = 4 then begin 
                    {WriteLn('f ', IArgs[1], ' ', IArgs[2], ' ', IArgs[3], ' ', IArgs[4]);
                    AddPolygon(PolygonList, 
                        VertexList.m[IArgs[1] - 1], 
                        VertexList.m[IArgs[2] - 1], 
                        VertexList.m[IArgs[3] - 1]);
                    AddPolygon(PolygonList, 
                        VertexList.m[IArgs[1] - 1], 
                        VertexList.m[IArgs[3] - 1], 
                        VertexList.m[IArgs[4] - 1]);}
                end else if j = 3 then begin
                    {WriteLn('f ', IArgs[1], ' ', IArgs[2], ' ', IArgs[3]);
                    AddPolygon(PolygonList, 
                        VertexList.m[IArgs[1] - 1], 
                        VertexList.m[IArgs[2] - 1], 
                        VertexList.m[IArgs[3] - 1]);
                    if NormalArgs[j] <> ARBITARILY_SMALL_NUM then
                        AddPolygon(@NormalList, 
                            VertexList.m[NormalArgs[1] - 1], 
                            VertexList.m[NormalArgs[2] - 1], 
                            VertexList.m[NormalArgs[3] - 1]); }
                end;
            end;
        end;
    end;
 
    multiplyMatrix(Transform, PolygonList);
    DrawPolygons(PolygonList);

    //WriteLn('VertexList ^.length: ', VertexList.length);
    //WriteLn('Polygons: ', PolygonList.length div 3);
    Close(objFile);
end;


end.