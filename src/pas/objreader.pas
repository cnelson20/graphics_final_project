program ObjReader;
uses Sysutils, Math, Process, 
Lines, Graphicsmatrix, Curves, Shapes, Gmath, StringHelper;

var
    PolygonList, PolygonNormalList, VertexList, NormalList : matr;
    Transform : matr;
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

procedure main;
var
    FArgs : ArrayFloat;
    IArgs : Array[1..4] of LongInt;
    TextureArgs, NormalArgs : Array[1..4] of LongInt;

    comm : String;
    i : LongInt;
    j : LongInt;
    scaleFactor, xMove, yMove, xRotate, yRotate, zRotate : Real;
begin 

    scaleFactor := 80;
    xMove := 250;
    yMove := 250;
    xRotate := 0;
    yRotate := 0;
    zRotate := 0;
    i := 3;
    while i <= ParamCount() do begin 
        if (ParamStr(i) = '--scale') and (i <> ParamCount()) then begin 
            scaleFactor := StrToFloat(ParamStr(i + 1));
            Inc(i);
        end else if (ParamStr(i) = '--move') and (i < ParamCount() - 1) then begin 
            xMove := StrToFloat(ParamStr(i + 1));
            yMove := StrToFloat(ParamStr(i + 2));
            Inc(i, 2);
        end else if (ParamStr(i) = '--rotatex') and (i <> ParamCount()) then begin 
            xRotate := StrToFloat(ParamStr(i + 1));
            Inc(i);
        end else if (ParamStr(i) = '--rotatey') and (i <> ParamCount()) then begin 
            yRotate := StrToFloat(ParamStr(i + 1));
            Inc(i);
        end else if (ParamStr(i) = '--rotatez') and (i <> ParamCount()) then begin 
            zRotate := StrToFloat(ParamStr(i + 1));
            Inc(i);
        end else begin
            WriteLn('Illegal flag "', Paramstr(i),'"');
        end;
        Inc(i);
    end;
    setMatrixLength(@ Transform, 4);
    identMatrix(@ Transform);
    moveMatrix(xMove, yMove, 0, @ Transform);
    scaleMatrix(scaleFactor, scaleFactor, scaleFactor, @ Transform);
    rotateMatrixZ(zRotate, @ Transform);
    rotateMatrixY(yRotate, @ Transform);
    rotateMatrixX(xRotate, @ Transform);


    initMatrix(@VertexList);
    initMatrix(@NormalList);
    initMatrix(@PolygonList);
    initMatrix(@PolygonNormalList);

    Assign(objFile, paramStr(1));
    Reset(objFile);
    while not Eof(objFile) do begin
        ReadLn(objFile, comm);
        if (Length(comm) >= 4) and (comm[1] = '#') then begin
            if Copy(comm,1,strchr(comm,' ', 1)-1) = '#!!c' then begin
                WriteLn('Special color comment!');
                multiplyMatrix(@Transform, @PolygonList);
                DrawPolygons(@PolygonList);
                clearMatrix(@PolygonList);
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
                    WriteLn('vn ', FloatToStrF(FArgs[1], ffFixed, 8, 4),
                            ' ', FloatToStrF(FArgs[2], ffFixed, 8, 4),
                            ' ', FloatToStrF(FArgs[3], ffFixed, 8, 4));
                    AddPoint(@NormalList, FArgs[1], FArgs[2], FArgs[3]);
                end else begin
                    j := getFloatsFromString(Copy(comm,3), @FArgs);
                    WriteLn('v ', FloatToStrF(FArgs[1], ffFixed, 8, 4),
                            ' ', FloatToStrF(FArgs[2], ffFixed, 8, 4),
                            ' ', FloatToStrF(FArgs[3], ffFixed, 8, 4));
                    AddPoint(@VertexList, FArgs[1], FArgs[2], FArgs[3]);
                end;

            end
            else if InstT = 'f' then begin 
                j := getIntsFromString(Copy(comm,3), IArgs, TextureArgs, NormalArgs);
                for i := 1 to j do begin 
                    if IArgs[i] < 0 then
                        IArgs[i] := IArgs[i] + VertexList.length + 1;
                    if (NormalArgs[i] < 0) and (NormalArgs[i] > ARBITARILY_SMALL_NUM) then
                        NormalArgs[i] := NormalArgs[i] + VertexList.length + 1;
                end;
                if j = 4 then begin 
                    WriteLn('f ', IArgs[1], ' ', IArgs[2], ' ', IArgs[3], ' ', IArgs[4]);
                    AddPolygon(@PolygonList, 
                        VertexList.m[IArgs[1] - 1], 
                        VertexList.m[IArgs[2] - 1], 
                        VertexList.m[IArgs[3] - 1]);
                    AddPolygon(@PolygonList, 
                        VertexList.m[IArgs[1] - 1], 
                        VertexList.m[IArgs[3] - 1], 
                        VertexList.m[IArgs[4] - 1]);
                end
                else if j = 3 then begin
                    WriteLn('f ', IArgs[1], ' ', IArgs[2], ' ', IArgs[3]);
                    {Write('f ');
                    for i := 1 to 3 do
                        if NormalArgs[i] = ARBITARILY_SMALL_NUM then
                            Write(IArgs[i], ' ')
                        else
                            Write(IArgs[i], '/', TextureArgs[i], '/', NormalArgs[i], ' ');
                    WriteLn;}
                    AddPolygon(@PolygonList, 
                        VertexList.m[IArgs[1] - 1], 
                        VertexList.m[IArgs[2] - 1], 
                        VertexList.m[IArgs[3] - 1]);
                    if NormalArgs[i] <> ARBITARILY_SMALL_NUM then
                        AddPolygon(@NormalList, 
                            VertexList.m[NormalArgs[1] - 1], 
                            VertexList.m[NormalArgs[2] - 1], 
                            VertexList.m[NormalArgs[3] - 1]); 
                end;
            end;
        end;
    end;
 
    i := 0;
    while i < PolygonList.length do begin
        PolygonList.m[i][0] *= 2;
        PolygonList.m[i][1] *= 2;
        PolygonList.m[i][2] *= 0.35;
        Inc(i);
    end;

    //multiplyMatrix(@ Transform, @PolygonList);
    DrawPolygons(@PolygonList);

    {i := 0;
    WriteLn('Polygon Faces');
    while i < PolygonList.length do begin
        if CheckPolygonFacing(@PolygonList, i) then begin
            WriteLn('1: 0x', 
                Hex(Round(Abs(PolygonList.m[i][0] * 256))), ' ', BoolToInt(PolygonList.m[i][1] < 0) ,' 0x', 
                Hex(Round(Abs(PolygonList.m[i][1] * 256))), ' ', BoolToInt(PolygonList.m[i][2] < 0) ,' 0x', 
                Hex(Round(Abs(PolygonList.m[i][2] * 256))), ' ', BoolToInt(PolygonList.m[i][3] < 0));
			//WriteLn('1: ', Round(PolygonList.m[i][0] * 256), ' ', Round(PolygonList.m[i][1] * 256), ' ', Round(PolygonList.m[i][2] * 256));
            WriteLn('2: 0x', 
                Hex(Round(Abs(PolygonList.m[i + 1][0] * 256))), ' ', BoolToInt(PolygonList.m[i + 1][0] < 0) ,' 0x', 
                Hex(Round(Abs(PolygonList.m[i + 1][1] * 256))), ' ', BoolToInt(PolygonList.m[i + 1][1] < 0) ,' 0x', 
                Hex(Round(Abs(PolygonList.m[i + 1][2] * 256))), ' ', BoolToInt(PolygonList.m[i + 1][2] < 0));
			//WriteLn('2: ', Round(PolygonList.m[i + 1][0] * 256), ' ', Round(PolygonList.m[i + 1][1] * 256), ' ', Round(PolygonList.m[i + 1][2] * 256));
            WriteLn('3: 0x', 
                Hex(Round(Abs(PolygonList.m[i + 2][0] * 256))), ' ', BoolToInt(PolygonList.m[i + 2][0] < 0) ,' 0x', 
                Hex(Round(Abs(PolygonList.m[i + 2][1] * 256))), ' ', BoolToInt(PolygonList.m[i + 2][1] < 0) ,' 0x', 
                Hex(Round(Abs(PolygonList.m[i + 2][2] * 256))), ' ', BoolToInt(PolygonList.m[i + 2][2] < 0));
			//WriteLn('3: ', Round(PolygonList.m[i + 1][0] * 256), ' ', Round(PolygonList.m[i + 1][1] * 256), ' ', Round(PolygonList.m[i + 2][2] * 256));
        end;
        i += 3;
    end;}
    SaveFile(paramStr(2));

    WriteLn('VertexList ^.length: ', VertexList.length);
    WriteLn('Polygons: ', PolygonList.length div 3);
    Close(objFile);
end;

Begin 
    main;
End.