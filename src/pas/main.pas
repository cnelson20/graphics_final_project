program Main;
uses Sysutils, Math, Process, fgl, Dos,
ObjReader, Lines, GraphicsMatrix, Curves, Shapes, Gmath, MatrixStack, StringHelper;

const 
	GRAPH_HALF_STEP = 0.05;
	GRAPH_STEP = (2 * GRAPH_HALF_STEP);

var
	curr : color;
	PolygonList : matrix;
	EdgeList : matrix;
	Transform : matrix;

procedure printArrayFloat(f : ArrayFloat);
var 
	i : LongInt;
begin
	Write('[', FloatToStrF(f[1], ffFixed, 8, 3));
	for i := 2 to 9 do begin
		Write(', ', FloatToStrF(f[i], ffFixed, 8, 3));
	end;
	WriteLn(']');
end;

procedure printCoord(c : coord);
var 
	i : LongInt;
begin
	Write('[', FloatToStrF(c[0], ffFixed, 8, 3));
	for i := 1 to 3 do begin
		Write(', ', FloatToStrF(c[i], ffFixed, 8, 3));
	end;
	WriteLn(']');
end;

procedure readCommandsFile(s : String);
type
	ConstantsMap = specialize TFPGMap<String, ArrayFloat>;
	KnobsMap = specialize TFPGMap<String, Real>;
var 
	comm : String;
	ft : TextFile;
	FxnArgs : ArrayFloat;
	CmdArgs : Array[0..2] of AnsiString;
	bool : Boolean;
	ansi : AnsiString;
	
	constants : ConstantsMap;
	const_name : String;
	i : LongInt;
	TempFloat : Real;

	CommandName, ArgsDef : String;
	Arg_ConstantName : String;
	Arg_KnobName : String;
	Arg_CSName : String;

	FramesIsDef  : Boolean = False;
	BaseNameIsDef  : Boolean = False;
	VaryIsDef : Boolean = False;

	BaseNameString : String = 'image';
	NumOfFrames : LongInt = 1;
	CurrentFrameNum : LongInt;

	KnobTableArray : Array of KnobsMap;


	VaryFuncType : String = '';

	KnobKeyList : Array of String;
	CurrentKnobList : KnobsMap;
	TempKnobList : KnobsMap;

	DefaultView : coord = (0, 0, 1, 1);
	DefaultLight : Array[0..1] of coord = ((1.0, 1.0, 1.0, 1), (255, 255, 255, 1));
	DefaultAmbient : coord = (255, 255, 255, 1);

	DefaultColor : Array[0..2] of coord = ((0.1, 0.1, 0.1, 1) , (0.5, 0.5, 0.5, 1), (0.5, 0.5, 0.5, 1));
begin 
	new(PolygonList);
	initMatrix(PolygonList);
	new(EdgeList);
	initMatrix(EdgeList);
	
	InitializeStack();
	Transform := Peek();

	constants := ConstantsMap.Create;

	// Open File
	assign(ft, s);
	reset(ft);
	ReadLn(ft, comm);
	if comm <> '__DEF__' then begin
		WriteLn('oopsie!');
		exit;
	end;
	{
		Pass #0 (First pass)
		Look for frames, basename, vary, 

	}
	while not Eof(ft) do begin
		readLn(ft, comm);
		if comm = '__OP__' then begin
			ReadLn(ft, comm);
			if comm = 'frames' then begin
				FramesIsDef := True;
				ReadLn(ft, comm);
				comm := Copy(comm, Pos(' ' , comm) + 1);
				While comm[Length(comm)] = ' ' do
					Dec(comm[0]);
				NumOfFrames := Trunc(StrToFloat(comm));
			end else if comm = 'basename' then begin
			  	BaseNameIsDef := True;
				ReadLn(ft, comm);
				comm := Copy(comm, Pos(' ' , comm) + 1);
				While comm[Length(comm)] = ' ' do
					Dec(comm[0]);
				BaseNameString := comm;
			end else if comm = 'vary' then begin
			  	VaryIsDef := True;
			end;
		end;
	end;

	If VaryIsDef and (not FramesIsDef) then begin
		WriteLn('vary is present but frames is not!');
		Exit();
	end;
	If FramesIsDef and (not BaseNameIsDef) then
		WriteLn('basename not defined! using "image" as default.');

	WriteLn('Frames: ', NumOfFrames);
	WriteLn('BaseName: ', BaseNameString);

	{
		Pass #1 (Second pass)
		Look for vary's
	}
	SetLength(KnobTableArray, NumOfFrames);
	for i := 0 to NumOfFrames - 1 do begin
		KnobTableArray[i] := KnobsMap.Create;
	end;

	if VaryIsDef then begin
		reset(ft);
		repeat
			ReadLn(ft, comm);
		until (comm = '__RUN__') or (Eof(ft));	
		while not Eof(ft) do begin
			ReadLn(ft, comm);
			if comm = '__KNOB__' then begin
				SetLength(KnobKeyList, Length(KnobKeyList) + 1);
				ReadLn(ft, comm);
				KnobKeyList[Length(KnobKeyList) - 1] := comm;
				continue;  
			end else if comm <> '__OP__' then
				continue;
			ReadLn(ft, comm);
			if comm <> 'vary' then
				continue;
			ReadLn(ft, comm);
			VaryFuncType := '';
			ArgsDef := Copy(comm, Pos(' ', comm) + 1);
			if Comm[Length(comm)] = ' ' then
				SetLength(comm, Length(comm) - 1);
			WriteLn('"', ArgsDef, '"');
			getFloatsFromString(ArgsDef, @FxnArgs);
			repeat begin
				ReadLn(ft, comm);
				Arg_KnobName := comm;
				WriteLn(Copy(comm, 1, 4));
				if (Copy(comm, 1, 4) = 'func') then begin
					VaryFuncType := Copy(comm, Pos(' ', comm) + 1);
				end;
			end until (Copy(Arg_KnobName, 1, 4) = 'knob') or Eof(ft);
			Arg_KnobName := Copy(Arg_KnobName, Pos(' ', Arg_KnobName) + 1);
			if (Trunc(FxnArgs[1]) <> FxnArgs[1]) or (Trunc(FxnArgs[2]) <> FxnArgs[2]) then begin
				WriteLn('Frame limits must be integers!');
				exit();
			end;	
			WriteLn('VaryFuncType = ', VaryFuncType);
			if VaryFuncType = '' then begin
				WriteLn('Linear Knob');
				for i := Trunc(FxnArgs[1]) to Trunc(FxnArgs[2]) do begin
					CurrentKnobList := KnobTableArray[i];
					CurrentKnobList[Arg_KnobName] := (i - FxnArgs[1]) / (FxnArgs[2] - FxnArgs[1]) * (FxnArgs[4] - FxnArgs[3]) + FxnArgs[3];
				end;
			end else if VaryFuncType = 'quad' then begin
				WriteLn('Quadratic Knob');
				for i := Trunc(FxnArgs[1]) to Trunc(FxnArgs[2]) do begin
					CurrentKnobList := KnobTableArray[i];
					CurrentKnobList[Arg_KnobName] := Power(i - FxnArgs[1], 2) / Power(FxnArgs[2] - FxnArgs[1], 2) * (FxnArgs[4] - FxnArgs[3]) + FxnArgs[3];
				end;
			end else if VaryFuncType = 'revquad' then begin
				WriteLn('Reverse Quadratic Knob');
				for i := Trunc(FxnArgs[1]) to Trunc(FxnArgs[2]) do begin
					CurrentKnobList := KnobTableArray[i];
					CurrentKnobList[Arg_KnobName] := Power(FxnArgs[2] - i, 2) / Power(FxnArgs[1] - FxnArgs[2], 2) * (FxnArgs[4] - FxnArgs[3]) + FxnArgs[3];
				end;
			end;	
		end;
	end;

	{ 

		Pass #2 (Third pass)
		Do actual graphics stuff
	}
	CurrentKnobList := KnobsMap.Create;
	for CurrentFrameNum := 0 to NumOfFrames - 1 do begin
		TempKnobList := KnobTableArray[CurrentFrameNum];
		if FramesIsDef then
			WriteLn('Saving Frame ', CurrentFrameNum);
		for i := 0 to TempKnobList.Count - 1 do begin
			//WriteLn(TempKnobList.Keys[i], ' => ', TempKnobList[TempKnobList.Keys[i]]);
			//WriteLn(TempKnobList.Keys[i], ' => ', FloatToStrF(TempKnobList[TempKnobList.Keys[i]], ffFixed, 8, 3));
			CurrentKnobList[TempKnobList.Keys[i]] := TempKnobList[TempKnobList.Keys[i]];
		end;

		reset(ft);
		while not Eof(ft) do begin 
			readLn(ft, comm);
			//WriteLn('Comm: ', comm);
			if comm = '__RUN__' then
				break;
			if comm = '__CONST__' then begin
				readln(ft, comm);
				i := Pos(' ', comm);
				const_name := Copy(comm, 1, i - 1);
				getFloatsFromString(Copy(comm, i + 1), @FxnArgs);
				{WriteLn('const_name: "', const_name, '"');}
				constants[const_name] := FxnArgs;
			end else if comm = '__KNOB__' then begin
				ReadLn(ft, comm);
			end;
		end; 
		while not Eof(ft) do 
		begin 
			readln(ft, comm);
			//WriteLn('Comm: ', comm);
			if comm = '__OP__' then 
				continue;
			CommandName := comm;
			readln(ft, ArgsDef);
			ArgsDef := Copy(ArgsDef, Pos(' ', ArgsDef) + 1);
			Arg_ConstantName := '';
			Arg_KnobName := '';
			Arg_CSName := '';
			while not Eof(ft) do begin
				ReadLn(ft, comm);
				if comm = '__OP__' then
				break;
				i := Pos(' ', comm);
				const_name := Copy(comm, 1, i - 1);	
				if const_name = 'constants' then begin
					Arg_ConstantName := Copy(comm, i + 1);
				end else if const_name = 'knob' then begin
					Arg_KnobName := Copy(comm, i + 1);
					//WriteLn(CommandName, ' ', Arg_KnobName);
				end else if const_name = 'cs' then begin
					Arg_CSName := Copy(comm, i + 1);
					//WriteLn(CommandName, ' ', Arg_KnobName);
				end;

			end;
			{ Print current command } 
			{WriteLn('R:', CommandName, ' ', ArgsDef, ' ', Arg_ConstantName);}
			{ 3d objects }
			if (CommandName = 'sphere') or (CommandName = 'torus') or (CommandName = 'box') then begin
				if (Arg_ConstantName = '') or (Arg_ConstantName = 'None') then begin
					ObjAmbientRefl := DefaultColor[0];
					ObjDiffuseRefl := DefaultColor[1];
					ObjSpecularRefl := DefaultColor[2];
				end else begin
					if constants.indexOf(Arg_ConstantName) <> -1 then
						FxnArgs := constants[Arg_ConstantName]
					else begin
						WriteLn('Constant "', Arg_ConstantName, '" not found! Defaulting to normal color');
						for i := 0 to 8 do
							FxnArgs[i + 1] := DefaultColor[i mod 3][i div 3];
					end;
					for i := 0 to 2 do 
						ObjAmbientRefl[i] := FxnArgs[1 + i * 3];
					for i := 0 to 2 do 
						ObjDiffuseRefl[i] := FxnArgs[2 + i * 3];
					for i := 0 to 2 do 
						ObjSpecularRefl[i] := FxnArgs[3 + i * 3];
					{Write('Ambient: '); printCoord(ObjAmbientRefl);
					Write('Diffuse: '); printCoord(ObjDiffuseRefl);
					Write('Specular: '); printCoord(ObjSpecularRefl);}
				end;
				getFloatsFromString(ArgsDef, @FxnArgs);
				if CommandName = 'box' then
					DrawBox(PolygonList, FxnArgs[1], FxnArgs[2], FxnArgs[3], FxnArgs[4], FxnArgs[5], FxnArgs[6])
				else if CommandName = 'sphere' then
					DrawSphere(PolygonList, FxnArgs[1], FxnArgs[2], FxnArgs[3], FxnArgs[4])
				else 
					DrawTorus(PolygonList, FxnArgs[1], FxnArgs[2], FxnArgs[3], FxnArgs[4], FxnArgs[5]);
				multiplyMatrix(Transform, PolygonList);
				DrawPolygons(PolygonList);
				clearMatrix(PolygonList);
			{ 2d shapes / lines }
			end else if (CommandName = 'hermite') or (CommandName = 'bezier') or (CommandName = 'circle') or (CommandName = 'line') then begin
				getFloatsFromString(ArgsDef, @FxnArgs);
				if CommandName = 'hermite' then
					HermiteCurve(EdgeList, FxnArgs[1], FxnArgs[2], FxnArgs[3], FxnArgs[4], FxnArgs[5], FxnArgs[6], FxnArgs[7], FxnArgs[8])
				else if CommandName = 'bezier' then	  
					BezierCurve(EdgeList, FxnArgs[1], FxnArgs[2], FxnArgs[3], FxnArgs[4], FxnArgs[5], FxnArgs[6], FxnArgs[7], FxnArgs[8])
				else if CommandName = 'circle' then
					DrawCircle(EdgeList, Round(FxnArgs[1]), Round(FxnArgs[2]), Round(FxnArgs[3]), Round(FxnArgs[4]))
				else
					AddEdge(EdgeList, FxnArgs[1], FxnArgs[2], FxnArgs[3], FxnArgs[4], FxnArgs[5], FxnArgs[6]);

				multiplyMatrix(Transform, EdgeList);
				DrawEdges(EdgeList, curr);
				clearMatrix(EdgeList);
			{ Mesh }
			end else if CommandName = 'mesh' then begin
				if constants.indexOf(Arg_ConstantName) <> -1 then
					FxnArgs := constants[Arg_ConstantName]
				else begin
					if Arg_ConstantName <> ':' then
						WriteLn('Constant "', Arg_ConstantName, '" not found! Defaulting to normal color');
					for i := 0 to 8 do
						FxnArgs[i + 1] := DefaultColor[i mod 3][i div 3];
				end;
				for i := 0 to 2 do 
					ObjAmbientRefl[i] := FxnArgs[1 + i * 3];
				for i := 0 to 2 do 
					ObjDiffuseRefl[i] := FxnArgs[2 + i * 3];
				for i := 0 to 2 do 
					ObjSpecularRefl[i] := FxnArgs[3 + i * 3];
				if Arg_CSName[Length(Arg_CSName)] = ' ' then
					SetLength(Arg_CSName, Length(Arg_CSName) - 1); 
				readObj(Concat(Arg_CSName, '.obj'), PolygonList, Transform);
				clearMatrix(PolygonList);
			{ Stack operations }
			end else if CommandName = 'push' then begin
				Push();
				Transform := Peek(); 
			end else if CommandName = 'pop' then begin
				Pop();
				Transform := Peek();	
			{ Transform Matrix Operations }
			end else if CommandName = 'move' then begin
				getFloatsFromString(ArgsDef, @FxnArgs);
				if Arg_KnobName = '' then
					TempFloat := 1
				else
					TempFloat := CurrentKnobList[Arg_KnobName];
				moveMatrix(FxnArgs[1] * TempFloat, FxnArgs[2] * TempFloat, FxnArgs[3] * TempFloat, Transform);
			end else if CommandName = 'scale' then begin
				getFloatsFromString(ArgsDef, @FxnArgs);
				if Arg_KnobName = '' then
					TempFloat := 1
				else
					TempFloat := CurrentKnobList[Arg_KnobName];
				scaleMatrix(FxnArgs[1] * TempFloat, FxnArgs[2] * TempFloat, FxnArgs[3] * TempFloat, Transform);
			end else if CommandName = 'rotate' then begin
				if Arg_KnobName = '' then
					TempFloat := 1
				else
					TempFloat := CurrentKnobList[Arg_KnobName];
				if (ArgsDef[1] = 'Z') or (ArgsDef[1] = 'z') then 
					rotateMatrixZ(StrToFloat(Copy(ArgsDef,3)) * TempFloat, Transform)
				else if (ArgsDef[1] = 'X') or (ArgsDef[1] = 'x') then 
					rotateMatrixX(StrToFloat(Copy(ArgsDef,3)) * TempFloat, Transform)
				else if (ArgsDef[1] = 'Y') or (ArgsDef[1] = 'y') then 
					rotateMatrixY(StrToFloat(Copy(ArgsDef,3)) * TempFloat, Transform);
			{ Save / View files }
			end else if CommandName = 'display' then begin
				SaveFile('TEMPFILE.ppm');
				CmdArgs[0] := 'TEMPFILE.ppm';
				bool := RunCommand('display', CmdArgs[0..0], ansi);
				bool := DeleteFile('TEMPFILE.ppm');
			end else if CommandName = 'save' then begin
				while ArgsDef[Ord(ArgsDef[0])] = ' ' do
					Dec(ArgsDef[0]);
				if Pos('.', ArgsDef) = 0 then begin
					WriteLn('Saving to "', ArgsDef, '.ppm"');
					SaveFile(Concat(ArgsDef, '.ppm'));
				end else begin
					SaveFile('TEMPFILE.ppm');
					if RunCommand(Concat('convert TEMPFILE.ppm ', ArgsDef), ansi) then
						bool := DeleteFile('TEMPFILE.ppm')
					else begin
						WriteLn('Convert failed. saving ppm to "', ArgsDef, '.ppm" instead');
						WriteLn('You can run "convert ', ArgsDef, '.ppm ', ArgsDef, '" manually to convert what you want');
						RenameFile('TEMPFILE.ppm', Concat(ArgsDef, '.ppm'));
						DeleteFile('TEMPFILE.ppm');
					end;
				end;
			end;
		end;

		if FramesIsDef then begin
			{
				TODO : prepend zeros to filenames 
			}
			ArgsDef := IntToStr(CurrentFrameNum);
			while Length(ArgsDef) < 3 do
				ArgsDef := Concat('0', ArgsDef);
		  	SaveFile(Concat('anim/', BaseNameString, '_', ArgsDef, '.ppm'));
			clearMatrix(EdgeList);
			clearMatrix(PolygonList);
			while cs.length > 0 do begin
				Pop();
			end;
			InitializeStack();
			initScreen();
		end;

	end;
	Close(ft);

	if FramesIsDef then begin
		{$ifdef WINDOWS}
		Exec('convert', Concat('anim/', BaseNameString, '* ', BaseNameString, '.gif'));
		{$else}
		bool := RunCommand(Concat('python3 src/py/animate.py ', BaseNameString), ansi);
		{$endif}
	end;

	for i := 0 to NumOfFrames - 1 do
		KnobTableArray[i].Destroy;
	CurrentKnobList.Destroy;

	// Close file, free mem
	{
	Dispose(PolygonList);
	Dispose(EdgeList);
	Dispose(Transform);}
end;

procedure main;
var 
	{$ifndef WINDOWS}
	b : boolean;
	AnsiDummy : AnsiString;
	{$endif}
begin
	curr.red := $ff;
	curr.green := $ff;
	curr.blue := $ff;

	if paramCount() = 0 then 
	begin 
		exit;
	end;
	if (paramCount() >= 2) and (paramStr(2) = '--compiled') then
		readCommandsFile(paramStr(1))
	else begin
		{$ifndef WINDOWS}
		RunCommand(Concat('python3 src/py/main.py ', paramStr(1), ' out.pars'), AnsiDummy);
		{$else}
		Exec('python3', Concat('src/py/main.py ', paramStr(1), ' out.pars'));
		{$endif}
		if false then begin
			{$ifndef WINDOWS}
			WriteLn('python did an oopsie. its output: ');
			WriteLn(AnsiDummy);
			{$else}
			WriteLn('python did an oopsie.');
			{$endif}

			WriteLn('You can try again, by running python3 src/main.py ', paramStr(1), ' out.pars, and then');
			WriteLn('running ./pas/main out.pars');
			WriteLn('if this wasn`t an error with your scripting file.');
			exit;
		end;
	  	readCommandsFile('out.pars');
		if (paramCount() >= 3) and (paramStr(2) = '--save_pars') then
			RenameFile('out.pars', paramStr(3))
		else
			DeleteFile('out.pars');
	end;
end;

Begin
	Main;
end.