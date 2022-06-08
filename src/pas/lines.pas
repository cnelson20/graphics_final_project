unit Lines;

interface
uses Graphicsmatrix, GMath, Math, Sysutils;

type
	RealColorArray = Array[0..2] of Real;

var
	s : screen;
	Black : color;

procedure plot(x,y : longInt; c : color; z : Real);
procedure DrawLineGraph(x0, y0, x1, y1 : LongInt; c : color; z0, z1: Real);
procedure DrawHorizontalLine(x0, x1, y : LongInt; c : color; z0, z1: Real);
procedure AddPoint(m : matrix; x, y, z: Real);
procedure AddEdge(m : matrix; x0, y0, z0, x1, y1, z1: Real);
procedure AddPolygon(p : matrix; x0, y0, z0, x1, y1, z1, x2, y2, z2: Real);
procedure AddPolygon(pm : matrix; p0, p1, p2 : coord);
procedure DrawEdges(e : matrix; c : color);
procedure DrawPolygons(p : matrix);
procedure DrawOBJPolygons(p, n : matrix);
procedure Fill(c : color);
procedure initScreen;
procedure SaveFile(filename : String);
procedure FillTriangle(top, middle, bottom : coord; c : color);
procedure FillTriangleGourand(top, middle, bottom : coord; ctop, cmiddle, cbottom : color);
procedure DrawHorizontalLineGourand(x0, x1, y : LongInt; z0, z1 : Real; color0, color1 : RealColorArray);

implementation
var 
	i : longInt;

procedure plot(x,y : longInt; c : color; z : Real);
begin 
	if (x < 0) or (y < 0) or (x >= IMAGE_WIDTH) or (y >= IMAGE_HEIGHT) then 
		exit;
	//WriteLn(FloatToStrF(z, ffFixed, 8, 3));
	if z < s.zbuffer[1 + x + y * IMAGE_WIDTH] then
		exit;
	s.redarray[1 + x + y * IMAGE_WIDTH] := c.red;
	s.greenarray[1 + x + y * IMAGE_WIDTH] := c.green;
	s.bluearray[1 + x + y * IMAGE_WIDTH] := c.blue;
	s.zbuffer[1 + x + y * IMAGE_WIDTH] := z + 0.0000001;
end;

procedure drawlineh(x0, y0, x1, y1 : longInt; c : color; z0, z1 : Real);	
var 
	x : longInt;
	finalx : longInt;
	y : longInt;
	a : longInt;
	b : longInt;
	d : longInt;
	direction : longInt;

	// Z Buffer
	z : Real;
	delta_z : Real;
	z_xy : Boolean;
begin
	if y1 < y0 then
	begin
		drawlineh(x1, y1, x0, y0, c, z0, z1);
		exit;
	end; 
	if x0 <= x1 then 
		direction := 1
	else 
		direction := -1;
	finalx := x1 + direction;
	
	if Abs(y1 - y0) > Abs(x1 - x0) then begin
		delta_z := (z1 - z0) / (y1 - y0);
		z_xy := True;
	end else begin
		delta_z := (z1 - z0) / (x1 - x0);
		z_xy := False;
	end;
	z := z0; 

	a := (y1 - y0) * 2;
	b := (x0 - x1) * direction;
	d := a + b; // 2 * a + b
	b := b * 2;
	x := x0;
	y := y0; 
	if x1 <> x0 then 
	begin 
		while x <> finalx do
		begin 
			plot(x, y, c, z);
			while (d > 0) and (y <= y1) do
			begin
				plot(x, y, c, z);
				inc(y);
				if z_xy then
					z += delta_z;
				d += b;
			end;
			d += a;
			x += direction;
			if not z_xy then
				z += delta_z;
		end;
	end;
	while y <= y1 do
	begin 
		plot(x1, y, c, z);
		inc(y);
	end;
end;

procedure DrawLineGraph(x0, y0, x1, y1 : LongInt; c : color; z0, z1: Real);
begin 
	drawlineh(x0, MAXY - y0, x1, MAXY - y1, c, z0, z1);
end;

procedure drawhoriz(x0, x1, y : LongInt; c : color; z0, z1: Real);
var 
	x : LongInt;
	z, delta_z : Real;
begin
	if x0 > x1 then
		drawhoriz(x1, x0, y, c, z1, z0)
	else begin
		if x0 = x1 then
			plot(x0, y, c, z0)
		else begin
			delta_z := (z1 - z0) / (x1 - x0);
			
			z := z0;
			for x := x0 to x1 do begin
				plot(x, y, c, z);
				z += delta_z;
			end;
		end;
	end;
end;

procedure DrawHorizontalLine(x0, x1, y : LongInt; c : color; z0, z1: Real);
begin
	drawhoriz(x0, x1, MAXY - y, c, z0, z1);
end;

procedure AddPoint(m : matrix; x, y, z: Real);
var 
	i : longInt;
begin 
	//WriteLn('AddPoint(', x, ',', y, ',', z, ')');
	i := m ^.length;
	//WriteLn('i: ', i);
	//WriteLn('Length(m ^.m): ', Length(m ^.m));
	if (i >= Length(m ^.m)) then 
		setLength(m ^.m, max(i * 3 div 2, i + 4));
	m ^.m[i][0] := x;
	m ^.m[i][1] := y;
	m ^.m[i][2] := z;
	m ^.m[i][3] := 1.0;
	m ^.length := i + 1;

end;
procedure AddEdge(m : matrix; x0, y0, z0, x1, y1, z1: Real);
begin
	AddPoint(m, x0, y0, z0);
	AddPoint(m, x1, y1, z1);
end;

procedure AddPolygon(p : matrix; x0, y0, z0, x1, y1, z1, x2, y2, z2: Real);
begin 
	AddPoint(p, x0, y0, z0);
	AddPoint(p, x1, y1, z1);
	AddPoint(p, x2, y2, z2);
end;

procedure AddPolygon(pm : matrix; p0, p1, p2 : coord);
begin 
	AddPoint(pm, p0[0], p0[1], p0[2]);
	AddPoint(pm, p1[0], p1[1], p1[2]);
	AddPoint(pm, p2[0], p2[1], p2[2]);
end;

procedure DrawEdges(e : matrix; c : color);
var 
	i : LongInt;
begin 
	i := 0;
	while i < (e ^.length - 1) do
	begin
		DrawLineGraph(Round(e ^.m[i][0]), Round(e ^.m[i][1]), Round(e ^.m[i+1][0]), Round(e ^.m[i+1][1]), c, e ^.m[i][2], e ^.m[i+1][2]); 
		i += 2;
	end;
end;

procedure SortThree(p : matrix; ind : LongInt);
var 
	min : LongInt;
	i : LongInt;
	tc : coord;
begin
	min := ind;
	for i := ind + 1 to ind + 2 do
		if p ^.m[i][1] < p ^.m[min][1] then
			min := i;
	tc := p ^.m[min];
	p ^.m[min] := p ^.m[ind];
	p ^.m[ind] := tc;

	if p ^.m[ind+2][1] < p ^.m[ind+1][1] then begin
		tc := p ^.m[ind+1];
		p ^.m[ind+1] := p ^.m[ind+2];
		p ^.m[ind+2] := tc;  
	end;
end;

procedure SortThreeWithColors(p : matrix; ind : LongInt; var carr : Array of color);
var 
	min : LongInt;
	i : LongInt;
	tempcoord : coord;
	tempcolor : color;
begin
	min := ind;
	for i := ind + 1 to ind + 2 do
		if p ^.m[i][1] < p ^.m[min][1] then
			min := i;
	tempcoord := p ^.m[min];
	tempcolor := carr[min - ind];
	p ^.m[min] := p ^.m[ind];
	carr[min - ind] := carr[0];
	p ^.m[ind] := tempcoord;
	carr[0] := tempcolor;

	if p ^.m[ind+2][1] < p ^.m[ind+1][1] then begin
		tempcoord := p ^.m[ind+1];
		tempcolor := carr[1];
		p ^.m[ind+1] := p ^.m[ind+2];
		carr[1] := carr[2];
		p ^.m[ind+2] := tempcoord;  
		carr[2] := tempcolor;
	end;
end;

procedure DrawPolygons(p : matrix);
var
	i : LongInt;
begin 
	i := 0;
	while i < (p ^.length - 2) do
	begin
		if CheckPolygonFacing(p, i) then
		begin
			SortThree(p, i);
			FillTriangle(p ^.m[i+2], p ^.m[i+1], p ^.m[i], GetPolygonColor(p, i));
		end;
		{WriteLn(FloatToStrF(p ^.m[i][0], ffFixed, 8, 3), ' ', 
			FloatToStrF(p ^.m[i][1], ffFixed, 8, 3), ' ', 
			FloatToStrF(p ^.m[i][2], ffFixed, 8, 3));
		WriteLn(FloatToStrF(p ^.m[i+1][0], ffFixed, 8, 3), ' ', 
			FloatToStrF(p ^.m[i+1][1], ffFixed, 8, 3), ' ', 
			FloatToStrF(p ^.m[i+1][2], ffFixed, 8, 3));
		WriteLn(FloatToStrF(p ^.m[i+2][0], ffFixed, 8, 3), ' ', 
			FloatToStrF(p ^.m[i+2][1], ffFixed, 8, 3), ' ', 
			FloatToStrF(p ^.m[i+2][2], ffFixed, 8, 3));}
		i += 3;
	end;
end;

procedure DrawOBJPolygons(p, n : matrix);
var
	i, j, k : LongInt;
	carr : Array[0..2] of color;
begin
	if n ^.length = 0 then begin
		DrawPolygons(p);
		exit;
	end;
	i := 0;
	while i < (p ^.length - 2) do
	begin
		if CheckPolygonFacing(p, i) then
		begin
			for k := 0 to 2 do begin
				normaltemp := n ^.m[i + k];
				//WriteLn(Format('(%.3f,%.3f,%.3f)',[normaltemp[0], normaltemp[1], normaltemp[2]]));
				carr[k] := GetPolygonColor(p, i + k);
			end;

			SortThreeWithColors(p, i, carr);
			FillTriangleGourand(p ^.m[i+2], p ^.m[i+1], p ^.m[i], carr[2], carr[1], carr[0]);
		end;
		i += 3;
	end;
end;

procedure Fill(c : color);
begin 
	for i := 1 to IMAGE_SIZE do
	begin 
		s.redarray[i] := c.red;
		s.greenarray[i] := c.green;
		s.bluearray[i] := c.blue;
	end;
end;

procedure SaveFile(filename : String);
var 
	f : TextFile;
	i : LongInt;
begin
	Assign(f, filename);
	Rewrite(f);
	WriteLn(f, 'P3');
	WriteLn(f, IMAGE_WIDTH, ' ', IMAGE_HEIGHT);
	WriteLn(f, '255');
	for i := 1 to IMAGE_SIZE do 
	begin 
		write(f, s.redarray[i], ' ');
		write(f, s.greenarray[i], ' ');
		write(f, s.bluearray[i], ' ');
	end;
	Close(f);
end;

function getColorFromRealArray(arr : RealColorArray): color;
var
	c : color;
begin
	c.red := Trunc(Round(arr[0]));
	c.green := Trunc(Round(arr[1]));
	c.blue := Trunc(Round(arr[2]));

	getColorFromRealArray := c;
end;

function CalculateDeltaColor(a, b : RealColorArray; delta : LongInt): RealColorArray;
var
	re : RealColorArray;
	i : LongInt;
begin
	for i := 0 to 2 do begin
		re[i] := (a[i] - b[i]) / delta;
		//WriteLn(Format('a[%d]: %.2f b[%d]: %.2f', [i, a[i], i, b[i]]));
	end;

	CalculateDeltaColor := re;
end;

function CalculateDeltaColor(a, b : color; delta : LongInt): RealColorArray;
var
	re : RealColorArray;
begin
	re[0] := (a.red - b.red) / delta;
	re[1] := (a.green - b.green) / delta;
	re[2] := (a.blue - b.blue) / delta;

	CalculateDeltaColor := re;
end;

function AddRealColorArray(a, b : RealColorArray): RealColorArray;
begin
	a[0] += b[0];
	a[1] += b[1];
	a[2] += b[2];

	AddRealColorArray := a;
end;

function getRealArrayFromColor(c : color): RealColorArray;
var
	arr : RealColorArray;
begin
	arr[0] := c.red;
	arr[1] := c.green;
	arr[2] := c.blue;

	getRealArrayFromColor := arr;
end;

procedure FillTriangleGourand(top, middle, bottom : coord; ctop, cmiddle, cbottom : color);
var
	ymin, midy, ymax : LongInt;
	x0, x1 : Real;
	z0, z1 : Real;
	y : LongInt;
	dz0, dz1, dz1_1 : Real;
	dx0, dx1, dx1_1 : Real;

	color0, color1 : RealColorArray;
	dcolor0, dcolor1, dcolor1_1 : RealColorArray;
begin
	ymin := Round(bottom[1]);
	midy := Round(middle[1]);
	ymax := Round(top[1]);

	if (ymin = midy) and (midy = ymax) then
		exit;

	x0 := bottom[0];
	x1 := bottom[0];
	y := ymin;
	z0 := bottom[2];
	z1 := bottom[2];
	color0 := getRealArrayFromColor(cbottom);
	color1 := getRealArrayFromColor(cbottom);

	dcolor0 := getRealArrayFromColor(ctop);
	{Write(Format('(%d,%d,%d)',[Trunc(dcolor0[0]), Trunc(dcolor0[1]), Trunc(dcolor0[2])]));
	Write(' ');
	dcolor0 := getRealArrayFromColor(cmiddle);
	Write(Format('(%d,%d,%d)',[Trunc(dcolor0[0]), Trunc(dcolor0[1]), Trunc(dcolor0[2])]));
	Write(' ');
	dcolor0 := getRealArrayFromColor(cbottom);
	Write(Format('(%d,%d,%d)',[Trunc(dcolor0[0]), Trunc(dcolor0[1]), Trunc(dcolor0[2])]));
	WriteLn();}

	dx0 := (top[0] - bottom[0]) / (ymax - ymin);
	dz0 := (top[2] - bottom[2]) / (ymax - ymin);
	dcolor0 := CalculateDeltaColor(ctop, cbottom, ymax - ymin);
	dx1_1 := 0;
	dz1_1 := 0;
	dcolor1_1 := getRealArrayFromColor(Black); 
	if ymax <> midy then begin
		dx1_1 := (top[0] - middle[0]) / (ymax - midy);
		dz1_1 := (top[2] - middle[2]) / (ymax - midy);
		dcolor1_1 := CalculateDeltaColor(ctop, cmiddle, ymax - midy);
	end;
	
	if ymin <> midy then begin
		dx1 := (middle[0] - bottom[0]) / (midy - ymin);
		dz1 := (middle[2] - bottom[2]) / (midy - ymin);
		dcolor1 := CalculateDeltaColor(cmiddle, cbottom, midy - ymin);
	end else begin
		dx1 := dx1_1;
		x1 := middle[0];
		dz1 := dz1_1;
		z1 := middle[2];
		dcolor1 := dcolor1_1;
		color1 := getRealArrayFromColor(cmiddle);
	end;

	{if ymin = midy then begin
		dx1 := dx1_1;
		x1 := middle[0];
		dz1 := dz1_1;
		z1 := middle[2];
		dcolor1 := dcolor1_1;
		color1 := getRealArrayFromColor(cmiddle);
	end;}
	while y <= ymax do begin
		{WriteLn('z0: ', FloatToStrF(z0, ffFixed, 8, 3), 
			'  z1: ', FloatToStrF(z1, ffFixed, 8, 3)
		);}
		DrawHorizontalLineGourand(Round(x0), Round(x1), y, z0, z1, color0, color1);
		x0 += dx0;
		x1 += dx1;
		z0 += dz0;
		z1 += dz1;
		color0 := AddRealColorArray(color0, dcolor0);
		color1 := AddRealColorArray(color1, dcolor1);
		Inc(y);
		if y = midy then begin
			if y = ymax then begin
				DrawHorizontalLineGourand(
					Round(top[0]), Round(middle[0]), 
					ymax, 
					top[2], middle[2], 
					getRealArrayFromColor(ctop), getRealArrayFromColor(cmiddle));
				break;
			end;
			dx1 := dx1_1;
			x1 := middle[0];
			dz1 := dz1_1;
			z1 := middle[2];
			dcolor1 := dcolor1_1;
			color1 := getRealArrayFromColor(cmiddle);
		end;
	end;

end;

procedure DrawHorizontalLineGourand(x0, x1, y : LongInt; z0, z1 : Real; color0, color1 : RealColorArray);
var 
	x : LongInt;
	z, delta_z : Real;
	color, delta_color : RealColorArray;
begin
	if x0 > x1 then
		DrawHorizontalLineGourand(x1, x0, y, z1, z0, color1, color0)
	else begin
		y := MAXY - y;
		if x0 = x1 then
			plot(x0, y, getColorFromRealArray(color0), z0)
		else begin
			delta_z := (z1 - z0) / (x1 - x0);
			delta_color := CalculateDeltaColor(color1, color0, x1 - x0);
			
			z := z0;
			color := color0;
			for x := x0 to x1 do begin
				plot(x, y, getColorFromRealArray(color), z);
				z += delta_z;
				color := AddRealColorArray(color, delta_color);
			end;
		end;
	end;
end;

procedure FillTriangle(top, middle, bottom : coord; c : color);
var
	ymin, midy, ymax : LongInt;
	x0, x1 : Real;
	z0, z1 : Real;
	y : LongInt;
	dz0, dz1, dz1_1 : Real;
	dx0, dx1, dx1_1 : Real;
begin
	ymin := Round(bottom[1]);
	midy := Round(middle[1]);
	ymax := Round(top[1]);

	if (ymin = midy) and (midy = ymax) then
		exit;

	x0 := bottom[0];
	x1 := bottom[0];
	y := ymin;
	z0 := bottom[2];
	z1 := bottom[2];

	dx0 := (top[0] - bottom[0]) / (ymax - ymin);
	dz0 := (top[2] - bottom[2]) / (ymax - ymin);
	dx1_1 := 0;
	dz1_1 := 0;
	if ymax <> midy then begin
		dx1_1 := (top[0] - middle[0]) / (ymax - midy);
		dz1_1 := (top[2] - middle[2]) / (ymax - midy);
	end;
	
	if ymin <> midy then begin
		dx1 := (middle[0] - bottom[0]) / (midy - ymin);
		dz1 := (middle[2] - bottom[2]) / (midy - ymin);
	end else begin
		dx1 := dx1_1;
		x1 := middle[0];
		dz1 := dz1_1;
		z1 := middle[2];
	end;

	if ymin = midy then begin
		dx1 := dx1_1;
		x1 := middle[0];
		dz1 := dz1_1;
		z1 := middle[2];
	end;
	while y <= ymax do begin
		{WriteLn('z0: ', FloatToStrF(z0, ffFixed, 8, 3), 
			'  z1: ', FloatToStrF(z1, ffFixed, 8, 3)
		);}
		DrawHorizontalLine(Round(x0), Round(x1), y, c, z0, z1);
		x0 += dx0;
		x1 += dx1;
		z0 += dz0;
		z1 += dz1;
		Inc(y);
		if y = midy then begin
			if y = ymax then begin
				DrawHorizontalLine(Round(top[0]), Round(middle[0]), ymax, c, top[2], middle[2]);
				break;
			end;
			dx1 := dx1_1;
			x1 := middle[0];
			dz1 := dz1_1;
			z1 := middle[2];
		end;
	end;

end;

procedure initScreen;
var 
	i : LongInt;
begin
	for i := 1 to IMAGE_SIZE do
	begin 
		s.redarray[i] := 0;
		s.greenarray[i] := 0;
		s.bluearray[i] := 0;
		//s.zbuffer[i] := -1.0 / 0.0; 
		s.zbuffer[i] := -10000000000000000;
	end;
end;

initialization
begin 
	initScreen();

	black.red := 0;
	black.green := 0;
	black.blue := 0;
end;

end.