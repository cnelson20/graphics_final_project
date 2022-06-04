unit StringHelper;
interface


const
    ARBITARILY_SMALL_NUM = -100001;

type
    ArrayFloat = Array[1..9] of Real;
    PArrayFloat = ^ ArrayFloat;

function strchr(s : String; c : Char; startchr : longInt): longInt;
function countChar(s : String; c : Char): LongInt;
function getIntsFromString(s : String; var arr1, arr2, arr3 : Array of LongInt): LongInt;
function getFloatsFromString(s : String; args : PArrayFloat) : LongInt;

implementation
uses Sysutils;

function strchr(s : String; c : Char; startchr : longInt): longInt;
var 
	i : longInt;
begin 
	i := startchr;
	strchr := -1;
	while i <= length(s) do 
	begin
		if s[i] = c then 
		begin 
			strchr := i;
			break;
		end;
		i := i + 1;
	end;
end;

function countChar(s : String; c : Char): LongInt;
var 
    i : LongInt;
    count : LongInt = 0;
begin
    for i := 1 to Length(s) do
        if s[i] = c then
            Inc(count);
    countChar := count;
end;

function getIntsFromString(s : String; var arr1, arr2, arr3 : Array of LongInt): LongInt;
var
	i : LongInt;
	j : LongInt;
	k : LongInt;
	temp, temp2 : String;
    slashCount : LongInt;
begin 
	i := Low(arr1);
	j := 1;
	while j <= Length(s) do
		begin
		k := strchr(s, ' ', j);
		if k = -1 then 
			k := Length(s) + 1;
        if k = j then
            while k = j do begin
                j := j + 1;
                k := strchr(s, ' ', j);
            end;
		temp := Copy(s, j, k - j);

        slashCount := countChar(temp, '/');
        j := 0;
        while j < slashCount do begin
            Inc(j);
            temp2 := Copy(temp, 1, strchr(temp, '/', 1) - 1);
            //WriteLn(temp2);

            if j = 1 then
                arr1[i] := StrToInt(temp2)
            else if j = 2 then
                arr2[i] := StrToInt(temp2)
            else
                arr3[i] := StrToInt(temp2);
            temp := Copy(temp, strchr(temp, '/', 1) + 1);
        end;

        //WriteLn(temp);
        Inc(j);
        if j = 1 then
            arr1[i] := StrToInt(temp)
        else if j = 2 then
            arr2[i] := StrToInt(temp)
        else
            arr3[i] := StrToInt(temp);

        for j := slashCount + 2 to 3 do begin
            if j = 2 then
                arr2[i] := ARBITARILY_SMALL_NUM
            else
                arr3[i] := ARBITARILY_SMALL_NUM;
        end;
        {if Pos('/', temp) <> 0 then
            temp := Copy(temp,1,Pos('/', temp) - 1);
		arr[i] := StrToInt(temp);}
        //WriteLn(arr1[i], '/', arr2[i], '/', arr3[i]);
		j := k + 1;
		Inc(i);
	end;
    getIntsFromString := i - Low(arr1);
end;

function getFloatsFromString(s : String; args : PArrayFloat) : LongInt;
var
	i : LongInt;
	j : LongInt;
	k : LongInt;
	temp : String;
begin 
	i := 1;
	j := 1;
	while j <= Length(s) do	begin
		k := strchr(s, ' ', j);
        //WriteLn('s : "', s , '"');
        //WriteLn('j : ', j, ' k : ', k);
		if k = -1 then 
			k := Length(s) + 1;
        if k = j then
            while k = j do begin
                j := j + 1;
                k := strchr(s, ' ', j);
            end;
		temp := Copy(s, j, k - j);
        if Pos('/', temp) <> 0 then
            temp := Copy(temp,1,Pos('/', temp) - 1);
		args ^[i] := StrToFloat(temp);
		j := k + 1;
		i += 1;
	end;
    getFloatsFromString := i - 1;
end;

end.