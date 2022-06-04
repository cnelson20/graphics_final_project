unit MatrixStack;
interface
uses GraphicsMatrix;
    
type
    CStack = record 
        s : Array of Matrix;
        length : LongInt;
    end;

var 
    cs : CStack;

procedure InitializeStack;
procedure Push;
function Peek(): Matrix;
procedure Pop;

implementation
var 
    m : matrix;

procedure InitializeStack;
begin
    //WriteLn('Initializing stack...');
    New(m);
    setMatrixLength(m, 4);
    identMatrix(m);

    //WriteLn('Creating CStack object');
    SetLength(cs.s, 4);
    cs.s[0] := m;
    cs.length := 1;
end;

function Peek(): Matrix;
begin 
    Peek := cs.s[cs.length - 1];
end;

procedure Push;
begin 
    New(m);
    setMatrixLength(m, 4);
    copyMatrix(Peek(), m);

    if (cs.length + 1 >= Length(cs.s)) then
        SetLength(cs.s, cs.length * 2);
    cs.s[cs.length] := m;
    Inc(cs.length);

end;

procedure Pop;
begin 
    Dec(cs.length);
    Dispose(cs.s[cs.length]);
end;

end.