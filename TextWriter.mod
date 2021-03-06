(* Writer - simple slightly formatted text output *)

MODULE TextWriter;
IMPORT Out;

VAR ChClass: INTEGER;

PROCEDURE Classify(ch: CHAR): INTEGER;
BEGIN
  CASE ch OF
    'a'..'z', 'A'..'Z', '0'..'9':      RETURN 0 (* Space may be required required before and after*)
  | ',', '.', ';', ':', '}', ']', ')': RETURN 1 (* No space needed before, space may be required after *)
  | 0DX, 0AX:                          RETURN 3 (* End of line *)
  ELSE                                 RETURN 2 (* No space needed before or after *)
  END
END Classify;

PROCEDURE Break(c1, c2: INTEGER); BEGIN
  CASE c1 OF
    0: IF c2 > 0 THEN RETURN END
  | 1: IF c2 > 1 THEN RETURN END
  ELSE RETURN
  END;
  Out.Char(' ')
END Break;

PROCEDURE Char*(c: CHAR);         BEGIN Out.Char(c);                     ChClass := 2 END Char;
PROCEDURE NewLine*;               BEGIN Out.Ln;                          ChClass := 3 END NewLine;
PROCEDURE Integer*(i,n: LONGINT); BEGIN Break(ChClass, 0); Out.Int(i,n); ChClass := 0 END Integer;
PROCEDURE Hex*(i,n: LONGINT);     BEGIN Break(ChClass, 0); Out.Hex(i,n); ChClass := 0 END Hex;

PROCEDURE NoBreak*;   BEGIN ChClass := 2 END NoBreak;
PROCEDURE Flush*;     BEGIN Out.Flush END Flush;
PROCEDURE StartLine*; BEGIN IF ChClass < 3 THEN NewLine END; Flush END StartLine;

PROCEDURE StringLength*(s: ARRAY OF CHAR): LONGINT;
VAR result: LONGINT;
BEGIN result := 0; WHILE (result < LEN(s)) & (s[result] # 0X) DO INC(result) END;
RETURN result END StringLength;

PROCEDURE String*(VAR s: ARRAY OF CHAR);
VAR l: LONGINT;
BEGIN
  l := StringLength(s);
  IF l > 0 THEN Break(ChClass, Classify(s[0])); Out.String(s); ChClass := Classify(s[l-1]) END
END String;

BEGIN NoBreak;
END TextWriter.
