MODULE fasmlist;

IMPORT TextWriter, w, Modules, Platform, SYSTEM;

TYPE
  i64 = SYSTEM.INT64;
  i32 = SYSTEM.INT32;
  i16 = SYSTEM.INT16;
  i8  = SYSTEM.INT8;

  File = POINTER TO FileDesc;
  FileDesc = RECORD
    bytes:  POINTER TO ARRAY OF BYTE;
    length: i64;
  END;

  FasHeader = RECORD
    signature: i32;  (* Signature 1A736166h (little-endian) *)
    majorver:  i8;   (* Major version of flat assembler *)
    minorver:  i8;   (* Minor version of flat assembler *)
    length:    i16;  (* Length of header *)
    inputfn:   i32;  (* Offset of fasm input file name in the strings table *)
    outputfn:  i32;  (* Offset of fasm output file name in the strings table *)
    strtab:    i32;  (* Offset of strings table *)
    strlen:    i32;  (* Length of strings table *)
    symtab:    i32;  (* Offset of symbols table *)
    symlen:    i32;  (* Length of symbols table *)
    srctab:    i32;  (* Offset of preprocessed source *)
    srclen:    i32;  (* Length of preprocessed source *)
    asmtab:    i32;  (* Offset of assembly dump *)
    asmlen:    i32;  (* Length of assembly dump *)
    sectab:    i32;  (* Offset of section names table *)
    seclen:    i32;  (* Length of section names table *)
    srftab:    i32;  (* Offset of symbol references dump *)
    srflen:    i32;  (* Length of symbol references dump *)
  END;

  SourcePtr = POINTER [1] TO SourceRecord;
  SourceRecord = RECORD
    fileid:   i32;  (* Indicates name of file or macro generating this lien *)
    linenum:  i32;  (* Line number in file or macro *)
    linepos:  i32;  (* Offset of start of source line in source file *)
    macropos: i32;  (* Offset of preprocessed line within macro *)
  END;

  AssemblyPtr = POINTER [1] TO AssemblyRecord;
  AssemblyRecord = RECORD
    binoffset: i32;  (* Offset in fasm output file *)
    srcoffset: i32;  (* Offset in srctab *)
    taddress:  i64;  (* Target address  *)
    tsibreg:   ARRAY 2 OF BYTE; (* registers target is relative to *)
    tsibscale: ARRAY 2 OF BYTE; (* scale for each tsibreg *)
    tsecsym:   i32;  (* Section or symbol to which target is relative *)
    ttype:     BYTE; (* Relocation type *)
    codesize:  BYTE; (* Code size - 16/32/64 *)
    flags:     BYTE; (* 6/0, 1/not in output, 1/in virtual block *)
    tadrhi:    BYTE; (* Bits 39-32 of target pc address *)
  END;

VAR
  fasname:  ARRAY 300 OF CHAR;
  fas:      File;
  header:   FasHeader;
  codeend:  i32;

  sourceid: i32;
  source:   File;

  binary:   File;

  inclusive: BOOLEAN;  (* If included files should be included in listing *)
  curfile:   ARRAY 20 OF CHAR;  (* Name part of current file *)


PROCEDURE LoadFile(fn: ARRAY OF CHAR): File;
VAR
  fh:  Platform.FileHandle;
  err: Platform.ErrorCode;
  f:   File;
  i:   i32;
BEGIN
  NEW(f);

  err := Platform.OldRO(fn, fh);
  IF err # 0 THEN
    w.s("Couldn't read '"); w.s(fn); w.s("', error code "); w.i(err); w.sl('.');
    w.Fail("")
  END;

  err := Platform.Size(fh, f.length);
  IF err # 0 THEN
    w.s("Couldn't get size of '"); w.s(fn); w.s("', error code "); w.i(err); w.sl('.');
    w.Fail("")
  END;

  NEW(f.bytes, f.length);

  err := Platform.ReadBuf(fh, f.bytes^, f.length);
  IF err # 0 THEN
    w.s("Couldn't read '"); w.s(fn); w.s("', error code "); w.i(err); w.sl('.');
    w.Fail("")
  END;

  err := Platform.Close(fh);
  IF err # 0 THEN
    w.s("Couldn't close '"); w.s(fn); w.s("', error code "); w.i(err); w.sl('.');
  END;

  i := 0;
  WHILE (i < LEN(fn)) & (fn[i] # '.') & (fn[i] # 0X) & (i < LEN(curfile)-1) DO
    curfile[i] := fn[i];
    INC(i);
  END;
  curfile[i] := 0X;
RETURN f END LoadFile;


PROCEDURE ProcessHeader;
BEGIN
  w.Assert(fas.length >= SIZE(FasHeader), "Fas file header is incomplete");

  SYSTEM.MOVE(SYSTEM.ADR(fas.bytes[0]), SYSTEM.ADR(header), SIZE(FasHeader));

  w.Assert(header.signature = 1A736166H, "Fas file header signature incorrect.");

  (*
  w.s("FASM Listing. Assembler version "); w.i(header.majorver); w.c('.');
  w.i(header.minorver); w.sl(".");
  *)

  w.Assert(header.length >= SIZE(FasHeader), "Fas file header is incomplete");

  (* Extract ending binary file offset *)
  DEC(header.asmlen, 4);

  SYSTEM.GET(SYSTEM.ADR(fas.bytes[0]) + header.asmtab + header.asmlen, codeend);

  (* w.s("Code end: $"); w.x(codeend, 8); w.sl("."); *)
END ProcessHeader;


PROCEDURE AsmRec(offset: i64): AssemblyPtr;
BEGIN
  w.Assert(offset < header.asmlen, "AsmRec passed offset beyond asm table.");
  RETURN SYSTEM.VAL(AssemblyPtr, SYSTEM.ADR(fas.bytes[0]) + header.asmtab + offset);
END AsmRec;


PROCEDURE SrcRec(offset: i64): SourcePtr;
BEGIN
  w.Assert(offset < header.srclen, "SrcRec passed offset beyond src table.");
  RETURN SYSTEM.VAL(SourcePtr, SYSTEM.ADR(fas.bytes[0]) + header.srctab + offset);
END SrcRec;


PROCEDURE PatchAssemblyOffsets;
(*  Walk the assembly dump, patching the offsets of assembly entries
    into the source records that they correspond to.

    This codre repurposes the SourceRecord field 'macropos', replacing
    it with the offset of the assembly record corresponding to this source
    line, or with 0 if there is no assembly to be displayed.
*)
VAR
  asm:    AssemblyPtr;
  src:    SourcePtr;
  offset: i32;
  limit: i32;
BEGIN
  offset := 0;
  limit := header.asmlen;
  (* w.i(limit DIV 28); w.sl(' assembly dump records.'); *)

  WHILE offset < limit DO
    asm := AsmRec(offset);

    (*w.s("Asmrec["); w.i(offset); w.s("] target address: "); w.x(asm.taddress,8); w.sl('.'); *)

    src := SrcRec(asm.srcoffset);

    (* If this is in a macro work back to the original macro calling line *)
    WHILE src.linenum < 0 DO src := SrcRec(src.linepos) END;

    IF src.macropos = 0 THEN src.macropos := offset END; (* Record asm rec offset *)

    INC(offset, 28);
  END
END PatchAssemblyOffsets;


PROCEDURE SkipTokens(offset: i32) : i32;
VAR  kind, plen: BYTE;  dwlen: i32;
BEGIN
  REPEAT
    SYSTEM.GET(SYSTEM.ADR(fas.bytes[0]) + header.srctab + offset, kind);
    INC(offset);
    CASE kind OF
    | 1AH, 3BH: SYSTEM.GET(SYSTEM.ADR(fas.bytes[0]) + header.srctab + offset, plen);
                INC(offset, 1+plen)
    | 22H:      SYSTEM.GET(SYSTEM.ADR(fas.bytes[0]) + header.srctab + offset, dwlen);
                INC(offset, 4+dwlen)
    ELSE
    END
  UNTIL kind = 0;
RETURN offset END SkipTokens;


PROCEDURE WriteStringPos(pos: i32);  (* Write ASCIIZ string from fas file *)
VAR limit: SYSTEM.ADDRESS;
BEGIN
  limit := LEN(fas.bytes^);
  WHILE (pos < limit) & (fas.bytes[pos] # 0) DO
    w.c(CHR(fas.bytes[pos]));
    INC(pos);
  END
END WriteStringPos;

PROCEDURE LoadAsciizFile(pos: i32): File; (* Load file named in fas file content *)
VAR  fn: ARRAY 300 OF CHAR;  i: i32;  lim: SYSTEM.ADDRESS;
BEGIN
  (* w.s("LoadAsciizFile($"); w.x(pos,1); w.sl(")."); *)
  i := 0;  lim := 299;
  IF LEN(fas.bytes^) - pos > lim THEN lim := LEN(fas.bytes^) - pos END;
  WHILE (i < lim) & (fas.bytes[pos] # 0) DO
    fn[i] := CHR(fas.bytes[pos]);
    INC(i); INC(pos);
  END;
  fn[i] := 0X;
RETURN LoadFile(fn) END LoadAsciizFile;


PROCEDURE LoadSourceFile(fileid: i32);
VAR pos: i32;
BEGIN
  IF sourceid # fileid THEN
    sourceid := 0;
    IF fileid = 0 THEN
      pos := header.strtab + header.inputfn
    ELSE
      pos := header.srctab + fileid
    END;
    w.l; w.c('<'); WriteStringPos(pos); w.sl('>');
    source := LoadAsciizFile(pos);
    sourceid := fileid;
  END;
END LoadSourceFile;


PROCEDURE WriteSourceLine(VAR bytes: ARRAY OF BYTE; pos: i32);
BEGIN
  WHILE (pos < LEN(bytes)) & (bytes[pos] # 0DH) & (bytes[pos] # 0AH) DO
    w.c(CHR(bytes[pos]));
    INC(pos);
  END
END WriteSourceLine;

PROCEDURE WriteReg(n: i32): i32;  (* Write reg name from fasm code *)
BEGIN
  n := n MOD 256;
  CASE n OF
  | 23H: w.s('bx');   RETURN 2   | 25H: w.s('bp');   RETURN 2
  | 26H: w.s('si');   RETURN 2   | 27H: w.s('di');   RETURN 2
  | 40H: w.s('eax');  RETURN 3   | 41H: w.s('ecx');  RETURN 3
  | 42H: w.s('edx');  RETURN 3   | 43H: w.s('ebx');  RETURN 3
  | 44H: w.s('esp');  RETURN 3   | 45H: w.s('ebp');  RETURN 3
  | 46H: w.s('esi');  RETURN 3   | 47H: w.s('edi');  RETURN 3
  | 48H: w.s('r8d');  RETURN 3   | 49H: w.s('r9d');  RETURN 3
  | 4AH: w.s('r10d'); RETURN 4   | 4BH: w.s('r11d'); RETURN 4
  | 4CH: w.s('r12d'); RETURN 4   | 4DH: w.s('r13d'); RETURN 4
  | 4EH: w.s('r14d'); RETURN 4   | 4FH: w.s('r15d'); RETURN 4
  | 80H: w.s('rax');  RETURN 3   | 81H: w.s('rcx');  RETURN 3
  | 82H: w.s('rdx');  RETURN 3   | 83H: w.s('rbx');  RETURN 3
  | 84H: w.s('rsp');  RETURN 3   | 85H: w.s('rbp');  RETURN 3
  | 86H: w.s('rsi');  RETURN 3   | 87H: w.s('rdi');  RETURN 3
  | 88H: w.s('r8');   RETURN 2   | 89H: w.s('r9');   RETURN 2
  | 8AH: w.s('r10');  RETURN 3   | 8BH: w.s('r11');  RETURN 3
  | 8CH: w.s('r12');  RETURN 3   | 8DH: w.s('r13');  RETURN 3
  | 8EH: w.s('r14');  RETURN 3   | 8FH: w.s('r15');  RETURN 3
  | 94H: w.s('eip');  RETURN 3   | 98H: w.s('rip');  RETURN 3
  ELSE w.c('?'); w.x(n,3); RETURN 4
  END
END WriteReg;

PROCEDURE WriteScale(n: i32): i32;
VAR i: i32;
BEGIN i := 0;
  n := n MOD 256;
  IF n > 1 THEN
    w.c('*'); INC(i);
    IF n < 10H THEN
      w.x(n,1); INC(i,1)
    ELSIF n < 100H THEN
      w.x(n,2); INC(i,2)
    ELSE
      w.x(n,3); INC(i,3)
    END
  END;
RETURN i END WriteScale;


PROCEDURE GenerateListing;
VAR
  offset:   i32;  (* Offset into src table *)
  limit:    i32;  (* Source table limit *)
  src:      SourcePtr;
  asm:      AssemblyPtr;
  nextsrc:  SourcePtr;
  nextasm:  AssemblyPtr;
  nextoff:  i32;
  binfirst: i32;
  binlimit: i32;
  binaddr:  i64;
  i:        i32;
BEGIN
  binary   := LoadAsciizFile(header.strtab + header.outputfn);
  binfirst := 0;
  binlimit := 0;

  offset := 0;
  limit  := header.srclen;

  WHILE offset < limit DO
    src := SrcRec(offset);

    IF (src.linenum >= 0)  &  ((src.fileid = 0) OR inclusive) THEN
      LoadSourceFile(src.fileid);

      IF inclusive THEN w.s(curfile); w.nb END;

      w.in(src.linenum, 6); w.s(": ");

      IF src.macropos = 0 THEN
        w.s('                            ');
      ELSE
        asm := AsmRec(src.macropos);
        IF asm.flags MOD 2 # 0 THEN
          (* virtual block *)
          w.s('          ');
          i := 10;
          IF asm.tsibreg[0] # 0 THEN
            INC(i, WriteReg(asm.tsibreg[0]));
            INC(i, WriteScale(asm.tsibscale[0]));
            w.c('+'); INC(i)
          END;
          IF asm.tsibreg[1] # 0 THEN
            INC(i, WriteReg(asm.tsibreg[1]));
            INC(i, WriteScale(asm.tsibscale[1]));
            w.c('+'); INC(i)
          END;
          w.x(asm.taddress,4);  INC(i,4);
          WHILE i < 28 DO w.s(' '); INC(i) END;
        ELSE
          (* Determine start and limit of assembled binary *)
          binfirst := asm.binoffset;
          IF binfirst < binlimit THEN binfirst := binlimit END;

          (* Find next source line with non-virtual assembly *)
          nextoff := SkipTokens(offset+16);
          binlimit := -1;
          WHILE (nextoff < limit) & (binlimit < 0) DO
            nextsrc := SrcRec(nextoff);
            IF (nextsrc.linenum >= 0) & (nextsrc.macropos # 0) THEN
              nextasm := AsmRec(nextsrc.macropos);
              IF nextasm.flags = 0 THEN
                binlimit := nextasm.binoffset
              END
            END;
            IF binlimit < 0 THEN nextoff := SkipTokens(nextoff+16) END;
          END;
          IF binlimit < 0 THEN binlimit := codeend END;
          IF binfirst >= binlimit THEN
            w.s('                            ')
          ELSE
            w.x(asm.taddress,8);  w.s('  ');
            i := 0;
            WHILE (binfirst < binlimit) & (i < 8) DO
              w.x(binary.bytes[binfirst], 2);  w.nb;
              INC(binfirst); INC(i)
            END;
            WHILE i < 8 DO w.s('  '); INC(i) END;
            w.s('  ');
            binaddr := asm.taddress + 8
          END
        END
      END;

      WriteSourceLine(source.bytes^, src.linepos);  w.l;

      WHILE binfirst < binlimit DO
        IF inclusive THEN w.s(curfile); w.nb END;
        w.s('        ');
        w.x(binaddr,8);  w.s('  '); INC(binaddr, 8);
        i := 0;
        WHILE (binfirst < binlimit) & (i < 8) DO
          w.x(binary.bytes[binfirst], 2);  w.nb;
          INC(binfirst); INC(i)
        END;
        w.l
      END
    END;
    offset := SkipTokens(offset + 16);  (* Skip tokens following this line's source record *)
  END
END GenerateListing;

PROCEDURE GetOptions;
  VAR  i: i32;  arg: ARRAY 300 OF CHAR;  help, err: BOOLEAN;
BEGIN
  fasname   := '';
  inclusive := FALSE;
  err       := FALSE;
  help      := Modules.ArgCount < 2;

  i := 1;
  WHILE i < Modules.ArgCount DO
    arg[0] := 0X;
    Modules.GetArg(i, arg);
    IF    arg = '-i' THEN inclusive := TRUE
    ELSIF arg = '-h' THEN help := TRUE
    ELSIF arg = '-?' THEN help := TRUE
    ELSIF arg[0] = '-' THEN
          w.s("Unrecognised option '"); w.s(arg); w.sl("'."); err := TRUE;
    ELSE
      fasname := arg
    END;
    INC(i);
  END;

  IF err OR help THEN
    w.sl('fasmlist - generate listing for fasm assembly.');
    w.l;
    w.sl('Usage:');
    w.sl('  fasm <fn>.asm -s <fn>.fas');
    w.sl('  fasmlist <options> <fn>.fas');
    w.l;
    w.sl('Where <options>:');
    w.sl('  -i  --  show included files as well as <fn>.asm.');
    IF err THEN HALT(1) END;
  END
END GetOptions;

BEGIN
  sourceid := -1;
  GetOptions;
  IF fasname[0] # 0X THEN
    fas := LoadFile(fasname);
    ProcessHeader;
    PatchAssemblyOffsets;
    GenerateListing
  END
END fasmlist.




