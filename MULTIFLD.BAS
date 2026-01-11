'I think many QB programmers will find this routine usefull. I wrote it
'because I HAD to have it. QUICKLY. Having recieved so much good stuff from
'QuickShare, I figure this small donation is the least I can do for now.
'I have since this was written, refined this and other high level routines
'which I will eventually make available at the usual cost - FREE.
'Use this code 'till your sick of it. It has served its purpose for me.
'Laugh at it 'till your gut hurts.
'When I wrote this, I threw it together from pieces parts of other
'things I had handy. It certainly is not optimized but it works fairly well.
'Jerry C. Jackson - Deland, Fl.
  
   DEFINT A-Z
  
' Define color constants
  
   CONST black = 0
   CONST blue = 1
   CONST green = 2
   CONST cyan = 3
   CONST red = 4
   CONST magenta = 5
   CONST brown = 6
   CONST white = 7
   CONST bright = 8
   CONST blink% = 16
   CONST yellow = brown + bright
  
'define constants used by the KeyCode% function CVI(a$ + STRING$(2, 0))
   CONST FALSE = 0
   CONST TRUE = NOT FALSE
   CONST BACKSPACE = 8
   CONST CTRLLEFTARROW = 29440
   CONST CTRLRIGHTARROW = 29696
   CONST CTRLY = 25
   CONST CTRLQ = 17
   CONST DEL = 21248
   CONST ENDKEY = 20224
   CONST ENTER = 13
   CONST ESCAPE = 27
   CONST HOME = 18176
   CONST INSERTKEY = 20992
   CONST UPARROW = 18432
   CONST DOWNARROW = 20480
   CONST LEFTARROW = 19200
   CONST RIGHTARROW = 19712
   CONST TABKEY = 9
   CONST SHIFTTABKEY = 3840
   CONST PGUP = 18688
   CONST PGDN = 20736
   CONST F10 = 17408
   CONST CTRLHOME = 30464
   CONST CTRLEND = 29952

'define some more constants

   CONST illegal$ = "t255o0g64o2c64o0g64o2c64o0g64o2c64"
   CONST fullfield$ = "t255o4g64o2c64o0g64o2c64o0g64o2c64"
   CONST click$ = "t255o6c64"
   CONST upperalpha$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   CONST loweralpha$ = "abcdefghijklmnopqrstuvwxyz"
   CONST decimal$ = "."
   CONST number$ = "0123456789"
   CONST otherkeys$ = "!@#$%^&*()-_=+[]{};':,./<>?\|`~"
   CONST c32$ = " "
  
  
   TYPE RegType
      ax    AS INTEGER
      bx    AS INTEGER
      cx    AS INTEGER
      dx    AS INTEGER
      bp    AS INTEGER
      si    AS INTEGER
      di    AS INTEGER
      flags AS INTEGER
   END TYPE
  
  
'make arrays dynamic
' $DYNAMIC
  
   TYPE MultiFieldType
     
      edgeLine       AS INTEGER
      ulrow          AS INTEGER
      ulcol          AS INTEGER
      lrrow          AS INTEGER
      lrcol          AS INTEGER
      fgedge         AS INTEGER
      bgedge         AS INTEGER
      fgbody         AS INTEGER
      bgbody         AS INTEGER
      fgNameActive       AS INTEGER
      bgNameActive       AS INTEGER
      fgNameInactive     AS INTEGER
      bgNameInactive     AS INTEGER
      fgValueActive  AS INTEGER
      bgValueActive  AS INTEGER
      fgValueInactive   AS INTEGER
      bgValueInactive   AS INTEGER
      fgtitle        AS INTEGER
      bgtitle        AS INTEGER
      fgPrompt       AS INTEGER
      bgPrompt       AS INTEGER
      fghelp         AS INTEGER
      bghelp         AS INTEGER
      rowhelp        AS INTEGER
     
   END TYPE
  
  
   DECLARE FUNCTION KeyCode% ()
   DECLARE FUNCTION EdKeyCode% ()
  
   DECLARE SUB EndProg ()
   DECLARE SUB VideoState (mode%, columns%, page%)
   DECLARE SUB EditCustomerInfo ()
   DECLARE SUB MFI (mf AS MultiFieldType, mfFieldName$(), mfFieldCap%(), mfFieldValue$(), mfFieldMask$(), mfFieldLen%(), mfFieldPos(), mfFieldHelp$(), mfTitle$, mfPrompt$, mfScrollFileSpec$())
   DECLARE SUB INTERRUPT (intnum%, inreg AS RegType, outreg AS RegType)
   DECLARE SUB GetLinesInFile (filespec$, size%)
   DECLARE SUB Hold ()
   DECLARE SUB EndProgram ()
   DECLARE SUB SetCursor (row%, col%, fc%, bc%)
   DECLARE SUB MFIedit (a$, strlen%, mask$, extramask$, exitcode%, fg%, bg%)
  
  
  
   DIM SHARED allkeys$
   allkeys$ = upperalpha$ + loweralpha$ + number$ + decimal$ + otherkeys$
  
'get # of lines in file
  
   filespec$ = "0001.dat"
   GetLinesInFile filespec$, n%
   n% = n% - 15 'don't dimension anything for the file comment.
  
'now that we know how big to dim the arrays, do it.
   DIM SHARED mf1 AS MultiFieldType, mf1FieldName$(1 TO n%), mf1FieldCap%(1 TO n%), mf1FieldValue$(1 TO n%), mf1FieldMask$(1 TO n%, 1 TO 2), mf1FieldLen%(1 TO n%), mf1FieldPos%(1 TO n%, 1 TO 2), mf1FieldHelp$(1 TO n%), mf1Title$, mf1Prompt$,  _
mf1ScrollFileSpec$(1 TO n%)
  
  
'don't get lost in video pages just yet
   SCREEN , , 0, 0
  
   EditCustomerInfo
  
   EndProg

REM $STATIC
   SUB EditCustomerInfo
     
      filespec$ = "0001.dat"
      GetLinesInFile filespec$, n%
      n% = n% - 15   'don't count the file "header" as field items
     
'field information is stored in data files to save memory
     
      filenum% = FREEFILE
      OPEN filespec$ FOR INPUT AS filenum%
     
      FOR p2% = 1 TO 15         ''trash the file comments
         LINE INPUT #1, trash$
      NEXT p2%
     
      FOR p1% = 1 TO n%         'read the comma delimited data from the file
         INPUT #1, t$, mf1FieldName$(p1%), mf1FieldHelp$(p1%), mf1FieldPos%(p1%, 1), mf1FieldPos%(p1%, 2), mf1FieldLen%(p1%), mf1FieldCap%(p1%), mf1FieldMask$(p1%, 1), mf1FieldMask$(p1%, 2), mf1ScrollFileSpec$(p1%)
      NEXT p1%
     
      CLOSE filenum%
     
     
     
'--------------------------------------------------------------------------
'this stuff could also become part of the data file, but I got lazy <grin>
     
      mf1.edgeLine = 1
      mf1.ulrow = 1
      mf1.ulcol = 1
      mf1.lrrow = 23
      mf1.lrcol = 80
      mf1.fgedge = cyan
      mf1.bgedge = blue
      mf1.fgbody = white
      mf1.bgbody = black
      mf1.fgtitle = yellow
      mf1.bgtitle = blue
      mf1.fgPrompt = yellow
      mf1.bgPrompt = blue
      mf1.fgNameActive = yellow
      mf1.bgNameActive = red
      mf1.fgNameInactive = green
      mf1.bgNameInactive = black
      mf1.fgValueActive = black
      mf1.bgValueActive = white
      mf1.fgValueInactive = cyan + bright
      mf1.bgValueInactive = black
      mf1.rowhelp = 25
      mf1.fghelp = cyan + bright
      mf1.bghelp = blue
      mf1Title$ = " Enter Customer / Vehicle Information Below "
      mf1Prompt$ = " TAB / SHIFT-TAB to move between fields þ F10 when finished "
     
      CLS
     
'This is it! After you've read the info from disk and set the other variables
'all you do is call this routine and input to your hearts content.
'Play with the colors above and see how easy it is to make it look different.
'Play with the data file "0001.dat", but don't get the comma delimited
'data out of place.
     
      MFI mf1, mf1FieldName$(), mf1FieldCap%(), mf1FieldValue$(), mf1FieldMask$(), mf1FieldLen%(), mf1FieldPos%(), mf1FieldHelp$(), mf1Title$, mf1Prompt$, mf1ScrollFileSpec$()
     
     
      SCREEN , , 0, 0
     
   END SUB

DEFSNG A-Z
   FUNCTION EdKeyCode% STATIC
     
      DO
         k$ = INKEY$
      LOOP UNTIL k$ <> ""
      EdKeyCode% = CVI(k$ + CHR$(0))
     
     
     
   END FUNCTION

DEFINT A-Z
'
   SUB EndProg

'whatever code you need to clean up after yourself goes in here too.
     
      SCREEN , , 0, 0
      COLOR 7, 0
      CLS
      END
     
     
   END SUB

'
   SUB GetLinesInFile (filespec$, size%)
     
     
'---------------------------------------------------------------------------
'
'  This sub finds the exact size for the array dimension so that no memory
'  gets wasted. A little less than elegant, but I was in a hurry.
'
'---------------------------------------------------------------------------
     
     
      filenum% = FREEFILE
      OPEN filespec$ FOR INPUT AS filenum%
     
      size% = 0
     
      DO WHILE NOT EOF(filenum%)
         size% = size% + 1
         LINE INPUT #1, test$
      LOOP
     
      CLOSE filenum%
     
     
     
   END SUB

   SUB Hold
      DO UNTIL LEN(INKEY$): LOOP
   END SUB

DEFSNG A-Z
   FUNCTION KeyCode% STATIC
     
      KeyCode% = CVI(INKEY$ + STRING$(2, 0))
     
   END FUNCTION

DEFINT A-Z
'****************************************************************************
'        MultiFieldInputWindow - the ALL purpose input window
'****************************************************************************
'
'  I would like to add the ability to pull up a scrollable select window
'  to point and shoot values that will be plugged into the active field.
'
'
   SUB MFI (mf AS MultiFieldType, mfFieldName$(), mfFieldCap%(), mfFieldValue$(), mfFieldMask$(), mfFieldLen%(), mfFieldPos(), mfFieldHelp$(), mfTitle$, mfPrompt$, mfScrollFileSpec$()) STATIC
     
     
     
'Record current cursor location
      cursorRow% = CSRLIN
      cursorCol% = POS(0)
     
'Determine current video page
      CALL VideoState(mode%, columns%, page%)
     
'window will be on next page if available
      newpage% = page% + 1
     
      IF newpage% > 7 THEN
         SCREEN , , 0, 0
         PRINT "ERROR: MFI - not enough video pages"
         SYSTEM
      END IF
     
'Copy current page to new page
      PCOPY page%, newpage%
     
'---------------------------------------------------------------------------
'print to the apage while still looking at original vpage
     
      SCREEN , , newpage%, page%  'for "popping" the window onto the screen
     
'after all the screen is printed to the not yet visible page, then make it visible
'SCREEN , , newpage%, newpage%   'finally make the new page visible
'---------------------------------------------------------------------------
     
'determine how many fields this MFI window has
      lbField% = LBOUND(mfFieldName$)
      ubField% = UBOUND(mfFieldName$)
     
     
'double check the array bounds - lbField% must be 1
      IF lbField% <> 1 OR ubField% < 1 THEN
         SCREEN , , 0, 0
         PRINT "ERROR: MFI - text array dimensioned incorrectly"
         SYSTEM
      END IF
     
' Check that MFI window is on screen
     
      IF mf.ulrow < 1 OR mf.ulcol < 1 OR mf.lrrow > 25 OR mf.lrcol > columns% THEN
         SCREEN , , 0, 0
         PRINT "Error: MFI - part of MFI window is off screen"
         PRINT mf.ulrow, mf.ulcol, mf.lrrow, mf.lrcol, columns%
         SYSTEM
      END IF
     
'determine the width and height of window to be displayed
     
      mfwidth% = mf.lrcol - mf.ulcol - 1
      mfheight% = mf.lrrow - mf.ulrow
     
' Set the edge characters
     
      SELECT CASE mf.edgeLine
      CASE 0
         ul% = 32
         ur% = 32
         ll% = 32
         lr% = 32
         vl% = 32
         hl% = 32
      CASE 1
         ul% = 218
         ur% = 191
         ll% = 192
         lr% = 217
         vl% = 179
         hl% = 196
      CASE 2
         ul% = 201
         ur% = 187
         ll% = 200
         lr% = 188
         vl% = 186
         hl% = 205
      CASE IS > 2
         t% = mf.edgeLine
         ul% = t%
         ur% = t%
         ll% = t%
         lr% = t%
         vl% = t%
         hl% = t%
      CASE ELSE
         SCREEN , , 0, 0
         PRINT "Error: MFI - Edge line type incorrect"
         SYSTEM
      END SELECT
     
' Draw top edge of the box
     
      LOCATE mf.ulrow, mf.ulcol, 0
      COLOR mf.fgedge, mf.bgedge
      PRINT CHR$(ul%); STRING$(mfwidth%, hl%); CHR$(ur%);
     
' Draw the body of the window
     
      FOR r% = mf.ulrow + 1 TO mf.lrrow - 1
         LOCATE r%, mf.ulcol, 0
         COLOR mf.fgedge, mf.bgedge
         PRINT CHR$(vl%);
         COLOR mf.fgbody, mf.bgbody
        
         tmp$ = SPACE$(mfwidth% - 2)
         PRINT " "; tmp$; " ";
        
         COLOR mf.fgedge, mf.bgedge
         PRINT CHR$(vl%);
      NEXT r%
     
' Draw bottom edge of the window box
      LOCATE mf.lrrow, mf.ulcol, 0
      COLOR mf.fgedge, mf.bgedge
      PRINT CHR$(ll%); STRING$(mfwidth%, hl%); CHR$(lr%);
     
' Center and print top title if present
      IF mfTitle$ <> "" THEN
         LOCATE mf.ulrow, (mf.ulcol + mf.lrcol - LEN(mfTitle$) + 1) \ 2, 0
         COLOR mf.fgtitle, mf.bgtitle
         PRINT mfTitle$;
      END IF
     
' Center and print prompt if present
      IF mfPrompt$ <> "" THEN
         LOCATE mf.lrrow, (mf.ulcol + mf.lrcol - LEN(mfPrompt$) + 1) \ 2, 0
         COLOR mf.fgPrompt, mf.bgPrompt
         PRINT mfPrompt$;
      END IF
     
'un-comment for double check of array boundries
'SetCursor mf.ulrow + 2, mf.ulcol + 2, 0, 7
'PRINT " LBOUND="; lbField%; " and UBOUND="; ubField%
'END
     
     
'get row, col, colors, etc. from variables and print
'field names in the Inactive colors so they're all visible
     
      FOR t% = 1 TO ubField%
        
         SetCursor mfFieldPos%(t%, 1), mfFieldPos%(t%, 2), mf.fgNameInactive, mf.bgNameInactive
         PRINT mfFieldName$(t%);
        
         COLOR mf.fgValueInactive, mf.bgValueInactive
        
'pad the fields with spaces
         p$ = mfFieldValue$(t%) + STRING$(ABS(mfFieldLen%(t%)) - ABS(LEN(mfFieldValue$(t%))), 32)
        
         PRINT p$;
        
      NEXT t%
     
'finally make the new page visible after it's ready to be displayed
     
      SCREEN , , newpage%, newpage%
     
'----------------------------------------------
'initiate a loop that does everything else.
     
     
      ptr% = 1
     
      DO
         IF ptr% > ubField% THEN ptr% = 1
         IF ptr% = 0 THEN ptr% = ubField%
        
        
         IF mf.rowhelp THEN     'pad the help line with spaces and print it
            COLOR mf.fghelp, mf.bghelp
            mfFieldHelp$(ptr%) = mfFieldHelp$(ptr%) + STRING$(80 - LEN(mfFieldHelp$(ptr%)), 32)
            LOCATE mf.rowhelp, 1
            PRINT mfFieldHelp$(ptr%);
         END IF
        
         SetCursor mfFieldPos%(ptr%, 1), mfFieldPos%(ptr%, 2), mf.fgNameActive, mf.bgNameActive
         PRINT mfFieldName$(ptr%);
        
        
         mask$ = mfFieldMask$(ptr%, 1)
         extramask$ = mf1FieldMask$(ptr%, 2)
         exitcode% = mfFieldCap%(ptr%)          'set caps flag
        
wrongkey:
        
'uncomment here for sending string length into editor for testing
'         mfFieldValue$(ptr%) = LTRIM$(RTRIM$(STR$(mfFieldLen%(ptr%))))
        
         MFIedit mfFieldValue$(ptr%), mfFieldLen%(ptr%), mask$, extramask$, exitcode%, mf.fgValueActive, mf.bgValueActive
        
         SELECT CASE exitcode%
         CASE CTRLEND
            endflag% = TRUE
         CASE CTRLHOME
            homeflag% = TRUE
         CASE TABKEY, ENTER, DOWNARROW, PGDN
            direction% = 1
         CASE SHIFTTABKEY, PGUP, UPARROW
            direction% = 0
         CASE F10
            EXIT DO
         CASE ELSE
            GOTO wrongkey       ''go get another exitcode%
         END SELECT
        
        
        
         SetCursor mfFieldPos%(ptr%, 1), mfFieldPos%(ptr%, 2), mf.fgNameInactive, mf.bgNameInactive
         PRINT mfFieldName$(ptr%);
        
         COLOR mf.fgValueInactive, mf.bgValueInactive
         PRINT mfFieldValue$(ptr%);
        
        
        
         SELECT CASE direction%
         CASE 1
            ptr% = ptr% + 1
         CASE 0
            ptr% = ptr% - 1
         END SELECT
        
         IF homeflag% THEN ptr% = 1: homeflag% = FALSE
         IF endflag% THEN ptr% = ubField%: endflag% = FALSE
        
        
      LOOP
     
     
   END SUB

'*****************************************************************************
'  SUB MFIedit  (Multi Field Input Edit)
'*****************************************************************************
'
   SUB MFIedit (a$, flen%, mask$, extramask$, exitcode%, fg%, bg%) STATIC       'keep all variables local
     
      IF flen% = 0 THEN flen% = LEN(a$)         'use space already allocated
     
     
'uncomment this line to show field lengths upon field entry for testing
'a$ = LTRIM$(RTRIM$(STR$(strlen%)))
     
     
      clik% = 0
      IF flen% < 0 THEN clik% = TRUE
      strlen% = ABS(flen%)
     
     
''set up some variables
      row% = CSRLIN
      col% = POS(0)
      length% = strlen%
      ptr% = 0  ''position in the string to place the cursor to start out
      inserton% = TRUE          ''default starts in insert mode
      quit% = FALSE
      original$ = a$
      caps% = exitcode%
     
      accept$ = ""
     
'mask specifiers
'
' 1 - allkeys$
' 2 - number$
' 3 - c32$
' 4 - upperalpha$
' 5 - loweralpha$
' 6 - otherkeys$
' 7 - decimal$
     
      IF INSTR(mask$, "1") THEN accept$ = accept$ + allkeys$
      IF INSTR(mask$, "2") THEN accept$ = accept$ + number$
      IF INSTR(mask$, "3") THEN accept$ = accept$ + c32$
      IF INSTR(mask$, "4") THEN accept$ = accept$ + upperalpha$
      IF INSTR(mask$, "5") THEN accept$ = accept$ + loweralpha$
      IF INSTR(mask$, "6") THEN accept$ = accept$ + otherkeys$
      IF INSTR(mask$, "7") THEN accept$ = accept$ + decimal$
     
      IF INSTR(mask$, "c") THEN shortkeyclick% = TRUE
      IF INSTR(mask$, "C") THEN longkeyclick% = TRUE
     
     
      accept$ = accept$ + extramask$
     
      COLOR fg%, bg%
     
'''' ******* Main processing loop ********
     
      DO
        
'Display the line
         LOCATE row%, col%, 0
         a$ = a$ + SPACE$(length% - LEN(a$))
         PRINT a$;
        
'Show appropriate cursor type
        
        
         IF inserton% THEN
            LOCATE row%, col% + ptr%, 1, 5, 7
         ELSE
            LOCATE row%, col% + ptr%, 1, 0, 7
         END IF
        
'Get next keystroke
        
        
mfi0:
        
         keynumber% = EdKeyCode%
        
        
        
'process the key
         SELECT CASE keynumber%
         CASE INSERTKEY
            IF inserton% THEN
               inserton% = FALSE
            ELSE
               inserton% = TRUE
            END IF
           
         CASE BACKSPACE
            IF ptr% THEN
               a$ = a$ + " "
               a$ = LEFT$(a$, ptr% - 1) + MID$(a$, ptr% + 1)
               ptr% = ptr% - 1
            END IF
           
         CASE DEL
            a$ = a$ + " "
            a$ = LEFT$(a$, ptr%) + MID$(a$, ptr% + 2)
           
CASE UPARROW  'these will be used in the scroll select routine
   exitcode% = UPARROW
   quit% = TRUE
           
CASE DOWNARROW
   exitcode% = DOWNARROW
   quit% = TRUE
           
         CASE LEFTARROW
            IF ptr% THEN
               ptr% = ptr% - 1
            END IF
           
         CASE RIGHTARROW
            IF ptr% < length% - 1 THEN
               ptr% = ptr% + 1
            END IF
           
         CASE ESCAPE
'exitcode% = ESCAPE
            a$ = original$
            quit% = FALSE
           
         CASE ENTER
            exitcode% = ENTER
            quit% = TRUE
           
         CASE SHIFTTABKEY
            exitcode% = SHIFTTABKEY
            quit% = TRUE
           
         CASE TABKEY
            exitcode% = TABKEY
            quit% = TRUE
           
         CASE CTRLHOME
            exitcode% = CTRLHOME
            quit% = TRUE
           
         CASE CTRLEND
            exitcode% = CTRLEND
            quit% = TRUE
           
         CASE F10               'change this value for other exit keys
            exitcode% = F10
            quit% = TRUE
           
         CASE PGUP
            exitcode% = PGUP
            quit% = TRUE
           
         CASE PGDN
            exitcode% = PGDN
            quit% = TRUE
           
         CASE HOME
            ptr% = 0
           
         CASE ENDKEY
            ptr% = length% - 1
           
         CASE CTRLRIGHTARROW
            DO UNTIL MID$(a$, ptr% + 1, 1) = " " OR ptr% = length% - 1
               ptr% = ptr% + 1
            LOOP
            DO UNTIL MID$(a$, ptr% + 1, 1) <> " " OR ptr% = length% - 1
               ptr% = ptr% + 1
            LOOP
           
         CASE CTRLLEFTARROW
            DO UNTIL MID$(a$, ptr% + 1, 1) = " " OR ptr% = 0
               ptr% = ptr% - 1
            LOOP
            DO UNTIL MID$(a$, ptr% + 1, 1) <> " " OR ptr% = 0
               ptr% = ptr% - 1
            LOOP
            DO UNTIL MID$(a$, ptr% + 1, 1) = " " OR ptr% = 0
               ptr% = ptr% - 1
            LOOP
            IF ptr% THEN
               ptr% = ptr% + 1
            END IF
           
         CASE CTRLY
            a$ = SPACE$(length%)
            ptr% = 0
           
         CASE CTRLQ
            ctrlqflag% = TRUE
           
         CASE ESCAPE
            a$ = original$
            ptr% = 0
            inserton% = TRUE
           
         CASE IS > 255
            PLAY illegal$
           
         CASE IS < 32
            PLAY illegal$
           
         CASE ELSE
           
'convert key code to character string
           
            kee$ = CHR$(keynumber%)
           
            IF INSTR(accept$, kee$) THEN GOTO mfi2
           
mfi1:
           
            PLAY illegal$
            GOTO mfi0
           
           
mfi2:
           
            IF NOT quit% THEN
               IF clik% THEN
                  IF LEN(RTRIM$(LTRIM$(a$))) > strlen% THEN
                     PLAY "t255 o3 g64o2c64o4g64o2c64o4g64o2c64"
                  END IF
                 
                  PLAY click$
               END IF
            END IF
           
           
           
'insert or overstrike
            IF inserton% THEN
               a$ = LEFT$(a$, ptr%) + kee$ + MID$(a$, ptr% + 1)
               a$ = LEFT$(a$, length%)
            ELSE
               IF ptr% < length% THEN
                  MID$(a$, ptr% + 1, 1) = kee$
               END IF
            END IF
           
'are we up against the wall?
            IF ptr% < length% THEN
               ptr% = ptr% + 1
            ELSE
               PLAY fullfield$
            END IF
           
'special check for Ctrl-Q-Y (del to end of line)
            IF kee$ = "y" AND ctrlqflag% THEN
               IF ptr% <= length% THEN
                  sp% = length% - ptr% + 1
                  MID$(a$, ptr%, sp%) = SPACE$(sp%)
                  ptr% = ptr% - 1
               END IF
            END IF
           
            IF caps% THEN       ''''if caps flag then auto capitalize the string
               a$ = UCASE$(a$)
            END IF
           
'clear out ctrl-q signal
            ctrlqflag% = FALSE
           
           
         END SELECT
        
      LOOP UNTIL quit%
     
   END SUB

DEFSNG A-Z
'*****************************************************************************
'                       SUB - SetCursor
'*****************************************************************************
'
'
   SUB SetCursor (row%, col%, fc%, bc%)
     
      COLOR fc%, bc%
     
      LOCATE row%, col%, 0
     
   END SUB

'
   SUB VideoState (mode%, columns%, page%) STATIC
      DIM reg AS RegType
      reg.ax = &HF00
      INTERRUPT &H10, reg, reg
      mode% = reg.ax AND &HFF
      columns% = (CLNG(reg.ax) AND &HFF00) \ 256
      page% = (CLNG(reg.bx) AND &HFF00) \ 256
   END SUB

