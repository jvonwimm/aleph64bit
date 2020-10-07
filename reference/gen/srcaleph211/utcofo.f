      SUBROUTINE UTCOFO(COLTY,COLFO,FORMA)
C ---------------------------------------------------------------------
C.
C! - Get the format for the printing of the attributes
C.
C. - Author   : A. Putzer  - 87/08/18
C. - Modified : A. Putzer  - 89/03/14
C.
C.
C.   Arguments: -  COLTY CHA*4 (input) Generic column type
C.              -  COLFO CHA*8 (input) Format as the variable is stored
C.              -  FORMA CHA*4 (output) Array containing the format for
C.                                      the printing of the colums
C.              -  INDC  INTE  (input) INDEX (BOS) for the .COL table
C.              -  LUNPR INTE  (input) Unit number for print output
C.
C ---------------------------------------------------------------------
C
      PARAMETER (NFORM=23)
      CHARACTER*4 COLTY,FORMA(*)
      CHARACTER*8 COLFO
      CHARACTER*8 FORM1(NFORM)
      CHARACTER*4 ICHA4,ICHA8,ICH16,IINTE,ISNUM,IIREL,IIGEN,IREAL
      CHARACTER*8 ICOMM
      CHARACTER*12 IFREA,IFCHA,IFINT
      CHARACTER*12 FORM2(NFORM)
      DATA ICHA4/'CHA4'/,ICHA8/'CHA8'/,ICH16/'CH16'/,IIGEN/'GEN '/
      DATA IINTE/'INTE'/,ISNUM/'SNUM'/,IIREL/'REL '/,IREAL/'REAL'/
      DATA IFREA/'E11.5   '/,IFCHA/'1X,A4,6X'/,IFINT/'I10,1X'/
      DATA FORM2/'F10.4,1X'  ,'1X,F9.6,1X','F11.5  ',
     +           '6X,F4.2,1X','5X,F5.1,1X','5X,F5.3,1X',
     +           '  F10.6,1X','6X,F4.1,1X','2X,F8.6,1X',
     +           '3X,F7.4,1X','7X,F3.1,1X','4X,F6.4,1X',
     +           '3X,F7.5,1X','4X,F6.2,1X','5X,F5.2,1X',
     +           '3X,F7.3,1X','2X,F8.4,1X','4X,F6.3,1X',
     +           '4X,F6.1,1X','  F11.6   ','2X,F8.5,1X',
     +           '   F11.3  ','  F11.1   '             /
      DATA FORM1/'F10.4   ','F9.6   ','F11.5  ',
     +           'F4.2    ','F5.1   ','F5.3   ',
     +           'F10.6   ','F4.1   ','F8.6   ',
     +           'F7.4    ','F3.1   ','F6.4   ',
     +           'F7.5    ','F6.2   ','F5.2   ',
     +           'F7.3    ','F8.4   ','F6.3   ',
     +           'F6.1    ','F12.6  ','F8.5   ',
     +           'F13.3   ','F11.1  '          /
      DATA ICOMM/',''|'','/
C
C - Find the generic column type and store the corresponding print forma
C
C - Character format = A4
C
      IF (COLTY.EQ.ICHA4.OR.COLTY.EQ.ICHA8.OR.COLTY.EQ.ICH16.
     X    OR.COLTY.EQ.IIGEN) THEN
        CALL UTCCOP(IFCHA,FORMA,3)
C
C - Integer format = I10
C
      ELSEIF (COLTY.EQ.IINTE.OR.COLTY.EQ.ISNUM.OR.COLTY.EQ.IIREL) THEN
        CALL UTCCOP(IFINT,FORMA,3)
C
C - Floating point format
C
      ELSE
C
C - Decide on precise format
C
        DO 1000 I=1,NFORM
          IF(COLFO.EQ.FORM1(I)) THEN
            CALL UTCCOP(FORM2(I),FORMA,3)
            GO TO 2000
          ENDIF
 1000   CONTINUE
        CALL UTCCOP(IFREA,FORMA,3)
 2000   CONTINUE
      ENDIF
      CALL UTCCOP(ICOMM,FORMA(4),2)
      RETURN
      END
