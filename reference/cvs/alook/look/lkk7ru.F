      FUNCTION LKK7RU (LRUK7,NRUN,TYPE,TAPE,FILENM,FORMA)
C ------------------------------------------------------------
C! get the filename and format for a given run ofa given type
C - F.Ranjard - 920116
C - Input   : - LRUK7   / I = logical unit number for
C                             RUNCARTS.LIST
C               NRUN    / I = run number
C               TYPE    / A = data type 'RAW/POT/DST/MINI/NANO'
C
C - Output  : - TAPE    / A = tape number
C               FILENM  / A = file name which contains the run
C               FORMA   / A = file format
C               LKK7RU  / I = return code
C                             = 0  ok
C                             = 1  cannot open RUK7FILE
C                             = 2  file is empty
C                             = 3  too many runs, increase K7COM
C                             = 13 wrong data type
C                             = 14 run does not exist
C                             = 15 no tape avalaible
C                             = 16 cannot open input file
C                             = 17 run exists on the current file
C                             = 23 wrong data type
C                             = 24 JDRUNL bank does not exist
C                             = 25 TAPE does not exist
C
C ------------------------------------------------------------
#ifndef DOC
      CHARACTER*(*) FILENM, FORMA, TAPE, TYPE
#include "bcs.h"
#include "lkfil.h"
      LOGICAL LEX
      INTEGER ALK7FRU, ALK7TRU
      CHARACTER CHAINT*4, TYPOLD*3
      DATA JDRUNL/0/ , TYPOLD/'   '/
C ----------------------------------------------------------
C
      IRK7 = ALK7FRU (LRUK7,NRUN,TYPE,TAPE)
      IF (IRK7.NE.0) THEN
         IF (IRK7.EQ.1) THEN
            WRITE(LOUT,*) ' [Cannot open RUNCARTS.LIST file]'
         ELSEIF (IRK7.EQ.2) THEN
            WRITE(LOUT,*) ' [RUCARTS.LIST file is empty]'
         ELSEIF (IRK7.EQ.3) THEN
            WRITE(LOUT,*) ' [too many runs,tell F.Ranjard to ',
     &                    'increase ALK7COM size'
         ELSEIF (IRK7.EQ.13) THEN
            WRITE(LOUT,*) ' [wrong data type ',TYPE,' ]'
         ELSEIF (IRK7.EQ.14) THEN
            WRITE(LOUT,*) ' [run ',NRUN,' does not exist in ',
     &                    'RUNCARTS.LIST]'
         ENDIF
         GOTO 999
      ELSE
         IF (TAPE.EQ.' ') THEN
            WRITE(LOUT,*)' [No tape avalaible for this run]'
            IRK7 = 15
            GOTO 999
         ELSE
C           get the list of runs avalaible on TAPE
            IRK7 = ALK7TRU (LRUK7,TYPE,TAPE,JDRUNL)
            IF (IRK7.EQ.0) THEN
               CALL BKFRW (IW,'RUNL',0,IW,JDRUNL,*4)
 4             CALL BKFMT ('RUNL','3A,(I)')
            ENDIF
            LT  = LNBLNK(TAPE)
            FILENM = TAPE(1:LT)
            IF (TYPE.EQ.'DST' .OR. TYPE.EQ.'MINI') THEN
C            read the EDIR file
               FORMA = 'EDIR'
               FILENM = FILENM(1:LT)//'.EDIR'
            ELSE
C            read the EPIO file
               FORMA  = 'CART'
            ENDIF
         ENDIF
      ENDIF
      GOTO 999
 998  CONTINUE
      NOLD = NRUN
      TYPOLD = TYPE(1:3)
 999  CONTINUE
      LKK7RU = IRK7
      END
#endif
