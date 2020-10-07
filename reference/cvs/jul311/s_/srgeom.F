      SUBROUTINE SRGEOM
C----------------------------------------------------------------------
C! Fill SATR GEOMetry common block
C!
C!    Author:     H. Meinhard       19-Aug-1987
C!    Modified:   H. Meinhard       29-May-1990  (3)
C!
C!    Description
C!    ===========
C!    Constants describing the SATR geometry are filled into common
C!    block SGEOMC.
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "lalijj.h"
#include "lslojj.h"
#include "rcurnt.h"
#include "rlunit.h"
#include "saligc.h"
#include "satrjj.h"
#include "sgeomc.h"
#include "slayjj.h"
C>>> HM890307 TEMPORARY MOD FOR BACKWARD COMPATIBILITY
#include "sflagc.h"
C>>> END HM890307
      INTEGER ALGTDB,KSEC(MSCSG,MGRSG,MMDSG)
#include "bosext.h"
      EXTERNAL ALGTDB
      DATA KSEC/1,2,3,4,5,6,7,8, 1,3,2,4,5,7,6,8, 1,4,2,3,5,8,6,7/
#include "bmacro.h"
C----------------------------------------------------------------------
      ISTAT = ALGTDB(LRCONS,'LALILSLOSATRSLAY',IRUNRC)
      IF (ISTAT .EQ. 0)                                     GOTO 901
C
      KLALI = IW(NAMIND('LALI'))
      KLSLO = IW(NAMIND('LSLO'))
      KSATR = IW(NAMIND('SATR'))
      KSLAY = IW(NAMIND('SLAY'))
C
      DO 340 IMOD = 1, MHASA
        LCMOD = MHASA - IMOD + 1
        XSHISA(IMOD) = RTABL(KLSLO,LCMOD,JLSLXS) +
     +                 RTABL(KLALI,LCMOD,JLALDX)
        YSHISA(IMOD) = RTABL(KLSLO,LCMOD,JLSLXS+1) +
     +                 RTABL(KLALI,LCMOD,JLALDX+1)
        ZSHISA(IMOD) = RTABL(KLSLO,LCMOD,JLSLXS+2) +
     +                 RTABL(KLALI,LCMOD,JLALDX+2)
        YZANSA(IMOD) = - RTABL(KLSLO,LCMOD,JLSLRS) -
     +                 RTABL(KLALI,LCMOD,JLALDR)
        ZXANSA(IMOD) = - RTABL(KLSLO,LCMOD,JLSLRS+1) -
     +                 RTABL(KLALI,LCMOD,JLALDR+1)
        XYANSA(IMOD) = - RTABL(KLSLO,LCMOD,JLSLRS+2) -
     +                 RTABL(KLALI,LCMOD,JLALDR+2)
  340 CONTINUE
C
      RZERSG = RTABL(KSATR,1,JSATIM)
      RDELSG = RTABL(KSATR,1,JSATWD)
      ZZERSG = (ABS(ZSHISA(1)) + ABS(ZSHISA(2)) + ABS(ZSHISA(3)) +
     +  ABS(ZSHISA(4))) / 4.
      ZDELSG = RTABL(KSATR,1,JSATLD)
      ZOFFSG = RTABL(KSATR,1,JSATLO)
      DO 350 ILAY = 1, MLASG
        PHIBSG(ILAY) = RTABL(KSLAY,ILAY,JSLAAS)
        MDORSG(ILAY) = ITABL(KSLAY,ILAY,JSLAMO)
  350 CONTINUE
C>>> HM890307 TEMPORARY MOD FOR BACKWARD COMPATIBILITY
      IF (LOLGSF) THEN
        MDORSG(1) = 11
        MDORSG(2) = 12
        MDORSG(3) = 13
        MDORSG(4) = 12
        MDORSG(5) = 13
        MDORSG(6) = 11
        MDORSG(7) = 13
        MDORSG(8) = 11
        MDORSG(9) = 12
      ENDIF
      IF (LOLDSF) THEN
        PHIBSG(1) = 4.18879
        PHIBSG(4) = 4.18879
        PHIBSG(7) = 4.18879
      ENDIF
C>>> END HM890307
      PHIOSG = 0.
      TUBESG = RTABL(KSATR,1,JSATBT)
      SECDSG = RTABL(KSATR,1,JSATDL)
      GASDSG = RTABL(KSATR,1,JSATDR)
      MLAYSG = ITABL(KSATR,1,JSATLA)
      MSECSG = ITABL(KSATR,1,JSATSC)
      MWIRSG = ITABL(KSATR,1,JSATWI)
      MCRASG = ITABL(KSATR,1,JSATCR)
      MCRDSG = ITABL(KSATR,1,JSATCA)
      MTDCSG = ITABL(KSATR,1,JSATTD)
      MCHASG = 1024
      DO 360 K5 = 1, MMDSG
      DO 360 J5 = 1, MGRSG
      DO 360 I5 = 1, MSCSG
  360 KSECSG(I5,J5,K5) = KSEC(I5,J5,K5)
C
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      CALL RERROR('SRGEOM',-IER,
     +  'No SATR, SLAY, or SPOS bank from database')
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
