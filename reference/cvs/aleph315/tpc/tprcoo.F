      SUBROUTINE TPRCOO
C -------------------------------------------------------------------
C - M.MERMIKIDES - 860410                     F.Ranjard - 860418
C!  print TPC coodinates from TPCO and TPCH banks
#ifndef DOC
      SAVE
#include "bcs.h"
C
#include "bmacro.h"
      DATA NACO/0/
C --------------------------------------------------------------------
      LOUT = IW(6)
C
C  NACO  is the name-index of the coordinate bank 'TPCO'
C  NACH  is the name-index of the coord-hit ref. bank 'TPCH'
C  NAHT  is the name-index of the hit bank 'TPHT'
C
      IF (NACO .EQ. 0) THEN
         NACO = NAMIND ('TPCO')
         NACH = NAMIND ('TPCH')
         NAHT = NAMIND ('TPHT')
      ENDIF
C
C  Print out TPCO bank
C
      CALL BPRNT (IW,'TPCHTPCO')
      JTPCO = IW(NACO)
      IF (JTPCO.EQ.0) GO TO 999
          KTPCO = JTPCO + LMHLEN
          NWPCO = LCOLS(JTPCO)
          NCO   = LROWS(JTPCO)
          KTPCH = IW(NACH) + LMHLEN
          JTPHT = IW(NAHT)
C
          IREF = 0
          KREF = 0
          WRITE (LOUT,1000) NCO
1000      FORMAT(/1X,'+++TPRCOO+++ TPCO No of coords = ', I6,/
     &            2X,'IC',2X,'ROW',2X,'SLOT',2X,'PAD',4X,'R',5X,
     &          'Phi',5X,'Z',5X,'D(Phi)/DR',1X,'D(Z)/DR',1X,'Stat',1X,
     &          'HitRef',1X, 'KinRef')
          DO 20 IC = 1,NCO
             IREF = IW(KTPCH+IC)
             KREF = ITABL(JTPHT,IREF,1)
             IROW = IW(KTPCO+1)/100000
             ISLOT= MOD (IW(KTPCO+1)/1000,100)
             IPAD = MOD (IW(KTPCO+1),1000)
             WRITE(LOUT,1001) IC,IROW,ISLOT,IPAD,
     &          (RW(KTPCO+M),M= 2,6),IW(KTPCO+7),IREF,KREF
 1001        FORMAT(1X,I4,')',I3,I6,I5,F8.3,F7.4,3F9.3,I5,2I7)
             KTPCO= KTPCO + NWPCO
 20       CONTINUE
C
 999  CONTINUE
      RETURN
      END
#endif
