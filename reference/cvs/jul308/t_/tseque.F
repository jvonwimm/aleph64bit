      SUBROUTINE TSEQUE
C
C-------------------------------------------------------------------
C! Drop TPC sectors which are out of sequence
C
C  Author:   R. Johnson    3-08-89
C  Modified: R. Johnson   21-01-91  also drop TRIR and TRDI
C
C-------------------------------------------------------------------
#ifndef DOC
C
#include "tpgpar.h"
#include "tmonit.h"
#include "tinfjj.h"
#include "bcs.h"
C
      DIMENSION ICNEV(2*LTSECT),IVOTE(2*LTSECT)
      CHARACTER TEXT*88
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTINF=NAMIND('TINF')
      ENDIF
C
C++   Loop over DAQ info banks and accumulate votes for event number
C
      NEVN=0
      KTINF=IW(NTINF)
  121 IF (KTINF.EQ.0) GO TO 151
        IEVT=IW(KTINF+JTINEC)
        DO 124 I=1,NEVN
          IF (ICNEV(I).EQ.IEVT) THEN
            IVOTE(I)=IVOTE(I)+1
            GO TO 127
          ENDIF
  124   CONTINUE
        NEVN=NEVN+1
        ICNEV(NEVN)=IEVT
        IVOTE(NEVN)=1
  127   CONTINUE
        KTINF=IW(KTINF-1)
        GO TO 121
  151 CONTINUE
C
C++   The event number agreed on by the majority wins
C
      IF (NEVN.LE.1) GO TO 999
      MXV=0
      DO 170 I=1,NEVN
        IF (IVOTE(I).GT.MXV) THEN
          MXV=IVOTE(I)
          JEVT=ICNEV(I)
        ENDIF
  170 CONTINUE
C
C++   Drop sectors which voted for the losers
C
      KTINF=IW(NTINF)
  250 IF (KTINF.EQ.0) GO TO 300
        IEVT=IW(KTINF+JTINEC)
        IF (IEVT.NE.JEVT) THEN
          NR=IW(KTINF-2)
          IF (NR.GT.LTSECT .AND. NR.LE.2*LTSECT) THEN
            ISLOT=NR-LTSECT
            WRITE(TEXT,270) ISLOT
  270       FORMAT('Wire TPP for sector ',I2,' is out of sequence.',
     &             '&NO ACTION TAKEN.  SECTOR USED ANYWAY.      ')
C    &             '&Dropping all wire data for this TPC sector.')
C           IND=NDROP('TWIR',ISLOT)
C           IND=NDROP('TWDI',ISLOT)
C           IND=NDROP('TRIR',ISLOT)
C           IND=NDROP('TRDI',ISLOT)
            CALL RERROR('TSEQUE',1,TEXT(1:87))
            NWSYTM(ISLOT)=NWSYTM(ISLOT)+1
          ELSEIF (NR.GT.0 .AND. NR.LE.LTSECT) THEN
            ISLOT=NR
            WRITE(TEXT,271) ISLOT
  271       FORMAT('Pad TPP for sector ',I2,' is out of sequence.',
     &             '&NO ACTION TAKEN.  SECTOR USED ANYWAY.     ')
C    &             '&Dropping all pad data for this TPC sector.')
C           IND=NDROP('TPAD',ISLOT)
C           IND=NDROP('TPDI',ISLOT)
C           IND=NDROP('TISL',ISLOT)
            CALL RERROR('TSEQUE',2,TEXT(1:86))
            NPSYTM(ISLOT)=NPSYTM(ISLOT)+1
          ENDIF
        ENDIF
        KTINF=IW(KTINF-1)
        GO TO 250
  300 CONTINUE
C
  999 CONTINUE
      RETURN
      END
#endif
