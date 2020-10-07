      SUBROUTINE HDGPOT
C***********************************************************
C! Create HCAL Digital Patterns POT                        *
C! Author: R.Tenchini           041188                     *
C!                                                         *
C! INPUT Banks : HPCO, HPDS                                *
C! OUTPUT Banks : PPOB, PPDS                               *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "hpcojj.h"
#include "hpdsjj.h"
#include "ppobjj.h"
#include "pphyjj.h"
#include "ppdsjj.h"
#include "bmacro.h"
C
C We link HPCO
C
      KHPCO=NLINK('HPCO',0)
      IF(KHPCO.EQ.0) THEN
         GO TO 999
      ENDIF
      NHPCO=LROWS(KHPCO)
C
C WE DEFINE PPOB,PPHY
C
      LNGTH=LMHLEN+LPPOBA*NHPCO
      CALL AUBOS('PPOB',0,LNGTH,KPPOB,IGARB)
      IF(IGARB.EQ.2) THEN
         CALL RERROR('HDGPOT',1,' NOT ENOUGH SPACE AFTER G.C.')
         GO TO 999
      ELSEIF(IGARB.EQ.1) THEN
         KHPCO=NLINK('HPCO',0)
      ENDIF
      LPPHY=LMHLEN+NHPCO*LPPHYA
      CALL AUBOS('PPHY',0,LPPHY,KPPHY,IGARB)
      IF (IGARB.EQ.2) THEN
         CALL RERROR('HDGPOT',2,' NOT ENOUGH SPACE AFTER G.C.')
         GO TO 999
      ELSEIF(IGARB.EQ.1) THEN
         KHPCO=NLINK('HPCO',0)
      ENDIF
C
C The length for PPDS is calculated and then PPDS is booked
C
      NPPDS=0
      DO 5 I=1,NHPCO
         IPOIN=ITABL(KHPCO,I,JHPCHP)
         KHPDS=NLINK('HPDS',IPOIN)
         IF (KHPDS.EQ.0) THEN
             CALL RERROR('HDGPOT',3,' HPDS EMPTY')
             GO TO 5
         ENDIF
         NHPDS=LROWS(KHPDS)
         NPPDS=NPPDS+NHPDS
 5    CONTINUE
      LENGG=LMHLEN+LPPDSA*NPPDS
      CALL AUBOS('PPDS',0,LENGG,IPPDS,IGARB)
      IF(IGARB.EQ.2) THEN
         CALL RERROR('HDGPOT',4,' NOT ENOUGH SPACE AFTER G.C.')
         GO TO 999
      ELSEIF(IGARB.EQ.1) THEN
         KHPCO=NLINK('HPCO',0)
      ENDIF
C
C Headers
C
      IW(KPPOB+LMHCOL)=LPPOBA
      IW(KPPOB+LMHROW)=NHPCO
      IW(KPPHY+LMHCOL)=LPPHYA
      IW(KPPHY+LMHROW)=NHPCO
      IW(IPPDS+LMHCOL)=LPPDSA
      IW(IPPDS+LMHROW)=0
C
C  And now filling
C
      DO 10 I=1,NHPCO
         IW(KROW(KPPOB,I)+JPPODI)=ITABL(KHPCO,I,JHPCDI)
         RW(KROW(KPPOB,I)+JPPODE)=RTABL(KHPCO,I,JHPCDE)
         RW(KROW(KPPOB,I)+JPPOC1)=RTABL(KHPCO,I,JHPCC1)
         RW(KROW(KPPOB,I)+JPPOC2)=RTABL(KHPCO,I,JHPCC2)
         IW(KROW(KPPOB,I)+JPPOIP)=ITABL(KHPCO,I,JHPCIP)
         IW(KROW(KPPOB,I)+JPPOFP)=ITABL(KHPCO,I,JHPCFP)
         IW(KROW(KPPOB,I)+JPPOLP)=ITABL(KHPCO,I,JHPCLP)
         RW(KROW(KPPOB,I)+JPPOMD)=RTABL(KHPCO,I,JHPCMD)
         RW(KROW(KPPOB,I)+JPPOPD)=RTABL(KHPCO,I,JHPCPD)
         IW(KROW(KPPHY,I)+JPPHBH) = 0
         IW(KROW(KPPHY,I)+JPPHKT) = 0
         IW(KROW(KPPHY,I)+JPPHPP) = I
         IPOIN=ITABL(KHPCO,I,JHPCHP)
         KHPDS=NLINK('HPDS',IPOIN)
         IF (KHPDS.EQ.0) THEN
            GO TO 10
         ENDIF
         NHPDS=LROWS(KHPDS)
         DO 20 J=1,NHPDS
            IW(KNEXT(IPPDS)+JPPDNL)=ITABL(KHPDS,J,JHPDNL)
            RW(KNEXT(IPPDS)+JPPDFL)=RTABL(KHPDS,J,JHPDFL)
            RW(KNEXT(IPPDS)+JPPDMD)=RTABL(KHPDS,J,JHPDMD)
            IW(KNEXT(IPPDS)+JPPDPP)=I
            IW(IPPDS+LMHROW)=IW(IPPDS+LMHROW)+1
 20      CONTINUE
 10   CONTINUE
 999  RETURN
      END
#endif
