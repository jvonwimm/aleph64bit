      SUBROUTINE VUMCTR(IERR)
C---------------------------------------------------------------
C! Unpack vdet history bank
C -  Author: Alain Bonissent (Nov 1994)
C! Modified November 1996 A. Bonissent, handle properly strips which 
C!                                 belong to two clusters (after splitting)
CKEY V_DECK VDET
C--------------------------------------------------------------
C   RETURN CODES: IERR=-1,-2 MISSING BANKS,
C                      -3 : Could not booK VUFK,
C                       0 OK
C-----------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "vcpljj.h"
#include "vdxyjj.h"
#include "vdztjj.h"
#include "vufkjj.h"
C
      PARAMETER(NMAX=10)
      DIMENSION NUMB(2),JHVI(2),NAXYZT(2)
      DIMENSION ITM(NMAX),NSTR(NMAX),QFR(NMAX)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE NAFKIN,NAXYZT
C
C  Inline functions
C
#include "bmacro.h"
      IF(FIRST) THEN
         NAFKIN=NAMIND('FKIN')
         NAXYZT(1)=NAMIND('VDZT')
         NAXYZT(2)=NAMIND('VDXY')
         FIRST=.FALSE.
      ENDIF
C
      IERR=0
C
C Delete old existing bank
C
      CALL BDROP(IW,'VUFK')
C
C Get number of VDXY,VDZT hits
C
      NHITS=0
      DO 10 IV=1,2
      KBNK=NAXYZT(IV)
      KBNK=KBNK+1
   11 KBNK=IW(KBNK-1)
      IF(KBNK.GT.0)THEN
         NHITS=NHITS+LROWS(KBNK)
         GO TO 11
      ENDIF
   10 CONTINUE

C
C Get number of FKIN tracks
C
      KFKIN = IW(NAFKIN)
      IF(KFKIN.LE.0) THEN
       IERR=-1
       GOTO 999
      ENDIF
      NTRK = LROWS(KFKIN)
C
C Now that we have the delta rays simulated, some hits may be attached
C to no track, i.e. track 0. Therefore we have one more track than rows
C in the FKIN bank (possibly)
      NTRK=NTRK+1
C
C Create new relations bank with max possible size
C
      NREL = NHITS*NTRK
      NDATA = NREL*LVUFKA+LMHLEN
      CALL AUBOS('VUFK',0,NDATA,KVUFK,IGARB)
      IF(IGARB.GE.2)THEN
         IERR = -3
         GO TO 999
      ELSEIF(IGARB.EQ.1)THEN
       KFKIN = IW(NAFKIN)
      ENDIF
      IW(KVUFK+LMHCOL) = LVUFKA
      IW(KVUFK+LMHROW) = 0
      ITRK0 = 0
      DO 120 IV=1,2
      KBNK=NAXYZT(IV)
      KBNK=KBNK+1
   20 KBNK=IW(KBNK-1)
      IF(KBNK.EQ.0)GO TO 120
         NUMBK = IW(KBNK-2)
         NRVS=NUMBK+IV-1
         NHITS=LROWS(KBNK)
         DO 40 IHIT=1,NHITS
C
C is this hit assocaited to at least one track
C
         IF(IV.EQ.1)THEN
           IASOC = ITABL(KBNK,IHIT,JVDZNA)
         ELSE
           IASOC = ITABL(KBNK,IHIT,JVDXNA)
         ENDIF
C If hit is associated to no FRFT track ...
         IF(IASOC.EQ.0)THEN
            IF(IV.EQ.1)THEN
               NBA = ITABL(KBNK,IHIT,JVDZIW)
               NTZP = ITABL(KBNK,IHIT,JVDZIH)
               QTOT = RTABL(KBNK,IHIT,JVDZPH)
            ELSE
               NBA = ITABL(KBNK,IHIT,JVDXIW)
               NTZP = ITABL(KBNK,IHIT,JVDXIH)
               QTOT = RTABL(KBNK,IHIT,JVDXPH)
            ENDIF
            CALL VTRURL(2,NBA,NTZP,IHIT,
     >                  NRVS,NMAX,NMAT,ITM,NSTR,QFR,IER)
C
C Now, we divide by the total cluster charge, to make percentages
C
            DO III=1,NMAT
               QFR(III)=QFR(III)/QTOT
            ENDDO
            IF(IER.EQ.1)THEN
               IERR=-2
               GO TO 999
            ENDIF
C
C Put info in VUFK bank
C
            DO 70 IMAT=1,NMAT
            NVUFK = LROWS(KVUFK)+IMAT
            KLINE = KROW(KVUFK,NVUFK)
            IW(KLINE+JVUFBN) = NUMBK
            IW(KLINE+JVUFHN) = IHIT
            IW(KLINE+JVUFVI) = IV
            RW(KLINE+JVUFPC) = QFR(IMAT)
            IW(KLINE+JVUFSC) = NSTR(IMAT)
            IW(KLINE+JVUFFK) = ITM(IMAT)
   70       CONTINUE
            IW(KVUFK+LMHROW) = LROWS(KVUFK)+NMAT
         ENDIF
   40    CONTINUE
      GO TO 20
  120 CONTINUE
C
  999 CONTINUE
      CALL AUBPRS('VUFK')
      RETURN
      END
#endif
