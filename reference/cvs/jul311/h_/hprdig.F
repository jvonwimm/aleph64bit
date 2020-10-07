      SUBROUTINE HPRDIG(NHTUB)
C----------------------------------------------------------
C
C! prepare Hcal digital data
C
C      G.Capon               date : 861001
C
C            output : NHTUB = number of fired tubes in Hcal
C!     called by HPREDA
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "bcs.h"
#include "hcnamc.h"
#include "htubjj.h"
#include "htubec.h"
#include "hcjosu.h"
#include "rcurnt.h"
#include "rlunit.h"
#include "hdeach.h"
#include "hclust.h"
#include "hlwdjj.h"
#include "hltujj.h"
      LOGICAL BTEST
      DIMENSION LASTL(23)
      DATA LASTL / 5*143, 5*159, 4*175, 4*191, 5*207 /
      DATA NERR/0/
#include "bmacro.h"
C
      NHTUB=0
      JHWDI=IW(NAHWDI)
      IF(JHWDI.EQ.0) GO TO 99
      NFTU=IW(JHWDI+2)
      NTUBHC=NTUBHC+NFTU
      IF (JHISRF(JULHC).GE.1) CALL HFILL(7051,FLOAT(NFTU),0.,1.)
      NAHWNO=NAMIND('HWNO')
      JHWNO=IW(NAHWNO)
      IF(JHWNO.EQ.0) THEN
         NHWNO=0
      ELSE
         NHWNO=LROWS(JHWNO)
      ENDIF
C
C?      create HTUB bank
C
C?              max bank length (nwclus.le.15)
      LNHTU=2+LHTUBA*NFTU*16
      CALL AUBOS('HTUB',0,LNHTU,JHTUB,IGARB)
      IF (IGARB.EQ.1) JHWDI=IW(NAHWDI)
      IF (IGARB.EQ.2) THEN
         CALL RERROR('HPRDIG',1,'no space for HTUB bank')
         RETURN
      ENDIF
C
C?      create HLTU bank (MonteCarlo only)   G. Capon 1-feb-1992
C
      JHLWD=NLINK('HLWD',0)
      IF (JHLWD.GT.0) THEN
         LNHLT=2+LHLTUA*NFTU*16
         CALL AUBOS('HLTU',0,LNHLT,JHLTU,IGARB)
         IF (IGARB.EQ.1) THEN
            JHWDI=IW(NAHWDI)
            JHTUB=NLINK('HTUB',0)
         ENDIF
         IF (IGARB.EQ.2) THEN
            CALL RERROR('HPRDIG',4,'no space for HLTU bank')
            RETURN
         ENDIF
         KHLTU=JHLTU+2
      ENDIF
C
C?      loop on fired tubes, unpack raw data
C?      MODAHT (acquisition module nr) =1,12 endcapA - 13,24 barrel - 25
C
      KHTUB=JHTUB+2
      LAST=0
      DO 30 N=1,NFTU
         ICURHW=N
         IRDW=IW(JHWDI+2+N)
         DO 330 INOISE=1,NHWNO
            KNOISE=ITABL(JHWNO,INOISE,1)
            IF(IRDW.EQ.KNOISE) GO TO 30
 330     CONTINUE
         ILWIHT=IBITS(IRDW,0,8)
         NWCLHT=IBITS(IRDW,8,4)
         IF (JHISRF(JULHC).GE.1) CALL HFILL(7053,FLOAT(NWCLHT),0.,1.)
         ILAYHT=IBITS(IRDW,16,8)+1
         IF (JHISRF(JULHC).GE.1) CALL HFILL(7052,FLOAT(ILAYHT),0.,1.)
         MODAHT=IBITS(IRDW,24,8)
C      correction for new numbering scheme of Astros (Dig. Read. Proc.)
C      Astros number = 101,103,........,171
         IF (MODAHT.GT.100) MODAHT = (MODAHT-101)/2 + 1
C?           check indices within expected range
         IF(NWCLHT.EQ.0.OR.NWCLHT.GT.15.OR.ILAYHT.GT.23.OR.MODAHT.EQ.0.
     *      OR.MODAHT.GT.36) THEN
            NERR=NERR+1
            CALL RERROR('HPRDIG',3,'hcal digit addr out of range')
            IF (NERR.GT.10) GO TO 30
            WRITE(LDEBRL,*)' routine HPRDIG:Hcal digital addr out of ',
     +      'range - RUN,EV =',IRUNRC,IEVTRC
            WRITE(LDEBRL,*)' MODAHT,ILAYHT,NWCLHT =',MODAHT,ILAYHT,
     +      NWCLHT
            GO TO 30
         ENDIF
C
         ISUBC=(MODAHT-1)/12+1
C
C     correction for end cap data, for runs .lt. 5022
C     F. Ruggieri December 1989
C
      IF(ISUBC.NE.2.AND.IRUNRC.LT.5022.AND..NOT.FMCRUN) THEN
         INVRS = 32*(ILWIHT/16) - ILWIHT + NWCLHT + 14
         LASTP=LASTL(ILAYHT)
         IF(ILWIHT.LE.15) THEN
            ILWIHT = INVRS
            GOTO 10
         ENDIF
         IF(ILWIHT.GT.15.AND.ILWIHT.LE.31) THEN
            ILWIHT = INVRS + 16
            GOTO 10
         ENDIF
         IF(ILWIHT.GT.31.AND.ILWIHT.LE.47) THEN
            ILWIHT = ILWIHT - 16
            GOTO 10
         ENDIF
         IF(ILWIHT.GT.(LASTP-64).AND.ILWIHT.LE.(LASTP-48)) THEN
            ILWIHT = ILWIHT + 16
            GOTO 10
         ENDIF
         IF(ILWIHT.GT.(LASTP-48).AND.ILWIHT.LE.(LASTP-32)) THEN
            ILWIHT = ILWIHT + 32
            GOTO 10
         ENDIF
         IF(ILWIHT.GT.(LASTP-32).AND.ILWIHT.LE.(LASTP-16)) THEN
            ILWIHT = INVRS - 32
            GOTO 10
         ENDIF
         IF(ILWIHT.GT.(LASTP-16).AND.ILWIHT.LE.LASTP) THEN
            ILWIHT = INVRS - 16
            GOTO 10
         ENDIF
 10      CONTINUE
      ENDIF
C            skip dead/missing elements
         IF (IHSUBC(ISUBC).EQ.0 .OR. MODULE(MODAHT).EQ.0) GO TO 30
         IF ( BTEST(IHCBUS(MODAHT),ILAYHT) ) GO TO 30
C?           fill HTUB bank
         IF (ILAYHT.GT.LAST) LAST=ILAYHT
         NHTU1=NHTUB
         CALL HTUBFI(KHTUB,NHTUB)
         IF(JHLWD.GT.0) THEN
            IF(KHLTU.GE.JHLTU+LNHLT) GO TO 30
            MMODA=IBITS(IRDW,24,8)
            DO 33 ICL=1,NWCLHT
            IWIRE=NNWIRE(ICL)
            IHIT=NHITWI(ICL)
            DO 31 NN=1,LROWS(JHLWD)
            KRDW=ITABL(JHLWD,NN,JHLWWA)
            COOR=RTABL(JHLWD,NN,JHLWHP)
            KLWI=IBITS(KRDW,0,8)
            KWCL=IBITS(KRDW,8,8)
            KLAY=IBITS(KRDW,16,8)+1
            KMOD=IBITS(KRDW,24,8)
            IF(KMOD.NE.MMODA .OR. KLAY.NE.ILAYHT) GO TO 31
            IF(IWIRE.EQ.KLWI) THEN
               IW(KHLTU+JHLTHI)=NHTU1+IHIT
               RW(KHLTU+JHLTXL)=COOR
               IW(JHLTU+2)=IW(JHLTU+2)+1
               KHLTU=KHLTU+LHLTUA
            ENDIF
   31    CONTINUE
   33    CONTINUE
      ENDIF
   30 CONTINUE
      IF (JHISRF(JULHC).GE.1) CALL HFILL(7054,FLOAT(LAST),0.,1.)
C
C?           adjust HTUB bank length
C
      LNHTU=2+LHTUBA*NHTUB
      IW(JHTUB+1)=LHTUBA
      IW(JHTUB+2)=NHTUB
      LNHTU=2+LHTUBA*NHTUB
      CALL AUBOS('HTUB',0,LNHTU,JHTUB,IGARB)
      IF (IGARB.EQ.2) CALL RERROR('HPRDIG',2,'no update of HTUB bank')
C
C           adjust HLTU bank length
C
      IF (JHLWD.GT.0) THEN
         IW(JHLTU+1)=LHLTUA
         LNHLT=2+LHLTUA*IW(JHLTU+2)
         CALL AUBOS('HLTU',0,LNHLT,JHLTU,IGARB)
      ENDIF
C
C
 99   RETURN
      END
#endif
