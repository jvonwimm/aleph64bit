      SUBROUTINE PRRHAH
C ----------------------------------------------------------------------
C! Prints bank RHAH in Readable Format
CKEY PRINT RHAH RUN HEADER / USER
C Author     J. Boucrot     26-Sept-1988    modified  02-Feb-1989
C Called from USER
C Input bank : RHAH  ( Run Header Analysis History )
C ----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "runrjj.h"
#include "rhahjj.h"
      EXTERNAL CHAINT,INTCHA,NAMIND
      CHARACTER*4 CHAINT,PRNAM,COMPE
      CHARACTER*6 CALDA(4),FILCA(6)
      CHARACTER*12 CINOU(2)
      INTEGER ICHN(2,3)
      DATA FILCA /'  None','  Kine','   Raw','   POT','   DST',
     +            '  Mdst' /
      DATA CALDA / '      ','MCarlo','  Data' , 'S Data' /
      DATA IONC / 0 /
#include "bmacro.h"
C ----------------------------------------------------------------------
      IF (IONC.EQ.0) THEN
         LOUT=IW(6)
         NAMRH=NAMIND('RHAH')
         NRUNR=NAMIND('RUNR')
         IONC=1
      ENDIF
      JRHAH=IW(NAMRH)
      JRUNR=IW(NRUNR)
      IF (JRHAH.EQ.0.OR.JRUNR.EQ.0) THEN
         WRITE (LOUT,'(''  +++ PRRHAH : no RHAH/RUNR bank - RETURN'')')
         GO TO 999
      ENDIF
C
      NRUN=IW(JRUNR+JRUNRN)
      WRITE (LOUT,1020) NRUN
 1020 FORMAT(/1X,'+++ PRRHAH +++ Run Header Analysis History Bank',
     &   ' for RUN Number : ',I8//,
     &   T3,'Program',T16,'Date and hour',T34,'Program',
     &   T44,'Correction',T57,'Alephlib',
     &   T70,'Database',T87,'Input File',T103,'Output File',
     &   T117,'Computer',/
     &   T5,'Name',T16,'of processing',T34,'Vers. #',T44,'Vers. #',
     &   T57,'Vers. #',T68,'Vers. #',T79,'Date'/)
C
      NROWS=LROWS(JRHAH)
      IMCAR=2
      DO 10 I=1,NROWS
         KRHAH=KROW(JRHAH,I)
         ISDIN=0
         NATIN=IW(KRHAH+JRHANI)
         IF (NATIN.GT.10) THEN
            ISDIN=1
            NATIN=MOD(NATIN,10)
         ENDIF
         ISDOU=0
         NATOU=IW(KRHAH+JRHANO)
         IF (NATOU.GT.10) THEN
            ISDOU=1
            NATOU=MOD(NATOU,10)
         ENDIF
         PRNAM=CHAINT(IW(KRHAH+JRHAPN))
         IF (PRNAM.EQ.'GALE') IMCAR=1
         IF (NATOU.EQ.1) THEN
            CINOU(2)=CALDA(1)//FILCA(2)
         ELSE
            IMOU=IMCAR
            IF (ISDOU.EQ.1) IMOU=3
            CINOU(2)=CALDA(IMOU+1)//FILCA(NATOU+1)
         ENDIF
         IF (NATIN.LE.1) THEN
            CINOU(1)=CALDA(1)//FILCA(NATIN+1)
         ELSE
            IMIN=IMCAR
            IF (ISDIN.EQ.1) IMIN=3
            CINOU(1)=CALDA(IMIN+1)//FILCA(NATIN+1)
         ENDIF
         IF (CHAINT(IW(KRHAH+JRHAPN+1)).EQ.'ALPH'.
     +       AND.MOD(NATIN,10).EQ.5)   GO TO 10
         DO 2 II=1,2
            DO 1 LL=1,3
               LLL=4*(LL-1)+1
 1          ICHN(II,LL)=INTCHA(CINOU(II)(LLL:LLL+3))
 2       CONTINUE
C
         COMPE=' '
         IF (LCOLS(JRHAH).GT.JRHACV) THEN
            IF (IW(KRHAH+JRHANU).NE.0) COMPE=CHAINT(IW(KRHAH+JRHANU))
         ENDIF
         WRITE (LOUT,1005)   IW(KRHAH+JRHAPN),IW(KRHAH+JRHAPN+1),
     +         (IW(KRHAH+LI),LI=JRHAPD,JRHAPV),IW(KRHAH+JRHACV),
     +         (IW(KRHAH+LL),LL=JRHAAV,JRHADD),(ICHN(1,K),K=1,3),
     +         (ICHN(2,L),L=1,3),COMPE
 1005    FORMAT (2X,2A4,T14,I6,T23,I9,T33,I6,T42,I6,T58,I4,T69,I4,
     &           T77,I6,T87,3A4,T103,3A4,T120,A4)
C
 10   CONTINUE
C
 999  RETURN
      END
#endif
