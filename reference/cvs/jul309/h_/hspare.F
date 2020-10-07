      SUBROUTINE HSPARE(NSPN0,ICLU)
C*************************************************************
C! Superpattern reconstruction                               *
C!                                                           *
C! Authors : G. BAGLIESI                                     *
C!           R. TENCHINI    880408                           *
C!                                                           *
C! VERSION 1.0  880408                                       *
C!                                                           *
C! Input   HPCO (ECLU OR HCLU) BANKS                         *
C! Output  HSPX FAMILY OF BANKS (HSPE for Eclusters          *
C!                               HSPH for Hclusters)         *
C!                                                           *
C! NSPN0=Number  of superpatterns                            *
C! ICLU =1 E SUPERPATTERN (connected with Eclusters)         *
C! ICLU =2 H SUPERPATTERN (connected with Hclusters)         *
C!                                                           *
C! Each HSPA contains the superpattern elements              *
C! coded as 10000 + address in HPCO for TUBE PATTERNS,       *
C!       as 20000 + address in ECLU for ECLU CLUSTERS,       *
C! and   as 30000 + address in HCLU for HCLU CLUSTERS        *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "hwbnkw.h"
#include "hpcojj.h"
#include "hspejj.h"
#include "hsphjj.h"
#include "hclujj.h"
#include "eclujj.h"
#include "alcons.h"
#include "rlunit.h"
      LOGICAL HPLINK
      LOGICAL HSLINK,BTEST
      LOGICAL DUMP
      CHARACTER*4  XCLU,HSPX
      LOGICAL FIRST
      DATA FIRST/.TRUE./
#include "bmacro.h"
      DUMP=.FALSE.
      NSPN0=0
C
C  We link the proper banks
C
      IF(FIRST) THEN
         FIRST=.FALSE.
         ISPAHW=0
         IPPAHW=0
         IHSPHW=0
      ENDIF
      IHPCO=NLINK('HPCO',0)
      IF(IHPCO.EQ.0) THEN
         GOTO 240
      ENDIF
      NPATT=LROWS(IHPCO)
      IF(NPATT.EQ.0) GO TO 240
      IF(ICLU.EQ.1) THEN
         XCLU='ECLU'
         HSPX='HSPE'
         JHSPAR=JHSPER
      ELSEIF(ICLU.EQ.2) THEN
         XCLU='HCLU'
         HSPX='HSPH'
         JHSPAR=JHSPHR
      ELSE
         GO TO 240
      ENDIF
      IXCLU=NLINK(XCLU,0)
      IF(IXCLU.EQ.0) THEN
         GOTO 240
      ENDIF
      NXCLU=LROWS(IXCLU)
C
C Two working banks are defined. ISPAHW is a matrix relating patterns
C to XClusters. IPPAHW is a vector that links patterns together
C through XClusters.
C
      NWORD=NXCLU/32+1
      CALL WBANK(IW,ISPAHW,NWORD*NPATT+LMHLEN,*230)
      DO 12345 I=1,NWORD*NPATT+LMHLEN
      IW(ISPAHW+I)=0
12345 CONTINUE
      IW(ISPAHW+LMHCOL)=NWORD
      IW(ISPAHW+LMHROW)=NPATT
      CALL WBANK(IW,IPPAHW,NPATT,*230)
      DO 12346 I=1,NPATT
      IW(IPPAHW+I)=0
12346 CONTINUE
      IF(DUMP) THEN
         WRITE(LDEBRL,*) ' HSPARE: NPATT',NPATT,'NXCLU',NXCLU,'NWORD',
     +                 NWORD
      ENDIF
      DO 20 I=1,NPATT
         DO 10 J=0,NXCLU-1
C
C The proper linking cohordinates are defined
C
            COR1=RTABL(IHPCO,I,JHPCC1)
            COR2=RTABL(IHPCO,I,JHPCC2)
            IDIR1=ITABL(IHPCO,I,JHPCDI)
            IF(ICLU.EQ.1) THEN
               XCOR1=RTABL(IXCLU,J+1,JECLT4)
               XCOR2=RTABL(IXCLU,J+1,JECLF4)
C
C This is because Eclu phi starts from -pigrec
C
               IF(XCOR2.LT.0) XCOR2=XCOR2+2.*PI
            ELSEIF(ICLU.EQ.2) THEN
               XCOR1=RTABL(IXCLU,J+1,JHCLTC)
               XCOR2=RTABL(IXCLU,J+1,JHCLPC)
            ENDIF
            IDIR2=10+ICLU*10
C
C If it is linked the pattern/XCLU bit in ISPAHW is turned on
C
            IF(HSLINK(IDIR1,COR1,COR2,IDIR2,XCOR1,XCOR2)) THEN
               JJ=J/32+1
               JB=MOD(J,32)
               NDUM=IBSET(IW(KROW(ISPAHW,I)+JJ),JB)
               IW(KROW(ISPAHW,I)+JJ)=NDUM
            ENDIF
   10    CONTINUE
   20 CONTINUE
      IF(DUMP) THEN
         DO 40 I=1,NWORD
            DO 30 J=1,NPATT
               WRITE(LDEBRL,*)' HSPARE: DBG1',I,J,IW(KROW(ISPAHW,J)+I)
   30       CONTINUE
   40    CONTINUE
      ENDIF
C
C Now we check if two patterns are connected through an XCluster
C to link them together
C
      IF(NPATT.GT.1) THEN
         DO 80 I=1,NPATT-1
            DO 70 J=I+1,NPATT
               ICK=0
               DO 50 K=1,NWORD
                  ICK=IAND(ITABL(ISPAHW,I,K),ITABL(ISPAHW,J,K))
                  IF(ICK.GE.1) GOTO 51
   50          CONTINUE
   51          CONTINUE
               IF(ICK.NE.0) THEN
                  DO 60 K=1,NWORD
                     IW(KROW(ISPAHW,J)+K)=IOR(ITABL(ISPAHW,I,K),
     +               ITABL(ISPAHW,J,K))
   60             CONTINUE
                  IW(IPPAHW+I)=J
                  GO TO 80
               ENDIF
   70       CONTINUE
   80    CONTINUE
         NSPN0=0
         DO 90 I=1,NPATT
            IF(IW(IPPAHW+I).EQ.0) NSPN0=NSPN0+1
   90    CONTINUE
         DO 110 I=1,NPATT-1
            IND=IW(IPPAHW+I)
            IF(IND.NE.0) THEN
               DO 100 J=I+1,NPATT
                  IND1=IW(IPPAHW+J)
                  IF(IND1.EQ.IND) THEN
                     IW(IPPAHW+I)=J
                     GO TO 110
                  ENDIF
  100          CONTINUE
            ENDIF
  110    CONTINUE
      ELSE
         NSPN0=1
      ENDIF
      IF(DUMP) THEN
         DO 130 I=1,NWORD
            DO 120 J=1,NPATT
               WRITE(LDEBRL,*) ' HSPARE: DBG2',I,J,IW(KROW(ISPAHW,J)+I)
  120       CONTINUE
  130    CONTINUE
         DO 140 I=1,NPATT
            WRITE(LDEBRL,*) ' HSPARE: DBG2',IW(IPPAHW+I)
  140    CONTINUE
      ENDIF
C
C We sail through IPPAHW to define the Superpatterns. For each
C of them an HSPA is written.
C
      ND=LMHLEN
      NR=1
      CALL WBANK(IW,IHSPHW,NXCLU+NPATT+LMHLEN,*230)
      IW(IHSPHW+LMHCOL)=1
      IW(IHSPHW+LMHROW)=0
      DO 200 I=1,NPATT
         I1=I
  150    CONTINUE
         IF(IW(IPPAHW+I1).GT.0) THEN
C
C This one is connected to something else
C
            I2=IW(IPPAHW+I1)
            ND=ND+1
            IW(KNEXT(IHSPHW)+1)=10000+I1
            IW(IHSPHW+LMHROW)=IW(IHSPHW+LMHROW)+1
            IW(IPPAHW+I1)=-1
            I1=I2
            GO TO 150
         ELSEIF(IW(IPPAHW+I1).EQ.0) THEN
C
C This one is not connected to any further pattern
C
            ND=ND+1
            IW(KNEXT(IHSPHW)+1)=10000+I1
            IW(IHSPHW+LMHROW)=IW(IHSPHW+LMHROW)+1
            IW(IPPAHW+I1)=-1
C
C Now we add to the Superpattern is own XClusters
C
            NXCLN=0
            DO 160 J=1,NWORD
               NXCLN=NXCLN+NUMBIT(ITABL(ISPAHW,I1,J))
  160       CONTINUE
            IW(IHSPHW+LMHROW)=IW(IHSPHW+LMHROW)+NXCLN
            I3=0
            DO 190 J=1,NWORD
               IF(J.EQ.NWORD) THEN
                  JB=MOD(NXCLU-1,32)
                  DO 170 K=0,JB
                     IF(BTEST(ITABL(ISPAHW,I1,NWORD),K)) THEN
                        I3=I3+1
                        IW(KROW(IHSPHW,ND+I3-LMHLEN)+JHSPAR)=
     +                  K+1+(NWORD-1)*32+10000*(ICLU+1)
                     ENDIF
  170             CONTINUE
               ELSE
                  DO 180 K=0,31
                     IF(BTEST(ITABL(ISPAHW,I1,J),K)) THEN
                        I3=I3+1
                        IW(KROW(IHSPHW,ND+I3-LMHLEN)+JHSPAR)=
     +                  K+1+(J-1)*32+10000*(ICLU+1)
                     ENDIF
  180             CONTINUE
               ENDIF
  190       CONTINUE
C
C We prepare the filling of one more bank
C
            LNGTH=ND+NXCLN
            CALL AUBOS(HSPX,NR,LNGTH,LHSPA,IGARB)
            IF(IGARB.EQ.2) THEN
               CALL RERROR('HSPARE',1,'INS. SPACE FOR HSPA')
               GOTO 240
            ENDIF
            CALL UCOPY(IW(IHSPHW+1),IW(LHSPA+1),LNGTH)
            IF(NR.EQ.NSPN0) GO TO 210
            NR=NR+1
            ND=LMHLEN
            IW(IHSPHW+LMHCOL)=1
            IW(IHSPHW+LMHROW)=0
         ENDIF
  200 CONTINUE
  210 CONTINUE
      IF(DUMP) THEN
         DO 220 I=1,NSPN0
            NR=I
            IND=NPRNT(HSPX,NR)
  220    CONTINUE
      ENDIF
      CALL WDROP(IW,ISPAHW)
      CALL WDROP(IW,IPPAHW)
      CALL WDROP(IW,IHSPHW)
      RETURN
  230          CALL RERROR('HSPARE',2,'INSUFF. SPACE FOR WBANKS')
  240 CONTINUE
      CALL WDROP(IW,ISPAHW)
      CALL WDROP(IW,IPPAHW)
      CALL WDROP(IW,IHSPHW)
      RETURN
      END
#endif
