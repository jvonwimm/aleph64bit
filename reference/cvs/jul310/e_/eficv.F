      SUBROUTINE EFICV(IER)
C***********************************************************************
C!FIND CREATION VERTEX                                                 *
C!                                                                     *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  861128                                    *
C!  MODIFIED : DENIS COURVOISIER  890418                               *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : KINE,FKIN,PART,ETP1                                    *
C!    OUTPUT  : ETP1                                                   *
C!    CREATED : ETP1                                                   *
C!                                                                     *
C!    INPUT  : NONE                                                    *
C!    OUTPUT : NONE                                                    *
C!                                                                     *
C!   GET NUMBER OF TRACKS IN EVENT                                     *
C!   CREATE ETP1 BANK WITH CORRECT SIZE                                *
C!   DO FOR ALL TRACKS :                                               *
C!      GET KINEMATIC DATA AT CREATION VERTEX                          *
C!      FIND IF TRACK IS RELEVANT FOR ECAL EXTRAPOLATION               *
C!      FILL ONE LINE IN ETP1 BANK                                     *
C!    ENDDO                                                            *
C!    ETP1 BANK IS NOW FILLED WITH INFORMATIONS AT VERTEX OF CREATION  *
C!                                                                     *
C!
C!       IER = return code ( 0 if normal)                              *
C!
C***********************************************************************
#ifndef DOC
#include "bcs.h"
C
#include "fkinjj.h"
#include "etp1jj.h"
#include "rlunit.h"
C
      DIMENSION XXX(3,2)
#include "bmacro.h"
C
      NFLDE= 0
      IER=0
      LHEV3=3
      KKINE=NAMIND('KINE')
C       choose FKIN bank if KINE do not exist
      IF(IW(KKINE).EQ.0)THEN
        WRITE(LDEBRL,*)'EFICV --> BANKS KINE DO NOT EXIST'
        NFLDE= 1
        KFKIN= NLINK('FKIN',0)
        IF (KFKIN.LE.0) GO TO 997
      ENDIF
      IF (NFLDE.EQ.0) THEN
        KKINE=KKINE+1
C?
C?  COUNT NUMBER OF KINE OR FKIN BANKS
C?
        KCNT=KKINE
        NTRAK=-1
   30   KCNT=IW(KCNT-1)
        NTRAK=NTRAK+1
        IF(KCNT.NE.0)GO TO 30
      ELSE
        NTRAK= LROWS(KFKIN)
      ENDIF
C?
C?  CREATE ETP1 BANK
C?
      CALL AUBOS('ETP1',0,NTRAK*LETP1A+LMHLEN,KETP1,IGARB)
      IF (IGARB.EQ.2) GOTO 998
      CALL BLIST(IW,'E+','ETP1')
C
C  FILL MINIHEADER
C
      IW(KETP1+LMHCOL)=LETP1A
      IW(KETP1+LMHROW)=NTRAK
C?
C?    RELINK ON KINE BANK, IN CASE OF A GARBAGE COLLECTION
C?
      IF(IGARB.EQ.1.AND.NFLDE.EQ.0)KKINE=NAMIND('KINE')+1
      IF(IGARB.EQ.1.AND.NFLDE.EQ.1)KFKIN=NAMIND('FKIN')+1
C
      ITRAK=0
      KPART=NLINK('PART',0)
      IF(KPART.EQ.0)THEN
        WRITE(LDEBRL,*)'EFICV --> BANKS PART DO NOT EXIST'
        GOTO 997
      ENDIF
C
C  LOOP ON TRACKS
C
 10   ITRAK= ITRAK+1
      IF (NFLDE.EQ.0) THEN
        KKINE=IW(KKINE-1)
        IF(KKINE.LE.0) GO TO 999
        IKINE=KKINE+LHEV3
C
C   GET PARTICLE NUMBER AND CHARGE
C
        IPART=IW(KKINE+LHEV3+5)
C?
C?   FILL MOMENTUM
C?
        PX=RW(IKINE+1)
        PY=RW(IKINE+2)
        PZ=RW(IKINE+3)
C?
C?  GET NUMBER OF VERTICES ON THE TRACK
C?
        NVERT=IW(KKINE+LHEV3)
C?
C?  ESLCT WILL DECIDE IF TRACK SHOULD BE EXTRAPOLATED IN ECAL
C?
        CALL VZERO(XXX,6)
        CALL ESLCT(XXX,NVERT,KKINE,IFLAG)
C                if not KINE, use FKIN bank
      ELSE
          IF (ITRAK.GT.NTRAK) GO TO 999
          IPART= ITABL(KFKIN,ITRAK,JFKIPA)
          PX= RTABL(KFKIN,ITRAK,JFKIPX)
          PY= RTABL(KFKIN,ITRAK,JFKIPY)
          PZ= RTABL(KFKIN,ITRAK,JFKIPZ)
          CALL ESLCTF(XXX,ITRAK,IFLAG)
      ENDIF
C
C  FILL CHARGE
C
        INDLO=KROW(KETP1,ITRAK)
        RW(INDLO+JETPCH)=RTABL(KPART,IPART,7)
        P=SQRT(PX*PX+PY*PY+PZ*PZ)
        PX=PX/P
        PY=PY/P
        PZ=PZ/P
        RW(INDLO+JETPP3)=PX
        RW(INDLO+JETPP3+1)=PY
        RW(INDLO+JETPP3+2)=PZ
        RW(INDLO+JETPPT)=P
        IW(INDLO+JETPEF)=IFLAG
C
C   NEUTRALS SHOULD NOT BE EXTRAPOLATED
C
        IF(RW(INDLO+JETPCH).EQ.0.)IW(INDLO+JETPEF)=0
C
C?
C?   FILL X,Y,Z COORDINATES
C?
        DO 20 I=1,3
   20   RW(INDLO+I+JETPX3-1)=XXX(I,1)
        GO TO 10
C
  999 CONTINUE
      RETURN
  998 CONTINUE
      CALL RERROR('EFICV',3,' ABORT')
      IER=1
  997 CONTINUE
C
C   Create empty etp1 bank, in case kine or part are missing
C
      NTRAK=0
      CALL AUBOS('ETP1',0,NTRAK*LETP1A+LMHLEN,KETP1,IGARB)
      IF (IGARB.EQ.2) GOTO 998
      CALL BLIST(IW,'E+','ETP1')
C
C  FILL MINIHEADER
C
      IW(KETP1+LMHCOL)=LETP1A
      IW(KETP1+LMHROW)=NTRAK
      RETURN
      END
#endif
