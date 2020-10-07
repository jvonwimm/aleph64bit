      SUBROUTINE YTSTRK(IER)
C
C----------------------------------------------------------*
C!    Selects tracks to be considered for vertex search
CKEY YTOP
C!    Author :     G. Lutz   30/11/87
C!    Modified :   M. Bosman 01/12/88
C!    Modified :   G. Lutz      02/91
C!
C!
C!    Description
C!    ===========
C!    This routine calculates the momentum of the
C!    reconstructed tracks and selects the tracks
C!    to be considered in the first step of vertex
C!    reconstruction
C!
C!---------------------------------------------------------*
#ifndef DOC
      SAVE
#if defined(YDEBUG)
      LOGICAL LDMP1,LDMP2
#endif
C     LOGIC FLAG FOR MULTIPLE REJECTION OF SAME TRACK
      LOGICAL LREJ
#include "bcs.h"
#include "frftjj.h"
#include "frtljj.h"
#include "ydimto.h"
#include "ytrkto.h"
#include "yparto.h"
#include "ydumto.h"
C!---------------------------------------------------------*
      DIMENSION IFAIL(2)
      DOUBLE PRECISION EHI(5,5,2),EHTEMP(5,5,2)
C!---------------------------------------------------------*
#include "bmacro.h"
C!---------------------------------------------------------*
C
C-- Define the logical unit for printout
C
C     MAXIMUM/MINIMUM MOMENTUM OF TRACKS
      DATA PRMAX/200./
C     MAXIMUM RELATIVE CURVATURE ERROR
      DATA DCURM/.3/
C
      DATA ICNER1/0/
C
      LOUT = IW(6)

#if defined(YDEBUG)
      IF(IDUMPP(21).GE.1) THEN
        LDMP1=.TRUE.
      ELSE
        LDMP1=.FALSE.
      ENDIF
C
      IF(IDUMPP(22).GE.1) THEN
        LDMP2=.TRUE.
      ELSE
        LDMP2=.FALSE.
      ENDIF
      IDUMPP(21)=IDUMPP(21)-1
      IDUMPP(22)=IDUMPP(22)-1
#endif
C
      IER = 0
C
C     conversion radius of track <=> momentum
C     radius in meter , B in Tesla, p in GeV/c  q in units of e
C
C      p = 0.29979 * q * B * r
C
C     R[cm] = ROFP * P[Gev/c]:
C
      ROFP = 1./ (0.29979 * BFIELD / 10.) * 100.
C
C     initialize bank indices
      CALL YDEFRF(KFRFT0,KFRFT,KFRTL,JFAIL)
      IF(JFAIL.NE.0) GOTO 999
      IF(KFRFT0.EQ.0) KFRFT0=KFRFT
C
      NGTRTO = 0
C
#if defined(YDEBUG)
      IF(LDMP1)
     +WRITE(LOUT,100)
  100 FORMAT(' TRK #',5X,' P',5X,' PT', 3X,
     &  ' HITS IN MV,ITC,TPC',2X,'R0',5X,'DO',5X,'Z0',5X,'ERRORS')
#endif
C
      DO 10 I=1,LROWS(KFRFT)
        IF(I.GT.MAXHLX) THEN
C --- GET RUN AND EVENT NUMBER
          CALL ABRUEV(IRUN,IEVT)
          WRITE(LOUT,9838) IRUN,IEVT,LROWS(KFRFT),MAXHLX
 9838     FORMAT(' YTSTRK : RUN',I7,' EVENT',I8,
     &      ' MAXIMUM NB OF CHARGED TRACKS EXCEEDED',I8,'>',I8)
          GOTO 11
        ENDIF
        LREJ=.FALSE.
        NGTRTO=NGTRTO+1
        PTRECT(I)=-1./ROFP/RTABL(KFRFT,I,JFRFIR)
        PRECTO(I)=PTRECT(I)*SQRT(1.+RTABL(KFRFT,I,JFRFTL)**2)
        IPTRTO(NGTRTO)=I
C--     Reject tracks that have a non-zero error flag
C       in the FRFT bank
        IERR = MOD(ITABL(KFRFT,I,JFRFNO),10)
        IF(IERR.NE.0.AND.IERR.NE.4) THEN
          LREJ=.TRUE.
#if defined(YDEBUG)
          IF(LDMP1)
     +      WRITE(LOUT,40) I,IERR
   40     FORMAT(' YTSTRK: TRACK',I3,' WITH ERROR FLAG ',I2,
     &      ' REJECTED FOR PATTERN RECOGNITION')
#endif
        ENDIF
C--     REJECT TRACKS THAT HAVE A NON-POSITIVE DEFINITE ERROR MATRIX
C
C     INVERT 5X5 ERROR MATRIX
        IJ=0
        DO II=1,5
          DO JJ=1,II
            IJ=IJ+1
            EHI(II,JJ,1)=RTABL(KFRFT0,I,JFRFEM+IJ-1)
            EHI(JJ,II,1)=EHI(II,JJ,1)
            EHI(II,JJ,2)=RTABL(KFRFT,I,JFRFEM+IJ-1)
            EHI(JJ,II,2)=EHI(II,JJ,2)
          ENDDO
        ENDDO
        DO II=1,5
          DO JJ=1,5
            EHTEMP(II,JJ,1)=EHI(II,JJ,1)
            EHTEMP(II,JJ,2)=EHI(II,JJ,2)
            IF(II.NE.JJ) EHTEMP(II,JJ,1)=EHI(II,JJ,1)/
     &            SQRT(EHI(II,II,1)*EHI(JJ,JJ,1))
            IF(II.NE.JJ) EHTEMP(II,JJ,2)=EHI(II,JJ,2)/
     &            SQRT(EHI(II,II,2)*EHI(JJ,JJ,2))
          ENDDO
        ENDDO
        DO K=1,2
#if defined(BIT64)
          CALL RSINV(5,EHI(1,1,K),5,IFAIL(K))
#else
          CALL DSINV(5,EHI(1,1,K),5,IFAIL(K))
#endif
        ENDDO
C
        IF(IFAIL(1).NE.0.OR.IFAIL(2).NE.0) THEN
C
          LREJ=.TRUE.
#if defined(YDEBUG)
          IF(LDMP1)THEN
            WRITE(LOUT,45) I,(IFAIL(J),J=1,2)
   45       FORMAT(' YTSTRK: TRACK',I3,' WITH NON-POS.DEF. ERROR MX',
     &        ' IFAIL=',2I3,' REJECTED FOR PATTERN RECOGNITION')
            WRITE(LOUT,9854) (((EHTEMP(II,J,KKK),II=1,5),J=1,5),KKK=1,2)
 9854       FORMAT(5(5X,5G13.4/)   /5(5X,5G13.4/))
          ENDIF
#endif
        ENDIF
C      REJECT VERY LOW MOMENTUM TRACKS
        IF(ABS(PRECTO(I)).LT.PMINSE ) THEN
          LREJ=.TRUE.
#if defined(YDEBUG)
          IF(LDMP1)
     +      WRITE(LOUT,49) I,PRECTO(I),PMINSE
   49     FORMAT(' YTSTRK: TRACK',I3,' WITH MOMENTUM ',E10.3,
     &      ' BELOW LIMIT',E10.3,' REJECTED FOR PATTERN RECOGNITION')
#endif
        ENDIF
C      REJECT VERY HIGH MOMENTUM TRACKS
        IF(ABS(PRECTO(I)).GT.PRMAX) THEN
          LREJ=.TRUE.
#if defined(YDEBUG)
          IF(LDMP1)
     +      WRITE(LOUT,50) I,PRECTO(I),PRMAX
   50     FORMAT(' YTSTRK: TRACK',I3,' WITH MOMENTUM ',E10.3,
     &      ' ABOVE LIMIT',E10.3,' REJECTED FOR PATTERN RECOGNITION')
#endif
        ELSE
C      REJECT TRACKS WITH LARGE ERROR ON CURVATURE
          CURV=RTABL(KFRFT,I,JFRFIR)
          DCURV=SQRT(RTABL(KFRFT,I,JFRFEM))
          IF(DCURV/ABS(CURV).GT.DCURM) THEN
            LREJ=.TRUE.
#if defined(YDEBUG)
            IF(LDMP1)
     +        WRITE(LOUT,51) I,CURV,DCURV,DCURM
   51       FORMAT(' YTSTRK: TRACK',I3,' WITH RELATIVE CURVATURE ERROR'
     &        ,E10.3,'/',E10.3,
     &        ' ABOVE LIMIT',E10.3,' REJECTED FOR PATTERN RECOGNITION')
#endif
          ENDIF
        ENDIF
C
        IF(LREJ) NGTRTO=NGTRTO-1
#if defined(YDEBUG)
        IF(LDMP1)
     +    WRITE(LOUT,110) I,PRECTO(I),PTRECT(I),
     +    ITABL(KFRTL,I,JFRTNV),ITABL(KFRTL,I,JFRTNI),
     +    ITABL(KFRTL,I,JFRTNT),
     +    RTABL(KFRFT,I,JFRFIR),
     +    RTABL(KFRFT,I,JFRFD0),RTABL(KFRFT,I,JFRFZ0),
     +    SQRT(RTABL(KFRFT,I,JFRFEM)),
     +    SQRT(RTABL(KFRFT,I,JFRFEM+9)),
     +    SQRT(RTABL(KFRFT,I,JFRFEM+14))
  110   FORMAT(1X,I3,1X,2F09.3,2X,
     &    3I3, 1X,E9.2,2F7.2,1X,E9.2,F7.3,F10.3)
#endif
   10 CONTINUE
   11 CONTINUE
C
#if defined(YDEBUG)
      IF(LDMP1)
     +  WRITE(LOUT,120) NGTRTO,(IPTRTO(I),I=1,NGTRTO)
  120 FORMAT(' YTSTRK: NGTRTO=',I5,' IPTRTO:', 20 I3/(20X,20I3))
#endif
      RETURN
  999 CONTINUE
#if defined(YDEBUG)
      ICNER1=ICNER1+1
      IF(ICNER1.LT.100)
     &WRITE(LOUT,*)'YTSTRK : no FRFT bank available return IER=1'
      IF(ICNER1.EQ.100)
     &WRITE(LOUT,*)
     & 'YTSTRK : WARNING! NO FRFT BANK 100 TIMES STOP PRINTING'
#endif
      IER=1
      RETURN
      END
#endif
