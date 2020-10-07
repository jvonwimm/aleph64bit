      SUBROUTINE EPRTPC(IER)
C----------------------------------------------------------------------
C!  - PREPARE TPC TRKS FOR ECAL EXTRAP.
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870125                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : FRFT                                                   *
C!    OUTPUT  : ETP1                                                   *
C!    CREATED : ETP1                                                   *
C!                                                                     *
C! CHANGES HELIX PARAMETRISATION FROM TPC TO ECAL DESCRIPTION          *
C!                                                                     *
C!  MODIFIED : D.Casper 960605  Use extrapolation from outermost       *
C!                              coordinate, as calculated by UFECAL    *
C!                              and stored in the bank TRIK.  This     *
C!                              feature can be turned off by giving    *
C!                              an ENKF card, in which case the old    *
C!                              values, extrapolating from the origin, *
C!                              will be used.                          *
C!             D.Casper 960609  Do not extrapolate incoming tracks of  *
C!                              a kink.                                *
C!           M.Cattaneo 970716  Comment call to UFITQL (results are    *
C!                              never used!!)                          *
C!           M.Cattaneo 981014  Protect against divide by zero         *
C!                                                                     *
C!       IER = return code ( 0 if normal)                              *
C?                                                                     *
C!======================================================================
#ifndef DOC
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "rparac.h"
#include "rflags.h"
#include "frftjj.h"
#include "tarcjj.h"
#include "alcons.h"
#include "ectcom.h"
#include "etp1jj.h"
#include "trikjj.h"
#include "yknkjj.h"
      INTEGER TTLOOP
      INTEGER UFITQL
      LOGICAL FIRST
      SAVE FIRST,NAFRFT,NATARC,NAYKNK
      DATA FIRST /.TRUE./
      DATA RECMN/186.2/,ZECMN/254.2/,THRTR/5.E-05/,NTURN/5/
C
C?  NTURN = MAX NUMBER OF TURNS
C
#include "bmacro.h"
      IF(FIRST)THEN
         FIRST=.FALSE.
         NAFRFT=NAMIND('FRFT')
         NATARC=NAMIND('TARC')
         NAYKNK=NAMIND('YKNK')
         NAENKF=NAMIND('ENKF')
      ENDIF
      IER=0
      KFRFT  = IW(NAFRFT)
      IF(KFRFT.NE.0)THEN
C
C? GET NUMBER OF TRACKS
C
         NTRKS=LROWS(KFRFT)
C
C?   CREATE ETP1 BANK
C
         CALL AUBOS('ETP1',0,NTRKS*LETP1A+LMHLEN,KETP1,IGARB)
         IF (IGARB.EQ.2) GOTO 30
C
C?   FILL MINIHEADER
C
         IW(KETP1+1)=LETP1A
         IW(KETP1+2)=NTRKS
C        KTGFT=IW(NATGFT)
         KFRFT  = IW(NAFRFT)
         KTARC  = IW(NATARC)
         KYKNK  = IW(NAYKNK)
C
C? LOOP ON TRACKS
C
         DO 10 ITK=1,NTRKS
C
C? First of all, set up bank line
C
             KLINE=KROW(KETP1,ITK)
C
C***** Call to UFITQL and subsequent check on return code commented out
C***** by M.Cattaneo, 16-Jul-1997 because UFITQL never returns 5 
C
C? First, verify that the track should be extrapolated, according
C?  to Dave's new flag
C?     4 means bad track
C
C*****             INEWFL=UFITQL(ITK)
C
C? AUG. 29 1991 : bad tracks which should not be associated
C?   are now type 5
C
C*****             IF(INEWFL.EQ.5)THEN
C*****               IFLAG=0
C*****               GO TO 11
C*****             ENDIF
C
C  Do not extrapolate tracks coming into a kink
C
             IF(KYKNK.GT.0)THEN
                DO IKNK = 1, LROWS(KYKNK)
                    IF (ITK .EQ. ITABL(KYKNK,IKNK,JYKNIT)) THEN
                        IFLAG=0
                        GO TO 11
                    ENDIF
                ENDDO
             ENDIF
C
             IARC=TTLOOP(ITK)
             IF(IARC.EQ.0)THEN
C
C? Normal tracks (1 arc only)
C
               RECIP=-RTABL(KFRFT,ITK,JFRFIR)
               TLAM=RTABL(KFRFT,ITK,JFRFTL)
               COTH=TLAM/SQRT(1.+TLAM**2)
               Z0  =RTABL(KFRFT,ITK,JFRFZ0)
               IFRFT=KROW(KFRFT,ITK)
               CALL UNEWDP(RW(IFRFT+1),D0P,PHI0P)
               D0  =   D0P * SIGN(1.,-RECIP)
               PHI0=   PHI0P
             ELSE IF(IARC.GT.0)THEN
C
C? Track loops many turns; take the last arc
C
               RECIP = -RTABL(KTARC,IARC,JTARIR)
               TLAM  = RTABL(KTARC,IARC,JTARTL)
               PHI0  = RTABL(KTARC,IARC,JTARP0)
               D0    = RTABL(KTARC,IARC,JTARD0)
               Z0    = RTABL(KTARC,IARC,JTARZ0)
C
C? If we move in the inverse sense, some parameters have to be
C?   inverted
C
               INVER = ITABL(KTARC,IARC,JTARAN)
               IF(INVER.LT.0)THEN
                 PHI0 = PHI0 - PI
                 D0   = -D0
                 RECIP= -RECIP
                 TLAM = -TLAM
               ENDIF
               COTH  = TLAM/SQRT(1.+TLAM**2)
             ELSE
C
C? Iarc<0 : the track in not good (eg ITC only, etc...)
C
               IFLAG=0
               GO TO 11
             ENDIF
C
C?      SENSE OF TURN (SIGN OF RADIUS)
C
             SIGRA=SIGN(1.,RECIP)
C
C? SIGN OF COSTH
C
             SIGCO=SIGN(1.,COTH)
             SITH=SQRT(1.-COTH**2)
             THETA=ACOS(COTH)
C
C
C?  TO GO TO BARREL
C
             SIPH0=SIN(PHI0)
             COPH0=COS(PHI0)
             DPHI = 0.
C
C?   CHECK THAT CURVATURE IS FINITE; ELSE, MAKE LINEAR EXTRAPOLATION,
C?    AND GIVE STANDARD MAX VALUE TO MOMENTUM
C
             IF(ABS(RECIP).LT.THRTR)THEN
               CCNST=0.
               PXY=90.
               IF(COTH.NE.0)THEN
                 DSZ=(ZECMN-Z0*SIGCO)/ABS(COTH)
                 DSXY=SQRT(RECMN**2-D0**2)/SIN(THETA)
                 DS=AMIN1(DSZ,DSXY)
               ELSE
                 DS=RECMN-ABS(D0)
               ENDIF
               SINBR=-DS*SIN(THETA)
               IFLAG=1
             ELSE
C
C?  ANGLE BETWEEN TRACK AND X AXIS AT ENTRY POINT
C
               RADIU=1./RECIP
               RAD=ABS(RADIU)
               DIST=RAD-D0
               IF( DIST .EQ. 0.0 ) THEN
                  IFLAG=0
                  GO TO 11
               ENDIF
               COPHI=-(RECMN**2-RAD**2-DIST**2)/(2.*RAD*DIST)
C
C?  GET MOMENTUM PROJECTION IN X-Y PLANE
C
               PXY=AAAKEC*RAD*ABS(FILDEC)
C
C?  SEE IF TRACK INTERSECTS ECAL
C
               IF(ABS(COPHI).LT.1.)THEN
                 DSXY=RAD*ACOS(COPHI)/SIN(THETA)
C
C?  Check that costheta is finite (avoid divide by zero)
C
                 IF(COTH.NE.0)THEN
                    DSZ=(ZECMN-Z0*SIGCO)/ABS(COTH)
                    DS=AMIN1(DSZ,DSXY)
                 ELSE
                    DS=DSXY
                 ENDIF
               ELSE
                 IF(COTH.NE.0)THEN
                    DS=(ZECMN-Z0*SIGCO)/ABS(COTH)
                 ELSE
                    IFLAG=0
                    GO TO 11
                 ENDIF
               ENDIF
C
C?    DO NOT EXTRAPOLATE TRACKS WHICH MAKE
C?   MORE THAN NTURN  LOOPS BEFORE THEY REACH ECAL
C
               IFLAG=0
               IF(DS.GT.TWOPI*RAD*FLOAT(NTURN))GO TO 11
               IFLAG=1
C
C? PHI CHANGE FROM POINT 0, TO ENTRY INTO ECAL
C
               DPHI=-DS*SITH/RADIU
C
               SINB=SIN(DPHI)
               COSB=COS(DPHI)
               CCNST=(1.-COSB)/SINB
               SINBR=SINB*RADIU
             ENDIF
             X=-D0*SIGRA*SIPH0+SINBR*(CCNST*SIPH0-COPH0)
             Y= D0*SIGRA*COPH0-SINBR*(CCNST*COPH0+SIPH0)
             Z=Z0+DS*COTH
             ALPHA=PHI0+DPHI
C
C?  MAKE PROJECTIONS OF P
C
             PX=PXY*COS(ALPHA)
             PY=PXY*SIN(ALPHA)
             PZ=PXY*COTH/SITH
             PTOT=SQRT(PX**2+PY**2+PZ**2)
             PX=PX/PTOT
             PY=PY/PTOT
             PZ=PZ/PTOT
C
C?  MAKE CHARGE
C
             CHRGE=SIGN(1.,FILDEC*SIGRA)
C
C?  FILL BANK ETP1 (ONE ROW)
C
 11          CONTINUE
             IW(KLINE+JETPEF)=IFLAG
             IF (IFLAG.NE.0) THEN
               KTRIK = NLINK('TRIK',ITK)
               IF (IW(NAENKF).NE.0 .OR. KTRIK.EQ.0 ) THEN
                 RW(KLINE+JETPX3)=X
                 RW(KLINE+JETPX3+1)=Y
                 RW(KLINE+JETPX3+2)=Z
                 RW(KLINE+JETPP3)=PX
                 RW(KLINE+JETPP3+1)=PY
                 RW(KLINE+JETPP3+2)=PZ
                 RW(KLINE+JETPPT)=PTOT
                 RW(KLINE+JETPCH)=CHRGE
               ELSE
                 CALL UCOPY(RW(KTRIK+LMHLEN+JTRIX3),RW(KLINE+JETPX3),8)
               ENDIF
             ENDIF
   10    CONTINUE
      ELSE
C
C?   IF FRFT DOES NOT EXIST,CREATE EMPTY BANK; FILL MINIHEADER
C
         NTRKS=0
         CALL AUBOS('ETP1',0,NTRKS*LETP1A+LMHLEN,KETP1,IGARB)
         IF (IGARB.EQ.2) GOTO 30
         IW(KETP1+1)=LETP1A
         IW(KETP1+2)=NTRKS
         IF (JDBDRF(4) .GE. 1) THEN
            CALL RERROR('EPRTPC',1,' BANK FRFT DOES NOT EXIST')

         ENDIF
      ENDIF
C
C?  PUT BANK IN LIST
C
      CALL BLIST(IW,'E+','ETP1')
      RETURN
   30 CONTINUE
      CALL RERROR('EPRTPC',2,' exit')
      IER=1
      RETURN
      END
#endif
#endif
