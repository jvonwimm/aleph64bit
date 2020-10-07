      SUBROUTINE QFD0Z0                   
CKEY  FILL CHARGED / INTERNAL
C----------------------------------------------------------------------
C! Recompute d0 and z0 of ALPHA charged tracks
C!                                               J.Boucrot  03.02.98
C!   modified for new getlep/getbp               J.Boucrot  28.09.98
C Called from QFILL 
C Recomputes the d0 and z0 of all charged tracks using the event
C primary vertex
C The execution of this routine is under the control of the data card
C D0NW
C In common /IDZVRT/ the flag ID0Z0FL is set to the following value:
C  ID0Z0FL = 1 if d0,z0 from the QFNDIP event primary vertex 
C  ID0Z0FL = 2 if d0,z0 from the event-chunk beam position 
C  ID0Z0FL = 3 if d0,z0 from the LEP 1 run-by-run average
C  ID0Z0FL = 0 if none of the above values available 
C  This value can be accessed through the user-function KD0FL(dummy)
C----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
      PARAMETER ( NLRL1 = 37853 )
      COMMON / IDZVRT  / ID0Z0FL
      COMMON / GETFLGS / IGETEN, IGETXY
#include "qmacro.h"
C----------------------------------------------------------------------
      ID0Z0FL=0
      IF (IGETXY.EQ.4) ID0Z0FL=2
      IF (IGETXY.GE.1.AND.IGETXY.LE.3) ID0Z0FL=3
      QVXN=0.     
      QVYN=0. 
      QVZN=0.
      QVED=0.
      QVEZ=0.
C the data card 'D0NW' MUST be present to get d0,z0 from QFNDIP::
      IF (IW(NAMIND('D0NW')).EQ.0) GO TO 999
C the 'QFND' card must be present:
C and the main vertex must have been computed with QFNDIP :
      IF (IW(NAMIND('QFND')).EQ.0) GO TO 999
      IOKV=0 
      DO 10 IVX=KFREV,KLREV
         IF (KVTYPE(IVX).NE.1) GO TO 10
         IF (QVCHIF(IVX).LE.0.) GO TO 10
         IOKV=IVX
         GO TO 20
 10   CONTINUE
 20   CONTINUE
      IF (IOKV.EQ.0) GO TO 999
      QVXN=QVX(IOKV)
      QVYN=QVY(IOKV)
      QVZN=QVZ(IOKV)
      IF (.NOT.XFILEM) GO TO 30 
      IF (QVEM(IOKV,1,1).LT.0.) GO TO 30
      QVED=SQRT(QVEM(IOKV,1,1)*QVEM(IOKV,1,1)+QVEM(IOKV,2,2)*
     +          QVEM(IOKV,2,2))
      QVEZ=QVEM(IOKV,3,3)
 30   ID0Z0FL=1
 40   CONTINUE
C----------------------------------------------------------------------
C Now recompute the d0,z0 of all charged tracks and store them in QVEC:
C
      JFRFT = IW(NAFRFT) + LMHLEN
      JQVEC = KOQVEC + KFCHT * KCQVEC
      LFRFT = IW(JFRFT-1) 
      NFRFT = IW(JFRFT)      
C
C  Loop on all tracks of bank FRFT :
C
      DO 100 NJUL = 1, KNCHT
C
C         track angles :
C
        CP = COS (RW(JFRFT+JFRFP0))
        SP = SIN (RW(JFRFT+JFRFP0))
        TL = RW(JFRFT+JFRFTL)
C
C         distance to interaction point :
C
        RW(JQVEC+JQVEDB) = RW(JFRFT+JFRFD0) +
     +    QVYN * CP - QVXN * SP
        RW(JQVEC+JQVEZB) = RW(JFRFT+JFRFZ0) - QVZN
     &   + RW(JFRFT+JFRFTL) * ( QVXN * CP + QVYN * SP )
C Errors can be defined only if XFILEM=.TRUE. :
        IF (.NOT.XFILEM) GO TO 50
        RW(JQVEC+JQVESD) = SQRT(RW(JFRFT+JFRFEM+9) * 
     &  RW(JFRFT+JFRFEM+9) + QVED * QVED )
        RW(JQVEC+JQVESZ) = SQRT(RW(JFRFT+JFRFEM+14) *
     &  RW(JFRFT+JFRFEM+14) + QVEZ * QVEZ )
        DET = RW(JQVEC+JQVESD) * RW(JQVEC+JQVESZ) -
     &        RW(JFRFT+JFRFEM+13)**2
        IF (DET .GT. 0.)  RW(JQVEC+JQVECB) =
     &   (RW(JQVEC+JQVEDB)**2 * RW(JQVEC+JQVESZ) +
     &    RW(JQVEC+JQVEZB)**2 * RW(JQVEC+JQVESD) -
     &    2. * RW(JQVEC+JQVEDB) * RW(JQVEC+JQVEZB) *
     &    RW(JFRFT+JFRFEM+13)) / DET
 50     CONTINUE
C
        JQVEC = JQVEC + KCQVEC
        JFRFT = JFRFT + LFRFT
 100  CONTINUE
C-----------------------------------------------------------------------
C Now do the same with Energy Flow objects which are charged tracks
C
      IF (KNEFT.LE.0) GO TO 999
      
      DO 500 IEFT=KFEFT,KLEFT
         IEFTY=KEFOTY(IEFT)
C Consider only charged tracks:
         IF (IEFTY.LT.0.OR.IEFTY.GT.3) GO TO 500
         IT=KEFOLT(IEFT)
         IF (IT.LE.0.OR.IT.GT.NFRFT) GO TO 500
         ICHT=KFCHT+IT-1
         JQVFR = KOQVEC + ICHT * KCQVEC
         JQVEF = KOQVEC + IEFT * KCQVEC
         RW(JQVEF+JQVEDB) = RW(JQVFR+JQVEDB)
         RW(JQVEF+JQVEZB) = RW(JQVFR+JQVEZB)
         IF (.NOT.XFILEM) GO TO 500
         RW(JQVEF+JQVESD) = RW(JQVFR+JQVESD)
         RW(JQVEF+JQVESZ) = RW(JQVFR+JQVESZ)
         RW(JQVEF+JQVECB) = RW(JQVFR+JQVECB)
 500  CONTINUE
C
 999  RETURN
      END
#endif

