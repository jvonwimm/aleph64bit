      SUBROUTINE QFD0Z0                   
CKEY  FILL CHARGED / INTERNAL
C----------------------------------------------------------------------
C! Recompute d0 and z0 of ALPHA charged tracks
C!                                               J.Boucrot  03.02.98
C Called from QFILL 
C Computes the best beam position QVXN,QVYN,QVZN from user's request
C then recomputes the d0 and z0 of all charged tracks
C In common /IDZVRT/ the flag ID0Z0FL is set to the following value:
C  ID0Z0FL = 1 if QVXN,QVYN,QVZN from the QFNDIP event primary vertex 
C  ID0Z0FL = 2 if QVXN,QVYN,QVZN from the event-chunk beam position 
C   To get any of the above values, the data card 'D0NW' must be
C   present in the user's data cards
C  ID0Z0FL = 3 if QVXN,QVYN,QVZN from the LEP 1 run-by-run average
C              (= QVXNOM,QVYNOM,QVZNOM from the "Toby Burnett method")
C  ID0Z0FL = 0 if none of the above values available 
C              in that case QVXN=QVYN=QVZN=0.
C  This value can be accessed through the user-function KD0FL(dummy)
C----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
      PARAMETER ( NLRL1 = 37853 )
      COMMON /IDZVRT/ ID0Z0FL
#include "qmacro.h"
C----------------------------------------------------------------------
C First compute QVXN,QVYN,QVZN = best beam position for this event
C
C Initialisations :
      ID0Z0FL=0
      QVXN=0.     
      QVYN=0. 
      QVZN=0.
      QVED=0.
      QVEZ=0.
C For LEP 1 data the default value may be the run-by-run one: 
      IF (KRUN.LE.NLRL1) THEN
         ID0Z0FL=3
         QVXN=QVXNOM
         QVYN=QVYNOM
         QVZN=QVZNOM
         QVED=SQRT(QVXNSG+QVYNSG)
         QVEZ=QVZNSG
      ENDIF                                                         
C Other values cannot be obtained without the data card 'D0NW':
      IF (IW(NAMIND('D0NW')).EQ.0) GO TO 40
C .. or if the event-chunk beam position is not available for this event: 
      IF (.NOT.XGETBP) GO TO 40
C If this is OK, the event-chunk beam position may be taken:
      ID0Z0FL=2
      QVXN=QVTXBP(1)    
      QVYN=QVTXBP(2)
      QVZN=QVTXBP(3)
      QVED=SQRT(QVTEBP(1)*QVTEBP(1)+QVTEBP(2)*QVTEBP(2))
      QVEZ=QVTEBP(3)
C If the user required QFNDIP, one may have an even better value:
C the 'QFND' card must be present:
C and the main vertex must have been computed with QFNDIP :
      IF (IW(NAMIND('QFND')).EQ.0) GO TO 40
      IOKV=0 
      DO 10 IVX=KFREV,KLREV
         IF (KVTYPE(IVX).NE.1) GO TO 10
         IF (QVCHIF(IVX).LE.0.) GO TO 10
         IOKV=IVX
         GO TO 20
 10   CONTINUE
 20   CONTINUE
      IF (IOKV.EQ.0) GO TO 40 
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


