      SUBROUTINE TFILPA(KCO,KTR,PADCRO,WIRCRO)
C----------------------------------------------------------------------
C! Return pad and wire crossing angles of a track
C!
C!   Author   :- A. Roth, D.Cinabro        14-DEC-1989
C!   Modified:   R. Johnson                20-OCT-1990
C!               Changes so it can be used in FTPCER
C!               Cleaned out code not being used.
C!               Return results in arg list instead of common block.
C!               Redo calculation using standard ALEPHLIB routines.
C!
C!   Inputs: KCO = Pointer to the row in the TPCO bank
C!           KTR = Pointer to row in TGFT or FRFT bank
C!   Output:
C!      PADCRO    = Pad crossing angle of a track
C!                      0=parallel to the pad
C!      WIRCRO    = Wire crossing angle of a track
C!                      0=perpendicular to the wire
C!
C!======================================================================
#ifndef DOC
#include "tpgpar.h"
#include "bcs.h"
#include "tgftjj.h"
#include "tpcojj.h"
#include "tpccon.h"
#include "tpgeop.h"
#include "tpgeom.h"
#include "alcons.h"
C
      DIMENSION HPT(5),HP(5),S(2),PHIP(2),ZP(2)
C
#include "bmacro.h"
C
C--   Preset angles
C
      PADCRO =  1.48
      WIRCRO =  PADCRO
C
C--   Get some information out of track fit bank TGFT
C
      DO 739 J=JTGFIR,JTGFZ0
        HPT(J)=RW(KTR+J)
  739 CONTINUE
C
C--   and of bank TPCO
C
      INDEX  =       IW(KCO+JTPCIN)
      IPDROW =       INDEX/100000
      ISLOT  =       INDEX/1000 - IPDROW*100
      R      =       RW(KCO+JTPCRV)
C
C--   Get intersection of track with radial position of hit
C
      CALL THLCIR(HPT,R,S,PHIP,ZP,IER)
      IF (IER.NE.0) GO TO 999
C
C--   Get angle of track with radial direction (pad crossing angle)
C
      COSL=1./SQRT(1.+HPT(2)**2)
      SP=S(1)*COSL
      ALPHA=SP*HPT(1)
      THETA= ALPHA + HPT(3)
      IF (THETA.LT.0.) THEN
        THETA=THETA+TWOPI
      ELSEIF (THETA.GE.TWOPI) THEN
        THETA=THETA-TWOPI
      ENDIF
      PADCRO=THETA-PHIP(1)
      IF (PADCRO.LT.-PI) THEN
        PADCRO=PADCRO+TWOPI
      ELSEIF (PADCRO.GE.PI) THEN
        PADCRO=PADCRO-TWOPI
      ENDIF
C
C--   Transform the helix parameters to the sector frame
C
      CALL TGHPTS(ISLOT,HPT,HP)
C
C--   Get the angle of the track with the wires
C
      ALPHA=S(1)*COSL*HP(1)
      WIRCRO=HP(3)+ALPHA
      IF (WIRCRO.LT.-PI) THEN
        WIRCRO=WIRCRO+TWOPI
      ELSEIF (WIRCRO.GT.PI) THEN
        WIRCRO=WIRCRO-TWOPI
      ENDIF
C
  999 CONTINUE
      END
#endif
