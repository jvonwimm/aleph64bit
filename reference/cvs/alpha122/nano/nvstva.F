      SUBROUTINE NVSTVA(ITK,JET,IVX)
CKEY NANO IN ALPHA /INTERNAL
C----------------------------------------------------------------------
C! NANO unpacking : auxiliary routine to QVSTVA. Gets jet and vertex
C!                  assignment if input is a NanoDST
C!
C!   Author   :- Gerrit Graefe         25-MAR-1995
C!
C!   Inputs:
C!        - ITK  / I        ALPHA TRACK NUMBER
C!
C!
C!   Outputs:
C!        - JET  / I        JET=1 OR 2 ACCORDING TO JET DIRECTION
C!          IVX  / I        Vertex assignment from QVSRCH
C!                          1 FOR PRIMARY,
C!                          2 FOR SECONDARY (IN ABOVE JET)
C!                          0 FOR TRACK PASSING CUTS BUT NOT CLOSE
C!                                TO EITHER VERTEX
C!                         -1 FOR TRACK FAILING CUTS
C!
C!   Libraries required: ALPHA,BOS77
C!
C!   Description
C!   ===========
C!
C?   This subroutine is a dummy one that only gets the jet/vertex assignment
C?   of a track from the NDTK bank. The real work was done during the
C?   NanoDst production by QVSTVA. Therfore all input arguments except
C?   the track number to QVSTVA are dummy, if a NanoDST is the input.
C?
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
      SAVE M2,NERR
#include "qdecl.h"
#include "qcde.h"
#include "ndtkjj.h"
      INTEGER NVERS,NAMIND,NERR
      INTEGER IVX,JET,INDTK,IROWN,KCNDTK,ITK,KRNDTK,KONDTK
      INTEGER M2
      DATA    NERR/0/
      DATA    M2/3/
C----------------------------------------------------------------------
      JET=0
      IVX=-10
C
C!..CHECK NANODST VERSION
C
      CALL GETNAV(NVERS)
      IF(NVERS.LT.116.AND.NERR.LT.10)THEN
        NERR=NERR+1
        CALL QWMESE
     &    ('+++ NVSTVA +++ No QVSRCH information for NANO<116 !')
        GOTO 999
      ENDIF
C
C!..LINK TO BANK NDTK
C
      INDTK=IW(NAMIND('NDTK'))
      IF(INDTK.EQ.0)THEN
        RETURN
      ENDIF
      KONDTK=INDTK
      KCNDTK=IW(KONDTK+1)
      KRNDTK=IW(KONDTK+2)
      IROWN=KONDTK+LMHLEN+(ITK-KFCHT)*KCNDTK
      JET=IAND(ISHFT(IW(IROWN+JNDTTA),-15),M2)
      IVX=IAND(ISHFT(IW(IROWN+JNDTTA),-17),M2)-1
  999 RETURN
      END
#endif
