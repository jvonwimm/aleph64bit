      SUBROUTINE TPRFX (NSUB, PADCRO, WIRCRO, PRFCOR)
C----------------------------------------------------------------------
C! Get pad response correction factor
C!
C!   Author   :- Andreas ROTH          14-DEC-1989
C!
C!   Inputs:
C!        - NSUB     Number of subpulses
C!        - PADCRO   Pad crossing angle
C!        - WIRCRO   Wire crossing angle
C!
C!   Outputs:
C!        - PRFCOR   Pad response correction factor
C!
C!   Libraries required:   BOS
C!
C!   Description
C!   ===========
C!      This routine corrects the pad response width as determined insid
C!      TPRF2 with respect to the pad and wire crossing angle.
C!      It is called by TPRF2.
C?
C!======================================================================
#ifndef DOC
C
#include "bcs.h"
#include "ternjj.h"
#include "alcons.h"
C
      DATA NTERN/0/
C
C--   Preset pad response correction factor
      PRFCOR =  1.
C
C--   Get the parameters from the database bank
C
      IF (NTERN.EQ.0) NTERN=NAMIND('TERN')
      KTERN=IW(NTERN)
      IF (KTERN.EQ.0) THEN
        CALL RERROR('TPRFX',1,'Cannot find the TERN bank.'//
     +               '  TPC pad response width will be rubbish.')
        RETURN
      ENDIF
      KTERN=KTERN+LMHLEN
      D2= RW(KTERN+JTERP2)
      D3= RW(KTERN+JTERP3)
      C = RW(KTERN+JTERPB)
C
      IF (NSUB .NE. 0) THEN
C
         IF (NSUB .EQ. 2) THEN
            PRFCOR  =  PRFCOR*(1. + C*WIRCRO*RADEG)
     &                       *(1. + D2*(TAN(PADCRO))**2)
         ELSE
            PRFCOR  =  PRFCOR*(1. + C*WIRCRO*RADEG)
     &                       *(1. + D3*(TAN(PADCRO))**2)
         END IF
C
      END IF
      RETURN
      END
#endif
