      SUBROUTINE TPGETR(IBRTE)
C-----------------------------------------------------------------------
C!  Fill the track element common with the primary parameters from
C!  one broken track element for the dE/dX routine
C
C  Called from:  TSTEER
C  Calls:        None
C
C  Inputs:   PASSED:      --IBRTE,   the broken track element number
C            /TRAKEL/     --xxB(,i), arrays containing primary parameter
C                                    for all broken track elements from
C                                    the current full track element
C
C  Outputs:  /TRAKEL/     --primary track parameters for this broken
C                           track element as if it were a full track
C                           element, to be used by TSDEDX
C  D. DeMille
C
C-----------------------------------------------------------------------
C
C  TRAKEL:  track parameters for dE/dX and carrying around broken
C  tracks
C
      COMMON/TRAKEL/NTRK,X(3),VECT(3),ABSMOM,SEGLEN,TOF,AMASS,CHARGE,
     *              RAD,CENT(2),DELPSI,PSI1,ALPH01,ALPH02
C - MXBRK = 2* MAX(NLINES(1..3)) + 2 , NLINES= 8,10,10 in /SCTBND/
      PARAMETER (MXBRK=22, MXBRTE=MXBRK/2)
      COMMON/BRKNTK/XB(3,6),VECTB(3,6),SEGLNB(6)
C
C  If this is the first broken track element from a particular track,
C  change units to MeV for the dE/dX routine
C
      IF ( IBRTE .EQ. 1 ) THEN
         ABSMOM = ABSMOM*1.E3
         AMASS = AMASS*1.E3
      ENDIF
C
      X(1) = XB(1,IBRTE)
      X(2) = XB(2,IBRTE)
      X(3) = XB(3,IBRTE)
C
      VECT(1) = VECTB(1,IBRTE)
      VECT(2) = VECTB(2,IBRTE)
      VECT(3) = VECTB(3,IBRTE)
C
      SEGLEN = SEGLNB(IBRTE)
C
      RETURN
      END
