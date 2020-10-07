      SUBROUTINE TPFBRT(PSIN,PSOUT,NBRTE)
C-----------------------------------------------------------------------
C!  Find the primary track parameters for pieces of a track element
C!  and load them into a named common block.
C
C  Called from:  TPBRTK
C  Calls:        None
C
C  Inputs:   PASSED:      --NBRTE,  the number of broken track elements
C                                   from the original element
C                         --PSIN,   the angle (measured counter-
C                                   clockwise) between the X-axis and
C                                   the radius to the first point of
C                                   each broken track element (all in
C                                   X-Y projection)
C                         --PSOUT,  same for last point of each broken
C                                   track element
C            /TRAKEL/     --primary and secondary parameters of the
C                           original track element
C
C  Outputs:  /TRAKEL/     --xB(,i), arrays containing the primary
C                                   parameters for each broken
C                                   track element
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
      DIMENSION PSIN(NBRTE),PSOUT(NBRTE)
C
      A1 = SQRT( 1. - VECT(3)*VECT(3) )
      A2 = RAD * VECT(3) / A1
      A3 = SQRT( RAD*RAD + A2*A2 )
C
      IF ( DELPSI .GT. 0. ) THEN
         PSIBGN = PSI1
      ELSE
         PSIBGN = PSI1 + ABS(DELPSI)
      ENDIF
C
C  Loop over broken track elements to fill common BRKNTK
C
      DO 1 IBRTE = 1, NBRTE
C
         CPSI = COS( PSIN(IBRTE) )
         SPSI = SIN( PSIN(IBRTE) )
C
         XB(1,IBRTE) = CENT(1) + RAD*CPSI
         XB(2,IBRTE) = CENT(2) + RAD*SPSI
         XB(3,IBRTE) = X(3) + A2*ABS( PSIN(IBRTE) - PSIBGN )
C
         VECTB(1,IBRTE) = -A1*SPSI*SIGN(1.,DELPSI)
         VECTB(2,IBRTE) =  A1*CPSI*SIGN(1.,DELPSI)
         VECTB(3,IBRTE) =  VECT(3)
C
         SEGLNB(IBRTE) = A3 * ABS( PSOUT(IBRTE) - PSIN(IBRTE) )
C
 1    CONTINUE
C
      RETURN
      END
