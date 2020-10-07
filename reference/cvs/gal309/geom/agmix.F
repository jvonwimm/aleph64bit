*DK agmix
      SUBROUTINE AGMIX(N,NLIS,EPS,A,Z,W,D)
C------------------------------------------------
C! Fills density from thickness of components
C! and prepare weigth , atomic weight and atomic number
C! for subsequent use in GSMIXT
C   Input arguments
C   N     number of components
C   NLIS  array containing the material numbers
C   EPS   array of thicknesses
C   Output arguments
C   A     array of atomic weights
C   Z     array of atomic numbers
C   W     array of relative weights
C   D     density of the compound
C
C   units are cm,g/cm**3
C
C   B. Bloch-Devaux    april 30 1987
C
C-----------------------------------------------------------
#ifndef DOC
      INTEGER NLIS(*)
      REAL EPS(*),A(*),Z(*),W(*)
      REAL UBUF(20)
      INTEGER    NAM(5)
      CHARACTER NAMG*20
#include "gcbank.h"
C ----------------------------------------------------------
      D=0.
C
C  Fetch properties of requested components
C
      E=0.
      DO 10 I=1,N
        IF (GVERSN.LT.3.15) THEN
           CALL GFMATE(NLIS(I),NAM,A(I),Z(I),DENS,RADL,ABSL,UBUF,NW)
        ELSE
           CALL GFMATE(NLIS(I),NAMG,A(I),Z(I),DENS,RADL,ABSL,UBUF,NW)
        ENDIF
        W(I)=EPS(I)*DENS
        D=D+W(I)
        E=E+EPS(I)
 10   CONTINUE
C
C   Compute weigths and density
C
      DO 20 I=1,N
 20   W(I)=W(I)/D
      D=D/E
      END
#endif
