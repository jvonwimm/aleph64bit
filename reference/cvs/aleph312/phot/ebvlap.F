      SUBROUTINE EBVLAP( NVTM , ISC , TVLP , STAC )
C ----------------------------------------------------
C   AUTHOR   : J. Badier     05/10/89
C! Add overlap correction to the stack 1 of the endcap.
CKEY PHOTONS OVERLAP / INTERNAL
C
C   The correction calculated in EBCLAP is added to the first endcap
C   stack. The central tower overlap code NVTM allows to increment the
C   good element of the STAC array.
C
C   Input     : NVTM    Overlap code.
C               ISC     Subcomponent.
C               TVLP    Calculated correction.
C               STAC    Gathered stacs accordingly to EBSLIM.
C                       STAC is modified.
C
C   BANKS :
C     INPUT   : NONE
C     OUTPUT  : NONE
C
C ----------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION STAC(*)
      IF( NVTM .EQ. 0 ) THEN
C   Out of the overlap region.
        IF( ISC .EQ. 2 ) THEN
          STAC(3) = STAC(3) + TVLP
        ELSE
          STAC(1) = STAC(1) + TVLP
        ENDIF
      ELSE
        IF( NVTM .EQ. 1 ) STAC(1) = STAC(1) + TVLP
        IF( NVTM .EQ. 2 .OR. NVTM .EQ. 4 ) STAC(2) = STAC(2) + TVLP
        IF( NVTM .EQ. 3 .OR. NVTM .EQ. 5 .OR.
     +      NVTM .EQ. 6 )                  STAC(3) = STAC(3) + TVLP
      ENDIF
      RETURN
      END
#endif
