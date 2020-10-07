      SUBROUTINE SIDAMP(IAD,IADEX,NPA)
C******************************************************************
C!  - Expand addresses of a dead Amplex of address IAD into NPA addresse
C!
C!  AUTHOR  :  B. Bloch-Devaux   june 1992
C!
C!    INPUT  : IAD  address of AMPLEX (14 bits ) +Amplex bit
C!    OUTPUT : IADEX array of expanded addresses ( 14 bits)
C!             NPA   size of array
C?
C!======================================================================
#ifndef DOC
      DIMENSION IADEX(16)
      PARAMETER ( IAMPX = 16384,IOFR = 4)
C
      IAD = IAD-IAMPX
      DO 10 I = 1,16
        IADEX(I) = IAD +(I-1)*IOFR
   10 CONTINUE
      NPA = 16
      RETURN
C
      END
#endif
