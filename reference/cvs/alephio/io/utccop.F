      SUBROUTINE UTCCOP(ICHIN,ICOUT,NWORD)
C.
C! - Copy one textstring into another
C! - This subroutine is needed since UCOPY doesn't work properly for
C! - characters on VAX.
C.
C. - Author   : A. Putzer  - 87/08/18
C.
C.
C.   Arguments: -  ICHIN CHA*4 (input) Character string to be copied
C.              -  ICOUT CHA*4 (output) Character to be copied into
C.              -  NWORD INTE  (input) Number of 32-bit words to be
C.                                     copied
C.  Comment   :    The order of the arguments which does not correspond
C.                 to the ALEPH rules is kept as for the original UCOPY
C.
C ---------------------------------------------------------------------
#ifndef DOC
      CHARACTER*4 ICHIN(*),ICOUT(*)
      DO 100 I=1,NWORD
 100  ICOUT(I) = ICHIN(I)
      RETURN
      END
#endif
