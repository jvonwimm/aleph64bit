      SUBROUTINE OPENSQ (LUNIT,FORMA,FNAME)
C----------------------------------------------------------------------
CKEY IO OPEN DATA
C! Open for sequential read a file on unit # LUNIT
C - F.Ranjard - 880330
C - Input arguments:
C             LUNIT   = input unit #
C             FORMA   = 4 character flag  'EPIO' or '    ' or 'FORT'
C - Output argument:
C             FNAME   = file name
C --------------------------------------------------------------------
#ifndef DOC
      CHARACTER*4 FORMA
      CHARACTER*(*) FNAME
      DATA LREC / 0 /
C----------------------------------------------------------------------
      FNAME = ' '
#if defined(ALEPH_DEC) || defined(ALEPH_SGI)
      OPEN(LUNIT,STATUS='OLD',FORM='UNFORMATTED',READONLY)
      INQUIRE (LUNIT,NAME=FNAME)
      IF (FORMA .EQ. 'EPIO') CLOSE(LUNIT)
#endif
      CALL BUNIT(LUNIT,FORMA,LREC)
      END
#endif
