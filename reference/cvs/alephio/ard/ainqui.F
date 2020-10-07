      SUBROUTINE AINQUI(LUN,FNAME,LEX)
C-----------------------------------------------------------------------
C - D.schlatter - 880811
C!        INQUIRE on VAX and APOLLO, FILEDEF on IBM
C    ON VAX,APOLLO:
C       Input:  LUN    --> Output: FNAME
C    or         FNAME  -->         LEX=.true. or .false.
C
C    on IBM:
C       Input:  LUN,FNAME --> Output:  LEX
C-----------------------------------------------------------------------
#ifndef DOC
      CHARACTER*(*) FNAME
      CHARACTER*80 MSG
      LOGICAL LEX
C
#if defined(ALEPH_DEC) || defined(UNIX)
      IF(LUN.EQ.0) INQUIRE(FILE=FNAME,EXIST=LEX)
      IF(LUN.NE.0) INQUIRE(UNIT=LUN,NAME=FNAME)
#else
#if defined(IBM)
      IF(LUN.EQ.0) INQUIRE(FILE='/'//FNAME,EXIST=LEX)
      IF(LUN.NE.0) INQUIRE(UNIT=LUN,NAME=FNAME)
#else
      LEX=.TRUE.
#endif
#endif
      RETURN
      END
#endif
