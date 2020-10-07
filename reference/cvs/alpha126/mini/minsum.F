      SUBROUTINE MINSUM(MODE,LRUN,SUMRY)
C-----------------------------------------------------------------------
C! Write summary file for the Mini-DST.
CKEY MDST /INTERNAL
C
C     Author: Stephen Haywood         08-May-90
C     Modify: Agnieszka Jacholkowska  18-May-95
C
C     Input  : MODE   = -1 for initialisation of job
C                     = +1 for run termination, then run initialisation
C                     = +2 for run termination only
C              LRUN   = last run number
C              SUMRY  = summary array
C-----------------------------------------------------------------------
#ifndef DOC
C
      REAL SUMRY(*)

#include "bcs.h"
      PARAMETER (JRLELE=1)
      PARAMETER (JRHAPV=5)
      PARAMETER (JASIYM=1)
      LOGICAL PRESNT
      CHARACTER*120 STRING
      CHARACTER*16  FLNAM
      CHARACTER*8  DATE,TIME
      CHARACTER*80 C1
      INTEGER SYSTEM
      EXTERNAL SYSTEM
      INTEGER NCHARS,NLINES,IRC,IOVFL, LUN
      DIMENSION ERR(2)
#include "bmacro.h"
c --------------------------------------------------------------------
C
      FLNAM = 'MINITEMP.SUMMARY'
      LUN   = 66
C
      CALL DATIMH(DATE,TIME)

      IF(MODE.eq.-1) THEN
C
C --- start of job

       INQUIRE(FILE=FLNAM,EXIST=PRESNT,IOSTAT=IOS) 
       IF (PRESNT) THEN
          OPEN(LUN,FILE=FLNAM,STATUS='old')
C        position at the end of file
   20     READ(LUN,'(A)',END=30) C1
          GOTO 20
   30     BACKSPACE LUN
       ELSE
          OPEN(LUN,FILE=FLNAM,STATUS='new')
       ENDIF
C
C++   At the start of job, make an initial entry in summary file.
C
       WRITE(LUN,'('' Start job '',2A10)') DATE,TIME

C -----------------

      ELSE IF(MODE.ge.+1) THEN
C
C --- MODE=1 end of run followed by start of next run
C     MODE=2 job termination

C++   Replace initial entry in summary file by summary information.
C
       BACKSPACE LUN
       IF(LRUN.GE.0) THEN
C
C++   Obtain LEP energy.
C
         KRLEP = IW(NAMIND('RLEP'))
         IF(KRLEP.GT.0) THEN
          ROOTS = 2. / 1000. * FLOAT(ITABL(KRLEP,1,JRLELE))
         ELSE
          ROOTS = 0.
         ENDIF
C
C++   Investigate whether errors have occurred; reset them if so.
C
         NERR = 0
C
         CALL TIMEX(T)
C
C++   Fill string.
C
         STRING = ' '
         WRITE(STRING(  1:  6),'(I6)') LRUN
         WRITE(STRING(  7: 12),'(I6)') NINT(SUMRY(1))
         WRITE(STRING( 13: 18),'(F6.1)') ROOTS
         STRING( 19: 21) = ' '
         WRITE(STRING( 22: 25),'(I4)') MINVSN(DUM)
         WRITE(STRING( 26: 34),'(A9)') DATE
         WRITE(STRING( 35: 43),'(A9)') TIME
         WRITE(STRING( 44: 49),'(I6)') NINT(T)
         STRING( 50: 52) = ' '
         WRITE(STRING( 53: 58),'(I6)') NINT(SUMRY(2))
         WRITE(STRING( 59: 63),'(F5.2)') SUMRY(3)
         WRITE(STRING( 64: 69),'(I6)') NINT(SUMRY(4))
         STRING( 70: 72) = ' '
         WRITE(STRING( 73: 81),'(I9)') NINT(SUMRY(5))
         WRITE(STRING( 82: 86),'(F5.2)') SUMRY(6)
         WRITE(STRING( 87: 94),'(I8)') NINT(SUMRY(7))
         STRING( 95: 97) = ' '
         WRITE(STRING( 98:120),'(I7)') NERR
C
C++   If this is MC data, save generator code, Julia vsn and year.
C
         IF (LRUN.LE.2000) THEN
           MCGEN = 0
           MCJUL = 0
           IYEAR = 0
           KRHAH = IW(NAMIND('RHAH'))
           IF (KRHAH.GT.0) THEN
            MCGEN = ITABL(KRHAH,1,JRHAPV)
            MCJUL = ITABL(KRHAH,3,JRHAPV)
           ENDIF
           KASIM = IW(NAMIND('ASIM'))
           IF (KASIM.GT.0) THEN
            IYEAR = ITABL(KASIM,1,JASIYM)
           ENDIF
           WRITE(STRING( 73: 81),'(I9)') MCGEN
           WRITE(STRING( 82: 86),'(I5)') MCJUL
           WRITE(STRING( 87: 94),'(I8)') IYEAR
         ENDIF
C
C++   Write summary information.
C
         WRITE( IW(6),'('' >>'',A120)') STRING
         WRITE(LUN,'(A120)') STRING
       ENDIF
C
C++   Make an initial entry in summary file.
C++   The deletion of this entry indicates the successful completion
C++   of the program.
C
       IF(MODE.EQ.+1) THEN
        CALL ABRUEV(IRUN,IEVT)
        WRITE(LUN,'('' Start run '',I10,2A10)') IRUN,DATE,TIME
       ELSE
C++ MODE=2 Close the summary file.
        CLOSE(LUN)
       ENDIF
C
      ENDIF
C ---------------------------
C
      CALL MINCHK(MODE,LRUN,SUMRY)
C
      RETURN
      END
#endif
