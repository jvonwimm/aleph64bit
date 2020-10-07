#if defined(CTEST)
       PROGRAM COTEST
C----------------------------------------------------------------------
C!     Compress a BOS file.
CKEY PRESS
C!
C!     Author : D.Harvatis  MAR 89
C!
C!     Subroutines called : CMPINI, CMPLIS, CMPDRO
C-----------------------------------------------------------------------
#ifndef DOC
       COMMON/BCS/IW(500000)
       CHARACTER*4 FST
       CHARACTER*80 FNAME,FNAMO
       CHARACTER*1 STR
       LOGICAL NOFLOAT
C-----------------------------------------------------------------------
       CALL TIMEST(999.0)
       TIME=0.0
       CALL BNAMES(1000)
       CALL BOS(IW,500000)
       CALL AOPDBS(' ',IER)
       IF (IER.NE.0) THEN
           WRITE(6,*)' Error in opening the DataBase'
           STOP
       END IF
   20  WRITE (6,*) ' Input filename ?'
       READ (5,703) FNAME
  703  FORMAT(A80)
       IF (FNAME.EQ.' ') GO TO 20
       CALL AOPEN(15,FNAME,'    ','DISK',IER)
       IF (IER.NE.0) THEN
           WRITE(6,*)' Error in opening input file '
           STOP
       END IF
   30  WRITE(6,*) ' Output filename ?'
       READ (5,703) FNAMO
       IF (FNAMO.EQ.' ') GO TO 30
       CALL AOPENW(16,FNAMO,'    ','DISK',IER)
       IF (IER.NE.0) THEN
           WRITE(6,*)' Error in opening output file',IER
           STOP
       END IF
  701  FORMAT (I2)
  702  FORMAT (A1)
       CALL TIMEX(T1)
#if defined(APOLLO) || defined(ALEPH_HP) || defined(RS6K) || defined(ALEPH_SGI)
       CLOSE(5)
       OPEN(5,FILE='*DATA CARDS ?',STATUS='OLD')
#endif
       CALL BREADC
       CALL CMPINI(' ',IER)
       IF (IER.NE.0) THEN
           WRITE(6,*) 'CMPINI failed. Error Code :',IER
           STOP
       END IF
       I=-1
   10  CALL AREAD(15,'E',' ',IRET,IER)
       IF (IRET.GE.4) GO TO 100
       IF (IER.NE.0) GO TO 900
       I=I+1
       CALL TIMEX(T3)
       CALL CMPLIS('E',NB)
       CALL TIMEX(T4)
       WRITE (6,710) I,NB
  710  FORMAT (' Event ',I5,' : ',I3,' banks compressed.')
       TIME=T4-T3+TIME
       CALL BWRITE(IW,16,'E')
       CALL BDROP(IW,'E')
       CALL BGARB(IW)
       GOTO 10
  100  CALL BWRITE(IW,16,'0')
       CALL CMPDRO
       CALL TIMEX(T2)
       WRITE (6,*) T2-T1,TIME
       CALL ACLOSE(0,IER)
       STOP
  900  WRITE (6,*) 'ERROR',I+1
       I=I+1
       GOTO 10
       END
#endif
#endif
