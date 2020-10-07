#if defined(DTEST)
       PROGRAM DETEST
C-----------------------------------------------------------------------
C!     Decompress a compressed BOS file.
CKEY PRESS DETEST DECOMPRESS
C!
C!     Author : D.Harvatis
C!
C-----------------------------------------------------------------------
#ifndef DOC
       COMMON/BCS/IW(500000)
       CHARACTER*80 INAME,ONAME,ERLIS
       CHARACTER*4 FST,FORMAT
C-----------------------------------------------------------------------
       CALL TIMEST(999.0)
       TIME=0.0
       CALL BNAMES(1000)
       CALL BOS(IW,500000)
       FORMAT='EPIO'
   20  WRITE(6,*)'Input Filename ?'
       READ(5,703) INAME
       IF (INAME.EQ.' ') GO TO 20
       CALL AOPEN(16,INAME,FORMAT,'DISK',IER)
       IF (IER.NE.0) THEN
           WRITE(6,*)'Failed to open file ',INAME
           STOP
       END IF
   30  WRITE(6,*)'Output Filename ?'
       READ(5,703) ONAME
  703  FORMAT(A80)
       IF (ONAME.EQ.' ') GO TO 30
       CALL AOPENW(17,ONAME,FORMAT,'DISK',IER)
       IF (IER.NE.0) THEN
           WRITE(6,*)'Failed to open file ',ONAME
           STOP
       END IF
       I=-1
       CALL BDROP (IW,'E')
       CALL BLIST(IW,'E=','0')
   10  CALL TIMEX(T3)
       CALL AREAD(16,'E',' ',IRET,IER)
       CALL TIMEX(T4)
       I=I+1
       WRITE (6,701) I,IRET,IER
 701   FORMAT(' Event:',I5,'  Return Code :',I3,'  Error Code :',I8)
       TIME=TIME+T4-T3
       IF (IRET.GE.4) GO TO 100
       IF (IER.LT.0) GO TO 900
       CALL BWRITE(IW,17,'E')
       CALL BDROP(IW,'E')
       CALL BGARB(IW)
       GOTO 10
 100   CALL BWRITE(IW,17,'0')
       WRITE(6,*) 'Read and decompress time :',TIME
       CALL ACLOSE(16,IER)
       CALL ACLOSE(17,IER)
       STOP
 900   WRITE (6,*) ' Not Writting Event ',I
       GOTO 10
       END
#endif
#endif
