      SUBROUTINE WGARB(JW)
C
C     GARBAGE COLLECTION FOR WORK BANKS
C
#include "boscom.h"
      INTEGER INDN(200)
      EQUIVALENCE (INTA(1),INDN(1))
#include "param.h"
      INTEGER JW(*)
C     ...
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'WGARB')
      JW(1)=0
      JW(2)=0
      IF(JW(NDW).EQ.0) GOTO 100
C
C     STATISTIC ON GARBAGE COLLECTION
C
      JW(NGW)=JW(NGW)+1
      NUS=JW(NJW)-JW(IWK)+JW(IGP)-JW(NDW)
      IH=10.0*FLOAT(NUS)/FLOAT(JW(NJW))+1.0
      IH=MIN(10,MAX(1,IH))
      JW(NFH+IH)=JW(NFH+IH)+1
C
      ICOP=JW(IGW)
      ISTP=JW(IGW)
C
   10 ID  =JW( IWK)
      NSTP=0
      N   =0
C
C     STORE INDEX AND WORD COUNT FOR LATER COPY OF WORK BANK
C
   20 IF(JW(ID+3).GE.0) THEN
C
C        WORK BANK IS DROPPED
C
         IF(N.GT.0.AND.ID.EQ.INDN(N-1)+INDN(N)) THEN
            INDN(N)=INDN(N)+JW(ID+1)
         ELSE
            IF(N.GE.200) THEN
               NSTP=INDN(N-1)+INDN(N)
               N   =0
            END IF
            N=N+2
            INDN(N-1)=ID
            INDN(N  )=JW(ID+1)
         END IF
      END IF
      ID=ID+JW(ID+1)
      IF(ID.LT.ISTP) GOTO 20
C
C     COPY BANKS, TO REMOVE GAPS
C
      DO 30 J=1,N,2
         I=N-J
         ICOP=ICOP-INDN(I+1)
         CALL UCOPY2(JW(INDN(I)),JW(ICOP),INDN(I+1))
   30 CONTINUE
      ISTP=NSTP
      IF(ISTP.NE.0) GOTO 10
C
C     UPDATE INDICES
C
      JW(IWK)=ICOP
      JW(NDW)=0
      ID=ICOP+3
   40 IF(ID.LT.JW(IGW)) THEN
         JW(JW(ID-1))=ID
         ID=ID+JW(ID-2)
         GOTO 40
      END IF
      JW(IGW)=0
C
  100 RETURN
      END
