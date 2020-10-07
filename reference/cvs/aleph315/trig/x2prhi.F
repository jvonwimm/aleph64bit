      SUBROUTINE X2PRHI
C-----------------------------------------------------------------------
CKEY TRIG LEVEL2 PRINT
C! dump X2DF banks: martin's tabular format, agrees with DDL
C - T. Medcalf, 4/10/89
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "x2namc.h"
#include "x2cons.h"
C
 9500   FORMAT(/' +++X2TRIG+++  Event accepted by Level 2 Trigger'/)
 9510   FORMAT(/' +++X2TRIG+++  Event rejected by Level 2 Trigger'/)
C
C Extract yes/no decision
C
      SAVE
      NYES = 0
      IX2MS = IW(NAX2MS)
      IF (IX2MS.GT.0) THEN
        DO 9200 IMN = 1, MSKLN
          IF (IW(IX2MS+LMHLEN+IMN).EQ.1) NYES = 1
 9200   CONTINUE
      ELSE
        WRITE(IW(6),*)' No X2MS bank found'
      ENDIF
C
C  Announce result
C
      IF (NYES.EQ.1) THEN
         WRITE(IW(6),9500)
      ELSE
         WRITE(IW(6),9510)
      ENDIF
C
      NAX2DF = NAMIND('X2DF')
      IF ( IW(NAX2DF).EQ.0) THEN
         WRITE(IW(6),*)' No X2DF bank found'
         GOTO 999
      END IF
C
      IPX2DF = 0
      ISTART = 0
      JX2DF  = NAX2DF+1
   30 CONTINUE
      JX2DF = IW(JX2DF-1)
      IF ( JX2DF.EQ.0 ) GO TO 9999
C
C     IPX2DF is pointer to start of processor information
      IPX2DF = JX2DF+2+1
      ISTART = IPX2DF
C
      WRITE (IW(6),4000) IW(JX2DF+1),IW(JX2DF+2)
 4000 FORMAT('0 ==> X2DF BANK FOUND WITH:',/,
     +       '      ',I5,' COLUMNS AND ',I5,' ROWS')
      NWORDS = IW(JX2DF+2)
C
C     Loop over processors
C
      DO 10 I = 1, 24
C
      WRITE (IW(6),1000) IW(IPX2DF),IW(IPX2DF+1),IW(IPX2DF+2)
 1000 FORMAT
     +(1X,' ==> Processor:',I5,'   No. hits:',I10,'   No. tracks:',I10)
C
C     Loop over hits for this processor
      IHIT = IW(IPX2DF+1)
      DO 40 J = 1, IHIT
      IF ( J.EQ.1 ) WRITE (IW(6),1030)
 1030 FORMAT('  ==>  Hit number:     Hit pattern:    Drift time:')
      WRITE (IW(6),1040) J,IW(IPX2DF+2+J),IW(IPX2DF+2+IHIT+J)
 1040 FORMAT('  ==>     ',I5,'          ',Z8,' ',I18)
   40 CONTINUE
C
C     Loop over tracks for this processor
      IOFF = IW(IPX2DF+1)*2
      DO 20 J = 1, IW(IPX2DF+2)
      IF ( J.EQ.1 ) WRITE (IW(6),1010)
 1010 FORMAT('  ==>  Track number:   Track theta:')
      WRITE (IW(6),1020) J,IW(IPX2DF+2+IOFF+J)
 1020 FORMAT('  ==> ',I10,8X,I10)
   20 CONTINUE
C
C     Reset pointer to processor
      IPX2DF = IPX2DF+IOFF+IW(IPX2DF+2)+3
   10 CONTINUE
      GO TO 30
C
 9999 CONTINUE
      LENGTH = IPX2DF-ISTART
      WRITE (IW(6),1050) LENGTH
 1050 FORMAT(1X,' ==> NUMBER OF WORDS IN X2DF BANK WAS ',I6)
C
  999 RETURN
      END
#endif
