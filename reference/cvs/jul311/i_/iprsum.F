      SUBROUTINE IPRSUM
C----------------------------------------------------------------------
C!Print run statistics for ITC
C!
C!   Author     :-  J. Sedgbeer  88/05/09
C!   Modified   :-  I. Tomalin   88/08/25 to print out timing info.
C!   Modified   :-  J.Sedgbeer + I.Tomalin   89/06/05
C!
C!   Input:   commons: /IPDSTC/ prepare data stats.
C!                     /ITTIME/ timing information.
C!
C!   Output:  none.
C!
C!   called by : RSMJOB
C!   calls     : none
C!
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "rlunit.h"
C-----------------------------------------------------------------------
C I/O commons etc.
#include "ipdstc.h"
#include "ittime.h"
      REAL AVEL(8)
C-----------------------------------------------------------------------
      WRITE(LDEBRL,1000)
 1000 FORMAT('0',20('-'),' IPRSUM: ITC JOB SUMMARY ',33('-'))
C
C-----------------------------------------------------------------------
C ITC Prepare data stats. from /IPDSTC/
C
      WRITE(LDEBRL,1001) (ISTAIP(I),I=1,4)
 1001 FORMAT('0','ITC Prepare Data Statistics:',/
     +        1X,'No. of events processed thru IPREDA   ',I10,/
     +        1X,'No. of events with IDIG bank          ',I10,/
     +        1X,'No. of events with empty IDIG bank    ',I10,/
     +        1X,'No. of events with non-empty IDIG bank',I10)
C
      IF (ISTAIP(4).EQ.0) GOTO 999
C
      SEVT  = FLOAT(ISTAIP(4))
      AVDIG = FLOAT(ISTAIP(5))/SEVT
      WRITE(LDEBRL,1002) AVDIG,(ISTAIP(I),I=6,7)
 1002 FORMAT (1X,'Average no. of digis per event        ',F10.1,/
     +        1X,'Maximum no. of digis. in an event     ',I10,/
     +        1X,'Minimum no. of digis. in an event     ',I10)
C
      AVCOO = 0.
      IF(ISTAIP(8).GT.0)AVCOO = FLOAT(ISTAIP(9))/FLOAT(ISTAIP(8))
      WRITE(LDEBRL,1003) AVCOO,(ISTAIP(I),I=10,11)
 1003 FORMAT (1X,'Average no. of coords per event       ',F10.1,/
     +        1X,'Maximum no. of coords. in an event    ',I10,/
     +        1X,'Minimum no. of coords. in an event    ',I10)
C
      DO 10 I=1,LSTAIP
        AVEL(I) = FLOAT(ILSTIP(I,1))/SEVT
   10 CONTINUE
C
      WRITE(LDEBRL,1004) (AVEL(I),I=1,8),(ILSTIP(I,2),I=1,8),
     +                   (ILSTIP(I,3),I=1,8)
 1004 FORMAT('0',23X,'Layer',5X,'1',5X,'2',5X,'3',5X,'4',5X,'5',
     +                       5X,'6',5X,'7',5X,'8',/
     +        1X,'Ave. no. of digis. per layer',8F6.1,/
     +        1X,'Max. no. of digis. per layer',8I6,/
     +        1X,'Min. no. of digis. per layer',8I6)
C
C-----------------------------------------------------------------------
C Module timing statistics from /ITTIME/
C
      WRITE(LDEBRL,1005) (TPREDA/SEVT),(TTRECI/SEVT),(TTCTPC/SEVT),
     +(TNSECT/SEVT),(TSRCH/SEVT),(TTLNK1/SEVT),(TTREE1/SEVT),
     +(TFIND1/SEVT),(TFITP1/SEVT),(TTRECF/SEVT),
     +(TTCTRK/SEVT),(TTLNK2/SEVT),(TTREE2/SEVT),(TFIND2/SEVT),
     +(TFITP2/SEVT),(TMATCH/SEVT),(TTRMCA/SEVT)
 1005 FORMAT(
     +  /' Average CPU time per event used by IPREDA = ',F5.3,' secs',
     +  /' Average CPU time per event used by ITRECI = ',F5.3,' secs',
     +  /' Average CPU time per event used by ITCTPC = ',F5.3,' secs',
     +  /'    (including INSECT=',F5.3,' secs, ISRCH=',F5.3,
     +   ' secs, ITLNK1=',F5.3,' secs, ITREE=',F5.3,' secs',
     +   ' and IFIND1=',F5.3,' secs',
     +  /'    (the latter including IFITP=',F5.3,' secs))',
     +  /' Average CPU time per event used by ITRECF = ',F5.3,' secs',
     +  /' Average CPU time per event used by ITCTRK = ',F5.3,' secs',
     +  /'    (including ITLNK2=',F5.3,'secs, ITREE=',F5.3,
     +   ' secs and IFIND2=',F5.3,' secs (the latter including',
     +   ' IFITP=',F5.3,' secs))',
     +  /' Average CPU time per event used by IMATCH = ',F5.3,' secs',
     +  /' Average CPU time per event used by ITRMCA = ',F5.3,' secs')
C
C-----------------------------------------------------------------------
  999 CONTINUE
      WRITE(LDEBRL,9000)
 9000 FORMAT('0',78('-'))
C
      END
#endif
