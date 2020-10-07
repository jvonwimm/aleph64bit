      SUBROUTINE RPRSUM
C--------------------------------------------------------------------
C! Write out summary information
C!
C!  Author       J. Bunn
C!
C!  Modified     S. Orteu      25-FEB-1987
C!  Modified     J.Knobloch    15-Dec-1988
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "ravtim.h"
#include "revent.h"
#include "rlunit.h"
#include "rstatr.h"
      PARAMETER (MLEN=50)
      CHARACTER*(MLEN) TRUN(MRSTA)
      CHARACTER*(MLEN)  TRUNE(MRERR)
      CHARACTER FNAME*80,FTYPE*8,FDEVI*8
      DATA TRUN /
     $  'Total runs processed in this job..................',
     $  'Runs written to POT...............................',
     $  'Runs summarised in database.......................',
     $  'Total events processed in this job................',
     $  'Events written to POT.............................',
     $  'Average reconstruction time per event (msecs).....',
     $  'Average event size (KBytes).......................'/
      DATA TRUNE/
     $  'Runs that failed for any reason...................',
     $  'Runs not processed due to absence of constants....',
     $  'Runs not processed due to bad constants...........',
     $  'Runs not in log book..............................',
     $  'Runs not on given tape............................',
     $  'Total events with bad header this job.............',
     $  'Total events absent on input this job.............',
     $  'Total events rejected in <prepare data>...........',
     $  'Total events rejected in <reconstruct event>......'/
C--------------------------------------------------------------------
      IF (NRUNRS(4).GT.0) THEN
        NRUNRS(6) = INT(1000.*TIMJRA) / NRUNRS(4)
        NRUNRS(7) = INT(TKBYRE) / NRUNRS(4)
      ENDIF
C Print summary
      WRITE(LOUTRL,500)
  500 FORMAT('1',80('!'),/,1X,'            ',
     +'R E C O N S T R U C T I O N    T E R M I N A T I N G',
     +/,1X,80('!'), //,1X,'Statistics ....',/)
      WRITE(LOUTRL,'(1X,A,I6)') (TRUN(I),NRUNRS(I),I=1,MRSTA)
      WRITE(LOUTRL,'(/,1X,A,/)')  'Errors ....'
      WRITE(LOUTRL,'(1X,A,I6)') (TRUNE(I),NRUNRR(I),I=1,MRERR)
C--------------------------------------------------------------------
C   print summary of non-fatal errors
C
      CALL RPERRS
C
C     print MODULE summary
      CALL RNXMOD(999,RMOD)
C------------------------------------------
C Put histograms to file
      KHIST = IW (NAMIND('HIST'))
      IF (KHIST.NE.0) THEN
        CALL ACDARG('HIST','NATIVE','A',FNAME,FTYPE,FDEVI,IER)
        LHISRL=15
        CALL HROPEN(LHISRL,'JULHIST',FNAME,'N',1024,IRET)
        CALL HROUT(0,ICYCLE,' ')
        CALL HREND('JULHIST')
      ENDIF
C Print histograms
      IF(IW(NAMIND('HPRI')).NE.0)THEN
        CALL HISTDO
      ENDIF
      END
#endif
