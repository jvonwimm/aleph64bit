      SUBROUTINE RPRSTA(TIME)
C--------------------------------------------------------------------
C! Print statistics for this run
C!
C!  Author       J. Bunn
C!
C!  Modified     S. Orteu      25-FEB-1987
C!   Modified :- E. Lancon             10-APR-1991   Add JSUM Printout
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rparac.h"
#include "tparac.h"
#include "rlunit.h"
#include "rstate.h"
#include "rcurnt.h"
#include "jsumjj.h"
      CHARACTER*1 BAR(130)
      PARAMETER (MLEN=50)
      CHARACTER*(MLEN) TEVT(MESTA)
      CHARACTER*(MLEN) TEVER(MEERR)
      CHARACTER*(MLEN) TJSUM(LJSUMA)
#include "bmacro.h"
C--------------------------------------------------------------------
      DATA TEVT/
     $  'Total events processed in this run................',
     $  'Events written to POT.............................',
     $  'Average reconstruction time per event (msecs).....'/
      DATA TEVER/
     $  'Total events that failed for any reason...........',
     $  'Events with bad header............................',
     $  'Events absent on input file.......................',
     $  'Events rejected in <prepare data>.................',
     $  'Events rejected in <reconstruct event>............'/
      DATA BAR/130*'-'/
      DATA (TJSUM(I),I=1,10) /
     1  'Number of triggers................................',
     2  'Number of events with vertex......................',
     3  'Number of hadronic events.........................',
     4  'Number of Lumin coincidences......................',
     5  'Number of BhaBhas.................................',
     6  'Number of triggers, HVs on........................',
     7  'Number of events with vertex,HVs on...............',
     8  'Number of hadronic events,HVs on..................',
     9  'Number of Lumin coincidences,HVs on...............',
     .  'Number of BhaBhas,HVs on..........................'/
      DATA (TJSUM(I),I=11,20) /
     1  'Total time for reconstruction.....................',
     2  'Total time for hadr events........................',
     3  'Total time for Bhabhas............................',
     4  'Total length of input records [kb]................',
     5  'Total length of input for Z.......................',
     6  'Total length of POT records [kb]..................',
     7  'Length of POT for hadr. events [kb]...............',
     8  'Sum of vertex X positions.........................',
     9  'Sum of vertex Y positions.........................',
     .  'Sum of vertex Z positions.........................'/
      DATA (TJSUM(I),I=21,30) /
     1  'Sum of vertex X positions squared.................',
     2  'Sum of vertex Y positions squared.................',
     3  'Sum of vertex Z positions squared.................',
     4  'Nb. of killed storeys -Birgitte...................',
     5  'Nb. of killed storeys - Wires.....................',
     6  'Num of t0s........................................',
     7  'Sum of t0s........................................',
     8  'Sum of t0s squared - for variance.................',
     9  'Group A, number of tracks used for beam position .',
     .  'Group A, sum cos phi..............................'/
      DATA (TJSUM(I),I=31,40) /
     1  'Group A, sum sin phi..............................',
     2  'Group A, sum cos(phi)*sin(phi)....................',
     3  'Group A, sum sin(phi)**2..........................',
     4  'Group A, sum cos(phi)**2..........................',
     5  'Group A, sum d0 * sin(phi)........................',
     6  'Group A, sum d0 * cos(phi)........................',
     7  'Group A, sum d0...................................',
     8  'Group A, sum d0**2................................',
     9  'Group B, number of tracks used for beam position..',
     .  'Group B, sum cos phi..............................'/
      DATA (TJSUM(I),I=41,48) /
     1  'Group B, sum sin phi..............................',
     2  'Group B, sum cos(phi)*sin(phi)....................',
     3  'Group B, sum sos(phi)**2..........................',
     4  'Group B, sum sin(phi)**2..........................',
     5  'Group B, sum d0 * sin(phi)........................',
     6  'Group B, sum d0 * cos(phi)........................',
     7  'Group B, sum d0...................................',
     8  'Group B, sum d0**2................................' /
C--------------------------------------------------------------------
      WRITE(LOUTRL,'(''1'',130A)') BAR
      WRITE(LOUTRL,'(30X,A)') 'J U L I A - R U N - S U M M A R Y '
      WRITE(LOUTRL,'(130A)') BAR
      WRITE(LOUTRL,'(A,I6,A,F12.5,A)')
     $  '0End of run number ',IRUNRC,' which took ',TIME,' CPU seconds'
      WRITE(LOUTRL,'(/,1X,A,/)') 'with statistics :'
      WRITE(LOUTRL,'(1X,A,I6)') (TEVT(I),NEVTRS(I),I=1,MESTA)
      WRITE(LOUTRL,'(/,1X,A,/)') 'and errors :'
      WRITE(LOUTRL,'(1X,A,I6)') (TEVER(I),NEVTRR(I),I=1,MEERR)
      WRITE(LOUTRL,'(1X,130A)') BAR
C
      NAJSUM = NAMIND('JSUM')
      KJSUM = IW(NAJSUM)
      IF ( KJSUM.GT.0 ) THEN
        WRITE (LOUTRL,'(/,130A,/)') BAR
        WRITE (LOUTRL,8000) (TJSUM(I),ITABL(KJSUM,1,I), I=1, 10)
        WRITE (LOUTRL,8001) (TJSUM(I),RTABL(KJSUM,1,I), I=11, 23)
        WRITE (LOUTRL,8000) (TJSUM(I),ITABL(KJSUM,1,I), I=24,26)
        WRITE (LOUTRL,8001) (TJSUM(I),RTABL(KJSUM,1,I), I=27,28)
        WRITE (LOUTRL,'(25X,A,/,25X,A)')
     &    'Group A = tracks with at least one VDET r-phi coordinate',
     &    'Group B = all other tracks'
        DO I =  29, 38
          WRITE (LOUTRL,8001)
     &      TJSUM(I),RTABL(KJSUM,1,I), TJSUM(I+10),RTABL(KJSUM,1,I+10)
        ENDDO

        WRITE (LOUTRL,'(/,130A,/)') BAR
      ELSE
      ENDIF
C
      CALL ALSUMCL (-1)
C--------------------------------------------------------------------
  999 CONTINUE
 8000 FORMAT( 2(2X,A,I12) )
 8001 FORMAT( 2(2X,A,G12.4) )
      END
#endif
