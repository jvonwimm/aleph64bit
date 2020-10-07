      SUBROUTINE BOSTA
#include "boscom.h"
#include "param.h"
#include "mparam.h"
      CHARACTER*4 CHAINT
      LOGICAL START
      DATA START/.TRUE./
#include "date.h"
      IF(START) WRITE(NPR,1006) IDATE,MACHIN
      START=.FALSE.
      CALL TIMEL(TLEFTC)
      SEC=TLEFT-TLEFTC
      MIN=SEC/60.0
      SEC=SEC-FLOAT(MIN)*60.0
      WRITE(NPR,1001) MIN,SEC,TLEFTC
      WRITE(NPR,1002)
      IMAX=1
      DO 10 J=1,NARR
      I=IARR(J)-IARR(1)
      IMAX=MAX0(IMAX,IW(I+LCH))
      MNAM=IW(I+IGP)-IW(I+INM)
      MGAP=IW(I+IWK)-IW(I+IGP)
      MWRK=IW(I+NJW)-IW(I+IWK)+1
      WRITE(NPR,1003) J,CHAINT(IW(I+IAR+1)),CHAINT(IW(I+IAR+2)),
     1   IW(I+NJW),NSYST,NAMES,MNAM,IW(I+NDN),
     2   MGAP,MWRK,IW(I+NDW)
   10 CONTINUE
      IF(IMAX.EQ.1) GOTO 100

*

      WRITE(NPR,1004)
      DO 20 J=1,NARR
      I=IARR(J)-IARR(1)
      WRITE(NPR,1005) J,CHAINT(IW(I+IAR+1)),CHAINT(IW(I+IAR+2)),
     1   (IW(I+IRC+K),K=1,6),IW(I+NGN),IW(I+NGW),
     2   (IW(I+NFH+K),K=1,10)
   20 CONTINUE

      WRITE(NPR,1007)
     +'Return code (1) = insufficient space to create new bank'
      WRITE(NPR,1007)
     +'            (2) = insuff. space to increase bank length'
      WRITE(NPR,1007)
     +'            (3) = named bank not found (BKTOW, BKFRW)'
      WRITE(NPR,1007)
     +'            (4) = read error'
      WRITE(NPR,1007)
     +'            (5) = formal error in record structure'
      WRITE(NPR,1007)
     +'            (6) = insuff. space for record input'
      WRITE(NPR,1007) ' '
      WRITE(NPR,1007) 'The histogram (in form of counts) shows usage ',
     +' of the BOS commons in units of 10 percent.'
      WRITE(NPR,1007) 'Interesting is especially the last nonzero ',
     +'number. If for example the last nonzero number is before the 70,'
      WRITE(NPR,1007) 'then the program never used more than 70 percent'
     +,' of the total BOS common.'

C
      CALL BOSIO
      CALL BOSRPT
  100 RETURN
 1001 FORMAT('0BOS++BOSTA+  Status of BOS',I4,' min',F7.3,' sec after',
     1   ' initialization  (',F7.1,' sec left)')
 1002 FORMAT('0     BOS-ARRAY  LENGTH  NUMBER OF WORDS IN AREAS...'/
     1   25X,'SYSTEM   INDEX NAMED-B   (DROP)    GAP  WORK-B   (DROP)')
 1003 FORMAT(1X,I3,3X,2A4,8I8)
 1004 FORMAT('0     BOS-array      return code statistic...',22X,
     1   'garb.collect.',3X,
     2   'histogram  of  used  space  in  10 percent bins'/,21X,
     3   '(1)',5X,'(2)',5X,'(3)',5X,'(4)',5X,'(5)',5X,'(6)',6X,'N',7X,
     4   'W',4X,'10   20   30   40   50   60   70   80   90  100')
 1005 FORMAT(1X,I3,3X,2A4,8I8,10(1X,I4))
 1006 FORMAT('0+++++++++++  B O S      Version',I10,' / ',A6/)
 1007 FORMAT(1X,A)
      END
