       SUBROUTINE TISLND(ISECT,KTPAD,KTISL,NCTOT,JGARB,IER)
C
C--------------------------------------------------------------------
C! Find TPC pad islands
C!
C!    Author:    R. Johnson    25-11-86
C!    Modified:  R. Johnson     8-04-88
C!    Modified:  H. Meinhard   18-Jul-1991
C!                      Zero TCPL bank before it is used
C!
C!    Input:
C!       - ISECT     /I     Sector number to be analyzed
C!       - KTPAD     /I     BOS pointer to the 'TPAD' bank for ISECT
C!                          This should never be zero!
C!       - KTISL     /I     BOS pointer to the 'TISL' bank for ISECT
C!                          Note: KTISL should be zero if the bank
C!                                'TISL' doesn't yet exist for ISECT
C!    Output:
C!       - KTPAD     /I     BOS pointer to the 'TPAD' bank
C!       - KTISL     /I     BOS pointer to the 'TISL' bank for ISECT
C!       - NCTOT     /I     Total number of islands (clusters)
C!       - JGARB     /I     Set to 1 if garbage collection done.
C!                          Otherwise it is set to zero.
C!       - IER       /I     Set to 1 if BOS runs out of space.
C!                          Otherwise it is set to zero.
C!
C!    Called by TPADS
C!
C!    Description
C!    ===========
C!    This routine finds the pad islands and fills TISL if that has
C!    not already been done by the TPP online.  In some cases the TPP
C!    may time out before finishing the islands.  In that case, this
C!    routine will retain what the TPP has finished and then complete
C!    the job for the remaining sector rows.
C!----------------------------------------------------------------------
#ifndef DOC
C
C--------------------- COMMON blocks -----------------------------------
C
#include "rparac.h"
#include "tparac.h"
#include "tpgpar.h"
#include "rlunit.h"
#include "bcs.h"
#include "tpgeom.h"
C
C--------------------- Local variables ---------------------------------
C
      LOGICAL LFILL,LFRST
      DATA MXHIT/LTHIDI/,LFRST/.TRUE./,ICLDR/0/
C
C-----------------------------------------------------------------------
C
      IF (LFRST) THEN
        KTCPL=0
        NTCPL=NAMIND('TCPL')
        LFRST=.FALSE.
      ENDIF
      JGARB=0
      NCTOT=0
      ISTYP=ITPTYP(ISECT)
C
C++   Has the TPP already found the islands?  If no TISL bank
C++   exists, then we must create it here.  If it does exist,
C++   we still must check that the TPP finished all rows.
C
      IF (KTISL.NE.0) THEN
        IF (IW(KTISL-2).NE.ISECT) THEN
          KTISL=NLINK('TISL',ISECT)
        ENDIF
      ENDIF
      IF (KTISL.EQ.0) THEN
        LEN=2*IW(KTPAD)
        IW(1)=1
        CALL AUBOS('TISL',ISECT,LEN,KTISL,IGARB)
        IF (IGARB.EQ.2) GO TO 998
        CALL BLIST(IW,'T+','TISL')
        IF (IGARB.NE.0) THEN
          KTPAD=NLINK('TPAD',ISECT)
          JGARB=1
        ENDIF
        LFILL=.TRUE.
      ELSE
        LFILL=.FALSE.
      ENDIF
C
C++   Loop over the rows with data
C
      IOFRB=2
      IPISL=0
  320 IF (IOFRB.GT.IW(KTPAD)) GO TO 400
        IROW=IBITS(IW(KTPAD+IOFRB-1),0,4)
        NHITR=IW(KTPAD+IOFRB)
C
C++     Has the TPP finished cluster finding for this row?
C
        IPISL=IPISL+1
        IF (.NOT.LFILL) THEN
          IRISL=IBITS(IW(KTISL+IPISL),16,16)
          IF (IRISL.NE.IROW) THEN
            CALL RERROR('TISLND',3,'TISL bank is out of sequence')
            IER=2000+IROW
            GO TO 999
          ENDIF
          NCLUS=IBITS(IW(KTISL+IPISL),0,16)
          IF (NCLUS.EQ.65535) THEN
            LFILL=.TRUE.
C
C++         Here the TPP timed out, so we must finish filling TISL.
C++         First, we must extend the TISL bank.
C
            LEN=2*IW(KTPAD)
            IW(1)=1
            CALL AUBOS('TISL',ISECT,LEN,KTISL,IGARB)
            IF (IGARB.EQ.2) GO TO 998
            IF (IGARB.NE.0) THEN
              KTPAD=NLINK('TPAD',ISECT)
              JGARB=1
            ENDIF
          ELSE
            NCTOT=NCTOT+NCLUS
            IPISL=IPISL+1
            IPISL=IPISL+IW(KTISL+IPISL)
          ENDIF
        ENDIF
C
C++     If TISL is not filled for this row, then we fill it here.
C
        IF (LFILL) THEN
C
C++       Open the TCPL bank to provide working space for TPADPR and
C++       for the cluster pulse lists.
C
          KTCPL=IW(NTCPL)
          IF (KTCPL.EQ.0 .OR. MXHIT.LT.NHITR) THEN
            MXHIT=MAX(NHITR,MXHIT)
            CALL AUBOS('TCPL',0,6*MXHIT,KTCPL,IGARB)
            IF (IGARB.EQ.2) GO TO 998
            IF (IGARB.NE.0) THEN
              KTPAD=NLINK('TPAD',ISECT)
              KTISL=NLINK('TISL',ISECT)
              JGARB=1
            ENDIF
            CALL VZERO(IW(KTCPL+1),IW(KTCPL))
C
C           Description of TCPL bank contents:
C                   1-->  MXHIT : Pointer to pulse list for each cluster
C             MXHIT+1-->2*MXHIT : Ordering flag for each cluster
C           2*MXHIT+1-->6*MXHIT : Internal work space for TPADPR
C
          ENDIF
C
C++       Call routine for cluster pattern recognition.
C
          CALL TPADPR(IW(KTPAD+IOFRB+1),NHITR,ICLDR,IW(KTCPL+1),
     &                  IW(KTCPL+2*MXHIT+1),IW(KTCPL+4*MXHIT+1),
     &                  IW(KTCPL+MXHIT+1),NCLUS)
          NCTOT=NCTOT+NCLUS
C
C++       Fill the row header into TISL.
C
          IW(KTISL+IPISL) = 0
          CALL MVBITS(IROW,0,16,IW(KTISL+IPISL),16)
          CALL MVBITS(NCLUS,0,16,IW(KTISL+IPISL),0)
          LEN=NCLUS+NHITR
          IPISL=IPISL+1
          IW(KTISL+IPISL)=LEN
C
C++       Call routine to fill the TISL bank for this row
C
          CALL TISFIL(IW(KTISL+IPISL+1),LEN,IOFRB,
     &                ICLDR,IW(KTCPL+1),IW(KTCPL+MXHIT+1),NCLUS)
          IPISL=IPISL+LEN
        ENDIF
C
C++     End of the row loop.  Increment TPAD offset for the next row.
C
        IOFRB=IOFRB+NHITR+2
        GO TO 320
  400 CONTINUE
C
C++   Fix the length of the TISL bank
C
      IF (LFILL) THEN
        CALL AUBOS('TISL',ISECT,IPISL,KTISL,IGARB)
        IF (IGARB.NE.0) JGARB=1
      ENDIF
      GO TO 999
C
  998 CONTINUE
      CALL RERROR('TISLND',1,'No space for named banks')
      IER=1
      JGARB=2
  999 CONTINUE
C
C++   Drop the TCPL bank
C
      KTCPL = NDROP('TCPL',0)
C
      RETURN
      END
#endif
