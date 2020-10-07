      SUBROUTINE SRSELK(IER)
C----------------------------------------------------------------------
C! Select track candidates
C!
C!    Author:     H. Meinhard       04-Feb-1988
C!    Modified:   H. Meinhard       07-Mar-1989 (2)
C!
C!    Output:     - IER       /I    error flag (0 = no error)
C!
C!    Description
C!    ===========
C!    Among track candidates being derived from the same patch, select
C!    the best one only (for the time being, 'best' means the candidate
C!    with most wire parallels and, if there are several, the one with
C!    the best chisquare, but this may be modified later on)
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "sanamc.h"
#include "skanjj.h"
#include "skcpjj.h"
#include "sskpjj.h"
#include "swpajj.h"
      INTEGER ISTRT(2)
#include "bmacro.h"
C----------------------------------------------------------------------
C link to wire parallele samples, track candidates
      KSWPA = IW(NASWPA)
      KSKAN = IW(NASKAN)
      KSKCP = IW(NASKCP)
      KSSKP = IW(NASSKP)
      ISTRT(1) = 1
      ISTRT(2) = ITABL(KSSKP,2,JSSKPB)
C
C NTOT counts the remaining track candidates
      NTOT = 0
C
C top of loop over sides
      DO 350 ISIDE = 1, 2
        IW(KROW(KSSKP,ISIDE)+JSSKPB) = NTOT+1
C
C top of outer loop over track candidates
        DO 310 IKA1 = ISTRT(ISIDE), ITABL(KSSKP,ISIDE,JSSKPE) - 1
          IWPA1 = ITABL(KSKCP,IKA1,JSKCKM)
          IPAT1 = ITABL(KSWPA,IWPA1,JSWPMP)
          NPAR1 = ITABL(KSKAN,IKA1,JSKANP)
          CHI21 = RTABL(KSKAN,IKA1,JSKAC2)
C
C top of inner loop over track candidates
          DO 300 IKA2 = IKA1+1, ITABL(KSSKP,ISIDE,JSSKPE)
            IWPA2 = ITABL(KSKCP,IKA2,JSKCKM)
            IPAT2 = ITABL(KSWPA,IWPA2,JSWPMP)
            IF (IPAT2 .EQ. IPAT1) THEN
              NPAR2 = ITABL(KSKAN,IKA2,JSKANP)
              CHI22 = RTABL(KSKAN,IKA2,JSKAC2)
              IF (NPAR1 .GT. NPAR2 .OR.
     +            (NPAR1 .EQ. NPAR2 .AND. CHI21 .LT. CHI22)) THEN
C mark second track candidate for deletion by putting the side to 0
                IW(KROW(KSKAN,IKA2)+JSKASI) = 0
              ELSE
C mark first track candidate for deletion by putting the side to 0
                IW(KROW(KSKAN,IKA1)+JSKASI) = 0
                GOTO 310
              ENDIF
            ENDIF
C
C bottom of loops over track candidates
  300     CONTINUE
  310   CONTINUE
C
C loop over all entries in work bank, copy bank into itself,
C skipping the entries marked for deletion
        DO 340 IKAN = ISTRT(ISIDE), ITABL(KSSKP,ISIDE,JSSKPE)
          JSIDE = ITABL(KSKAN,IKAN,JSKASI)
          IF (JSIDE .NE. 0) THEN
            NTOT = NTOT+1
            DO 320 IWRD1 = 1, LSKANA
  320       IW(KROW(KSKAN,NTOT)+IWRD1) = ITABL(KSKAN,IKAN,IWRD1)
            DO 330 IWRD2 = 1, LSKCPA
  330       IW(KROW(KSKCP,NTOT)+IWRD2) = ITABL(KSKCP,IKAN,IWRD2)
          ENDIF
  340   CONTINUE
C
C bottom of loop over sides
        IW(KROW(KSSKP,ISIDE)+JSSKPE) = NTOT
  350 CONTINUE
C
C compress SKAN, SKCP banks to actual size
      IW(KSKAN+LMHROW) = NTOT
      IW(KSKCP+LMHROW) = NTOT
      CALL AUBPRS('SKANSKCP')
C
      GOTO 999
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
#endif
