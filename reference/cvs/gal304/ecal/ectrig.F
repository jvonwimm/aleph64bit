*DK ectrig
      SUBROUTINE ECTRIG
C-----------------------------------------------------------------
C    G.DEBOUARD     20-JAN-86
C! EC trigger anal. signals
C
C  -Called from ECDIGI                                  this .HLB
C. - calls  ECRWRG                                      this .HLB
C.          RANNOR                                      CERNLIB
C-----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "jqcom.h"
#include "ecnamc.h"
#include "ecsize.h"
#include "ewtmco.h"
#include "ewtrjj.h"
#include "ettrjj.h"
#include "edpara.h"
      DIMENSION KTHEB(NTHSG)
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
#include "bmacro.h"
C ----------------------------------------------------------------------
C  ===  initialize theta boundaries in array KTHEB
C
      IF(FIRST) THEN
        CALL EDTRGI(KTHEB)
        FIRST = .FALSE.
      ENDIF
C ----------------------------------------------------------
C
C === first , compute tower trigger signals
C
      CALL ALBOS( 'ETTR', 0, LMHLEN+3*NTHSG*NPHSG, KETTR, IGARB)
      IW( KETTR+LMHCOL) = 3
      IW( KETTR+LMHROW) = NTHSG*NPHSG
      CALL BLIST (IW,'E+','ETTR')
      JETTR = KETTR + LMHLEN + 1
C
      DO 20 K=1,NTHSG
        DO 10 J=1,NPHSG
          DO 5 I=1,3
            CALL RANNOR(SIG,SIH)
            IW(JETTR) = NINT(ETTNOI(K,I) * SIG)
            JETTR     = JETTR + 1
    5     CONTINUE
   10   CONTINUE
   20 CONTINUE
C
      KETHT = IW(NAETHT)
      IF( KETHT .EQ. 0 ) GOTO 55
      NETHT = LROWS( KETHT )
      LETHT = LCOLS (KETHT)
      JETHT = KETHT + LMHLEN + 1
C
C
      DO 50 N=1,NETHT
        IETIQ = IW(JETHT)
        JPHI  = IBITS(IETIQ,2,9)
        KTHE  = IBITS(IETIQ,16,8)
C
C === computation of trigger region number NUMTR
C
        NUMTR = 0
        NUTHE = 1
   35   IF(KTHE.GT.KTHEB(NUTHE)) THEN
          NUTHE = NUTHE + 1
          GO TO 35
        ENDIF
        CALL ECRWRG(KTHE,NUREG,MAXPH)
        NUPHI = INT((NPHSG*(JPHI-1))/MAXPH) + 1
        NUMTR = (NUTHE-1)*NPHSG + NUPHI
        IF(NUMTR.LE.0.OR.NUMTR.GT.NPHSG*NTHSG) GO TO 50
        KPTR = KROW (KETTR,NUMTR)
        DO 40 I=1,3
          SIGNA = ETSTWE(I) * FLOAT(IW(JETHT+I))
          IW(KPTR+I) = IW(KPTR+I) + NINT(SIGNA)
   40   CONTINUE
        JETHT = JETHT + LETHT
   50 CONTINUE
C
C === then , compute wire trigger signals
C
   55 CONTINUE
      CALL ALBOS( 'EWTR', 0, LMHLEN+2*LMODU*LCOMP, JEWTR, IGARB)
      IW( JEWTR+LMHCOL) = 2
      IW( JEWTR+LMHROW) = LMODU*LCOMP
      CALL BLIST (IW,'E+','EWTR')
      KEWTR = JEWTR + LMHLEN
      DO 70 I=1,LMODU*LCOMP
         DO 71 J=2,LWPLA+1
            KCONS=1
C  KCONS simulates the electronic gain in stack 3
            IF(J.GE.35)         KCONS=2
            IF(MOD(J,2).EQ.0)   THEN
               IW(KEWTR+1)=IW(KEWTR+1)+IWIRES(J,I)*KCONS
            ELSE
               IW(KEWTR+2)=IW(KEWTR+2)+IWIRES(J,I)*KCONS
            ENDIF
  71     CONTINUE
  70  KEWTR=KEWTR+LEWTRA
C
 980  RETURN
      END
#endif
