      SUBROUTINE EFERRR (NAUTO)
C----------------------------------------------------------------------
C!  - Filter ERRR bank from on-line
C
C     Bank input ERRR
C     Bank output ERRF
C                        AUTHOR M.N.MINARD 14-3-90
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rcurnt.h"
      INTEGER IRUN94
      PARAMETER ( IRUN94 = 25000 )
      EXTERNAL NAMIND
      INTEGER NAUTO(36)
      EXTERNAL MODROC
      INTEGER MODROC
      PARAMETER (LERRFA=11)
#include "bmacro.h"
C
C-    Look for ERRR bank
C
      KERRF = NDROP('ERRF',0)
      NAERRR = NAMIND ('ERRR')
      KERRR = IW ( NAERRR)
      NERRR = 0
      IF ( KERRR.NE.0) NERRR = LROWS (KERRR)
C
C-    Built ERRF bank
C
      IF ( NERRR.EQ.0 ) GO TO 900
      NLENG = NERRR*LERRFA + LMHLEN
      CALL AUBOS ('ERRF', 0, NLENG , KERRF , IGARB )
      IF ( IGARB .EQ.2 ) GO TO 990
      CALL BLIST (IW,'E+','ERRF')
      IW(KERRF+LMHCOL) = LERRFA
      IW(KERRF+LMHROW) = 0
      IF ( IGARB.NE.0 ) THEN
           KERRR = IW(NAERRR)
      ENDIF
C
C-    Check ERRR mini - header
C
      LCER= LCOLS(KERRR)
      IF(LCER.LT.1.OR.LCER.GT.99999) GOTO 960
      LRER= LROWS(KERRR)
      IF(LRER.LT.1.OR.LRER.GT.99999) GOTO 960
      IF ( LCER*LRER+LMHLEN.NE.IW(KERRR) ) GO TO 960
C
C-    Loop on ERRR to fill ERRF
C
      IERRF = 0
      NFATAL = 0
      DO 10 IERRR = 1 , NERRR
C
C-    Skip pedestal information
C
      IF(ITABL(KERRR,IERRR,1) .EQ.100) THEN
          IF ( IRUNRC .GT. IRUN94 ) THEN
           CALL RERROR('EFERRR',5,'invalid TY parameter in bank ERRR')
          END IF
          IROC = ITABL(KERRR,IERRR,2)
          IADC = ITABL(KERRR,IERRR,3)
          IPLUS1 = ITABL(KERRR,IERRR,4)
          IPLUS2 = ITABL(KERRR,IERRR,5)
          IPLUS3 = ITABL(KERRR,IERRR,6)
          IMINS1 = ITABL(KERRR,IERRR,7)
          IMINS2 = ITABL(KERRR,IERRR,8)
          IMINS3 = ITABL(KERRR,IERRR,9)
          IMOD = MODROC ( IROC , IADC )
          IF (IMOD .GT. 0.AND.IMOD.LT.37) THEN
              NAUTO (IMOD ) = IPLUS1 + IPLUS2 + IPLUS3
     *                      + IMINS1 + IMINS2 + IMINS3
          END IF
      END IF
      IF(ITABL(KERRR,IERRR,1) .GE.100) GO TO 10
      IF (ITABL(KERRR,IERRR,1).EQ.-8) NFATAL = NFATAL+1
      JERRR = KROW(KERRR,IERRR)
      IERRF = IERRF +1
      JERRF = KROW (KERRF,IERRF)
      CALL UCOPY (IW(JERRR+1), IW(JERRF+1),LERRFA)
 10   CONTINUE
      IW ( KERRF+LMHROW ) = IERRF
      CALL AUBPRS('ERRF')
      IF (NFATAL.NE.0) THEN
          CALL RERROR('EFERRR',-2,' at least one Ecal Roc missing')
      ENDIF
      GO TO 900
  960 WRITE(IW(6),100) IW(KERRR),LCOLS(KERRR),LROWS(KERRR)
  100 FORMAT (' ++++ EFERRR inconsistent bank ERRR length =',I5
     + ,' #col',I4, '# Rows ',I5 )
      CALL RERROR('EFERRR',-2,' Wrong ERRR bank ')
      GO TO 900
  990 CALL RERROR('EFERRR',-1,' no space for bank ERRF')
      GO TO 900
 900  CONTINUE
      RETURN
      END
#endif
