      SUBROUTINE CPADWR (IERR)
C==================================================
C
C!    Create & fill PRPW banks
C-
C-     Author  : M.N. Minard                 26/08/89
C-     Relation Between PECO  & Module #
C
C      Banks Created PRPW
C      Banks used PECO , PEST , ETDI , PEWI
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "etdijj.h"
#include "pecojj.h"
#include "pestjj.h"
#include "prpwjj.h"
      DIMENSION EFIL (3,36),IFIL(36)
      DIMENSION IPOS ( 36)
#include "bmacro.h"
C
C-    Loop on PECO
C
      IERR = 0
      NAPECO = NAMIND ('PECO')
      KPECO = IW (NAPECO)
      IF ( KPECO .LE. 0) GO TO 999
      NPECO = LROWS (KPECO)
C
C-    Lift relation banks wires - PECO
C
      LEN = LMHLEN + LPRPWA*NPECO*2
      CALL AUBOS ( 'PRPW',0,LEN,KPRPW,IGARB)
      IF ( IGARB.EQ.2 ) GO TO 997
      CALL BLIST (IW,'E+','PRPW')
      IW ( KPRPW + LMHCOL ) = LPRPWA
      IW ( KPRPW + LMHROW ) = 0
      NLMAX = NPECO*2
      IF ( IGARB.EQ.1) THEN
        KPECO = IW (NAPECO)
      ENDIF
C
C-    Initialise pointers
C
      NAPEST = NAMIND ('PEST')
      KPEST = IW (NAPEST)
      IF (KPEST .EQ.0) GO TO 999
      NPEST = LROWS(KPEST)
      NAETDI = NAMIND ('ETDI')
      KETDI = IW ( NAETDI)
C
C-    Fill from PEWI bank module position
C
      CALL VZERO (IPOS,36)
      NAPEWI = NAMIND ('PEWI')
      NPEWI = 0
      KPEWI = IW ( NAPEWI)
      IF ( KPEWI.EQ.0) KPEWI = IW(NAMIND('PWEI'))
      IF ( KPEWI.GT.0) NPEWI = LROWS ( KPEWI)
      DO 20 IPEWI = 1,NPEWI
      IMOD = ITABL ( KPEWI,IPEWI,1)
      IF ( IMOD.LT.1.OR.IMOD.GT.36) GO TO 998
      IPOS (IMOD) = IPEWI
 20   CONTINUE
C
C-    Loop on PECO
C
      DO 100 IPECO = 1,NPECO
      ECSUM = 0.
C
C-    Skip LCAL PECO
C
      IF ( ITABL (KPECO,IPECO,JPECKD).EQ.192) GO TO 100
C
C-    Extract info from PECO bank
C
      CALL VZERO (EFIL,36*3)
      CALL VZERO (IFIL,36)
C
C-    Search for PEST elements belonging to this cluster
C
      DO 50 IPEST = 1,NPEST
      IF ( ITABL(KPEST,IPEST,JPESED).NE.0 ) GO TO 50
      IF ( ITABL(KPEST,IPEST,JPESPE).NE.IPECO ) GO TO 50
      IK = ITABL ( KPEST,IPEST,JPESKS)
      IETDI = ITABL (KPEST ,IPEST,JPESET)
      ITOW = ITABL (KETDI,IETDI,JETDTL)
      ITETA = IBITS ( ITOW , 16 , 8)
      IPHI  = IBITS ( ITOW , 2 , 9 )
      CALL ECSCMD ( IPHI,ITETA,IMOD,ISC)
      IGMOD = (ISC-1)*12+IMOD
      IFIL ( IGMOD ) = IFIL (IGMOD) +1
      ESDT = FLOAT ( ITABL(KETDI,IETDI,IK+1))/1000000.
      EFIL ( IK,IGMOD) = EFIL ( IK,IGMOD) + ESDT
      ECSUM = ECSUM + ESDT
 50   CONTINUE
C
C-    Now store fraction
C
      DO 60 IL =1,36
      IF ( IFIL(IL).EQ.0) GO TO 60
C
C-    Look if bank should be enlarged
C
      NRMOD = LROWS(KPRPW)
      IF ( NRMOD.LE.NLMAX ) THEN
         LEN = LMHLEN + NRMOD*LPRPWA + NPECO*LPRPWA
         CALL AUBOS ( 'PRPW',0,LEN,KPRPW,IGARB)
         IF ( IGARB.EQ.2 ) GO TO 997
         NLMAX = NLMAX + NPECO
C
C-     In case garbage collection update pointers
C
        IF ( IGARB.EQ.1) THEN
           KPECO = IW (NAPECO)
           KPEST = IW (NAPEST)
           KETDI = IW (NAETDI)
        ENDIF
      ENDIF
C
C-      Fill PRPW bank
C
      NPRPW = LROWS ( KPRPW )
      JPRPW = KROW ( KPRPW , NPRPW+1)
      IW ( JPRPW + JPRPPN ) = IPECO
      IW ( JPRPW + JPRPMR ) = IPOS (IL)
      IW ( JPRPW + JPRPMN ) = IL
      ERAW = EFIL ( 1,IL)+EFIL(2,IL)+EFIL(3,IL)
      DO 70 IKL = 1,2
      RW ( JPRPW + JPRPMN + IKL ) = EFIL (IKL,IL)/ERAW
 70   CONTINUE
      RW ( JPRPW + JPRPER ) = ERAW
      IW ( KPRPW + LMHROW ) = IW (KPRPW + LMHROW) + 1
 60   CONTINUE
 100  CONTINUE
C
C-    Compress PRPW  banks
C
      CALL AUBPRS ( 'PRPW' )
      GO TO 999
 997  CALL REPORT ( 'CPADWR' , 'AUBOS failure',1)
      IERR = 1
      GO TO 999
 998  CALL RERROR ( 'CPADWR',1, ' Ecal Wire Module out of range')
      IERR = 1
      GO TO 999
 999  CONTINUE
      RETURN
      END
#endif
