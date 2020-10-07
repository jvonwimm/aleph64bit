      SUBROUTINE HROAD( PMOM , VEXTR , ITRACK , NETP1 ,XCOS)
C========================================================================
C!    Calculates energy in road for HCAL
C!    Fills bank HCTE
C-
C-     Authors : M.N. Minard , M. Pepe       26/02/89
C-     Modified: M.N  Minard                 04/01/91
C-
C- Input:  PMOM              = Particle momentum
C-         VEXTR (1--> 3)    = Extrapolated coordinates in HCAL
C-         ITRACK ( 1--> 5 ) = Address of cell crossed by extrapolation
C-         NETP1             = Track # to extrapolate
C?
C!======================================================================
#ifndef DOC
#include "hsdajj.h"
#include "hctejj.h"
#include "ecnmsk.h"
#include "emskjj.h"
#include "prtmjj.h"
#include "emskpa.h"
#include "alcons.h"
      PARAMETER (NWHCTE = 2 )
#include "bcs.h"
      DIMENSION  VEXTR(3,2) , ITRACK (5,2),XCOS(3,2)
      DIMENSION DVECT(3)
      DIMENSION  COSI(3) , DX(3) , DEX(3) , XPOIN(3,8) , ER(7)
      DIMENSION  XV(4) , DCOSI(3)
      DIMENSION XOR(3),XFAR(3)
      DATA NAEMSK /0/
#include "bmacro.h"
C
C-    Determine pointers
C
      IF (NAEMSK.EQ.0) THEN
        NAEMSK = NAMIND('EMSK')
        NAPRTM = NAMIND('PRTM')
        NAHCTE = NAMIND('HCTE')
      ENDIF
      KPRTM  = IW (NAPRTM)
      IF ( KPRTM.EQ.0) THEN
        CALL RERROR('HROAD',1,'No Mask-Track relation ')
        GO TO 990
      ENDIF
      KEMSK = IW(NAEMSK)
      JPRTM = KROW(KPRTM,NETP1)
      IF ( IW(JPRTM+1).EQ.0) GO TO 990
      JEMSK = KROW ( KEMSK, IW(JPRTM+1))
      KHCTE = IW(NAHCTE)
      IF ( KHCTE.EQ.0 ) THEN
        CALL RERROR('HROAD',2,'Problem no HCTE bank exists')
        GO TO 990
      ENDIF
C
C     Temporary bank exists loop on HSDA
C
      KHSDA = IW (NAHSDA)
      NHSDA = LROWS(KHSDA)
      IF ( NHSDA.LT.1) GO TO 900
C
      DO 250 IHSDA = 1,NHSDA
        CALL VZERO (ER,3)
        JHSDA = KROW ( KHSDA,IHSDA )
        IK = IW ( JHSDA+JHSDSN)
        IF ( IK.LT.1.OR.IK.GT.2) IK=1
        IPHI = IW ( JHSDA+JHSDPI )
        ITET = IW ( JHSDA+JHSDTI )
        EN = RW ( JHSDA + JHSDDE )
        ITOW = (ITET-1) * 256 + (IPHI-1) * 2 + IK - 1
C
        IF ( ITRACK ( 1, IK) . EQ. 0 ) GO TO 250
C
C-    Calculate cell center coordinate
C
        LHPOI = 8
        CALL HSRCR ( 'ALEPH',ITET , IPHI, IK , LHPOI , XPOIN )
C
C-     Average for center
C
        CALL VZERO (COSI,3)
        XDMAX = 0.
        DO 141 IJ = 1,LHPOI
          DO 140 IFI= 1,3
            IF ( IJ.EQ.1) THEN
              XOR(IFI) = XPOIN(IFI,IJ)
            ELSE
              XFAR(IFI) = XPOIN(IFI,IJ)
            ENDIF
            COSI (IFI) = COSI (IFI) + XPOIN(IFI,IJ)/FLOAT(LHPOI)
  140     CONTINUE
          IF ( IJ.NE.1) THEN
            DR = SQRT((XOR(1)-XFAR(1))**2+(XOR(2)-XFAR(2))**2+(XOR(3)-
     &          XFAR(3)
     &)**2)
            IF ( DR.GT.XDMAX) XDMAX = DR
          ENDIF
  141   CONTINUE
C
C-    Calculate distance to extrapolated point
C
        DR = 0.
        DO 100 IX =1,3
          DR = DR + COSI(IX)**2
  100   CONTINUE
        CANG2 = VDOTN(VEXTR,COSI,3)
      IF (ABS(CANG2).GT.1.) CANG2 = SIGN(1.,CANG2)
        TANG2 = ACOS(CANG2)
        RANG2 = SIN(TANG2)*SQRT(DR)
        XVAR =ABS( RANG2)/XNORM(IK+3)
        DVECT(1) =-VEXTR(1,1)+COSI(1)
        DVECT(2) =-VEXTR(2,1)+COSI(2)
        DVECT(3) =-VEXTR(3,1)+COSI(3)
        DIST = SQRT((VEXTR(1,1)-COSI(1))**2+(VEXTR(2,1)-COSI(2))**2+
     & (VEXTR(3,1)-COSI(3))**2)
        DANG1= VDOTN(XCOS,DVECT,3)
        DANG = VDOTN(COSI,XCOS,3)
        XVAR2= DIST*SQRT(1.-DANG1**2)

        IF ( XVAR2.GT.0.5*XDMAX.OR.DANG.LT.0) GO TO 250
C
C-    Determine region
C
        IS = 1
        IF (XVAR.LT.0.5*ZON(IK+3)) IS = 3
        IF (XVAR.GT.ZON(IK+3))     IS = 2
        IF ( CANG2.LT.0.)          IS = 0
        IF ( XVAR2.GT.0.5*XDMAX)   IS = 0
        IF (IS.EQ.0 ) GO TO 250
        IF(IS.EQ.1.OR.IS.EQ.2)EMAX  = PMOM *EZON(IK+3,IS)
        IF(IS.EQ.3) EMAX = PMOM
        IF ( IS.EQ.3) IS =1
C
C-    Check if cell not already removed
C
        EPREV = 0.
        JHCTE = 0
        DO 200 IHCTE = 1,LROWS(KHCTE)
          JHTCA = KROW(KHCTE,IHCTE)
          IF ( IW(JHTCA+1).EQ.ITOW) THEN
            JHCTE = JHTCA
            EPREV = RW(JHCTE+2)
          ENDIF
  200   CONTINUE
        IF(JHCTE.EQ.0) THEN
          IF (LFRROW(KHCTE).LT.1) THEN
            CALL RERROR('HROAD',4,'HCTE too small,contact experts')
            CALL AUBOS ('HCTE',0,IW(KHCTE)+LHCTEA,KHCTE,IGARB)
            IF (KHCTE.EQ.0) THEN
              CALL RERROR ('HROAD',5,'not enough space in BOS array')
              GOTO 990
            ENDIF
          ENDIF
          JHCTE = KNEXT(KHCTE)
          IW ( JHCTE + 1 ) = ITOW
          RW ( JHCTE + 2 ) = 0.
          IW ( KHCTE + 2 ) = IW(KHCTE+2) + 1
        ENDIF
C
C-    Now compare to energy already removed
C
        IF ( EMAX .GT.EN-EPREV) THEN
          ER(IS) = EN-EPREV
          IF ( ER(IS).LE.0) ER(IS)=0
        ELSE
          ER(IS) = EMAX
        ENDIF
        IF (ER(IS).GT.0.01) THEN
          IW ( JHCTE+1) = ITOW
          RW (JHCTE+2)  = ER(IS) + EPREV
C
C-    Store energy found
C
          INCL = (IS-1) * 15 + ( IK+2)*3
C
C-    Set vector to calculate average sums
C-    Fill and update EMSK bank with new energy found in road
C
          CALL HCOS ( ER(IS),IPHI,ITET,IK,XV )
          CALL UCOPY(RW(JEMSK+INCL+1),DCOSI(1),3)
          CALL EMSKF(DCOSI,XV)
          CALL UCOPY(DCOSI(1),RW(JEMSK+INCL+1),3)
        ENDIF
  250 CONTINUE
C
C-    End of loop on storeys
C
      RETURN
C
  900 CALL RERROR('HROAD',3,'No HSDA bank to treat')
C
  990 RETURN
      END
#endif
