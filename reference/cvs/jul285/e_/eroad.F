      SUBROUTINE EROAD ( NESDA , PMOM , VEXTR , ITRACK , ER )
C=======================================================================
C!    Calculates energy inside road in Ecal
C!    Fills temporary working bank ECTE
C-
C-     Authors : M.N. Minard , M. Pepe       26/02/89
C-
C- Input :  NESDA          = Relative address for storey in ESDA bank
C-          PMOM           = Track momemtum measured by TPC
C-          VEXTR (1->3)   = Extrapolated point in stack
C-          ITRACK(1)      = Nb. of storeys crossed by track in stack
C-          ITRACK(2->5)   = Address of storey crossed by track
C- Output   ER (1->3)      = Energy vector to be removed in inner region
C-             (4->6)      =   "      "     "  "    "     " outer "
C-             (7)         = Energy of storey crossed by track
C?
C!======================================================================
#ifndef DOC
#include "ecnmsk.h"
#include "esdajj.h"
#include "eclujj.h"
#include "alcons.h"
#include "emskpa.h"
#include "bcs.h"
      DIMENSION COSI(8) , DX (3) , DEX(3) , EVECT(4)
      DIMENSION VEXTR (3),ITRACK (5) , ER (7)
C
#include "bmacro.h"
C
C-    If no storeys are intercepted by track go away
C
        CALL VZERO (ER ,7)
      IF ( ITRACK(1).EQ.0) GO TO 300
C
C-    Set pointers
C
      KESDA = IW (NAESDA)
      JESDA = KROW(KESDA,NESDA)
C
C-    Determine storey coordinates
C
      IK   = IW ( JESDA+JESDDK)
      IPHI = IW ( JESDA+JESDFI)
      ITHE = IW ( JESDA+JESDTJ)
      EN   = RW ( JESDA+JESDME)
      ITOW = IW ( JESDA+JESDIO)
C
C
C-   Cell center coordinates
C
      CALL ESRBC('ALEPH',ITHE,IPHI,IK,COSI)
C
C-    Calculate storey distance to the track impact
C
      DR = 0.
      DO 100 IX =1,3
       DR = DR + COSI(IX)**2
 100  CONTINUE
      CANG2 = VDOTN(VEXTR,COSI,3)
      IF (ABS(CANG2).GT.1.) CANG2 = SIGN(1.,CANG2)
      ANG2  = ACOS(CANG2)
      RANG2 = SIN(ANG2)*SQRT(DR)
      XVAR  = ABS(RANG2)/XNORM(IK)
C
C-    Establish whether storey is in road
C
C-    Determine region IR and maximum energy allowed EMAX
C
      IR = 2
      IF (XVAR.LT.0.5*ZON(IK)) IR = 1
      IF (XVAR.GT.ZON(IK))     IR = 0
      IF (CANG2.LT.0.)         IR = 0
      IF (IR.EQ.0 ) GO TO 300
      EMAX  = PMOM * EZON(IK,IR)
C
C-    Check if cell not already partially removed
C
      NAECTE = NAMIND ('ECTE')
      KECTE  = IW(NAECTE)
      IF ( KECTE.EQ.0 ) THEN
         CALL RERROR('EROAD',1,'Problem no ECTE bank exists')
         GO TO 300
      ENDIF
      NECTE = IW (KECTE + LMHROW )
      EPREV = 0.
      JECTE = 0
      IF ( NECTE . NE. 0 ) THEN
         DO 200 IECTE = 1,NECTE
         JETCA = KROW(KECTE,IECTE)
         IF ( IW(JETCA+1).EQ.ITOW) THEN
            JECTE = JETCA
            EPREV = RW(JECTE+2)
         ENDIF
 200     CONTINUE
      ENDIF
C
      IF(JECTE.EQ.0) THEN
C
C-    First time that storey is included in mask
C
         IW (KECTE+2) = IW(KECTE+2) + 1
         JECTE = KECTE+NECTE*IW(KECTE+1)+LMHLEN
         CALL VZERO (IW (JECTE+1) , IW(KECTE+1))
      ENDIF
C
C-    Now compare to energy already removed
C
      INDI = (IR-1) * 3
      IF ( EMAX .GT.EN-EPREV) THEN
C-    Remove everything unless...
         ER(INDI + 1) = EN-EPREV
C-    ..energy has already been completely removed
         IF (ER(INDI+1).LT.0.) ER(INDI+1)= 0.
      ELSE
         ER(INDI + 1) = EMAX
      ENDIF
      CALL ECOS(ER(INDI+1),IPHI,ITHE,IK,EVECT)
C
C-    Tower address
      IW ( JECTE+1) = ITOW
C-    Removed energy
      RW ( JECTE+2) = EPREV + EVECT(4)
C
      CALL UCOPY (EVECT(1),ER(INDI+1),3)
C
 300  CONTINUE
C
      RETURN
      END
#endif
