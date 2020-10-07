      SUBROUTINE ECTREA  (NETP1 , ICL, ITYPC)
C=======================================================================
C
C!    Choose to use track or cluster in electron case
C-    NETP1 : Track #
C-    ICL   : Cluster #
C-    ITYPC : =1 electron
C-            =11 electron from track
C-
C-     Author  : M.N. Minard                 04/01/91
C?
C!======================================================================
#ifndef DOC
#include "ectejj.h"
#include "eclujj.h"
#include "etp1jj.h"
#include "esdajj.h"
      PARAMETER (PRES=0.0015 , ERES = 0.18 )
#include "bcs.h"
#include "bmacro.h"
      NAETP1 = NAMIND('ETP1')
      NAECLU = NAMIND('ECLU')
      KETP1 = IW(NAETP1)
      KECLU = IW(NAECLU)
C
C-    Choose the best resolution
C
      PMOM = RTABL(KETP1,NETP1,JETPPT)
      EMOM = RTABL(KECLU,ICL,JECLE4)
      ERRS = SQRT(PRES**2+(ERES**2/(EMOM**2)))
      DP =   (EMOM-PMOM)/(EMOM*PMOM)
      IF (ABS(DP).GT.3.*ERRS) THEN
C
C-      Not P=E
C
        ITYPC = 1
        GO TO 900
      ENDIF
C
C-      Choose best energy determination
C
      ERRP = PRES*(PMOM**2)
      ERRE = ERES*SQRT(EMOM)
      IF ( ERRP.GT.ERRE ) THEN
        ITYPC = 1
        GO TO 900
      ELSE
      ITYPC = 11
C
C-      Remove calorimeter cells
C
      NAECTE = NAMIND ('ECTE')
      KECTE = IW(NAECTE)
      NAESDA = NAMIND ('ESDA')
      KESDA = IW(NAESDA)
      NECTE = 0
      IF ( KECTE.NE.0) NECTE = LROWS(KECTE)
      JECLU = KROW (KECLU,ICL)
      IESDA = IW(JECLU+JECLES)
C
C-    Loop on storeys in cluster
C-    check whether they are in mask table
C
 50   JESDA = KROW(KESDA,IESDA)
      ITOW = IW(JESDA+JESDIO)
C
C-    Scan ECTE to find storey in mask table
C
      IK = IW(JESDA+JESDDK)
      IPHI = IW ( JESDA+JESDFI)
      ITHE = IW ( JESDA+JESDTJ)
      EN   = RW ( JESDA+JESDME)
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
C-    Remove everything unless...
         ERIN         = EN-EPREV
C-    ..energy has already been completely removed
         IF (ERIN      .LT.0.) ERIN= 0.
C
C-    Tower address
      IW ( JECTE+1) = ITOW
C-    Removed energy
      RW ( JECTE+2) = EPREV + ERIN
C
C-     go to next storey of the cluster
C
 70    IESDA = IW( JESDA+JESDES )
       IF ( IESDA.NE.0 ) GO TO 50
 100   CONTINUE
       ENDIF
 900   CONTINUE
       RETURN
       END
#endif
