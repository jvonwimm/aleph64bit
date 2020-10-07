      FUNCTION GATRAL ( ITPIC,JFPIC,THE,PHI)
C.----------------------------------------------------------------------
CKEY GAMPACK F4    / INTERNAL
C   J.C.Brient      Creation  1/10/91
C! COMPUTE F4/ETOT as function of theta,phi
C  J.Badier fitted function is used for the transverse energy
C  deposition of single photon/electron . The widths of the storeys
C  have been obtained from ECAL GEOM.PACKAGE by H.Videau.
C   Input :
C           ITPIC   Itheta peak                  INTEGER
C           JFPIC   Jphi   peak                  INTEGER
C           THE     theta  peak                  INTEGER
C           PHI     phi    peak                  INTEGER
C   Output:
C           Function = F4/Etot(itpic,jfpic,the,phi)
C   Calls: None
C   Called by GAMPEK
C.----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "ecoxa.h"
      DIMENSION POINT(3)
      GATRAL = E4ETB
      IF(ITPIC .GE. ITOV1 .AND. ITPIC .LE. ITOV3) RETURN

      ITREF = ITPIC
      IF(ITPIC .GT. ITOV3) ITREF = ITHTO+1 -ITPIC
C
C now compute the region for the F4
C----------------------------------
      KST = 2
      CALL ESRBC('ALEPH',ITPIC,JFPIC,KST,POINT)
      CT  = ABS(POINT(3))/VMOD(POINT,3)
      CTPIC=COS(THE)
      IT2 = ITREF + 1
      IF(CT .GT. CTPIC) IT2=ITREF-1
      IF(IT2 .GE. ITOV1) IT2 = ITOV1-1
      IF(IT2 .EQ. 0 ) IT2 = 1

      RMEAN = STRPHI(ITREF) + STRPHI(IT2)

      X = (STWIDT + RMEAN/2. )/ 2.

C -----------------------------------------
C function (J.Badier) of transv. deposition
C -----------------------------------------
C
C  G(x) = 1. - 1./(2.+2.4*x + 1.35*x**1.9)
C
C -----------------------------------------

      G1 = 1. - 1./(2.+2.4*X + 1.35*X**1.9)
      GATRAL = 3.*G1-2.

      RETURN
      END
#endif
