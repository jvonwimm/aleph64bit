       SUBROUTINE EFAXE (IERR)
C=======================================================================
C!   For the different fits calculates the total energy
C!   and performs the jet algorithms
C-
C-   Authors : M.N. Minard , M. Pepe       10/05/89
C-   Modified: M.N  Minard                 04/01/91
C-           : M.N Minard                  28/02/91
C
C?
C!======================================================================
#ifndef DOC
       LOGICAL FIRST
#include "jarray.h"
#include "alcons.h"
#include "jetset.h"
#include "rparac.h"
#include "pjetef.h"
#include "efoljj.h"
#include "ejetjj.h"
#include "bcs.h"
       DIMENSION PVEC(4),PBEAM(3)
       DATA PBEAM/0.,0.,1./
      DATA YMIN , EVIS ,ETHR / 0.003,0.,0.5/
       DATA FIRST /.TRUE./
#include "bmacro.h"
       NHIS = 1000*JULEF
       CALL VZERO(VRECNF,10)
       CALL VZERO(NFITJT,2)

       IF(FIRST) THEN
        KJTOF = NLINK('JTOF',0)
        FIRST = .FALSE.
       ENDIF
C
C-    Loop on two different fits
C
      NFITM = 2
      IF (IERR.NE.0) THEN
         NFITJT(2) = -10.
         GO TO 990
      ENDIF
      DO 100 IFIT = 1 , NFITM
      N = 0
      ETRACK = 0.
C
      ISTOP = 0
      CALL ESFIL ( JERR,ESUMW)
      IF ( JERR.NE.0) GO TO 980
      CALL ESVETO ( ESUMW,JERR)
      IF ( JERR.NE.0) GO TO 980
C
C-    Now Built EFOL bank & fill jet array
C
      CALL EFOLMK(IFIT,ESUMW,JERR)
      IF ( JERR.LT.0) GO TO 999
      IF ( JERR.GT.0) GO TO 980
C
C-    Now perform jets algorithm & thrust
C
      IF ( IFIT.EQ.2) THEN
        CALL ALTHRU (IFIT,THR,OBL)

        DO 55 IK =  1,3
        DO 55 IL =  1,4
C
C- IL= 1,3 -> Px,Py,Py components
C-   = 4   -> eigenvalues
C- IK= 1   -> Thrust axis
C- IK= 2   -> major axis
C- IK= 3   -> minor axis
C
         THRTEF (IL,IK,IFIT) =  P(N+IK,IL)

 55   CONTINUE

C
C Call ALCLUS only in absence of JTOF cards
C
      NJET = 0
      IF(KJTOF.EQ.0)  CALL AJMMCL(YMIN,EVIS,NJET)
      IF ( KJTOF.EQ.0.AND.NJET.LE.0) NJET = 0

      NMAX = NJET
      IF (NMAX.GT.20) NMAX = 20
      NFITJT(IFIT) = NMAX
      IF (NMAX.GT.0) THEN
      DO 60 IL = 1,NMAX
         DO 58 IK=1,5
         PJETRF (IK,IL,IFIT) = P(N+IL,IK)
 58      CONTINUE
 60   CONTINUE
      ENDIF
C
C
      ENDIF
  100  CONTINUE
C
C-    In case problem fill only calorimetry
C
C
C-    IDENTIFY JET # FOR EACH CELL
C
      KEFOL = NLINK('EFOL',1)
      IF ( KEFOL.NE.0) THEN
        NEFOL = LROWS(KEFOL)
        DO 180 IEFOL = 1,NEFOL
        ICLUS = ITABL(KEFOL,IEFOL,JEFOLJ)
        IW(KROW(KEFOL,IEFOL) +JEFOLJ ) = K(ICLUS,1)
 180    CONTINUE
      ENDIF
C
C-    Fill EJET bank
      LENJ = LMHLEN + LEJETA*NJET
      CALL AUBOS('EJET',0,LENJ,KEJET,IGARB)
      IF ( IGARB.EQ.2) GO TO 997
      IW(KEJET+LMHCOL) = LEJETA
      IW(KEJET+LMHROW) = NJET
      DO 700 IJET = 1,NJET
        JEJET = KROW(KEJET,IJET)
        RW(JEJET+JEJEPX) = P(N+IJET,1)
        RW(JEJET+JEJEPY) = P(N+IJET,2)
        RW(JEJET+JEJEPZ) = P(N+IJET,3)
        RW(JEJET+JEJEPE) = P(N+IJET,4)
  700 CONTINUE
C
C     Fill relevant histogrammes
C
      CALL HFILL (NHIS+10,VRECNF(4,2),DUM,1.)
      CALL HFILL (NHIS+11,VRECNF(4,1),DUM,1.)
      CALL HFILL (NHIS+12,THRTEF(4,1,2),DUM,1.)
C
C     Calculate thrust angle / beam direction
      THRLNG=SQRT(THRTEF(1,1,2)**2+THRTEF(2,1,2)**2+THRTEF(3,1,2)**2)
      IF (THRLNG.GT.0.001) THEN
C
      ATET =  VDOTN(THRTEF(1,1,2),PBEAM(1),3)
      COSI = ACOS (ABS(ATET))
      CALL HFILL (NHIS+13,COSI*RADEG,VRECNF(4,2),1.)
      IF(KJTOF.EQ.0) CALL HFILL (NHIS+14,FLOAT(NFITJT(2)),DUM,1.)
      ENDIF
 990  CONTINUE
      RETURN
 980  NFITJT ( 2 ) = -10
      RETURN
  997 CALL RERROR('EFAXE',1,'No space to book EJET ')
      IERR = 1
      RETURN
 999   CALL RERROR('EFAXE',1,'Array JETSET too small')
       IERR = 1
      END

#endif
