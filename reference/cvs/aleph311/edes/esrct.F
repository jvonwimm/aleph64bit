      SUBROUTINE ESRCT(SYSTM,RW,CL,ST,LECON,XCONTR,YCONTR,ZCONTR)
C-----------------------------------------------------------------
CKEY ECALDES STOREY CONTOUR / USER
C     H.Videau      Creation 12/03/87   modification 08/02/88
C! Computes storey contours
C  Gets the LEPOIN corners of a storey (RW,CL,ST) in the system 'SYSTM'
C  stores their coordinates in the arrays XCONTR,YCONTR,ZCONTR in such
C  a way that the contour of the storey can be drawn in one go.
C  The number of points is LECON.
C  This ugly definition is adopted to conform to the fortran binding
C  of GKS.Please read GKS notice if necessary.
C  The ordering of the points is given by the relationship ECNC with
C  the type of the volume "Storey".
C   Input :
C           SYSTM         reference system (character*5)
C           RW,CL,ST      storey address   (Integers)
C   Output:
C           LECON         number of points in the contour
C           XCONTR(LECON)      Max(LECON) = 16 in type 1
C           YCONTR(LECON)
C           ZCONTR(LECON)  contour points coordinates
C   Calls: ESRCR
C   Called by USER.
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C    Variables d'input
        CHARACTER*5 SYSTM
        INTEGER RW,CL,ST
C     Variables d'output
        INTEGER LECON
        REAL XCONTR(*),YCONTR(*),ZCONTR(*)
C     Locales
        INTEGER I,J, LEPOIN,TYPE
        REAL CORNER(ECALLS-1,8)
C Etablir le type   A VOIR
      TYPE=1
        CALL ESRCR(SYSTM,RW,CL,ST,LEPOIN,CORNER)
C
        LECON=- ETYVFT(TYPE)+ETYVLT(TYPE)+1
        DO 1 I=ETYVFT(TYPE),ETYVLT(TYPE)
              XCONTR(ECNCEQ(I)) = CORNER(1,ECNCEC(I))
              YCONTR(ECNCEQ(I)) = CORNER(2,ECNCEC(I))
              ZCONTR(ECNCEQ(I)) = CORNER(3,ECNCEC(I))
 1      CONTINUE
C
        END
#endif
