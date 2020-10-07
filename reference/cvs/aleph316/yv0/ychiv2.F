      FUNCTION YCHIV2 (KPOI,VPRIM,SIVPR,IFAIL)
C----------------------------------------------------------------------
C! Calculate the chi2 of a track,whith the constraint of primary vertex
C
CKEY YV0 CHI2 TRACK /INTERNAL
C    AUTHOR: M.A.CIOCCI,L.ROLANDI 24/3/88
C    MODIFIED:M.A.CIOCCI 20/2/90
C    Modified: J.Sedgbeer 19/6/91. Bug fix in ELIP1(4) = ........
C    MODIFIED:M.A.CIOCCI 20/1/93. Now the chi2 that a track comes
C                                 from the primary includes the
C                                 covariance of the primary.
C
C          DESCRIPTION
C          ===========
C  COMPUTES THE CHI2 OF A TRACK WITH THE CONSTRAINT
C   OF THE PRIMARY VERTEX, ASSUMED CLOSE TO THE ORIGIN
C
C    INPUT:
C               KPOI/I  NUMBER OF THE TRACK IN THE
C                       FRFT  BANK
C               VPRIM/R COORDINATES OF THE PRIMARY
C                      VERTEX
C               SIVPR/R COVARIANCE MATRIX OF THE PRIMARY
C                       vertex
C    OUTPUT:
C               IFAIL/I IF 0 ALL IS OK
C                       IF 1 BANK FRFT MISSING OR PROBLEM WITH COV MAT
C   BANKS:
C          FRFT
C
C   CALL: YFIXIS  (CHANGE CONVENCTION ABOUT COVARIANCE MATRIX
C                  IN FRFT)
C         YPSIVN  (PSI ANGLE WITH RESPECT TO THE PRIMARY +
C                  COORDINATES OF THE POINT MINIMIZING THE
C                  DISTANCE BETWEEN PRIMARY VERTEX AND HELIX )
C-------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "frftjj.h"
      EXTERNAL NLINK,NAMIND,NBANK,CHAINT,INTCHA,NDROP
      CHARACTER*4 CHAINT
      INTEGER NLINK,NAMIND,NBANK,INTCHA,NDROP

C
       DOUBLE PRECISION XX(3,3),SS,BV(6)
       DOUBLE PRECISION PP(3,3),XI(3,3)
       DOUBLE PRECISION B(3)
       REAL ELIP1(5),XD0(3),PSIN,WR(3)
       DIMENSION VPRIM(3),SIVPR(3,3)
#include "bmacro.h"
C
C
      KFRFT=IW(NAMIND('FRFT'))
          IF(KFRFT.LE.0)THEN
              IFAIL=1
               GO TO  999
          ENDIF
C
C+    GET TRACK PARAMETERS  AS IN FRFT
C
C
        ELIP1(1)        =RTABL(KFRFT,KPOI,JFRFIR)
        ELIP1(2)        =RTABL(KFRFT,KPOI,JFRFTL)
        ELIP1(3)        =RTABL(KFRFT,KPOI,JFRFP0)
        ELIP1(4)        =RTABL(KFRFT,KPOI,JFRFD0)
        ELIP1(5)        =RTABL(KFRFT,KPOI,JFRFZ0)
         CALL YPSIVN(VPRIM,ELIP1,PSIN,XD0)
C
C+  FINDS THE HELIX CLOSEST POINT COORDINATES TO THE PRIMARY VERTEX
C
        B(1)   = XD0(1)
        B(2)   = XD0(2)
        B(3)   = XD0(3)
C
C+FINDS THE COVARIANCE MATRIX OF COORDINATES (XX) OF THAT POINT
C
        CALL YV0ONG(KPOI,PSIN,XX,PP,BV,XI,IFAIL)
        IF(IFAIL.NE.0)GO TO 999
C
C+ THE COVARIANCE MATRIX OF THE QUANTITY (VPRIM-B) IS GIVEN
C+ BY (XX+SIVPR)
C
              DO 31 I=1,3
              DO 29 J=1,3
                  XX(I,J)=XX(I,J)+DBLE(SIVPR(I,J))
  29          CONTINUE
  31          CONTINUE
C
C+ IN THE CHI SQUARE CALCULATION WE MUST USE THE INVERSE OF XX
C
#ifndef BIT64
        CALL DINV(3,XX,3,WR,IFAIL)
        IF(IFAIL.NE.0)GO TO 999
#else
        CALL RINV(3,XX,3,WR,IFAIL)
        IF(IFAIL.NE.0)GO TO 999
#endif
        SS=0.D0
        DO 40 I=1,3
        DO 38 J=1,3
        SS=SS+(DBLE(VPRIM(I))-B(I))*XX(I,J)*(DBLE(VPRIM(J))-B(J))
   38   CONTINUE
   40   CONTINUE
        YCHIV2=SS
        RETURN
  999      CONTINUE
           RETURN
        END
#endif
