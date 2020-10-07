      SUBROUTINE QCPTETI(IFCHT,TETIFL,IECL)
C =============================================================================
C! Computes  TETIFL  ECAL theta impact in ECAL numbering
C
C     Input  IFCHT   Track number
C     Output TETIFL  ECAL theta impact in ECAL numbering
C            IECL
C =============================================================================
#ifndef DOC
#include "qcde.h"
#include "evehjj.h"
#include "rhahjj.h"
#include "decojj.h"
      DIMENSION PORI(7),XTRAP(3,3)
      EXTERNAL CHAINT,NAMIND
      CHARACTER*4 CHAINT
      CHARACTER*8 PRNAM
      DATA  IFIR / 1 /
#include "qmacro.h"    
C ------------------------------------------------------------------------------
        KRHAH = IW(NARHAH)
        KEVEH = IW(NAEVEH)
        KEIDT = IW(NAEIDT)
        KFRFT = IW(NAFRFT)
        KPECO = IW(NAPECO)
        IF (KRHAH.EQ.0.OR.KEVEH.EQ.0) GO TO 990
C ------------------------------------------------------------------------------
      TETIFL  = 399.
      IECL = 0
       IF (XEID(IFCHT)) THEN
C
C-    Take Track parameter
C
         PTT = QP(IFCHT)
         ICH = QCH(IFCHT)
         COST = ABS(QZ(IFCHT)/QP(IFCHT))
C
C
C-   Calculate theta/phi angle of cluster
C
         PORI(1) = QX(IFCHT)
         PORI(2) = QY(IFCHT)
         PORI(3) = QZ(IFCHT)
         PORI(6) = ICH
         DO 70 IL =1,3
         PORI (3+IL) = PORI(IL)/QP(IFCHT)
 70      CONTINUE
         PORI ( 7) = QP(IFCHT)
         IREFT =KTN(IFCHT)
         IFRFT = IREFT
         JFRFT = KROW(KFRFT,IFRFT)
         CP = COS (RW(JFRFT+JFRFP0))
         SP = SIN (RW(JFRFT+JFRFP0))
         D0 = RTABL ( KFRFT,IFRFT,JFRFD0)
         PORI ( 1) = D0*SP
         PORI (2) = - D0*CP
         PORI ( 3) = RTABL ( KFRFT,IFRFT,JFRFZ0)
         CALL QEXTRA(PORI,ICH,XTRAP,IFIR)
         IF (IFIR.NE.0) THEN
           I = IFIR
           PHICL = ATAN2(XTRAP(2,I),XTRAP(1,I))
           RDEL = SQRT(XTRAP(1,I)**2+XTRAP(2,I)**2+XTRAP(3,I)**2)
           THECL = ACOS(XTRAP(3,I)/RDEL)
           IECL = -1
           IF ( PHICL.LT.0) PHICL = PHICL+TWOPI
         ELSE
C
C-    Take Cluster parameter if any
C
         IREFT =KTN(IFCHT)
         IFRFT = IREFT
         IECL = 0
         IF(KEIDT.NE.0) THEN
           NEIDT = LROWS(KEIDT)
           DO IEL = 1, NEIDT
           ITR  = ITABL(KEIDT,IEL,JEIDFR)
             IF ( ITR.EQ.IREFT ) THEN
               IECL = ITABL(KEIDT,IEL,JEIDPE)
             ENDIF
           ENDDO
           IF ( KPECO.EQ.0) IECL=0
           IF ( KPECO.NE.0.AND.IECL.NE.0) THEN
             PHICL = RTABL(KPECO,IECL,JPECPH)
             THECL = RTABL(KPECO,IECL,JPECTH)
           ENDIF
         ENDIF

       ENDIF
       IF ( IECL.NE.0) THEN
C
C-      Calculate the row number in theta
C
         SK=1.
         CALL QETHPH(PHICL,THECL,SK,EPHI,ETET)
         TETIFL = ETET
c        ISIDE = 1
c        IF ( TETIFL.LT.51.OR.TETIFL.GT.178) ISIDE = ISIDE + 2
c        IF ( TETIFL.LT.45.OR.TETIFL.GT.188) ISIDE = ISIDE - 1
       ENDIF
      ENDIF

 990  CONTINUE
      RETURN
      END
#endif

