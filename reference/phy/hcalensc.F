      SUBROUTINE HCALENSC
C
C   Corrects the HCAL energy for 1996 and 1997 data
C   in all the relevant banks (PHST, PHCO, HMAD)
C                                 A.Sciaba`      5-98
C
C
#include "qcde.h"
#include "qhac.h"
      REAL OLDEN(500)
      INTEGER VERSION
      LOGICAL FIRST / .TRUE. /
      CHARACTER*4 PROG, CHAINT
      EXTERNAL QCORFIC
#include "bmacro.h"
C
C     Runs only on POTs and DSTs
      IF (XMINI) RETURN
C
C     Make the correction only if the JULIA version of the last
C     reprocessing is 307 or older
      JRHAH = IW(NAMIND('RHAH'))
      IF (JRHAH.GT.0) THEN
         DO I = 1, LROWS(JRHAH)
            PROG = CHAINT(ITABL(JRHAH,I,1))
            IF (PROG.EQ.'JULI') THEN
               VERSION = ITABL(JRHAH,I,5)
               IF (VERSION.GE.307) RETURN
            ENDIF
         ENDDO
      ELSE
         CALL QWMESS('++++++ Bank RHAH not found: '//
     &        'as default, the correction for the HCAL '//
     &        'online bug is NOT applied')
      ENDIF

 10   IF (FIRST) THEN
         IND = MDARD(IW,KUCONS,'H1EC',2)
         IND = MDARD(IW,KUCONS,'H2EC',2)
         IND = MDARD(IW,KUCONS,'H3EC',2)
         IND = MDARD(IW,KUCONS,'H4EC',2)
         FIRST = .FALSE.
      ENDIF

      CALL VZERO(OLDEN,100)
C
C- PHST
      JPHST = NLINK('PHST',0)
      IF (JPHST.NE.0) THEN
         NCOLS = IW(JPHST+1)
         DO I = 1, LROWS(JPHST)
            ITH = ITABL(JPHST,I,1)
            IPH = ITABL(JPHST,I,2)
            ENE = RTABL(JPHST,I,3)
            RW(JPHST+2+(I-1)*NCOLS+3) = QCORFIC(ENE,ITH,IPH)
         ENDDO
      ENDIF
C
C- PHCO
      JPHCO = NLINK('PHCO',0)
      IF(JPHCO.NE.0) THEN
         NCOLS = IW(JPHCO+1)
         IF (LROWS(JPHCO).GT.500) THEN
            WRITE(6,*)'+++++ TOO MANY HCAL CLUSTERS!'
            RETURN
         ENDIF
         DO I = 1, LROWS(JPHCO)
            OLDEN(I) = RTABL(JPHCO,I,4)
            ENER = 0.
            ENEC = 0.
            DO J = 1, LROWS(JPHST)
               IF (ITABL(JPHST,J,4).EQ.I) THEN
                  ITH = ITABL(JPHST,J,1)
                  IPH = ITABL(JPHST,J,2)
                  ENE = RTABL(JPHST,J,3)
                  ENER = ENER + ENE / COFLIG(ITH,IPH) 
                  ENEC = ENEC + ENE
               ENDIF
            ENDDO
            RW(JPHCO+2+(I-1)*NCOLS+1) = ENER
            RW(JPHCO+2+(I-1)*NCOLS+4) = ENEC
         ENDDO
      ENDIF
C
C- HMAD
      JHMAD = NLINK('HMAD',0)
      IF (JHMAD.NE.0) THEN
         NCOLS = IW(JHMAD+1)
         DO I = 1, LROWS(JHMAD)
            ENE = RTABL(JHMAD,I,6)
            DO J = 1, LROWS(JPHCO)
               IF ((ABS(ENE-OLDEN(J))).LE.1.E-4)
     &           RW(JHMAD+2+(I-1)*NCOLS+6) = RTABL(JPHCO,J,4)
            ENDDO
         ENDDO
      ENDIF
C
      RETURN
      END
C-----------------------------------------------------------
      REAL FUNCTION COFLIG(IBTH0,IBPH0)

#include "qcde.h"
#include "qmacro.h"
C
      COFLIG = 1.
      CALL HCSIMM(IBTH0,IBPH0,IBTH,IBPH,IRGN)
      IBANK1 = IW(NAMIND('H1EC'))
      IBANK2 = IW(NAMIND('H2EC'))
      IBANK3 = IW(NAMIND('H3EC'))
      IBANK4 = IW(NAMIND('H4EC'))
      IF(IRGN.EQ.2) IBTH = IBTH-4
      IF(IRGN.EQ.3) IBTH = IBTH-10
      IF(IRGN.EQ.4) IBTH = IBTH-17
      IF(IRGN.EQ.1) IBANK = IBANK1
      IF(IRGN.EQ.2) IBANK = IBANK2
      IF(IRGN.EQ.3) IBANK = IBANK3
      IF(IRGN.EQ.4) IBANK = IBANK4
      IF (IBANK.LE.0) THEN
         WRITE(6,*)'++++ BANKS H1EC, H2EC, H3EC, H4EC MISSING!'
      ENDIF
      COFLIG = RTABL(IBANK,IBTH,IBPH)
      RETURN
      END      
