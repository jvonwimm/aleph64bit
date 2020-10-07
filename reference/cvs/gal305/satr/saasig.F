*DK saasig
      SUBROUTINE SAASIG
C-----------------------------------------------------------------------
C!    Delete multihits and reduce SAHI Bank to actual size
C   called at the end of the event before SADIGI by ASASIG
C                                   H.Burkhardt   October 1986
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "iocom.h"
#include "jobcom.h"
#include "jqcom.h"
#include "sanamc.h"
#include "satrco.h"
      DATA NASAHT /0/
#include "bmacro.h"
C if multihits are in the same brass tube (same address)
C keep only the hit which is the closest to the wire
      IF (NASAHT.EQ.0) NASAHT = NAMIND('SAHT')
      KSAHT=IW(NASAHT)
      IF(KSAHT.EQ.0) GOTO 900
      NHIT=LROWS(KSAHT)
      KSA=KSAHT+LMHLEN
      DO 1 IT1=1,NHIT-1
        ILOW=IT1+1
        IF(IW(KSA+IT1).EQ.0) GOTO 1
        IAD1=IBITS(IW(KSA+IT1),16,16)
        DO 2 IT2=ILOW,NHIT
          IF(IW(KSA+IT2).EQ.0) GOTO 2
          IAD2=IBITS(IW(KSA+IT2),16,16)
          IF(IAD1.EQ.IAD2) THEN
C           look for shorter dist, mark other hit for deletion
C           by putting the word to zero
            IF(IBITS(IW(KSA+IT1),0,16).LT.
     &         IBITS(IW(KSA+IT2),0,16)) THEN
              IW(KSA+IT2)=0
            ELSE
              IW(KSA+IT1)=0
              GOTO 1
            ENDIF
          ENDIF
    2   CONTINUE
    1 CONTINUE
      IND=0
      DO 3 IT3=1,NHIT
C       copy, skip the hits to be deleted
        IF(IW(KSA+IT3).NE.0) THEN
          IND=IND+1
          IW(KSA+IND)=IW(KSA+IT3)
        ENDIF
    3 CONTINUE
      IF(IND.LT.NHIT) THEN
C       only change bank size if multihits have been found
C       number of rows
        IW(KSAHT+2)=IND
C       now reduce SAHT bank to actual size
        CALL AUBPRS('SAHT')
      ENDIF
      IF(FDEBJO.AND.IPRIJO(6).EQ.1) THEN
         WRITE (LOUTIO,*)' +++SAASIG+++ # of deleted double hits= ',
     &                    NHIT-IND
         CALL SAPRHI
      ENDIF
  900 CONTINUE
      END
#endif
