      SUBROUTINE SAPRHI
C ----------------------------------------------------------------
C! Print out SAHIt Bank in readable format
C                    H.Burkhardt    27/05/87
C.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
C
      EXTERNAL NAMIND
      DATA NASAH /0/
C
#include "bmacro.h"
C ----------------------------------------------------------
C - Get output unit #
C
      LOUT = IW(6)
      IF (NASAH .EQ. 0) NASAH=NAMIND('SAHT')
      KSAHT=IW(NASAH)
      IF(KSAHT.EQ.0) THEN
        WRITE(LOUT,'(/1X,''+++SAPRHI no SAHI bank found'')')
        GOTO 900
      ENDIF
      NHIT=LROWS(KSAHT)
      KSA=KSAHT+LMHLEN
      WRITE(LOUT,1001) NHIT
 1001 FORMAT(/1X,'+++SAPRHI printout of SAHI bank,',
     &   ' number of data words or hits  :',I5,
     &   /'        raw  data        decoded in',/' #word',
     &   '     in HEX       ISIDe     ILAYer    ISECtor   IWIRe',
     &   '     DIST in cm')
      DO 10 I=1,NHIT
        IDAT=IW(KSA+I)
        ISID=IBITS(IDAT,28,3)
        ILAY=IBITS(IDAT,24,4)
        ISEC=IBITS(IDAT,20,4)
        IWIR=IBITS(IDAT,16,4)
        DIST=FLOAT(IBITS(IDAT, 0,16))/65536.
        WRITE(LOUT,'(I5,5X,Z8,4I10,3X,F7.4)') I,IDAT,ISID,ILAY,ISEC,
     &    IWIR,DIST
   10 CONTINUE
  900 CONTINUE
      END
#endif
