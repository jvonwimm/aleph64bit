      SUBROUTINE SAPRDI
C ----------------------------------------------------------------
C! Print out SADIgitization Bank in readable format
C                    H.Burkhardt    27/05/87
C  Modified          H. Meinhard    14/03/89 (crate numbering corr.)
C.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
C
      EXTERNAL NAMIND

C
#include "bmacro.h"
C ----------------------------------------------------------
C - Get output unit #
C
      LOUT = IW(6)
      NASADI=NAMIND('SADI')
      KSADI=IW(NASADI)
      IF(KSADI.EQ.0) THEN
        WRITE(LOUT,'(/1X,''+++SAPRDI no SADI bank found'')')
        GOTO 900
      ENDIF
      NHIT=LROWS(KSADI)
      KSA=KSADI+LMHLEN
      WRITE(LOUT,1001) NHIT
 1001 FORMAT(/1X,'+++SAPRDI printout of SADI bank,',
     &   ' number of data words or TDCs :',I5,
     &   /'        raw  data        decoded in',/' #word',
     &   '     in HEX      JCRAT     JCARD     JTDCN     ITDCC')
      DO 10 I=1,NHIT
        IDAT=IW(KSA+I)
        JCARD=IBITS(IDAT,16,5)+1
        JCRAT=IBITS(IDAT,21,3)
        JTDCN=IBITS(IDAT,24,8)+1
        ITDCC=IBITS(IDAT,0,16)
        WRITE(LOUT,'(I5,5X,Z8,5I10)') I,IDAT,JCRAT,JCARD,JTDCN,ITDCC
   10 CONTINUE
  900 CONTINUE
      END
#endif
