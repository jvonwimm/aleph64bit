      SUBROUTINE QEWSUM(eweven,ewodd,ier)
C-----------------------------------------------------------------------------
#ifdef doc
C! CKEY ECAL/USER
C  Gets energy on ECAL wires summed over even/odd planes
C  Authors L.Duflot,J.Boucrot    28-Apr-1997
C
C  Output arguments:
C   eweven = sum of wire energies on even planes for the 36 ECAL modules
C            (in GeV) 
C   ewodd  = sum of wire energies on  odd planes for the 36 ECAL modules
C            (in GeV)
C   eweven and ewodd are REAL*4 arrays 
C   which must be dimensioned to 36 in the calling routine
C
C   ier    = 1 if no banks of wire energies for this event 
C          = 0 if all OK
#else
C-----------------------------------------------------------------------------
#include "pweijj.h"
#include "dwesjj.h"
#include "qcde.h"
      PARAMETER ( NMOD=36 , FACMI = 1000.)
      REAL*4   EWEVEN(NMOD),EWODD(NMOD)
#include "qmacro.h"
C-----------------------------------------------------------------------------
      IER=1
      CALL VZERO(EWEVEN,NMOD)
      CALL VZERO(EWODD,NMOD)
      IF (XNANO) GO TO 999
C
C Reading a MINI: Information is in bank 'DWES' :
C
      IF (XMINI) THEN
        JDWES=IW(NAMIND('DWES'))
        IF(JDWES .EQ. 0) GO TO 999 
        IER=0
        DO 100 IDWES=1,LROWS(JDWES)                     
           KDWES=KROW(JDWES,IDWES)
           IMOD=IW(KDWES+JDWEMN)
           IF (IMOD.LE.0.OR.IMOD.GT.NMOD) GO TO 100
           EWODD(IMOD) =FLOAT(IW(KDWES+JDWESO))/FACMI
           EWEVEN(IMOD)=FLOAT(IW(KDWES+JDWESE))/FACMI
 100    CONTINUE                                                     
        GO TO 999
      ENDIF
C
C Reading a POT or a DST: information in PWEI or PEWI bank:
C
      JPWEI = IW(NAMIND('PWEI'))            
      IF (JPWEI .EQ. 0) JPWEI = IW(NAMIND('PEWI'))
      IF (JPWEI.EQ.0) GO TO 999  
      FACPO = FACMI*FACMI    
C
      DO 200 I = 1,LROWS(JPWEI)
        IMODUL = ITABL(JPWEI,I,JPWEMN)
        IF (IMODUL.LE.0.OR.IMODUL.GT.NMOD) GO TO 200
        IPMIN=JPWEPD-1
        IPMAX=JPWESS-2
        DO IPLA = ipmin,ipmax	
          ENEPL = FLOAT(ITABL(JPWEI,I,IPLA+1))/FACPO
          IF(MOD(IPLA,2).EQ.0) THEN
            EWEVEN(IMODUL) = EWEVEN(IMODUL) + ENEPL 
          ELSE
            EWODD(IMODUL) = EWODD(IMODUL) + ENEPL 
          ENDIF
        ENDDO
 200  CONTINUE
      IER=0
C
 999  RETURN
      END
#endif
