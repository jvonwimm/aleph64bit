*DK vfndmp
      SUBROUTINE VFNDMP (IMOD,MAXS,IPICH,MAXC,MUXL,COEFS,
     $                   COEFN,CSTRP,ICDIM,MXCPL)
C----------------------------------------------------------------------
C! Find module parameters for one module
CKEY VDET DBASE
C!
C!
C!  Author         A. Bonissent 15-Jan-1994
C!
C!  Description
C!  ===========
C!  Input : IMOD module number
C!  Output : MAXS  Nb strips in one module
C!           IPICH Readout pitch
C!           MAXC  Nb readout channels in one module
C!           MUXL  Multiplexing length
C!           COEFS Coupling coefficients for signal
C!           COEFN Coupling coefficients for signal
C!           CSTRP Strip capacitance
C!           ICDIM Dimension of arrays above
C!           MXCPL Max number of neighbours to be taken into account
C
C-----------------------------------------------------------------------
C
#ifndef DOC
      SAVE NAVDCC,IVSTP
C
      DIMENSION NBCLN(2),NVDCR(2),VDSLP(2),VDPAL(2),VDELC(2)
      DIMENSION MXCNO(2),MXCSI(2),VELGV(2),VDLCO(2),IOFSET(2),NBITSH(2)
      DIMENSION MAXS(2),IPICH(2),MAXC(2),MUXL(2),NROS(2)
      DIMENSION COEFS(2,0:*),COEFN(2,*),MXCPL(2),CSTRP(2)
      DIMENSION STRL(2)
      INTEGER VSENSI,VPHSTM,VNRSSC
#include "vdccjj.h"
#include "bcs.h"
      LOGICAL FIRST
      DATA FIRST /.TRUE./
#include "bmacro.h"
      IF (FIRST) THEN
        FIRST = .FALSE.
        NAVDCC = NAMIND('VDCC')
      ENDIF
      CALL VFNDEL
     $   (IMOD,NBCLN,NVDCR,VDSLP,VDPAL,VDELC,
     $    MXCSI,MXCNO,VELGV,VDLCO,IOFSET,NBITSH)
C
C We need to know the strip length
C
      IBID = VSENSI(1,S1MI,S1MA,S2MI,S2MA)
      STRL(1) = S2MA-S2MI
      IBID = VSENSI(2,S1MI,S1MA,S2MI,S2MA)
      STRL(2) = S1MA-S1MI
C
C Detectors are daisy chained
C
      DO IV=1,2
        STRL(IV)=STRL(IV)*VNRSSC(IV)
      ENDDO
C
C Get  max number of strips in one wafer
C
      DO IV=1,2
         IBID = VPHSTM(IV,MAXS(IV),PITCH)
      ENDDO
C
C Get  max number of readout channels in one module,
C and readout frequency ipich
C
      CALL VFNDMC(IMOD,MAXC,NROS,MUXL,IPICH)
C
C max number of neighbours to take into account.
C
      MXCPL(1) = MXCSI(1)
      MXCPL(2) = MXCSI(2)
C
C Reset the arrays
C
      CALL VZERO(COEFN,2*ICDIM)
      CALL VZERO(COEFS(1,0),2*(ICDIM+1))
C
      KVDCC=IW(NAVDCC)
      NVDCC=LROWS(KVDCC)
      DO 41 IVDCC=1,NVDCC
      IV=ITABL(KVDCC,IVDCC,JVDCVI)
      IDIST=ITABL(KVDCC,IVDCC,JVDCDI)
C
C Protection for the case when the bank contains more coupling
C coefficients than was foreseen by the code : use only the
C first few
      IF (IDIST.GT.ICDIM)GO TO 41
C
      COEFS(IV,IDIST)=RTABL(KVDCC,IVDCC,JVDCSC)
C
C Compute the coefficients for the correlated noise.
C These are defined in the bank per unit length
C When distance is zero, we have the strip capacitance
C
      IF(IDIST.GT.0)THEN
         COEFN(IV,IDIST)=RTABL(KVDCC,IVDCC,JVDCNC)*STRL(IV)
      ELSE
         CSTRP(IV)=RTABL(KVDCC,IVDCC,JVDCNC)*STRL(IV)
      ENDIF
   41 CONTINUE
C
C Add the amplifier input capacitance
C
      DO IV=1,2
         CSTRP(IV) = VDELC(IV)+CSTRP(IV)
      ENDDO
      RETURN
      END
#endif
