      SUBROUTINE TPK1CO(IC,IPTNC)
C
C-----------------------------------------------------------------------
C! Fill one entry into the PTNC bank
C!
C!    Author:  R. Johnson     15-06-88
C!
C!    Input:   IC       /I      Coordinate number in TPCO
C!    Output:  IPTNC(6) /I      A single row of the PTNC bank
C!
C!    Called by TPAKCO
C!
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "alcons.h"
#include "bcs.h"
#include "tpcojj.h"
#include "ptncjj.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "ptunjj.h"
C
      CHARACTER TEXT*90
      DIMENSION IPTNC(*)
      LOGICAL FIRST
      DATA FIRST/.TRUE./, LBYTE/255/, LHBYT/127/
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NPTUN=NAMIND('PTUN')
        NTPCO=NAMIND('TPCO')
        NPTNC=NAMIND('PTNC')
      ENDIF
      KPTUN=IW(NPTUN)+LMHLEN
      KTPCO=IW(NTPCO)
      KPTNC=IW(NPTNC)
C
      ID=ITABL(KTPCO,IC,JTPCIN)
      IROW=ID/100000
      ISLOT=(ID-IROW*100000)/1000
      ISTYP=ITPTYP(ISLOT)
      IF (ISTYP.NE.1) THEN
        IROWS=IROW-NTPDRW(1)
      ELSE
        IROWS=IROW
      ENDIF
      IPTNC(JPTNSL)=ISLOT
      IPTNC(JPTNSR)=IROWS
      PHI=RTABL(KTPCO,IC,JTPCPH)
      Z=RTABL(KTPCO,IC,JTPCZV)
      R=RTABL(KTPCO,IC,JTPCRV)
C
C++   Save only the raw, uncorrected values (in sector coord sys.)
C
      RPHIS=RTABL(KTPCO,IC,JTPCRR)
      ZS=RTABL(KTPCO,IC,JTPCRZ)
C
      IPTNC(JPTNRP)=NINT(RPHIS/RW(KPTUN+JPTURP))
      IPTNC(JPTNZV)=NINT(ZS/RW(KPTUN+JPTUZS))
      DRPHI=RTABL(KTPCO,IC,JTPCSR)
C
C++   If the variances of r*phi and z are negative, then there must have
C++   been a bug somewhere.  Nonetheless, we patch them here by setting
C++   them to some more reasonable value.
C
      IF (DRPHI.GE.0.) THEN
        SRPHI=SQRT(DRPHI)
      ELSE
        SRPHI=0.01
        CALL RERROR('TPK1CO',3,
     &                   'DRPHI**2 negative! Set DRPHI to 100 microns')
      ENDIF
      DZ=RTABL(KTPCO,IC,JTPCSZ)
      IF (DZ.GE.0.) THEN
        SZ=SQRT(DZ)
      ELSE
        SZ=0.08
        CALL RERROR('TPK1CO',4,'DZ**2 negative! Set DZ to 800 microns')
      ENDIF
      IPTNC(JPTNSP)=NINT(SRPHI/RW(KPTUN+JPTUSR))
      IF (IPTNC(JPTNSP).GT.LBYTE) THEN
        IPTNC(JPTNSP)=LBYTE
        WRITE(TEXT,36) SRPHI
   36   FORMAT('Sigma r*phi of ',E12.5,' is too large to pack',
     &         ' into a single byte and is truncated.')
        CALL RERROR('TPK1CO',2,TEXT(1:85))
      ENDIF
      IPTNC(JPTNSZ)=NINT(SZ/RW(KPTUN+JPTUSZ))
      IF (IPTNC(JPTNSZ).GT.LBYTE) THEN
        IPTNC(JPTNSZ)=LBYTE
        WRITE(TEXT,37) SZ
   37   FORMAT('Sigma z of ',E12.5,' is too large to pack',
     &         ' into a single byte and is truncated.')
        CALL RERROR('TPK1CO',3,TEXT(1:81))
      ENDIF
C
      RETURN
      END
#endif
