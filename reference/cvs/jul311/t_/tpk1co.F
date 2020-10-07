      SUBROUTINE TPK1CO(IC,IPTNC)
C
C-----------------------------------------------------------------------
C! Fill one entry into the PTNC bank
C!
C!    Author:  R. Johnson     15-06-88
C!
C!    Modified:D. Casper      26-03-97  Remove requirement that errors
C!                                      fit into a byte, to allow scale
C!                                      factors to change and round-off
C!                                      to be reduced.
C!
C!                            13-05-97  Recalculate the coordinate errors
C!                                      appropriate for only pad coordinates,
C!                                      since the content of the TPCO
C!                                      bank represents the error on the
C!                                      combined pad and wire coordinate
C!                                      (if any) at this point.
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
#include "tgftjj.h"
#include "pcoijj.h"
C
      CHARACTER TEXT*90
      DIMENSION IPTNC(*)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NPTUN=NAMIND('PTUN')
        NTPCO=NAMIND('TPCO')
        NPCOI=NAMIND('PCOI')
        NTGFT=NAMIND('TGFT')
        NPTNC=NAMIND('PTNC')
      ENDIF
      KPTUN=IW(NPTUN)+LMHLEN
      KTPCO=IW(NTPCO)
      KPCOI=IW(NPCOI)
      KTGFT=IW(NTGFT)
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
C
C++  Recalculate the coordinate errors here.  Only coordinates on tracks
C++  need special treatment.
C
      ITK = ITABL(KTPCO,IC,JTPCTN)
      IF (ITK.GT.0 .AND.ITK.LE.LROWS(KTGFT)) THEN
        KCO = KROW(KTPCO,IC)
        KTK = KROW(KTGFT,ITK)
C
C++  Check whether Landau corrections were done, since this affects the error
C
        ICORW = IW(KCO+JTPCOF)/10
C
C++  Dip angle OF track and z position of coordinate
C
        TALA = RW(KTK+JTGFTL)
        Z = RW(KCO+JTPCZV)
C
C++  Number of half pads and the pad crossing angle complete the story
C
        NHALF = ITABL(KPCOI,IC,JPCONH)
        CALL TFILPA(KCO,KTK,PADCRO,WIRCRO)
C
C++  Get the error for the pads alone
C
        CALL TERPAR(PADCRO,Z,TALA,NHALF,ICORW,SRPHI,SZ)

C
C++  If not on a track, just copy TPCO with some sanity checks
C
      ELSE
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
      ENDIF
      IPTNC(JPTNSP)=NINT(SRPHI/RW(KPTUN+JPTUSR))
      IPTNC(JPTNSZ)=NINT(SZ/RW(KPTUN+JPTUSZ))
C
      RETURN
      END
#endif
