      SUBROUTINE VTFILL (ITK,ICOMB)
C----------------------------------------------------------------------
C! Fill VDCO and VCPL  banks.
CKEY VDET TRACK
C
C  Author      : B. Mours   901001
C  modified by : TSM        910918
C     track ambiguity
C  modified by : B. Mours   911023
C     dont store in VCPL rejected hit (large error)
C  modified by : A. Bonissent 950714
C     new logic for multitrack assignment for new Vdet only
C     Old Vdet unchanged
C
C  input : ITK   = track number
C          ICOMB = combinaison number in VTMA bank
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "vdcojj.h"
#include "vdxyjj.h"
#include "vdztjj.h"
#include "vdmrjj.h"
#include "vdgcjj.h"
#include "vtxtjj.h"
#include "vtmajj.h"
#include "vcpljj.h"
#include "vtbos.h"
#include "vtkrec.h"
#include "vtrpar.h"
#include "bcs.h"
      INTEGER NFRFT,NVDCO,KFRFT,KVDCO,NVTMA,KVTMA,
     +        JVTMA,IL,IVDCO,JVDCO,JVCPL,KVDXY,ICL,KVDZT
      REAL    SIGMU,SIGMZ
      LOGICAL FIRST
C-- Local variables for ambiguous pattern search
      LOGICAL WAMBIG,UAMBIG,WFOUND,UFOUND
      INTEGER JL,IWFW,IWFU,ICLW,ICLU
C   NRR = NR for appropriate view (view included)
      INTEGER NRR(2),IHIT(2),NASS(2)
      INTEGER VDYEAR
      REAL CBAUG,CPAUG
C -- bit 2 (IVPAMB=4) is set in the VDCO quality flag to indicate
C    a R-Phi ambiguous hit.
C    bit 3 (IVZAMB=8) is set in the VDCO quality flag to indicate
C    a Z ambiguous hit.
      INTEGER IVPAMB,IVZAMB
      PARAMETER (IVPAMB=4, IVZAMB=8)
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        NVDCO = NAMIND('VDCO')
        NVTMA = NAMIND('VTMA')
        NAVDGC=NAMIND('VDGC')
      ENDIF
C
      KVTMA = IW(NVTMA)
      IF(KVTMA.EQ.0)        GO TO 999
      IF(LROWS(KVTMA).EQ.0) GO TO 999
C
      KVCPL = NLINK('VCPL',ITK)
      KVDCO = IW(NVDCO)
C
C-- loop over all clusters found for this track
C
      JVTMA = KROW(KVTMA,ICOMB)
      IF(IW(JVTMA+JVTMNL).GT.NLYRMX) THEN
          IERVTR = 12
          GO TO 999
      ENDIF
C
C-- Check other VTMA patterns for ambiguity
C   Ambiguity means the AUGMENTED chisquare of another pattern
C   (which includes a penalty for using fewer hits)
C   is not significantly more than the AUGMENTED chisquare
C   of the "best" pattern
C   (significantly means more than CH2AMB units larger)
C
C-- Initialize flags
      WAMBIG=.FALSE.
      UAMBIG=.FALSE.
C-- Get "best" augmented chisquare
      CBAUG=RW(KROW(KVTMA,ICOMB)+JVTMFR)
C-- Loop over patterns
      DO 70 JCOMB = 1, LROWS(KVTMA)
C-- Save time
        IF (WAMBIG .AND. UAMBIG) GO TO 70
C-- Don't compare pattern to itself
        IF (JCOMB .EQ. ICOMB) GO TO 70
C-- Get augmented chisquare of this pattern
        CPAUG=RW(KROW(KVTMA,JCOMB)+JVTMFR)
C-- Compare
        IF (CPAUG .GT. CBAUG+CH2AMB) GO TO 70
C-- We have found an ambiguous pattern
C-- The following stuff is only necessary to determine if the ambiguity
C--   is in phi, z, or both
C-- Loop over layers in ICOMB
        DO 60 IL = 1, IW(JVTMA+JVTMNL)
C-- Save time
          IF (WAMBIG .AND. UAMBIG) GO TO 60
C-- Find wafer and cluster numbers
          IWFW=IW(JVTMA+IL-1+JVTMWW)
          ICLW=IW(JVTMA+IL-1+JVTMIW)
          IWFU=IW(JVTMA+IL-1+JVTMUW)
          ICLU=IW(JVTMA+IL-1+JVTMIU)
C-- Initialize flags for layer hit search
          WFOUND=.FALSE.
          UFOUND=.FALSE.
C-- Loop over layers in JCOMB
          DO 50 JL = 1, IW(KROW(KVTMA,JCOMB)+JVTMNL)
C-- Save time
            IF (.NOT. WFOUND) THEN
C-- Check if same W wafer
              IF (IW(KROW(KVTMA,JCOMB)+JL-1+JVTMWW) .EQ. IWFW) THEN
C-- Set flag if cluster number matches or alternate is null hit
                IF (IW(KROW(KVTMA,JCOMB)+JL-1+JVTMIW) .EQ. ICLW .OR.
     >                 RW(KROW(KVTMA,JCOMB)+JL-1+JVTMSW).GT.HBIGER) THEN
                  WFOUND=.TRUE.
                ENDIF
              ENDIF
            ENDIF
C-- Same for U hit
            IF (.NOT. UFOUND) THEN
              IF (IW(KROW(KVTMA,JCOMB)+JL-1+JVTMUW) .EQ. IWFU) THEN
                IF (IW(KROW(KVTMA,JCOMB)+JL-1+JVTMIU) .EQ. ICLU .OR.
     >                 RW(KROW(KVTMA,JCOMB)+JL-1+JVTMSU).GT.HBIGER) THEN
                  UFOUND=.TRUE.
                ENDIF
              ENDIF
            ENDIF
C-- End loop over JCOMB layers
   50     CONTINUE
C-- Update ambiguity flags
          UAMBIG=UAMBIG .OR. .NOT. UFOUND
          WAMBIG=WAMBIG .OR. .NOT. WFOUND
C       (i.e. Ambiguous if the hit on the best track was NOT
C               present on the 2nd best track)
C
C-- End loop over ICOMB layers
   60   CONTINUE
C-- End loop over JCOMB=other VTMA patterns
   70 CONTINUE
C-- WAMBIG and UAMBIG now contain the ambiguity information
      NARCVD = 0
C
      DO 100 IL=1,IW(JVTMA+JVTMNL)
        SIGMU = RW(JVTMA+JVTMSU+IL-1)
        SIGMZ = RW(JVTMA+JVTMSW+IL-1)
        IF(SIGMU.GT.HBIGER .AND. SIGMZ.GT.HBIGER)  GO TO 100
        NARCVD = NARCVD + 1
        IF(NARCVD.EQ.1) IOFVCL = LROWS(KVDCO)
C
C-- fill VDCO bank
C
        JVDCO = KNEXT(KVDCO)
        IW(JVDCO+JVDCWI) = IW(JVTMA+JVTMWW+IL-1)
        RW(JVDCO+JVDCR0) = RW(JVTMA+JVTMR0+IL-1)
        RW(JVDCO+JVDCPH) = RW(JVTMA+JVTMPH+IL-1)
        RW(JVDCO+JVDCZ0) = RW(JVTMA+JVTMZ0+IL-1)
        RW(JVDCO+JVDCSR) = SIGMU
        RW(JVDCO+JVDCSZ) = SIGMZ
        IW(JVDCO+JVDCQF) = 3
        IF(SIGMU.GT.HBIGER) IW(JVDCO+JVDCQF) = IW(JVDCO+JVDCQF) - 1
        IF(SIGMZ.GT.HBIGER) IW(JVDCO+JVDCQF) = IW(JVDCO+JVDCQF) - 2
C-- Set ambiguity bits
        IF (UAMBIG) IW(JVDCO+JVDCQF)=IOR(IW(JVDCO+JVDCQF),IVPAMB)
        IF (WAMBIG) IW(JVDCO+JVDCQF)=IOR(IW(JVDCO+JVDCQF),IVZAMB)
        IW(JVDCO+JVDCTN) = ITK
        IW(KVDCO+LMHROW) = LROWS(KVDCO) + 1
C
C-- fill the VCPL bank
C
        JVCPL = KNEXT(KVCPL)
        IW(JVCPL+JVCPXB) = IW(JVTMA+JVTMUW+IL-1)
        IW(JVCPL+JVCPZB) = IW(JVTMA+JVTMWW+IL-1)
        IF(SIGMU.LT.HBIGER) IW(JVCPL+JVCPNX) = IW(JVTMA+JVTMIU+IL-1)
        IF(SIGMZ.LT.HBIGER) IW(JVCPL+JVCPNZ) = IW(JVTMA+JVTMIW+IL-1)
        RW(JVCPL+JVCPC2) = 0.
        IW(KVCPL+LMHROW) = LROWS(KVCPL) + 1
        IF(VDYEAR().NE.95)THEN
C
C-- update Nass in VDXT and VDZT
C
          KVDXY = NLINK('VDXY',IW(JVCPL+JVCPXB))
          IF(KVDXY.NE.0) THEN
            ICL = IW(JVCPL+JVCPNX)
            IHIT(2)=ICL
            IF(ICL.NE.0) IW(KROW(KVDXY,ICL)+JVDXNA) =
     +                 IW(KROW(KVDXY,ICL)+JVDXNA) + 1
          ENDIF
          KVDZT = NLINK('VDZT',IW(JVCPL+JVCPZB))
          IF(KVDZT.NE.0) THEN
            ICL = IW(JVCPL+JVCPNZ)
            IHIT(1)=ICL
            IF(ICL.NE.0) IW(KROW(KVDZT,ICL)+JVDZNA) =
     +                 IW(KROW(KVDZT,ICL)+JVDZNA) + 1
          ENDIF
C
        ELSE
C
C For Vdet95, update number of associated tracks at both levels
C
C
C     pulseheight correction factor for inclined tracks
C
          KVTXT = NLINK('VTXT',ITK)
          PTOT = SQRT( RTABL(KVTXT,IL,JVTXPV)**2 +
     +                 RTABL(KVTXT,IL,JVTXPU)**2 +
     +                 RTABL(KVTXT,IL,JVTXPW)**2 )
          PCOR = ABS(RTABL(KVTXT,IL,JVTXPV)) / PTOT
C
C Access the hit banks and get info
C
          KVDXY = NLINK('VDXY',IW(JVCPL+JVCPXB))
          NRR(2)=IW(JVCPL+JVCPXB)+1
          IHIT(2)=0
          IF(KVDXY.NE.0) THEN
            ICL = IW(JVCPL+JVCPNX)
            IHIT(2)=ICL
            IF(ICL.NE.0)NASS(2)=ITABL(KVDXY,ICL,JVDXNA)
          ENDIF
          KVDZT = NLINK('VDZT',IW(JVCPL+JVCPZB))
          NRR(1)=IW(JVCPL+JVCPZB)
          IHIT(1)=0
          IF(KVDZT.NE.0) THEN
            ICL = IW(JVCPL+JVCPNZ)
            IHIT(1)=ICL
            IF(ICL.NE.0)NASS(1)=ITABL(KVDZT,ICL,JVDZNA)
          ENDIF
C
C Now update nb of tracks
C The algorithm is (for local or global cluster) :
C      if 0 track associated -> 1 if pulseheight compatible only with 1
C                              -1 if large pulseheight, and a
C                                  second track can be associated later
C      else -> NASS=2
C Never more than 2 tracks will be associated
C
          KVDGC=IW(NAVDGC)
          DO IV=1,2
            IF(IV.EQ.1)THEN
              KBNK=KVDZT
              JNAS=JVDZNA
              JPULS=JVDZPH
            ELSE
              KBNK=KVDXY
              JNAS=JVDXNA
              JPULS=JVDXPH
            ENDIF
            ICL=IHIT(IV)
            IF(ICL.NE.0)THEN
              PULS=RTABL(KBNK,ICL,JPULS)*PCOR
              IF(NASS(IV).EQ.0)THEN
                NASS(IV)=1
                IF(PULS.GT.PULMIN)NASS(IV)=-1
              ELSE
                NASS(IV)=IABS(NASS(IV))+1
              ENDIF
              IW(KROW(KBNK,ICL)+JNAS)=NASS(IV)
C Find the relation to global cluster
              KVDMR=NLINK('VDMR',NRR(IV))
              IVDGC=ITABL(KVDMR,ICL,JVDMVD)
              PULS=RTABL(KVDGC,IVDGC,JVDGPH)*PCOR
              NAS =ITABL(KVDGC,IVDGC,JVDGNA)
              IF(NAS.EQ.0)THEN
                NAS=1
                IF(PULS.GT.PULMIN)NAS=-1
              ELSE
                NAS=IABS(NAS)+1
              ENDIF
              IW(KROW(KVDGC,IVDGC)+JVDGNA)=NAS
            ENDIF
          ENDDO
        ENDIF
  100   CONTINUE
C
  999 RETURN
      END
#endif
