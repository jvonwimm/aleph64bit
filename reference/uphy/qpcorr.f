      FUNCTION QPCORR(ITRK,ASCALE,NVDET,ERCORR)
C-----------------------------------------------------------------------
C! Correct particle momenta for effects of residual distortions in
C! the central tracking detector.
C!   This is the so called "sagitta correction".
C! It depends upon cos(theta) and the year of data taking.
C!
C!    Author   :  I.R. Tomalin
C!                Based on subroutine QFBDEV written by I. ten Have.
C!    Date     :  10-3-1994
C!
C! Motivation:
C!   Every year, even after the detector alignment is finished and
C! corrections have been made for field distortions etc., it is found
C! that Ebeam/P in Z0 -> mu+mu- events is not precisely 1, presumably
C! because of residual distortions.
C!   (Typically, in the region |cos(theta)| > 0.9, Ebeam/p is about 0.94
C! for +ve tracks and 1.06 for -ve tracks. Elsewhere, Ebeam/p is usually
C! consistent with 1 to within a percent or so. The effect is not quite
C! forward-backward symmetric).
C!   The effect is expected to be proportional to P**2, so most people
C! analysing hadronic events can ignore it. Exceptions include analyses
C! using the ECAL electron identifiers in the region |cos(theta)| > 0.9,
C! or analyses which are very sensitive to systematic biases in the
C! momenta (e.g. jet charge, tau polarization).
C!   This routine provides a correction for the momenta based upon
C! Ebeam/P measurements in Z0 -> mu+mu- events. It assumes that the
C! corrections for -ve and +ve particles are equal in size, but of
C! opposite sign. This is observed to be true, apart from a constant
C! offset, Ebeam/p = 1.002, which is also present in the MC and so not
C! corrected for.
C!   In principle, the correction depends upon your track selection
C! cuts, but providing that the corrections have a small effect on your
C! analysis, you can ignore this. Arguement NVDET does correct for this
C! to first approximation however.
C!   For those needing details: the Z0 -> mu+mu- events analysed used
C! FRFT 2 tracks (except in 1990 when FRFT 0 was used), but did not
C! require VDET runs in SCANBOOK. Only tracks with > 4 ITC and
C! > 5 TPC coords. were used.
C!
C! Input Arguments:
C!    ITRK    (Integer): ALPHA track number.
C!    ASCALE  (Logical): Set .TRUE. if ALPHA variables like QX,QY,QZ
C!                       are to be rescaled by calling QVSCAL.
C!                       N.B. Don't call this routine twice for the
C!                       same track with ASCALE = .TRUE. !
C!    NVDET   (Integer): If your analysis only uses tracks with at
C!                       least 1 VDET hit, set NVDET=1; otherwise =0.
C! Output Arguments:
C!    QPCORR  (Real)   : Scale factor applied/to be applied to momentum.
C!    ERCORR  (Real)   : Statistical error on this factor.
C!
C! If this routine is not up to date, please contact the tracking group.
C-----------------------------------------------------------------------
      LOGICAL ASCALE
C
      LOGICAL DEBUG
      DATA DEBUG/.TRUE./
      PARAMETER(NBIN=20,BSIZE=0.1)
      DIMENSION C90_V0(NBIN),E90_V0(NBIN),C90_V1(NBIN),E90_V1(NBIN),
     +          C91_V0(NBIN),E91_V0(NBIN),C91_V1(NBIN),E91_V1(NBIN),
     +          C92_V0(NBIN),E92_V0(NBIN),C92_V1(NBIN),E92_V1(NBIN),
     +          C93_V0(NBIN),E93_V0(NBIN),C93_V1(NBIN),E93_V1(NBIN),
     +          C94_V0(NBIN),E94_V0(NBIN),C94_V1(NBIN),E94_V1(NBIN)
C
C Min. and Max. years available.
      PARAMETER(MINYER=90,MAXYER=94)
C
C-------------- Corrections using a bin size of 0.10 --------------------
C 1994 corrections with no VDET hit requirement.
      DATA C94_V0/
     + -0.00118,-0.00027,-0.00005,-0.00001,-0.00001, 0.00002,
     +  0.00007, 0.00011, 0.00021, 0.00023,-0.00024,-0.00028,
     + -0.00020,-0.00011,-0.00002, 0.00008, 0.00013, 0.00012,
     +  0.00002,-0.00042/
      DATA E94_V0/
     +  0.00005, 0.00001, 0.00001, 0.00001, 0.00001, 0.00001,
     +  0.00001, 0.00001, 0.00001, 0.00001, 0.00001, 0.00001,
     +  0.00001, 0.00001, 0.00001, 0.00001, 0.00001, 0.00001,
     +  0.00001, 0.00004/
C 1994 corrections for >= 1 VDET hit requirement.
      DATA C94_V1/
     + -0.00008,-0.00008,-0.00007,-0.00001,-0.00001, 0.00003,
     +  0.00008, 0.00011, 0.00022, 0.00023,-0.00023,-0.00028,
     + -0.00020,-0.00011,-0.00001, 0.00008, 0.00012, 0.00011,
     + -0.00004,-0.00004/
      DATA E94_V1/
     +  0.00001, 0.00001, 0.00001, 0.00001, 0.00001, 0.00001,
     +  0.00001, 0.00001, 0.00001, 0.00001, 0.00001, 0.00001,
     +  0.00001, 0.00001, 0.00001, 0.00001, 0.00001, 0.00001,
     +  0.00001, 0.00001/
C
C 1993 corrections with no VDET hit requirement.
      DATA C93_V0/
     + -0.00159,-0.00037,-0.00007,-0.00006,-0.00002,-0.00003,
     +  0.00004, 0.00011, 0.00018, 0.00026,-0.00003,-0.00005,
     + -0.00002, 0.00000, 0.00001, 0.00006, 0.00004, 0.00001,
     + -0.00021,-0.00099/
      DATA E93_V0/
     +  0.00008, 0.00002, 0.00001, 0.00001, 0.00002, 0.00002,
     +  0.00002, 0.00002, 0.00003, 0.00003, 0.00002, 0.00002,
     +  0.00002, 0.00002, 0.00002, 0.00002, 0.00001, 0.00001,
     +  0.00002, 0.00007/
C 1993 corrections for >= 1 VDET hit requirement.
      DATA C93_V1/
     + -0.00006,-0.00006,-0.00008,-0.00006,-0.00003,-0.00002,
     +  0.00004, 0.00011, 0.00019, 0.00027,-0.00002,-0.00005,
     + -0.00001, 0.00000, 0.00001, 0.00005, 0.00005, 0.00001,
     + -0.00004,-0.00004/
      DATA E93_V1/
     +  0.00002, 0.00002, 0.00001, 0.00001, 0.00001, 0.00002,
     +  0.00002, 0.00002, 0.00003, 0.00003, 0.00002, 0.00002,
     +  0.00002, 0.00002, 0.00002, 0.00001, 0.00001, 0.00001,
     +  0.00002, 0.00002/
C
C 1992 corrections with no VDET hit requirement.
      DATA C92_V0/
     + -0.00160,-0.00030,-0.00007,-0.00009,-0.00006,-0.00006,
     + -0.00007, 0.00000, 0.00007, 0.00014, 0.00004,-0.00003,
     + -0.00016,-0.00001, 0.00002, 0.00002, 0.00001,-0.00001,
     + -0.00030,-0.00119/
      DATA E92_V0/
     +  0.00008, 0.00002, 0.00001, 0.00001, 0.00002, 0.00002,
     +  0.00002, 0.00002, 0.00003, 0.00003, 0.00003, 0.00002,
     +  0.00002, 0.00002, 0.00002, 0.00002, 0.00001, 0.00001,
     +  0.00002, 0.00006/
C 1992 corrections for >= 1 VDET hit requirement.
      DATA C92_V1/
     + -0.00002,-0.00002,-0.00008,-0.00009,-0.00007,-0.00006,
     + -0.00007, 0.00001, 0.00008, 0.00013, 0.00003,-0.00002,
     + -0.00015,-0.00001, 0.00003, 0.00002, 0.00001, 0.00000,
     + -0.00012,-0.00012/
      DATA E92_V1/
     +  0.00002, 0.00002, 0.00001, 0.00001, 0.00002, 0.00002,
     +  0.00002, 0.00002, 0.00002, 0.00003, 0.00003, 0.00002,
     +  0.00002, 0.00002, 0.00002, 0.00002, 0.00001, 0.00001,
     +  0.00002, 0.00002/
C
C 1991 corrections with no VDET hit requirement.
      DATA C91_V0/
     + -0.00108,-0.00025,-0.00006,-0.00001,-0.00003, 0.00001,
     +  0.00003, 0.00011, 0.00005, 0.00027,-0.00004,-0.00010,
     + -0.00009,-0.00004, 0.00005, 0.00001, 0.00002,-0.00004,
     + -0.00031,-0.00104/
      DATA E91_V0/
     +  0.00011, 0.00003, 0.00002, 0.00002, 0.00002, 0.00003,
     +  0.00003, 0.00003, 0.00004, 0.00004, 0.00004, 0.00003,
     +  0.00003, 0.00003, 0.00003, 0.00002, 0.00002, 0.00002,
     +  0.00004, 0.00009/
C 1991 corrections for >= 1 VDET hit requirement.
      DATA C91_V1/
     + -0.00006,-0.00006,-0.00002,-0.00002,-0.00005, 0.00001,
     +  0.00002, 0.00011, 0.00005, 0.00027,-0.00002,-0.00010,
     + -0.00009,-0.00002, 0.00005, 0.00000, 0.00001,-0.00004,
     + -0.00008,-0.00008/
      DATA E91_V1/
     +  0.00004, 0.00004, 0.00002, 0.00002, 0.00002, 0.00003,
     +  0.00003, 0.00003, 0.00004, 0.00004, 0.00004, 0.00003,
     +  0.00003, 0.00003, 0.00003, 0.00002, 0.00002, 0.00002,
     +  0.00004, 0.00004/
C
C 1990 corrections with no VDET hit requirement.
      DATA C90_V0/
     + -0.00096,-0.00032,-0.00013, 0.00004,-0.00022,-0.00006,
     +  0.00010, 0.00013, 0.00019, 0.00031, 0.00017, 0.00004,
     + -0.00006, 0.00000,-0.00014, 0.00000,-0.00009, 0.00000,
     + -0.00026,-0.00150/
      DATA E90_V0/
     +  0.00016, 0.00005, 0.00003, 0.00005, 0.00006, 0.00005,
     +  0.00006, 0.00005, 0.00006, 0.00007, 0.00006, 0.00006,
     +  0.00005, 0.00006, 0.00005, 0.00004, 0.00003, 0.00003,
     +  0.00005, 0.00015/
C 1990 corrections for >= 1 VDET hit requirement ?
C     (Assumed to be the same as requiring no VDET hits).
      DATA C90_V1/
     + -0.00096,-0.00032,-0.00013, 0.00004,-0.00022,-0.00006,
     +  0.00010, 0.00013, 0.00019, 0.00031, 0.00017, 0.00004,
     + -0.00006, 0.00000,-0.00014, 0.00000,-0.00009, 0.00000,
     + -0.00026,-0.00150/
      DATA E90_V1/
     +  0.00016, 0.00005, 0.00003, 0.00005, 0.00006, 0.00005,
     +  0.00006, 0.00005, 0.00006, 0.00007, 0.00006, 0.00006,
     +  0.00005, 0.00006, 0.00005, 0.00004, 0.00003, 0.00003,
     +  0.00005, 0.00015/
C------------------------------------------------------------------------
C
      DIMENSION PCORR(NBIN,MINYER:MAXYER,0:1),
     +          ECORR(NBIN,MINYER:MAXYER,0:1)
C
      EQUIVALENCE
     +  (PCORR(1,90,0),C90_V0(1)),(PCORR(1,90,1),C90_V1(1)),
     +  (PCORR(1,91,0),C91_V0(1)),(PCORR(1,91,1),C91_V1(1)),
     +  (PCORR(1,92,0),C92_V0(1)),(PCORR(1,92,1),C92_V1(1)),
     +  (PCORR(1,93,0),C93_V0(1)),(PCORR(1,93,1),C93_V1(1)),
     +  (PCORR(1,94,0),C94_V0(1)),(PCORR(1,94,1),C94_V1(1))
C
      EQUIVALENCE
     +  (ECORR(1,90,0),E90_V0(1)),(ECORR(1,90,1),E90_V1(1)),
     +  (ECORR(1,91,0),E91_V0(1)),(ECORR(1,91,1),E91_V1(1)),
     +  (ECORR(1,92,0),E92_V0(1)),(ECORR(1,92,1),E92_V1(1)),
     +  (ECORR(1,93,0),E93_V0(1)),(ECORR(1,93,1),E93_V1(1)),
     +  (ECORR(1,94,0),E94_V0(1)),(ECORR(1,94,1),E94_V1(1))
C
      INCLUDE '/aleph/phy/qcde.inc'
      INCLUDE '/aleph/phy/qmacro.inc'
C-----------------------------------------------------------------------
      QPCORR = 1.0
      ERCORR = 0.0
C Do nothing if this is Monte Carlo.
      IF (XMCEV) GOTO 999
C
C Find out which year it is.
      IDATE = KEVEDA
      IYEAR = IDATE/10000
      IF (IYEAR.LT.MINYER.OR.IYEAR.GT.MAXYER) THEN
        IF (DEBUG) WRITE(6,10) IYEAR
   10   FORMAT(/,' WARNING: Year ',I2,' not available in QPCORR. ',
     +  'Will take nearest available year.')
        IYEAR = MAX(IYEAR,MINYER)
        IYEAR = MIN(IYEAR,MAXYER)
        DEBUG = .FALSE.
      END IF
C
C Find out which cos(theta) bin this track lies in.
      IBIN = 1 + INT((1.0 + QCT(ITRK))/BSIZE)
      IBIN = MIN(IBIN,NBIN)
C
      NVD = MIN(NVDET,1)

C Now find scale factor for momentum.
      IF (KCH(ITRK).GT.0) THEN
        QPCORR = 1.0 + PCORR(IBIN,IYEAR,NVD)*QP(ITRK)
      ELSE
        QPCORR = 1.0 - PCORR(IBIN,IYEAR,NVD)*QP(ITRK)
      END IF
      ERCORR = ECORR(IBIN,IYEAR,NVD)*QP(ITRK)
C
C Scale ALPHA momentum if required.
      IF (ASCALE) CALL QVSCAL(ITRK,QPCORR)
C
  999 CONTINUE
      END
