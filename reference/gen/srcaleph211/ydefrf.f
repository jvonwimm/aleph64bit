      SUBROUTINE YDEFRF(KFRFT0,KFRFT,KFRTL,IFAIL)
C
C----------------------------------------------------------*
C!    DEFINE BANK INDICES FOR FRFT AND FRTL BANKS
CKEY YTOP
C!    Author :     W. Maenner,M.Bosman   09/07/91
C!    Rewritten:   W. Manner             04/03/93
C!
C!    Description
C!    ===========
C!    Define bank indices for banks FRFT and FRTL
C!
C!    In order to cope with recent developments of ALPHA
C!    the logic was changed in the following way:
C!    FRFT nr. 2 exists:  KFRFT = index(nr. 2)
C!    ELSE                KFRFT = 0
C!
C!    FRFT nr. 0 exists:  KFRFT0 = index(nr. 0)
C!    ELSE IF
C!    FRFT nr. 3 exists:  KFRFT0 = index(nr. 3)
C!    ELSE                KFRFT0  = KFRFT
C!    force KFRFT to be equal to KFRFT0 if ITC-TPC
C!        tracks requested, i.e. FRF2 option not called
C!    return with error in case VDET tracks are requested
C!        via the option FRF2 and FRFT bank nr 2 does
C!        not exists
C!
C!---------------------------------------------------------*
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C! YTOP parameters
      COMMON/YPARTO/DHXLIM,CHISEL,PMINSE,PIDACP,BFIELD,
     &       MNTHPV,MXTSPV, PMINRQ,PMINRA,
     &       CHVXCO,CHPTCO,RVACCO,AMCTCO,DZMXCO,NAMXCO,EPLOCO,EPHICO,
     &       CHVXV0,CHPTV0,CHVSV0,CHMLV0,DZMXV0,NAMXV0,
     &       PIPKV0,PRPLV0,PIPLV0,
     &       LBCRFD,LRYOLD,LRFRF2,
     &       LRPVTX,LRSVTX,LVBCR0,LRMVPV,LRLPVX,LCONVS,LVZERS,LRUSER
      LOGICAL LBCRFD,LRYOLD,LRFRF2
      LOGICAL LRPVTX,LRSVTX,LVBCR0,LRMVPV,LRLPVX,LCONVS,LVZERS,LRUSER
C!---------------------------------------------------------*
      DATA ICNER1/0/,ICNER2/0/,ICNER3/0/
C
C
C-- Define the logical unit for printout
C
      LOUT = IW(6)
C
      IFAIL=0
C look if any FRFT bank exists
      KFRFT=IW(NAMIND('FRFT'))
      IF(KFRFT.EQ.0) THEN
        IFAIL=1
        RETURN
      ENDIF
      KFRFT  = NLINK('FRFT',2)
      KFRFT0 = NLINK('FRFT',0)
      IF(KFRFT0.EQ.0) KFRFT0 = NLINK('FRFT',3)
      IF(KFRFT0.EQ.0) KFRFT0 = KFRFT
C
      KFRTL=IW(NAMIND('FRTL'))
      IF(KFRTL.EQ.0) THEN
        IFAIL=1
        RETURN
      ENDIF
  203 NXTI=IW(KFRTL-1)
      IF(NXTI.NE.0) THEN
        KFRTL=NXTI
        GO TO 203
      ENDIF
C
C--- check which track bank was requested
      IF(LRFRF2) THEN
C--- VDET refitted tracks bank FRFT nr 2 requested
C--- check that it exists
        IF(KFRFT.EQ.0) THEN
          IFAIL=1
          RETURN
        ENDIF
      ELSE
C--- ITC-TPC track bank FRFT nr 0 requested
        KFRFT = KFRFT0
      ENDIF
      RETURN
      END
