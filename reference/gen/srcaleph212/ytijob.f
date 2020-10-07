      SUBROUTINE YTIJOB
C
C----------------------------------------------------------*
C!    Initialize the job for TOPOLOGY reconstruction
C!    from JULIA
CKEY YTOP
C!    Author :     M. Bosman    30/11/88
C!    Modified:    G. Lutz        /01/91
C!    Modified:    M. Bosman    12/07/91
C!    Modified  :  G. Lutz   30/03/92
C!    Modified  :  G. Lutz    1/10/92
C!
C!    Description
C!    ===========
C!    This routine is called once at the JOB beginning to
C!    initialize the TOPOLOGY part of the reconstruction
C!    program.
C!    It modifies default options according to YOPT cards
C!    It reads the bank YTPA that contains cuts for the
C!    TOPOLOGY reconstruction program to overwrite
C!    default options
C!    Call YTOIJO that initializes the output banks and
C!    the summary variables
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
      PARAMETER(JYTGDM=1,JYTGMM=2,JYTGPP=3,LYTGPA=3)
      PARAMETER(JYTPMT=1, JYTPNF=2, JYTPMM=3,JYTPMA=4,LYTPAA=4)
      PARAMETER(JYTCVC=1,JYTCPC=2,JYTCRD=3,JYTCMM=4,JYTCZD=5,JYTCNA=6,
     +          JYTCLI=7,JYTCHI=8, LYTCPA=8)
      PARAMETER(JYTVVC=1,JYTVPC=2,JYTVDC=3,JYTVMD=4,JYTVZD=5,JYTVNA=6,
     +          JYTVPI=7,JYTVPR=8,JYTVPJ=9, LYTVPA=9)
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
C! YTOP summary
      COMMON /YSUMTO/NEPVTO,NRPVTO,ATPVTO,AMPVTO,ACPVTO,APPVTO(3),
     +               NRCOTO,AMCOTO,ACCOTO,ARCOTO,
     +               NRK0TO,AMK0TO,ACK0TO,ATK0TO,
     +               NRLATO,AMLATO,ACLATO,ATLATO,
     +               NRLBTO,AMLBTO,ACLBTO,ATLBTO,
     +               KYFLAG(20)
C!---------------------------------------------------------*
      CHARACTER*4 CHAINT
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C!---------------------------------------------------------*
C
C-- Define the logical unit for printout
C
      LOUT = IW(6)
C
C-- Set the defaults for the run options
C
      LRYOLD = .FALSE.
      LRFRF2 = .FALSE.
      LRPVTX = .FALSE.
      LRSVTX = .FALSE.
      LVBCR0 = .FALSE.
      LRMVPV = .FALSE.
      LRLPVX = .FALSE.
      LCONVS = .FALSE.
      LVZERS = .FALSE.
      LRUSER = .FALSE.
C
C-- Read the YOPT cards and modify the defaults accordingly
C
      KYOPT = NLINK('YOPT',0)
      IF(KYOPT.NE.0) THEN
        DO 10 I = 1,IW(KYOPT)
          IF(CHAINT(IW(KYOPT+I)).EQ.'SVTX') THEN
            WRITE(LOUT,999)
            LRSVTX = .TRUE.
          ENDIF
          IF(CHAINT(IW(KYOPT+I)).EQ.'RLEP') THEN
            WRITE(LOUT,998)
            LRLPVX = .TRUE.
          ENDIF
          IF(CHAINT(IW(KYOPT+I)).EQ.'RCON') THEN
            WRITE(LOUT,997)
            LCONVS = .TRUE.
          ENDIF
          IF(CHAINT(IW(KYOPT+I)).EQ.'RV0S') THEN
            WRITE(LOUT,996)
            LVZERS = .TRUE.
          ENDIF
          IF(CHAINT(IW(KYOPT+I)).EQ.'PVTX') THEN
            WRITE(LOUT,995)
            LRPVTX = .TRUE.
          ENDIF
          IF(CHAINT(IW(KYOPT+I)).EQ.'USER') THEN
            WRITE(LOUT,994)
            LRUSER = .TRUE.
          ENDIF
          IF(CHAINT(IW(KYOPT+I)).EQ.'BCRO') THEN
            WRITE(LOUT,993)
            LVBCR0 = .TRUE.
          ENDIF
          IF(CHAINT(IW(KYOPT+I)).EQ.'VHIT') THEN
            WRITE(LOUT,992)
            LRMVPV = .TRUE.
          ENDIF
          IF(CHAINT(IW(KYOPT+I)).EQ.'YOLD') THEN
            WRITE(LOUT,991)
            LRYOLD = .TRUE.
          ENDIF
          IF(CHAINT(IW(KYOPT+I)).EQ.'FRF2') THEN
            WRITE(LOUT,990)
            LRFRF2 = .TRUE.
          ENDIF
   10   CONTINUE
      ELSE
        LRPVTX = .TRUE.
      ENDIF
C
C
C-- LOOK FOR BANK YTGA
C-- if the bank is not present
C-- set defaults values
C
C  GENERAL PARAMETERS FOR YTOP
      KYTGP = IW(NAMIND('YTGP'))
      IF(KYTGP.NE.0)THEN
        DHXLIM = RTABL(KYTGP,1,JYTGDM)
        PMINSE = RTABL(KYTGP,1,JYTGMM)
        PIDACP = RTABL(KYTGP,1,JYTGPP)
C
      ELSE
        WRITE(LOUT,981)
        DHXLIM = 2.
        PMINSE = 0.5
        PIDACP = 0.01
C
      ENDIF
C
C
C-- LOOK FOR BANK YTPP
C-- if the bank is not present
C-- set defaults values
C
C  PARAMETERS FOR PRIMARY VERTEX
      KYTPP = IW(NAMIND('YTPP'))
      IF(KYTPP.NE.0)THEN
        MNTHPV = ITABL(KYTPP,1,JYTPMT)
        MXTSPV = ITABL(KYTPP,1,JYTPNF)
        PMINRQ = RTABL(KYTPP,1,JYTPMM)
        PMINRA = RTABL(KYTPP,1,JYTPMA)
C
      ELSE
        WRITE(LOUT,982)
        MNTHPV = 4
        MXTSPV = 15
        PMINRQ = 0.5
        PMINRA = 0.
C
      ENDIF
C
C-- LOOK FOR BANK YTCP
C-- if the bank is not present
C-- set defaults values
C
C  PARAMETERS FOR CONVERSION SEARCH
      KYTCP = IW(NAMIND('YTCP'))
      IF(KYTCP.NE.0)THEN
        CHVXCO = RTABL(KYTCP,1,JYTCVC)
        CHPTCO = RTABL(KYTCP,1,JYTCPC)
        RVACCO = RTABL(KYTCP,1,JYTCRD)
        AMCTCO = RTABL(KYTCP,1,JYTCMM)
        DZMXCO = RTABL(KYTCP,1,JYTCZD)
        NAMXCO = ITABL(KYTCP,1,JYTCNA)
        EPLOCO = RTABL(KYTCP,1,JYTCLI)
        EPHICO = RTABL(KYTCP,1,JYTCHI)
C
      ELSE
        WRITE(LOUT,983)
        CHVXCO = 10.8
        CHPTCO = 13.8
        RVACCO = 4.
        AMCTCO = 0.3
        DZMXCO = 2.
        NAMXCO = 1.
        EPLOCO = 0.01
        EPHICO = 0.01
C
      ENDIF
C
C-- LOOK FOR BANK YTVP
C-- if the bank is not present
C-- set defaults values
C
C  PARAMETERS FOR VZERO SEARCH
      KYTVP = IW(NAMIND('YTVP'))
      IF(KYTVP.NE.0)THEN
        CHVXV0 = RTABL(KYTVP,1,JYTVVC)
        CHPTV0 = RTABL(KYTVP,1,JYTVPC)
        CHVSV0 = RTABL(KYTVP,1,JYTVDC)
        CHMLV0 = RTABL(KYTVP,1,JYTVMD)
        DZMXV0 = RTABL(KYTVP,1,JYTVZD)
        NAMXV0 = ITABL(KYTVP,1,JYTVNA)
        PIPKV0 = RTABL(KYTVP,1,JYTVPI)
        PRPLV0 = RTABL(KYTVP,1,JYTVPR)
        PIPLV0 = RTABL(KYTVP,1,JYTVPJ)
C
      ELSE
        WRITE(LOUT,984)
        CHVXV0 = 10.8
        CHPTV0 = 13.8
        CHVSV0 = 25.
        CHMLV0 = 25.
        DZMXV0 = 2.
        NAMXV0 = 10
        PIPKV0 = 0.01
        PRPLV0 = 0.01
        PIPLV0 = 0.01
C
      ENDIF
C
      WRITE(LOUT,1991) DHXLIM,PMINSE,PIDACP
 1991 FORMAT(' General parameters for YTOPOL'/
     &       ' DHXLIM =',F8.3,' max.dist. betw trks for start vtx.'/
     &       ' PMINSE =',F8.3,' min.charged trk. mom. for YTOP'/
     &       ' PIDACP =',F8.3,' min.prob. for setting part.id. flag')
      IF(LCONVS) THEN
        WRITE(LOUT,1997) CHVXCO,CHPTCO,RVACCO,AMCTCO,DZMXCO,
     &                       NAMXCO,EPLOCO,EPHICO
 1997   FORMAT(' Parameters for gamma conv. reconstruction'/
     &      ' CHVXCO =',F8.3,' conv. vertex chisq.'/
     &      ' CHPTCO =',F8.3,' chisq pointing (inc.trk. to BCR)'/
     &      ' RVACCO =',F8.3,' min. rad. dist. of vtx from beam line'/
     &      ' AMCTCO =',F8.3,' max. invariant mass'/
     &      ' DZMXCO =',F8.3,' max. z-dist. of track from BCR'/
     &      ' NAMXCO =',I8,' max. # of add. trks through vertex'/
     &      ' EPLOCO =',F8.3,' min.electron prob. for both tracks'/
     &      ' EPHICO =',F8.3,' min.electron prob. for at least 1 trk.')
      ENDIF
      IF(LVZERS) THEN
        WRITE(LOUT,1996) CHVXV0,CHPTV0,CHVSV0,CHMLV0,DZMXV0,
     &                       NAMXV0,PIPKV0,PRPLV0,PIPLV0
 1996   FORMAT(' Parameters for V0 reconstruction'/
     &      ' CHVXV0 =',F8.3,' V0 vertex chisq'/
     &      ' CHPTV0 =',F8.3,' chisq pointing (inc.tr. to BCR)'/
     &      ' CHVSV0 =',F8.3,' chisq vertex separation'/
     &      ' CHMLV0 =',F8.3,' chisq mass deviation'/
     &      ' DZMXV0 =',F8.3,' max. z-dev. from BCR for tracks'/
     &      ' NAMXV0 =',I8  ,' max. # of add. trks throug V0 vtx.'/
     &      ' PIPKV0 =',F8.3,' min. pion prob. of K0 decay tracks'/
     &      ' PRPLV0 =',F8.3,' min. proton prob. in lambda dec.'/
     &      ' PIPLV0 =',F8.3,' min. pion prob. in lambda decay')
      ENDIF
      IF(LRPVTX) THEN
        WRITE(LOUT,1995) MNTHPV,MXTSPV,PMINRQ,PMINRA
 1995   FORMAT(' Parameters for primary vertex reconstruction'/
     &             ' MNTHPV = ',I8,' min. TPC hits on track'/
     &             ' MXTSPV = ',I8,' max. # of tracks in first step'/
     &             ' PMINRQ = ',F8.3,' min. momentum in first step'/
     &             ' PMINRA = ',F8.3,' min. mom. in second step')
      ENDIF
C
C
C-- Initialize formats for vertex banks
C
      CALL BKFMT('PYER','2I,(I,10F,I)')
      CALL BKFMT('PYFR','2I,(2I)')
      CALL BKFMT('YNFT','2I,(21F,7I,1F)')
      CALL BKFMT('YNMA','2I,(2I,2F,3F,1I,2F)')
      CALL BKFMT('YNPE','2I,(3F)')
      CALL BKFMT('YNTR','2I,(5I,1F,1I)')
      CALL BKFMT('YCMA','2I,(2I,2F)')
      CALL BKFMT('YCPE','2I,(3F)')
      CALL BKFMT('YCTR','2I,(4I)')
      CALL BLIST(IW,'E+','PYERPYFRYNFTYNMAYNPEYNTRYCFTYCMAYCPEYCTR')
C
C
C-- Initialize the summary variables
C
      NEPVTO = 0
      NRPVTO = 0
      ATPVTO = 0.
      AMPVTO = 0.
      ACPVTO = 0.
      DO 20 I =1,3
        APPVTO(I) = 0.
   20 CONTINUE
      NRCOTO=0
      AMCOTO=0
      ACCOTO=0
      ARCOTO=0
      NRK0TO=0
      AMK0TO=0
      ACK0TO=0
      ATK0TO=0
      NRLATO=0
      AMLATO=0
      ACLATO=0
      ATLATO=0
      NRLBTO=0
      AMLBTO=0
      ACLBTO=0
      ATLBTO=0
      DO 30 I =1,20
        KYFLAG(I) = 0
   30 CONTINUE
C
C --  call YTIJOB
C
C     BOOK USER HISTOS
      IF(LRUSER) CALL YHUSER
C
      RETURN
  999 FORMAT(' YTIJOB (initialization YTOPOL) :',
     +  ' The option SVTX is called')
  998 FORMAT(' YTIJOB (initialization YTOPOL) :',
     +  ' The option RLEP is called')
  997 FORMAT(' YTIJOB (initialization YTOPOL) :',
     +  ' The option RCON is called')
  996 FORMAT(' YTIJOB (initialization YTOPOL) :',
     +  ' The option RV0S is called')
  995 FORMAT(' YTIJOB (initialization YTOPOL) :',
     +  ' The option PVTX is called')
  994 FORMAT(' YTIJOB (initialization YTOPOL) :',
     +  ' The option USER is called')
  993 FORMAT(' YTIJOB (initialization YTOPOL) :',
     +  ' The option BCRO is called')
  992 FORMAT(' YTIJOB (initialization YTOPOL) :',
     +  ' The option VHIT is called')
  991 FORMAT(' YTIJOB (initialization YTOPOL) :',
     +  ' The option YOLD is called')
  990 FORMAT(' YTIJOB (initialization YTOPOL) :',
     +  ' The option FRF2 is called')
  981 FORMAT(' YTIJOB (initialization YTOPOL) :',
     +  ' YTGP  bank not found :use default values for YTOP parameters')
  982 FORMAT(' YTIJOB (initialization YTOPOL) :',
     +  ' YTPP  bank not found :use default values for YTOP parameters')
  983 FORMAT(' YTIJOB (initialization YTOPOL) :',
     +  ' YTCP  bank not found :use default values for YTOP parameters')
  984 FORMAT(' YTIJOB (initialization YTOPOL) :',
     +  ' YTVP  bank not found :use default values for YTOP parameters')
      END
