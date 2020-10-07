C#######################################################################
C
      
      SUBROUTINE DVVXJ
C=====================================
C
C   initialize things needed for vertex-fitting routines....
C
C=================================================
C
      INCLUDE 'DALINEWSRC:DALI_CF.INC'
      INCLUDE 'A_BCS.INC'
C
*CD YPARTO          YTOP / SYST
      COMMON/YPARTO/DHXLIM,CHISEL,PMINSE,PIDACP,BFIELD,
     &       MNTHPV,MXTSPV, PMINRQ,PMINRA,
     &       CHVXCO,CHPTCO,RVACCO,AMCTCO,DZMXCO,NAMXCO,EPLOCO,EPHICO,
     &       CHVXV0,CHPTV0,CHVSV0,CHMLV0,DZMXV0,NAMXV0,
     &       PIPKV0,PRPLV0,PIPLV0,
     &       LBCRFD,LRYOLD,LRFRF2,
     &       LRPVTX,LRSVTX,LVBCR0,LRMVPV,LRLPVX,LCONVS,LVZERS,LRUSER
      LOGICAL LBCRFD,LRYOLD,LRFRF2
      LOGICAL LRPVTX,LRSVTX,LVBCR0,LRMVPV,LRLPVX,LCONVS,LVZERS,LRUSER
c--please check ALEPHLIB for documentation...
C
      CHARACTER*5 DT5
      DATA IDEB/0/
C  ....................................
c
      IF(IDEB.EQ.1)PRINT *,' U1: initialize things.....'
C
C   initialization for beam-spot fit:
C
c       call setting magnetfield to common deck YPARTO, IRUNDE(2)=RUN#
        CALL YTOIRU(IRUNDE(2),15.0)
C
C   initialization for secondary vertex fit:
C
C       LRYNEW = .TRUE.
        LRFRF2 = .TRUE.
        LRPVTX = .TRUE.
        LRSVTX = .TRUE.
        LVBCR0 = .TRUE.
        LRLPVX = .FALSE.
        LRMVPV = .TRUE.
        LCONVS = .TRUE.
        LVZERS = .TRUE.
        LRUSER = .FALSE.
        IF (IDEB.EQ.1)
     &  call DWRT(' PMINRQ='//DT5(PMINRQ)//',PMINRA='//DT5(PMINRA)//
     &             ',DHXLIM='//DT5(DHXLIM)//',CHISEL='//DT5(CHISEL))
        IF (IDEB.EQ.1)
     &  call DWRT(' PMINSE='//DT5(PMINSE)//',PIDACP='//DT5(PIDACP)//
     &             ',BFIELD='//DT5(BFIELD))
C
C       initialize YTOPOL-routines.....
C
        CALL YTIJOB
        CALL YTOPNW
C
      RETURN
      END
C
C#######################################################################
*DK DVBPCA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVBPCA
CH
      SUBROUTINE DVBPCA(NRBP)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C
C   Author    :- Gabi & Co.            January 1993
C
C   Purpose   : better beam-spot-position using ALPHA-routine QFGET_BP
C   Inputs    :
C   Outputs   : NRBP = row# in PYER
C   SRs called: ALPHA:QFGET_BP
C   Data IN   :
C   Data out  : BOS-bank PYER #0
C
C============================================
C
      INCLUDE 'DALINEWSRC:DALI_CF.INC'
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_PYERJJ.INC'
C
      INTEGER NRBP
C
      INTEGER IBNK0/0/
      INTEGER NPYER,NRWS,IRW
C
      INTEGER INIT/0/
      CHARACTER*80 BPBB
C
      INTEGER BPFLAG/0/
      REAL VTX(3),VTERR(3),VTSIZ(3)
      DIMENSION VRBCR(6)
C
      INTEGER IBTYP,NVDOF,NTRK,ITRKS(1)
      REAL CHVX2
      INTEGER IER,LGARB
C
      CHARACTER*3 DT3
C
      PARAMETER (NPT=61)
      DATA IDEB/0/
C
      INCLUDE 'A_BMACRO.INC'
C
C  ....................................
C
c  init....
      IF (INIT.EQ.0) THEN
        CALL DWRT('please wait - read bank BPBB once from cards...')
        INIT=1
        CALL DW_GET_PLATFORM_TEXT('BP',BPBB,80)
C            ...... DATA BPBB/'PHY:BEAM.POSITION'/
C       call BOS-read-cards-file
        CALL AOPEN(32,BPBB,'CARDS',' ',IER)
        REWIND(32)
        CALL MRESET(32)
        IF (IER.NE.0) THEN
          CALL DWRT(' problems reading file '//bpbb//' !!!')
        ELSE
          IW(4)=0
          CALL BREADC
          IW(4)=1
          IF(IDEB.EQ.1)print *,' BPBB read successfully...'
        END IF
        CLOSE(32)
      ENDIF
C
        NTRK=0
        ITRKS(1)=0
        CALL UZERO(VRBCR,1,6)
C
c  normal entry:
c---------------
      IF(IDEB.EQ.1)print *,' U1: get better BP....'
C     get PYER bank to check if beamposition there already:
      NPYER=NLINK('PYER',IBNK0)
      IF(NPYER.NE.0) THEN
        NRWS = LROWS(NPYER)
        DO IRW=1,NRWS
          IND=KROW(NPYER,IRW)
C         check on ITYP=0
          IF (IW(IND+1).EQ.0) THEN
      IF(IDEB.EQ.1)print *,' beamposition found...'
            NRBP = IRW
            GO TO 90
          END IF
        END DO
      END IF
C
#ifdef Linux
      CALL QFGET_BP_(IRUNDE(2),IEVTDE(2),BPFLAG,VTX,VTERR,VTSIZ)
#else
      CALL QFGET_BP(IRUNDE(2),IEVTDE(2),BPFLAG,VTX,VTERR,VTSIZ)
#endif
C
      IF (BPFLAG.EQ.-1) THEN
        IF(IDEB.EQ.1)print *,' no BP found -> set to 0.0.0'
        VRBCR(1) = 1.
        VRBCR(3) = 1.
        VRBCR(6) = 1.
      ELSE
        VRBCR(1) = VTERR(1)**2 + VTSIZ(1)**2
        VRBCR(3) = VTERR(2)**2 + VTSIZ(2)**2
        VRBCR(6) = VTERR(3)**2 + VTSIZ(3)**2
      END IF
C
      IBTYP = 0
      CHVX2 = 0.
      NVDOF = 1
      IF(IDEB.EQ.1)print *,' call YSVVTX to store beamposition...'
      CALL YSVVTX(IBNK0,IBTYP,VTX,VRBCR,CHVX2,NVDOF,ITRKS,NTRK,
     $    IER,LGARB)
      IF(IER.NE.0) THEN
        NRBP = 0
        CALL DWRT(' -YSVVTX- failed in DVBPCA - IER='//DT3(FLOAT(IER)))
        RETURN
      ENDIF
      NPYER=NLINK('PYER',IBNK0)
      NRBP = LROWS(NPYER)
C
  90  CONTINUE
      RETURN
      END
C
C#######################################################################
*DK DVJET0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVJET0
CH
      SUBROUTINE DVJET0(YCUT,NJETS)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C   Author    :- Gabi & Co.            January 1993
C
C   Purpose   : recluster the JETS
C   Inputs    : YCUT
C   Outputs   : NJETS   = # of Jets reclustered
C   SRs called: DVJTCA  (calls -> ALE:GETLEP
C                              -> ALE:FJMMCL)
C   Data IN   : BOS-bank EJET #0
C   Data out  : BOS-bank EJET #1 for primary-vertex fit ycut=0.01
C                          or #2 for other jets-reclustering
C
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DB
      INCLUDE 'A_BCS.INC'
C
      REAL        YCUT
      INTEGER     NJETS
C
      REAL PXYZ(3)
      REAL ENERG
C
      INTEGER IBNK0/0/,IBNK1/1/,IBNK2/2/
      REAL YCDEF/-99999.0/
      REAL YCPV2/.01/
      REAL YTOGEV
C
      INCLUDE 'A_BMACRO.INC'
C
C  open Jets bank EJET (#0=original, #1 or #2 = reclustered!):
C
      YTOGEV = 1.
      IF (YCUT.EQ.YCDEF) THEN
        NJETS = 0
        NEJET = NLINK('EJET',IBNK0)
        IF(NEJET.LE.0) THEN
          NEJET = NLINK('DJET',IBNK0)
          IF(NEJET.LE.0) RETURN
          YTOGEV = .001
        END IF
      ELSE IF (YCUT.EQ.YCPV2) THEN
        NJETS = 0
        NEJET = NLINK('EJET',IBNK1)
        IF(NEJET.LE.0) THEN
          CALL DVJTCA(YCUT,IBNK1)
          NEJET = NLINK('EJET',IBNK1)
          IF(NEJET.LE.0) RETURN
        END IF
      ELSE
        NJETS = 0
        CALL DVJTCA(YCUT,IBNK2)
        NEJET = NLINK('EJET',IBNK2)
        IF(NEJET.LE.0) RETURN
      END IF
C
      NJETS = LROWS(NEJET)
C
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVJET
CH
      ENTRY DVJET(NJ,PXYZ,ENERG)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann and Gabi              26-FEB-1993
C
C!:
C    Inputs    : NJ      = Jet # to be used
C    Outputs   : PXYZ(3) = PX, PY, PZ of Jet
C                ENERG   = Energy of Jet
C
C ---------------------------------------------------------------------
C
      IND=KROW(NEJET,NJ)
      PXYZ(1) = RW(IND+1)*YTOGEV
      PXYZ(2) = RW(IND+2)*YTOGEV
      PXYZ(3) = RW(IND+3)*YTOGEV
      ENERG   = RW(IND+4)*YTOGEV
C
      RETURN
      END


C
C#######################################################################
C

      SUBROUTINE DVJTCA(YCUT,IBANK)
C=======================================
C
C
C   Author    :- Gabi & Co.            January 1993
C
C   Purpose   : refit (recluster) the jets using ALEPHLIB:FJMMCL
C   Input     : YCUT  - set in DALI for recluster of jets
C                       or 0.01 for vertex-fit
C               IBANK - write into bank# IBANK = 1 for vertex-fit
C                                              = 2 for jets-recluster
C=====================================================================
C
      INCLUDE 'DALINEWSRC:DALI_CF.INC'
      INCLUDE 'A_BCS.INC'
C
      REAL YCUT
      INTEGER IBANK
C
      INTEGER NEJET,N1JET
      INTEGER IBNK0/0/
      INTEGER IND,IJT,IRET
      REAL YTOGEV
C
      INTEGER IFOUN,IFILL,NV
      REAL ELEP,XYZ(3),DXYZ(3)
C
      PARAMETER (NP=50,NJ=10)
C
      INTEGER NWDS,NPAR,NJTMX/NJ/
      REAL ENORM
      CHARACTER TSCHM*2, TVERS*6
      CHARACTER *2 DT2
      REAL MASMA(NP,NP),PP(5,NP)
      REAL PPARX(NP), PPARY(NP), PPARZ(NP), PPARE(NP)
      INTEGER NJET
      REAL PJETX(NJ), PJETY(NJ), PJETZ(NJ), PJETE(NJ)
      INTEGER JNOFP(NP)
      DATA IDEB/0/
C
      INCLUDE 'A_BMACRO.INC'
C
C  ....................................
C
C..open original JETS-bank
      YTOGEV = 1.
      NEJET = NLINK('EJET',IBNK0)
      IF(NEJET.LE.0) THEN
        NEJET = NLINK('DJET',IBNK0)
        IF(NEJET.LE.0) THEN
          CALL DWRT('ERROR: EJET(DJET) BANK #'//DT2(FLOAT(IBNK0))//
     &      ' NOT FOUND')
          CALL DWRC
          RETURN
        ENDIF
        YTOGEV = .001
      END IF
C
C..define EVIS (visible energy)
      CALL GETLEP(IRUNDE(2),IFOUN,IFILL,NV,ELEP,XYZ,DXYZ)
      ENORM = ELEP
C
C..some other parameters...
      TSCHM  = 'E'
      TVERS  = 'NORMAL'
C
C..readin the jets from EJET-bank
      NWDS = LCOLS(NEJET)
      NPAR = LROWS(NEJET)
      IF (NPAR.LE.0) THEN
        CALL DWRT(' no JETS found in bank EJET - give up!')
        RETURN
      END IF
      DO IJT=1,NPAR
        IND=KROW(NEJET,IJT)
        PPARX(IJT) = RW(IND+1)*YTOGEV
        PPARY(IJT) = RW(IND+2)*YTOGEV
        PPARZ(IJT) = RW(IND+3)*YTOGEV
        PPARE(IJT) = RW(IND+4)*YTOGEV
      END DO
C
C..call (the 3rd line are OUTPUTparameters!)
C
        CALL FJMMCL( YCUT, ENORM, TSCHM,  TVERS, MASMA,  PP,
     &               NPAR, PPARX, PPARY,  PPARZ, PPARE,  NJTMX,
     &               NJET, PJETX, PJETY,  PJETZ, PJETE,  JNOFP  )
C
        IF(IDEB.EQ.1)print *,' output FJMMCL: NJET=',NJET
C
C..copy into EJET-bank number IBANK !!
C
      N1JET = NLINK('EJET',IBANK)
      IF (N1JET.LE.0) THEN
        IF(IDEB.EQ.1)print *,'book and fill new bank# for EJET'
        CALL AUBOS('EJET',IBANK,LMHLEN+NWDS*NJ,N1JET,IRET)
        IF (IRET.NE.0) THEN
          CALL DWRT(' not able to book new EJET bank - give up!')
          RETURN
        END IF
      END IF
C
      IW(N1JET+LMHCOL) = NWDS
      IW(N1JET+LMHROW) = NJET
      DO IJT=1,NJET
        IND = KROW(N1JET,IJT)
        RW(IND+1) = PJETX(IJT)
        RW(IND+2) = PJETY(IJT)
        RW(IND+3) = PJETZ(IJT)
        RW(IND+4) = PJETE(IJT)
      END DO
C
      IF(IDEB.EQ.1)print*,' new jets reclustered - please redraw!'
C
      RETURN
      END
C
C#######################################################################
*DK DVPV0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVPV0
CH
      SUBROUTINE DVPVCA(NLOCKT,LOCKT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C   Author    :- Gabi & Co.            February 1993
C
C   Purpose   : calculate primary vertex using UPHY:FINDIP
C   Inputs    : NLOCKT = number of tracks to be excluded
C               LOCKT  = array of tracknumbers to be excluded
C   Outputs   :
C   SRs called: UPHY:FINDIP
C   Data IN   : Beam-Position                  ->PYER bank#0
C               reclustered Jets (YCUT=0.01)   ->EJET bank#1
C   Data out  : BOS-bank PYER #0  TYPE = 6
C               BOS-bank PYER #0  TYPE = 8  if tracks were EXcluded !!!
C
C============================================
C
      INCLUDE 'DALINEWSRC:DALI_CF.INC'
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_PYERJJ.INC'
      INCLUDE 'A_FRFTJJ.INC'
      INCLUDE 'A_FRTLJJ.INC'
C
      INTEGER NLOCKT,LOCKT(*)
C
*CD YPARTO          YTOP / SYST
      COMMON/YPARTO/DHXLIM,CHISEL,PMINSE,PIDACP,BFIELD,
     &       MNTHPV,MXTSPV, PMINRQ,PMINRA,
     &       CHVXCO,CHPTCO,RVACCO,AMCTCO,DZMXCO,NAMXCO,EPLOCO,EPHICO,
     &       CHVXV0,CHPTV0,CHVSV0,CHMLV0,DZMXV0,NAMXV0,
     &       PIPKV0,PRPLV0,PIPLV0,
     &       LBCRFD,LRYOLD,LRFRF2,
     &       LRPVTX,LRSVTX,LVBCR0,LRMVPV,LRLPVX,LCONVS,LVZERS,LRUSER
      LOGICAL LBCRFD,LRYOLD,LRFRF2
      LOGICAL LRPVTX,LRSVTX,LVBCR0,LRMVPV,LRLPVX,LCONVS,LVZERS,LRUSER
c--please check ALEPHLIB for documentation...
C
      INTEGER NVTX
C
C  temporarely - vertex type for QFNDIP = 6 !!!
      INTEGER ITYPE/6/
C
      REAL VXYZ(3),VERR(3),VCMAT(6)
      INTEGER ITKLST(93)
C
      INTEGER NPYER,NFRFT,NFRTL,NEJET
      INTEGER IPYER
      INTEGER IBNK0/0/,IBNK1/1/,IBNK2/2/
      INTEGER I,IJT,ITRK
      INTEGER IND,INDL,NWDS,NTRK
      INTEGER NRBP
      INTEGER IPTYP/6/
C
      CHARACTER*3 DT3
C
      REAL YCUT/0.01/
C
      REAL IR,D0,PHI,Z0,TANL,NDF,C2
      INTEGER    MAXTRK,MAXJET
      PARAMETER (MAXTRK=200,MAXJET=20)
      REAL MXCH2/4./,MXERR(2)/.1,.5/
      REAL D0CUT/.5/,Z0CUT/8./
      REAL QQIRP,COSL
      PARAMETER (QQIRP=.00029979)
      REAL PP,PMIN/.2/
      INTEGER MINITC/0/,MINTPC/4/
      INTEGER NJET,NNJET
      REAL JETS(3,MAXJET),SUM
      REAL BP(3),SIGBP(3)
      REAL SIGD0,SIGZ0
      INTEGER IGOOD(MAXTRK)
      INTEGER NGOOD,MINTRK/1/
      REAL TPAR(5,MAXTRK),TERR(3,MAXTRK)
      REAL IP(3),IPERR(3,3),C2DOF
      INTEGER NUSED(2)
      DATA IDEB/0/
C
      INCLUDE 'A_BMACRO.INC'
C
C
C  ....................................
C
      NVTX = 0
C     use the contents of VXYZ and VERR = 0. as flag for failed.....
      CALL UZERO(VXYZ,1,3)
      CALL UZERO(VERR,1,3)
C
C     get PYER bank to get primary vertex:
      NPYER=NLINK('PYER',IBNK0)
      IF(NPYER.NE.0) THEN
        NWDS = LCOLS(NPYER)
        NVTX = LROWS(NPYER)
        IF (NLOCKT.EQ.0) THEN
          DO IVER=1,NVTX
            IND=KROW(NPYER,IVER)
C           check on type of VERTEX:
            IF (IW(IND+1).EQ.ITYPE) THEN
              VXYZ(1) = RW(IND+JPYEVX)
              VXYZ(2) = RW(IND+JPYEVY)
              VXYZ(3) = RW(IND+JPYEVZ)
              VERR(1) = RW(IND+JPYEVM)
              VERR(2) = RW(IND+JPYEVM+2)
              VERR(3) = RW(IND+JPYEVM+5)
      IF(IDEB.EQ.1)print *,' requested vertex found - TYPE = ',ITYPE
      IF(IDEB.EQ.1)print *,VXYZ,VERR
              GO TO 90
            END IF
          END DO
        END IF
      END IF
C
C     refit primary vertex with QFNDIP:
      IF(IDEB.EQ.1)print *,' fit Primary Vertex calling FINDIP!'
C
C       get beam-position:
        CALL DVBPCA(NRBP)
        IF (NRBP.NE.0) THEN
          NPYER = NLINK('PYER',IBANK0)
          IPYER = KROW(NPYER,NRBP)
          BP(1) = RW(IPYER+JPYEVX)
          BP(2) = RW(IPYER+JPYEVY)
          BP(3) = RW(IPYER+JPYEVZ)
          SIGBP(1) = RW(IPYER+JPYEVM)
          SIGBP(2) = RW(IPYER+JPYEVM+2)
          SIGBP(3) = RW(IPYER+JPYEVM+5)
      IF(IDEB.EQ.1)print *,'beamposition:',BP,SIGBP
        ELSE
          CALL UZERO(BP,1,3)
          CALL UZERO(SIGBP,1,3)
        END IF
C
C       get the jets:
        NEJET = NLINK('EJET',IBNK1)
        IF(NEJET.LE.0) THEN
C         no jets reclustered yet - do it:
      IF(IDEB.EQ.1)print *,' call DVJTCA !!!!'
          CALL DVJTCA(YCUT,IBNK1)
          NEJET = NLINK('EJET',IBNK1)
          IF (NEJET.EQ.0) RETURN
        END IF
        NJET = LROWS(NEJET)
        NNJET=0
        DO IJT=1,NJET
          IND=KROW(NEJET,IJT)
          JETS(1,IJT) = RW(IND+1)
          JETS(2,IJT) = RW(IND+2)
          JETS(3,IJT) = RW(IND+3)
C         normalize the jets
c  with new FINDIP routine in ALPHA normalization not needed anymore!
          SUM = JETS(1,IJT)**2 + JETS(2,IJT)**2 + JETS(3,IJT)**2
          SUM = SQRT(SUM)
          IF(SUM .GT. 10.0)THEN
            NNJET = NNJET + 1
            DO I=1,3
c              JETS(I,NNJET) = JETS(I,IJT)/SUM
              JETS(I,NNJET) = JETS(I,IJT)
            END DO
          END IF
        END DO
      IF(IDEB.EQ.1)print *,' JETS found: ',NNJET
C       require 2 jets:
        IF (NNJET.LT.2) RETURN
C
C       get the tracks:
        NFRFT = NLINK('FRFT',IBNK2)
        IF (NFRFT.EQ.0) THEN
          CALL DWRT(' no FRFT bank#2 found - give up!')
          RETURN
        END IF
        NFRTL = NLINK('FRTL',IBNK0)
        IF (NFRTL.EQ.0) THEN
          CALL DWRT(' no FRTL bank#2 found - give up!')
          RETURN
        END IF
        NTRK  = LROWS(NFRFT)
      IF(IDEB.EQ.1)print *,' FRFT read - NTRK=',NTRK
C
        CALL VZERO(IGOOD,MAXTRK)
        NGOOD = 0
        DO ITRK=1,NTRK
          IND  = KROW(NFRFT,ITRK)
          INDL = KROW(NFRTL,ITRK)
C
C         check if track is 'locked':
          DO I=1,NLOCKT
            IF (ITRK.EQ.LOCKT(I)) GO TO 999
          ENDDO
C
C         copy from FRFT-bank:
          IR  = RW(IND+JFRFIR)
          D0  = RW(IND+JFRFD0)
          PHI = RW(IND+JFRFP0)
          Z0  = RW(IND+JFRFZ0)
          TANL= RW(IND+JFRFTL)
          NDF = IW(IND+JFRFDF)
          C2  = RW(IND+JFRFC2)
C
C         cut on momentum:
          COSL = 1./SQRT(1.+TANL**2)
          PP = QQIRP * BFIELD * ABS(1./IR) * (1./COSL)
          IF (PP .LT. PMIN) THEN
      IF(IDEB.EQ.1)print *,' test on momentum failed',PP,IR,COSL
            GO TO 999
          ENDIF
C
C         cut on # of hits:
          IF (IW(INDL+JFRTNI) .LT. MINITC  .OR.
     &        IW(INDL+JFRTNT) .LT. MINTPC) THEN
      IF(IDEB.EQ.1)print *,' test on #of hits failed'
            GO TO 999
          ENDIF
C
C         basic cuts:
          IF (NDF.GT.0) THEN
            CHI = C2/NDF
          ELSE
            CHI = 100.
          END IF
          SIGD0  = SQRT(RW(IND+JFRFEM+9))
          SIGZ0  = SQRT(RW(IND+JFRFEM+14) / (1.+TANL**2))
C
C         cut on Chisquared:
          IF (CHI .GT. MXCH2) THEN
      IF(IDEB.EQ.1)print *,' test on CHI2 failed'
            GO TO 999
          END IF
C
C         cut on D0 (relative to BP), Z0
          IF( ABS(D0-SIN(PHI)*BP(1)+COS(PHI)*BP(2)) .GT. D0CUT .OR.
     &      ABS(Z0) .GT. Z0CUT)THEN
      IF(IDEB.EQ.1)print *,' test on D0 failed'
            GOTO 999
          END IF
C
C         cut on track errors
          IF (SIGD0 .GT. MXERR(1) .OR.
     &        SIGZ0 .GT. MXERR(2) )THEN
      IF(IDEB.EQ.1)print *,' test on track-errors failed'
            GOTO 999
          END IF
C
C         this is a good track:
          TPAR(1,ITRK) = RW(IND+JFRFIR)
          TPAR(2,ITRK) = RW(IND+JFRFTL)
          TPAR(3,ITRK) = RW(IND+JFRFP0)
          TPAR(4,ITRK) = RW(IND+JFRFD0)
          TPAR(5,ITRK) = RW(IND+JFRFZ0)
          TERR(1,ITRK) = RW(IND+JFRFEM+9)
          TERR(2,ITRK) = RW(IND+JFRFEM+14)
          TERR(3,ITRK) = RW(IND+JFRFEM+13)
          IGOOD(ITRK)= 1
          NGOOD = NGOOD + 1
          ITKLST(NGOOD) = ITRK
 999      CONTINUE
        END DO
C
        IF (NGOOD.LT.MINTRK) GO TO 90
C
C       call the routine which does the real work......
C       -----------------------------------------------
      IF(IDEB.EQ.1)print *,' call FINDIP ! NGOOD=',NGOOD
        CALL FINDIP(BP,SIGBP,NNJET,JETS,NTRK,TPAR,TERR,IGOOD,
     &              IP,IPERR,C2DOF,NUSED)
      IF(IDEB.EQ.1)print *,' NUSED=',NUSED
C
C       check on error-flag:
        IF (NUSED(1).EQ.0 .AND. NUSED(2).EQ.0) GO TO 90
C
C       fill local array
        VXYZ(1) = IP(1)
        VXYZ(2) = IP(2)
        VXYZ(3) = IP(3)
        VERR(1) = IPERR(1,1)
        VERR(2) = IPERR(2,2)
        VERR(3) = IPERR(3,3)
      IF(IDEB.EQ.1)print *,' new VETEX:'
      IF(IDEB.EQ.1)print *,VXYZ,VERR
C
C       call YSVVTX to store vertex...
      IF(IDEB.EQ.1)print *,' call YSVVTX to store vertex...'
        IF (NLOCKT.EQ.0) THEN
          IPTYP = 6
        ELSE
          IPTYP = 8
        END IF
        NVDOF = NUSED(1) + NUSED(2) - 3
        CALL UZERO(VCMAT,1,6)
        VCMAT(1) = VERR(1)
        VCMAT(3) = VERR(2)
        VCMAT(6) = VERR(3)

        CALL YSVVTX(IBNK0,IPTYP,VXYZ,VCMAT,C2DOF,NVDOF,ITKLST,NGOOD,
     $      IER,LGARB)
        IF(IER.NE.0) THEN
          CALL DWRT('-YSVVTX- failed in DVPVCA-IER='//DT3(FLOAT(IER)))
          RETURN
        ENDIF
C
        NPYER = NLINK('PYER',IBANK0)
        NVTX = LROWS(NPYER)
      IF(IDEB.EQ.1)print *,' ok - bank PYER#0,TYPE=6 filled, NVTX=',NVTX
  90  CONTINUE
C
      END
C
C#######################################################################
*DK DVSVCA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVSVCA
CH
c... modif Damir 20/3/97
      SUBROUTINE DVSVCA(NT,ITRN,ITRTY,NVROW,NTRTY,NTROW,NP0,PPI0,ERRP0)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C   Author    :- Gabi & Co.            March 1993
C   pi0's (PPI0,ERRP0) introduced 21-mar-97 by Damir Buskulic 
C
C   Purpose   : more extended secondary-vertex-fitting
C   Inputs    : NT             = no of inputtracks for fit
C               ITRN           = array with track# (in FRFT)
C               ITRTY          = array with track-types:
C                                0 = not to be used
C                                1 = from bank FRFT
C                                2 = from bank CRFT (new charged track)
C                                3 = from bank NRFT (neutral track)
c... modif Damir 20/3/97
C               NP0           = # OF PI0'S must be 1
c               PPI0          = optional Pi0 momentum (=0 if no Pi0)
c               ERRP0         = optional error matrix of Pi0 momentum (4*4)
c... fin modif damir
c
C   Outputs   : NVROW          = vertex created (row# in PYER)
C                                (=zero if failed....)
C               NTRTY          = type of new fitted track
C               NTROW          = row# in new trackbank CRFT or NRFT
C   SRs called: alephlib:YTIJOB  initialize vertex-fit-routines | at
C               "        YTOPNW  initialize vertex-fit-routines | init.....
C               "        YFMVTR  fit secondary vertex
C                                using charged and neutral tracks
C               "        YSVVTX  store fitted vertex
C               "        YTPAR   calculate track parameters from
C                                vertex and momentum
C   Data IN   : BOS-banks FRFT      input-track-parameters
C               "         CRFT+NRFT if existing from previous fits
C   Data out  : PYER (ITYPE=7)   for new sec. vertex
C               PYFR             associated tracks...
C               CRFT or NRFT     for fitted charged or neutral track
C
C============================================
c
      INCLUDE 'DALINEWSRC:DALI_CF.INC'
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_FRFTJJ.INC'
ccc      INCLUDE 'PHY:QCDE.INC'
c
c... modif Damir 20/3/97
      DIMENSION PPI0(3),ERRP0(4,4)
c... fin modif damir
C
      INTEGER NT, ITRN(*), ITRTY(*)
      INTEGER NVROW, NTRTY, NTROW
C
      CHARACTER*3 DT3
C
      INTEGER I,IND
c
      INTEGER KFRFT,KCRFT,KNRFT,KPYER
      INTEGER NCRFT,NNRFT
      INTEGER IBNK0/0/,IBNK2/2/
      INTEGER NWDS,NRLAST,NVLAST
      INTEGER NRCRFT,NRNRFT
c
      INTEGER IPTYP/6/,ISTYP/7/
c
      INTEGER NTRK,NNEU,NCHR
      DIMENSION ITKLST(93)
      DIMENSION ITNLST(10)
      DIMENSION ITCLST(10)
      DIMENSION DUM(3,2),DUMY(6,2)
      REAL VOUT(3), VVOUT(6)
      REAL HXOU(5,10),VHXOU(15,10)
      REAL TNUO(5,10),VTNUO(15,10)
      REAL PSUM(3),VPSUM(6),VPSVX(3,3),VMVX(3),VMPS(3)
      REAL FRF(50),VFRF(150)
      REAL FRFN(50),VFRFN(150)
      REAL AMPC(10)
      DATA AMPC/10*0.135/
      REAL AMASS,DMASS
      REAL FRF_OUT(5),VFRF_OUT(15)
      INTEGER NVDOF
      DATA IDEB/0/
C
      LOGICAL LGARB
C
      INCLUDE 'A_BMACRO.INC'
c
C  ....................................
C
      NVROW = 0
      NTRTY = 0
c
      IF(IDEB.EQ.1)print *,' U1: secondary vertex fit:'
C
      CALL BLIST(IW,'E+','CRFTNRFT')
C
C..check on inputtracks...
      IF(NT.LE.1) THEN
        call DWRT('less than 2 tracks selected! EXIT vertex-fitting!')
        RETURN
      ENDIF
C...define bank number 0 for PYER in DALI
      CALL  DVVXBN(IBNK0)
      IF(IDEB.EQ.1)print *,' DVVXBN called...'
C... and 2 for FRFT bank (set DALI-flag if necessary)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HTB'
      CALL DPARAM(20
     &  ,J_HTB)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF (PARADA(2,J_HTB).NE.2) THEN
         PARADA(2,J_HTB)=2
         CALL DWRT('FRFT-BOS-bank-number 2 slected ! (GT::DD::TB=2)')
      ENDIF
      KFRFT = NLINK('FRFT',IBNK2)
      IF(KFRFT.LE.0) THEN
        CALL DWRT(' DVSVCA-error: FRFT BANK #2 NOT FOUND')
        RETURN
      ENDIF
      NWDS = LCOLS(KFRFT)
      KCRFT = NLINK('CRFT',IBNK0)
      KNRFT = NLINK('NRFT',IBNK0)
C
C.. get track numbers
      NTRK = 0
      NNEU = 0
      NCHR = 0
      DO I=1,NT
        IF (ITRTY(I).EQ.1) THEN
          NTRK = NTRK+1
          ITKLST(NTRK) = ITRN(I)
        END IF
        IF (ITRTY(I).EQ.2) THEN
          NCHR = NCHR+1
          ITCLST(NCHR) = ITRN(I)
        END IF
        IF (ITRTY(I).EQ.3) THEN
          NNEU = NNEU+1
          ITNLST(NNEU) = ITRN(I)
        END IF
      END DO
C
C..the tracks are to be not compatible with the primary vertex....
C..try to fit vertex
      ISQ = 0
C
C     initialize rundependent values for YTOPOL-routines...:
C
      CALL YTOIRU(IRUNDE(2),15.0)
      IF(IDEB.EQ.1)print *,' YTOIRU called....'
C
C     fill the track parameters in the array FRF, VFRF
C
C      first for FRFT tracks
C
      IF (NTRK.GT.0) THEN
        IF(IDEB.EQ.1)print *,' fill tracklist with FRFT-tracks'
        DO I = 1,NTRK
          DO J = 1,5
            FRF(5*(I-1)+J) = RTABL(KFRFT,ITKLST(I),J)
          END DO
          DO J = 1,15
            VFRF(15*(I-1)+J) = RTABL(KFRFT,ITKLST(I),6+J)
          END DO
          IF(RTABL(KFRFT,ITKLST(I),1).GT.0.) ISQ = ISQ - 1
          IF(RTABL(KFRFT,ITKLST(I),1).LT.0.) ISQ = ISQ + 1
          ITKLST(I) = I
        END DO
      END IF
C
C      then for charge fitted tracks (= result of previous vertex fit)
C      (they are treated like normal charge tracks)
C
C      KCRFT = link to CRFT bank (which has to be there !!!)
C
      IF (NCHR.GT.0) THEN
        IF(IDEB.EQ.1)print *,' fill tracklist with CRFT-tracks'
        DO I = 1,NCHR
          DO J = 1,5
            FRF(5*(NTRK+I-1)+J) = RTABL(KCRFT,ITCLST(I),J)
          ENDDO
          DO J = 1,15
            VFRF(15*(NTRK+I-1)+J) = RTABL(KCRFT,ITCLST(I),6+J)
          ENDDO
          ITKLST(NTRK+I) = NTRK+I
          IF(RTABL(KFRFT,ITCLST(I),1).GT.0.) ISQ = ISQ - 1
          IF(RTABL(KFRFT,ITCLST(I),1).LT.0.) ISQ = ISQ + 1
        ENDDO
        NTRK = NTRK + NCHR
      END IF
C
C
C      then for neutral fitted tracks (= result of previous vertex fit)
C
C      KNRFT = link to NRFT bank (which has to be there !!!)
C
      IF (NNEU.GT.0) THEN
        IF(IDEB.EQ.1)print *,' fill tracklist with NRFT-tracks'
        DO I = 1,NNEU
          DO J = 1,5
            FRFN(5*(I-1)+J) = RTABL(KNRFT,ITNLST(I),J)
          ENDDO
          DO J = 1,15
            VFRFN(15*(I-1)+J) = RTABL(KNRFT,ITNLST(I),6+J)
          ENDDO
          ITNLST(I) = I
        ENDDO
      END IF
C
C
C      fit secondary vertex using charged and neutral tracks
C
      IF(IDEB.EQ.1)print *,'calling YFMVTR: NTRK,NNEU=',ntrk,nneu

      CALL YFMVTR(0,NTRK,NNEU,.TRUE.,.TRUE.,.TRUE.,.FALSE.,
     &            DUM,DUMY, ITKLST(1),5,15,FRF,VFRF,
     &            ITNLST(1),5,15,FRFN,VFRFN,
     &            1,AMPC,
     &            VOUT,VVOUT,HXOU,VHXOU,TNUO,VTNUO,
     &            PSUM,VPSUM,VPSVX,
     &            AMASS,DMASS,VMVX,VMPS,
     &            CHVX2,IFAIL)
C
      IF( IFAIL .NE. 0 ) THEN
        CALL DWRT('-YFTVTR- failed in DVSVCA - IFAIL='//
     &               DT3(FLOAT(IFAIL)))
        RETURN
      ENDIF
C
C
C     store fitted vertex as ITYPE=7 into PYER bank#0
C
      ISTYP = 7
      NVDOF = 2*NTRK-3
      IF(IDEB.EQ.1)print *,' call YSVVTX to store vertex...'
      CALL YSVVTX(IBNK0,ISTYP,VOUT,VVOUT,CHVX2,NVDOF,ITKLST,NTRK,
     $    IER,LGARB)
      IF(IER.NE.0) THEN
        CALL DWRT(' -YSVVTX- failed in DVSVCA - IER='//
     &              DT3(FLOAT(IER)))
        RETURN
      ENDIF
C    find out absolute row number in PYER:
      KPYER = NLINK('PYER',IBNK0)
      NVROW = LROWS(KPYER)
      IF(IDEB.EQ.1)print *,' new row# in PYER=',NVROW
C
c... modif Damir 20/3/97 ............................
	IF (IDEB.EQ.0) THEN
C
C... Ajout de l'impulsion et de la matrice d'erreur 
c...  du Pi0 si il est present
	   IF (NP0.EQ.1) THEN
	      PSUM(1) = PSUM(1)+PPI0(1)
	      PSUM(2) = PSUM(2)+PPI0(2)
	      PSUM(3) = PSUM(3)+PPI0(3)
	      VPSUM(1) = VPSUM(1)+ERRP0(1,1)
	      VPSUM(2) = VPSUM(2)+ERRP0(1,2)
	      VPSUM(3) = VPSUM(3)+ERRP0(2,2)
	      VPSUM(4) = VPSUM(4)+ERRP0(1,3)
	      VPSUM(5) = VPSUM(5)+ERRP0(2,3)
	      VPSUM(6) = VPSUM(6)+ERRP0(3,3)
	   ENDIF
        ENDIF
c... fin modif damir .................................
C
C
C     store resulting tracks !!!!
C
C       calculate track parameters from vertex and momentum
C
      IF(IDEB.EQ.1)print *,' call YTPAR to get track parameters...'
      CALL YTPAR(ISQ,VOUT,VVOUT,PSUM,VPSUM,VPSVX,
     &           FRF_OUT,VFRF_OUT,IFAIL)
      IF(IDEB.EQ.1)print *,' ISQ=',ISQ
      IF(IFAIL.NE.0) THEN
        CALL DWRT(' -YTPAR- failed in DVSVCA - IFAIL='//
     &    DT3(FLOAT(IFAIL)))
        RETURN
      END IF
C
C
C     store in track banks:
C
C     charged ->CFRF (ISQ.NE.0)      neutral ->NFRF (ISQ.EQ.0)
C
C      1 inversRadi                1 momentum                 !!!
C      2 TanLambda                 2 TanLambda
C      3 Phi0                      3 Phi0
C      4 D0                        4 D0
C      5 Z0                        5 Z0
C      6 absolute row# in PYER     6 absolute row# in PYER    !!!
C      7-21 Errors                 7-21 Errors
C      22-30  ZERO                 22-30  ZERO                !!!
C
C
C    charged tracks....
C
      IF (ISQ.NE.0) THEN
        NTRTY = 2
        IF (KCRFT.LE.0) THEN
          NRCRFT=1
        ELSE
          NRLAST = LROWS(KCRFT)
          NRCRFT = NRLAST + 1
        END IF
        NTROW = NRCRFT
        IF(IDEB.EQ.1)print *,' store charged: NRCRFT=',NRCRFT
        CALL AUBOS('CRFT',IBNK0,LMHLEN+(NWDS*NRCRFT),KCRFT,IRET)
        IF (IRET.NE.0) THEN
          CALL DWRT(' not able to book new CRFT bank - give up!')
          RETURN
        END IF
        IW(KCRFT+LMHCOL) = NWDS
        IW(KCRFT+LMHROW) = NRCRFT
        NCRFT = KROW(KCRFT,NRCRFT)
        DO I=1,5
          RW(NCRFT+I) = FRF_OUT(I)
        END DO
          IW(NCRFT+6) = NVROW
        DO I=1,15
          RW(NCRFT+6+I) = VFRF_OUT(I)
        END DO
        DO I=22,30
          RW(NCRFT+I) = 0
        END DO
C
C    neutral tracks....
C
      ELSEIF (ISQ.EQ.0) THEN
        NTRTY = 3
        IF (KNRFT.LE.0) THEN
          NRNRFT=1
        ELSE
          NRLAST = LROWS(KNRFT)
          NRNRFT = NRLAST + 1
        END IF
        NTROW = NRNRFT
        IF(IDEB.EQ.1)print *,' store charged: NRNRFT=',NRNRFT
        CALL AUBOS('NRFT',IBNK0,LMHLEN+(NWDS*NRNRFT),KNRFT,IRET)
        IF (IRET.NE.0) THEN
          CALL DWRT(' not able to book new NRFT bank - give up!')
          RETURN
        END IF
        IW(KNRFT+LMHCOL) = NWDS
        IW(KNRFT+LMHROW) = NRNRFT
        NNRFT = KROW(KNRFT,NRNRFT)
        DO I=1,5
          RW(NNRFT+I) = FRF_OUT(I)
        END DO
          IW(NNRFT+6) = NVROW
        DO I=1,15
          RW(NNRFT+6+I) = VFRF_OUT(I)
        END DO
        DO I=22,30
          RW(NNRFT+I) = 0
        END DO
C
      ENDIF
      RETURN
C
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVSVDL
CH
      ENTRY DVSVDL(NLVTX,NLCTR,NLNTR)
CH
CH --------------------------------------------------------------------
CH
CH   delete last entry (=row#) in PYER for secondary vertex
CH    "                        in CRFT for corresponding charged track
CH    "                        in NRFT "                 neutral track
CH
C ---------------------------------------------------------------------
C
C    Outputs   : NLVTX = last row number in PYER bank after delete
C                NLCTR = "                  CRFT "
C                NLNTR = "                  NRFT "
C
C ---------------------------------------------------------------------
C
      KPYER = NLINK('PYER',IBNK0)
      NVLAST = LROWS(KPYER)
      IND=KROW(KPYER,NVLAST)
C     check on type of VERTEX:
      IF (IW(IND+1).EQ.ISTYP) THEN
        IW(KPYER+LMHROW) = NVLAST - 1
C
        KCRFT = NLINK('CRFT',IBNK0)
        NRLAST = LROWS(KCRFT)
        IND=KROW(KCRFT,NRLAST)
        IF (IW(IND+6).EQ.NVLAST) THEN
          IW(KCRFT+LMHROW) = NRLAST-1
        END IF
C
        KNRFT = NLINK('NRFT',IBNK0)
        NRLAST = LROWS(KNRFT)
        IND=KROW(KNRFT,NRLAST)
        IF (IW(IND+6).EQ.NVLAST) THEN
          IW(KNRFT+LMHROW) = NRLAST-1
        END IF
C
        NLVTX = LROWS(KPYER)
        NLCTR = LROWS(KCRFT)
        NLNTR = LROWS(KNRFT)
C
      ELSEIF (IW(IND+1).EQ.IPTYP) THEN
        IW(KPYER+LMHROW) = NVLAST - 1
        NLVTX = LROWS(KPYER)
      ELSE
        NLVTX = 0
      END IF
C
      RETURN
      END


C#######################################################################
*DK DVNT0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVNT0
CH
      SUBROUTINE DVNT0(NTRKS)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C   Author    :- Gabi & Co.            January 1993
C
C   Purpose   : setting up for Neutral Tracks
C   Inputs    :
C   Outputs   : no. of neutral tracks in bank NRFT
C   Data IN   : BOS-bank NRFT created by DVSVCA
C   Data out  :
C
C==================================================
C
      INCLUDE 'DALINEWSRC:DALI_CF.INC'
      INCLUDE 'A_BCS.INC'
C
C  ....................................
C
      INTEGER NTRKS
      INTEGER KNRFT
      INTEGER IBNK0/0/
C
      DIMENSION TRKPAR(21)
      INTEGER NT
      INTEGER I,KNTRK
      DATA IDEB/0/
C
      INCLUDE 'A_BMACRO.INC'
C
C
      IF(IDEB.EQ.1)print *,' U1: neutral tracks!!!'
C
      NTRKS = 0
      KNRFT = NLINK('NRFT',IBNK0)
      IF (KNRFT.EQ.0) THEN
        CALL DWRT(' no bank NRFT wiht neutral tracks found..')
        RETURN
      END IF
      NTRKS = LROWS(KNRFT)
      IF(IDEB.EQ.1)print *,'KNRFT, NTRKS =',KNRFT, NTRKS
C
      RETURN
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DVNT
CH
      ENTRY DVNT(NT,TRKPAR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C   Inputs    : NT         = which track# wanted
C   Outputs   : TRKPAR(21) = neutral track parameters
C
C==================================================
CH
C
C    neutral track parameters  (!!! is different from FRFT bank!)
C
C    1 momentum                 !!!
C    2 TanLambda
C    3 Phi0
C    4 D0
C    5 Z0
C    6 absolute row# in PYER    !!!
C    7-21 Errors
C    22-30  ZERO                !!!
C
      KNTRK = KROW(KNRFT,NT)
      IF(IDEB.EQ.1)print *,' DVNT - track#wanted:',NT
C
      DO I=1,21
        TRKPAR(I) = RW(KNTRK+I)
      END DO
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE FFINDIP(BP,SIG2_BP,NJET,J1,NTRACK,TPAR,TERR,
     &  IGOOD,
     & IP,IP_ERR,CHI2_DOF,NUSED)
C
C  A standalone routine to find the interaction point, given the beamspo
C  the jet axes, and the tracks.  See ALEPH note 92-47 for a description
C
      IMPLICIT NONE
      SAVE
C                                                                       
C  Inputs;                                                              
C  beam point and sigmas**2, number of jets, jet direction UNIT VECTORS 
C  Track parameters and d0-Z0 corner of the error marix                 
C                                                                       
      INTEGER NJET,NTRACK,IGOOD(*)                                      
      REAL BP(3),SIG2_BP(3),J1(3,*)                                     
      REAL TPAR(5,*),TERR(3,*)                                          
C                                                                       
C  Outputs; Interaction point with full covariance matrix,  chisq/dof,  
C  number of tracks used; THIS NOW HAS 2 DIMENSIONS!!!                  
C                                                                       
      REAL IP(3),IP_ERR(3,3),CHI2_DOF                                   
      INTEGER NUSED(2)
C                                                                       
C  Local variables                                                      
C                                                                       
C                                                                     
C --     MAX. NUMBER OF TRACKS, jets ALLOWED                          
C                                                                     
      INTEGER    MAXTRK,MAXJET                                        
      PARAMETER (MAXTRK=200,MAXJET=20)                                
      INTEGER ICOR,JCOR,IERR
      INTEGER ITRACK,IMIN,JMIN,IMETA
      INTEGER IJET,JJET,MAX_META,NDOF,NUSED_SAVE(2)
      REAL MAX_CHI2                                                     
      REAL D0,PHI,Z0,TANL,COSL,SINL,IR,COSP,SINP,DPHI                   
      REAL T0(3),T1(3)                                                  
      REAL SIG2_D0,SIG2_D0ZPERP,SIG2_ZPERP                              
      REAL ALPHA, BETA(3), GAMMA(3,3)                                   
      REAL ALPHA_SAVE, BETA_SAVE(3), GAMMA_SAVE(3,3)                    
      REAL NORME,NORMM                                                  
      REAL CHI2,MAX_EDIF,MAX_MDIF,CHI2_SAVE                             
      REAL T_PHAT(3),T_THAT(3)                                          
      REAL DENOM, DOTP, DOTT, MAXDOT, DOT, DM,DIST,DIST_ERR             
      REAL DELTA(3,3)                                                   
      REAL EHAT(3,MAXTRK),MHAT(3,MAXTRK)                                
      REAL ET0(MAXTRK),MT0(MAXTRK),TANJET2(MAXTRK)                      
      REAL SIGE2(MAXTRK),SIGM2(MAXTRK),SIGE2_RAW(MAXTRK),SIGM           
      REAL OLD_IP(3)                                                    
      REAL DOT_CUT,JET_SIG2,NSIGM(2)
      LOGICAL KILL(MAXTRK),TAGE(MAXTRK),TAGM(MAXTRK)                    
      LOGICAL GOOD,GOODE,GOODM                                          
      DATA DELTA/1.,0.,0.,0.,1.,0.,0.,0.,1./                            
C                                                                       
C  Common for computing 1-track-removed vertex                          
C                                                                       
      COMMON/IPVTX/ET0,MT0,EHAT,MHAT,SIGE2,SIGM2,KILL,TAGE,TAGM,        
     &  ALPHA_SAVE,BETA_SAVE,GAMMA_SAVE,NUSED_SAVE                      
C                                                                       
C  Cut values                                                           
C                                                                       
      DATA MAX_CHI2/6./                                                 
      DATA DOT_CUT/0.9999999/,JET_SIG2/0.01/                            
      DATA MAX_META/5/, NSIGM/-3.0,0.0/                                 
C                                                                       
C  Inlines for defining good tracks                                     
C                                                                       
      GOOD(ITRACK) = .NOT.KILL(ITRACK)                                  
      GOODE(ITRACK) = .NOT.(KILL(ITRACK) .OR. TAGE(ITRACK))             
      GOODM(ITRACK) = .NOT.(KILL(ITRACK) .OR. TAGM(ITRACK)              
     &  .OR. TAGE(ITRACK))                                              
C                                                                       
C  loop over the tracks                                                 
C                                                                       
      NDOF = MIN(NTRACK,MAXTRK)                                         
      DO ITRACK=1,MIN(NTRACK,MAXTRK)                                    
C                                                                       
C  Start with all tracks declared 'good' for the information perpendicul
C  to the jet, but 'bad' for the information along the jet.  The paralle
C  information is recovered after the first meta-iteration for all      
C  useable tracks.  Remove tracks not passing cuts.                     
C                                                                       
        TAGE(ITRACK) = .FALSE.                                          
        TAGM(ITRACK) = .TRUE.                                           
        KILL(ITRACK) = IGOOD(ITRACK).LE.0                               
        IF(KILL(ITRACK))GOTO 1231                                       
C                                                                       
C  Convert the track parameters into vector notation.                   
C  Correct (approximately) for the curvature in calculating the directio
C  due to beamspot displacement.  Curvature doesn't significantly       
C  affect the endpoint or Z direction.                                  
C                                                                       
        IR  = TPAR(1,ITRACK)                                            
        TANL= TPAR(2,ITRACK)                                            
        PHI = TPAR(3,ITRACK)                                            
        D0  = TPAR(4,ITRACK)                                            
        Z0  = TPAR(5,ITRACK)                                            
        COSL = 1./SQRT(1.+TANL**2)                                      
        SINL = SIGN(SQRT(1.-COSL**2),TANL)                              
        COSP = COS(PHI)                                                 
        SINP = SIN(PHI)                                                 
        DPHI = IR*(BP(1)*COSP+BP(2)*SINP)                               
        T1(1) =  COSL*(COSP-DPHI*SINP)                                  
        T1(2) =  COSL*(SINP+DPHI*COSP)                                  
        T1(3) =  SINL                                                   
        T0(1) =  D0*SINP                                                
        T0(2) = -D0*COSP                                                
        T0(3) =  Z0                                                     
C                                                                       
C  Associate the track with one of the jets by minimizing the dot       
C  product                                                              
C                                                                       
        MAXDOT = -1000.                                                 
        DO IJET=1,NJET                                                  
          DOT = 0.0                                                     
          DO ICOR=1,3                                                   
            DOT = DOT + T1(ICOR)*J1(ICOR,IJET)                          
          END DO                                                        
          IF(DOT .GT. MAXDOT)THEN                                       
            JJET = IJET                                                 
            MAXDOT = DOT                                                
          END IF                                                        
        END DO                                                          
C                                                                       
C  Reject tracks that are too close to the jet direction; they have     
C  'compromised' information.  This cut is extremely loose; only        
C  1-track jets should be rejected                                      
C                                                                       
        IF(MAXDOT .GT. DOT_CUT)THEN                                     
          NDOF = NDOF - 1                                               
          KILL(ITRACK) = .TRUE.                                         
          GOTO 1231                                                     
        END IF                                                          
C                                                                       
C  Save the tangent**2 of the angle between the track and the jet       
C                                                                       
        TANJET2(ITRACK) = (1.-MAXDOT**2)/MAXDOT**2                      
C                                                                       
C  Find the Phi and theta directions (unit vectors) for this track      
C  These are usefull since the track error matrix is expressed in this  
C  basis.                                                               
C                                                                       
        DENOM = SQRT(T1(1)**2 + T1(2)**2)                               
        T_PHAT(1) =  T1(2)/DENOM                                        
        T_PHAT(2) = -T1(1)/DENOM                                        
        T_PHAT(3) =  0.0                                                
        T_THAT(1) = -T1(1)*T1(3)/DENOM                                  
        T_THAT(2) = -T1(2)*T1(3)/DENOM                                  
        T_THAT(3) =  DENOM                                              
C                                                                       
C  Find the direction perpendicular to both the track and               
C  the jet (EHAT).  Find also the direction perpendicular to            
C  the track and to this last vector (MHAT).  We thus create a new      
C  coordinate system T1, EHAT, and MHAT, which "diagonalizes" the       
C  chisquared calculation for finding the IP, and renders the equations 
C  solveable in closed form.                                            
C                                                                       
        EHAT(1,ITRACK) = T1(2)*J1(3,JJET) - T1(3)*J1(2,JJET)            
        EHAT(2,ITRACK) = T1(3)*J1(1,JJET) - T1(1)*J1(3,JJET)            
        EHAT(3,ITRACK) = T1(1)*J1(2,JJET) - T1(2)*J1(1,JJET)            
        DO ICOR=1,3                                                     
          MHAT(ICOR,ITRACK) = J1(ICOR,JJET) - MAXDOT*T1(ICOR)           
        END DO                                                          
C                                                                       
C  Normalize these into unit vectors                                    
C                                                                       
        NORME = 0.0                                                     
        NORMM = 0.0                                                     
        DO ICOR=1,3                                                     
          NORME = NORME + EHAT(ICOR,ITRACK)**2                          
          NORMM = NORMM + MHAT(ICOR,ITRACK)**2                          
        END DO                                                          
        NORME = SQRT(NORME)                                             
        NORMM = SQRT(NORMM)                                             
        DO ICOR=1,3                                                     
          EHAT(ICOR,ITRACK) = EHAT(ICOR,ITRACK)/NORME                   
          MHAT(ICOR,ITRACK) = MHAT(ICOR,ITRACK)/NORMM                   
        END DO                                                          
C                                                                       
C  Save projections of T0 onto PERP and MHAT; this plus the directions  
C  and the errors are all we need to calculate chisquared.              
C                                                                       
        ET0(ITRACK) = 0.                                                
        MT0(ITRACK) = 0.                                                
        DO ICOR=1,3                                                     
          ET0(ITRACK) = ET0(ITRACK) + EHAT(ICOR,ITRACK)*T0(ICOR)        
          MT0(ITRACK) = MT0(ITRACK) + MHAT(ICOR,ITRACK)*T0(ICOR)        
        END DO                                                          
C                                                                       
C  Compute the error on the distance between the track                  
C  and the thrust axis. Take the errors from the track D0 and Z0 terms. 
C  Correct the Z error to be perpendicular to the track direction       
C                                                                       
        SIG2_D0 = TERR(1,ITRACK)                                        
        SIG2_ZPERP = TERR(2,ITRACK)*COSL**2                             
        SIG2_D0ZPERP = TERR(3,ITRACK)*COSL                              
        DOTP = 0.0                                                      
        DOTT = 0.0                                                      
        DO ICOR=1,3                                                     
          DOTP = DOTP + EHAT(ICOR,ITRACK)*T_PHAT(ICOR)                  
          DOTT = DOTT + EHAT(ICOR,ITRACK)*T_THAT(ICOR)                  
        END DO                                                          
C                                                                       
C  Compute the errors (squared!) in the ehat and mhat directions; the eh
C  error is 'raw' in the sense that it doesn't yet have the jet         
C  direction error correction.  To get the error in the M direction,    
C  we can just use the fact that it's perpendicular to both ehat        
C  and the track direction                                              
C                                                                       
        SIGE2_RAW(ITRACK) = SIG2_D0*DOTP**2 + SIG2_ZPERP*DOTT**2 +      
     &         2.*SIG2_D0ZPERP*DOTP*DOTT                                
        SIGM2(ITRACK) = SIG2_D0*DOTT**2 + SIG2_ZPERP*DOTP**2 -          
     &         2.*SIG2_D0ZPERP*DOTP*DOTT                                
C                                                                       
C  For the moment, set the corrected eta-direction error to be the      
C  simple eta-direction error                                           
C                                                                       
        SIGE2(ITRACK) = SIGE2_RAW(ITRACK)                               
 1231   CONTINUE                                                        
      END DO                                                            
C                                                                       
C  Setup tensors and iterations to find the IP                          
C                                                                       
      DO ICOR=1,3                                                       
        OLD_IP(ICOR) = -1000.                                           
      END DO                                                            
      IMETA = 0                                                         
C                                                                       
C  Meta-iteration loop on track errors                                  
C                                                                       
 1444 CONTINUE                                                          
C                                                                       
C  Put the beamspot information into the intial tensors                 
C                                                                       
      ALPHA = 0.0                                                       
      DO ICOR=1,3                                                       
        ALPHA = ALPHA + BP(ICOR)**2/SIG2_BP(ICOR)                       
        BETA(ICOR) = BP(ICOR)/SIG2_BP(ICOR)                             
        DO JCOR=1,3                                                     
          GAMMA(ICOR,JCOR) = DELTA(ICOR,JCOR)/SIG2_BP(ICOR)             
        END DO                                                          
      END DO                                                            
C                                                                       
C  Put track information into the initial tensors; first the E direction
C                                                                       
      DO ITRACK=1,NTRACK                                                
        IF(GOODE(ITRACK))THEN                                           
          ALPHA = ALPHA + ET0(ITRACK)**2/SIGE2(ITRACK)                  
          DO ICOR=1,3                                                   
            BETA(ICOR) = BETA(ICOR) +                                   
     &      ET0(ITRACK)*EHAT(ICOR,ITRACK)/SIGE2(ITRACK)                 
            DO JCOR=1,3                                                 
              GAMMA(ICOR,JCOR) = GAMMA(ICOR,JCOR) +                     
     &        EHAT(ICOR,ITRACK)*EHAT(JCOR,ITRACK)/SIGE2(ITRACK)         
            END DO                                                      
          END DO                                                        
        END IF                                                          
C                                                                       
C  Same for the M direction                                             
C                                                                       
        IF(GOODM(ITRACK))THEN                                           
          ALPHA = ALPHA + MT0(ITRACK)**2/SIGM2(ITRACK)                  
          DO ICOR=1,3                                                   
            BETA(ICOR) = BETA(ICOR) +                                   
     &      MT0(ITRACK)*MHAT(ICOR,ITRACK)/SIGM2(ITRACK)                 
            DO JCOR=1,3                                                 
              GAMMA(ICOR,JCOR) = GAMMA(ICOR,JCOR) +                     
     &        MHAT(ICOR,ITRACK)*MHAT(JCOR,ITRACK)/SIGM2(ITRACK)         
            END DO                                                      
          END DO                                                        
        END IF                                                          
      END DO                                                            
C                                                                       
C  Copy the saved tensors                                               
C                                                                       
      ALPHA_SAVE = ALPHA                                                
      DO ICOR=1,3                                                       
        BETA_SAVE(ICOR) = BETA(ICOR)                                    
        DO JCOR=1,3                                                     
          GAMMA_SAVE(ICOR,JCOR) = GAMMA(ICOR,JCOR)                      
        END DO                                                          
      END DO                                                            
C                                                                       
C  Solve for chisquared                                                 
C                                                                       
      CALL RSINV(3,GAMMA,3,IERR)                                        
      IF(IERR .NE. 0)THEN                                               
        NUSED(1) = -1                                                   
        NUSED(2) = -1                                                   
        GOTO 1000                                                       
      END IF                                                            
      CHI2_SAVE = ALPHA                                                 
      DO ICOR=1,3                                                       
        DO JCOR=1,3                                                     
          CHI2_SAVE = CHI2_SAVE - BETA(ICOR)*BETA(JCOR)*GAMMA(ICOR,JCOR)
        END DO                                                          
      END DO                                                            
C                                                                       
C  Iterate over outlyers; this is a DO WHILE for stupid compilers.  This
C  checks to see how the chi2 changes if we remove any one track        
C  Do this separately for the 2 directions, starting with the E directio
C                                                                       
1111  CONTINUE                                                          
        MAX_EDIF = 0.0                                                  
        DO ITRACK=1,NTRACK                                              
          IF(GOODE(ITRACK))THEN                                         
C                                                                       
C  Subtract the track contribution to the tensors                       
C                                                                       
            ALPHA = ALPHA_SAVE - ET0(ITRACK)**2/SIGE2(ITRACK)           
            DO ICOR=1,3                                                 
              BETA(ICOR) = BETA_SAVE(ICOR) -                            
     &        ET0(ITRACK)*EHAT(ICOR,ITRACK)/SIGE2(ITRACK)               
              DO JCOR=1,3                                               
                GAMMA(ICOR,JCOR) = GAMMA_SAVE(ICOR,JCOR) -              
     &        EHAT(ICOR,ITRACK)*EHAT(JCOR,ITRACK)/SIGE2(ITRACK)         
              END DO                                                    
            END DO                                                      
C                                                                       
C  Solve for 1-removed chisquared                                       
C                                                                       
            CALL RSINV(3,GAMMA,3,IERR)                                  
            IF(IERR .NE. 0)THEN                                         
              NUSED(1) = -1                                             
              NUSED(2) = -1                                             
              GOTO 1000                                                 
            END IF                                                      
            CHI2 = ALPHA                                                
            DO ICOR=1,3                                                 
              DO JCOR=1,3                                               
                CHI2 = CHI2 - BETA(ICOR)*BETA(JCOR)*GAMMA(ICOR,JCOR)    
              END DO                                                    
            END DO                                                      
C                                                                       
C  Latch on the biggest difference                                      
C                                                                       
            IF(CHI2_SAVE - CHI2 .GT. MAX_EDIF)THEN                      
              MAX_EDIF = CHI2_SAVE-CHI2                                 
              IMIN = ITRACK                                             
            END IF                                                      
          END IF                                                        
        END DO                                                          
C                                                                       
C  If the biggest difference is above the limit, flag off the           
C  offending track and try again.                                       
C                                                                       
        IF(MAX_EDIF .GE. MAX_CHI2)THEN                                  
          TAGE(IMIN) = .TRUE.                                           
          NDOF = NDOF - 1                                               
          CHI2_SAVE = CHI2_SAVE - MAX_EDIF                              
C                                                                       
C  Subtract the bad track from the saved tensors                        
C                                                                       
          ALPHA_SAVE = ALPHA_SAVE - ET0(IMIN)**2/SIGE2(IMIN)            
          DO ICOR=1,3                                                   
            BETA_SAVE(ICOR) = BETA_SAVE(ICOR) -                         
     &      ET0(IMIN)*EHAT(ICOR,IMIN)/SIGE2(IMIN)                       
            DO JCOR=1,3                                                 
              GAMMA_SAVE(ICOR,JCOR) = GAMMA_SAVE(ICOR,JCOR) -           
     &          EHAT(ICOR,IMIN)*EHAT(JCOR,IMIN)/SIGE2(IMIN)             
            END DO                                                      
          END DO                                                        
          GOTO 1111                                                     
        END IF                                                          
C                                                                       
C  Repeat the outlyer search for the perpendicular direction            
C                                                                       
 1112   CONTINUE                                                        
        MAX_MDIF = 0.0                                                  
        DO ITRACK=1,NTRACK                                              
          IF(GOODM(ITRACK))THEN                                         
            ALPHA = ALPHA_SAVE - MT0(ITRACK)**2/SIGM2(ITRACK)           
            DO ICOR=1,3                                                 
              BETA(ICOR) = BETA_SAVE(ICOR) -                            
     &        MT0(ITRACK)*MHAT(ICOR,ITRACK)/SIGM2(ITRACK)               
              DO JCOR=1,3                                               
                GAMMA(ICOR,JCOR) = GAMMA_SAVE(ICOR,JCOR) -              
     &        MHAT(ICOR,ITRACK)*MHAT(JCOR,ITRACK)/SIGM2(ITRACK)         
              END DO                                                    
            END DO                                                      
            CALL RSINV(3,GAMMA,3,IERR)                                  
            IF(IERR .NE. 0)THEN                                         
              NUSED(1) = -1                                             
              NUSED(2) = -1                                             
              GOTO 1000                                                 
            END IF                                                      
            CHI2 = ALPHA                                                
            DO ICOR=1,3                                                 
              DO JCOR=1,3                                               
                CHI2 = CHI2 - BETA(ICOR)*BETA(JCOR)*GAMMA(ICOR,JCOR)    
              END DO                                                    
            END DO                                                      
            IF(CHI2_SAVE - CHI2 .GT. MAX_MDIF)THEN                      
              MAX_MDIF = CHI2_SAVE-CHI2                                 
              JMIN = ITRACK                                             
            END IF                                                      
          END IF                                                        
        END DO                                                          
        IF(MAX_MDIF .GE. MAX_CHI2)THEN                                  
          TAGM(JMIN) = .TRUE.                                           
          NDOF = NDOF - 1                                               
          CHI2_SAVE = CHI2_SAVE - MAX_MDIF                              
          ALPHA_SAVE = ALPHA_SAVE - MT0(JMIN)**2/SIGM2(JMIN)            
          DO ICOR=1,3                                                   
            BETA_SAVE(ICOR) = BETA_SAVE(ICOR) -                         
     &      MT0(JMIN)*MHAT(ICOR,JMIN)/SIGM2(JMIN)                       
            DO JCOR=1,3                                                 
              GAMMA_SAVE(ICOR,JCOR) = GAMMA_SAVE(ICOR,JCOR) -           
     &          MHAT(ICOR,JMIN)*MHAT(JCOR,JMIN)/SIGM2(JMIN)             
            END DO                                                      
          END DO                                                        
          GOTO 1112                                                     
        END IF                                                          
C                                                                       
C  Copy the saved tensors                                               
C                                                                       
      ALPHA = ALPHA_SAVE                                                
      DO ICOR=1,3                                                       
        BETA(ICOR) = BETA_SAVE(ICOR)                                    
        DO JCOR=1,3                                                     
          GAMMA(ICOR,JCOR) = GAMMA_SAVE(ICOR,JCOR)                      
        END DO                                                          
      END DO                                                            
C                                                                       
C  Solve for the IP                                                     
C                                                                       
      CALL RSINV(3,GAMMA,3,IERR)                                        
      IF(IERR .NE. 0)THEN                                               
        NUSED(1) = -1                                                   
        NUSED(2) = -1                                                   
        GOTO 1000                                                       
      END IF                                                            
      DO ICOR=1,3                                                       
        IP(ICOR) =  0.0                                                 
        DO JCOR=1,3                                                     
          IP(ICOR) = IP(ICOR) + BETA(JCOR)*GAMMA(ICOR,JCOR)             
        END DO                                                          
      END DO                                                            
C                                                                       
C  Check to see if the IP has moved significantly (>1 sigma) this       
C  meta-iteration.  If so, re-calculate the errors and start again      
C                                                                       
      DIST = 0.0                                                        
      DO ICOR=1,3                                                       
        DIST = DIST + (OLD_IP(ICOR)-IP(ICOR))**2                        
      END DO                                                            
      DIST_ERR = 0.0                                                    
      IF(DIST .GT. 0.0)THEN                                             
        DO ICOR=1,3                                                     
          DO JCOR=1,3                                                   
            DIST_ERR = DIST_ERR + (OLD_IP(ICOR)-IP(ICOR))*              
     &      (OLD_IP(JCOR)-IP(JCOR))*GAMMA(ICOR,JCOR)/DIST               
          END DO                                                        
        END DO                                                          
      END IF                                                            
      IF(DIST .GT. DIST_ERR .AND. IMETA .LT. MAX_META)THEN              
        IMETA = IMETA + 1                                               
C                                                                       
C  Loop over the tracks, giving them an additional ehat error coming    
C  from their displacement from the IP along the jet direction,         
C  coupled with the error on the jet axis direction.                    
C                                                                       
        NDOF = 0                                                        
        DO ITRACK=1,NTRACK                                              
          TAGE(ITRACK) = .FALSE.                                        
          TAGM(ITRACK) = .FALSE.                                
          IF(GOOD(ITRACK))THEN                                          
            NDOF = NDOF + 2                                             
            DM = MT0(ITRACK)                                            
            DO ICOR=1,3                                                 
              DM = DM - MHAT(ICOR,ITRACK)*IP(ICOR)                      
            END DO                                                      
C                                                                       
C  If the length is significant (>1 sigma), add the                     
C  jet angle error term to this track.                                  
C                                                                       
            SIGM = SQRT(SIGM2(ITRACK))                                  
            IF(DM .GT. SIGM)THEN                                        
C                                                                       
C  Here, we need the true length between the IP and the track,          
C  not just it's projection along the M direction.  When adding         
C  the error, subtract off a generic 1 sigma from the                   
C  length, so that the total statistical error remains balanced.        
C                                                                       
              SIGE2(ITRACK) = SIGE2_RAW(ITRACK) +                       
     &           JET_SIG2*(DM**2-SIGM2(ITRACK))/TANJET2(ITRACK)         
            END IF                                                      
C                                                                       
C  Trim out the positive impact parameter tracks; these                 
C  cannot be used in the mhat (along jet) direction.                    
C  Also through out wildly negative (>3 sigma) tracks.                  
C                                                                       
            IF(DM .LT. NSIGM(1)*SIGM .OR.                               
     &         DM .GT. NSIGM(2)*SIGM )THEN                              
              TAGM(ITRACK) = .TRUE.                                     
              NDOF = NDOF - 1                                           
            END IF                                                      
          END IF                                                        
        END DO                                                          
C                                                                       
C  Save the IP for the next comparison                                  
C                                                                       
        DO ICOR=1,3                                                     
          OLD_IP(ICOR) = IP(ICOR)                                       
        END DO                                                          
C                                                                       
C  Iterate again with corrected errors.                                 
C                                                                       
        GOTO 1444                                                       
      END IF                                                            
C                                                                       
C  Final convergence; solve for the chisquared and pack the error matrix
C                                                                       
      CHI2 = ALPHA                                                      
      DO ICOR=1,3                                                       
        DO JCOR=1,3                                                     
          IP_ERR(ICOR,JCOR) = GAMMA(ICOR,JCOR)                          
          CHI2 = CHI2 - BETA(ICOR)*BETA(JCOR)*GAMMA(ICOR,JCOR)          
        END DO                                                          
      END DO                                                            
C                                                                       
C  1 Constraint/track, 2 constraints from the BP, 3 variables solved for
C                                                                       
      CHI2_DOF = CHI2/MAX(1,NDOF-1)                                     
C                                                                       
C  Number of tracks used, separated by direction                        
C                                                                       
      NUSED(1) = 0                                                      
      NUSED(2) = 0                                                      
      DO ITRACK=1,NTRACK                                                
        IF(GOODE(ITRACK))THEN                                           
          NUSED(1)=NUSED(1)+1                                           
        ELSE                                                            
          SIGE2(ITRACK) = 0.0                                           
        END IF                                                          
        IF(GOODM(ITRACK))THEN                                           
          NUSED(2)=NUSED(2)+1                                           
        ELSE                                                            
          SIGM2(ITRACK)=0.0                                             
        END IF                                                          
      END DO                                                            
      NUSED_SAVE(1) = NUSED(1)                                          
      NUSED_SAVE(2) = NUSED(2)                                          
1000  CONTINUE                                                          
      RETURN                                                            
      END
*DK DVVDPH
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DVVDPH
CH
      SUBROUTINE DVVDPH(N9195,JLAY,JFAC,JWFF,IER)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by G.Waltermann                   28-JUL-1988
C
C!:
C    Inputs    :LAYER = 1/2, NFACE = 1/9 OR 1/15, 
C               NWFF = 1/4 (VDET 91) OR 1/6 (VDET 95)
C    Outputs   :PULSES,IER
C
C ---------------------------------------------------------------------
C
C  This routine fills the common block PULSEDISP for displaying the
C  pulseheights of the hits of a single module of the ALEPH mini-vertex
C
C     IMPLICIT NONE
C
c      INCLUDE 'O_VDET$SRC:VD_BOS.INC'
c      ---------------------------------
c
      INCLUDE 'A_BCS.INC'

      INTEGER NAMIND
      INTEGER Bank_VFHL, Bank_VFPH
C
 	COMMON/MVDPULSE/nclu(2),pulses(1021,2)
C
C  Function names
C
      INTEGER NLINK
C
C  Local variables
C
      INTEGER IADDR,JSTRP,KSTRIP,ISTRIP,KSTRP
      INTEGER IHIT, IBNUM
c--
C     INTEGER VROSTM,VNRDIR,VJWFFW,VABCVU
      INTEGER VROSTM,VNRDIR
      INTEGER status
      INTEGER rstrps,irfreq
      REAL rpitch
c--
      INTEGER       JLAY,JWFF,JFAC
      INTEGER NSTRP,ILAY,IWFF,IFAC,IVIEW,ISTRP
c--
      INTEGER OFFSET,ISIGN
      INTEGER NHIT
      INTEGER VFHLIND, VFPHIND
C
      REAL PH
      INTEGER JWAF
      REAL VUW(3),ABC(3)
      DATA ABC/1.,1.,1./

      INTEGER IPHI,IZED
      DATA IPHI/2/,IZED/1/
      DIMENSION KST(2)
C
C  Initialization
C
      Bank_VFHL = NAMIND('VFHL')
      Bank_VFPH = NAMIND('VFPH')
      IER = 0
C
C  Zero array
C
      NCLU(1) = 0
      NCLU(2) = 0
      CALL UZERO(PULSES,1,2042)
C
C  First, find the needed banks
C
        VFPHIND = IW(BANK_VFPH)
        IF(VFPHIND .EQ. 0)THEN
          IER = 1
          RETURN
        END IF
C
C  Loop over banks
C
        DO WHILE(VFPHIND .GT. 0)
          IBNUM = IW(VFPHIND-2)
          VFHLIND = NLINK('VFHL',IBNUM)
          IF(VFHLIND .EQ. 0)THEN
            IER = 1
            RETURN
          END IF
C
C  Main loop over the hits
C
        NHIT = IW(VFHLIND+2)
        KSTRIP = 0
        DO IHIT=1,NHIT
C
C  Does this hit match the requested module ID?
C
          IADDR = IW(VFHLIND+2+IHIT)
C         CALL VADDUN(IADDR,NSTRP,ILAY,IWFF,IFAC,IVIEW,ISTRP)
          CALL VUNADD(IADDR,NSTRP,ILAY,IWFF,IFAC,IVIEW,ISTRP)
C
C  Check on VDET 91 in PHI-VIEW: (only one wafer per side -> 1 OR 4!)
          IF (N9195.LT.6 .AND. IVIEW.EQ.2) THEN
            IF (JWFF.EQ.2 .AND. IWFF.EQ.1) IWFF=2
            IF (JWFF.EQ.3 .AND. IWFF.EQ.4) IWFF=3 
          END IF
C
          IF (JLAY.EQ.ILAY .AND. JFAC.EQ.IFAC .AND. JWFF.EQ.IWFF) THEN
C
C  Match- pack the needed information into the common
C
            NCLU(IVIEW) = NCLU(IVIEW) + 1
C
C  Now, correct the readout direction of the hits for which module
C
            CALL VJWFFW(ILAY,IFAC,IWFF,JWAF)
            CALL VABCVU(ABC,JWAF,VUW)
C           .................................................. PHI
            IF(     IVIEW.EQ.IPHI) THEN
              IF(ABC(2).NE.VUW(2)) THEN
                ISIGN=-1
                status = VROSTM(2,rstrps,rpitch,irfreq)
                OFFSET = rstrps+1
              ELSE
                ISIGN=1
                OFFSET=0
              END IF
C           .................................................. Z
            ELSE IF(IVIEW.EQ.IZED) THEN
C             -----------------------------VDET91
              IF (N9195.LT.6) THEN
                ISIGN = VNRDIR(IVIEW)
                IF (ABC(1).NE.VUW(3)) ISIGN=-ISIGN
                IF(ISIGN.GT.0)THEN
                  OFFSET = 0
                ELSE
                  status = VROSTM(1,rstrps,rpitch,irfreq)
                  OFFSET = rstrps+1
                END IF
C             -----------------------------VDET95
              ELSE
                IF(ABC(1).NE.VUW(3)) THEN
                  ISIGN=-1
                  status = VROSTM(1,rstrps,rpitch,irfreq)
                  OFFSET = rstrps+1
                ELSE
                  ISIGN=1
                  OFFSET = 0
                END IF
              ENDIF
            END IF
C
C  Pack the array
C
            DO JSTRP=1,NSTRP
C
C  Calculate the strip number in the wafer system
C
              KSTRP = ISTRP+JSTRP-1
C
C  Unpack PH from the bank
C
              PH = IW(VFPHIND+2+KSTRIP+JSTRP)/4.
              ISTRIP = OFFSET + ISIGN*KSTRP
              PULSES(ISTRIP,IVIEW) = PH
            END DO
          END IF
          KST(IVIEW)=ISTRIP
C
C  Advance the pointer in the VFPH bank
C
          KSTRIP = KSTRIP + NSTRP
        END DO
C
C  Update pointer, and go back for the next bank
C
        VFPHIND = IW(VFPHIND-1)
      END DO

      RETURN
      END
c
C======================================================================
C
      SUBROUTINE VDET_DECODE_TRACKS(ISTAT)
C
C--------------------------------------------------------------------
C
CJL AUTHOR: JOCHEN LAUBER  20.5.1990; 22.5.1990
C** ADDED: STORING OF ANGLES FOR HELIX-EXTRAPOLATION....
C**        G. WALTERMANN     AUG. 1990
C**  adapted for DALI  Nov. 93   Gabi:
C**        everything in CM !!!!!!
CJL READS VTXT-BANKS AND FILLS INFORMATION TO COMMON VD_TRACKS
CJL NO INPUT , NO PRINTOUT
CJL OUTPUT:  ISTAT=0 IF O.K.
CJL          ISTAT=1 IF NO VTXT-BANKS ARE AVAILABLE
C--------------------------------------------------------------------
C

      INTEGER MAX_TRACKS,ZP,XYZ,XYC
      PARAMETER (MAX_TRACKS=200, ZP=2, XYZ=3, XYC=3)

      COMMON/VD_TRACKS/  NTRACKS, TRK_FLAG (MAX_TRACKS),
     $                   TRK_NUMBER(MAX_TRACKS),
     $                   TRK_LOCAL (ZP,MAX_TRACKS),
     $                   TRK_GLOBAL(XYZ,MAX_TRACKS),
     $                   TRK_ELLIPS(XYC,MAX_TRACKS),
     $                   TRK_ADDRESS(MAX_TRACKS),
     $                   TRK_MOMENTUM(MAX_TRACKS)

      INTEGER     NTRACKS, TRK_FLAG, TRK_NUMBER
      REAL        TRK_LOCAL, TRK_GLOBAL, TRK_ELLIPS
      INTEGER     TRK_ADDRESS
      REAL        TRK_MOMENTUM

C--------------------------------------------------------------------
C
      INTEGER ISTAT,IERR
      INTEGER NVTXT
      INTEGER NAMIND,NAMI,NIND
      INTEGER BANKNR,WORDSHIT,HITSTRK
      INTEGER NT,INDEX,NHITT
      INTEGER IWAF,ILAY,IPHI,IVIEW

C
      REAL BFIELD,ALFIEL
      REAL RDUMMY
      LOGICAL FIRST/.TRUE./
      INTEGER NLINK
C--------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_BMACRO.INC'
C
C     ......................................... ENTRY:
C
      IF(FIRST)THEN
        FIRST = .FALSE.
        BFIELD = ALFIEL(BFIELD)
        NAMI=NAMIND('VTXT')
      END IF
      ISTAT=0
      NT=0
C
C
C     ................................. EXTRAPOLATE TRACKS INTO VDET
C
      CALL VTRKEX(BFIELD,IERR)
C
C     ..................................... ADD THE BANKS TO THE E LIST
C
      CALL BLIST(IW,'E+','VTXTVTERFRFTFRTL')
C
C     ............ JL LOOP OVER ALL EXISTING VTXT-BANKS IN INCREASING NUMBER
C
      NIND = NAMI + 1
   10 NIND = IW(NIND-1)
      IF (NIND .NE. 0) THEN
        BANKNR = IW(NIND-2)
        NVTXT = NLINK('VTXT',BANKNR)
        WORDSHIT = LCOLS(NVTXT)
        HITSTRK  = LROWS(NVTXT)
C
C       ....................... JL LOOP OVER (HITSTRK) HITS ON THE SAME TRACK
C
        DO NHITT=1,HITSTRK
          INDEX=KROW(NVTXT,NHITT)
C         ... JL COUNT HITS, AND IF THERE IS NO MORE SPACE IN THE ARRAYS, EXIT !
          NT=NT+1
          IF (NT .GT. MAX_TRACKS) GOTO 30
C         .............................................. JL TRACK-FLAG
          TRK_FLAG(NT) = IW(INDEX+2)
C         ............................... JL TRACK-REFERENCE-NUMBER TO ORIGIN...
          TRK_NUMBER(NT) = BANKNR
C         ............................... JL STORE WAFER ADDRESS
          TRK_ADDRESS(NT) = IW(INDEX+1)
          CALL VADEWA(TRK_ADDRESS(NT),ILAY,IWAF,IPHI,IVIEW)
C
C         ................................ TRACK MOMENTUM
C
          TRK_MOMENTUM(NT) = SQRT(RW(INDEX+11)**2 +
     &      RW(INDEX+12)**2 + RW(INDEX+13)**2 )
          TRK_MOMENTUM(NT) = SIGN(TRK_MOMENTUM(NT),RW(INDEX+11))
C         ............................ JL GLOBAL COORDINATES IN CM, AS BEFORE
          TRK_GLOBAL(1,NT)=RW(INDEX+8)
          TRK_GLOBAL(2,NT)=RW(INDEX+9)
          TRK_GLOBAL(3,NT)=RW(INDEX+10)
C         ........... JL LOCAL COORDINATES IN 100 MICRON SHIFTED BY CENTERPOINT
          TRK_LOCAL(1,NT)=RW(INDEX+3)
          TRK_LOCAL(2,NT)=RW(INDEX+4)
C
C         ..................... JL ERRORS OF LOCAL COORDINATES IN 100 MICRON
C         ..................................  AND CORRELATION BETWEEN THEM
          RDUMMY=RW(INDEX+5)
          IF(RDUMMY.EQ.0.0) RDUMMY=0.015
          TRK_ELLIPS(1,NT)=RDUMMY
          RDUMMY=RW(INDEX+6)
          IF(RDUMMY.EQ.0.0) RDUMMY=0.025
          TRK_ELLIPS(2,NT)=RDUMMY
          TRK_ELLIPS(3,NT)=RW(INDEX+7)

        ENDDO
        GOTO 10
      ENDIF
   30 NTRACKS=NT
C     .......................................  ***
   99 RETURN
      END
C======================================================================
C
      SUBROUTINE VDET_ELLIPS(CENTER,ELLIPS,SCALE,NPT,XE,YE,XCH,YCH)
C
C  CALCULATES AN ELLIPS DEFINED AS A POLYLINE AND ROTATES IT OVER PHI
C
C-------------------------------------------------------------
C
      IMPLICIT NONE

      REAL CENTER(*),ELLIPS(*)
      REAL SCALE
      REAL XE(61),YE(61),XCH(2,2),YCH(2,2)
C
      INTEGER I
      INTEGER NPT
      REAL PI/3.14159/
C          RADIAN=PI*2/(NPT-1)
      REAL RADIAN/0.10472/
C
      REAL XCENT, YCENT, XC, YC, RHO
C
      REAL SIGQX, SIGQY, COVXY
      REAL TALPHA, ALPHA
      REAL CPHI, SPHI, CPHIQ, SPHIQ, CSPHI
      REAL RDIDND
      REAL A,B
C
      REAL THETA
      REAL X,Y
      REAL ASC, ASS, BSC, BSS
      INTEGER LDEB/0/
C
C----------------------------------------------------------------
C
C  ENTRY:
C
      XCENT = CENTER(2)
      YCENT = CENTER(1)
      IF(LDEB.EQ.0) THEN
        XC    = ELLIPS(2)
        YC    = ELLIPS(1)
      ELSE
        XC    = MIN(ELLIPS(2),.512)
        YC    = MIN(ELLIPS(1),.512)
      END IF
      RHO   = ELLIPS(3)
C******************
C
C  CALCULATE A/B AND ANGLE ALPHA FOR ELLIPS FROM X/Y-CORRELATION:
C
      SIGQX = XC**2
      SIGQY = YC**2
      COVXY = RHO * XC * YC
      IF(SIGQX .NE. SIGQY)THEN
        TALPHA = 2. * COVXY / (SIGQX-SIGQY)
        ALPHA  = 0.5 * ATAN(TALPHA)
      ELSE
        ALPHA = SIGN(PI/4.,COVXY)
      END IF
      CPHI = COS(ALPHA)
      SPHI = SIN(ALPHA)
      CPHIQ = CPHI**2
      SPHIQ = SPHI**2
      CSPHI = 2.*COVXY*CPHI*SPHI
      RDIDND = SIGQX*SIGQY - COVXY**2
C
      A = SQRT(  MAX(  SIGQX*CPHIQ - CSPHI + SIGQY*SPHIQ ,  0.) )
      B = SQRT(  MAX(  SIGQX*SPHIQ + CSPHI + SIGQY*CPHIQ ,  0.) )
C
C      IF (RDIDND.GT.0.) THEN
C        RDIV1 = SIGQY*CPHIQ - CSPHI + SIGQX*SPHIQ
C        RDIV2 = SIGQX*CPHIQ + CSPHI + SIGQY*SPHIQ
C        A     = SQRT(RDIDND / RDIV1)
C        B     = SQRT(RDIDND / RDIV2)
C      ELSE
C        IF (ALPHA.GE.0.0) THEN
C          A = SQRT ( SIGQX + SIGQY )
C          B = 0.0
C        ELSE
C          A = 0.0
C          B = SQRT ( SIGQX + SIGQY )
C        ENDIF
C      ENDIF

C***********************
C
C  CALCULATE ELLIPS IN WAFER-COORDS:
C
      THETA=0.
      DO I=1,NPT-1
        X    = A*SCALE*COS(THETA)
        Y    = B*SCALE*SIN(THETA)
        XE(I)= XCENT + X*CPHI - Y*SPHI
        YE(I)= YCENT + X*SPHI + Y*CPHI
        THETA= THETA + RADIAN
      ENDDO
      XE(NPT)=XE(1)
      YE(NPT)=YE(1)
C
C
C  SETUP CROSS-HAIRS ON ELLIPS
C
      ASC = A*SCALE*CPHI
      ASS = A*SCALE*SPHI
      BSC = B*SCALE*CPHI
      BSS = B*SCALE*SPHI

      XCH(1,1)= XCENT+ASC
      YCH(1,1)= YCENT+ASS
      XCH(2,1)= XCENT-ASC
      YCH(2,1)= YCENT-ASS

      XCH(1,2)= XCENT-BSS
      YCH(1,2)= YCENT+BSC
      XCH(2,2)= XCENT+BSS
      YCH(2,2)= YCENT-BSC
C
      RETURN
      END
