      SUBROUTINE VTRKEX(BFLD,IER)
C----------------------------------------------------------------------
C!  Creates VDET track extrapolation and error banks
CKEY VDET TRACK
C!  -
C!
C!   Author   :- Jochen A. Lauber       4-OCT-1990
C!             modified David Brown 14-10-90
C!             modified 23-9-92 to include face extrapolation
C!
C!   Inputs:  BFLD- Z magnetic field in KG
C!
C!   Outputs:
C!        -    VTXT banks
C!        -    VTER banks
C!        -    VDMS banks
C!        -    IER 0=o.k. otherwise error
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!    called once per event, loops over tracks
C!    Track extrapolation to the Mini-Vertex-Detektor Wafers
C!    Framework for calling VTXTRK
C!======================================================================
      SAVE
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
C!  Defines flag bits used in VTXT flag word
C
      INTEGER NITCHB,NTPCHB,MCHISB,NSIGUB,NSIGWB
      PARAMETER (NITCHB = 1)
      PARAMETER (NTPCHB = 2)
      PARAMETER (MCHISB = 4)
      PARAMETER (NSIGUB = 8)
      PARAMETER (NSIGWB = 16)
      INTEGER JVTXWI,JVTXHF,JVTXUC,JVTXWC,JVTXSU,JVTXSW,
     +          JVTXUW,JVTXXC,JVTXYC,JVTXZC,JVTXPV,
     +          JVTXPU,JVTXPW,JVTXUR,JVTXUT,JVTXUP,
     +          JVTXUD,JVTXUZ,JVTXWR,JVTXWT,JVTXWP,
     +          JVTXWD,JVTXWZ,LVTXTA
      PARAMETER(JVTXWI=1,JVTXHF=2,JVTXUC=3,JVTXWC=4,JVTXSU=5,JVTXSW=6,
     +          JVTXUW=7,JVTXXC=8,JVTXYC=9,JVTXZC=10,JVTXPV=11,
     +          JVTXPU=12,JVTXPW=13,JVTXUR=14,JVTXUT=15,JVTXUP=16,
     +          JVTXUD=17,JVTXUZ=18,JVTXWR=19,JVTXWT=20,JVTXWP=21,
     +          JVTXWD=22,JVTXWZ=23,LVTXTA=23)
      INTEGER JVTEEM,LVTERA
      PARAMETER(JVTEEM=1,LVTERA=36)
      INTEGER JVTPMI,JVTPMT,JVTPMC,JVTPUS,JVTPWS,LVTPAA
      PARAMETER(JVTPMI=1,JVTPMT=2,JVTPMC=3,JVTPUS=4,JVTPWS=5,LVTPAA=5)
C
C  Declare FRFT,FRTL hac parameter variables
C
      PARAMETER(JFRTIV=1,JFRTNV=2,JFRTII=3,JFRTNI=4,JFRTNE=5,JFRTIT=6,
     +          JFRTNT=7,JFRTNR=8,LFRTLA=8)
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)
C
C  New bank VDMS for VDET multiple scattering
C
      INTEGER LVDMSA,JVDMWI,JVDMFL,JVDMRA,JVDMUC,JVDMWC
      INTEGER JVDMPV,JVDMPU,JVDMPW,JVDMCU,JVDMSG
      PARAMETER(JVDMWI=1,JVDMFL=2,JVDMRA=3,JVDMUC=4,JVDMWC=5,JVDMPV=6,
     +          JVDMPU=7,JVDMPW=8,JVDMCU=9,JVDMSG=10,LVDMSA=10)
C
C  Outputs
C
      INTEGER IER
C
C  Functions
C
      INTEGER   NAMIND,NLINK
C
C  Track extrapolation parameters per track- Dimensioned max possible
C  for 2 layers with overlap.
C
      INTEGER NHIWA,IFLAG(4),JFLAG(4),IWAFI(4),IERR
      REAL LCPOS(2,4),LCMOM(3,4),LCERR(36),LCDER(10,4),GBPOS(3,4)
      INTEGER NFACE,IFACE(4),ISORT(4)
      REAL FPOS(2,4),FMOM(3,4),FRAD(4)
C
C  Modified track error matrix (upper diagonal)
C
      REAL TERRM(15)
C
C  Track and bank parameters
C
      INTEGER NVTXT,NVTER,NVTPA,NFRFT,NFRTL,NVDMS
      INTEGER IVTXT,IVTER,INDEX,IVDMS
      INTEGER NTRKS,ITRAK,ITFLG,JFACE,KFACE
      INTEGER IFRFT,IFRTL
      INTEGER NITC,NTPC,NDOF
      INTEGER IWAF,ILEN
      INTEGER IGARB1,IGARB2,IGARB3
      REAL CHIDF,MOM,BFLD,CHI2
C
C  VTPA bank parameters
C
      INTEGER IVTPA,MNITC,MNTPC
      REAL CHIMX,RSIGU,RSIGW
      REAL SLGHT
      PARAMETER (SLGHT = CLGHT*1.E-5)
C
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
C  Data statements
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C  Statement functions
C
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
C
C
C  Preset flag to success
C
      IER=0
C
C  Define bos formats first time through
C
      IF(FIRST)THEN
        FIRST = .FALSE.
        NVTXT = NAMIND('VTXT')
        NVTER = NAMIND('VTER')
        NVTPA = NAMIND('VTPA')
        NVDMS = NAMIND('VDMS')
        NFRFT = NAMIND('FRFT')
        NFRTL = NAMIND('FRTL')
        CALL BKFMT('VTXT','2I,(2I,21F)')
        CALL BKFMT('VTER','2I,(F)')
        CALL BKFMT('VDMS','2I,(2I,8F)')
C
C  reference to thresholds in bank VTPA
C  VDET Track Extrapolation Parameters
C  These are used to set bits in the flag word,
C  and to determine the extended active area of a wafer
C
        IVTPA = IW(NVTPA)
        IF (IVTPA.GT.0) THEN
          MNITC = ITABL(IVTPA,1,JVTPMI)
          MNTPC = ITABL(IVTPA,1,JVTPMT)
          CHIMX = RTABL(IVTPA,1,JVTPMC)
          RSIGU = RTABL(IVTPA,1,JVTPUS)
          RSIGW = RTABL(IVTPA,1,JVTPWS)
        ELSE
C
C  Setup some nominal values
C
          MNITC = 4
          MNTPC = 6
          CHIMX = 4.
          RSIGU = 4.
          RSIGW = 4.
        ENDIF
      ENDIF
C
C    drop previous banks
C
      CALL BDROP(IW,'VTXTVTERVDMS')
C
C  Find Track banks- these MUST be julia banks, if
C  POT banks are to be used they must first be expanded.
C
      IFRFT = IW(NFRFT)
      IFRTL = IW(NFRTL)
      IF( IFRFT .LE. 0 .OR. IFRTL .LE. 0) THEN
C
C no bank present- return
C
        IER = 5
        RETURN
      ENDIF
C
C  track loop
C
      NTRKS= LROWS(IFRFT)
      DO 100 ITRAK=1,NTRKS
C
C  Find the # of hits, and compute the CHI/DOF
C
        NITC = ITABL(IFRTL,ITRAK,JFRTNI)
        NTPC = ITABL(IFRTL,ITRAK,JFRTNT)
        CHI2 = RTABL(IFRFT,ITRAK,JFRFC2)
        NDOF = ITABL(IFRFT,ITRAK,JFRFDF)
        IF (NDOF.GT.0)THEN
          CHIDF = CHI2/NDOF
        ELSE
          CHIDF = 100.
        END IF
C
C  Move the track error matrix to the inner VDET layer
C
        CALL UMSERR(4,NITC,0,RW(KROW(IFRFT,ITRAK)+JFRFIR),
     &       RW(KROW(IFRFT,ITRAK)+JFRFEM),TERRM,IERR)
        IF(IERR .NE. 0)CALL ALTELL('VTRKEX: UMSERR error',0,'RETURN')
C
C  Extrapolate the track to the wafers and faces
C
        CALL VTXTRK(RW(KROW(IFRFT,ITRAK)+JFRFIR),
     &              TERRM,RSIGU,RSIGW,
     &  NHIWA,IWAFI,LCPOS,LCMOM,LCERR,LCDER,GBPOS,IFLAG,IERR,
     &  NFACE,IFACE,FPOS,FMOM,FRAD,JFLAG)
C
C  Sum up the errors
C
        IER = IOR(IER,IERR)
C
C  If there are any hit wafers, build banks and fill them.
C
        IF (NHIWA .GT. 0 .AND. IERR .EQ. 0)THEN
C
C  Set bits if the values PASS the cuts given
C
          ITFLG = 0
          IF(NITC.GE.MNITC)ITFLG = IOR(ITFLG,NITCHB)
          IF(NTPC.GE.MNTPC)ITFLG = IOR(ITFLG,NTPCHB)
          IF(CHIDF.LE.CHIMX)ITFLG = IOR(ITFLG,MCHISB)
C
C  Setup the output banks- bank number is simply the track number
C  from FRFT
C
          CALL AUBOS('VTXT',ITRAK,NHIWA*LVTXTA+LMHLEN,IVTXT,IGARB1)
          CALL AUBOS('VTER',ITRAK,LVTERA+LMHLEN,IVTER,IGARB2)
          IF(IVTXT .LE. 0 .OR. IVTER .LE. 0)THEN
C
C  Not enough BOS space- return with error
C
            IER = 6
            RETURN
          ELSE IF(IGARB1 .NE. 0 .OR. IGARB2 .NE. 0)THEN
C
C  Garbage collection- update the bank pointers
C
            IFRFT = IW(NFRFT)
            IFRTL = IW(NFRTL)
            IVTER = NLINK('VTER',ITRAK)
            IVTXT = NLINK('VTXT',ITRAK)
          END IF
C
C  Load mini-headers
C
          IW(IVTXT+LMHROW) = 0
          IW(IVTXT+LMHCOL) = LVTXTA
          IW(IVTER+LMHROW) = 1
          IW(IVTER+LMHCOL) = LVTERA
C
C  Store error matrix- first zero any unused elements in the array
C
          IF(NHIWA .LT. 4)THEN
            CALL UZERO(LCERR,NHIWA*(2*NHIWA+1)+1,36)
          END IF
          CALL UCOPY(LCERR,IW(IVTER+LMHLEN+JVTEEM),LVTERA)
C
C  Loop over the wafers
C
          DO 110 IWAF=1,NHIWA
C
C   store information in bank
C
            INDEX = KNEXT(IVTXT)
C  wafer number
            IW(INDEX+JVTXWI) = IWAFI(IWAF)
C  Flag
            IW(INDEX+JVTXHF) = IOR(IFLAG(IWAF),ITFLG)
C  local positions
            RW(INDEX+JVTXUC) = LCPOS(1,IWAF)
            RW(INDEX+JVTXWC) = LCPOS(2,IWAF)
C  local position errors- pick out the ones needed
            RW(INDEX+JVTXSU) = SQRT(LCERR((2*IWAF-1)*IWAF))
            RW(INDEX+JVTXSW) = SQRT(LCERR((2*IWAF+1)*IWAF))
            RW(INDEX+JVTXUW) = LCERR((2*IWAF+1)*IWAF-1)/
     &      (RW(INDEX+JVTXSU)*RW(INDEX+JVTXSW))
C  Global positions
            RW(INDEX+JVTXXC) = GBPOS(1,IWAF)
            RW(INDEX+JVTXYC) = GBPOS(2,IWAF)
            RW(INDEX+JVTXZC) = GBPOS(3,IWAF)
C  Momentum in local frame- multiply by signed momentum
            MOM = -BFLD*SLGHT*SQRT(1.0+RTABL(IFRFT,ITRAK,JFRFTL)**2)/
     &      RTABL(IFRFT,ITRAK,JFRFIR)
            RW(INDEX+JVTXPV) = LCMOM(1,IWAF)*MOM
            RW(INDEX+JVTXPU) = LCMOM(2,IWAF)*MOM
            RW(INDEX+JVTXPW) = LCMOM(3,IWAF)*MOM
C  Derivatives track parameters WRT local frame
            RW(INDEX+JVTXUR) = LCDER(1,IWAF)
            RW(INDEX+JVTXUT) = LCDER(2,IWAF)
            RW(INDEX+JVTXUP) = LCDER(3,IWAF)
            RW(INDEX+JVTXUD) = LCDER(4,IWAF)
            RW(INDEX+JVTXUZ) = LCDER(5,IWAF)
            RW(INDEX+JVTXWR) = LCDER(6,IWAF)
            RW(INDEX+JVTXWT) = LCDER(7,IWAF)
            RW(INDEX+JVTXWP) = LCDER(8,IWAF)
            RW(INDEX+JVTXWD) = LCDER(9,IWAF)
            RW(INDEX+JVTXWZ) = LCDER(10,IWAF)
C
C  Advance row pointer
C
            IW(IVTXT+LMHROW) = IW(IVTXT+LMHROW) + 1
C
C  End of loop over wafers
C
  110     CONTINUE
        END IF
C
C  Now take care of face extrapolation
C
        IF(NFACE .GT. 0 .AND. IERR .EQ. 0)THEN
C
C  Make face extrapolation banks
C
          CALL AUBOS('VDMS',ITRAK,NFACE*LVDMSA+LMHLEN,IVDMS,IGARB3)
          IF(IVDMS .LE. 0)THEN
C
C  Not enough BOS space- return with error
C
            IER = 6
            RETURN
          ELSE IF(IGARB3 .NE. 0)THEN
C
C  Garbage collection; make sure we still have FRFT
C
            IFRFT = IW(NFRFT)
            IFRTL = IW(NFRTL)
          END IF
C
C  Preset the mini-header
C
          IW(IVDMS+LMHROW) = 0
          IW(IVDMS+LMHCOL) = LVDMSA
C
C  Sort the rows in VDMS according to the extrapolation radius
C
          CALL SORTZV(FRAD,ISORT,NFACE,1,1,0)
C
C  Loop over the faces and fill the bank
C
          DO KFACE=1,NFACE
            JFACE = ISORT(KFACE)
            INDEX = KNEXT(IVDMS)
C  wafer number
            IW(INDEX+JVDMWI) = IFACE(JFACE)
C  Flag; set to true only if the extrapolated position actually
C  lies in the face, not just the buffer region
            IW(INDEX+JVDMFL) = JFLAG(JFACE)
C  Extrapolated radius
            RW(INDEX+JVDMRA) = FRAD(JFACE)
C  Face coordinates
            RW(INDEX+JVDMUC) = FPOS(1,JFACE)
            RW(INDEX+JVDMWC) = FPOS(2,JFACE)
C  Track momentum
            RW(INDEX+JVDMPV) = FMOM(1,JFACE)
            RW(INDEX+JVDMPU) = FMOM(2,JFACE)
            RW(INDEX+JVDMPW) = FMOM(3,JFACE)
C  Track curvature
            RW(INDEX+JVDMCU) = RW(KROW(IFRFT,ITRAK)+JFRFIR)
C  Advance row pointer
            IW(IVDMS+LMHROW) = IW(IVDMS+LMHROW) + 1
          END DO
        END IF
  100 CONTINUE
C
C  End of loop over tracks
C
      RETURN
      END
