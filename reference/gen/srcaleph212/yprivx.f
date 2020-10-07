      SUBROUTINE YPRIVX(NHXTOT,BCR,VBCR,NHX,IXHX,NEU,IXNU,
     &  NSHX,NSVHX,HXIN,VHXIN, NSNU,NSVNU,TNU,VTNU,
     &  PRVX,VPRVX,NTPVX,IXTVP,CHISQ,ITRPVX,IFL)
C
C----------------------------------------------------------*
C!    reconstructs the primary vertex
CKEY YTOP PRIMARY VERTEX
C!    Author :     G. Lutz   30/11/87
C!    Modified :   M. Bosman 01/12/88
C!    Modified :   S. Wasserbaech 09/01/90  Added NPRTR=2 procedure.
C!    Rewritten:   G. Lutz   10/01/91
C!    Modified :   M. Bosman 12/07/91
C!    Modified  :  G. Lutz   30/03/92
C!
C!    replaces routine yprimv. main difference: common fit of beam
C!    crossing and tracks
C!    exclusion of tracks with large distance from beam crossing
C!    exclusion of itc only tracks
C!
C!
C!    Description
C!    ===========
C!    This routine finds the primary vertex
C!    from beam crossing and set of track candidates
C!
C!    NHXTOT ..... TOTAL # OF TRACKS (HELICES)
C!    BCR(I) ..... X,Y,Z OF AVERAGE BEAM CROSSING POINT
C!    VBCR(I) .... CORRESPONDING VARIANCE
C!    NHX ........ NUMBER OF TRACKS (HELICES)
C!    IXHX(I) ..... TRACK INDICES
C!    NEU ........ NUMBER OF NEUTRAL TRACKS (HELICES)
C!    IXNU(I) ..... TRACK INDICES
C!    NSHX ....... SPACING BETWEEN CONSECUTIVE HELIX PARAMETERS
C!    NSVHX ...... SPACING BETWEEN CONSECUTIVE HELIX VARIANCES
C!    HXIN(I) .... BUFFER FOR HELIX PARAMETERS
C!    VHXIN(I) ... BUFFER FOR HELIX VARIANCES
C!    NSNU ....... SPACING BETWEEN CONSECUTIVE NEUTR.TR. PARAMETERS
C!    NSVHX ...... SPACING BETWEEN CONSECUTIVE NEUTR. TR. VARIANCES
C!    TNU(I)  .... BUFFER FOR NEUTRAL TRACK PARAMETERS
C!    VTNU(I)  ... BUFFER FOR NEUTRAL TRACK VARIANCES
C!    PRVX(I) .... X,Y,Z OF PRIMARY VERTEX
C!    VPRVX(I).... CORRESPONDING VARIANCES
C!    NTPVX ...... # OF TRACKS IN PRIM. VTX
C!    IXTVP ..... MARKER WORD FOR TRACKS IN VERTEX
C!    CHISQ ...... VERTEX CHISQ
C!    ITRPVX(I) .. INDICES OF TRACKS IN PRIM. VTX
C!    IFL ........ PROBLEM FLAG
C!                    0 -> successful completion
C!                   10 -> successful completion for two-track vertex
C!                 else -> failure
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
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)
      PARAMETER(JFRTIV=1,JFRTNV=2,JFRTII=3,JFRTNI=4,JFRTNE=5,JFRTIT=6,
     +          JFRTNT=7,JFRTNR=8,LFRTLA=8)
C! YTOP array dimensions
      PARAMETER(NMSIZZ=31,MKDIMM=6,MAXTRK=186,MAXHLX=124,MAXNTR=62,
     &  MXVTOP=10,MXMULS=10,MAXVXP=50,MAXEXC=60,MXVTYP=2)
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
C! YTOP vertex summary
      COMMON/YVXPTO/ NTRV00(MAXVXP),
     +   ITRV00(MKDIMM,MAXVXP),VTXMS0(3,MAXVXP),
     +   CHIVMS(MAXVXP),NVPOSS,VARVMS(6,MAXVXP),
     +   MRKEXC(MKDIMM,MAXEXC)
C!---------------------------------------------------------*
C
      DIMENSION BCR(*),VBCR(*),IXHX(*),HXIN(*),VHXIN(*),
     &                         IXNU(*),TNU(*),VTNU(*),
     &                         PRVX(*),VPRVX(*),ITRPVX(*)
C
C
      DIMENSION IXTVP(*)
C
C  ARRAY FOR INDICES OF TRACKS MISSING PRIMARY VERTEX
      DIMENSION MKTRK(MAXTRK)
C
      DIMENSION JTREQ(MAXVXP)
C
C     INDICES OF NON CROSSING TRACKS RETURNED FROM YVPOSS
      DIMENSION KSNGL(MAXTRK)
C
C DIMENSION CHIVXT(MAXTRK),IPRTR(MAXTRK)
      DIMENSION CHIVXT(MAXTRK),IPRTR(MAXTRK)
C
C     TEMPORARY VERTEX
      DIMENSION WTX(3),VARWX(6)
C
C     DIMENSION MKTV1(MKDIMM)
      DIMENSION MKTV1(MKDIMM)
C
      LOGICAL LSET
C
C     LIMIT FOR IMPACT PARAMETER OF TRACKS :
C     IN R  2 CM
      DATA AIMPLR/2./
C     IN Z  5 CM
      DATA AIMPLZ/5./
C
      DATA NENTY/0/
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
C-- Define the logical unit for printout
C
      LOUT = IW(6)
C
      IFL=0
C
C  ENTRY ONCE PER EVENT
C
C     Copy beam crossing into output vertex for the case that
C     no better vertex is found
      DO I=1,3
        PRVX(I)=BCR(I)
      ENDDO
      DO I=1,6
        VPRVX(I)=VBCR(I)
      ENDDO
      NTPVX=0
      CALL YMKZER(MKDIMM,NMSIZZ,IXTVP(1))
      CHISQ=0.
C
      CALL YDEFRF(KFRFT0,KFRFT,KFRTL,IFAIL)
      IF(IFAIL.NE.0) THEN
        IER=1
        RETURN
      ENDIF
C
C     SELECT TRACKS COMPATIBLE WITH BEAM CROSSING POINT
      NPRTR=0
C
      DO 1100 I=1,NHX
        JHX=IXHX(I)
        IF(JHX.LE.0) GO TO 1100
C     REJECT TRACKS WITH LARGE Z-DISTANCE FROM BEAM CROSSING
        Z0=RTABL(KFRFT,JHX,JFRFZ0)
        IF(RTABL(KFRFT,JHX,JFRFEM+14).LT.0.) THEN
          DZ0=0.
          WRITE(LOUT,8876) JHX
 8876     FORMAT(' NEGATIVE ERROR2 FOR Z0 IN TRACK JHX=',I5)
        ELSE
          DZ0=SQRT(RTABL(KFRFT,JHX,JFRFEM+14))
        ENDIF
        TANT=RTABL(KFRFT,JHX,JFRFTL)
        COST=1./SQRT(1.+TANT**2)
        DZMAX=AIMPLR/COST
        DZMAX=SQRT(AIMPLZ**2+DZMAX**2)

        IF(ABS(Z0).GT.(DZMAX+3.*DZ0)) GO TO 1100
C     REJECT TRACKS WITH LARGE R-DISTANCE FROM BEAM CROSSING
        D0=RTABL(KFRFT,JHX,JFRFD0)
        IF(RTABL(KFRFT,JHX,JFRFEM+9).LT.0.) THEN
          DD0=0.
          WRITE(LOUT,8877) JHX
 8877     FORMAT(' NEGATIVE ERROR2 FOR D0 IN TRACK JHX=',I5)
        ELSE
          DD0=SQRT(RTABL(KFRFT,JHX,JFRFEM+9))
        ENDIF
        DRMAX=AIMPLR
        IF(ABS(D0).GT.(DRMAX+3.*DD0)) GO TO 1100
C
        NPRTR=NPRTR+1
        IPRTR(NPRTR)=JHX
C
 1100 CONTINUE
C
C
C+-+- PROTECT AGAINST THE CASE WHERE NO TRACKS COMPATIBLE
C+-+- WITH THE BEAM PROFILE ARE FOUND
      IF((NPRTR+NEU).EQ.0) THEN
        NVPOSS=0
        IFL=2
        RETURN
      ENDIF
C
C+-+-
      NGT =NHX
C
  100 CONTINUE
C+-+-
C     Special treatment for events with NPRTR=2:
C     Find primary vertex using beam crossing position
C     and its size in the xy-plane.
C     The size is set to one meter in z, so this
C     constraint is effectively removed.
C     The number of degrees of freedom in this fit
C     with two tracks is therefore 2 + 2*2 - 3 = 3.
      IF (NPRTR .EQ. 2 .AND. NEU .EQ. 0) THEN
        I1 = IPRTR(1)
        I2 = IPRTR(2)
        DO 110 I=1,3
          WTX(I) = BCR(I)
  110   CONTINUE
        DO 120 I=1,5
          VARWX(I) = VBCR(I)
  120   CONTINUE
        VARWX(6) = 100.**2
C
        CALL YFTVTR(1,2,0,.FALSE.,WTX,VARWX,
     &              IPRTR,NSHX,NSVHX,HXIN,VHXIN,
     &              IXNU,NSNU,NSVNU,TNU,VTNU,
     &              PRVX,VPRVX,CHISQ,IFAIL)
        IF(IFAIL.GT.0) THEN
          IER=1
          RETURN
        ENDIF
C
        NTPVX = 2
C       CHISQ = CHI1 + CHI2
C     CHECK FOR AN ACCEPTABLE CHISQ : maximum value 50.
        IF(CHISQ.GT.50.) THEN
          NVPOSS = 0
          IFL = 1
          RETURN
        ENDIF
        ITRPVX(1) = I1
        ITRPVX(2) = I2
        IFL = 10
        CALL YMKZER(MKDIMM,NMSIZZ,IXTVP(1))
        CALL YMKSET(MKDIMM,NMSIZZ,IXTVP(1),I1)
        CALL YMKSET(MKDIMM,NMSIZZ,IXTVP(1),I2)
        RETURN
      ENDIF
C
C     parameters for vertex search
      MXMUL=NPRTR
      JTREQ(1)=0
      NEXCL=0
C
      IF(LVBCR0) THEN
        NVX=1
        MNMUL=1
      ELSE
        NVX=0
        MNMUL=2
      ENDIF
C
      CALL YVPOSS(3,NVX,NPRTR,NEU,BCR,VBCR,
     &  IPRTR,NSHX,NSVHX,
     &  HXIN,VHXIN,
     &  IXNU,NSNU,NSVNU,TNU,VTNU,
     &  JTREQ,0,MNMUL,MXMUL,NEXCL
     &  ,NSNGL,KSNGL)
C
C
      IF(NVPOSS.LE.0) THEN
        IFL=1
        RETURN
      ENDIF
C
C     SELECT BEST VERTEX CANDIDATE
      CHMIN=1.E+30
      IVBST=0
      DO 1200 IV=1,NVPOSS
        IF(CHIVMS(IV).LT.CHMIN) THEN
          IVBST=IV
          CHMIN=CHIVMS(IV)
        ENDIF
 1200 CONTINUE
C
C
C     STORE PRIMARY VERTEX
      DO 1300 I=1,3
        PRVX(I)=VTXMS0(I,IVBST)
 1300 CONTINUE
      DO 1400 I=1,6
        VPRVX(I)=VARVMS(I,IVBST)
 1400 CONTINUE
      CALL YMKCOP(MKDIMM,ITRV00(1,IVBST),IXTVP(1))
      CHISQ=CHIVMS(IVBST)
      CALL YMKEXP(MKDIMM,NMSIZZ,ITRV00(1,IVBST),MAXTRK,NTPVX,ITRPVX)
C
C     REQUIRE MORE THAN 2 TRACKS IF NO BEAM CONSTRAINT IS REQUIRED
      IF((NTPVX+NVX).LE.2) THEN
        IFL=1
        RETURN
      ENDIF
C
C
C
      RETURN
      END
