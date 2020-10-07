      SUBROUTINE YTPVTX(IER)
C
C----------------------------------------------------------*
C!    reconstructs the primary vertex
CKEY YTOP VERTEX
C!    Author :     G. Lutz   30/11/87
C!    Modified :   M. Bosman 01/12/88
C!    Modified :   S. Wasserbaech 09/01/90
C!    Rewritten:   G. Lutz     /02/91
C!    Modified :   M. Bosman 11/07/91
C!    Modified  :  G. Lutz   30/03/92
C!
C!
C!    Description
C!    ===========
C!    This routine reconstructs the primary vertex.
C!    It has been adapted by G.Lutz from the routine YTOPVT
C!    to use the FRFT bank with VDET information
C!    when available and "reconstructed neutral tracks"
C!    like V0 or gamma-conversion.
C!    If only the ITC-TPC track bank exists and the reconstruction
C!    of V0 and gamma conversions is not requested, it performs
C!    an identical task to YTOPVT.
C!
C!    Return codes
C!    ============
C!    IER= 0 .... primary vertex found
C!       = 1 .... no primary vertex found
C!       =11 .... no space to create PYFR
C!       =12 .... no space to create PYER
C!       =13 .... no FRFT bank found
C!       =14 .... error return from mult.scatt. rout. UMSERR
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
      PARAMETER(JYNFMO=1,JYNFTL=2,JYNFP0=3,JYNFD0=4,JYNFZ0=5,JYNFEM=6,
     +          JYNFC2=21,JYNFDF=22,JYNFCH=23,JYNFND=24,JYNFPT=25,
     +          JYNFNM=26,JYNFPM=27,JYNFPV=28,JYNFPC=29,LYNFTA=29)
      PARAMETER(JPYETY=1,JPYEVX=2,JPYEVY=3,JPYEVZ=4,JPYEVM=5,JPYEC2=11,
     +          JPYEDF=12,LPYERA=12)
      PARAMETER(JPYFTN=1,JPYFVN=2,LPYFRA=2)
C! beam crossing position
      COMMON/YBCRTO/BCROSS(3),VBCROS(6)
C! YTOP array dimensions
      PARAMETER(NMSIZZ=31,MKDIMM=6,MAXTRK=186,MAXHLX=124,MAXNTR=62,
     &  MXVTOP=10,MXMULS=10,MAXVXP=50,MAXEXC=60,MXVTYP=2)
C! YTOP particle masses
      PARAMETER(    JPAFEP=1,JPAFEM=2,JPAFMP=3,JPAFMM=4,
     &              JPAFPP=5,JPAFPM=6,JPAFKP=7,JPAFKM=8,
     &              JPAFPR=9,JPAFPB=10,JPAFPH=11,JPAFPZ=12,
     &              JPAFKZ=13,JPAFLA=14,JPAFLB=15   )
      COMMON/YPMASS/ YPMASS(20)
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
C! YTOP vertex parameters
      COMMON/YVTXTO/ NEVTOP,NVXTOP,NVXLOT,
     &  VXTOP0(3,MXVTOP,MXVTYP),VVXTOP(6,MXVTOP,MXVTYP),
     &  CHVTOP(MXVTOP,MXVTYP),DRVTOP(MXVTOP,MXVTYP),
     &  MULTVX(MXVTOP,MXVTYP),
     &  IXTRVT(MKDIMM,MXVTOP,MXVTYP),IXVQLI(MAXTRK,MXVTOP),
     &  CHIVXI(MAXTRK,MXVTOP),
     &  NLOTRK,ILOTRK(MAXTRK),
     &  JTRVXA(MAXTRK,MXVTYP),NTRVXA(MAXTRK,MXVTYP),
     &  NEVVTX(3,MXVTOP),NMULVX(MXMULS,MXVTYP),
     &  NMULTA(MXVTOP,MXVTYP),MULMAT(MXMULS,MXMULS)
C! YTOP track parameters
      COMMON/YTRKTO/NGTRTO,IPTRTO(MAXTRK),
     &              PTRECT(MAXTRK),PRECTO(MAXTRK),
     &              KPIDF0(MAXTRK),KPORF0(MAXTRK)
C
C!---------------------------------------------------------*
      DIMENSION IPTR(MAXTRK),IXTRI(MAXTRK)
      DIMENSION IXNU(MAXTRK)
      DIMENSION JBTR(MAXTRK),MPTR(MAXTRK)
      DIMENSION VHXIN(15,MAXTRK)
      DIMENSION IXSORT(MAXTRK),ABSMOM(MAXTRK)
      LOGICAL LMRK
      LOGICAL LFIRST
C
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
C!---------------------------------------------------------*
C
C     dimension of buffer for track errors
      DATA LVHXIN / 15/
      DATA LFIRST/.TRUE./
C
C
C-- Define the logical unit for printout
C
      LOUT = IW(6)
C
      IF(LFIRST) THEN
C     GENERATE MARKERS
        CALL YMKZER(1,NMSIZZ,MKHAD)
        CALL YMKSET(1,NMSIZZ,MKHAD,JPAFPP)
        CALL YMKSET(1,NMSIZZ,MKHAD,JPAFKP)
        CALL YMKSET(1,NMSIZZ,MKHAD,JPAFPR)
        CALL YMKSET(1,NMSIZZ,MKHAD,JPAFPM)
        CALL YMKSET(1,NMSIZZ,MKHAD,JPAFKM)
        CALL YMKSET(1,NMSIZZ,MKHAD,JPAFPB)
        CALL YMKZER(1,NMSIZZ,MKV0)
        CALL YMKSET(1,NMSIZZ,MKV0,JPAFKZ)
        CALL YMKSET(1,NMSIZZ,MKV0,JPAFLA)
        CALL YMKSET(1,NMSIZZ,MKV0,JPAFLB)
        CALL YMKZER(1,NMSIZZ,MKPH)
        CALL YMKSET(1,NMSIZZ,MKPH,JPAFPH)
        LFIRST=.FALSE.
      ENDIF
C
      IER = 0
C--   fill the summary information
      NEPVTO = NEPVTO + 1
C     FIND THE FRFT BANK INDICES
      CALL YDEFRF(KFRFT0,KFRFT,KFRTL,IFAIL)
      IF(IFAIL.NE.0) GOTO 999
C
C-- Perform the different selections of tracks
C
C-- Count the tracks selected for the 1rst search of PVTX
      ITCAND = 0
      DO 10 K=1,NGTRTO
        I=IPTRTO(K)
C--     First the optionnal selections
        IPTR(K)=I
        MPTR(I)=I
C--     Select tracks with minivertex hits
        IF(LRMVPV) THEN
          IF(ITABL(KFRTL,I,JFRTNV).EQ.0) THEN
            IPTR(K)=0
            MPTR(I)=0
          ENDIF
        ENDIF
C--     Remove leptons (non hadrons)
        IF(LRLPVX) THEN
          CALL YMKAND(1,NMSIZZ,MKHAD,KPIDF0(I),IDUM,LMRK)
          IF(LMRK) THEN
            IPTR(K)=0
            MPTR(I)=0
          ENDIF
        ENDIF
C--     Remove V0 decay tracks
        IF(LVZERS) THEN
          CALL YMKAND(1,NMSIZZ,MKV0,KPORF0(I),IDUM,LMRK)
          IF(.NOT.LMRK) THEN
            IPTR(K)=0
            MPTR(I)=0
          ENDIF
        ENDIF
C--     Remove gamma conversion decay tracks
        IF(LCONVS) THEN
          CALL YMKAND(1,NMSIZZ,MKPH,KPORF0(I),IDUM,LMRK)
          IF(.NOT.LMRK) THEN
            IPTR(K)=0
            MPTR(I)=0
          ENDIF
        ENDIF
C--     Select tracks with at least MNTHPV hits in TPC
        IF(ITABL(KFRTL,I,JFRTNT).LT.MNTHPV) THEN
          IPTR(K)=0
          MPTR(I)=0
        ENDIF
C--     Finally select tracks with momentum above PMINRQ
        IF(ABS(PRECTO(I)).LT.PMINRQ) THEN
          IPTR(K)=0
        ENDIF
C
C--     save the absolute momentum for sorting if needed
        ABSMOM(K) = ABS(PRECTO(I))
        IF(IPTR(K).NE.0) THEN
          ITCAND = ITCAND+1
          IXSORT(ITCAND) = K
        ENDIF
   10 CONTINUE
C--     check that not too many tracks are selected
C--     for the first step of the primary vertex search
C--     limit = MXTSPV
      IF(ITCAND.GT.MXTSPV) THEN
        CALL SORTZV(ABSMOM,IXSORT,ITCAND,1,1,1)
        DO 11 KK=MXTSPV+1,ITCAND
          IPTR(IXSORT(KK))=0
   11   CONTINUE
      ENDIF
C
C--- add neutral tracks
C
      KYNFT=IW(NAMIND('YNFT'))
      IF(KYNFT.LE.0) THEN
        NEU=0
      ELSE
        NEU=0
        NT=LROWS(KYNFT)
        DO 15 I=1,NT
C---- QUESTION------
          CALL YMKAND(1,NMSIZZ,MKV0,KPIDF0(I+MAXHLX),IDUM,LMRK)
          IF(.NOT.LMRK) THEN
C--      ADD VZEROS
            NEU=NEU+1
            IXNU(NEU)=I
          ENDIF
   15   CONTINUE
      ENDIF
C
C-- Add multiple scattering component to ITC-TPC track errors
C-- and copy them to an array
C
      DO 40 I=1,LROWS(KFRFT)
        IF(I.GT.MAXTRK) GOTO 40
        IFRFT=KROW(KFRFT,I)
        DO 41 J=1,15
   41   VHXIN(J,I)=RW(IFRFT+JFRFEM+J-1)
        IF(KFRFT.EQ.KFRFT0) THEN
C     --ITC-TPC bank, add multiple scattering
          NITC=ITABL(KFRTL,I,JFRTNI)
          CALL UMSERR(1,NITC,0,RW(IFRFT+JFRFIR),
     &                VHXIN(1,I),VHXIN(1,I),IFAIL)
          IF(IFAIL.NE.0) GOTO 995
        ENDIF
   40 CONTINUE
C
C
C-- First step of the primary vertex reconstrucion
C
      CALL YPRIVX(LROWS(KFRFT),BCROSS(1),VBCROS(1),NGTRTO,IPTR(1),
     &            NEU,IXNU(1),
     +            LCOLS(KFRFT),LVHXIN,
     +            RW(KFRFT+LMHLEN+JFRFIR),VHXIN,
     &            LCOLS(KYNFT),LCOLS(KYNFT),
     &            RW(KYNFT+LMHLEN+JYNFMO),RW(KYNFT+LMHLEN+JYNFEM),
     +            VXTOP0(1,1,1),VVXTOP(1,1,1),MULTVX(1,1),
     +            IXTRVT(1,1,1),CHVTOP(1,1),IXVQLI(1,1),IFLV)
C
C     Quit if no initial vertex found.
C     INDVX gives the index in VXTOP0, etc., for the
C     final vertex.
      IF (IFLV .EQ. 0) THEN
C     GOOD VERTEX
        NVXTOP = 1
        INDVX = 2
      ELSEIF (IFLV .EQ. 10) THEN
C     GOOD 2-PRONG VERTEX
        NDFVX = 3
        NVXTOP = 1
        INDVX = 1
        GO TO 210
      ELSE
        IF(LBCRFD) THEN
C    USE BEAM CROSSING AS FIRST STEP PRIM. VTX.
          NDFVX = 0
          NVXTOP = 1
          INDVX = 2
        ELSE
          NVXTOP = 0
          GO TO 996
        ENDIF
      ENDIF
C
C-- Second step of primary vertex reconstruction
C-- add tracks with momentum above PMINRA that
C-- that are compatible with the primary vertex.
C-- Don't do this, however, if the vertex position
C-- comes from two tracks plus the beam spot (IFLV=10).
C
      NHX=0
      DO 100 K=1,NGTRTO
        I=IPTRTO(K)
        IF(ABS(PRECTO(I)).LT.PMINRA) GO TO 100
C--   and apply the optionnal selections
        IF(MPTR(I).EQ.0) GO TO 100
        NHX=NHX+1
        JBTR(NHX)=I
  100 CONTINUE
C
      IF(LVBCR0) THEN
        NVX=1
      ELSE
        NVX=0
      ENDIF
C
      CALL YVXBLD(NHX,NEU,JBTR,IXNU,
     +  RW(KFRFT+LMHLEN+JFRFIR),LCOLS(KFRFT),VHXIN,LVHXIN,
     +  RW(KYNFT+LMHLEN+JYNFMO),LCOLS(KYNFT),RW(KYNFT+LMHLEN+JYNFEM),
     +  LCOLS(KYNFT),
     +  VXTOP0(1,1,1),VVXTOP(1,1,1),
     +  CHVTOP(1,1),(2*MULTVX(1,1)+(NVX-1)*3),
     +  IXTRVT(1,1,1),VXTOP0(1,1,2),VVXTOP(1,1,2),
     +  CHVTOP(1,2),NDFVX,IXTRVT(1,1,2),1,
     +  NADTR,IXTRI,CHIVXI(1,1))
C
      MULTVX(1,2)=MULTVX(1,1)+NADTR
C-- complement IXVQLI with the indices of the tracks that
C-- have been added in the second step
      IF(NADTR.EQ.0) GOTO 210
      II=MULTVX(1,1)
      DO 200 IADD=1,NADTR
        II=II+1
        IXVQLI(II,1) = IXTRI(IADD)
  200 CONTINUE
  210 CONTINUE
C-- save the reconstructed primary vertex in the bank PYER
      KPYER=IW(NAMIND('PYER'))
      IF(KPYER.GT.0) THEN
C----- bank already exists
        KLASTV = LROWS(KPYER)+1
      ELSE
        KLASTV = 1
      ENDIF
C-- we book here the space for the bank
      CALL AUBOS('PYER',0,LMHLEN+LPYERA*KLASTV,KPYER,IRET)
      IF(IRET.EQ.2) GOTO 998
      IW(KPYER+LMHCOL) = LPYERA
      IW(KPYER+LMHROW) = KLASTV
      IPYER = KROW(KPYER,KLASTV)
C-- vertex type is 1 for primary vertex,
C--             or 3 if beam position was used with 2 tracks(Bhabha)
C--             or 5 if beam position was used as first step vtx.
      IW(IPYER+JPYETY) = 1
      IF (IFLV .EQ. 10) THEN
        IW(IPYER+JPYETY) = 3
      ELSE
        IF(IFLV .NE. 0) IW(IPYER+JPYETY) = 5
      ENDIF
C-- copy the vertex position
      CALL UCOPY(VXTOP0(1,1,INDVX),RW(IPYER+JPYEVX),3)
C-- copy the variances
      CALL UCOPY(VVXTOP(1,1,INDVX),RW(IPYER+JPYEVM),6)
C-- copy the chisq
      RW(IPYER+JPYEC2) = CHVTOP(1,INDVX)
C-- copy the number of degrees of freedom
      IW(IPYER+JPYEDF) = NDFVX
C-- pointer to reconstructed mother tracks
C-- 0 for primary vertex
C     IW(IPYER+JPYEPR) = 0
C-- save the track indices belonging to the primary vertex
C-- in the bank PYFR
      KPYFR=IW(NAMIND('PYFR'))
      IF(KPYFR.GT.0) THEN
C----- bank already exists
        KLAST = LROWS(KPYFR)+MULTVX(1,INDVX)
      ELSE
        KLAST = MULTVX(1,INDVX)
      ENDIF
      CALL AUBOS('PYFR',0,LMHLEN+LPYFRA*KLAST,KPYFR,IRET)
      IF(IRET.EQ.2) GOTO 997
      IW(KPYFR+LMHCOL) = LPYFRA
      IW(KPYFR+LMHROW) = KLAST
      MOMTTR = 0.
      DO 300 ITR = 1,MULTVX(1,INDVX)
        IPYFR = KROW(KPYFR,KLAST-MULTVX(1,INDVX)+ITR)
        IW(IPYFR+JPYFVN) = KLASTV
C DISTINGUISH BETWEEN FRFT TRACKS AND RECONSTRUCTED TRACKS
        IF(IXVQLI(ITR,1).LE.MAXHLX) THEN
          IW(IPYFR+JPYFTN) = IXVQLI(ITR,1)
        ELSE
C RECONSTRUCTED TRACK STORE ADDRESS OF 1ST MASS ASSIGNMENT IN YNMA
          KYNFT=IW(NAMIND('YNFT'))
          IW(IPYFR+JPYFTN) = ITABL(KYNFT,IXVQLI(ITR,1)-MAXHLX,JYNFPM) +
     +      10000
        ENDIF
        MOMTTR = MOMTTR + ABS(PRECTO(IXVQLI(ITR,1)))
  300 CONTINUE
      IF(MULTVX(1,INDVX).GT.0)
     >           MOMTTR = MOMTTR / FLOAT(MULTVX(1,INDVX))
C--   fill the summary information
      NRPVTO = NRPVTO + 1
      ATPVTO = ATPVTO + FLOAT(MULTVX(1,INDVX))
      AMPVTO = AMPVTO + MOMTTR
      IF (NDFVX .GT. 0) ACPVTO = ACPVTO +
     >           (CHVTOP(1,INDVX)/FLOAT(NDFVX))
      CALL VADD(APPVTO(1),VXTOP0(1,1,INDVX),APPVTO(1),3)
      RETURN

  996 KYFLAG(10) = KYFLAG(10) + 1
      IER=1
      RETURN
  997 KYFLAG(11) = KYFLAG(11) + 1
      IER=11
      RETURN
  998 KYFLAG(12) = KYFLAG(12) + 1
      IER=12
      RETURN
  999 KYFLAG(13) = KYFLAG(13) + 1
      IER=13
      RETURN
  995 KYFLAG(14) = KYFLAG(14) + 1
      IER=14
      RETURN
      END
