      SUBROUTINE YTOPNW
C
C----------------------------------------------------------*
C!    Steering routine for TOPOLOGY reconstruction
CKEY YTOP STEERING
C!    Author :     M. Bosman , G. Lutz   30/11/88
C!    Modified:    G.Lutz                  /02/91
C!    Modified  :  G. Lutz   30/03/92
C!
C!    Input: Comdeck YPARTO
C!
C!    Description
C!    ===========
C!    This routine steers the topology reconstruction
C!    according to the options selected with the YOPT cards
C!    By default, it reconstruct the primary vertex without
C!    beam crossing constraint.
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
C! beam crossing position
      COMMON/YBCRTO/BCROSS(3),VBCROS(6)
C! YTOP array dimensions
      PARAMETER(NMSIZZ=31,MKDIMM=6,MAXTRK=186,MAXHLX=124,MAXNTR=62,
     &  MXVTOP=10,MXMULS=10,MAXVXP=50,MAXEXC=60,MXVTYP=2)
C! YTOP track parameters
      COMMON/YTRKTO/NGTRTO,IPTRTO(MAXTRK),
     &              PTRECT(MAXTRK),PRECTO(MAXTRK),
     &              KPIDF0(MAXTRK),KPORF0(MAXTRK)
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
      DIMENSION DBCR(3)
C
      DATA NENTY/0/
C
      DATA ICNER1/0/,ICNER2/0/,ICNER3/0/,ICNER4/0/,ICNER5/0/,

     &     ICNER6/0/,ICNER7/0/,ICNER8/0/,ICNER9/0/
C
C-- Define the logical unit for printout
C
      LOUT = IW(6)
C
C
C --- GET RUN AND EVENT NUMBER
      CALL ABRUEV(IRUN,IEVT)
C
C-- drop banks from old event
      IND= NDROP('PYER',0)
      IND= NDROP('PYFR',0)
      IND= NDROP('YNFT',0)
      IND= NDROP('YNMA',0)
      IND= NDROP('YNPE',0)
      IND= NDROP('YNTR',0)
      IND= NDROP('YCFT',0)
      IND= NDROP('YCMA',0)
      IND= NDROP('YCPE',0)
      IND= NDROP('YCTR',0)
C--
      CALL YDEFRF(KFRFT0,KFRFT,KFRTL,IFAIL)
      IF(KFRFT.EQ.0) THEN
        ICNER1=ICNER1+1
        RETURN
      ENDIF
      IF(KFRTL.EQ.0) THEN
        ICNER2=ICNER2+1
        RETURN
      ENDIF
C FIND TIME LIMIT
      TLIMIT=0.
      INT=IW(NAMIND('TIME'))
      IF(INT.NE.0) TLIMIT=FLOAT(IW(INT+1))

C-- RESET PARTICLE ORIGIN AND IDENTIFICATION FLAG
      DO 1 I=1,MAXTRK
        KPORF0(I)=0
        KPIDF0(I)=0
    1 CONTINUE
C-- intialize the set of tracks selected for vertex search
C
      CALL YTSTRK(IER)
      IF(IER.NE.0) THEN
        ICNER3=ICNER3+1
        RETURN
      ENDIF
C
C-- get particle identity assignement possibilities
C
      IF(LRLPVX.OR.LCONVS.OR.LVZERS) THEN
        CALL YPIDAS(IER)
        IF(IER.NE.0) THEN
          ICNER4=ICNER4+1
          RETURN
        ENDIF
      ENDIF
C
C-- find gamma conversions before reconstructing the
C-- primary vertex
C
CHECK TIME
      CALL TIMAL(TLEFT)
      IF(TLEFT.LT.TLIMIT) RETURN
      IF(LCONVS) THEN
        CALL YTCONV(IER)
        IF(IER.NE.0) THEN
          ICNER5=ICNER5+1
        ENDIF
      ENDIF
C
C-- find V0 tracks before reconstructing the
C-- primary vertex
C
CHECK TIME
      CALL TIMAL(TLEFT)
      IF(TLEFT.LT.TLIMIT) RETURN
      IF(LVZERS) THEN
        CALL YTRV0S(IER)
        IF(IER.NE.0) THEN
          ICNER6=ICNER6+1
        ENDIF
      ENDIF
C
C-- reconstruct the primary vertex
C
CHECK TIME
      CALL TIMAL(TLEFT)
      IF(TLEFT.LT.TLIMIT) RETURN
      IF(LRPVTX) THEN
        CALL YTPVTX(IER)
        IF(IER.GT.1) THEN
          ICNER7=ICNER7+1
        ENDIF
      ENDIF
C
C-- search for secondary vertices
C
CHECK TIME
      CALL TIMAL(TLEFT)
      IF(TLEFT.LT.TLIMIT) RETURN
      IF(LRSVTX) THEN
        CALL YTOSVT(IER)
        IF(IER.NE.0) THEN
          ICNER8=ICNER8+1
        ENDIF
      ENDIF
C
C-- call user routine
C
CHECK TIME
      CALL TIMAL(TLEFT)
      IF(TLEFT.LT.TLIMIT) RETURN
      IF(LRUSER) THEN
        CALL YRUSER(IER)
        IF(IER.NE.0) THEN
          ICNER9=ICNER9+1
        ENDIF
      ENDIF
C
      RETURN
      END
