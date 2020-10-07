
      SUBROUTINE YTSTRK(IER)
C
C----------------------------------------------------------*
C!    Selects tracks to be considered for vertex search
CKEY YTOP
C!    Author :     G. Lutz   30/11/87
C!    Modified :   M. Bosman 01/12/88
C!    Modified :   G. Lutz      02/91
C!
C!
C!    Description
C!    ===========
C!    This routine calculates the momentum of the
C!    reconstructed tracks and selects the tracks
C!    to be considered in the first step of vertex
C!    reconstruction
C!
C!---------------------------------------------------------*
      SAVE
C     LOGIC FLAG FOR MULTIPLE REJECTION OF SAME TRACK
      LOGICAL LREJ
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
      DIMENSION IFAIL(2)
      DOUBLE PRECISION EHI(5,5,2),EHTEMP(5,5,2)
C!---------------------------------------------------------*
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
C     MAXIMUM/MINIMUM MOMENTUM OF TRACKS
      DATA PRMAX/200./
C     MAXIMUM RELATIVE CURVATURE ERROR
      DATA DCURM/.3/
C
      DATA ICNER1/0/
C
      LOUT = IW(6)

C
      IER = 0
C
C     conversion radius of track <=> momentum
C     radius in meter , B in Tesla, p in GeV/c  q in units of e
C
C      p = 0.29979 * q * B * r
C
C     R[cm] = ROFP * P[Gev/c]:
C
      ROFP = 1./ (0.29979 * BFIELD / 10.) * 100.
C
C     initialize bank indices
      CALL YDEFRF(KFRFT0,KFRFT,KFRTL,JFAIL)
      IF(JFAIL.NE.0) GOTO 999
      IF(KFRFT0.EQ.0) KFRFT0=KFRFT
C
      NGTRTO = 0
C
C
      DO 10 I=1,LROWS(KFRFT)
        IF(I.GT.MAXHLX) THEN
C --- GET RUN AND EVENT NUMBER
          CALL ABRUEV(IRUN,IEVT)
          WRITE(LOUT,9838) IRUN,IEVT,LROWS(KFRFT),MAXHLX
 9838     FORMAT(' YTSTRK : RUN',I7,' EVENT',I8,
     &      ' MAXIMUM NB OF CHARGED TRACKS EXCEEDED',I8,'>',I8)
          GOTO 11
        ENDIF
        LREJ=.FALSE.
        NGTRTO=NGTRTO+1
        PTRECT(I)=-1./ROFP/RTABL(KFRFT,I,JFRFIR)
        PRECTO(I)=PTRECT(I)*SQRT(1.+RTABL(KFRFT,I,JFRFTL)**2)
        IPTRTO(NGTRTO)=I
C--     Reject tracks that have a non-zero error flag
C       in the FRFT bank
        IERR = MOD(ITABL(KFRFT,I,JFRFNO),10)
        IF(IERR.NE.0.AND.IERR.NE.4) THEN
          LREJ=.TRUE.
        ENDIF
C--     REJECT TRACKS THAT HAVE A NON-POSITIVE DEFINITE ERROR MATRIX
C
C     INVERT 5X5 ERROR MATRIX
        IJ=0
        DO II=1,5
          DO JJ=1,II
            IJ=IJ+1
            EHI(II,JJ,1)=RTABL(KFRFT0,I,JFRFEM+IJ-1)
            EHI(JJ,II,1)=EHI(II,JJ,1)
            EHI(II,JJ,2)=RTABL(KFRFT,I,JFRFEM+IJ-1)
            EHI(JJ,II,2)=EHI(II,JJ,2)
          ENDDO
        ENDDO
        DO II=1,5
          DO JJ=1,5
            EHTEMP(II,JJ,1)=EHI(II,JJ,1)
            EHTEMP(II,JJ,2)=EHI(II,JJ,2)
            IF(II.NE.JJ) EHTEMP(II,JJ,1)=EHI(II,JJ,1)/
     &            SQRT(EHI(II,II,1)*EHI(JJ,JJ,1))
            IF(II.NE.JJ) EHTEMP(II,JJ,2)=EHI(II,JJ,2)/
     &            SQRT(EHI(II,II,2)*EHI(JJ,JJ,2))
          ENDDO
        ENDDO
        DO K=1,2
          CALL DSINV(5,EHI(1,1,K),5,IFAIL(K))
        ENDDO
C
        IF(IFAIL(1).NE.0.OR.IFAIL(2).NE.0) THEN
C
          LREJ=.TRUE.
        ENDIF
C      REJECT VERY LOW MOMENTUM TRACKS
        IF(ABS(PRECTO(I)).LT.PMINSE ) THEN
          LREJ=.TRUE.
        ENDIF
C      REJECT VERY HIGH MOMENTUM TRACKS
        IF(ABS(PRECTO(I)).GT.PRMAX) THEN
          LREJ=.TRUE.
        ELSE
C      REJECT TRACKS WITH LARGE ERROR ON CURVATURE
          CURV=RTABL(KFRFT,I,JFRFIR)
          DCURV=SQRT(RTABL(KFRFT,I,JFRFEM))
          IF(DCURV/ABS(CURV).GT.DCURM) THEN
            LREJ=.TRUE.
          ENDIF
        ENDIF
C
        IF(LREJ) NGTRTO=NGTRTO-1
   10 CONTINUE
   11 CONTINUE
C
      RETURN
  999 CONTINUE
      IER=1
      RETURN
      END
