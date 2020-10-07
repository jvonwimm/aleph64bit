      SUBROUTINE YVXBLD(NHX,NEU,IXHX,IXNU,
     &  HXI,NSHX,VHXI,NSVHX,
     &  TNU,NSNU,VTNU,NSVNU,
     &  VTX1,VVTX1,CHIV1,NDFV1,
     &  MKTV1,VTX2,VVTX2,CHVX2,NDFV2,MKTV2,ICONL,
     &  NADTR,IXTRI,CHVXI)
C
C----------------------------------------------------------*
C!    build up vertex by adding sequentially tracks
CKEY YTOP VERTEX
C!    Author :     G. Lutz   30/11/87
C!    Modified :   M. Bosman 01/12/88
C!REWRITTEN:   G. LUTZ   JULY 91
C!    corrected 6.2.92 G.Lutz
C!
C!    Description
C!    ===========
C!    builds up a vertex by adding sequentially
C!    the most compatible track
C!
C!    NHX ......... NUMBER OF CHARGED TRACKS TO BE INVESTIGATED
C!    NEU ......... NUMBER OF NEUTRAL TRACKS TO BE INVESTIGATED
C!    IXHX(I) ..... INDICES OF HELICES
C!    IXNU(I) ..... INDICES OF NEUTRAL TRACKS
C!    HXI(I) ...... HELIX PARAMETERS
C!    NSHX ....... SPACING BETWEEN CONSECUTIVE HELICES
C!    VHXI(I) ..... HELIX VARIANCES
C!    NSVHX ...... SPACING BETWEEN CONSECUTIVE HELICES
C!    TNU(I) ...... NEUTRAL TRACK PARAMETERS
C!    NSNU ....... SPACING BETWEEN CONSECUTIVE HELICES
C!    VTNU(I) ..... HELIX VARIANCES
C!    NSVNU ...... SPACING BETWEEN CONSECUTIVE HELICES
C!    VTX1 ........ COORD. OF INITIAL VERTEX
C!    VVTX1 ....... VARIANCES  "
C!    CHIV1 ...... CHISQ OF   "
C!    NDFV1 ...... # OF DEGREES OF FREEDOM OF INITIAL VERTEX
C!    MKTV1 ...... TRACK MARKER HAS BIT I SET IF TRACK I IS
C!                  INCLUDED IN INITAL VERTEX
C!    VTX2..MKTV2  SAME FOR FINAL VERTEX
C!    ICONL ...... =1 99% CONFIDENCE LIMIT
C!                  =2 99% CONFIDENCE LIMIT FOR VERTEX
C!    NADTR ...... # OF ADDED TRACKS
C!    IXTRI ....... INDICES OF ADDED TRACKS
C!    CHVXI ...... CHISQ OF INTERMEDIATE VERTICES
C----------------------------------------------------------*
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C! YTOP array dimensions
      PARAMETER(NMSIZZ=31,MKDIMM=6,MAXTRK=186,MAXHLX=124,MAXNTR=62,
     &  MXVTOP=10,MXMULS=10,MAXVXP=50,MAXEXC=60,MXVTYP=2)
C
C     DIMENSION MKTV1(MKDIMM),MKTV2(MKDIMM)
      DIMENSION MKTV1(*),MKTV2(*)
C
      DIMENSION IXHX(*),IXNU(*),VTX1(*),VVTX1(*),VTX2(*),VVTX2(*)
      DIMENSION WTX(3),VWTX(6),WTX2(3),VWTX2(6),CHVXI(*),IXTRI(*)
      DIMENSION MKTRK(MAXTRK)
      DIMENSION CONLM(20,2)
C
      LOGICAL LMRK
C
C
C
      DATA CONLM/ 6.6, 9.2,11.4,13.3,15.1,16.8,18.5,20.1,21.7,23.2,
     +            24.7,26.2,27.7,29.1,30.6,32.0,33.4,34.8,36.2,37.6,
     &            6.6, 9.2,11.4,13.3,15.1,16.8,18.5,20.1,21.7,23.2,
     +            24.7,26.2,27.7,29.1,30.6,32.0,33.4,34.8,36.2,37.6/
C
C
      DATA NENTY/0/
C
C-- Define the logical unit for printout
C
      LOUT = IW(6)
C
C
      NADTR=0
C
      DO 100 I=1,3
        WTX(I)=VTX1(I)
        VTX2(I)=VTX1(I)
  100 CONTINUE
      DO 200 I=1,6
        VWTX(I)=VVTX1(I)
        VVTX2(I)=VVTX1(I)
  200 CONTINUE
      CHIWX=CHIV1
      CHVX2=CHIV1
      NDFWX=NDFV1
      NDFV2=NDFV1
C
C     MKTV2=MKTV1
      CALL YMKCOP(MKDIMM,MKTV1(1),MKTV2(1))
C
      NADTR=0
C
      NTRKS=NHX+NEU
      DO ITRK=1,NTRKS
        MKTRK(ITRK)=1
      ENDDO
      DO 2000 IADD=1,NTRKS
C ****======= TEMPORARY CODING
        NDF=NDFV2+2
        IF(NDF.GT.20) GO TO 860
        XILIM=CONLM(NDF,ICONL)
        GO TO 870
  860   CONTINUE
        XILIM=2.25*NDF
        IF(ICONL.EQ.2) XILIM=2.25*NDF
C
  870   CONTINUE
        NMWD0=(NTRKS-1)/NMSIZZ+1
C
        CHMIN=1.E+30
        IBTRK=0
        DO 1000 ITRK=1,NTRKS
          IF(MKTRK(ITRK).EQ.0) GO TO 1000
          IF(ITRK.LE.NHX) THEN
            NH=1
            NU=0
            IXH=IXHX(ITRK)
            JTIX=IXH
            IF(IXH.LE.0) THEN
              MKTRK(ITRK)=0
              GO TO 1000
            ENDIF
          ELSE
            NH=0
            NU=1
            IXN=IXNU(ITRK-NHX)
            JTIX=IXN+MAXHLX
            IF(IXN.LE.0) THEN
              MKTRK(ITRK)=0
              GO TO 1000
            ENDIF
          ENDIF
C     TRACK ALREADY IN VERTEX?
          CALL YMKTST(MKDIMM,NMSIZZ,MKTV2(1),JTIX,LMRK)
          IF(.NOT.LMRK) THEN
            CALL YFTVTR(1  ,NH,NU,.FALSE.,WTX,VWTX,
     &        IXH,NSHX,NSVHX,HXI,VHXI,
     &        IXN,NSNU,NSVNU,TNU,VTNU,
     &        WTX2,VWTX2,CHISQ,IFAIL)
C     REMOVE VERY FAR OFF TRACKS FROM FURTHER SEARCH
            IF(CHISQ.LE.2.*CONLM(2,ICONL).AND.(CHIWX+CHISQ).LE.XILIM)
     &        GO TO 500
            MKTRK(ITRK)=0
            GO TO 1000
  500       CONTINUE
C
            IF(CHISQ.GT.CONLM(2,ICONL)) GO TO 1000
            IF(CHISQ+CHIWX.GT.XILIM) GO TO 1000
C
            CHNOR=CHISQ
C     GIVE PREFERENCE TO TRACKS WITH GOOD RESOLUTION
CC      CHNOR=CHISQ/TRRES(ITRK,1)**2
            CHNOR=CHISQ
C
            IF(CHNOR.GE.CHMIN) GO TO 1000
C     BETTER TRACK FOUND
            IBTRK=ITRK
            CHBTR=CHISQ
            CHMIN=CHNOR
          ENDIF
C
 1000   CONTINUE
        IF(IBTRK.LE.0) GO TO 3000
        IF(IBTRK.LE.NHX) THEN
          NH=1
          NU=0
          IXH=IXHX(IBTRK)
          JTIX=IXH
        ELSE
          NH=0
          NU=1
          IXN=IXNU(IBTRK-NHX)
          JTIX=IXN+MAXHLX
        ENDIF
        CALL YFTVTR(1  ,NH,NU,.FALSE.,WTX,VWTX,
     &    IXH,NSHX,NSVHX,HXI,VHXI,
     &    IXN,NSNU,NSVNU,TNU,VTNU,
     &    VTX2,VVTX2,CHISQ,IFAIL)
        NDFV2=NDFV2+2
        CHVX2=CHIWX+CHISQ
C
        DO 1100 I=1,3
          WTX(I)=VTX2(I)
 1100   CONTINUE
        DO 1200 I=1,6
          VWTX(I)=VVTX2(I)
 1200   CONTINUE
        NDFWX=NDFV2
        CHIWX=CHVX2
C
        CALL YMKSET(MKDIMM,NMSIZZ,MKTV2(1),JTIX)
C
        MKTRK(IBTRK)=0
C
        NADTR=NADTR+1
        IXTRI(NADTR)=JTIX
        CHVXI(NADTR)=CHVX2
C
 2000 CONTINUE
C
 3000 CONTINUE
C
      RETURN
      END
