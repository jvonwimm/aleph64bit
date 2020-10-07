      SUBROUTINE CLAS24(IGOOD)
C-----------------------------------------------------------------------
CKEY EDIR TAU CLASS24
C! Steering routine for events of class 24.
C! Lepton pair selection by hadron, 2-gamma and cosmic rejection.
C-
C   Input  : None
C   Output : IGOOD  = Class  logical flag
C-
C   Input banks : PFRF,PFRT,EGPC
C-
C     from ALEPHLIB routine LEPTO
C
C                  Lepto :     Author : J.C.Brient        Date : 7/7/89
C                              Updated: C.Geweniger       Date : 7/2/90
C                  Tauclass :           Tau group         Date : 2/12/92
C                              Last modification by
C                                       S. Orteu                 2/2/93
C                              The changes can be obtained by looking
C                              at the commentaries CL15
C-----------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JPFRIR=1,JPFRTL=2,JPFRP0=3,JPFRD0=4,JPFRZ0=5,JPFRAL=6,
     +          JPFREO=7,JPFREM=13,JPFRC2=28,JPFRNO=29,LPFRFA=29)
      PARAMETER(JPFRNV=1,JPFRNI=2,JPFRNE=3,JPFRNT=4,JPFRNR=5,LPFRTA=5)
      PARAMETER(JEGPPX=1,JEGPPY=2,JEGPPZ=3,JEGPR1=4,JEGPR2=5,JEGPF4=6,
     +          JEGPDM=7,JEGPST=8,JEGPQU=9,JEGPPE=10,LEGPCA=10)
      PARAMETER(JPGPEC=1,JPGPTC=2,JPGPPC=3,JPGPR1=4,JPGPR2=5,JPGPF4=6,
     +          JPGPDM=7,JPGPST=8,JPGPQU=9,JPGPQ1=10,JPGPQ2=11,
     +          JPGPM1=12,JPGPM2=13,JPGPMA=14,JPGPER=15,JPGPTR=16,
     +          JPGPPR=17,JPGPPE=18,LPGPCA=18)
C --
      DIMENSION PP1(4),PP2(4),VEPRO(3),ZVEC(3),CUTIS(20)
      PARAMETER (ITRMAX = 500)
      DIMENSION ITRQUA(ITRMAX),IJET(ITRMAX),AXTR(3)
      DIMENSION PJ1(4),PJ2(4)
      LOGICAL IGOOD,FIRST
      DATA FIRST/.TRUE./
      DATA ZVEC/0.,0.,1./
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
C --
      IGOOD = .FALSE.
C --
      IF(FIRST) THEN
        FIRST = .FALSE.
C --
C   all values of the cuts used in the selection
C   ----------------------------------------------
C                       Abs(Z0)
        CUTIS(1) = 10.0
C                       Abs(D0)
        CUTIS(2) =  2.0
C                       Max |cos| for ch.tr (not used)
        CUTIS(3) =  1.0
C                       Impulsion min. (gev/c)
        CUTIS(4) =  0.10
C                       Min. number of points in ITC (not used)
        CUTIS(5) =  0.0
C                       Min. number of points in TPC
        CUTIS(6) =  4.0
C                       Acol cut (not used)
        CUTIS(7) =  1.0
C                       At least one good tr. with p > 3.0 GeV
C--CL15 CUTIS(8) =   3.0
        CUTIS(8) =   2.0
C                       Cut on the smallest d0 (cosmic) at 0.5 cm
C                       (not used)
        CUTIS(9)=   0.5
C                       Max. opening angle
        CUTIS(10)=  0.85
C --
      ENDIF


      NEGPC = 0
      JEGPC = IW(NAMIND('EGPC'))
      IF(JEGPC.GT.0) NEGPC = LROWS(JEGPC)

      NPGPC = 0
      JPGPC = IW(NAMIND('PGPC'))
      IF(JPGPC.GT.0) NPGPC = LROWS(JPGPC)

      NPFRF = 0
      JPFRF = IW(NAMIND('PFRF'))
      IF(JPFRF.GT.0) NPFRF = LROWS(JPFRF)
C --
C  want at least 2 charged tracks
C -------------------------------
      IF(NPFRF.LT.2) GO TO 990
      IF(NPFRF.GT.ITRMAX) GO TO 990
C --
C  first select the good tracks
C ------------------------------
      KNCHS  = 0
      KNCHW  = 0
      ECHFRF = 0.
      PMA    = 0.
      SD0    = 10.
      JPFRT  = IW(NAMIND('PFRT'))
      NPITC  = NINT(CUTIS(5))
      NPTPC  = NINT(CUTIS(6))
      DO 10 I = 1 , NPFRF
        ITRQUA(I) = 0
        Z0     =   RTABL(JPFRF,I,JPFRZ0)
        D0     =   RTABL(JPFRF,I,JPFRD0)
        CALL TRPFRF(I,PP1,IRF)
        PP     =   ABS(PP1(4))
        CTX    =   ABS(PP1(3))/PP
        NPPT   =   ITABL(JPFRT,I,JPFRNT)
        NPPI   =   ITABL(JPFRT,I,JPFRNI)
        IF(NPPT     .LT.     NPTPC)     GO TO 10
C---    IF(NPPI     .LT.     NPITC)     GO TO 10
        IF(ABS(Z0)  .GT.  CUTIS(1))     GO TO 10
C---    IF(ABS(CTX) .GT.  CUTIS(3))     GO TO 10
        IF(PP       .LT.  CUTIS(4))     GO TO 10
        IF(ABS(D0)  .GT.    5.0   )     GO TO 10
          ITRQUA(I)     = 2
          KNCHW  = KNCHW + 1
        IF(ABS(D0)  .GT.  CUTIS(2))     GO TO 10
          ITRQUA(I)     = 1
          KNCHS  = KNCHS + 1
          IF(ABS(D0).LT.SD0) SD0 = ABS(D0)
          IF(PP.GT.PMA) PMA = PP
 10   CONTINUE
C --
C ADD GAMPEC PHOTON WITH E>CUTIS(8) GeV IN THE SEARCH FOR PMA
C
      PMAGAM = 0.
      IF (NEGPC.GT.0) THEN
        DO I = 1 , NEGPC
          EGAMMA =  SQRT ( RTABL(JEGPC,I,JEGPPX)**2  +
     &                     RTABL(JEGPC,I,JEGPPY)**2  +
     &                     RTABL(JEGPC,I,JEGPPZ)**2    )
          IF ( EGAMMA .GT. MAX(PMAGAM,CUTIS(8)) )   PMAGAM = EGAMMA
        ENDDO
      ELSE
        DO I=1, NPGPC
          EGAMMA = RTABL(JPGPC,I,JPGPEC)
          IF ( EGAMMA .GT. MAX(PMAGAM,CUTIS(8)) )   PMAGAM = EGAMMA
        ENDDO
      ENDIF
C --
C   KNCHW is the number of tracks with a wide d0 cut of 5 cm
C   SD0 is the smallest d0 for the "good" tracks
C   PMA is the highest momentum in the event
C --
C  first cut on charged multiplicity
C  ----------------------------------
C  use wide d0 cut for 2-track events, otherwise use narrow d0 cut
C  ---------------------------------------------------------------
      IF(KNCHW.NE.2) THEN
        IF(KNCHS.LT.2.OR.KNCHS.GT.8)  GO TO 990
      ENDIF
C --
C  reset track quality flag
C  ------------------------
      IF(KNCHW.EQ.2) KNCHS = 0
      DO 20 I = 1 , NPFRF
        IF(KNCHW.EQ.2) THEN
           IF(ITRQUA(I).EQ.2) ITRQUA(I) = 1
           IF(ITRQUA(I).EQ.1) KNCHS = KNCHS + 1
        ELSE
           IF(ITRQUA(I).EQ.2) ITRQUA(I) = 0
        ENDIF
 20   CONTINUE
C --
C  compute the thrust axis and value
C  ---------------------------------
      CALL VZERO(AXTR,3)
C --
      CALL TRUSLU(ITRQUA,KNCHS,THRUST,AXTR)
C --
C   define the 2 "jets"
C  --------------------
      MC1 = 0
      MC2 = 0
      DO 200 I = 1 , NPFRF
        IJET(I) = 0
        IF(ITRQUA(I).NE.1)     GO TO 200
        CALL TRPFRF(I,PP1,IRF)
C199    CONTINUE
        CS1 = VDOTN(PP1,AXTR,3)
        IF(CS1.GT.0.) THEN
          MC1 = MC1 + 1
          IJET(I) = 1
        ELSE
          MC2 = MC2 + 1
          IJET(I) = 2
        ENDIF
 200  CONTINUE
C --
C   at least 1 track per hemisphere
C  ----------------------------------
CL15  IF(MC1.LT.1.OR.MC2.LT.1)      GO TO 990
C --
C   store the 2 "jets"
C  --------------------
      CALL VZERO(PJ1,4)
      CALL VZERO(PJ2,4)
C --
      DO 100 I = 1 , NPFRF
          IF(IJET(I).EQ.1) THEN
              CALL TRPFRF(I,PP1,IRF)
              CALL VADD(PJ1,PP1,PJ1,3)
          ENDIF
          IF(IJET(I).EQ.2) THEN
              CALL TRPFRF(I,PP1,IRF)
              CALL VADD(PJ2,PP1,PJ2,3)
          ENDIF
 100  CONTINUE
C --
C --
C   Gamma gamma rejection
C  ----------------------
C---  IF(ACOL.GT.CUTIS(7))          GO TO 990
C--CL15    IF(PMA.LT.CUTIS(8))          GO TO 990
      IF(PMA.LT.CUTIS(8) .AND. PMAGAM.LT.CUTIS(8) )          GO TO 990
C --
C  reject COSMICS (not applied)
C  --------------
C---  IF(KNCHS.EQ.2) THEN
C---     IF(SD0.GT.CUTIS(9)) GO TO 990
C---  ENDIF

C --
C   compute the max opening angle in each hemisphere
C  ------------------------------------------------------
      DO 250 J = 1 , 2
        CMA =  2.
        DO 240 I = 1 , NPFRF
          IF(ITRQUA(I).NE.1)     GO TO 240
          CALL TRPFRF(I,PP1,IRF)
          IF(IJET(I).EQ.J) THEN
            IF(J.EQ.1)  CS1 = VDOTN(PP1,PJ1,3)
            IF(J.EQ.2)  CS1 = VDOTN(PP1,PJ2,3)
            IF(CS1.LT.CMA) CMA = CS1
          ENDIF
 240    CONTINUE
        IF(J.EQ.1) CM1 = CMA
        IF(J.EQ.2) CM2 = CMA
 250  CONTINUE
C --
      CMA1 = CM1
      CMA2 = CM2
      CUC  = CUTIS(10)
C --
C   cut on the max of the opening angle in each hemisphere
C  -------------------------------------------------------
      IF(KNCHS.GT.4) THEN
        IF(CM1.LT.CUC.OR.CM2.LT.CUC) GO TO 990
      ENDIF
C --
C  here are the "good" lepton pairs
C  --------------------------------
      IGOOD = .TRUE.
C --
 990  RETURN
      END
