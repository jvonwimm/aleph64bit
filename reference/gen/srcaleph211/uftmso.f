      SUBROUTINE UFTMSO(ITRAC,COVMAT)
C----------------------------------------------------------------
C! For FRFT (2) track ITRAC this routine will calculate the
C! contibution to the covariance matrix coming from multiple
C! scattering between the last coordinate and the point
C! of closest approach to the origin
C!
C!
CKEY COMPUTE FIT
C!    AUTHOR: G.Taylor 29/9/92
C!
C!  INPUT:  ITRAC  = FRFT track number of track being fitted
C!  OUTPUT: COVMAT(15) = covariance matrix coming from multiple
C!                scattering and the origin
C!
C----------------------------------------------------------------
      SAVE NFRTL,NFTCL,NFICL,NFVCL,NTPCO,NICCO,NVDCO
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JFRTIV=1,JFRTNV=2,JFRTII=3,JFRTNI=4,JFRTNE=5,JFRTIT=6,
     +          JFRTNT=7,JFRTNR=8,LFRTLA=8)
      PARAMETER(JFTCIT=1,LFTCLA=1)
      PARAMETER(JFICII=1,LFICLA=1)
      PARAMETER(JFVCIV=1,LFVCLA=1)
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)
      PARAMETER(JTPCIN=1,JTPCRV=2,JTPCPH=3,JTPCZV=4,JTPCSR=5,JTPCSZ=6,
     +          JTPCOF=7,JTPCTN=8,JTPCCN=9,JTPCIT=10,JTPCRR=11,
     +          JTPCRZ=12,LTPCOA=12)
      PARAMETER(JICCRV=1,JICCPH=2,JICCZV=3,JICCSR=4,JICCSZ=5,LICCOA=5)
      PARAMETER(JVDCWI=1,JVDCR0=2,JVDCPH=3,JVDCZ0=4,JVDCSR=5,JVDCSZ=6,
     +          JVDCQF=7,JVDCTN=8,LVDCOA=8)
C
      DIMENSION COV(5,5),COVMAT(15)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
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

      IF (FIRST) THEN
        FIRST = .FALSE.
        NFRTL = NAMIND('FRTL')
        NFTCL = NAMIND('FTCL')
        NFICL = NAMIND('FICL')
        NFVCL = NAMIND('FVCL')
        NTPCO = NAMIND('TPCO')
        NICCO = NAMIND('ICCO')
        NVDCO = NAMIND('VDCO')
      END IF
C
C  Zero output
C
      CALL VZERO(COVMAT,15)
C
C Find tracks
C
      KFRFT = NLINK('FRFT',0)
      IF (KFRFT .EQ. 0) THEN
       KFRFT = NLINK('FRFT',2)
      END IF
      KFRTL = IW(NFRTL)
      KFTCL = IW(NFTCL)
      KFICL = IW(NFICL)
      KFVCL = IW(NFVCL)

C -- CHECK THAT INDEX BANKS ARE THERE
      IF (KFRFT.EQ.0 .OR.
     1    KFRTL.EQ.0 .OR.
     1    KFTCL.EQ.0 .OR.
     1    KFICL.EQ.0 ) RETURN

      NTPC = ITABL(KFRTL,ITRAC,JFRTNT)
      NITC = ITABL(KFRTL,ITRAC,JFRTNI)
      NVDT = ITABL(KFRTL,ITRAC,JFRTNV)
C -- CHECK THAT COORDINATE BANKS ARE THERE
      IF (NTPC .GT. 0) THEN
         KTPCO = IW(NTPCO)
         IF (KTPCO .EQ. 0) RETURN
      END IF

      IF (NITC .GT. 0) THEN
         KICCO = IW(NICCO)
         IF (KICCO .EQ. 0) RETURN
      END IF

      IF (NVDT .GT. 0) THEN
         KVDCO = IW(NVDCO)
         IF (KVDCO .EQ. 0) RETURN
      END IF

      NPTPC = ITABL(KFRTL,ITRAC,JFRTIT)
      NPITC = ITABL(KFRTL,ITRAC,JFRTII)
      NPVDT = ITABL(KFRTL,ITRAC,JFRTIV)

      RAD=1000.
C
C---> TPC COORDINATES
C
      IF ( NTPCO .GT. 0 ) THEN
        KTPCO=IW(NTPCO)
        DO 10 I = 1, NTPC
          KSTRT     = KROW(KTPCO,IW(KFTCL+LMHLEN+NPTPC+NTPC-I+1))
          RADIUS    = RW(KSTRT+JTPCRV)
          IF(RADIUS.LT.RAD) RAD=RADIUS
   10   CONTINUE
      ENDIF
C
C---> ITC COORDINATES
C
      IF ( NICCO .GT. 0 ) THEN
        KICCO=IW(NICCO)
        DO  20  I = 1, NITC
          KSTRT  = KROW(KICCO,IABS(IW(KFICL+LMHLEN+NPITC+I)))
          RADIUS = RW(KSTRT+JICCRV)
          IF(RADIUS.LT.RAD) RAD=RADIUS
   20   CONTINUE
      ENDIF
C
C---> GET VDET COORDINATES
C
      IF ( NVDCO .GT. 0 ) THEN
        KVDCO=IW(NVDCO)
        DO  30  I = 1, NVDT
          KSTRT = KROW(KVDCO,IW(KFVCL+LMHLEN+NPVDT+NVDT-I+1))
          RADIUS = RW(KSTRT+JVDCR0)
          IF(RADIUS.LT.RAD) RAD=RADIUS
   30   CONTINUE
      ENDIF
C HERE RAD IS THE RADIUS OF THE INNERMOST POINT ON THE TRACK

      RIN  =  ABS( RTABL(KFRFT,ITRAC,JFRFD0)) + 0.00001
      IF (RIN .GT. RAD) RETURN
      CALL UFMS(RAD,RIN,RW(KROW(KFRFT,ITRAC)+JFRFIR), COV)
      COVMAT(1)  = COV(1,1)
      COVMAT(2)  = COV(2,1)
      COVMAT(3)  = COV(2,2)
      COVMAT(4)  = COV(3,1)
      COVMAT(5)  = COV(3,2)
      COVMAT(6)  = COV(3,3)
      COVMAT(7)  = COV(4,1)
      COVMAT(8)  = COV(4,2)
      COVMAT(9)  = COV(4,3)
      COVMAT(10) = COV(4,4)
      COVMAT(11) = COV(5,1)
      COVMAT(12) = COV(5,2)
      COVMAT(13) = COV(5,3)
      COVMAT(14) = COV(5,4)
      COVMAT(15) = COV(5,5)
      RETURN
      END
