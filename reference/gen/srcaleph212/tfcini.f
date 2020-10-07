      SUBROUTINE TFCINI(LOUT,LCONS,IRUN,IER)
C
C-----------------------------------------------------------------------
C! Initialize TPC field corrections constants in TFCORR
C!
C!    Author:   R. Johnson   22-09-88
C!              Adapted from code by M. Schmelling in order to
C!              interface to the ALEPH database.
C!
C!    Input:    LOUT        /I      Output unit for error messages
C!              LCONS       /I      Input unit for the data base
C!              IRUN        /I      Current run number
C!    Output:   IER         /I      Error code:
C!                                     0  successful initialization
C!                                     1  data-base banks not found
C!                                     2  NSPACE too small in TFCORR
C!                                     3  NGMAX  too small in TFCORR
C!    Called by TINIRU
C!
C-----------------------------------------------------------------------
      SAVE
C
      PARAMETER(JTCGID=1,JTCGVR=2,JTCGCN=4,JTCGNS=5,JTCGNC=6,JTCGNE=7,
     +          JTCGAS=8,JTCGRS=9,JTCGPH=10,JTCGPS=11,JTCGPW=12,
     +          JTCGGW=13,JTCGEW=14,JTCGES=15,JTCGBD=16,JTCGTS=17,
     +          JTCGTH=18,JTCGWP=19,JTCGWD=20,JTCGTO=21,JTCGTT=24,
     +          JTCGWT=27,JTCGWE=28,JTCGWW=29,JTCGWK=30,JTCGFT=33,
     +          LTCGDA=33)
      PARAMETER(JTCROC=1,JTCRNC=2,JTCRN1=3,LTCRLA=3)
      PARAMETER(JTMTID=1,JTMTVR=2,JTMTMO=4,JTMTPP=5,JTMTRF=6,JTMTNP=7,
     +          JTMTPR=8,JTMTRT=20,JTMTNT=21,JTMTTR=22,JTMTAT=33,
     +          JTMTTC=34,JTMTPW=38,JTMTNW=39,JTMTEF=40,JTMTEL=41,
     +          JTMTWF=42,LTMTYA=45)
      PARAMETER(JTPCIN=1,JTPCRV=2,JTPCPH=3,JTPCZV=4,JTPCSR=5,JTPCSZ=6,
     +          JTPCOF=7,JTPCTN=8,JTPCCN=9,JTPCIT=10,JTPCRR=11,
     +          JTPCRZ=12,LTPCOA=12)
      PARAMETER(JTPHIT=1,LTPHEA=1)
      PARAMETER(JTPHKT=1,JTPHCI=2,JTPHPH=3,JTPHZV=4,JTPHDD=5,JTPHDZ=6,
     +          LTPHTA=6)
      PARAMETER(JTPTKT=1,JTPTXA=2,JTPTYA=3,JTPTZA=4,JTPTDX=5,JTPTDY=6,
     +          JTPTDZ=7,JTPTPV=8,JTPTLN=9,JTPTTF=10,JTPTPM=11,
     +          JTPTCH=12,LTPTEA=12)
      PARAMETER(JTSGID=1,JTSGVR=2,JTSGNC=4,JTSGXC=5,JTSGYC=10,JTSGTM=15,
     +          LTSGMA=15)
      PARAMETER(JTSLID=1,JTSLVR=2,JTSLSN=4,JTSLSB=5,JTSLSS=6,JTSLDS=7,
     +          JTSLAS=8,JTSLRS=9,JTSLTM=10,JTSLTS=11,LTSLOA=11)
      PARAMETER(JTTHIT=1,LTTHEA=1)
      PARAMETER(JTTHKT=1,JTTHCI=2,JTTHPH=3,JTTHZV=4,JTTHDD=5,JTTHDZ=6,
     +          LTTHTA=6)
      PARAMETER(JTSCID=1,JTSCVR=2,JTSCSN=4,JTSCNS=5,JTSCRP=6,JTSCAZ=9,
     +          JTSCAX=10,JTSCAY=11,JTSCSG=12,JTSCTC=13,LTSCOA=13)
      PARAMETER(JT1FID=1,JT1FVR=2,JT1FLL=4,JT1FUL=5,JT1FSS=6,JT1FNP=7,
     +          LT1FCA=7)
      PARAMETER(JT2FID=1,JT2FVR=2,JT2FR1=4,JT2FR2=916,JT2FP1=1828,
     +          JT2FP2=2740)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (NFCPHI=19,NFCRAD=8,NFCZED=6,NFCVAL=4)
      PARAMETER (NSPACE=NFCVAL*NFCPHI*NFCRAD*NFCZED)
      PARAMETER (NGMAX=33)
      COMMON/TFCORR/ RLOWFC,RHIGFC,DRFCOR,NRFCOR,
     &               PLOWFC,PHIGFC,DPFCOR,NPFCOR,
     &               ZLOWFC,ZHIGFC,DZFCOR,NZFCOR,
     &               INDCOR(4),
     &               FSPACE(NSPACE),NAFCOR(3),AFCORR(NGMAX)
C
      INTEGER ALGTDB
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
      IRET= ALGTDB(LCONS,'T1FCT2FC',IRUN)
C
C++   Return immediately if the banks already are loaded and
C++   initialization has been done.
C
      IF (IRET.GT.0 .AND. .NOT.FIRST) THEN
        IER=0
        RETURN
      ENDIF
C
C++   Return with an error code if the banks cannot be found
C
      KT1FC=IW(NAMIND('T1FC'))
      KT2FC=IW(NAMIND('T2FC'))
      IF (KT1FC.EQ.0 .OR. KT2FC.EQ.0) THEN
        IER=1
        WRITE(LOUT,1) IRUN
    1   FORMAT(/' TFCINI:  TPC field correction banks T1FC, T2FC ',
     &          ' cannot be found for run ',I5/)
        RETURN
      ENDIF
      FIRST=.FALSE.
C
      RLOWFC= RTABL(KT1FC,1,JT1FLL)
      RHIGFC= RTABL(KT1FC,1,JT1FUL)
      DRFCOR= RTABL(KT1FC,1,JT1FSS)
      NRFCOR= ITABL(KT1FC,1,JT1FNP)
      PLOWFC= RTABL(KT1FC,2,JT1FLL)
      PHIGFC= RTABL(KT1FC,2,JT1FUL)
      DPFCOR= RTABL(KT1FC,2,JT1FSS)
      NPFCOR= ITABL(KT1FC,2,JT1FNP)
      ZLOWFC= RTABL(KT1FC,3,JT1FLL)
      ZHIGFC= RTABL(KT1FC,3,JT1FUL)
      DZFCOR= RTABL(KT1FC,3,JT1FSS)
      NZFCOR= ITABL(KT1FC,3,JT1FNP)
C
      NWORDS = NRFCOR*NPFCOR*NZFCOR
      INDCOR(1) = 1
      INDCOR(2) = INDCOR(1) + NWORDS
      INDCOR(3) = INDCOR(2) + NWORDS
      INDCOR(4) = INDCOR(3) + NWORDS
      NTOT= NWORDS*NFCVAL
      IF (NTOT.GT.NSPACE) THEN
        WRITE(LOUT,6903) NSPACE,NTOT
 6903   FORMAT(/1X,'TFCINI - buffer too small for displacement table'/
     &         1X,'         Increase NSPACE in common/TFCORR/'/
     &         1X,'         Current setting:  NSPACE = ',I10/
     &         1X,'         Needed:           NSPACE = ',I10/)
        IER=2
        RETURN
      ENDIF
C
      DO 11 I=1,NWORDS
        FSPACE(INDCOR(1)+I-1)= RTABL(KT2FC,1,JT2FR1+I-1)
        FSPACE(INDCOR(2)+I-1)= RTABL(KT2FC,1,JT2FR2+I-1)
        FSPACE(INDCOR(3)+I-1)= RTABL(KT2FC,1,JT2FP1+I-1)
        FSPACE(INDCOR(4)+I-1)= RTABL(KT2FC,1,JT2FP2+I-1)
   11 CONTINUE
C
      NLEN=NRFCOR+NPFCOR+NZFCOR
      IF (NLEN.GT.NGMAX) THEN
        WRITE(LOUT,6904) NGMAX,NLEN
 6904   FORMAT(/1X,'TFCINI - buffer for grid description too small'/
     &         1X,'         Increase NGMAX in common/TFCORR/'/
     &         1X,'         Current setting: NGMAX = ',I10/
     &         1X,'         Needed:          NGMAX = ',I10/)
        IER=3
        RETURN
      ENDIF
C
      NAFCOR(1) = NRFCOR
      NAFCOR(2) = NPFCOR
      NAFCOR(3) = NZFCOR
C
      IDOT    = 0
      DO 21 I=1,NRFCOR
        IDOT    = IDOT + 1
        AFCORR(IDOT) = RLOWFC + FLOAT(I-1)*DRFCOR
 21   CONTINUE
      DO 22 I=1,NPFCOR
        IDOT    = IDOT + 1
        AFCORR(IDOT) = PLOWFC + FLOAT(I-1)*DPFCOR
 22   CONTINUE
      DO 23 I=1,NZFCOR
        IDOT    = IDOT + 1
        AFCORR(IDOT) = ZLOWFC + FLOAT(I-1)*DZFCOR
 23   CONTINUE
C
      WRITE(LOUT,6901) IRUN
      WRITE(LOUT,6902) 'rlow,rhig,dr,nr: ',RLOWFC,RHIGFC,DRFCOR,NRFCOR
      WRITE(LOUT,6902) 'plow,phig,dp,np: ',PLOWFC,PHIGFC,DPFCOR,NPFCOR
      WRITE(LOUT,6902) 'zlow,zhig,dz,nz: ',ZLOWFC,ZHIGFC,DZFCOR,NZFCOR
      WRITE(LOUT,6905)
 6901 FORMAT(/1X,'TCOREC - TPC drift field correction constants',
     &           ' successfully read for run ',I6,'.'/
     &           ' Grid for coordinate correction:')
 6902 FORMAT(1X,'TCOREC - ',(A),3F12.4,I6)
 6905 FORMAT(1X,'---------------------------------------------------'/)
C
      IER=0
      RETURN
      END
