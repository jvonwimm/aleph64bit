      SUBROUTINE QCAF4C(PMOM,TETIFL,XCOS,IDEL,ISIDE,RTO,RTN)
CKEY  FILL / INTERNAL
C ------------------------------------------------------------------------------
C! Correct RT for various data sets - Auxiliary to FIXRTRL
C  Called from FIXRTRL
C                                    Author M.N Minard 10/11/93
C
C     Input  PMOM    Track momentum
C            TETIFL  ECAL theta impact in ECAL numbering
C            XCOS    Track Z cosine director
C            IDEL    Correction range according DB and Julia version
C            ISIDE   =1 Barrel , 2 Endcap , 3 Overlap
C            RTO     Previous RT value
C     Output RTN     New RT value
C ------------------------------------------------------------------------------
#ifndef DOC
      PARAMETER (NVER=4)
      DIMENSION PARMEC(3,NVER),PARMBR(3,NVER)
      DIMENSION PARSEC(3,NVER),PARSBR(3,NVER)
      DIMENSION PECRED(NVER),PBRRED(NVER)
      DIMENSION RES(5)
      DATA RES / 0.28 , 0.35,0.35,0.29,0.1925/
      DATA PARMEC/0.892,0.148,0.
     3           ,0.857,0.156,-0.014    ! DATA 92 NEW
     1           ,0.885,0.081,0.
     2           ,0.849,0.12,-0.034/
      DATA PARMBR/0.847,0.161,-0.044    ! DATA 92
     3           ,0.847,0.161,-0.044    ! DATA 92 NEW
     1           ,0.868,0.063,0.        ! MC   92
     2           ,0.854,0.170,-0.077/   ! MC   92 NEW
C ------------------------------------------------------------------------------
C Mean value :
      IKEEP = 0
      IF (ISIDE.GT.2) IKEEP = 1
      IF (TETIFL.GE.51..AND.TETIFL.LT.179.) THEN
         IF (IDEL.EQ.1) IKEEP = 1
         XE4100= PARMBR(1,IDEL)
         XN4100 = PARMBR(1,IDEL+1)
         CORSZ = PARMBR(2,IDEL)+PMOM*PARMBR(3,IDEL)
         CORSNZ = PARMBR(2,IDEL+1)+PMOM*PARMBR(3,IDEL+1)
         IF ( CORSZ.LE.0) CORSZ = 0.
         IF ( CORSNZ.LE.0) CORSNZ = 0.
      ELSE
         IF(TETIFL.LT.51.)TET100=TETIFL
         IF(TETIFL.GT.178.)TET100=229-TETIFL
         IF(TET100.LT.8.5)GG100=4.
         IF(TET100.GE.8.5.AND.TET100.LT.9.5)GG100=3.
         IF(TET100.GE.9.5.AND.TET100.LT.24.5)GG100=2.
         IF(TET100.GE.24.5.AND.TET100.LT.25.5)GG100=5./3.
         IF(TET100.GE.25.5.AND.TET100.LT.40.5)GG100=4./3.
         IF(TET100.GE.40.5.AND.TET100.LT.41.5)GG100=7./6.
         IF(TET100.GE.41.5)GG100=1.
         LONTET=45.95+3.35*TET100+.0107*TET100**2
         ALP100=4.4067E-3
         SUR100=12.09
         SURTET=.0548*GG100*LONTET
C For data :
         XE4100=ALP100*(SURTET-SUR100)+PARMEC(1,IDEL)
         XN4100=ALP100*(SURTET-SUR100)+PARMEC(1,IDEL+1)
         CORSZ = PARMEC(2,IDEL)+PMOM*PARMEC(3,IDEL)
         CORSNZ = PARMEC(2,IDEL+1)+PMOM*PARMEC(3,IDEL+1)
         IF ( CORSZ.LE.0) CORSZ = 0.
         IF ( CORSNZ.LE.0) CORSNZ = 0.
      ENDIF
C
C-    Now recalculate resolution used
C
      TETQ = TETIFL
      IF ( TETIFL.GT.100) TETQ = 229-TETQ
      ITT = NINT(TETQ)-50
      IF ( ITT.LE.0.OR.ITT.GT.5) ITT = 5
      RESE2 = (RES(ITT)*SQRT(PMOM)+0.0033*PMOM)**2
      IF ( XCOS.LE.0.76) THEN
        FCOS = 1.
      ELSE
        FC0S = .9-0.124*XCOS+3.9*XCOS**2+0.76*XCOS**3-20.64*XCOS**4
     &  -XCOS**5+24.54*XCOS**6
      ENDIF
      PTRAN = PMOM*SQRT(1.-XCOS**2)
      RESP2 = ((1.E-3*PTRAN+0.003)*FCOS*PMOM)**2
      RESO = SQRT((RESP2*XE4100**2)/PMOM**2+RESE2/PMOM**2)
      XE4 = RESO*RTO+XE4100
C
C-    Correct for different correction factor
C
      XRAT = XE4 *(1.+CORSNZ/PMOM)/(1.+CORSZ/PMOM)
      RTN =(XRAT-XN4100)/RESO
      IF ( IKEEP.EQ.1) RTN= RTO
*
 999  RETURN
      END
#endif
