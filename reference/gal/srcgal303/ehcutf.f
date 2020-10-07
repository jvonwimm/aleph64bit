      SUBROUTINE EHCUTF(MFLAG)
C.----------------------------------------------------------------
C  R.Clifft                       mod   M.Rumpf March 87
C! Fast tracking or not?
C  MFLAG = 0|1 --> no|yes
C  called by ECHIT
C.----------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
      EQUIVALENCE (NAPAR,NAPART)
C
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
      PARAMETER (LGKINE=100)
      COMMON/GCKING/KGCASE,NGKINE,GKIN(5,LGKINE),GTOFD(LGKINE)
     &             ,IGFLGK(LGKINE)
C
      COMMON/GCKIN3/GPOS(3,LGKINE)
      REAL GPOS
C
C
C     EC Analog signals conditions
      COMMON/EHCOND/ TCUTRK,TSMEAR,TIRAGE,TEDEPO,TPARAM
      CHARACTER * 16 TCUTRK,TSMEAR,TIRAGE,TEDEPO,TPARAM
C
C
C ----- Version du 16/03/87
      COMMON / EHPASH /
     1 RHODEP,PARGV1,ENMAX1,ENMAX2,PARGV2,NRAPID,RHOVIT(14),
     2 FASTNR(14),FLUCT1(14),FLUCT2(14),EMINPI(14),EPICUT(14),
     3 EMINLO,ETRANS,ASURB1(5),ASURB2(5),UNSRB1(5),UNSRB2(5),
     4 SIGMA1(5),SIGMA2(5),SIGMB1(5),SIGMB2(5),SIGMA3(5),SIGMB3(5),
     5 SEUSIG,USAMIN,USAMAX,BSUMIN,BETMIN,BETMAX,
     6 ZONSH1,ZONSH2,DEPMIN,
     7 EMINRA,EXPRD0,EXPRD1,RFACT0,RFACT1,KPAMAX,X1MAXI,EPSRAD,
     8 EMFRAC,ALPHA0,ALPHA1,BETAH0,BETAH1,RAYHA0,RAYHA1,PUISH0,
     9 PARGVH,NRJHAD,IPLMIN(14),IPLMAX(14),DESATU,FNFRAG,
     + ECHMX,ECHDC,EKEVH,ECHDN,ERADMX,
     + ERMAX,ETANG,ERHMX,EZMAX,EDSELM,EDSHAD,ECUTTE,
     + ST3BA0,ST3EC0,ST3BA1,ST3EC1,ST3BA2,ST3EC2
      COMMON / EHPADA /
     1   CNRJDA,C1PRDA,C2PRDA,C3PRDA,PIMADA,ANRJDA,A1PRDA,A2PRDA,
     2   A3PRDA,A4PRDA,A5PRDA,AMRJDA
      CHARACTER NAME*4
C
      MFLAG=0
C
C - Do not FAST track particles with a total energy before the step > EC
C   (ECUTTE is set in EHPASR)
C
         IF (TRKELE(8).GT.ECUTTE) RETURN
C
C *** if  electron OR positron
C
      IF (ITRKEL(4).EQ.2 .OR. ITRKEL(4).EQ.3) THEN
         IF(ITRKEL(9).NE.0)            GO TO 999
C
C *** get track segment start parameters
C
         PV=TRKNXT(7)
         XV=TRKNXT(1)
         YV=TRKNXT(2)
         ZV=TRKNXT(3)
         UV=TRKNXT(4)
         VV=TRKNXT(5)
         WV=TRKNXT(6)
         TGTET = ABS (SQRT(XV**2+YV**2)/ZV)
C
C-       TEST TRACK ELEMENT IN OVERLAP REGION
C
         IF ( TGTET . LT . 0.95 .AND. TGTET . GT. 0.8 )  GO TO 999
C
C *** find which segment we are in.  nst = 1,2,3,4  for
C ***  barrel-stacks1/2 , end cap-stacks1/2 , barrel-stack3 ,
C ***  end cap-stack3
C
         CALL UHTOC(ITRKEL(5),4,NAME,4)
         IF(NAME(3:3).EQ.'S')           THEN
           NST=3
           IF(NAME(2:2).EQ.'C') NST=4
         ELSE
           NST=1
           IF(NAME(2:2).EQ.'C') NST=2
         ENDIF
C
C *** call EHEDGE to check proximity to edge of wanted region
C
         CALL EHEDGE (NST,PV,XV,YV,ZV,UV,VV,WV,
     &                DPERP,DTOT,MFLEG)
         IF (MFLEG.EQ.0)  GO TO 999
C
C *** check path length to back criterion for parametrisation
C
         PTB=DPERP
         DTB=DTOT
         CALL EHCHEK (NST,DTB,PTB,PV,MFLSL)
         IF (MFLSL.EQ.0)  GO TO 999
C
C *** we contain shower, so set necessary parameters
C
         MFLAG=1
      ENDIF

  999 RETURN
      END
