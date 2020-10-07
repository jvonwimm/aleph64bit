      SUBROUTINE ASKUSI(IGCOD)
C--------------------------------------------------------------------
C Initialization for BHWIDE    J. Von Wimmersperg  19th Feb 2011
C--------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=50000)
C
      COMMON /BCS/   IW(LBCS )
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON / OUTIN / IINP,IOUT
      COMMON / INOUT / NINP,NOUT
C
      INTEGER ALTABL, ALRLEP
      EXTERNAL ALTABL,ALRLEP
      REAL TABL(23)
      INTEGER ITAB(23)
      EQUIVALENCE   (TABL(1), ITAB(1))
      REAL SDVRT, VPOS, VRTEX
      REAL*8 XPAR
      COMMON/CDGENE/ SDVRT(3),VPOS(3),VRTEX(4),XPAR(100),NPAR(100),
     +               NEVENT(8)
      COMMON/GENPAR/CMSENE,THMINP,THMAXP,THMINE,THMAXE,ENMINP,ENMINE,
     +              ACOLLI,EPSCMS,WTMAX,AMAZ,GAMMZ,SINW2,AMTOP,AMHIG,
     +              KEYWGT,KEYRND,KEYCHA,KEYZOF,KEYOPT,KEYEWC,KEYLIB,
     +              KEYMOD,KEYPIA,KEYRAD
      COMMON / MOMSET / p1(4),q1(4),p2(4),q2(4),phot(100,4),nphot
      REAL*8 P1,Q1,P2,Q2,PHOT
*      COMMON / KGCOMM / PF1(4),QF1(4),PF2(4),QF2(4),SPHOT(100,4)
      REAL*4 PF1(4),QF1(4),PF2(4),QF2(4),SPHOT(100,4)
      COMMON / WGTALL / wtmod,wtcrud,wttrig,wtset(300)
      REAL*8 WTMOD,WTCRUD,WTTRIG,WTSET
      COMMON / HISTGPAR / THP1,THP2,THE1,THE2,X1,X2,NGMAX,NBIN
      REAL*8 THP1,THP2,THE1,THE2,X1,X2
      COMMON / CGLIB / B(50000)
      REAL*8 B
*
C
C Generator code (see KINLIB DOC)
C
      PARAMETER ( IGCO = 2014 )
      PARAMETER ( IVER = 104  )
C
C   Return generator code
C
      IGCOD= IGCO
      IINP = IW(5)
      IOUT = IW(6)
      NINP = 15
      NOUT = 6
      WRITE(IOUT,101) IGCOD ,IVER
 101  FORMAT(/,10X,'BHWIDE - CODE NUMBER =',I4,
     &       /,10X,'**************************',
     &       /,10X,' SUBVERSION  :',I10 ,
     &   /,10x,'Interface last mod on Feb 19, 2011')
C
C Initialization of histogramming package --
C Here we use double precision HBOOK-like histogramming/plotting
C package GLIBK written by S. Jadach, (1990),  unpublished.

C      CALL GLIMIT(50000)
CB    this is too palform dependant....and one can always use PAW
CB to make plots from HBOOK
CB      OPEN(UNIT=NOUT,FILE='BHWIDE.OUTPUT',STATUS='NEW')
CB      REWIND(NOUT)
CB      CALL GOUTPU(NOUT)
C
C Input parameters for the generator
C
      CMSEne = 200.0    ! CMS Energy = 2*Ebeam [GeV]
      ThMinp =  25.0    ! Detector range ThetaMin [deg] for positrons
      ThMaxp = 155.0    ! Detector range ThetaMax [deg] for positrons
      ThMine =  25.0    ! Detector range ThetaMin [deg] for electrons
      ThMaxe = 155.0    ! Detector range ThetaMax [deg] for electrons
      EnMinp =   1.0    ! Energy minimum [GeV] for detected positrons
      EnMine =   1.0    ! Energy minimum [GeV] for detected electrons
      Acolli =  10.0    ! Maximum acollinearity [deg] of final e+e-
      epsCMS =  1d-5    ! Infrared cut on photon energy 
      WTMAX  =   3.0    ! Maximum Weight for rejection
      AMAZ   =  91.1882 ! Z mass
      GAMMZ  =  2.4952  ! Z width (may be recalculated by EW library)
      SINW2  =  0.22225 ! sin^2(theta_W) (may be recalculated by EW library)   
      AMTOP  = 174.3    ! top quark mass
      AMHIG  = 115.0    ! Higgs mass      
c Try both options for KeyWgt, result should be the same
      KeyWgt =   0   ! unweighted (WT=1) events, for detector simulation
c      KeyWgt = 1   ! weighted events
      KeyRnd =   1   ! RANMAR random numbers
      KeyCha =   0   ! Channel choice: all/s-only/t-only: =0/1/2
      KeyZof =   0   ! Z-contribution ON/OFF: =0/1
      KeyOpt = 1000*KeyZof +100*KeyCha +10*KeyWgt + KeyRnd
c      KeyEWC = 0   ! QED corrections only 
      KeyEWC =   1   ! Total O(alpha) ElectroWeak corr. included
c      KeyLib = 1   ! ElectroWeak corrections from BABAMC (obsolete!)
      KeyLib =   2   ! ElectroWeak corrections from ALIBABA
c      KeyMod = 1   ! Hard bremsstr. matrix element from MODEL1
      KeyMod =   2   ! Hard bremsstr. matrix alement from MODEL2
      KeyPia =   3   ! Vacuum polarization option (0/1/2/3)
      KeyRad = 1000*KeyEWC + 100*KeyLib + 10*KeyMod + KeyPia
C
C  The default values can be changed by the DATA CARD GBHP
C
      NAGBHP = NAMIND('GBHP')
      JGBHP = IW(NAGBHP)
      IF(JGBHP.NE.0) THEN
       CMSENE = RW(JGBHP+1)
       THMINP = RW(JGBHP+2)
       THMAXP = RW(JGBHP+3)
       THMINE = RW(JGBHP+4)
       THMAXE = RW(JGBHP+5)
       ENMINP = RW(JGBHP+6)
       ENMINE = RW(JGBHP+7)
       ACOLLI = RW(JGBHP+8)
       EPSCMS = RW(JGBHP+9)
       WTMAX  = RW(JGBHP+10)
      ENDIF
C
C by the DATA CARD GBSM
C
      NAGBSM = NAMIND('GBSM')
      JGBSM = IW(NAGBSM)
      IF(JGBSM.NE.0) THEN
       AMAZ  = RW(JGBSM+1)
       GAMMZ = RW(JGBSM+2)
       SINW2 = RW(JGBSM+3)
       AMTOP = RW(JGBSM+4)
       AMHIG = RW(JGBSM+5)
      ENDIF
C
C  by the DATA CARD GBHK
C
      NAGBHK = NAMIND('GBHK')
      JGBHK  = IW(NAGBHK)
      IF(JGBHK.NE.0) THEN
       KEYWGT = IW(JGBHK+1)
       KEYRND = IW(JGBHK+2)
       KEYCHA = IW(JGBHK+3)
       KEYZOF = IW(JGBHK+4)
       KEYEWC = IW(JGBHK+5)
       KEYLIB = IW(JGBHK+6)
       KEYMOD = IW(JGBHK+7)
       KEYPIA = IW(JGBHK+8)
      ENDIF
      KEYOPT = 1000*KEYZOF +100*KEYCHA +10*KEYWGT + KEYRND
      KEYRAD = 1000*KEYEWC + 100*KEYLIB + 10*KEYMOD + KEYPIA
C
C  Main vertex smearing
C
      SDVRT(1) = 0.0185
      SDVRT(2) = 0.0008
      SDVRT(3) = 0.7000
      VPOS(1) = 0.000
      VPOS(2) = 0.000
      VPOS(3) = 0.000
      NASVRT = NAMIND('SVRT')
      JSVRT = IW(NASVRT)
      IF (JSVRT .NE. 0) THEN
        SDVRT(1) = RW(JSVRT+1)
        SDVRT(2) = RW(JSVRT+2)
        SDVRT(3) = RW(JSVRT+3)
      END IF
C
C Main vertex mean position
C
      JXVRT = IW(NAMIND('XVRT'))
      IF (JXVRT .NE. 0) THEN
        VPOS(1) = RW(JXVRT+1)
        VPOS(2) = RW(JXVRT+2)
        VPOS(3) = RW(JXVRT+3)
      END IF
C
C  All the parameters are stored in TABL(I)
C
      TABL(1)  =  CMSENE
      TABL(2)  =  THMINP
      TABL(3)  =  THMAXP
      TABL(4)  =  THMINE
      TABL(5)  =  THMAXE
      TABL(6)  =  ENMINP
      TABL(7)  =  ENMINE
      TABL(8)  =  ACOLLI
      TABL(9)  =  EPSCMS
      TABL(10) =  WTMAX
      TABL(11) =  AMAZ
      TABL(12) =  GAMMZ      
      TABL(13) =  SINW2
      TABL(14) =  AMTOP
      TABL(15) =  AMHIG
      TABL(16) =  SDVRT(1)
      TABL(17) =  SDVRT(2)
      TABL(18) =  SDVRT(3)
      TABL(19) =  VPOS(1)
      TABL(20) =  VPOS(2)
      TABL(21) =  VPOS(3)
      TABL(22) =  FLOAT(KEYOPT)
      TABL(23) =  FLOAT(KEYRAD)
C
C  Fill the KPAR bank with the generator parameters
C
      NCOL = 23
      NROW = 1
      JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'2I,(F)','C')
c
C  Fill RLEP bank
       IEBEAM = NINT(0.5*CMSENE*1000.0)
       JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)
C
C Initialization of event counters
C
      DO 20 I = 1,8
       NEVENT(I) = 0
   20 CONTINUE
C
C Generator Initialisation
C
      NPAR(1)=KEYOPT
      NPAR(2)=KEYRAD
      XPAR(1)=DBLE(CMSENE)
      XPAR(2)=DBLE(THMINP)
      XPAR(3)=DBLE(THMAXP)
      XPAR(4)=DBLE(THMINE)
      XPAR(5)=DBLE(THMAXE)
      XPAR(6)=DBLE(ENMINP)
      XPAR(7)=DBLE(ENMINE)
      XPAR(8)=DBLE(ACOLLI)
      XPAR(9)=DBLE(EPSCMS)
      XPAR(10)=DBLE(WTMAX)
      XPAR(11)=DBLE(AMAZ)
      XPAR(12)=DBLE(GAMMZ)
      XPAR(13)=DBLE(SINW2)
      XPAR(14)=DBLE(AMTOP)
      XPAR(15)=DBLE(AMHIG)

      CALL BHWIDE(-1,XPAR,NPAR)
C
C  Print PART and KPAR banks
C
      CALL PRPART
      CALL PRTABL('RLEP',0)
      CALL PRTABL('KPAR',0)
C
C Book histograms
      Nbin = 50
      Ebeam = CMSEne/2
      THP1=0.D0
      THP2=3.14159265359d0
      THE1=0.D0
      THE2=3.14159265359d0
CB      CALL GBOOK1(1000,'e+: Theta distr. (deg) $',Nbin,thp1,thp2)
CB      CALL GBOOK1(1100,'e-: Theta distr. (deg) $',Nbin,the1,the2)
      call hBOOK1(11000,'e+: Theta distr. (deg)',Nbin,0.,180.,0.)
      call hBOOK1(11100,'e-: Theta distr. (deg)',Nbin,0.,180.,0.)
      x1 =-5.d0
      x2 = 0.d0
CB      CALL GBOOK1(1200,'Energy distr: x=log10(1-s1/s)',Nbin,x1,x2)
      CALL hBOOK1(11200,'Energy distr: x=log10(1-s1/s)',Nbin,-5.,0.,0.)
      Ngmax = 20
CB      CALL GBOOK1(1300,'Photon multiplicity $',Ngmax,0.d0,Ngmax*1.d0)
      CALL HBOOK1(11300,'Photon multiplicity',Ngmax,0.,20.,0.)
      IF (KeyWgt .NE .0)
CB     &CALL GBOOK1(1400,'Weight dist. accepted events$',Nbin,-1.d0,4.d0
     &CALL HBOOK1(11400,'Weight dist. accepted events',Nbin,-1.,4.,0.)
      RETURN
      END
      SUBROUTINE ASKUSE (IDP,IST,NTRK,NVRT,ECM,WEI)
C--------------------------------------------------------------------
C J. Von Wimmersperg  19th Feb 2011
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C     input     : none
C
C     output    : 6 arguments
C          IDP    : process identification
C          IST    : status flag ( 0 means ok)
C          NTRK   : number of tracks generated and kept
C          NVRT   : number of vertices generated
C          ECM    : center of mass energy for the event
C          WEI    : event weight
C--------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=50000)
C
      COMMON /BCS/   IW(LBCS )
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON / OUTIN / IINP,IOUT
      COMMON / INOUT / NINP,NOUT
C
      REAL TABL(17)
      INTEGER ITAB(17)
      EQUIVALENCE (TABL(1), ITAB(1))
      INTEGER ALTABL
      EXTERNAL ALTABL
      REAL SDVRT, VPOS, VRTEX
      REAL*8 XPAR
      COMMON/CDGENE/ SDVRT(3),VPOS(3),VRTEX(4),XPAR(100),NPAR(100),
     +               NEVENT(8)
      COMMON/GENPAR/CMSENE,THMINP,THMAXP,THMINE,THMAXE,ENMINP,ENMINE,
     +              ACOLLI,EPSCMS,WTMAX,AMAZ,GAMMZ,SINW2,AMTOP,AMHIG,
     +              KEYWGT,KEYRND,KEYCHA,KEYZOF,KEYOPT,KEYEWC,KEYLIB,
     +              KEYMOD,KEYPIA,KEYRAD
      COMMON / MOMSET / p1(4),q1(4),p2(4),q2(4),phot(100,4),nphot
      REAL*8 P1,Q1,P2,Q2,PHOT
      COMMON / KGCOMM / PF1(4),QF1(4),PF2(4),QF2(4),SPHOT(100,4)
      COMMON / WGTALL / wtmod,wtcrud,wttrig,wtset(300)
      REAL*8 WTMOD,WTCRUD,WTTRIG,WTSET
      COMMON / HISTGPAR / THP1,THP2,THE1,THE2,X1,X2,NGMAX,NBIN
      REAL*8 THP1,THP2,THE1,THE2,X1,X2
      COMMON / CGLIB / B(50000)
      REAL*8 B
      REAL*8 S1,V
      INTEGER IDPHL(100)
      PARAMETER( RADEG= 180./3.141592653589)
      LOGICAL SAVE
C --------------------------------------------------------------------
      AMNMX(R)   = AMAX1 (-0.999999,AMIN1 (0.999999,R) )
      SACOS(R)   = REAL(DACOS(DBLE(AMNMX(R))))
C --------------------------------------------------------------------
C
C  Generate primary vertex
C
      CALL RANNOR (RN1,RN2)
      CALL RANNOR (RN3,DUM)
      VRTEX(1) = VPOS(1) + RN1*SDVRT(1)
      VRTEX(2) = VPOS(2) + RN2*SDVRT(2)
      VRTEX(3) = VPOS(3) + RN3*SDVRT(3)
      VRTEX(4) = 0.0
C
C   Fill 'VERT' bank
C
      IVMAI = 1
      JVERT = KBVERT(IVMAI, VRTEX, 0)
      IF (JVERT .EQ. 0) GO TO 98
      ECM=CMSENE
      IST=0
      IDP=0
C NTRK is increased later on by the no of stored photons
      NTRK=2
      NVRT=1
      WEI=0.
C
C  Event generation
C
      NEVENT(1) = NEVENT(1) + 1
      CALL BHWIDE( 0,xpar,npar)
      WEI=REAL(WTMOD)
C reverse the 3-momenta of all particles for ALEPH --> e- beam in +z dir
      DO I=1,3
        PF1(I)=-REAL(P1(I))
        QF1(I)=-REAL(Q1(I))
        PF2(I)=-REAL(P2(I))
        QF2(I)=-REAL(Q2(I))
      ENDDO
      PF1(4)=REAL(P1(4))
      QF1(4)=REAL(Q1(4))
      PF2(4)=REAL(P2(4))
      QF2(4)=REAL(Q2(4))
      DO I=1,NPHOT
        DO J=1,3
         SPHOT(I,J) =-REAL(PHOT(I,J))
        ENDDO
        SPHOT(I,4) = REAL(PHOT(I,4))
      ENDDO
C

      IF(.NOT.(KEYWGT.EQ.0 .OR. WTCRUD*WTTRIG .NE. 0D0)) THEN
C reject events with zero weight
        IST=1
        GOTO 999
      ENDIF
C Calculate angles for events with non-zero weight
      thetp= SACOS(REAL(-p2(3)/SQRT(p2(1)**2+p2(2)**2+p2(3)**2)))
      thetq= SACOS(REAL(-q2(3)/SQRT(q2(1)**2+q2(2)**2+q2(3)**2)))
C Theta distribution
          CALL hfill(11000,thetp*radeg,dum,wei)
          CALL hfill(11100,thetq*radeg,dum,wei)
C Energy distribution
          s1 = 2*(p2(4)*q2(4)-p2(3)*q2(3)-p2(2)*q2(2)-p2(1)*q2(1))
          v  = 1-s1/CmsEne**2
          vl = -1000.
          IF(v.GT.1d-10) vl = log10(v)
          CALL hfill(11200,vl,dum,wei)
C Photon multiplicity distribution
          CALL hfill(11300,float(nphot),dum,wei)
C Weight distribution
          IF(KeyWgt .NE. 0) CALL hfill(11400,wei,dum,1.)
C
C  Book particles
C
C  Book incoming positron
C
c      DO I=1,3
c        TABL(I)=PF1(I)
c      ENDDO
C   Let KBKINE calculate the energy of the particle
      TABL(1)=0.
      TABL(2)=0.
      TABL(3)=PF1(3)
      TABL(4)=0.
      JKINE  = KBKINE(-1, TABL, 2, 0)
      IF (JKINE .EQ. 0) GO TO 98
C
C  Book incoming electron
C
c      DO I=1,3
c        TABL(I)=QF1(I)
c      ENDDO
      TABL(1)=0.
      TABL(2)=0.
      TABL(3)=QF1(3)
      TABL(4)=0.
      JKINE  = KBKINE(-2, TABL, 3, 0)
      IF (JKINE .EQ. 0) GO TO 98
C
C Book outgoing positron
C
      DO I=1,3
        TABL(I)=PF2(I)
      ENDDO
      TABL(4)=0.
      JKINE  = KBKINE(1, TABL, 2, IVMAI)
      IF (JKINE .EQ. 0) GO TO 98
C
C Book outgoing electron
C
      DO I=1,3
        TABL(I)=QF2(I)
      ENDDO
      TABL(4)=0.
      JKINE  = KBKINE(2, TABL, 3, IVMAI)
      IF (JKINE .EQ. 0) GO TO 98
C
C Book all photons with energy > 1.E-6 GeV
C
      DO I=1,NPHOT
        IF(SPHOT(I,4).GT.1.E-6) THEN
          NTRK=NTRK+1
C Update process as number of photons
          IDP=IDP+1
          DO J=1,3
            TABL(J)=SPHOT(I,J)
          ENDDO
          TABL(4)=0.
          JKINE  = KBKINE(NTRK, TABL, 1, IVMAI)
          IF (JKINE .EQ. 0) GO TO 98
        ENDIF
      ENDDO
C
C  Fill history with 'KHIS' bank
C
      CALL VZERO(ITAB, NTRK)
      JKHIS=ALTABL('KHIS',1,NTRK,ITAB,'I','E')
      IF (JKHIS .EQ. 0) GO TO 98
C
C  Event counters
C
  999 CONTINUE
      IF(IST.EQ.0) NEVENT(2)=NEVENT(2)+1
      IF(IST.EQ.1) NEVENT(3)=NEVENT(3)+1
      RETURN
   98 CONTINUE
      IST=2
      NEVENT(4) = NEVENT(4) + 1
      RETURN
      END
      SUBROUTINE USCJOB
C--------------------------------------------------------------------
C J. Von Wimmersperg  19th Feb 2011
C--------------------------------------------------------------------
C --------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=50000)
C
      COMMON /BCS/   IW(LBCS )
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON / OUTIN / IINP,IOUT
      COMMON / INOUT / NINP,NOUT
C
      REAL SDVRT, VPOS, VRTEX
      REAL*8 XPAR
      COMMON/CDGENE/ SDVRT(3),VPOS(3),VRTEX(4),XPAR(100),NPAR(100),
     +               NEVENT(8)
      COMMON/GENPAR/CMSENE,THMINP,THMAXP,THMINE,THMAXE,ENMINP,ENMINE,
     +              ACOLLI,EPSCMS,WTMAX,AMAZ,GAMMZ,SINW2,AMTOP,AMHIG,
     +              KEYWGT,KEYRND,KEYCHA,KEYZOF,KEYOPT,KEYEWC,KEYLIB,
     +              KEYMOD,KEYPIA,KEYRAD
      COMMON / MOMSET / p1(4),q1(4),p2(4),q2(4),phot(100,4),nphot
      REAL*8 P1,Q1,P2,Q2,PHOT
      COMMON / WGTALL / wtmod,wtcrud,wttrig,wtset(300)
      REAL*8 WTMOD,WTCRUD,WTTRIG,WTSET
      COMMON / HISTGPAR / THP1,THP2,THE1,THE2,X1,X2,NGMAX,NBIN
      REAL*8 THP1,THP2,THE1,THE2,X1,X2
      COMMON / CGLIB / B(50000)
      REAL*8 B
C
C End of generation
C
      CALL BHWIDE( 2,XPAR,NPAR)
      Xsec = xpar(10)
      Erel = xpar(11)
      Xcru = xpar(20)
C create cross section bank
      CALL UGTSEC
C Print histograms... now done by HBOOK
CB      CALL gprint(1000)
CB      CALL gprint(1100)
CB      CALL gprint(1200)
CB      CALL gprint(1300)
CB      IF(KeyWgt .NE. 0) CALL gprint(1400)
C----------------------------------------------------------------------
C The part below is optional i.e. it can be commented out.
C Up to four interesting plots will be  produced in LaTeX format
C----------------------------------------------------------------------
C----Initialize GPLOT
CB      CALL gplint(0)
CB      noufig=11
CB      OPEN(unit=noufig,file='BHWIDE.TEX',STATUS='NEW')
CB      CALL gplcap(-noufig)
C Re-Normalize properly histos in nanobarns
CB      NEVTOT=NEVENT(1)
CB      xrange = thp2 - thp1
CB      FacNor  = Nbin/(Nevtot*ABS(xrange)) *Xcru
CB      CALL Gopera(1000,'+',1000,2000,FacNor,0d0)
CB      xrange = the2 - the1
CB      FacNor  = Nbin/(Nevtot*ABS(xrange)) *Xcru
CB      CALL Gopera(1100,'+',1100,2100,FacNor,0d0)
CB      xrange = x2 - x1
CB      FacNor  = Nbin/(Nevtot*ABS(xrange)) *Xcru
CB      CALL Gopera(1200,'+',1200,2200,FacNor,0d0)
CB      xrange = Ngmax
CB      Nbin   = Ngmax
CB      FacNor  = Nbin/(Nevtot*ABS(xrange)) *Xcru
CB      CALL Gopera(1300,'+',1300,2300,FacNor,0d0)
CB      FacNor  = 1/(Nevtot*1d0)
CB      CALL Gopera(1400,'+',1400,2400,FacNor,0d0)
C Plot histos
CB      CALL GPLTIT('e+: d(sigma)/d(theta) [nb/rad] $')
CB      CALL gminim(2000, 0.0d0)
CB      CALL gmaxim(2000, 2.5d0)
CB      CALL gplset('DMOD',2d0)
CB      CALL gidopt(2000,'ERRO')
CB      CALL gplot( 2000,' ','B',0)
C
CB      CALL GPLTIT('e-: d(sigma)/d(theta) [nb/rad] $')
CB      CALL gminim(2100, 0.0d0)
CB      CALL gmaxim(2100, 2.5d0)
CB      CALL gplset('DMOD',2d0)
CB      CALL gidopt(2100,'ERRO')
CB      CALL gplot( 2100,' ','B',0)
C
CB      CALL GPLTIT('d(sigma)/d(x), x=log10(1-s1/s) [nb]$')
CB      CALL gminim(2200, 0.0d0)
CB      CALL gmaxim(2200, 0.5d0)
CB      CALL gplset('DMOD',2d0)
CB      CALL gidopt(2200,'ERRO')
CB      CALL gplot( 2200,' ','B',0)
C
CB      CALL GPLTIT('d(sigma)/d(Ngamma), Ngamma - photon multipl. [nb]$'
CB      CALL gminim(2300, 0.0d0)
CB      CALL gmaxim(2300, 0.5d0)
CB      CALL gplset('DMOD',2d0)
CB      CALL gidopt(2300,'ERRO')
CB      CALL gplot( 2300,' ','B',0)
C----------------------------------------------------------------------
C Only for weighted events !
CB      IF(KeyWgt .NE. 0) THEN
CB       CALL GPLTIT(' Weight distrib. accepted evts, arbitrary normal $
CB       CALL gminim(2400, 0.0d0)
CB       CALL gmaxim(2400, 0.2d0)
CB       CALL gplset('DMOD',1d0)
CB       CALL gplot( 2400,' ','B',0)
CB      ENDIF
C
C---- Close TeX file
CB      CALL gplend
C
C Print event counters
C
       WRITE(IOUT,101)
  101  FORMAT(//20X,'EVENTS STATISTICS',
     &         /20X,'*****************')
       WRITE(IOUT,102) NEVENT(1),NEVENT(2),NEVENT(3)
  102  FORMAT(/5X,'# OF GENERATED EVENTS                      = ',I10,
     &        /5X,'# OF ACCEPTED  EVENTS                      = ',I10,
     &        /5X,'# OF ZERO WEIGHT EVENTS (ISTA = 1 )        = ',I10)
       WRITE(IOUT,103)
  103  FORMAT(//20X,'ERRORS STATISTICS',
     &         /20X,'*****************')
       WRITE(IOUT,104) NEVENT(4)
  104  FORMAT(/10X,'# EVENTS IST=2 NOT ENOUGH SPACE IN BANK   = ',I10)
       WRITE(IOUT,105)
  105  FORMAT(//14X,'CROSS SECTION WITHIN ACCEPTANCE',
     &         /14X,'*******************************')
       WRITE(IOUT,106) XSEC,XSEC*EREL
  106  FORMAT(/10X,'XSEC_ACCEP (pb)               =  ',G15.8,
     &        /10X,'ERROR      (pb)               =  ',G15.8,//)
C
      RETURN
      END
      SUBROUTINE UGTSEC
C--------------------------------------------------------------------
C     B.Bloch jan 1999 - create cross section bank
C     J.von Wimmersperg Feb 2011- update IVER to 104
C--------------------------------------------------------------------
      REAL*8 XPAR
      DIMENSION XPAR(100),NPAR(100)
C
      CALL BHWIDE( 1,XPAR,NPAR)
C create cross section bank
      NTOT = NPAR(10)
      XTOT = XPAR(10)
      RTOT = XPAR(11)*XPAR(10)
      IS = 1
      IDC = 2014
      IVER = 104
      NACC = NTOT
      XACC = XTOT
      RACC = RTOT
      isec = KSECBK(IS,IDC,IVER,NTOT,NACC,XTOT,RTOT,XACC,RACC)
      call prtabl('KSEC',0)
      return
      end
