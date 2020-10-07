      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)
C ------------------------------------------------------------
C -  B.Bloch January 2001
C -  J. von Wimmersperg 2011 Updated
C ! GET AN EVENT FROM KK2F version 4.19
C! then transfer the information into kine and vert banks.
C
C  
C
C     structure : subroutine
C     output arguments :
C          IDPR   : process identification,each digit corresponds to
C          the flavor of the evnt ( several flavors /event is possible)
C          ISTA   : status flag ( 0 means ok), use it to reject
C                   unwanted events
C          NTRK   : number of tracks generated and kept
C                  (i.e. # KINE banks  written)
C          NVRT   : number of vertices generated
C                   (i.e. # VERT banks written)
C          ECMS   : center of mass energy for the event (may be
C                   different from nominal cms energy)
C          WEIT   : event weight ( not 1 if a weighting method is used)
C -----------------------------------------------------------------
C*CA pyt6com
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      INTEGER PYK,PYCHGE,PYCOMP
C...Commonblocks.
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)

      COMMON/PYJETS/N7LU,NPAD,K7LU(LJNPAR,5),P7LU(LJNPAR,5),
     $              V7LU(LJNPAR,5)
      COMMON/PYDAT1/MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON/PYDAT2/KCHG(L2PAR,4),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /PYDAT3/ MDCY(L2PAR,3),MDME(LJNPAR,2),BRAT(LJNPAR),
     &                KFDP(LJNPAR,5)


      COMMON/PYDAT4/CHAF(L2PAR,2)
      CHARACTER CHAF*16
C
      COMMON/PYSUBS/MSEL,MSELPD,MSUB(L2PAR),KFIN(2,-40:40),CKIN(L1MST)
      COMMON/PYPARS/MSTP(L1MST),PARP(L1MST),MSTI(L1MST),PARI(L1MST)
      COMMON/PYINT1/MINT(400),VINT(400)
      COMMON/PYINT2/ISET(500),KFPR(500,2),COEF(500,20),ICOL(40,4,2)
      COMMON/PYINT3/XSFX(2,-40:40),ISIG(1000,3),SIGH(1000)
      COMMON/PYINT4/MWID(500),WIDS(500,5)
      COMMON/PYINT5/NGENPD,NGEN(0:500,3),XSEC(0:500,3)
      COMMON/PYINT6/PROC(0:500)
      CHARACTER PROC*28
      COMMON/PYMSSM/IMSS(0:99),RMSS(0:99)

      SAVE /PYJETS/
      SAVE /PYDAT1/,/PYDAT2/,/PYDAT3/,/PYDAT4/,/PYSUBS/,
     &/PYPARS/,/PYINT1/,/PYINT2/,/PYINT3/,/PYINT4/,/PYINT5/,
     &/PYINT6/,/PYMSSM/
C
      REAL*4 VRTX(4)
      INTEGER KFL(8)
C
C*CA bcs
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)
C
      COMMON /BCS/   IW(LBCS )
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON / GLUPAR / SVERT(3),XVRT(3),SXVRT(3),ECM,IFL,IFVRT
      REAL*4 ecm,svert,xvrt,sxvrt
      integer ifl,ifvrt
      COMMON / genflav/ igflav(40)
      COMMON / GLUSTA / ICOULU(10)
      REAL*4 PP,XX(2),et,ect,ent,thr
      REAL*8 TMIN,TMAX,YY,TT,DX
      LOGICAL KEEP
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT
      INTEGER NBITW, NBYTW, LCHAR
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458)
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
C
      INTEGER nmxhep         ! maximum number of particles
      PARAMETER (nmxhep=4000)
      DOUBLE PRECISION   phep, vhep
      INTEGER nevhep, nhep, isthep, idhep, jmohep, jdahep
      COMMON /hepevt/
     $     nevhep,           ! event number
     $     nhep,             ! number of particles
     $     isthep(nmxhep),   ! status code
     $     idhep(nmxhep),    ! particle ident KF
     $     jmohep(2,nmxhep), ! parent particles
     $     jdahep(2,nmxhep), ! childreen particles
     $     phep(5,nmxhep),   ! four-momentum, mass [GeV]
     $     vhep(4,nmxhep)    ! vertex [mm]
      SAVE  /hepevt/

      INTEGER nmxisr
      PARAMETER (nmxisr=100)

      REAL*4 ECMS,WEIT,EBEAM,RX,RXX,RY,RYY,RZ,RZZ,DUM,PT
      INTEGER    IMAX
      PARAMETER (IMAX = 10000)               ! length ox xpar
      REAL*8 XPAR(IMAX)

      DATA IFI / 0/
C ------------------------------------------------------------------
C
 10   continue
      ISTA = 0
      IFI  =  IFI + 1
      NTRK = 0
      NVRT = 0
C - SET THE CMS ENERGY FOR THIS EVENT ECMS = ECM
      ECMS = ECM
      EBEAM = ECM*0.5
      WEIT = 1.
      IDPR = 0
      IFL1 = 0
C reset entries and fragmentation info
      N7LU = 0
      MSTU(90) = 0
C
C - GET THE PRIMARY VERTEX
C
      CALL RANNOR (RX,RY)
      CALL RANNOR (RZ,DUM)
      VRTX(1) = RX*SVERT(1)
      VRTX(2) = RY*SVERT(2)
      VRTX(3) = RZ*SVERT(3)
      VRTX(4) = 0.
      IF ( IFVRT.ge.2) then
         CALL RANNOR(RXX,RYY)
         CALL RANNOR(RZZ,DUM)
         VRTX(1) = VRTX(1) + RXX*SXVRT(1)
         VRTX(2) = VRTX(2) + RYY*SXVRT(2)
         VRTX(3) = VRTX(3) + RZZ*SXVRT(3)
      ENDIF
      IF ( IFVRT.ge.1) then
         VRTX(1) = VRTX(1) + XVRT(1)
         VRTX(2) = VRTX(2) + XVRT(2)
         VRTX(3) = VRTX(3) + XVRT(3)
      ENDIF
C
C - GET AN EVENT FROM KK2F
C
      CALL KK2f_Make 
C
C     DEBUGGING FIRST FIVE EVENTS:
C
      IF (IFI.LE.5) then
       CALL PYLIST(2)
      ENDIF
C
C Look for flavor generated
      call HepEvt_GetKFfin(KFfin)
      idpr = kffin
*       call HepEvt_GetIdpr(idpr)
*        write(*,*)'idpr = ',idpr
C
C      Call the specific routine KXP6AL to fill BOS banks
C      the secondary vertices are propagated
      CALL KXP6AL (VRTX,ISTA,NVRT,NTRK)
      if ( ista.ne.0) then
        WRITE(iw(6),
     & '('' -ERROR booking KINE/VERT AT EVENT #  '',I10)') IFI,ista
          call pylist(3)
      ENDIF

C -   book fragmentation info
      CALL KP6ZFR (IST)
      IF ( IST.ne.0) then
         ista = ista+ist
        WRITE(iw(6),
     & '('' -ERROR booking KZFR AT EVENT #  '',I10)') IFI,ist
      ENDIF
C Get interesting weights and store some in KWGT
      call KKWGT(ist)
      if ( IST.eq.0) then
         ista = ista + 1000
        WRITE(iw(6),
     &  '('' -ERROR booking KWGT AT EVENT #  '',I10)') IFI,ist
      ENDIF
      IF (MSTU(24).NE.0) THEN
        WRITE(iw(6),'(''  ---ERROR PYEXEC AT EVENT #  '',I10)') IFI
        CALL PYLIST(1)
        ISTA = -8
      ENDIF
      if (ista.ne.0) go to 998
 998  IF (ISTA.EQ.0 ) THEN
         ICOULU(10) = ICOULU(10)+1
      ELSEIF (ISTA.GT.0) THEN
         ICOULU(1) = ICOULU(1) +1
         ICOULU(9) = ICOULU(9) +1
      ELSEIF ( ISTA.LT.0) THEN
         ICOULU(-ISTA) = ICOULU(-ISTA) +1
         ICOULU(9) = ICOULU(9) +1
      ENDIF
      if ( kffin.lt.20) igflav(kffin+20) = igflav(kffin+20)+1
      if ( ista.ne.0) go to 10
      if ( kffin.lt.20) igflav(kffin) = igflav(kffin)+1
C
C  -  FILLING HISTOGRAMS
C
C      CALL MAKHISTO
       call fillhis
      call pytabu(11)
      call pytabu(21)
C
C -  ANALYSE TREE INFORMATION:
C
      CALL PYEDIT(2)
      if(ifl.eq.14) n7lu=n7lu-2
      NT=0
      NTC=0
      NNT=0
      ET=0.
      ECT=0.
      ENT=0.
      DO 30 I=1,N7LU
       if ( ifl.eq.14 .and. i.lt.3) go to 30
       NT=NT+1
       ET=ET+P7LU(I,4)
       IF(ABS(PYP(I,6)).GT.0.1)THEN
        NTC=NTC+1
        ECT=ECT+P7LU(I,4)
       ELSE
        NNT=NNT+1
        ENT=ENT+P7LU(I,4)
        pt = sqrt(P7LU(I,1)**2+P7LU(I,2)**2)
        IF(pt.LT.0.0001)THEN
C          CALL HFILL(10016,real(P7LU(I,4)),0.,1.)
          ENT=ENT-P7LU(I,4)
        ENDIF
       ENDIF
 30   CONTINUE

      RETURN
      END
*****************************************************************************
*                                                                           *
*                                                                           *
*****************************************************************************
      SUBROUTINE ASKUSI(IGCOD)
C ------------------------------------------------------------------
C - B.Bloch   January 2001
C! Initialization routine of KK2F generator
C ------------------------------------------------------------------
C
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      INTEGER PYK,PYCHGE,PYCOMP
C...Commonblocks.
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)

      COMMON/PYJETS/N7LU,NPAD,K7LU(LJNPAR,5),P7LU(LJNPAR,5),
     $              V7LU(LJNPAR,5)
      COMMON/PYDAT1/MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON/PYDAT2/KCHG(L2PAR,4),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /PYDAT3/ MDCY(L2PAR,3),MDME(LJNPAR,2),BRAT(LJNPAR),
     &                KFDP(LJNPAR,5)
      COMMON/PYDAT4/CHAF(L2PAR,2)
      CHARACTER CHAF*16
C
      COMMON/PYSUBS/MSEL,MSELPD,MSUB(L2PAR),KFIN(2,-40:40),CKIN(L1MST)
      COMMON/PYPARS/MSTP(L1MST),PARP(L1MST),MSTI(L1MST),PARI(L1MST)
      COMMON/PYINT1/MINT(400),VINT(400)
      COMMON/PYINT2/ISET(500),KFPR(500,2),COEF(500,20),ICOL(40,4,2)
      COMMON/PYINT3/XSFX(2,-40:40),ISIG(1000,3),SIGH(1000)
      COMMON/PYINT4/MWID(500),WIDS(500,5)
      COMMON/PYINT5/NGENPD,NGEN(0:500,3),XSEC(0:500,3)
      COMMON/PYINT6/PROC(0:500)
      CHARACTER PROC*28
      COMMON/PYMSSM/IMSS(0:99),RMSS(0:99)

      SAVE /PYJETS/
      SAVE /PYDAT1/,/PYDAT2/,/PYDAT3/,/PYDAT4/,/PYSUBS/,
     &/PYPARS/,/PYINT1/,/PYINT2/,/PYINT3/,/PYINT4/,/PYINT5/,
     &/PYINT6/,/PYMSSM/
C
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)
C
      COMMON /BCS/   IW(LBCS )
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON / GLUSTA / ICOULU(10)
      COMMON /genflav/ igflav(40)
      COMMON / GLUPAR / SVERT(3),XVRT(3),SXVRT(3),ECM,IFL,IFVRT
      REAL*4 ecm,svert,xvrt,sxvrt
 
C     SVERT    : vertex smearing, set to 1998's values by default.
C     XVRT     : vertex offset, default = 0.,0.,0.
C     SXVRT    : additionnal vertex smearing, default = 0.,0.,0.
C     ECM      : nominal cms energy
C     IFL      : KK2F flavour
C     IFVRT    : IFVRT = 0 normal smearing SVRT applied
C                        1 + offset          XVRT applied
C                        2 + extra smearing  SXVRT applied
      REAL*4 TABL(50),dum
C
      PARAMETER (LPDEC=48)
      INTEGER NODEC(LPDEC)
      INTEGER ALTABL,ALRLEP
      EXTERNAL ALTABL,ALRLEP

      REAL DIZETVER,PHOVERS,TAUOLAVER
      EXTERNAL DIZETVER,PHOVERS,TAUOLAVER
      INTEGER    IMAX
      PARAMETER (IMAX = 10000)               ! length ox xpar
      REAL*8 XPAR(IMAX)
C
C  maximum code value for IFL parameter
      PARAMETER ( IFLMX = 10 )
C    IGCOD  for KK2F
      PARAMETER ( IGCO  =  5048)
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
C -----------------------------------------------------------------
C
      IUT = IW(6)
      DO I=1,10
       ICOULU(I) = 0
      ENDDO
      do i=1,40
       igflav(i) = 0
      enddo
      call kk2f_getversion(versio)
C
C   Return generator code IGCOD
C
      IGCOD = IGCO
      WRITE(IUT,101) versio,IGCOD
 101  FORMAT(/,10X,
     &       'KK2F Version',f10.3,' CODE NUMBER =',I4,
     $       ' LAST MODIFICATION ',
     $  ' December,2 2012 '
     & ,/,10X,'***********************************************',//)

C
C  Set up some default values for masses and initial conditions
C
      CALL KK2F_DEFAULT(XPAR)

         NLUND = NAMIND ('GKK4')
         JLUND = IW(NLUND)
         I1 = 23
         i2 = 22
         IF (JLUND .NE. 0) THEN
            IFL  = IW(JLUND+1)
*   5 FLAVOUR HADRON PRODUCTION
            IF(IFL.EQ.10)THEN
             DO 9 IL=1,5
              XPAR(400+IL) = 1.D0
 9           CONTINUE
C   E+E- PRODUCTION
            ELSEIF(IFL.EQ.11)THEN
             XPAR(411) = 1.D0
C   Mu+Mu- PRODUCTION
            ELSEIF(IFL.EQ.13)THEN
             XPAR(413) = 1.D0
C   Tau+ Tau- PRODUCTION
            ELSEIF(IFL.EQ.15)THEN
             XPAR(415) = 1.D0
C   Nu Nu~ PRODUCTION
            ELSEIF(IFL.EQ.12)THEN
             XPAR(412) = 1.D0
             XPAR(414) = 1.D0
             XPAR(416) = 1.D0
C  u u~ PRODUCTION
            ELSEIF(IFL.EQ.2)THEN
             XPAR(402) = 1.D0
C  d d~ PRODUCTION
            ELSEIF(IFL.EQ.1)THEN
             XPAR(401) = 1.D0
C c c~ PRODUCTION
            ELSEIF(IFL.EQ.4)THEN
             XPAR(404) = 1.D0
C s s~ PRODUCTION
            ELSEIF(IFL.EQ.3)THEN
             XPAR(403) = 1.D0
C b b~ PRODUCTION
            ELSEIF(IFL.EQ.5)THEN
             XPAR(405) = 1.D0
C b b~ PRODUCTION
            ELSEIF(IFL.EQ.6)THEN
             XPAR(406) = 1.D0
C user defined final state
            ELSEIF(IFL.EQ.99)THEN
             WRITE(IUT,*) 'YOU HAVE REQUESTED a user/XPAR setup '
            ENDIF
C*******
            ECM  = RW(JLUND+2)
            IPRT = IW(JLUND+3)
            AMZ =  RW(JLUND+4)
            AMH =  RW(JLUND+5)
            AMT =  RW(JLUND+6)
         ELSE
            IFL = 0
            ECM = 91.2
            IPRT = 0
            AMZ = 91.187
            AMH = 100.0
            AMT = 175.
         ENDIF
C         do il=1,16
C          print *,'flav type',il,xpar(400+il)
C         enddo 

         XPAR(1) = ECM       !CENTER OF MASS ENERGY
         XPAR(5) = dble(IPRT)!PRINTOUT LEVEL
         XPAR(502) = AMZ     ! Z MASS
         XPAR(805) = AMH     ! HIGGS MASS
         XPAR(806) = AMT     ! TOP MASS
C -  INPUT relative to ISR and FSR
         NRAD = NAMIND ('GKKR')
         JRAD = IW(NRAD)
         IF (JRAD .NE. 0) THEN
          XPAR(20) = RW(JRAD+1)  !ISR ON/OFF (=1/0)
          XPAR(21) = RW(JRAD+2)  !FSR ON/OFF (=1/0) 
          XPAR(27) = RW(JRAD+3)  !ISR FSR Interference ON/OFF (=2/0)
          XPAR(29) = RW(JRAD+4)  !PHOTON FROM FINAL QUARK ON/OFF (0/1)
         ENDIF
C - input for tau decays   GTAU card
         jtau = iw(namind('GTAU'))
         if ( jtau.gt.0) then
           xpar(2001) = dble(iw(jtau+1))
           xpar(2002) = dble(iw(jtau+2))
           xpar(2004) = dble(iw(jtau+4))
           xpar(2005) = dble(rw(jtau+5))
           xpar(2008) = dble(rw(jtau+6))
           xpar(2009) = dble(rw(jtau+7))   
         endif
C - input for Tau branching ratios GKBR card
         jkbr = iw(namind('GKBR'))
         if ( jkbr.gt.0) then
           xpar(2010) = dble(rw(jkbr+1))
           xpar(2011) = dble(rw(jkbr+4))
           xpar(2012) = dble(rw(jkbr+2))
           xpar(2013) = dble(rw(jkbr+3))
           sum = 0.
           do i = 1,22
                sum = sum + rw(jkbr+4+i)
           enddo
           do i = 1,22
              xpar(2100+i) = dble(rw(jkbr+4+i)/sum)
           enddo
         endif
C - input of any input parameter in case it's not foreseen in a card
         NAMI=NAMIND('XPAR')
         IF (IW(NAMI).EQ.0) GOTO 50
         KIND=NAMI+1
   15    KIND=IW(KIND-1)
         IF (KIND.EQ.0) GOTO 49
         LUPAR = LUPAR+1
         J = IW(KIND-2)
         xpar(j) = dble(rw(kind+1))
         go to 15
   49    CONTINUE
         CALL BKFMT ('XPAR','F')
         CALL BLIST (IW,'C+','XPAR')
   50    CONTINUE
         IF ((XPAR(20).EQ.0.D0 .OR. XPAR(21).EQ.0.D0) .AND. 
     &       (XPAR(27).NE.0.D0)) THEN
C no Interference from quarks if No FSR requested
          if (xpar(401)+xpar(402)+xpar(403)+xpar(404)+xpar(405)
     &                 .gt.0) then
          WRITE(IUT,*) '******** WARNING, ********'
          WRITE(IUT,*) 'YOU HAVE REQUESTED ISR/FSR INTERFERENCE'
          WRITE(IUT,*) 'WITH ISR OFF ? ',XPAR(20)
          WRITE(IUT,*) 'OR FSR OFF ?   ',XPAR(21)
          call exit
          endif
         ENDIF
         IF (XPAR(20).EQ.0.D0 .AND. XPAR(21).EQ.0.D0) THEN
          WRITE(IUT,*) '******** WARNING, ********'
          WRITE(IUT,*) 'YOU HAVE REQUESTED NO ISR AND NO FSR'
          WRITE(IUT,*) 'THIS IS NOT ALLOWED IN gps_partitionstart'
         ENDIF
C - 
         CALL KK2f_Initialize(xpar)                  ! initialize generator
C - make use of a smearing of the vertex  if it is given
         NSVER = NAMIND ('SVRT')
         JSVER = IW(NSVER)
         IF (JSVER .NE. 0) THEN
            SVERT(1) = RW(JSVER+1)
            SVERT(2) = RW(JSVER+2)
            SVERT(3) = RW(JSVER+3)
         ELSE
            SVERT(1) = 0.0113
            SVERT(2) = 0.0005
            SVERT(3) = 0.79
         ENDIF
C   get an offset for position of interaction point
C   if needed get a smearing on this position
C   XVRT    x      y      z    ( sz    sy    sz)
C
        call vzero(XVRT,3)
        CALL VZERO(SXVRT,3)
        IFVRT = 0
        NAXVRT=NAMIND('XVRT')
        JXVRT=IW(NAXVRT)
        IF (JXVRT.NE.0) THEN
           IFVRT = 1
           XVRT(1)=RW(JXVRT+1)
           XVRT(2)=RW(JXVRT+2)
           XVRT(3)=RW(JXVRT+3)
           IF ( IW(JXVRT).gt.3) then
              IFVRT = 2
              SXVRT(1)=RW(JXVRT+4)
              SXVRT(2)=RW(JXVRT+5)
              SXVRT(3)=RW(JXVRT+6)
           ENDIF
        ENDIF
C
C   Issue the relevant parameters
C
      WRITE (IUT,1000)
      WRITE (IUT,1007)
 1000 FORMAT(1X,78('*'),/,/,10X,'WELCOME TO KK2F & PYTHIA 6.1'/
     $                      10X,'                                   '/)
 1007 FORMAT (1X,78('*') )
C
C -- complete PART bank with LUND  particles
C    use the library routine KXP6IN
      CALL KXP6IN (IPART,IKLIN)
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN
         WRITE (IW(6),'(1X,''error in PART or KLIN bank - STOP - ''
     +                 ,2I3)') IPART,IKLIN
         CALL EXIT
      ENDIF
C  do not ask for FSR in JETSET if already here in KK2f
      sum = xpar(401)+xpar(402)+xpar(403)+xpar(404)+xpar(405)
      IF (XPAR(21).gt.0.D0 .AND. sum.gt. 0.) THEN
        WRITE(IUT,*) 'YOU HAVE REQUESTED FSR AND quark final state '
        WRITE(IUT,*) 'FSR in Parton shower is disabled '
        MSTJ(41) = 1             ! only gluon emission
      ENDIF
C   Make sure that masses in PART bank are consistent
C   in case they have been modified by data cards
      NAPAR = NAMIND('PART')
C This is the aleph number of the Z0(lund code=23),top (6) and Higgs(25)
C function KGPART returns the ALEPH code corresponding to the LUND code
C required.
      JPART = IW(NAPAR)
      IZPART = KGPART(23)
      IF (IZPART.GT.0)  THEN
        PMAS(PYCOMP(23),1) = amz
        ZMAS = PMAS(PYCOMP(23),1)
        KPART = KROW(JPART,IZPART)
        RW(KPART+6)=ZMAS
        IANTI = ITABL(JPART,IZPART,10)
        IF (IANTI.NE.IZPART) THEN
          KAPAR = KROW(JPART,IANTI)
          RW(KAPAR+6)=ZMAS
        ENDIF
      ENDIF
      ITPART = KGPART(6)
      IF (ITPART.GT.0)  THEN
        PMAS(PYCOMP( 6),1) = amt
        ZMAS = PMAS(PYCOMP( 6),1)
        KPART = KROW(JPART,ITPART)
        RW(KPART+6)=ZMAS
        IANTI = ITABL(JPART,ITPART,10)
        IF (IANTI.NE.ITPART) THEN
          KAPAR = KROW(JPART,IANTI)
          RW(KAPAR+6)=ZMAS
        ENDIF
      ENDIF
      IHPART = KGPART(25)
      IF (IHPART.GT.0)  THEN
        PMAS(PYCOMP(25),1) = amh
        ZMAS = PMAS(PYCOMP(25),1)
        KPART = KROW(JPART,IHPART)
        RW(KPART+6)=ZMAS
        IANTI = ITABL(JPART,IHPART,10)
        IF (IANTI.NE.IHPART) THEN
          KAPAR = KROW(JPART,IANTI)
          RW(KAPAR+6)=ZMAS
        ENDIF
      ENDIF
C
C - Print PART and KLIN banks
      IF (IPRT.GT.0) CALL PRPART
      IF (IPRT.GT.1) CALL PYLIST(12)
      IF (IPRT.LE.1) then
        if ( i1.gt.0) call kxp6st(i1)
        if ( i2.gt.0) call kxp6st(i2)
      endif
C
C -- get list of  particle# which should not be decayed
C    in LUND  because they are decayed in GALEPH.
C    the routines uses the KLIN bank and fills the user array
C    NODEC in the range [1-LPDEC]
      MXDEC = KNODEC (NODEC,LPDEC)
      MXDEC = MIN (MXDEC,LPDEC)
C
C -- inhibit decays in LUND
C    If the user has set some decay channels by data cards they will
C    will not be overwritten
      IF (MXDEC .GT. 0) THEN
         DO 40 I=1,MXDEC
            IF (NODEC(I).GT.0) THEN
               JIDB = NLINK('MDC1',NODEC(I))
               IF (JIDB .EQ. 0) MDCY(PYCOMP(NODEC(I)),1) = 0
            ENDIF
   40    CONTINUE
      ENDIF
C
C   dump the generator parameters for this run in a bank
C assume all parameters are real and stored as a single row
C      print *,versio
C      phver = phovers(dum)
C      print *,phver
      TABL(1) = versio
      TABL(2) = 203.
      TABL(3) = 6.21
      TABL(4) = 2.6
      TABL(5) = FLOAT(IFL)
      TABL(6) = ECM
      TABL(7) = amz
      TABL(8) = amh
      TABL(9) = amt
      TABL(10) = xpar(20)   ! keyisr
      TABL(11) = xpar(21)   ! keyfsr
      TABL(12) = xpar(27)   ! keyint
      TABL(13) = xpar(29)   ! keyQSR
      DO 11 I=1,3
      TABL(16+I) = XVRT(I)
      TABL(19+I) = sXVRT(I)
 11   TABL(13+I) = SVERT(I)
      NWB =  22
      IND = ALTABL('KPAR',NWB,1,TABL,'2I,(F)','C')
C create KORL bank from Tauola initialization
      NCHAN = 22
      NCOL = NCHAN+5
      NROW = 1
      tabl(1) = 2.6
      tabl(2) = xpar(2010)
      tabl(3) = xpar(2012)
      tabl(4) = xpar(2013)
      tabl(5) = xpar(2011)
      sum = 0.
      DO 57 IBR = 1,22
          sum = sum + xpar(2100+ibr)
          tabl(5+IBR) = xpar(2100+ibr)
 57   CONTINUE
      write(iut,*) ' Tau branching fractions used add up to',sum
      JKORL = ALTABL('KORL',NCOL,NROW,tabl,'2I,(F)','C')

C  Fill RLEP bank
      IEBEAM = NINT(ECM* 500. )
      JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)
      CALL PRTABL('RLEP',0)
      CALL PRTABL('KPAR',0)
      CALL PRTABL('KORL',0)
C
C -   keep fragmentation info
      MSTU(17) = 1
      call pytabu(10)
      call pytabu(20)
C
C  -  BOOKING HISTOGRAMS
C
      call bookhis

 99   CONTINUE
      RETURN
      END
      SUBROUTINE USCJOB
C-------------------------------------------------------------------
C! End of job routine    KK2F
C
C   To be filled by user to print any relevant info
C
C------------------------------------------------------------------
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      INTEGER PYK,PYCHGE,PYCOMP
C...Commonblocks.
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)

      COMMON/PYJETS/N7LU,NPAD,K7LU(LJNPAR,5),P7LU(LJNPAR,5),
     $              V7LU(LJNPAR,5)
      COMMON/PYDAT1/MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON/PYDAT2/KCHG(L2PAR,4),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /PYDAT3/ MDCY(L2PAR,3),MDME(LJNPAR,2),BRAT(LJNPAR),
     &                KFDP(LJNPAR,5)
      COMMON/PYDAT4/CHAF(L2PAR,2)
      CHARACTER CHAF*16
C
      COMMON/PYSUBS/MSEL,MSELPD,MSUB(L2PAR),KFIN(2,-40:40),CKIN(L1MST)
      COMMON/PYPARS/MSTP(L1MST),PARP(L1MST),MSTI(L1MST),PARI(L1MST)
      COMMON/PYINT1/MINT(400),VINT(400)
      COMMON/PYINT2/ISET(500),KFPR(500,2),COEF(500,20),ICOL(40,4,2)
      COMMON/PYINT3/XSFX(2,-40:40),ISIG(1000,3),SIGH(1000)
      COMMON/PYINT4/MWID(500),WIDS(500,5)
      COMMON/PYINT5/NGENPD,NGEN(0:500,3),XSEC(0:500,3)
      COMMON/PYINT6/PROC(0:500)
      CHARACTER PROC*28
      COMMON/PYINT7/SIGT(0:6,0:6,0:5)
C
C
      COMMON / GLUPAR / SVERT(3),XVRT(3),SXVRT(3),ECM,IFL,IFVRT
      REAL*4 ecm,svert,xvrt,sxvrt
      COMMON /genflav/ igflav(40)
      COMMON / GLUSTA / ICOULU(10)
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)
C
      COMMON /BCS/   IW(LBCS )
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C
C.......................................................................
C
C
      CALL KK2f_Finalize                  ! final bookkeping, printouts etc.
      CALL UGTSEC


       IUT=IW(6)
       WRITE(IUT,101)
  101  FORMAT(//20X,'EVENTS STATISTICS',
     &         /20X,'*****************')
       WRITE(IUT,102) ICOULU(9)+ICOULU(10),ICOULU(10),ICOULU(9)
  102  FORMAT(/5X,'# OF GENERATED EVENTS                = ',I10,
     &        /5X,'# OF ACCEPTED  EVENTS                = ',I10,
     &        /5X,'# OF REJECTED  EVENTS                = ',I10)
       do i=1,16
       if (igflav(i).gt.0) WRITE(IUT,103) i,igflav(i+20)
       if (igflav(i).gt.0) WRITE(IUT,104) i,igflav(i)
       enddo
  103  FORMAT(/5X,' flavor code',i5,'# OF GENERATED EVENTS = ',I10)
 104   FORMAT(/5X,' flavor code',i5,'# OF  ACCEPTED EVENTS = ',I10)
      call pytabu(12)
      call pytabu(22)
      RETURN
      END
      SUBROUTINE KKWGT(ist)
C-------B.Bloch march 2k ------------------- 
C    stored some interesting weights for each event
C    in bank KWGT. If all ok ist = 1, if one or more booking pb, ist=0
C-------------------------------------------------------------------------
      INTEGER     len      ! maximum number of auxiliary weights in WtSet
      PARAMETER ( len = 1000)
      Double precision wtmain,wtset(len)
      INTEGER  kwgtbk
      EXTERNAL kwgtbk 
      ist = 1
      call kk2f_getwtlist(wtmain,wtset)
      weik = wtset(71)    ! Born
      ind = kwgtbk(1,1,weik)
      if (ind.le.0) ist = 0
      weik = wtset(72)    ! 1st order
      ind = kwgtbk(2,2,weik)
      if (ind.le.0) ist = 0
      weik = wtset(73)    ! 2nd order
      ind = kwgtbk(3,3,weik)
      if (ind.le.0) ist = 0
      weik = wtset(74)    ! 3rd order
      ind = kwgtbk(4,4,weik)
      if (ind.le.0) ist = 0
      weik = wtset(201)    ! FSR order 0
      ind = kwgtbk(5,11,weik)
      if (ind.le.0) ist = 0
      weik = wtset(202)    ! FSR 1st order
      ind = kwgtbk(6,12,weik)
      if (ind.le.0) ist = 0
      weik = wtset(203)    ! FSR 2nd order
      ind = kwgtbk(7,13,weik)
      if (ind.le.0) ist = 0
      weik = wtset(251)    ! FSR order 0 no interf
      ind = kwgtbk(8,21,weik)
      if (ind.le.0) ist = 0
      weik = wtset(252)    ! FSR 1st order no interf
      ind = kwgtbk(9,22,weik)
      if (ind.le.0) ist = 0
      weik = wtset(253)    ! FSR 2nd order no interf
      ind = kwgtbk(10,23,weik)
      if (ind.le.0) ist = 0
      weik = wtset(213)    ! Virtual pairs and IFI ON 
      ind = kwgtbk(11,31,weik)
      if (ind.le.0) ist = 0
      weik = wtset(263)    ! Virtual pairs and IFI Off
      ind = kwgtbk(12,32,weik)
      if (ind.le.0) ist = 0
      RETURN
      END
      SUBROUTINE UGTSEC
*-------B.Bloch dec 99 -------------------
C     get the cross section at any time in the program ...
*//////////////////////this is kk2f.h/////////////////////////////////////////
      DOUBLE PRECISION    m_version
      CHARACTER*14        m_Date
      PARAMETER ( m_Version     =          4.19d0 ) 
      PARAMETER ( m_Date        =  ' 25 Sept  2002') 
*////////////////////////////////////////////////////////////////////////////
      INTEGER     m_phmax             ! maximum photon multiplicity ISR+FSR
      PARAMETER ( m_phmax = 100)
      INTEGER     m_jlim
      PARAMETER(  m_jlim  = 10000)
      INTEGER     m_lenwt       ! maximum number of auxiliary weights in WtSet
      PARAMETER ( m_lenwt = 1000)
*/////////////////////////////////////////////////////////////////////////////
      DOUBLE PRECISION       m_xpar,   m_ypar
      INTEGER     m_out,    m_Idyfs,  m_nevgen, m_idgen,  m_KeyWgt
      INTEGER            m_npmax,  m_idbra,  m_nphot
      INTEGER     m_KeyHad, m_KeyISR, m_KeyFSR, m_KeyINT,  m_KeyGPS
      INTEGER            m_Phel,   m_isr,    m_KFini,  m_IsBeamPolarized
      DOUBLE PRECISION m_CMSene, m_DelEne, m_Xcrunb, m_WTmax,m_HadMin,
     &                    m_MasPhot,  m_Emin
      DOUBLE PRECISION       m_BornCru,m_WtCrud, m_WtMain, m_sphot
      DOUBLE PRECISION       m_WtSet,m_WtList,m_alfinv,m_vcut,m_Xenph
      DOUBLE PRECISION       m_p1,     m_p2,     m_q1,     m_q2
      DOUBLE PRECISION       m_PolBeam1,         m_PolBeam2
      DOUBLE PRECISION       m_xSecPb, m_xErrPb
*
      COMMON /c_KK2f/
     $    m_CMSene,           ! CMS energy average
     $    m_PolBeam1(4),      ! Spin Polarization vector first beam
     $    m_PolBeam2(4),      ! Spin Polarization vector second beam
     $    m_DelEne,           ! Beam energy spread [GeV]
     $    m_HadMin,           ! minimum hadronization energy [GeV]
     $    m_xpar(m_jlim),  ! input parameters, READ ONLY, never touch them!!!! 
                              ! them!!!!
     $    m_ypar(m_jlim),     ! debug facility
     $    m_WTmax,            ! maximum weight
     $    m_alfinv,           ! inverse alfaQED
     $    m_Emin,             ! minimum real photon energy, IR regulator
     $    m_MasPhot,          ! ficticious photon mass,     IR regulator
     $    m_Xenph,            ! Enhancement factor for Crude photon multiplicity
     $    m_vcut(3),   ! technical cut on E/Ebeam for non-IR real contributions
     $    m_xSecPb,           ! output cross-section available through getter
     $    m_xErrPb,           ! output crossxsection available through getter
*----------- EVENT-----------------
     $    m_p1(4),            ! e- beam
     $    m_p2(4),            ! e+ beam
     $    m_q1(4),            ! final fermion
     $    m_q2(4),            ! final anti-fermion
     $    m_sphot(m_phmax,4), ! photon momenta
     $    m_WtMain,           ! MAIN weight of KK2f
     $    m_WtCrud,           ! crude distr from ISR and FSR
     $    m_WtSet(m_lenwt),   ! complete list of weights
     $    m_WtList(m_lenwt),  ! complete list of weights
     $    m_Xcrunb,           ! crude in nanobarns
     $    m_BornCru,          ! Crude Born
     $    m_isr( m_phmax),    ! =1 for isr, 0 for fsr
     $    m_Phel(m_phmax),    ! photon helicity =1,0 for +,-
     $    m_nphot,            ! Total Photon multiplicity
*----------------------------------
     $    m_IsBeamPolarized,  ! status of beam polarization
     $    m_KFini,            ! KF of beam fermion
     $    m_KeyWgt,           ! weight=1 on/off switch
     $    m_KeyHad,           ! hadronization switch
     $    m_nevgen,           ! serial number of the event
     $    m_npmax,            ! maximum photon multiplicity
     $    m_KeyISR,           ! ISR switch
     $    m_KeyFSR,           ! FSR switch
     $    m_KeyINT,           ! ISR/FSR INTereference switch
     $    m_KeyGPS,           ! New exponentiation switch
     $    m_out,              ! output unit number
     $    m_Idyfs,            ! pointer for histograming
     $    m_idbra,            ! pointer for brancher
     $    m_idgen             ! special histogram for this generator
      DOUBLE PRECISION       xkarl,error,erkarl,erabs
      DOUBLE PRECISION       errela,averwt
      DOUBLE PRECISION       xsmc,erel
      DOUBLE PRECISION       WTsup
      DOUBLE PRECISION              BornV_Sig0nb,BornV_Integrated 
      DOUBLE PRECISION       AvUnd, AvOve
      DOUBLE PRECISION XSECPB, XERRPB
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)
      COMMON /BCS/   IW(LBCS )
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
      integer icoulu,igflav
      COMMON / GLUSTA / ICOULU(10)
      COMMON /genflav/ igflav(40)
      integer ito,LevelPrint,idc,iver,ntot,nacc,iut,is,isec,i
      integer Nneg, Nove, Nzer,KSECBK
      real*8 sig0pb,xBorPb
      real*4 XTOT,RTOT,XACC,RACC,big,XPART,eps
C-----------------------------------------------------------------------
      real*8 aaa0(6),aaac(6),aa0,aac
      data aaa0 /  300.0D0 , -300.0D0 ,  300.0D0 , 3*0.0D0 /
      data aaac /  300.0D0,  2*0.0D0, -300.0D0, 300.0D0,  0.0D0 /
C-----------------------------------------------------------------------
      DATA ITO/0/
      data big/ 1000./             !   nb to pb
      SAVE ITO
      ITO = ITO + 1      
      IF (ITO.EQ.1) THEN
      sig0pb =  BornV_Sig0nb(m_CMSene)*1000d0
      xBorPb =  BornV_Integrated(0d0,m_CMSene**2) * sig0pb
* Crude from karLud + printout
      LevelPrint = 0
      CALL KarLud_Finalize(LevelPrint,xkarl,error)
      erkarl = 0d0

* Printout from Karfin
      CALL KarFin_Finalize(LevelPrint)

* Average of the main weight
      CALL GLK_MgetAve(m_idbra, AverWt, ErRela, WtSup)

* main X-section = crude * <WtMain>
      xsmc   =  xkarl*averwt
      erel   =  SQRT(erkarl**2+errela**2)
      erabs  =  xsmc*erel
* The final cross section exported to user
* through getter KK2f_GetXsecMC(xSecPb, xErrPb)
      xSecPb =  xsmc*sig0pb     ! MC xsection in picobarns
      xErrPb =  xSecPb*erel     ! Its error   in picobarns
      print *,' x-section (pb)',xSecPb,' +-',xErrPb
      XTOT = xSecPb/big
      RTOT = xErrPb/big
      ELSEIF (ITO.GT.1) then 
       CALL KK2f_GetXsecMC(xSecPb, xErrPb) ! get MC x-section    
      print *,' x-section (pb)',xSecPb,' +-',xErrPb
         XTOT = xSecPb/big
         RTOT = xErrPb/big
      endif
      IDC = 5048
      IVER = 100*m_Version
      NTOT = ICOULU(10)+icoulu(9)
      NACC = ICOULU(10)             ! 
      XACC = XTOT*float(nacc)/float(ntot)                   !
      RACC = RTOT*xtot/xacc
      racc = xacc*sqrt(1./float(nacc)-1./float(ntot))                   !
      is =1
C

      IUT=IW(6)
      WRITE (IUT,*) '******* KK2F CROSS-SECTION ************'
      WRITE (IUT,*) '** CROSS-SECTION BANK CREATED WITH *******'
      WRITE (IUT,*) 'XS =',XTOT,' +- ',RTOT,' nb'
      WRITE (IUT,*) 'NUMBER OF EVENTS :', NTOT

      ISEC =  KSECBK(IS,IDC,IVER,NTOT,NACC,XTOT,RTOT,XACC,RACC)
C      ENDIF
      do i=1,16
          if (igflav(i).gt.0) is = is+1
      enddo
      if (is.eq.2 )  go to 100
      is =1
      do i=1,16
        if (igflav(i).gt.0) then
          is = is+1
          CALL GLK_MgetAve(m_idbra+1, AverWt, ErRela, WtSup)
          NTOT = igflav(i+20)
          NACC = igflav(i)
          eps = float(igflav(i+20))/float(icoulu(10)+icoulu(9))
C          deps = 1./float(igflav(i)) - 1./float(icoulu(10))
          XPART = XTOT*eps
          XACP = XACC*float(igflav(i))/float(icoulu(10))
          RTOT = XPART*errela
C          RTOT = xparT*sqrt(deps + erel*erel )
          RACC = RTOT
      ISEC =  KSECBK(IS,IDC,I,NTOT,NACC,XPART,RTOT,XACP,RACC)
        endif
      enddo
 100  continue
      CALL PRTABL('KSEC',0)
      return
      END

      SUBROUTINE fillhis
      real*8  pf1(4),pf2(4),phot(100,4)
      
      call HepEvt_GetFfins(pf1,pf2)
      e1 = pf1(4)
      e2 = pf2(4)
      call hfill (10001,e1,dum,1.)
      call hfill (10002,e2,dum,1.)
      call HepEvt_GetKFfin(KFfin)
      call hfill (10003,float(kffin),dum,1.)
      call HepEvt_GetPhotIni(nphot,phot)
      call hfill (10004,float(nphot),dum,1.)
      if ( nphot.gt.0) then
        do i=1,nphot
           eg = phot(i,4)
           call hfill(10005,eg,dum,1.)
        enddo
      endif
C      call HepEvt_GetPhotBst(nphot,phot)
C      call hfill (10006,float(nphot),dum,1.)
C      if ( nphot.gt.0) then
C        do i=1,nphot
C           eg = phot(i,4)
C           call hfill(10007,eg, dum,1.)
C        enddo
C      endif
      call HepEvt_GetPhotFin(nphot,phot)
      call hfill (10008,float(nphot),dum,1.)
      if ( nphot.gt.0) then
        do i=1,nphot
           eg = phot(i,4)
           call hfill(10009,eg,dum,1.)
        enddo
      endif
      return
      entry bookhis
      call hbook1 (10001,'Energy fermion 1 ',50,0.,150.,0.)
      call hbook1 (10002,'Energy fermion 2 ',50,0.,150.,0.)
      call hbook1 (10003,'fermion codes  ',20,0.,20.,0.)
      call hbook1 (10004,'Nbr ISR photons per event ',50,0.,50.,0.)
      call hbook1 (10005,'Energy ISR photons',50,0.,100.,0.)
      call hbook1 (10006,'Nbr Beam photons ',50,0.,50.,0.)
      call hbook1 (10007,'Energy Beam photons ',50,0.,100.,0.)
      call hbook1 (10008,'Nbr FSR photons',50,0.,50.,0.) 
      call hbook1 (10009,'Energy FSR photons',50,0.,100.,0.)
      return
      end
      SUBROUTINE MAKHISTO
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)

      REAL*8    p1(4),p2(4),p3(4),p4(4)
      INTEGER   nbv, idv, it, KFfin
      REAL*8    ss2, vv, ss, vmin,  vmax

      REAL*8 PX,PY,PZ,ETOT,SPH,APL,THR,OBL
      REAL*8 PXG,PYG,PZG,EG
      REAL*8 PXKK,PYKK,PZKK,ETOTKK
      INTEGER NT,NTKK,MCONV,NPI,NPIKK,NG
      INTEGER LREC,ISTAT,IDVF,NWPAWC,ICYCLE,NVAR,NVARP

      INTEGER nmxhep         ! maximum number of particles
      PARAMETER (nmxhep=2000)
      REAL*8  phep, vhep
      INTEGER nevhep, nhep, isthep, idhep, jmohep, jdahep
      COMMON /d_hepevt/
     $     nevhep,           ! serial number
     $     nhep,             ! number of particles
     $     isthep(nmxhep),   ! status code
     $     idhep(nmxhep),    ! particle ident KF
     $     jmohep(2,nmxhep), ! parent particles
     $     jdahep(2,nmxhep), ! childreen particles
     $     phep(5,nmxhep),   ! four-momentum, mass [GeV]
     $     vhep(4,nmxhep)    ! vertex [mm]

      PARAMETER (LJNPAR=4000)
      COMMON/PYJETS/N7LU,NPAD,K7LU(LJNPAR,5),P7LU(LJNPAR,5),
     $              V7LU(LJNPAR,5)

      PARAMETER (NVAR = 20)
      REAL VAR(NVAR)
      PARAMETER (NVARP = 4)
      REAL PHOT(NVARP)

         DO it=1,4
            p1(it) =phep(it,1)    ! first  beam
            p2(it) =phep(it,2)    ! second beam
            p3(it) =phep(it,3)    ! first fermion
            p4(it) =phep(it,4)    ! second fermion
         ENDDO
         KFfin = idhep(3)     ! Pythia manual, page 53 !

         ss  =  (p1(4)+p2(4))**2 -(p1(3)+p2(3))**2 
     &          -(p1(2)+p2(2))**2 -(p1(1)+p2(1))**2
         ss2 =  (p3(4)+p4(4))**2 -(p3(3)+p4(3))**2 
     &          -(p3(2)+p4(2))**2 -(p3(1)+p4(1))**2

         vv  = 1d0 -ss2/ss

* loop on not decayed particle - lujets
         call vzero(var, nvar)
         px     = 0.0
         py     = 0.0
         pz     = 0.0
         etot   = 0.0 
         nt     = 0
         npi    = 0
         pxg    = 0.0
         pyg    = 0.0
         pzg    = 0.0
         eg     = 0.0
         ng     = 0
   
         DO it = 3,n7LU
          IF (K7LU(it,1).ge.1 .and. K7LU(it,1).le.10) THEN
* calculate cos theta 
           ct = p7LU(it,3) / sqrt (p7LU(it,4)**2 - p7LU(it,5)**2)
           IF (abs(ct).lt.0.95) THEN 
            px = p7LU(it,1) + px
            py = p7LU(it,2) + py
            pz = p7LU(it,3) + pz
            etot = p7LU(it,4) + etot
            nt = nt  + 1 
            IF (abs(K7LU(it,2)).eq.211) npi = npi + 1
           ENDIF
          ENDIF
         ENDDO

* Take JETSET to HEPEVT routine, and do JETSET to D_HEPEVT

         MCONV  = 1
         call pyhepc(mconv)
C         CALL HEPEVT_LUHEPC(MCONV)
* Check with previous result         
         pxkk = 0.0
         pykk = 0.0
         pzkk = 0.0
         etotkk = 0.0 
         ntkk = 0
         npikk = 0

         DO it = 3,nhep
          IF (isthep(it).eq.1) THEN
* calculate cos theta 
           ct = phep(3,it) / sqrt (phep(4,it)**2 - phep(5,it)**2)
           IF (abs(ct).lt.0.95) THEN 
            pxkk = phep(1,it) + pxkk
            pykk = phep(2,it) + pykk
            pzkk = phep(3,it) + pzkk
            etotkk = phep(4,it) + etotkk
            ntkk = ntkk  + 1 
            IF (abs(IDHEP(it)).eq.211) npikk = npikk + 1
           ENDIF
* photons
           IF(idhep(it).eq.22 .and. jmohep(1,it).eq.1) THEN
            PHOT(1) = phep(1,it)
            PHOT(2) = phep(2,it)
            PHOT(3) = phep(3,it)
            PHOT(4) = phep(4,it)
            PXG = PXG + PHOT(1)
            PYG = PYG + PHOT(2)
            PZG = PZG + PHOT(3)
            EG  = EG  + PHOT(4)
            NG = NG + 1
            CALL HFN(501,PHOT)
           ENDIF
          ENDIF
         ENDDO
***********************************************************************
* Event analysis routine from PYTHIA                                  *
* all particles with K(it,1).ge.1 .and. K(it,1).le.10 considered      *
***********************************************************************     
         CALL PYSPHE(SPH, APL)
         CALL PYTHRU(THR, OBL)
*******************
* Histo filling...*
*******************
         VAR(1) = vv
         VAR(2) = float(nt)
         VAR(3) = px
         VAR(4) = py
         VAR(5) = pz
         VAR(6) = etot
         VAR(7) = float(ntkk)
         VAR(8) = etotkk
         VAR(9) = float(KFfin)
         VAR(10) = float(npi)
         VAR(11) = float(npikk)
         VAR(12) = sph
         VAR(13) = apl
         VAR(14) = thr
         VAR(15) = obl
         VAR(16) = pxg
         VAR(17) = pyg
         VAR(18) = pzg
         VAR(19) = eg
         VAR(20) = float(ng)

C         CALL HFN(500,VAR)

      RETURN
      END
