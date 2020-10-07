      SUBROUTINE ASKUSI(IGCOD)
C---------------------------------------

C     IGCOD  for DFGT
      Integer       IGCO
      PARAMETER    (IGCO=7022)
      Integer       IGCOD 
      PARAMETER(lpdec=48)
      INTEGER nodec(lpdec)
      INTEGER alrlep
      EXTERNAL alrlep
      Real          FVERS
      DATA          FVERS/   
     $1.00    
     $                   /
      CHARACTER*30  DATE 
      DATA DATE/
     $ 'April 11  ,1997 '
     $/ 

      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=100000)
      COMMON /BCS/   IW(LBCS )                           
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
      Integer       MXIDHS  
      Parameter    (MXIDHS=500)
      Logical       HSKEEP
      Common/WORKHS/HSKEEP
      INTEGER         MDCY,MDME,KFDP
      REAL            BRAT 
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      CHARACTER*8 chaf
      COMMON /ludat4/ chaf(500)

      Integer       IPRI,IHST
      Real          BEAM,SVERT
      Integer       icoulu
      common/SUWGHT/IPRI,SVERT(3),icoulu(10)
      COMMON /STDATB/ BEAM

C------------Common of dfgt-------------------------------------
      COMMON /USRPRM/ SQRTS, POLEBM, SGMEBM, GAMSW1, ISRB, GAMWINO 
      REAL   *4       SQRTS, POLEBM, SGMEBM, GAMSW1, GAMWINO 
      INTEGER*4       ISRB
      COMMON /SSCONS/ AM0, AMU, AM2, TANB, AMA, RELAX
      REAL*4          AM0, AMU, AM2, TANB, AMA, RELAX
      COMMON /SSPTAB/ SWM, SZM, SFM, SHM, GMSW, GMSZ, GMSF, GMSH
      REAL*4          SWM(2), SZM(4), SFM(7), SHM(4),
     .                GMSW(2), GMSZ(4), GMSF(7), GMSH(4)

      COMMON /BPARM2/ ACC1,ACC2,ITMX1,ITMX2
      REAL*8 ACC1,ACC2
      COMMON /BSITPRKIN/  NCALW,NCALX, ITM1W,ITM1X, ITM2W,ITM2X
      INTEGER*4    NCALW,NCALX, ITM1W,ITM1X, ITM2W,ITM2X,ITMX1,ITMX2

       INTEGER IGBAS
       INTEGER IPMC
       COMMON /MCPARA/ IPMC
       COMMON/MCONS1/DMZ
       COMMON/MCONS2/DMW

       COMMON/MCONS4/DMT
       COMMON/MCONS5/DALPE
       COMMON/MCONS6/DALPS
       COMMON/MCONS7/DSIN2THW
       PARAMETER (MAXUSR=6)
       REAL RUSR(MAXUSR)
       COMMON /MCUSER/ RUSR
       INTEGER    JSEL
       COMMON /HCDEFF/JSEL

c-----------------------------------------------------------------------
      Integer       IUT,I   
      Real          TABL(100)
      Integer       NWBL,INDL
      Integer       ALTABL
      External      ALTABL
      Integer       IPART,IKLIN,IDBNK,JGCHC,ISMP
      Integer       NLINK,NAMIND
      External      NLINK,NAMIND
      Integer       LUCOMP
      External      LUCOMP 

c      LOG unit
      IUT = IW(6)
C     Return generator code IGCOD
      IGCOD = IGCO
      WRITE(IUT,1002)
      WRITE(IUT,1000)
      WRITE(IUT,1001) FVERS,DATE
      WRITE(IUT,1003) IGCOD 
      WRITE(IUT,1002)

      CALL MCEEVT(-2)          

c      BASES parameter card
c     ======================

      IGBAS = IW(NAMIND('GBAS'))
      IF(IGBAS.NE.0) THEN
         NCALW = IW(IGBAS+1)
         NCALX = IW(IGBAS+2)
         ITM1W = IW(IGBAS+3)
         ITM1X = IW(IGBAS+4)
         ITM2W = IW(IGBAS+5)
         ITM2X = IW(IGBAS+6)
         ACC1 =  RW(IGBAS+7)
         ACC2 =  RW(IGBAS+8)
      ELSE
         NCALW = 20000
         NCALX = 80000
         ITM1W = 8
         ITM1X = 5
         ITM2W = 10
         ITM2X = 5
         ACC1 =  0.2
         ACC2 =  0.01
      ENDIF      
      WRITE(*,*)'NCALW, ITM1W, ITM2W',NCALW,ITM1W,ITM2W
      WRITE(*,*)'NCALX, ITM1X, ITM2X',NCALX,ITM1X,ITM2X
      WRITE(*,*)'ACC1, ACC2',ACC1,ACC2

C
C     GENE card
C     =========
     
      IDBNK = NLINK('GENE',0)
c      write(*,*)' Idbnk  -GENE- ', idbnk
      if( IDBNK.gt.0 ) then
        BEAM = RW(IDBNK+1)
        IPRI = IW(IDBNK+2)
        IHST = IW(IDBNK+3)
      else
        write(IUT,*) 'ASKUSI: no GENE card given',
     &               'default values are taken'
        BEAM = 100.0
        IPRI = 10
        IHST =  0
      endif


      HSKEEP=.false.
      if( IHST.eq.1 )   HSKEEP=.true.
C     make use of a smearing of the vertex 

      IDBNK = NLINK('SVRT',0)
      IF( IDBNK.gt.0 ) then 
         SVERT(1) = RW(IDBNK+1)
         SVERT(2) = RW(IDBNK+2)
         SVERT(3) = RW(IDBNK+3)
      ELSE 
         SVERT(1) = 0.018
         SVERT(2) = 0.001
         SVERT(3) = 0.7
      ENDIF
C     initialize counters


      DO 5  i=1,10
 5    ICOULU(i)=0

C     initialize LUND
C -   load block data from JETSET with brute force.
C -   so it can't be missed
C      IF ( ECMS.LT.0.) CALL LUDATA 
C -   Final   state radiation
      MSTJ(41) = 2
C -   use non discrete masses for resonnances
      MSTJ(24) = 2
C -   SLAC fragm. functions for b,c  Symetric LUND for u,d,s
      MSTJ(11) = 3
C -   mod to lund fragm. functions params 
      PARJ ( 21)  =0.358
      PARJ  (41)  =0.500
      PARJ  (42)  =0.840
      PARJ  (81)  =0.310
      PARJ  (82)  =1.500
C -   mod Peterson's fragm. functions params
      PARJ  (54)  = -0.200
      PARJ  (55)  = -0.006
      PARU (102)  =  0.232
      PARJ (123)  =  91.17
      PARU (124)  =  2.5
C -   Set up some default values for masses and initial conditions
C -   HIGGS Mass , TOP Mass and Z0 mass defined, can be overwritten by
C -   a PMA1 card                         
      PMAS(LUCOMP(25),1)= 100.
      PMAS(LUCOMP( 6),1)= 174.                                     
      PMAS(LUCOMP(23),1)= 91.2
C          
C     get the SUSYGEN parameters from data card 


      IDBNK=NLINK('RUSR',0)
      if( IDBNK.gt.0 ) then
        RUSR(1) = RW(IDBNK+1)
        RUSR(2) = RW(IDBNK+2)
        RUSR(3) = RW(IDBNK+3)  
        RUSR(4) = RW(IDBNK+4)
        RUSR(5) = RW(IDBNK+5)
        RUSR(6) = RW(IDBNK+6)
      endif
***new*******************************
      JGCHC= NLINK('GCHC',0)
      IF( JGCHC.GT.0 ) THEN
          JSEL= IW(JGCHC+1)
      ELSE
C - Data card missing: enble all the channels ...
          JSEL=0
      END IF
      WRITE(*,*)'Select JSEL ',JSEL
C-  parameter of standard model
      ISMP=NLINK('PASM',0)
      if( ISMP.gt.0 ) then
        DSIN2THW = RW(ISMP+1)
        DALPE = RW(ISMP+2)
        DALPS = RW(ISMP+3)  
        IPMC  = IW(ISMP+4)
      ELSE
         write(IUT,*) 'ASKUSI: no PASM card given',
     &   'default values (0.231243, 0.0078125, 0.1175 0) are taken'
        DSIN2THW=0.231243         
        DALPE=0.0078125
        DALPS=0.1175
        IPMC =0
      END IF   
      WRITE(*,*)'SW2 , ALPHA , ALPHAS, ISR',DSIN2THW,DALPE,DALPS,IPMC
C - Finished!
***new********************************
c      write(*,*)' Relax', relax
      CALL MCEEVT(-1)          
*** Fill the X- information 
      kchi = LUCOMP(70)
      CHAF(kchi)='Chi0'
      PMAS(kchi,1) = SWM(1)
      PMAS(kchi,2) = GMSW(1)
      KCHG(kchi,1) = -3
      KCHG(kchi,3) = 1      

*** Fill  the X0 information
      kne = LUCOMP(66)
      CHAF(kne)='Neu'
      PMAS(kne,1) = SZM(1)
      PMAS(kne,2) = GMSZ(1)
      MDCY(kne,1) = 0 
      KCHG(kne,1) = 0 
      KCHG(kne,3) = 0

      CALL KXL74A (IPART,IKLIN) 
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN
        WRITE (IW(6),102)IPART,IKLIN
        STOP  
      ENDIF 
  102 FORMAT(1X,'error in PART or KLIN bank - STOP - ',2(I3))

C...Get list of lund particle # which should not be decayed
      mxdec = KNODEC(nodec,lpdec)
C     WRITE(6,*) 'KNODEC = ',mxdec,' LPDEC = ',lpdec
      mxdec = MIN(mxdec,lpdec)
C...Inhibit lund decays which should be done in galeph
      IF ( mxdec .GT. 0 ) THEN
        DO 10 i=1,mxdec
          IF ( nodec(i) .GT. 0 ) THEN
            jidb = NLINK('MDC1',nodec(i))
            IF ( jidb .eq. 0 ) mdcy(lucomp(nodec(i)),1) = 0
          ENDIF
   10   CONTINUE
      ENDIF

      call VZERO(TABL,100)
      NWBL = 0
      TABL(NWBL+ 1) = FLOAT(JSEL)
      TABL(NWBL+ 2) = 2.*BEAM
      TABL(NWBL+ 3) = FLOAT(IPMC)    
      TABL(NWBL+ 4) = AM0
      TABL(NWBL+ 5) = AMU
      TABL(NWBL+ 6) = AM2
      TABL(NWBL+ 7) = TANB
      TABL(NWBL+ 8) = AMA 
      TABL(NWBL+ 9) = RELAX 
      NWBL = NWBL+ 9

      TABL(NWBL+1) = SVERT(1)
      TABL(NWBL+2) = SVERT(2)
      TABL(NWBL+3) = SVERT(3)
      NWBL = NWBL + 3
      INDL = ALTABL('KPAR',NWBL,1,TABL,'2I,(F)','C')  
      CALL PRTABL('KPAR',0) 

      IECMS = NINT(BEAM*1000)
      JRLEP  = ALRLEP(IECMS,'    ',0,0,0)
      CALL PRTABL('RLEP',0) 
C 
      CALL PRPART
      CALL PRTABL('KPAR',0)
      

1000  FORMAT(/,10X,'* WELCOME TO DFGT/JETSET7.4 ' )
1001  FORMAT(  10X,'* ','Version ',F6.2,' -Last modified on   ',A30)
1002  FORMAT(  10X,72('*'))
1003  FORMAT(/,10X,'* DFGT - CODE NUMBER = ',I10)


      RETURN
      END
c--------------------------------------------------------------------
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECSS,WEIT)
C--------------------------------------------------------------------
C     input     : none
C
C     output    : 6 arguments
C          IDPR   : process identification if meaningful
C          ISTA   : status flag ( 0 means ok), use it to reject
C                   unwanted events
C          NTRK   : number of tracks generated and kept
C                  (i.e. # KINE banks  written)
C          NVRT   : number of vertices generated
C                   (i.e. # VERT banks written)
C          ECMS   : center of mass energy for the event (may be
C                   different from nominal cms energy)
C          WEIT   : event weight always equal to 1
C--------------------------------------------------------------------
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=100000)
C
      COMMON /BCS/   IW(LBCS )
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C
      INTEGER         MDCY,MDME,KFDP
      REAL            BRAT 
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      CHARACTER*8 chaf
      COMMON /ludat4/ chaf(500)


      COMMON /BPARM2/ ACC1,ACC2,ITMX1,ITMX2
      REAL*8 ACC1,ACC2
      COMMON /BSITPRKIN/  NCALW,NCALX, ITM1W,ITM1X, ITM2W,ITM2X
      INTEGER*4    NCALW,NCALX, ITM1W,ITM1X, ITM2W,ITM2X,ITMX1,ITMX2

      Integer       IDPR,ISTA,NTRK,NVRT
      Real          ECSS,WEIT
      COMMON /USRPRM/ SQRTS, POLEBM, SGMEBM, GAMSW1, ISRB, GAMWINO 
      REAL   *4       SQRTS, POLEBM, SGMEBM, GAMSW1, GAMWINO 
      INTEGER*4       ISRB
      COMMON /SSCONS/ AM0, AMU, AM2, TANB, AMA, RELAX
      REAL*4          AM0, AMU, AM2, TANB, AMA, RELAX
      INTEGER*4        IDPTCL, IHLCMB, ISRBM
      INTEGER ALTABL
      Real          BEAM,SVERT
      Real          TABL(100)
      Integer       icoulu
      Integer       IPRI,IHST
      common/SUWGHT/IPRI,SVERT(3),icoulu(10)
       COMMON /STDATB/ BEAM
       COMMON/MCONS5/DALPE
       INTEGER    JSEL
       COMMON /HCDEFF/JSEL
      COMMON /DTYP/   EVTYP
      Real          ptrak(4,2)
      Real          RX,RY,RZ,RE
      Real          VRTX(4)
C     Reset entries in /LUJETS/
   
 70   N = 0

 
      ISTA  = 0

      ECSS = BEAM*2
      IDPR = 0

C
C  Store beam particles also in bos banks
C
      ipart = kgpart(11)
      DO 2 itr = 1,2
         DO 9 i=1,4
 9       ptrak(i,itr) = 0.
         ipart = kgpart(11)
         ptrak(3,itr) = 0.5*ecss
         IF ( itr .EQ. 2 ) THEN
           ipart = kgpart(-11)
           ptrak(3,itr) =- 0.5*ecss
         ENDIF
         ist=kbkine(-itr,ptrak(1,itr),ipart,0)
         IF ( ist .LE. 0 ) THEN
            ista = -2
            GO TO 998
         ENDIF
  2   CONTINUE

C
C  Event generation
 
      CALL MCEEVT(0)
      WEIT = 1.0
      IDPR = EVTYP

      CALL RANNOR (RX,RY)
      CALL RANNOR (RZ,RE)
      VRTX(1) = RX*SVERT(1)
      VRTX(2) = RY*SVERT(2)
      VRTX(3) = RZ*SVERT(3)
      VRTX(4) = 0.


C
C  Book all banks
      CALL KXL7AL(VRTX,IST,NVRT,NTRK)
 998  ista = ist
C -   check for errors 
      IF (ISTA.EQ.0 ) THEN
         if(ICOULU(10).LT.4) call lulist(1)
         ICOULU(10) = ICOULU(10)+1
      ELSEIF (ISTA.GT.0) THEN
         ICOULU(1) = ICOULU(1) +1
         ICOULU(9) = ICOULU(9) +1
      ELSEIF ( ISTA.LT.0) THEN
         ICOULU(-ISTA) = ICOULU(-ISTA) +1
 
      ENDIF
      END
c
c--------------------------------------------------------------------
      SUBROUTINE USCJOB
C --------------------------------------------------------------------
C!  end of job routine
C                   
C --------------------------------------------------------------------
      Implicit None
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=100000)
      COMMON /BCS/   IW(LBCS )                                         
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
      Integer       MXIDHS
      Parameter    (MXIDHS=500)
      Logical       HSKEEP
      Common/WORKHS/HSKEEP
      Integer       IPRI,IHST
      Real          BEAM,SVERT,DALPE
      Integer       icoulu
      common/SUWGHT/IPRI,SVERT(3),icoulu(10)
       COMMON /STDATB/ BEAM
       COMMON/MCONS5/DALPE
      integer IUT,I
      logical HEXIST

      COMMON /BPARM2/ ACC1,ACC2,ITMX1,ITMX2
      REAL*8 ACC1,ACC2
      COMMON /BSITPRKIN/  NCALW,NCALX, ITM1W,ITM1X, ITM2W,ITM2X
      INTEGER*4    NCALW,NCALX, ITM1W,ITM1X, ITM2W,ITM2X,ITMX1,ITMX2

C     delete working histograms
C     ========================= 
      if(.not.HSKEEP) then
        do i=1,100
          if( HEXIST(i) ) call HDELET(i)
        enddo
      endif

      CALL MCEEVT(1)
C
      IUT = IW(6)   
      WRITE(IUT,101)
  101 FORMAT(//20X,'EVENTS STATISTICS',
     &        /20X,'*****************')
      WRITE(IUT,102) ICOULU(9)+ICOULU(10),ICOULU(10),ICOULU(9)
  102 FORMAT(/5X,'# OF GENERATED EVENTS                = ',I10,
     &       /5X,'# OF ACCEPTED  EVENTS                = ',I10,
     &       /5X,'# OF REJECTED  EVENTS                = ',I10)
      WRITE(IUT,103)
  103 FORMAT(//20X,'REJECT STATISTICS',
     &        /20X,'*****************')
      WRITE(IUT,104) (ICOULU(I),I=1,6)
  104 FORMAT(/10X,'IR= 1 LUND ERROR unknown part    # OF REJECT =',I10,
     &       /10X,'IR= 2 BOS  ERROR KINE/VERT       # OF REJECT =',I10,
     &       /10X,'IR= 3 LUND ERROR too many tracks # OF REJECT =',I10,
     &       /10X,'IR= 4 LUND ERROR Beam wrong pos  # OF REJECT =',I10, 
     &       /10X,'IR= 5 LUND ERROR status code >5  # OF REJECT =',I10, 
     &       /10X,'IR= 6-7-8 free for user          # OF REJECT =',I10)

      RETURN
      END
   


