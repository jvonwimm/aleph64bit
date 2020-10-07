      SUBROUTINE ASKUSI(IGCOD)                                         
C--------------------------------------------------------------------   
C                                                                       
C                B.Bloch      September 2002                            
C                                                                       
C                NUNUGPV       Initialisation                                  
C                                                                       
C--------------------------------------------------------------------   
      IMPLICIT REAL*8 (A-H,O-Z)

      INTEGER LMHLEN, LMHCOL, LMHROW                                    
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          
C                                                                       
      PARAMETER (LBCS=50000,LCHAR=4)                                    
      COMMON/BCS/ IW(LBCS)                                              
      INTEGER IW                                                        
      REAL RW(LBCS)                                                     
      EQUIVALENCE (RW(1),IW(1))                                         
      COMMON / COUNTR / NEVENT(3)                                       
C                                                                       
      INTEGER IGCO,IGCOD
      PARAMETER ( IGCO = 1009)                                          
C 
      REAL*4 SVRT,ECMS,gpar, sumano                                                                     
      DIMENSION SVRT(3)
      INTEGER jgene,NFL,ALTABL, alrlep                                               integer izero
      character *4 blankc
      parameter(izero = 0, blankc='    ')
      common / genepar/ GPAR(50)
      common/ anomx/ sumano(6)
      integer irndm
      common/random/irndm(3)
C                                                                       
C     common(s) where the parameters are passed to the generator        
C                                                                       
C
C   commons from nunugpv
C
      CHARACTER*1 OGEN, NFLV, OANOM, OFAST
      REAL*8 MNU,MNU2

      COMMON/CMENERGY/RS,S  
      COMMON/PAR/WM,WG,CFACT,GF,G,ZM,ZW,PI,ALPHA,EM2       
      COMMON/PARD/ZM2,ZW2,WM2,WG2,GF2,G2,G4,G8
      COMMON/Higgs/HM,HW1
      COMMON/FM/EM,AMU,TLM,UQM,DQM,CQM,SQM,BQM,TQM
      COMMON/RPAR/RWM2,RWG2,RZM2,RZW2,RRW,RZW,RW2
      COMMON/THETAW/STH2
      COMMON/EM/RL,ETA,BETA,SDELTA,BETAM
C      COMMON/PAWC/PAW(NWPAWC)
      COMMON/OG/OGEN
      COMMON/S2CH/NSCH
      COMMON/AIN/ALPHAI
      COMMON/EMC/IQED
      COMMON/RELACC/EPS
      COMMON/QCM/QMF1,QMF2,QMF3,QMF4,QMF5,QMF6
      COMMON/CHANN/ICHANNEL
      COMMON/CUTS/EGMINL,EGMAXL,CMINL,CMAXL
      COMMON/CVETO/CMINV,CMAXV    
      COMMON/NG/NPHOT   
      COMMON/CALLS/NHITWMAX,NHITMAX,NCALLS
      COMMON/MAXVALUE/FMAX
      COMMON/ANOM/OANOM
      COMMON/ANOMV/XG,YG
      COMMON/NUMASS/MNU,MNU2
      COMMON/PT/IPT
C     input for QGC
      CHARACTER*1 OQGC
      real*8 XLAMBDA, A0W,ACW,A0Z,ACZ
      COMMON/QGC/XLAMBDA, A0W,ACW,A0Z,ACZ,OQGC
      PARAMETER (LUX=4,K1=0,K2=0,LEN=25)
      data inpt/1234543/
C                                                                       
C   Return the generator code as defined in the KINGAL library          
C                                                                       
      IGCOD = IGCO                                                      
C                                                                       
C   Get Generator parameters ( from a data card 
C    or by default values )                                
C                                                                       
      LWRITE = IW(6)                                                    
      WRITE(LWRITE,101) IGCOD                                           
 101  FORMAT(/,10X,                                                     
     &    'NuNuGPV- CODE NUMBER =',I4,' Last mod. September ,2002',   
     & /,10X,'**************************************************',//)   
C                                                                       
C   Input needed by NuNuGPV
C                                                                       
C---------------------------------------------------------------------  
C 
*1,2,3,-1,-4 [NUMBER OF DETECTED GAMMA NPHOT; -1 = AT LEAST ONE; -4 = MASSIVE]
*F,M,T,E     [NEUTRINO FLAVOUR; F= SUM OVER THREE FLAVOURS]
*I,G         [INTEGRATION OR GENERATION MODE]
*314159265   [RANDOM NUMBER SEED, INTEGER NUMBER]
*190.D0      [C.M. ENERGY]
*1000        [NUMBER OF EVENTS]
*1,0         [1= YES IS QED, 0= TREE LEVEL; 1 AUTOMATICALLY FOR NPHOT= -1]
*1,0         [1= ISR WITH P_T; 0= COLLINEAR ISR; 1 AUTOMATICALLY FOR NPHOT= -1]
*45.D0       [MINIMUM PHOTON SCATTERING ANGLE]
*10.D0       [MINIMUM PHOTON ENERGY]
*95.D0       [MAXIMUM PHOTON ENERGY]
*N           [OPTION FOR ANOMALOUS COUPLINGS (Y,N)]
*             IF Y, XG AND YG MUST BE PROVIDED
*N           [OPTION OFAST (Y,N); ACTIVE ONLY FOR NPHOT= -1]
*10.d0       [fourth generation neutrino mass]
*128.87D0    [1/ALPHA(M_Z)]
*N           [OPTION FOR QUARTIC ANOMALOUS GAUGE COUPLINGS (Y,N)]
*             IF Y, THE SCALE LAMBDA (GEV), A0W, ACW, A0Z, AND ACZ 
*             MUST BE PROVIDED ACCORDING TO THE PARAMETRIZATION 
*             OF HEP-PH/9907235. THE PARAMETERS A0W AND ACW REFER TO 
*             THE COUPLING WWGG, WHILE A0Z AND ACZ REFER TO 
*             THE COUPLING ZZGG
C---------------------------------------------------------------------  
       OGEN = 'I'
       NCALLS = 50000   !  NUMBER OF TRIES FOR INTEGRATION STEP
       NHITWMAX = 50000
       NHITMAX = 10
C defaults values     
       ECMS = 200.
       NFL = 10
       NPHO = -1
       IQED = 1
       IPT  = 1 
       ALPHM1 = 128.87D0
      JGENE=NLINK('GNNG',0)                                             
      IF (JGENE.NE.0) THEN                                              
       ECMS  = RW(JGENE+1)                                              
       NFL   = RW(JGENE+2)                                              
       NPHO  = RW(JGENE+3) 
       IQED  = RW(JGENE+4)
       IPT   = RW(JGENE+5)                                             
       ALPHM1= RW(JGENE+6)
       if (IW(JGENE).ge.7) NCALLS = RW(JGENE+7) 
      ENDIF                                                             

      NHITWMAX = NCALLS
      ALPHAI= 1.D0/ALPHM1
      rs = ecms
      S= RS*RS
      nphot = npho
      if (nfl.eq.12) nflv = 'E'
      if (nfl.eq.14) nflv = 'M'
      if (nfl.eq.16) nflv = 'T' 
      if (nfl.eq.10) nflv = 'F'

      IF(IQED.EQ.0) THEN
         PRINT*,'BORN CROSS SECTION IS COMPUTED'
      ELSE IF(IQED.EQ.1) THEN
         PRINT*,'QED CORR. CROSS SECTION IS COMPUTED'
      ELSE
         PRINT*,'IQED MUST BE 0 OR 1'
         call exit
      ENDIF
*   standard model parameters
      JGENE=NLINK('GMSP',0)                                             
      IF (JGENE.NE.0) THEN                                              
       WM  = RW(JGENE+1) 
       WG  = RW(JGENE+2)                                              
       ZM  = RW(JGENE+3)                                              
       ZW  = RW(JGENE+4) 
       HM  = RW(JGENE+5)
       HW1 = RW(JGENE+6)
       TQM = RW(JGENE+7)                                             
      ENDIF                                                             


      JGENE=NLINK('SVRT',0)                                             
      IF (JGENE.NE.0) THEN                                              
       SVRT(1)  = RW(JGENE+1)   
       SVRT(2)  = RW(JGENE+2)
       SVRT(3)  = RW(JGENE+3)
      ENDIF                                                             


      THMINL  = 40.d0
      EGMINL  = 10.d0
      EGMAXL  = 0.5*rs
      JGENE=NLINK('GGAM',0)                                             
      IF (JGENE.NE.0) THEN
       THMINL  = RW(JGENE+1)
       EGMINL  = RW(JGENE+2)
       EGMAXL  = RW(JGENE+3)
      ENDIF
      THMINL = THMINL*PI/180.D0
      THMAXL = PI - THMINL
      CMINL = COS(THMAXL)
      CMAXL = COS(THMINL)
*
      IF(EGMAXL.GT.(RS/2.D0)) EGMAXL = RS/2.D0
*
      OANOM = 'N'
      XG = 0d0
      YG = 0d0
      JGENE=NLINK('GAGC',0)                                             
      IF (JGENE.NE.0) THEN
         OANOM = 'Y'
         XG = RW(JGENE+1)
         YG = RW(JGENE+2)
      ENDIF
      OFAST= 'N'

      OQGC = 'N'
      XLAMBDA = 1.d0
      A0W = 0.D0
      ACW = 0.D0
      A0Z = 0.D0
      ACZ = 0.D0
      
      JGENE=NLINK('GQGC',0)                                             
      IF (JGENE.NE.0) THEN
         OQGC = 'Y'
         XLAMBDA = RW(JGENE+1)
         A0W = RW(JGENE+2)
         ACW = RW(JGENE+3)
         A0Z = RW(JGENE+4)
         ACZ = RW(JGENE+5)
      ENDIF
      OFAST= 'N'
*
      CALL PARAMETERS()
C      print *,'after parameters call'
*
*-----DERIVED PARAMETERS
*
      RWM2= WM2/S
      RWG2= WG2/S
      RZM2= ZM2/S
      RZW2= ZW2/S
*
      RRW = WG*WM/S
      RZW = ZW*ZM/S
      RW2 = RRW*RRW
*
      ZRM= 1.D-35
*
      CALL INITIAL0()
C      print *,'after initial0 call'
*
      NPHOT0= NPHOT
*
      INT  =INPT
      CALL RLUXGO(LUX,INT,K1,K2)
      call rdmout(irndm)
      IF((IQED.EQ.0.AND.NPHOT.GT.0).OR.
     #   (IQED.EQ.1.AND.IPT.EQ.0.AND.NPHOT.GT.0)) THEN
         IF(NPHOT.EQ.1) THEN 
            CALL PHOT1(INPT,NFLV,XSECT,DXSECT,NHITO,FMAXO,EFFO,ANBIASO)
         ELSE IF(NPHOT.EQ.2) THEN
            CALL PHOT2(INPT,NFLV,XSECT,DXSECT,NHITO,FMAXO,EFFO,ANBIASO)
         ELSE IF(NPHOT.EQ.3) THEN
            CALL PHOT3(INPT,NFLV,XSECT,DXSECT,NHITO,FMAXO,EFFO,ANBIASO)
         ELSE 
            WRITE(6,*)NPHOT
            WRITE(6,*)'OPTION INCONSISTENT WITH BORN'
         ENDIF
      ELSE IF(IQED.EQ.1.AND.IPT.EQ.1.AND.NPHOT.GT.0) THEN
         IF(NPHOT.EQ.1) THEN 
            print * ,'calling phot1pt'
            CALL PHOT1PT(INPT,NFLV,XSECT,DXSECT,
     >                   NHITO,FMAXO,EFFO,ANBIASO)
         ELSE IF(NPHOT.EQ.2) THEN
            print * ,'calling phot2pt'
            CALL PHOT2PT(INPT,NFLV,XSECT,DXSECT,
     >                  NHITO,FMAXO,EFFO,ANBIASO)
         ENDIF
      ELSE IF(NPHOT.EQ.-1) THEN
         IF(OFAST.EQ.'N') THEN
            CALL ATLEASTONE(INPT,NFLV)
         ELSE IF(OFAST.EQ.'Y') THEN
            CALL ATLEASTONEF(INPT,NFLV)
         ENDIF
      ELSE IF(NPHOT.EQ.-4) THEN
         CALL MASSIVENU(INPT,NFLV)
      ENDIF
            PRINT*, ' '
            PRINT*, 'Ecms = ',RS
            PRINT*, ' '
            PRINT*, 'NCALLS = ', NHITO
            PRINT*, ' '
            PRINT*, 'XSECT FOR WEIGHTED EVENTS'
            PRINT*, ' '
            PRINT*, 'EFF = ',EFFO
            PRINT*, 'FMAX= ',FMAXO,FMAX
            PRINT*, 'XSECT = ', XSECT, ' +- ', DXSECT, ' (PB)'
      FMAX = FMAXO
      CALL RLUXGO(LUX,INT,K1,K2)   !   restart randoms for event generation
      call rmarin(irndm(1),irndm(2),irndm(3))
      GPAR(1) = ECMS                                                   
      GPAR(2) = NFL
      GPAR(3) = NPHO
      GPAR(4) = IQED
      GPAR(5) = IPT
      GPAR(6) = ALPHM1
      GPAR(7) = WM
      GPAR(8) = WG
      GPAR(9) = ZM
      GPAR(10) = ZW
      GPAR(11) = HM
      GPAR(12) = HW1
      GPAR(13) = TQM
      GPAR(14) = SVRT(1)                                                  
      GPAR(15) = SVRT(2)                                                 
      GPAR(16) = SVRT(3)                                                 
      GPAR(17) = THMINL
      GPAR(18) = THMAXL
      GPAR(19) = EGMINL
      GPAR(20) = EGMAXL                                                  
      GPAR(21) = XG
      GPAR(22) = YG
      GPAR(23) = XLAMBDA
      GPAR(24) = a0w 
      GPAR(25) = acw
      GPAR(26) = a0z
      GPAR(27) = acz

C---------------------------------------------------------------------  
C  Fill the KPAR bank     
      NCOL = 27           
      NROW = 1            
      JKPAR = ALTABL('KPAR',NCOL,NROW,GPAR,'2I,(F)','C') 
C                         
      iebeam = nint(500*ecms)                           
      jrlep = alrlep(iebeam,blankc,izero,izero,izero)

C  Initialize counters    
      DO 10 I = 1,3       
   10  NEVENT(I) = 0      

C
C  Print PART and KLIN banks                             
C                                                        
C      CALL PRPART                                        
C                                                        
      CALL PRTABL('KPAR',0)                              
C                                                        
      call prtabl('RLEP',0)                              
      call hbook1(10001,'px balance',100,-5.,5.,0.)
      call hbook1(10002,'py balance',100,-5.,5.,0.)
      call hbook1(10003,'pz balance',100,-5.,5.,0.)
      call hbook1(10004,'E  balance',100,-5.,5.,0.)
      do i = 1,6
        sumano(i) = 0.
      enddo
      RETURN                                             
      END                                                
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)  
C--------------------------------------------------------------------   
C                                                                       

C                 B. Bloch September 2002
C                                                                       
C - call the generator routine to generate one event                   
C - fill KINE , VERT , KHIS banks                                       
C                                                                       
C     output    : 6 arguments                                           
C          IDPR   : process identification                              
C          ISTA   : status flag ( 0 means ok)                           
C          NTRK   : number of tracks generated and kept                 
C                  (i.e. # KINE banks  written)                         
C          NVRT   : number of vertices generated                        
C                   (i.e. # VERT banks written)                         
C          ECMS   : center of mass energy for the event (may be         
C                   different from nominal cms energy)                  
C          WEIT   : event weight ( not 1 if a weighting method is used) 
C--------------------------------------------------------------------   
      IMPLICIT REAL*8 (A-H,O-Z)

      INTEGER LMHLEN, LMHCOL, LMHROW                                    
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          
C                                                                       
      PARAMETER (LBCS=50000,LCHAR=4)                                    
      COMMON/BCS/ IW(LBCS)                                              
      INTEGER IW                                                        
      REAL RW(LBCS)                                                     
      EQUIVALENCE (RW(1),IW(1))                                         
C
C   commons from nunugpv
C
      CHARACTER*1 OGEN, NFLV, OANOM, OFAST
      REAL*8 MNU,MNU2

      COMMON/CMENERGY/RS,S  
      COMMON/PAR/WM,WG,CFACT,GF,G,ZM,ZW,PI,ALPHA,EM2       
      COMMON/PARD/ZM2,ZW2,WM2,WG2,GF2,G2,G4,G8
      COMMON/RPAR/RWM2,RWG2,RZM2,RZW2,RRW,RZW,RW2
      COMMON/THETAW/STH2
      COMMON/EM/RL,ETA,BETA,SDELTA,BETAM
C      COMMON/PAWC/PAW(NWPAWC)
      COMMON/OG/OGEN
      COMMON/S2CH/NSCH
      COMMON/AIN/ALPHAI
      COMMON/EMC/IQED
      COMMON/RELACC/EPS
      COMMON/QCM/QMF1,QMF2,QMF3,QMF4,QMF5,QMF6
      COMMON/CHANN/ICHANNEL
      COMMON/CUTS/EGMINL,EGMAXL,CMINL,CMAXL
      COMMON/CVETO/CMINV,CMAXV    
      COMMON/NG/NPHOT   
      COMMON/CALLS/NHITWMAX,NHITMAX,NCALLS
      COMMON/MAXVALUE/FMAX
      COMMON/ANOM/OANOM
      COMMON/ANOMV/XG,YG
      COMMON/NUMASS/MNU,MNU2
      COMMON/PT/IPT
      common/kinevt/cevent(24),iacc
      real*8 MOMEN(4,8),MOMEN_IN(4,8)
      real*8 SQMSM,SQMAN1,SQMAN2,SQMAN3,SQMAN4,SQMAN5
      CHARACTER*1 OQGC
      real*8 XLAMBDA, A0W,ACW,A0Z,ACZ
      COMMON/QGC/XLAMBDA, A0W,ACW,A0Z,ACZ,OQGC
C
      REAL*4 gpar,sumano
      COMMON / COUNTR / NEVENT(3)                                       
      common / genepar/ GPAR(50)
      REAL *8 xXSECT,xDXSECT,xFMAXO,xEFFO,xANBIASO
      common/ xsection / xXSECT,xDXSECT,xFMAXO,xEFFO,xANBIASO,NxHITO
      common/ anomx/ sumano(6)
C                                                                       
      REAL*4 SVRT(3),VERT(4),TABL(4),pmod,sumx,sumy,sumz,sume,dum
      REAL*4 rx,ry,rz,ebeam,ecms,weit,weigh(6)
      DIMENSION R2(4),T2(4)                                             
      INTEGER ALTABL,KWTKBK                                                    
      INTEGER IGG                                                       
      DATA IFIRST / 0 /                                                 

      pmod(j) = cevent(j)**2 +cevent(j+1)**2 +cevent(j+2)**2  
      inpt = 1234543
      nflv = 'F'

C
      OGEN = 'G'
      NCALLS = 1         ! number of events for generation step                       NHITMAX = 1                 
C  Generate vertex position                                             
C                                                                       
      CALL RANNOR(RX,RY)                                                
      CALL RANNOR(RZ,DUM)                                               
      IF(IFIRST.EQ.0) THEN                                              
       IFIRST = 1                                                       
       CMEIN = GPAR(1)
       SVRT(1)=GPAR(14)
       SVRT(2)=GPAR(15)
       SVRT(3)=GPAR(16)
      ENDIF                                                             
      VERT(1)=RX*SVRT(1)                                                
      VERT(2)=RY*SVRT(2)                                                
      VERT(3)=RZ*SVRT(3)                                                
      VERT(4)=0.                                                        
C                                                                       
C   Book VERT bank #1                                                   
C                                                                       
      IND=KBVERT(1,VERT,0)                                              
      IF (IND.LE.0) GO TO 20                                            
C                                                                       
C   Event generation (this generator returns either 1 or 2 photons)     
C                                                                       
 15   NEVENT(1) = NEVENT(1) + 1
      iacc = 0
C      print *,' start generating event ', nevent(1)   
C     print *,' begin gene fmaxo , fmax ',fmaxo , fmax
      fmaxo = fmax
      IF((IQED.EQ.0.AND.NPHOT.GT.0).OR.
     #   (IQED.EQ.1.AND.IPT.EQ.0.AND.NPHOT.GT.0)) THEN
         IF(NPHOT.EQ.1) THEN 
            CALL PHOT1(INPT,NFLV,XSECT,DXSECT,NHITO,FMAXO,EFFO,ANBIASO)
         ELSE IF(NPHOT.EQ.2) THEN
            CALL PHOT2(INPT,NFLV,XSECT,DXSECT,NHITO,FMAXO,EFFO,ANBIASO)
         ELSE IF(NPHOT.EQ.3) THEN
            CALL PHOT3(INPT,NFLV,XSECT,DXSECT,NHITO,FMAXO,EFFO,ANBIASO)
         ELSE 
            WRITE(6,*)NPHOT
            WRITE(6,*)'OPTION INCONSISTENT WITH BORN'
         ENDIF
      ELSE IF(IQED.EQ.1.AND.IPT.EQ.1.AND.NPHOT.GT.0) THEN
         IF(NPHOT.EQ.1) THEN 
C            print * ,'calling phot1pt'
            CALL PHOT1PT(INPT,NFLV,XSECT,DXSECT,
     >                   NHITO,FMAXO,EFFO,ANBIASO)
         ELSE IF(NPHOT.EQ.2) THEN
            print * ,'calling phot2pt'
            CALL PHOT2PT(INPT,NFLV,XSECT,DXSECT,
     >                  NHITO,FMAXO,EFFO,ANBIASO)
         ENDIF
      ELSE IF(NPHOT.EQ.-1) THEN
         IF(OFAST.EQ.'N') THEN
            CALL ATLEASTONE(INPT,NFLV)
         ELSE IF(OFAST.EQ.'Y') THEN
            CALL ATLEASTONEF(INPT,NFLV)
         ENDIF
      ELSE IF(NPHOT.EQ.-4) THEN
         CALL MASSIVENU(INPT,NFLV)
      ENDIF
      if (iacc.le.0) go to 15

      xXSECT  = XSECT
      xDXSECT = DXSECT
      nxHITO  = NHITO 
      xFMAXO  = FMAXO
      xEFFO   = EFFO
      xANBIASO = ANBIASO

C                                                                       
      igg =0
      a = sqrt(pmod(10))
      b = sqrt(pmod(13))
      c = sqrt(pmod(16))
C     print *,' energy g1,g2,g3 ',a,b,c
      if (pmod(10).le.0.) then
          igg =0
      elseif (pmod(13).le.0.) then
          igg = 1
      elseif (pmod(16).le.0.) then
          igg = 2
      else 
          igg = 3
      endif
      IDPR = IGG                                                        
      NTRK = 0                                                       
      NVRT = 1                                                          
      ISTA = 0                                                          
      ECMS = rs                                                      
      WEIT = 1.                                                         
      if (OQGC.eq.'Y') then
c
c  now look for matrix elem
c
         momen_in(1,1)= 0.5*ecms     !ebeam
         momen_in(2,1)= 0.
         momen_in(3,1)= 0.
         momen_in(4,1)= 0.5*ecms     !ebeam
         momen_in(1,2)= 0.5*ecms     !ebeam
         momen_in(2,2)= 0.
         momen_in(3,2)= 0.
         momen_in(4,2)= -0.5*ecms    !-ebeam
         momen(1,1)= sqrt(cevent(4)**2+cevent(5)**2+cevent(6)**2)
         momen(2,1)= cevent(4)
         momen(3,1)= cevent(5)
         momen(4,1)= cevent(6)
         momen(1,2)= sqrt(cevent(7)**2+cevent(8)**2+cevent(9)**2)
         momen(2,2)= cevent(7)
         momen(3,2)= cevent(8)
         momen(4,2)= cevent(9)
         momen(1,3)= sqrt(cevent(10)**2+cevent(11)**2+cevent(12)**2)
         momen(2,3)= cevent(10)
         momen(3,3)= cevent(11)
         momen(4,3)= cevent(12)
         momen(1,4)= sqrt(cevent(13)**2+cevent(14)**2+cevent(15)**2)
         momen(2,4)= cevent(13)
         momen(3,4)= cevent(14)
         momen(4,4)= cevent(15)

C         NFLV = 'F'
C         xlambda= 80.35
*     NOTE: EVEN IF THE CHARGED AND NEUTRAL CONTRIBUTIONS ARE SEPARATED,
*           IN ORDER TO ENSURE SU(2) CUSTODIAL SYMMETRY, 
*           A0W=A0Z AND ACW=ACZ SHOULD BE ENTERED

         a0w=-300.d0
         a0z=-300.d0
         acw= 0.d0
         acz= 0.d0
         CALL NUNUGPV_R(NFLV,MOMEN_IN,MOMEN,
     #                     XLAMBDA,A0W,A0Z,ACW,ACZ,SQMAN1)
         a0w= 300.d0
         a0z= 300.d0
         acw= 0.d0
         acz= 0.d0
         CALL NUNUGPV_R(NFLV,MOMEN_IN,MOMEN,
     #                     XLAMBDA,A0W,A0Z,ACW,ACZ,SQMAN2)

         a0w= 0.d0
         a0z= 0.d0
         acw=-300.d0
         acz=-300.d0
         CALL NUNUGPV_R(NFLV,MOMEN_IN,MOMEN,
     #                     XLAMBDA,A0W,A0Z,ACW,ACZ,SQMAN3)
         a0w= 0.d0
         a0z= 0.d0
         acw= 300.d0
         acz= 300.d0
         CALL NUNUGPV_R(NFLV,MOMEN_IN,MOMEN,
     #                     XLAMBDA,A0W,A0Z,ACW,ACZ,SQMAN4)

         a0w= 300.d0
         a0z= 300.d0
         acw= 300.d0
         acz= 300.d0
         CALL NUNUGPV_R(NFLV,MOMEN_IN,MOMEN,
     #                     XLAMBDA,A0W,A0Z,ACW,ACZ,SQMAN5)

C   normalisation ......last so it is   correct for next event
         a0w= 0.d0
         a0z= 0.d0
         acw= 0.d0
         acz= 0.d0
         CALL NUNUGPV_R(NFLV,MOMEN_IN,MOMEN,
     #                     XLAMBDA,A0W,A0Z,ACW,ACZ,SQMSM)
         weit1 = SQMAN1/SQMSM
         weit2 = SQMAN2/SQMSM
         weit3 = SQMAN3/SQMSM
         weit4 = SQMAN4/SQMSM
         weit5 = SQMAN5/SQMSM
C        print *,' weit1,2,3,4,5',weit1,weit2,weit3,weit4,weit5
         sumano(1) = sumano(1) + weit1
         sumano(2) = sumano(2) + weit2
         sumano(3) = sumano(3) + weit3
         sumano(4) = sumano(4) + weit4
         sumano(5) = sumano(5) + weit5
         sumano(6) = sumano(6) + 1.
         weigh(1)=1.
         weigh(2)=weit1
         weigh(3)=weit2
         weigh(4)=weit3
         weigh(5)=weit4
         weigh(6)=weit5
         ind = kwtkbk(1,weigh)
      endif
      sumx = 0.
      sumy = 0.
      sumz = 0.
      sume = 0.
C                                                                       
C   Book KINE banks for beam electrons                                  
C                                                                       
      EBEAM = rs/2.                                                  
      TABL(1) = 0.                                                      
      TABL(2) = 0.                                                      
      TABL(3) = -EBEAM                                                  
      TABL(4) = 0.                                                      
      IND=KBKINE(-1,TABL,2,0)                                           
      TABL(3) =  EBEAM                                                  
      JND=KBKINE(-2,TABL,3,0)                                           
      IF (IND*JND.EQ.0) GO TO 20                                        
C                                                                       
C   Book KINE banks for outgoing particles                              
C                                                                       
C scattered neutrino
      TABL(1) = cevent(4)
      TABL(2) = cevent(5)
      TABL(3) = cevent(6)
      TABL(4) = 0.
      sumx = sumx + tabl(1)
      sumy = sumy + tabl(2)
      sumz = sumz + tabl(3)
      sume = sume + sqrt(pmod(4))
      IND = KBKINE(1,TABL,4,1)                                          
      ntrk = ntrk +1
C      ECMS = ECMS - sqrt(pmod(4))                                             
      IF (IND.EQ.0) GO TO 20   
C scattered anti neutrino
      TABL(1) = cevent(7)
      TABL(2) = cevent(8)
      TABL(3) = cevent(9)
      TABL(4) = 0.                                                      
      sumx = sumx + tabl(1)
      sumy = sumy + tabl(2)
      sumz = sumz + tabl(3)
      sume = sume + sqrt(pmod(7))
      IND = KBKINE(2,TABL,4,1)
      ntrk = ntrk +1                                          
C      ECMS = ECMS - sqrt(pmod(7))                                             
      IF (IND.EQ.0) GO TO 20
      nbk = 2
C preemission photons
      if(cevent(19).gt.1.D-06) then
        cost = real(cevent(20))
        sint = sqrt(1.-cost*cost)
        phit = cevent(21)        
        TABL(1) = cevent(19)*sint*cos(phit)
        TABL(2) = cevent(19)*sint*sin(phit)
        TABL(3) = cevent(19)*cost
        TABL(4) = 0.
        sumx = sumx + tabl(1)
        sumy = sumy + tabl(2)
        sumz = sumz + tabl(3)
        sume = sume + cevent(19)
        if (nevent(2).le.5) print *,' E,cost,sint,phi',cevent(19),
     &           cost,sint,phit,(tabl(i),i=1,3)
        nbk = nbk+1                                              
        IND = KBKINE(nbk,TABL,1,1)
        ntrk = ntrk +1
        IF (IND.EQ.0) GO TO 20 
      endif
      if(cevent(22).gt.1.D-06) then
        cost = real(cevent(23))
        sint = sqrt(1.-cost*cost)
        phit = cevent(24)
        TABL(1) = cevent(22)*sint*cos(phit)
        TABL(2) = cevent(22)*sint*sin(phit)
        TABL(3) = cevent(22)*cost
        TABL(4) = 0. 
        sumx = sumx + tabl(1)
        sumy = sumy + tabl(2)
        sumz = sumz + tabl(3)
        sume = sume + cevent(22)

        if (nevent(2).le.5) print *,' E,cost,sint,phi',cevent(20),
     &           cost,sint,phit,(tabl(i),i=1,3)
        nbk = nbk+1 
        IND = KBKINE(nbk,TABL,1,1)
        ntrk = ntrk +1
        IF (IND.EQ.0) GO TO 20
      endif                                    
C  detected photons
      if (IGG.ge.1) then
         nbk = nbk+1 
         TABL(1) = cevent(10)
         TABL(2) = cevent(11)
         TABL(3) = cevent(12)
         TABL(4) = 0.        
         sumx = sumx + tabl(1)
         sumy = sumy + tabl(2)
         sumz = sumz + tabl(3)
         sume = sume + sqrt(pmod(10))
         ntrk = ntrk +1                                             
         IND = KBKINE(nbk,TABL,1,1)
         IF (IND.EQ.0) GO TO 20
      endif      
      IF (IGG.ge.2) THEN 
         nbk = nbk+1                                       
         TABL(1) = cevent(13)
         TABL(2) = cevent(14)
         TABL(3) = cevent(15)
         TABL(4) = 0.   
         sumx = sumx + tabl(1)
         sumy = sumy + tabl(2)
         sumz = sumz + tabl(3)
         sume = sume + sqrt(pmod(13))
         ntrk = ntrk +1
         JND = KBKINE(nbk,TABL,1,1)                                         
         IF (JND.EQ.0) GO TO 20 
      endif
      IF (IGG.ge.3) THEN 
         nbk = nbk+1                                       
         TABL(1) = cevent(16)
         TABL(2) = cevent(17)
         TABL(3) = cevent(18)
         TABL(4) = 0.                                                    
         sumx = sumx + tabl(1)
         sumy = sumy + tabl(2)
         sumz = sumz + tabl(3)
         sume = sume + sqrt(pmod(16))
         ntrk = ntrk +1
         JND = KBKINE(nbk,TABL,1,1)   
         IF (JND.EQ.0) GO TO 20    
      ENDIF       
      call hfill(10001,sumx,dum,1.)
      call hfill(10002,sumy,dum,1.)
      call hfill(10003,sumz,dum,1.)
      call hfill(10004,2.*ebeam-sume,dum,1.)
C                                                                       
C    Fill the history bank KHIS                                         
C                                                                       
      DO 10 I=1,Nbk
   10 TABL(I)=0.                                                        
      IND=ALTABL('KHIS',1,Nbk ,TABL,'I','E')                            
      IF (IND.LE.0) GO TO 20                                            
      NEVENT(2) = NEVENT(2) + 1                                         
      if (NEVENT(2).le.5) print *, cevent
      if ( ntrk.ne.nbk) print *, ' pb with banks/tracks',nbk,ntrk
      RETURN                                                            
   20 ISTA=1                                                            
      NEVENT(3) = NEVENT(3) + 1                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE USCJOB                                                
C-------------------------------------------------------------------   
C                                                                      
C                P. Gay and G. Bonneaud May, 1988.                     
C                                                                      
C                       End of job routine                             
C                                                                      
C------------------------------------------------------------------    
      COMMON / COUNTR / NEVENT(3)                                      
C                                                                       
      REAL *8 xXSECT,xDXSECT,xFMAXO,xEFFO,xANBIASO
      common/ xsection / xXSECT,xDXSECT,xFMAXO,xEFFO,xANBIASO,NxHITO
      REAL *8 rs,s
      COMMON/CMENERGY/RS,S
C                                                                       
C  Final printout                                                       
C                                                                       
      call ugtsec
      PRINT*, ' '
      PRINT*, 'cm energy = ',RS
      PRINT*, ' '
      PRINT*, 'NHIT = ',NxHITO
      PRINT*, ' '
      PRINT*, 'XSECT FOR UNWEIGHTED EVENTS'
      PRINT*, ' '
      PRINT*, 'EFF = ', xEFFO
      PRINT*, 'XSECT = ', xXSECT, ' +- ', xDXSECT, ' (PB)'
      PRINT*, 'NBIAS/NMAX = ', xANBIASO
      PRINT*, 'FMAX= ',xFMAXO
C                                                                       
      lwrite = 6
      WRITE(LWRITE,101)                                          
  101  FORMAT(//20X,'EVENTS AND ERRORS STATISTICS',               
     &         /20X,'****************************')               
      WRITE(LWRITE,102)NEVENT(1),NEVENT(2),NEVENT(3)             
  102  FORMAT(/5X,'# OF GENERATED EVENTS                       = ',I10, 
     &        /5X,'# OF ACCEPTED  EVENTS                       = ',I10, 
     &        /5X,'# OF REJECTED  EVENTS (ISTA # 0 IN ASKUSE)  = ',I10) 
      RETURN                                                            
      END                                                               

      SUBROUTINE UGTSEC
C
C-----------------------------------------
C
C   Author   :- B.Bloch              Nov 2000
C
C=========================================
C...Commonblocks.
      REAL *8 rs,s
      COMMON/CMENERGY/RS,S 
      CHARACTER*1 OGEN
      COMMON/OG/OGEN
      REAL *8 xXSECT,xDXSECT,xFMAXO,xEFFO,xANBIASO
      common/ xsection / xXSECT,xDXSECT,xFMAXO,xEFFO,xANBIASO,NxHITO
      CHARACTER*1 OQGC
      real*8 XLAMBDA, A0W,ACW,A0Z,ACZ
      COMMON/QGC/XLAMBDA, A0W,ACW,A0Z,ACZ,OQGC
      real*4 sumano
      common/ anomx/ sumano(6)
      data big/ 1000000./   !   mb to nb ......
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
      IDC = 1009

      IF(NPHOT.GT.0) THEN
         IF(OGEN.EQ.'G') THEN
            PRINT*, ' '
            PRINT*, 'RS = ',RS
            PRINT*, ' '
            PRINT*, 'NHIT = ',NxHITO
            PRINT*, ' '
            PRINT*, 'XSECT FOR UNWEIGHTED EVENTS'
            PRINT*, ' '
            PRINT*, 'EFF = ', xEFFO
            PRINT*, 'XSECT = ', xXSECT, ' +- ', xDXSECT, ' (PB)'
            PRINT*, 'NBIAS/NMAX = ', xANBIASO
            PRINT*, 'FMAX= ',xFMAXO
            PRINT*, 'XSECT FOR  a0 = -300 ,  ac =0 ',
     &                xXSECT*sumano(1)/sumano(6)
            PRINT*, 'XSECT FOR  a0 = +300 ,  ac =0 ',
     &                xXSECT*sumano(2)/sumano(6)
            PRINT*, 'XSECT FOR  a0 =    0 ,  ac =-300 ',
     &                xXSECT*sumano(3)/sumano(6)
            PRINT*, 'XSECT FOR  a0 =    0 ,  ac =+300 ',
     &                xXSECT*sumano(4)/sumano(6)
            PRINT*, 'XSECT FOR  a0 = +300 ,  ac =+300 ',
     &                xXSECT*sumano(5)/sumano(6)

         ELSE IF(OGEN.EQ.'I') THEN
            PRINT*, ' '
            PRINT*, 'RS = ',RS
            PRINT*, ' '
            PRINT*, 'NCALLS = ', NxHITO
            PRINT*, ' '
            PRINT*, 'XSECT FOR WEIGHTED EVENTS'
            PRINT*, ' '
            PRINT*, 'EFF = ',xEFFO
            PRINT*, 'FMAX= ',xFMAXO
            PRINT*, 'XSECT = ', xXSECT, ' +- ', xDXSECT, ' (PB)'
         ENDIF                 
      ENDIF
      IS = 1
      IVER = 1
      XTOT = xXSECT
      RTOT = xDXSECT
      NTOT = NxHITO
      XACC = XTOT
      RACC = RTOT
      NACC = NTOT
C
      ISEC =  KSECBK(IS,IDC,IVER,NTOT,NACC,XTOT,RTOT,XACC,RACC)
C
      if (OQGC.eq.'Y') then

         do i = 1,5
            is = is+1
            iver = is
            xtot = xXSECT*sumano(i)/sumano(6)
            xacc = xtot
            ISEC =  KSECBK(IS,IDC,IVER,NTOT,NACC,XTOT,RTOT,XACC,RACC)
         enddo
      endif
      CALL PRTABL('KSEC',0)
  999 RETURN
      END

      integer function KWTKBK(IS,wei)
C--------------------------------------------------------------------
C!  BOOK and fill bank KWTK with weight info
C      B. Bloch -Devaux February 2002
C     structure : integer function
C
C     input     : IS   row number to be filled
C                 IWEI weight number to be stored
C                 WEI  weight value  to be stored
C     output    : index of KWTK bank ( should be >0 if OK)
C                 KWTK bank is written to Event list
C
C--------------------------------------------------------------------

      SAVE

      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      INTEGER JKWTW1,JKWTW2,JKWTW3,JKWTW4,JKWTW5,JKWTW6,LKWTKA
      PARAMETER(JKWTW1=1,JKWTW2=2,JKWTW3=3,JKWTW4=4,JKWTW5=5,JKWTW6=6,
     +          LKWTKA=6)
      real wei
      dimension wei(6)
      INTEGER NAKWTK / 0 /
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
C--------------------------------------------------------------
C
      KWTKBK = -1
      IF( NAKWTK.EQ.0 ) NAKWTK = NAMIND('KWTK')
C
C   Get KWTK index
      JKWTK = IW(NAKWTK)
      IF ( JKWTK.LE.0) THEN
C   Create KWTK bank
         CALL AUBOS('KWTK',0,LKWTKA+LMHLEN,JKWTK,IGARB)
         IF ( JKWTK.LE.0) GO TO 999
         IW(JKWTK+LMHCOL) = LKWTKA
         IW(JKWTK+LMHROW) = 1
         CALL BKFMT('KWTK','2I,(6F)')
         CALL BLIST(IW,'E+','KWTK')
      ELSE
C  KWTK EXISTS, TEST THE LENGTH AND EXTEND IF NEEDED
         NKWTK=LROWS(JKWTK)
         IF ( IS.GT.NKWTK) THEN
           CALL AUBOS('KWTK',0,LKWTKA*IS+LMHLEN,JKWTK,IGARB)
           IF ( JKWTK.LE.0) THEN
              KWTKBK= -IS
              GO TO 999
           ELSE
              IW(JKWTK+LMHROW) = IS
           ENDIF
         ENDIF
      ENDIF
C  Fill KWTK BANK
      KKWTK = KROW(JKWTK,IS)
      call ucopy ( wei, RW(KKWTK+1),6)
      KWTKBK = JKWTK
C
 999  RETURN
      END
