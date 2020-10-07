      SUBROUTINE ASKUSI(IGCOD)
C--------------------------------------------------------------------
C                                  S. Jezequel July 1997
C Initialization                   A.Valassi May 1996                   AV
C Adapted from KRLW01 interface:   P.Perez   August 1995                AV
C                                  B.Bloch   November 1995              AV
C                                  B. Bloch March 98 for pythis 6.1
C--------------------------------------------------------------------
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
      COMMON /PYDAT3/MDCY(L2PAR,3),MDME(LJNPAR,2),BRAT(LJNPAR),
     &               KFDP(LJNPAR,5)
      COMMON/PYDAT4/CHAF(L2PAR,2)
      CHARACTER CHAF*16
C
      COMMON/PYSUBS/MSEL,MSELPD,MSUB(500),KFIN(2,-40:40),CKIN(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
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
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               
C                                                                       
      COMMON /BCS/   IW(LBCS )                                          
      INTEGER IW                                                        
      REAL RW(LBCS)                                                     
      EQUIVALENCE (RW(1),IW(1))                                         

      REAL*4 VRTEX,SDVRT,XVRT,SXVRT,ECMI
      COMMON / KGCOMM /VRTEX(4),SDVRT(3),XVRT(3),SXVRT(3),ECMI,
     $         NEVENT(10),IFVRT,ISTA

C      COMMON / DECAYS / IFLAV(4),AMDEC(4),BRRAT(2),BREL
C      COMMON / WGTALL / WTCRUD,WTMOD,WTSET(100)
C      REAL *8 WTCRUD,WTMOD,WTSET,AMDEC,BRRAT,BREL
C
      COMMON / INOUT / INUT,IOUT                                     
      REAL*4            GAMPRT
      COMMON / TAUBRA / GAMPRT(30),JLIST(30),NCHAN                      TAUOLA
      COMMON / TAUKLE / BRA1,BRK0,BRK0B,BRKS                            TAUOLA
      REAL*4            BRA1,BRK0,BRK0B,BRKS                            TAUOLA
C      real *4          BLIBK
C      COMMON / CGLIB / BLIBK(20000)                                     AV
C      SAVE   / CGLIB /                                                  AV
C      REAL*4          cmin,cmax,spt2,scut,scutee,spt2ee
C      COMMON /KWCTE0/ cmin,cmax,spt2,scut,scutee,spt2ee              !cav
      CHARACTER*80 BXOPE,BXTXT,BXL1F,BXCLO                              
C      DOUBLE PRECISION PI                                               AV
C      PARAMETER (PI=3.1415926535897932D0)                               AV
      REAL*4 TABL(96)                                                   AV
      REAL*8 XPAR(10000)
C      DIMENSION YPAR(100)                                               AV
C      DOUBLE PRECISION XPAR_INPUT(10000)
C      CHARACTER*3 CH_I
* For herwig
C      COMMON/IHRWG/ifhrwg
C
C Generator code (see KINLIB DOC)
C
      PARAMETER ( IGCO = 5038 )      
      PARAMETER ( IVER = 142  )      ! AV+SJ+BBL
C
      PARAMETER (LPDEC = 48)
      INTEGER NODEC(LPDEC)
      INTEGER ALTABL,ALRLEP
      EXTERNAL ALTABL ,ALRLEP

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
C   Return generator code                                               
C                                                                       
      IGCOD= IGCO                                                       
      INUT = IW(5)                                                      
      IOUT = IW(6)                                                      

      WRITE(IOUT,101) IGCOD ,IVER                                       
  101 FORMAT(/,10X,'KRLW03 - CODE NUMBER =',I4,                         
     &       /,10X,'**************************',                        
     &       /,10X,' SUBVERSION  :',I10 ,                               
     &       /,10X,' Last mod = 27 March 2000   ')
C                                                                       
C Input parameters for the generator (see subroutine KW for comment)
C                                                                       
C      CALL GLIMIT(20000)                                                
C      CALL GOUTPU(IOUT)                                                 

      CALL KRLWINI(XPAR)         ! defaults are initiliazed

      ifhrwg = 0   ! AST add herwig switch
      NOUT   = IOUT                                                     
      XPAR(1057) = NOUT
C                                                                       
C                                                                       
      SPT2   =600.
      SPT2EE = 300.
      SP = 300.
      XMAXPTPHO = 50.
      
C                                                                       
C  Default values can be changed by the DATA CARDS:                     
C  GKEY, GENE, GKAC, GKHG, GCE0                                         
C                                                                       
      NAGKEY = NAMIND('GKEY')                                           
      JGENE = IW(NAGKEY)                                                
      IF(JGENE.NE.0) THEN                                               
        KEYISR = IW(JGENE+1)                                            
        KEYFSR = IW(JGENE+2)                                            
        KEYNLL = IW(JGENE+3)                                            
        KEYCUL = IW(JGENE+4)                                            
        KEYBRA = IW(JGENE+5)                                            
        KEYMAS = IW(JGENE+6)
        KEYZET = IW(JGENE+6)
        KEYSPN = IW(JGENE+7)                                            
        KEYRED = IW(JGENE+8)                                            
        KEYWU  = IW(JGENE+9)                                            
        KEYSMP = IW(JGENE+10)                                            
        KEYMIX = IW(JGENE+11)                                            
        KEY4f  = IW(JGENE+12)
        KEYACC = IW(JGENE+13)
        KEYZON = IW(JGENE+14)
        KEYWON = IW(JGENE+15)
        KEYDWM = IW(JGENE+16)
        KEYDWP = IW(JGENE+17)
C      
        XPAR(1011) = KEYISR
        XPAR(1012) = KEYFSR
        XPAR(1013) = KEYNLL
        XPAR(1014) = KEYCUL
        XPAR(1021) = KEYBRA
        XPAR(1022) = KEYMAS
        XPAR(1023) = KEYZET
        XPAR(1024) = KEYSPN
        XPAR(1025) = KEYRED
        XPAR(1026) = KEYWU
        XPAR(1033) = KEYSMP
        XPAR(1041) = KEYMIX
        XPAR(1042) = KEY4f
        XPAR(1043) = KEYACC
        XPAR(1044) = KEYZON
        XPAR(1045) = KEYWon
        XPAR(1055) = KEYDWM
        XPAR(1056) = KEYDWP
      ENDIF
      NAGENE = NAMIND('GENE')                                           
      JGENE = IW(NAGENE)                                                
      IF(JGENE.NE.0) THEN                                             
C                                                                       
        CMSENE      = RW(JGENE+1)                                       
        AMAZ        = RW(JGENE+2)                                      
        GAMMZ       = RW(JGENE+3)                                      
        AMAW        = RW(JGENE+4)                                      
        GAMMW       = RW(JGENE+5)                                      
        AMH         = RW(JGENE+6)
        AGH         = RW(JGENE+7)
        VVMIN       = RW(JGENE+8)                                      
        VVMAX       = RW(JGENE+9)                                    
        WTMAX       = RW(JGENE+10)                                   
        WTMAX_CC03  = RW(JGENE+17)                                   
C
        XPAR(1) = CMSENE
        XPAR(4) = AMAZ 
        XPAR(5) = GAMMZ
        XPAR(6) = AMAW 
        XPAR(7) = GAMMW
        XPAR(11) = AMH 
        XPAR(12) = AGH 
        XPAR(8) = VVMIN
        XPAR(9) = VVMAX
        XPAR(10) = WTMAX
        XPAR(18) = WTMAX_cc03
      ENDIF   
C                                                                       
      NAGTAU = NAMIND('GDEK')                                           
      JGTAU = IW(NAGTAU)                                                
      IF(JGTAU.NE.0) THEN                                               
        JAK1   = IW(JGTAU+1)                                            
        JAK2   = IW(JGTAU+2)                                            
        ITDKRC = IW(JGTAU+3)                                            
        IFPHOT = IW(JGTAU+4)                                            
        IFHADM = IW(JGTAU+5)                                            
        IFHADP = IW(JGTAU+6)                                            
C
        XPAR(1071) = jak1 
        XPAR(1072) = jak2
        XPAR(1073) = itdkrc 
        XPAR(1074) = ifphot
        XPAR(1075) = ifhadm
        XPAR(1076) = ifhadp
      ENDIF
C - input of any input parameter in case it's not foreseen in a card
      NAMI=NAMIND('XPAR')
      IF (IW(NAMI).EQ.0) GOTO 50
      KIND=NAMI+1
 15   KIND=IW(KIND-1)
      IF (KIND.EQ.0) GOTO 49
      LUPAR = LUPAR+1
      J = IW(KIND-2)
      xpar(j) = dble(rw(kind+1))
      go to 15
 49   CONTINUE
      CALL BKFMT ('XPAR','F')
      CALL BLIST (IW,'C+','XPAR')
 50   CONTINUE

C                                                                       
C      KHRWG = NAMIND('GHRG')                                            
C      JHRWG = IW(KHRWG)                                                 
C      IF(JHRWG.NE.0) THEN
C       ifhrwg = iw(JHRWG+1)
C      ENDIF
C                                                                       
C      NAGCE0 = NAMIND('GCE0')                                           
C      JGCE0 = IW(NAGCE0)                                                
C      IF(JGCE0.NE.0) THEN                                               
C        THEMIN    = RW(JGCE0+1)     
C        SPT2      = RW(JGCE0+2)     
C        SPT2EE    = RW(JGCE0+3)     
C        XMAXPTPHO = RW(JGCE0+4)     
C      ENDIF                                                            
C                                                                       
C  Consistency checks between switches                                  
C                                                                       
      KeyWgt = xpar(1031)
      KeyMas =  xpar(1022)
      IF ((KeyWgt.NE.0).or.(KeyMas.NE.1)) THEN  
        xpar(1071) = -1    ! JAK1   = -1
        xpar(1072) = -1    ! JAK2   = -1             
        xpar(1073) =  0    ! ITDKRC = 0     
        xpar(1074) =  0    ! IFPHOT = 0  
        xpar(1075) =  0    ! IFHADM = 0             
        xpar(1076) =  0    ! IFHADP = 0        
        WRITE (IOUT,*)                             
        if (KeyWgt.NE.0) WRITE (IOUT,*)             
     +  '+++ASKUSI+++ Weighted events required -> no fragmentation'     
        WRITE (IOUT,*)                                                  
        if (KeyMas.NE.1) WRITE (IOUT,*)      
     +  '+++ASKUSI+++ Massless kinematics required -> no fragmentation'
        WRITE (IOUT,*)                                                  
     +  '             (GDEK card values superseeded)'                   
        WRITE (IOUT,*)                                                  
      ENDIF                                                             
C                                                                       AV
C  Warn the user that the W width may be not compatible to other inputs AV
C                                                                       AV
      KeyBra =  XPAR(1021)
      IF (GAMMW.GT.0E0) THEN                                            AV
        IF (KeyBra.EQ.1) THEN                                           AV
          brelectron = 0.1084                                           AV
          alphas = 3E0/2E0*PI * (1/9E0/brelectron-1E0)                  AV
          gammw_ok = 9E0 * GFERMI * amaw**3 /( 6E0 * sqrt(2E0) * PI)    AV
     +             * (1E0 + 2E0/3E0 * alphas/pi)                        AV
        ELSE                                                            AV
          gammw_ok = 9E0 * GFERMI * amaw**3 /( 6E0 * sqrt(2E0) * PI)    AV
        ENDIF                                                           AV
        WRITE (IOUT,*)                                                  AV
        WRITE (IOUT,                                                    AV
     +    '(A45,f10.7)')                                                AV
     +    '+++ASKUSI+++ W width (user input) will be   ', gammw         AV
        WRITE (IOUT,                                                    AV
     +    '(A45,f10.7)')                                                AV
     +    '             Self-consistent value would be ', gammw_ok      AV
        WRITE (IOUT,*)                                                  AV
      ENDIF                                                             AV
C                                                                       AV
C  Hardwired correction for alpha strong in W width (AV)                AV
C   Value of alphas derived from hardwired (in routine filexp) electron AV
C   branching ratio, used in all cross-section normalizations.          AV
C                                                                       AV

      IF (KeyBra.EQ.1 .AND. GAMMW.LE.0E0) THEN                          AV
        brelectron = 0.1084                                             AV
        alphas = 3E0/2E0*PI * (1/9E0/brelectron-1E0)                    AV
        gammw = 9E0 * GFERMI * amaw**3 /( 6E0 * sqrt(2E0) * PI)         AV
     +        * (1E0 + 2E0/3E0 * alphas/pi)                             AV
        WRITE (IOUT,*)                                                  AV
        WRITE (IOUT,*)                                                  AV
     +  '+++ASKUSI+++ Branching ratios with QCD required'               AV
        WRITE (IOUT,102) alphas                                         AV
 102    FORMAT                                                          AV
     +('              -> correct GammaW for QCD (alphas=',f8.6,')')     AV
        WRITE (IOUT,*)                                                  AV
      ENDIF                                                             AV
C                                                                       AV
C Warn user if fixed width is used in the calculation for either W or Z.AV
C         
      KeyZet = XPAR(1023) 
      IF (KeyZet.EQ.1) THEN                                             AV
        amaz_run  = amaz  * (1. + gammz**2/amaz**2/2.)                  AV
        gammz_run = gammz * (1. + gammz**2/amaz**2/2.)                  AV
        WRITE (IOUT,*)                                                  AV
        WRITE (IOUT,*)                                                  AV
     +  '+++ASKUSI+++ Fixed Z width required'                           AV
        WRITE (IOUT,*)                                                  AV
     +  ' !!!!!! Be aware of resulting MASS SHIFT !!!!!!'               AV
        WRITE (IOUT,*)                                                  AV
     +  ' Ref. Phys.Lett.B206(1988)539; CERN96-01,vol.1,page153'        AV
        WRITE (IOUT,111) gammz                                          AV
 111    FORMAT                                                          AV
     +('  Z width (fixed width) is ',f8.5,' GeV')                       AV
        WRITE (IOUT,112) gammz_run                                      AV
 112    FORMAT                                                          AV
     +('  ===> corresponds to Z width (running width) of ',f8.5,' GeV') AV
        WRITE (IOUT,113) amaz                                           AV
 113    FORMAT                                                          AV
     +('  Z mass  (fixed width) is ',f8.5,' GeV')                       AV
        WRITE (IOUT,114) amaz_run                                       AV
 114    FORMAT                                                          AV
     +('  ===> corresponds to Z mass  (running width) of ',f8.5,' GeV') AV
        WRITE (IOUT,*)                                                  AV
      ENDIF                                                             AV
C          
      KeyWu = XPAR(1026) 
      IF (KeyWu.EQ.1) THEN                                              AV
        amaw_run  = amaw  * (1. + gammw**2/amaw**2/2.)                  AV
        gammw_run = gammw * (1. + gammw**2/amaw**2/2.)                  AV
        WRITE (IOUT,*)                                                  AV
        WRITE (IOUT,*)                                                  AV
     +  '+++ASKUSI+++ Fixed W width required'                           AV
        WRITE (IOUT,*)                                                  AV
     +  ' !!!!!! Be aware of resulting MASS SHIFT !!!!!!'               AV
        WRITE (IOUT,*)                                                  AV
     +  ' Ref. Phys.Lett.B206(1988)539; CERN96-01,vol.1,page153'        AV
        WRITE (IOUT,211) gammw                                          AV
 211    FORMAT                                                          AV
     +('  W width (fixed width) is ',f8.5,' GeV')                       AV
        WRITE (IOUT,212) gammw_run                                      AV
 212    FORMAT                                                          AV
     +('  ===> corresponds to W width (running width) of ',f8.5,' GeV') AV
        WRITE (IOUT,213) amaw                                           AV
 213    FORMAT                                                          AV
     +('  W mass  (fixed width) is ',f8.5,' GeV')                       AV
        WRITE (IOUT,214) amaw_run                                       AV
 214    FORMAT                                                          AV
     +('  ===> corresponds to W mass  (running width) of ',f8.5,' GeV') AV
        WRITE (IOUT,*)                                                  AV
      ENDIF                                                             AV
C                                                                       AV
C  Print out cuts used in generation                                    AV
C                                                                       AV
      BXOPE =  '(//1X,15(5H*****)    )'                                 AV
      BXTXT =  '(1X,1H*,                  A48,25X,    1H*)'             AV
      BXL1F =  '(1X,1H*,F17.8,               12X, A30,7X,A6, 1X,1H*)'   AV
      BXCLO =  '(1X,15(5H*****)/   )'                                   AV
      WRITE(IOUT,BXOPE)                                                 AV
      WRITE(IOUT,BXTXT) '  +++ASKUSI+++ Following cuts will be   '      AV
      WRITE(IOUT,BXTXT) '    applied on final state fermions     '      AV
      WRITE(IOUT,BXTXT) '                                        '      AV
      WRITE(IOUT,BXTXT) '         Regardless of flavour:         '      AV
      WRITE(IOUT,BXL1F)  cmin ,' Min cos of outgoing fermion ',' cmin'  AV
      WRITE(IOUT,BXL1F)  cmax ,' Max cos of outgoing fermion ',' cmax'  AV
      WRITE(IOUT,BXTXT) '         For e+e- ch+ch- events:        '      AV
      WRITE(IOUT,BXL1F) spt2ee,' Min sum of Pt^2 after ISR   ','spt2ee' AV
      WRITE(IOUT,BXL1F)  spt2 ,' Min sum of Pt^2 ( no nu_e)  ',' spt2'  AV
      WRITE(IOUT,BXL1F)  scut ,' Min sum mass(1-2)+mass(3-4) ',' scut'  AV
      WRITE(IOUT,BXTXT) '         For e+e- e+e- events:          '      AV
      WRITE(IOUT,BXL1F) scutee,' Min sum mass(1-4)+mass(2-3) ','scutee' AV
      WRITE(IOUT,BXOPE)                                                 AV
C                                                                       AV
C  All the parameters are stored in TABL(I)                             ASKUS
C                         
      do ii = 1,80
        TABL(ii)  = xpar(ii)                    ! used are 1;73
        TABL(ii+80)  = xpar(1010+ii)            ! used are 1011;1076
      enddo
      ncol = 160
C                                                                       
C  Main vertex initialization                                           
C                                                                       
      SDVRT(1) = 0.0113                                                 
      SDVRT(2) = 0.0005                                                 
      SDVRT(3) = 0.790                                                 
      NASVRT = NAMIND('SVRT')                                          
      JSVRT = IW(NASVRT)                                                
      IF(JSVRT.NE.0) THEN                                               
        SDVRT(1) = RW(JSVRT+1)                                          
        SDVRT(2) = RW(JSVRT+2)                                          
        SDVRT(3) = RW(JSVRT+3)                                          
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
      do ii =1,3  
        TABL(73+ii) = SDVRT(ii)                       
        TABL(76+ii) =  XVRT(ii)                       
        TABL(156+ii) = SXVRT(ii)                       
      enddo
C                                                       
      ECMI = CMSENE                                                     ASKUS1
C  Fill the KPAR bank with the generator parameters                     ASKUS1
C                                                                       ASKUS1
      NROW = 1                                                          ASKUS1
      JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'2I,(F)','C')                ASKUS1
C                                                                       ASKUS1
C  Fill RLEP bank                                                       ASKUS1
      IEBEAM = NINT(CMSENE * 500.  )                                    ASKUS1
      JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)                               ASKUS1
C                                                                       ASKUS1
C Initialization event counters                                         ASKUS1
C                                                                       ASKUS1
      DO 20 I = 1,10                                                   
        NEVENT(I) = 0                                                   ASKUS1
   20 CONTINUE                                                          ASKUS1
C                                                                       ASKUS1
C Initialization particle data  mod BB march 98
C                                                                       ASKUS1
      CALL KXP6IN (IPART,IKLIN)                                         ASKUS1
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN                              ASKUS1
        WRITE (IOUT,                                                    ASKUS1
     &    '(1X,''ASKUSI :error in PART or KLIN bank - STOP - ''         ASKUS1
     &                 ,2I3)') IPART,IKLIN                              ASKUS1
        STOP                                                            ASKUS1
      ENDIF                                                             ASKUS1
C                                                                       ASKUS1
C  modify Lund masses according to input masses                         ASKUS1
      PMAS(PYCOMP(23),1)= AMAZ                                          ASKUS1
      PMAS(PYCOMP(24),1)= AMAW                                          ASKUS1
      PMAS(PYCOMP(25),1)= AMAH
      PMAS(PYCOMP( 7),1)= 170.                                          ASKUS1
      PMAS(PYCOMP( 8),1)= 300.                                          ASKUS1
CBB      IA1=20213                              !jetset7.3 code for a1  ASKUS1
CBB      PMAS(PYCOMP(IA1),1)= 1.251                                     ASKUS1
CBB      PMAS(PYCOMP(IA1),2)= 0.599                                     ASKUS1
C                                                                       ASKUS1
C   Make sure that masses and width in PART bank are consistent         ASKUS1
C function KGPART returns the ALEPH code corresponding to the LUND code ASKUS1
C required.                                                             ASKUS1
C Z0(lund code=23) top (lund code=6)  Higgs (lund code=25)              ASKUS1
C a1(lund code=20213)                                                   ASKUS1
      NAPAR = NAMIND('PART')                                            ASKUS1
      JPART = IW(NAPAR)                                                 ASKUS1
      IZPART = KGPART(23)                                               ASKUS1
      IF (IZPART.GT.0)  THEN                                            ASKUS1
        ZMAS = PMAS(PYCOMP(23),1)                                       ASKUS1
        KPART = KROW(JPART,IZPART)                                      ASKUS1
        RW(KPART+6)=ZMAS                                                ASKUS1
        IANTI = ITABL(JPART,IZPART,10)                                  ASKUS1
        IF (IANTI.NE.IZPART) THEN                                       ASKUS1
          KAPAR = KROW(JPART,IANTI)                                     ASKUS1
          RW(KAPAR+6)=ZMAS                                              ASKUS2
        ENDIF                                                           ASKUS2
      ENDIF                                                             ASKUS2
      ITPART = KGPART(6)                                                ASKUS2
      IF (ITPART.GT.0)  THEN                                            ASKUS2
        ZMAS = PMAS(PYCOMP( 6),1)                                       ASKUS2
        KPART = KROW(JPART,ITPART)                                      ASKUS2
        RW(KPART+6)=ZMAS                                                ASKUS2
        IANTI = ITABL(JPART,ITPART,10)                                  ASKUS2
        IF (IANTI.NE.ITPART) THEN                                       ASKUS2
          KAPAR = KROW(JPART,IANTI)                                     ASKUS2
          RW(KAPAR+6)=ZMAS                                              ASKUS2
        ENDIF                                                           ASKUS2
      ENDIF                                                             ASKUS2
      IHPART = KGPART(25)                                               ASKUS2
      IF (IHPART.GT.0)  THEN                                            ASKUS2
        ZMAS = PMAS(PYCOMP(25),1)                                       ASKUS2
        KPART = KROW(JPART,IHPART)                                      ASKUS2
        RW(KPART+6)=ZMAS                                                ASKUS2
        IANTI = ITABL(JPART,IHPART,10)                                  ASKUS2
        IF (IANTI.NE.IHPART) THEN                                       ASKUS2
          KAPAR = KROW(JPART,IANTI)                                     ASKUS2
          RW(KAPAR+6)=ZMAS                                              ASKUS2
        ENDIF                                                           ASKUS2
      ENDIF                                                             ASKUS2
                                                                        ASKUS2
      IHPART = KGPART(20213)                                            ASKUS2
      IF (IHPART.GT.0)  THEN                                            ASKUS2
        ZMAS = PMAS(PYCOMP(20213),1)                                    ASKUS2
        ZWID = PMAS(PYCOMP(20213),2)                                    ASKUS2
        KPART = KROW(JPART,IHPART)                                      ASKUS2
        RW(KPART+6)=ZMAS                                                ASKUS2
        RW(KPART+9)=ZWID                                                ASKUS2
        IANTI = ITABL(JPART,IHPART,10)                                  ASKUS2
        IF (IANTI.NE.IHPART) THEN                                       ASKUS2
          KAPAR = KROW(JPART,IANTI)                                     ASKUS2
          RW(KAPAR+6)=ZMAS                                              ASKUS2
          RW(KAPAR+9)=ZWID                                              ASKUS2
        ENDIF                                                           ASKUS2
      ENDIF                                                             ASKUS2
C                                                                       ASKUS2
C                                                                       ASKUS2
C   Inhibit decays                                                      ASKUS2
C                                                                       ASKUS2
      MXDEC=KNODEC(NODEC,LPDEC)                                         ASKUS2
      MXDEC=MIN(MXDEC,LPDEC)                                            ASKUS2
      IF (MXDEC.GT.0) THEN                                              ASKUS2
        DO 150 I=1,MXDEC                                                 
          IF (NODEC(I).GT.0) THEN                                       ASKUS2
            JIDB = NLINK('MDC1',NODEC(I))                               ASKUS2
            IF (JIDB .EQ. 0) MDCY(PYCOMP(NODEC(I)),1) = 0               ASKUS2
          ENDIF                                                         ASKUS2
 150       CONTINUE                                                      
      ENDIF                                                             ASKUS2
C                                                                       ASKUS2
C  Generator initialization
C                                                                    
      CALL KW_Initialize(XPAR)
*
      if(ifhrwg.eq.1) then        ! AST do herwig initialisation
        call iniher(xpar)
      endif
C                                                                       ASKUS2
C  Print PART and KPAR banks                                            ASKUS2
C                                                                       ASKUS2
C     CALL PYLIST(12)                                                   ASKUS2
C     CALL PRPART                                                       ASKUS2
      CALL PRTABL('RLEP',0)                                             ASKUS2
      CALL PRTABL('KPAR',0)                                             ASKUS2
C                                                                       ASKUS2
C                                                                       ASKUS2
C    possibly update branching ratios  with card GKBR                   ASKUS2
C                                                                       ASKUS2
      NAGKBR = NAMIND('GKBR')                                           ASKUS2
      JGKBR = IW(NAGKBR)                                                ASKUS2
      IF(JGKBR.NE.0) THEN                                               ASKUS2
C check consitency of length                                            ASKUS3
        NLEN = IW(JGKBR)                                                ASKUS3
        IF ( NLEN .NE.NCHAN+4 ) THEN                                    ASKUS3
            WRITE (IW(6),'(1X,'' Inconsistent number of Brs should be'',ASKUS3
     $                    I5,'' is '',I5)') NCHAN,NLEN-4                ASKUS3
            CALL EXIT                                                   ASKUS3
        ENDIF                                                           ASKUS3
        BRA1   = RW(JGKBR+1)                                            ASKUS3
        BRK0   = RW(JGKBR+2)                                            ASKUS3
        BRK0B  = RW(JGKBR+3)                                            ASKUS3
        BRKS   = RW(JGKBR+4)                                            ASKUS3
        DO 51 I = 1,NCHAN                                               ASKUS3
           GAMPRT(I) = RW(JGKBR+4+I)                                    ASKUS3
 51     CONTINUE                                                        ASKUS3
        IF ( GAMPRT(1).NE.1.) THEN                                      ASKUS3
         DO 52 I = 1, NCHAN                                             ASKUS3
           GAMPRT(I) = GAMPRT(I)/GAMPRT(1)                              ASKUS3
 52      CONTINUE                                                       ASKUS3
        ENDIF                                                           ASKUS3
      ENDIF                                                             ASKUS3
C     Necessary to keep info on fragmentation
C
      MSTU(17) = 1
C                                                                       ASKUS3
C   Store the version used in the job and the branching ratios in       ASKUS3
C   header bank  KORL                                                   ASKUS3
      NCOL = NCHAN+5                                                    ASKUS3
      NROW = 1                                                          ASKUS3
      TABL(1) = TAUOLAVER(dum)
      TABL(2) = BRA1                                                    ASKUS3
      TABL(3) = BRK0                                                    ASKUS3
      TABL(4) = BRK0B                                                   ASKUS3
      TABL(5) = BRKS                                                    ASKUS3
      DO 57 IBR = 1,NCHAN                                               ASKUS3
          TABL(5+IBR) = GAMPRT(IBR)                                     ASKUS3
 57   CONTINUE                                                          ASKUS3
      JKORL = ALTABL('KORL',NCOL,NROW,TABL,'2I,(F)','C')                ASKUS3
      CALL PRTABL('KORL',0)                                             ASKUS3
C                                                                       AV
C Book some control histograms - only for unweighted events             AV
C                                                                       AV
      IF (KeyWgt.EQ.0) THEN                                             AV
        call hbook1  (14,'Energy fermion 1',100,0.,cmsene,0.)           AV
        call hbook1  (24,'Energy fermion 2',100,0.,cmsene,0.)           AV
        call hbook1  (34,'Energy fermion 3',100,0.,cmsene,0.)           AV
        call hbook1  (44,'Energy fermion 4',100,0.,cmsene,0.)           AV
        call hbook1  (16,'Theta fermion 1',90,0.,180.,0.)               AV
        call hbook1  (26,'Theta fermion 2',90,0.,180.,0.)               AV
        call hbook1  (36,'Theta fermion 3',90,0.,180.,0.)               AV
        call hbook1  (46,'Theta fermion 4',90,0.,180.,0.)               AV
        call hbook1  (19,                                               AV
     +       'Flavour (pdg-code) fermion 1',33,-16.5,16.5,0.)           AV
        call hbook1  (29,                                               AV
     +       'Flavour (pdg-code) fermion 2',33,-16.5,16.5,0.)           AV
        call hbook1  (39,                                               AV
     +       'Flavour (pdg-code) fermion 3',33,-16.5,16.5,0.)           AV
        call hbook1  (49,                                               AV
     +       'Flavour (pdg-code) fermion 4',33,-16.5,16.5,0.)           AV
        call hbook1 (125,'Invariant mass fermions 1-2',100,0.,cmsene,0.)AV
        call hbook1 (135,'Invariant mass fermions 1-3',100,0.,cmsene,0.)AV
        call hbook1 (145,'Invariant mass fermions 1-4',100,0.,cmsene,0.)AV
        call hbook1 (235,'Invariant mass fermions 2-3',100,0.,cmsene,0.)AV
        call hbook1 (245,'Invariant mass fermions 2-4',100,0.,cmsene,0.)AV
        call hbook1 (345,'Invariant mass fermions 3-4',100,0.,cmsene,0.)AV
        call hbook1 (601,'Number of ISR photons',20,-0.5,19.5,0.)       AV
        call hbook1 (602,'Total energy of ISR photons',70,0.,70.,0.)    AV
        call hbook1 (603,'Transverse component of ISR momentum',        AV
     +       100,0.,20.,0.)                                             AV
      ENDIF
C List of weight of events per channel ( Grace nomenklatura)
      DO INUM=0,76
       call hbook1(inum+88000,'Wt/WtMax channel',100,0.,1.,0.)
       call hbook1(inum+89000,'Log10(Wt/WtMax) channel over',
     +100,0.,10.,0.)
      ENDDO
      RETURN                                                            ASKUS3
      END                                                               ASKUS3
      SUBROUTINE KRLWINI (XPAR)
C
C-----------------------------------------
C
C   Author   :- b. Bloch                     18-JAN-1999
C
C=========================================
C
C   Purpose   : give default values to input parameters
C   Inputs    :
C   Outputs   : xpar array
C
C=========================================
C +
C Declarations.
C -
      IMPLICIT NONE
      REAL*8 xpar(10000)
      real*8 apar1(10000)
      INTEGER i
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C Entry Point.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      data(apar1(I),I=1,140)/
     1            180d0,1.16639d-5,128.07d0,91.1888d0,2.4974d0,
     2            80.230d0,-2.085d0,1d-6,0.99d0,2d0,
     3            1000D0,1.0D0,0.12d0,600d0,8d0,
     4            1d-6,300d0,-1d0,0d0,0d0,
C*         entries 21-73 reserved for anomalous couplings
     5            .1191D+01,-.3060D+00,.1283D+01,-.3160D+01,-.7430D+00,
     6            -.6050D+00,.1530D+01, 3*0d0,
     7            -.8010D+00,.7237D+00,-.4960D+00,.8320D+00,-.1490D+01,
     8            .8380D+00,.9920D-01,  3*0d0,
     9            -.1960D+01,-.4970D+00,.6380D+00,-.9980D+00,.6410D+00,
     $            .1638D+00,-.5950D+00, 3*0d0,
     1            .5169D+00,.6489D+00,.1426D+01,-.2760D+00,.2986D+01,
     2            .4021D+00,.7230D+00, 3*0d0,
C* KeyAcc=2: Set 2, cf. YR CERN 96-01 "Physics at LEP2", Vol 1, p. 525
     3            0.2D+00,0.1D+00,0.1D+00,0.1D+00,0.1D+00,   5*0d0,
C* KeyAcc=3: Set 3, cf. YR CERN 96-01 "Physics at LEP2", Vol 1, p. 525
     4            0.1D+00,0.1D+00,-0.1D+00, 7*0d0,   19*0d0,
C*                el mas         1/alpha_QED, GeV-->picobarn
     5            0.51099906d-3,137.0359895D0,389.37966D6,   8*0d0,
C* Default values of for KeyBra =2    111-->119
     6            0.97525d0,0.22050d0,0.00315d0,-0.22050d0,0.97450d0,
     7            0.03900d0,0.00850d0,-0.03850d0,0.99925d0,0.d0, 10*0d0,
C*  120 unused     121-129 reserved for imaginary parts , 130 unused
C* Values of BRanchings for W decay channels for KeyBra = 1 131-->139
     8            0.32110D0,0.01630D0,0.01635D0,0.32043D0,0.00002D0,
     9            0.00070D0,0.10840D0,0.10840D0,0.10830D0,  0d0/
      data (apar1(I),I=141,1100) /
C* 140 unused, 141-> 150 unused,
C*               Energy dependent wtmax_cc03   151-->157 158-510 unused,
     &  10*0d0,   7.0D0,5.0D0,4.0D0,4.4D0,4.8D0,7.0D0,9.0D0, 353*0d0,
C*     PROPERTIES of Quarks and Leptons  500+10*KFlavour +j
     1           1,3,-1,-1,0,0.010d0,3.5d0,3*0d0,  ! d-quark 511-517
     2           2,3, 2, 1,0,0.005d0,3.5d0,3*0d0,  ! u-quark 521-527
     3           3,3,-1,-1,0,0.200d0,3.5d0,3*0d0,  ! s-quark 531-537
     4           4,3, 2, 1,0,1.300d0,3.5d0,3*0d0,  ! c-quark 541-547
     5           5,3,-1,-1,0,4.300d0,3.5d0,3*0d0,  ! b-quark 551-557
     6           6,3, 2, 1,0,175.0d0,3.5d0,3*0d0,  ! t-quark 561-567
     7  40*0d0, 11,1,-3,-1,0,.51099906d-3,3.5d0,3*0d0, ! electron 611-617
     8          12,1, 0, 1,0,   1d-3,3.5d0,3*0d0,  ! neutr electron 621-7
     9          13,1,-3,-1,0,.105658389d0,3.5d0,3*0d0, ! muon 631-637
     &          14,1, 0, 1,0,   1d-3,3.5d0,3*0d0,  ! neutr muon 641=647
     1          15,1,-3,-1,0,1.7771d0,3.5d0,3*0d0, ! tau 651-657
     2          16,1, 0, 1,0,   1d-3,3.5d0,3*0d0,  ! neutr tau 661-667
C               KeyISR KeyFSR KeyNLL KeyCul 1011 -1014
     3 340*0d0, 1d0,0d0,1d0,1d0,6*0d0,
C               KeyBra KeyMas KeyZet KeySpn KeyRed KeyWu   1021 -1026
     4          2d0,   1d0,   0d0,   1d0,   0d0,   0d0, 4*0d0,
C               KeyWgt KeyRnd KeySmp  1031-1033
     5          0d0,   1d0,   2d0,    7*0d0,
C               KeyMix Key4f KeyAcc KeyZon KeyWon  1041-1045
     6          0d0,   1d0,  0d0,   1d0,   1d0,    5*0d0,
C Exclusive W decays    KeyDWM KeyDWP Nout 1055-1057
     7  4*0d0,          0d0,   0d0,   16d0,          3*0d0,
C Tauola and Photos JAK1 JAK2 ITDKRC IFPHOT IFHADM IFHADP 1071-1076 -->1100
     8  10*0d0,     0d0, 0d0, 1d0,   1d0,   1d0,   1d0, 4*0d0, 20*0d0/
      data (apar1(I),i=1101,1181) /
C*   Entries from 1101 to 1302 are reserved for Umask matrix, next unused
C*   1-81: WW Wp=1:1-9; 2:10-18..
C Wm=  1:ud 2:cd 3:us 4:cs 5:ub 6:cb 7:el 8:mu 9:ta /  Wp=
     9 1,  1,    1,    1,    1,  1,    1,   1,    1,    !  1:ud
     & 1,  0,    1,    1,    1,  1,    1,   1,    1,    !  2:cd
     1 1,  1,    0,    1,    1,  1,    1,   1,    1,    !  3:us
     2 1,  1,    1,    1,    1,  1,    1,   1,    1,    !  4:cs
     3 1,  1,    1,    1,    0,  1,    1,   1,    1,    !  5:ub
     4 1,  1,    1,    1,    1,  0,    1,   1,    1,    !  6:cb
     5 1,  1,    1,    1,    1,  1,    1,   1,    1,    !  7:el
     6 1,  1,    1,    1,    1,  1,    1,   1,    1,    !  8:mu
     7 1,  1,    1,    1,    1,  1,    1,   1,    1/    !  9:ta
      data (apar1(I),I=1182,10000) /
C*  82-202: ZZ Z1=1:82-92; 2:93-103..
C Z1=  1:d  2:u  3:s  4:c  5:b  6:el 7:mu 8:ta 9:ve 10:vm 11:vt / Z2=
     8 1,    0,   0,   0,   0,   0,   0,   0,   0,    0,    0,    !  1:d
     9 0,    1,   0,   0,   0,   0,   0,   0,   0,    0,    0,    !  2:u
     & 1,    1,   1,   0,   0,   0,   0,   0,   0,    0,    0,    !  3:s
     1 1,    1,   0,   1,   0,   0,   0,   0,   0,    0,    0,    !  4:c
     2 1,    1,   1,   1,   1,   0,   0,   0,   0,    0,    0,    !  5:b
     3 1,    1,   1,   1,   1,   1,   0,   0,   0,    0,    0,    !  6:el
     4 1,    1,   1,   1,   1,   1,   1,   0,   0,    0,    0,    !  7:mu
     5 1,    1,   1,   1,   1,   1,   1,   1,   0,    0,    0,    !  8:ta
     6 1,    1,   1,   1,   1,   0,   1,   1,   1,    0,    0,    !  9:ve
     7 1,    1,   1,   1,   1,   1,   0,   1,   1,    1,    0,    !  10vm
     8 1,    1,   1,   1,   1,   1,   1,   0,   1,    1,    1,    !  11vt
     9  8*0d0,2690*0d0,
C*   4000-5000 reserved for user private exercises, for example:
     &  60*0d0,
C* BE parameters:  4061 - 4070
C      range ifun  pp    radius lambda   
     & 0.2d0,1d0,0.20d0,1.00d0,1.0675d0,
C      avewt    lambda2  avewt2   KeyRej WtMax
     & 0.1751d0,1.0510d0,0.4387d0,0d0,   3d0,
     & 5930*0d0 /
      do 10 i=1,10000
         xpar(i) = apar1(i)
  10  continue
  999 RETURN
      END

      SUBROUTINE ASKUSE (IDP,IST,NTRK,NVRT,ECM,WEI)                     
C --------------------------------------------------------------------  
C                                  S. Jezequel July 1997
C Generation                       A.Valassi May 1996                   
C Adapted from KRLW01 interface:   P.Perez   August 1995                
C                                  B.Bloch   November 1995              
C                                  B.Bloch March 98 for Pythia 6.1
C --------------------------------------------------------------------  
C--------------------------------------------------------------------   
C     input     : none                                                  
C                                                                       
C     output    : 6 arguments                                           
C          IDP    : process identification                              
C          IST    : status flag ( 0 means ok)                           
C          NTRK   : number of tracks generated and kept                 
C          NVRT   : number of vertices generated                        
C          ECM    : center of mass energy for the event                 
C          WEI    : event weight always equal to 1                      
C--------------------------------------------------------------------   
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
      COMMON /PYDAT3/MDCY(L2PAR,3),MDME(LJNPAR,2),BRAT(LJNPAR),
     &               KFDP(LJNPAR,5)
      COMMON/PYDAT4/CHAF(L2PAR,2)
      CHARACTER CHAF*16
C
      COMMON/PYSUBS/MSEL,MSELPD,MSUB(500),KFIN(2,-40:40),CKIN(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
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
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               
C                                                                       
      COMMON /BCS/   IW(LBCS )                                          
      INTEGER IW                                                        
      REAL RW(LBCS)                                                     
      EQUIVALENCE (RW(1),IW(1))                                         

      REAL*4 VRTEX,SDVRT,XVRT,SXVRT,ECMI
      COMMON / KGCOMM /VRTEX(4),SDVRT(3),XVRT(3),SXVRT(3),ECMI,
     $         NEVENT(10),IFVRT,ISTA
      COMMON / decays / IFlav(4), amdec(4)
      COMMON / wgtgen / wtves,wtyfs,wtborn !<-- only wtborn is used
      COMMON / wgtall / wtcrud,wtmod,wtset(100)
      REAL *8 WTCRUD,WTMOD,WTSET,AMDEC,BRRAT,BREL                       
      REAL *8 WTMOD4F                                                   

      COMMON / WGTUNW /wtunw,wtunwmax                              !cav AV
      DOUBLE PRECISION wtunw,wtunwmax                                   AV
      COMMON / OVRWGT /nevovrwgt                                   !cav AV
      DATA             nevovrwgt/0/                                     AV
      COMMON / SELCTO /nkwcte0, nkwcte1,nkwcte2                   !cav AV
      DATA             nkwcte0/0/,nkwcte1/0/,nkwcte2/0/
      COMMON / KeyKey / KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp       AV
      COMMON / momset / qeff1(4),qeff2(4),sphum(4),sphot(100,4),nphot
      COMMON / momdec / q1(4),q2(4),p1(4),p2(4),p3(4),p4(4)
      DOUBLE PRECISION  QEFF1,   QEFF2,   SPHUM,   SPHOT                AV
      DOUBLE PRECISION  Q1,   Q2,   P1,   P2,   P3,   P4                AV
      DOUBLE PRECISION  SUMISR(4)                                       AV
      COMMON / LIBRA  / JAK1,JAK2,ITDKRC,IFPHOT,IFHADM,IFHADP           AV
      COMMON /SJIDET/IDEF
!-- 4fermion weights wt4f(1) cc03 wt4f(2) 4f
      REAL*8 WT4F
      COMMON /WW4FM/ wt4f(9)
C
      REAL ECM,WEI
C      DOUBLE PRECISION XPAR(100)                                        ASKUSE
C      DIMENSION NPAR(100)                                               ASKUSE
      DIMENSION E1(3),E2(3)
      DIMENSION IRN(3),IRN1(3)
C
      INTEGER ALTABL
      REAL TABL(200)
      INTEGER KABL(200)
      EQUIVALENCE (TABL,KABL)
C                                                                       ASKUSE
      IST  = 0                                                          ASKUSE
      IDP  = 0                                                          ASKUSE
      ECM  = 0.                                                         ASKUSE
      WEI  = 0.
C Reset fragmentation storage in common
      MSTU(90)=0
C
C  Generate primary vertex                                             
C
      CALL RANNOR (RN1,RN2)                                            
      CALL RANNOR (RN3,DUM)                                             
      VRTEX(1) = RN1*SDVRT(1)                                           
      VRTEX(2) = RN2*SDVRT(2)                                           
      VRTEX(3) = RN3*SDVRT(3)                                           
      VRTEX(4) = 0.                                                     
      IF ( IFVRT.ge.2) then
         CALL RANNOR(RXX,RYY)
         CALL RANNOR(RZZ,DUM)
         VRTEX(1) = VRTEX(1) + RXX*SXVRT(1)
         VRTEX(2) = VRTEX(2) + RYY*SXVRT(2)
         VRTEX(3) = VRTEX(3) + RZZ*SXVRT(3)
      ENDIF
      IF ( IFVRT.ge.1) then
         VRTEX(1) = VRTEX(1) + XVRT(1)
         VRTEX(2) = VRTEX(2) + XVRT(2)
         VRTEX(3) = VRTEX(3) + XVRT(3)
      ENDIF
C                                                                       
C  Event generation                                                     
C                                                                       
      KeyWgt = MOD(KeyTek,10)                                           
      NEVENT(1) = NEVENT(1) + 1
      CALL KW_MAKE
      if(nevent(1).le.5) call pylist(1)
*      print *,'nevent',nevent                                          
      IDP  = abs(IFLAV(1))+100*abs(IFLAV(2))+10000*abs(IFLAV(3))        
     $        +1000000*abs(IFLAV(4))                                    
      ECM  = ECMI                                                       
      IF (KeyWgt.EQ.0) THEN       ! Unweighted events (wtmod>=1)        
C Get interesting weights and store some in KWGT
         call KKWGT(ist)
         if ( IST.eq.0) then
         ista = ista + 1000
         WRITE(iw(6),
     &  '('' -ERROR booking KWGT AT EVENT #  '',I10)') IFI,ist
      ENDIF
        WEI = wtmod                                                     
      ELSE IF (KeyWgt.EQ.1) THEN  ! Weighted events                     AV
        wtmod4f = wtset(40)                                             AV
        WEI = wtmod*wtmod4f                                             AV
      ELSE IF (KeyWgt.EQ.2) THEN  ! Weighted events (wtmod>=1 for CC3)  AV
        wtmod4f = wtset(40)                                             AV
        WEI = wtmod*wtmod4f                                             AV
      ENDIF                                                             AV
cav   IF(IST.NE.0) THEN           ! No path to following two statements AV
cav     NEVENT(4) = NEVENT(4) + 1 ! These can be used to reject events  AV
cav     GO TO 20                  ! for which KORALW has "bad" status   AV
cav   ENDIF                       ! (must set IST#0 in that case...)    AV
C  decay remaining pi0's                                                
C commented by sj IF (IFHADM.EQ.1 .AND. IFHADP.EQ.1) CALL LUEXEC
C  Book all banks                                                       
C   Modified B.Bloch for pythia 6.1
      CALL KXP6AL(VRTEX,ISTA,NVRT,NTRK)                                 
      IST = ISTA
C Book the 4f and CC03 matrix element in Bank KXME                      
      CALL VZERO(TABL,8)
      KABL(1)=0
      TABL(2)=SNGL(wt4f(1))
      KABL(5)=0
      TABL(6)=SNGL(WT4F(2))
      NROW=2
      JKXME = ALTABL('KXME',4,NROW,TABL,'2I,(I,3F)','E')
C
C   Book & fill the bank KZFR with info on fragmentation
C
      call kp6zfr(isk)
      IF(isk .NE.0) ISTAT = -1
C Do we have to keep this (comes from hzha)
      IF (MSTU(24).GT.0) THEN
        IST   = -8
        CALL PYLIST(1)
      ENDIF
C
      IF(IST.NE.0) THEN                                                 
        NEVENT(5) = NEVENT(5) + 1                                       
        GO TO 20                                                        
      ENDIF                                                             
C                                                                       AV
C Fill histograms - only for unweighted events                          AV
C                                                                       AV
      IF(IST.EQ.0) THEN                                                 AV
        IF (KeyWgt.EQ.0) THEN                                           AV
          call hfill (14,real(p1(4)),0.,1.)                             AV
          call hfill (24,real(p2(4)),0.,1.)                             AV
          call hfill (34,real(p3(4)),0.,1.)                             AV
          call hfill (44,real(p4(4)),0.,1.)                             AV
          call hfill (16,                                               AV
     &      acosd(real( p1(3)/sqrt(p1(1)**2+p1(2)**2+p1(3)**2) )),0.,1.)AV
          call hfill (26,                                               AV
     &      acosd(real( p2(3)/sqrt(p2(1)**2+p2(2)**2+p2(3)**2) )),0.,1.)AV
          call hfill (36,                                               AV
     &      acosd(real( p3(3)/sqrt(p3(1)**2+p3(2)**2+p3(3)**2) )),0.,1.)AV
          call hfill (46,                                               AV
     &      acosd(real( p4(3)/sqrt(p4(1)**2+p4(2)**2+p4(3)**2) )),0.,1.)AV
          call hfill (19,real(iflav(1)),0.,1.)                          AV
          call hfill (29,real(iflav(2)),0.,1.)                          AV
          call hfill (39,real(iflav(3)),0.,1.)                          AV
          call hfill (49,real(iflav(4)),0.,1.)                          AV
          call hfill (125,                                              AV
     &      real(sqrt( (p1(4)+p2(4))**2 -                               AV
     &                 (p1(1)+p2(1))**2 -                               AV
     &                 (p1(2)+p2(2))**2 -                               AV
     &                 (p1(3)+p2(3))**2   )), 0., 1.)                   AV
          call hfill (135,                                              AV
     &      real(sqrt( (p1(4)+p3(4))**2 -                               AV
     &                 (p1(1)+p3(1))**2 -                               AV
     &                 (p1(2)+p3(2))**2 -                               AV
     &                 (p1(3)+p3(3))**2   )), 0., 1.)                   AV
          call hfill (145,                                              AV
     &      real(sqrt( (p1(4)+p4(4))**2 -                               AV
     &                 (p1(1)+p4(1))**2 -                               AV
     &                 (p1(2)+p4(2))**2 -                               AV
     &                 (p1(3)+p4(3))**2   )), 0., 1.)                   AV
          call hfill (235,                                              AV
     &      real(sqrt( (p2(4)+p3(4))**2 -                               AV
     &                 (p2(1)+p3(1))**2 -                               AV
     &                 (p2(2)+p3(2))**2 -                               AV
     &                 (p2(3)+p3(3))**2   )), 0., 1.)                   AV
          call hfill (245,                                              AV
     &      real(sqrt( (p2(4)+p4(4))**2 -                               AV
     &                 (p2(1)+p4(1))**2 -                               AV
     &                 (p2(2)+p4(2))**2 -                               AV
     &                 (p2(3)+p4(3))**2   )), 0., 1.)                   AV
          call hfill (345,                                              AV
     &      real(sqrt( (p3(4)+p4(4))**2 -                               AV
     &                 (p3(1)+p4(1))**2 -                               AV
     &                 (p3(2)+p4(2))**2 -                               AV
     &                 (p3(3)+p4(3))**2   )), 0., 1.)                   AV
          do k=1,4                                                      AV
            sumisr(k)=0d0                                               AV
            do i=1,nphot                                                AV
              sumisr(k) = sumisr(k) + sphot (i,k)                       AV
            end do                                                      AV
          end do                                                        AV
          call hfill (601,real(nphot),0.,1.)                            AV
          call hfill (602,real(sumisr(4)),0.,1.)                        AV
          call hfill (603,real(sqrt(sumisr(1)**2+sumisr(2)**2)),0.,1.)  AV
          call hfill(88000,real(wtunw),0.,1.)
          call hfill(88000+idef,real(wtunw),0.,1.)                      AV
          IF (wei.GT.1.) then                                           AV
            nevovrwgt = nevovrwgt+1                                     AV
            call hfill(89000+idef,real(log(wtunw)/log(10d0)),0.,1.)     AV
            call hfill(89000,real(log(wtunw)/log(10d0)),0.,1.)
          ELSE
C            print *,'wt4f',wt4f
          ENDIF                                                         AV
        ENDIF                                                           AV
      ENDIF                                                             AV
C                                                                       
C  Event counters                                                       
C                                                                       
      IF(IST.EQ.0) THEN                                                 
        NEVENT(2) = NEVENT(2) + 1                                       
        DO 10 IP = 1,N7LU                                               
          IF(K7LU(IP,2).EQ.22) THEN                                     
            NEVENT(8) = NEVENT(8) + 1                                   
            GO TO 30                                                    
          ENDIF                                                         
   10   CONTINUE                                                        
        NEVENT(7) = NEVENT(7) + 1                                       
      ENDIF                                                             
   20 IF(IST.NE.0) NEVENT(3) = NEVENT(3) + 1                            
C                                                                       
   30 RETURN                                                            
      END                                                               
      SUBROUTINE KKWGT(ist)
C-------B.Bloch march 2k -------------------
C    stored some interesting weights for each event
C    in bank KWGT. If all ok ist = 1, if one or more booking pb, ist=0
C-------------------------------------------------------------------------
      INTEGER     len      ! maximum number of auxiliary weights in WtSet
      PARAMETER ( len = 100)
      Double precision wtmain,wtcrud,wtset(len)
      ist = 1
      call kw_getwtall(wtmain,wtcrud,wtset)
      weik = wtset(1)    ! Born
      ind = kwgtbk(1,1,weik)
      if (ind.le.0) ist = 0
      weik = wtset(2)    ! 1st order
      ind = kwgtbk(2,2,weik)
      if (ind.le.0) ist = 0
      weik = wtset(3)    ! 2nd order
      ind = kwgtbk(3,3,weik)
      if (ind.le.0) ist = 0
      weik = wtset(4)    ! 3rd order
      ind = kwgtbk(4,4,weik)
      if (ind.le.0) ist = 0
      weik = wtset(40)    ! 4f external matrix elemnt
      ind = kwgtbk(5,40,weik)
      if (ind.le.0) ist = 0
      RETURN
      END
************************************************************************
      SUBROUTINE USCJOB                                                 2
C --------------------------------------------------------------------  3
C End of generation                A.Valassi May 1996                   AV
C Adapted from KRLW01 interface:   P.Perez   August 1995                AV
C                                  B.Bloch   November 1995              AV
C --------------------------------------------------------------------  5
      COMMON / INOUT  / INUT,IOUT                    

      REAL*4 VRTEX,SDVRT,XVRT,SXVRT,ECMI
      COMMON / KGCOMM /VRTEX(4),SDVRT(3),XVRT(3),SXVRT(3),ECMI,
     $         NEVENT(10),IFVRT,ISTA

      COMMON / DECAYS / IFLAV(4),AMDEC(4),BRRAT(2),BREL                 3
      COMMON / WGTALL / WTCRUD,WTMOD,WTSET(100)                         4
      REAL *8 WTCRUD,WTMOD,WTSET,AMDEC,BRRAT,BREL                       5
      COMMON / WGTGEN /WTMAX,WTMAX4F(20),WTVES,WTYFS,WTSS,WTBWIG,WTBORN AV
      DOUBLE PRECISION WTMAX,WTMAX4F,WTVES,WTYFS,WTSS,WTBWIG,WTBORN     AV
      COMMON / KeyKey / KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp       AV
cav   COMMON / cmonit / averwt,errela,nevtot,nevacc,nevneg,nevove,nevzerAV
cav   DOUBLE PRECISION  averwt,errela                                   AV
      DOUBLE PRECISION svar,Xtot,erra,Xtot2e,errabs                     AV
      COMMON / WGTUNW /wtunw,wtunwmax                              !cav AV
      DOUBLE PRECISION wtunw,wtunwmax                                   AV
      COMMON / OVRWGT /nevovrwgt                                   !cav AV
      COMMON / SELCTO /nkwcte0, nkwcte1,nkwcte2                    !cav AV
      DOUBLE PRECISION XPAR(100)                                        8
      DIMENSION NPAR(100)                                               9
C                                                                       
C End of generation                                                     
C                                                                       
      CALL KW_Finalize
C                                                                       
C Print event counters                                                  
C                                                                       
       WRITE(IOUT,101)                                                  
  101  FORMAT(//20X,'EVENTS STATISTICS',                                
     &         /20X,'*****************')                                
       WRITE(IOUT,102) NEVENT(1),NEVENT(2),NEVENT(3),                   
     &                 NEVENT(7),NEVENT(8)                              
  102  FORMAT(/5X,'# OF GENERATED EVENTS                      = ',I10,  
     &        /5X,'# OF ACCEPTED  EVENTS                      = ',I10,  
     &        /5X,'# OF REJECTED  EVENTS (ISTA # 0 in ASKUSE) = ',I10,  
     &        /5X,'# OF EVENTS WITHOUT PHOTON                 = ',I10,  
     &        /5X,'# OF EVENTS WITH PHOTON                    = ',I10)  
       WRITE(IOUT,103)                                                  
  103  FORMAT(//20X,'ERRORS STATISTICS',                                
     &         /20X,'*****************')                                
       WRITE(IOUT,104) NEVENT(4),NEVENT(5)                              
  104  FORMAT(/10X,'ISTA # 0 FROM KORALW        # OF REJECT = ',I10,    AV
     &        /10X,'ISTA # 0 FROM KXL7AL        # OF REJECT = ',I10)    
C                                                                       
C Print cross-section (MC and Semi-analytical calculations)             AV
C                                                                       AV
      svar=ecmi**2                                                      AV
      keypho=0                                                          AV
      keypre=1                                                          AV
      call korwan(svar,0d0,1d0,keypho,keypre,Xtot,erra)                 AV
      keypho=302                                                        AV
      keypre=1                                                          AV
      call korwan(svar,0d0,1d0,keypho,keypre,Xtot2e,errabs)             AV
cav   XSCRUD = xpar(30)                                                 AV
cav   XSKB   = xscrud*averwt                                            AV
cav   ERKB   = xskb*errela                                              AV
      XSECT  = xpar(20)                                                 AV
      ERSECT = xpar(21)                                                 AV
      KEYWGT = MOD(KeyTek,10)                                           AV
      IF(KeyWgt .EQ. 0) then                                            AV
        NVTRU = npar(10)                                                AV
        NVTOT = npar(11)                                                AV
cav     EREL = (1-dble(nvtru)/dble(nvtot))/sqrt(dble(nvtot))            AV
      ENDIF                                                             AV
      WRITE(iout,*)                                                     AV
      WRITE(iout,'(A60)') '============ KRLW03: X-sections ============'AV
      WRITE(iout,*)                                                     AV
      WRITE(iout,'(1X,F17.8, 4H  +-, F17.8, 1X, A30)')                  AV
     $     Xtot,erra,'SemiAnal Born, KORWAN'                            AV
      WRITE(iout,*)                                                     AV
      WRITE(iout,'(1X,F17.8, 4H  +-, F17.8, 1X, A30)')                  AV
     $     Xtot2e,errabs,'SemiAnal O(alf2)exp.LL, KORWAN'               AV
      WRITE(iout,*)                                                     AV
      WRITE(iout,*)                                                     AV
      IF(KeyWgt .EQ. 0) then                                            AV
        WRITE(iout,'(1X,A40,F20.4)')                                    AV
     $       'Max weight for rejection was ',WTMAX                      AV
        WRITE(iout,'(1X,A40,I20)')                                      AV
     $       'Total generated events ',NVTOT                            AV
        WRITE(iout,'(1X,A40,I20)')                                      AV
     $       '- rejected by GCE0 cuts ',nkwcte0                         AV
        WRITE(iout,'(1X,A40,I20)')                                      AV
     $       '- rejected by GCE1 cuts ',nkwcte1                         AV
        WRITE(iout,'(1X,A40,I20)')                                      AV
     $       '- rejected by GCE2 cuts ',nkwcte2                         AV
        WRITE(iout,'(1X,A40,I20)')                                      AV
     $       'Total accepted events ',NVTRU                             AV
        WRITE(iout,'(1X,A40,I20)')                                      AV
     $       'amongst which, overweighted ',NEVOVRWGT                   AV
        IF (NEVOVRWGT.GT.0) THEN                                        AV
          WRITE(iout,'(1X,A40,f20.9)')                                  AV
     $       '===> INCREASE MAX WEIGHT TO AT LEAST ',WTUNWMAX*WTMAX     AV
          WRITE(iout,'(1X,A40,f20.9)')                                  AV
     $       '     SLOWDOWN FACTOR WILL BE ',WTUNWMAX                   AV
          WRITE(iout,'(1X,A40)')                                        AV
     $       '     (OR USE TIGHTER CUTS INSTEAD...)'                    AV
        ENDIF                                                           AV
        WRITE(iout,*)                                                   AV
cav     WRITE(iout,'(1X,F17.8, 4H  +-, F17.8, 1X, A30)')                AV
cav  $       Xscrud,Xscrud*erel, 'MC Best, XSCRUD, KORALW'              AV
cav     WRITE(iout,*)                                                   AV
      ENDIF                                                             AV
cav   WRITE(iout,'(1X,F17.8, 4H  +-, F17.8, 1X, A30)')                  AV
cav  $     xskb,erkb, 'MC Best, WTMOD, KORALW'                          AV
cav   WRITE(iout,*)                                                     AV
      WRITE(iout,'(1X,F17.8, 4H  +-, F17.8, 1X, A30, 4H <<<)')          AV
     $     xsect,ersect, 'MC Best, XPAR, KORALW'                        AV
      WRITE(iout,*)                                                     AV
      WRITE(iout,'(A60)') '========== KRLW03: End X-sections =========='AV
      WRITE(iout,*)                                                     AV
C                                                                       AV
      RETURN                                                            
      END                                                               

      subroutine user_selecto(p1,p2,p3,p4,qeff1,qeff2,wt)
C ######################################
C mask on phase space region, can be   #
C modified arbitrarily                 #
C ######################################
      implicit real*8 (a-h,o-z)
C
C
c
c Modified S.Jezequel 07/1997 => adapt to new version of koralw1.31
c Modified A.Valassi 26/06/96 => introduce Kingal interfaced cuts in the MC.
c          A.Valassi 16/07/96 => change qadra (integer!) to qadra (ZW 5/7/96)
c Input:  p1, p2, p3, p4 -> 4-momenta of 4 final state fermions
c         qeff1, qeff2   -> effective 4-mom. of initial state e+/e- after ISR
c         wt             -> Current weight # 0
c         /DECAYS/       -> Event properties (including flavour) via common
c         /KWCTE0/       -> Cuts via common
c         /KWCTE1/       -> Cuts via common
c         /KWCTE2/       -> Cuts via common
c Output: wt             -> Weight = 0 if event outside allowed phase space
c Remark: cuts are in single precision. Protection added on cos(theta).
c Cut description:
c 1. (COMMON/KWCTE0/) - applied to all events
c    => Cuts on acceptance and sum of pT^2
c 2. (COMMON/KWCTE1/) - only for e+e- ch+ ch- events
c    => Cuts on invariant masses
c 3. (COMMON/KWCTE2/) - only for e+e- ch+ ch- events
C    => Remove high pT ISR
c
cav      common /articut/ arbitr,cosmin
      COMMON / DECAYS / IFLAV(4), AMDEC(4), BR(2), BREL
      COMMON / WorZ   / probw,probz,ifwon,ifzon,ifWnow,icwm,icwp
      save / DECAYS /,/ WorZ   /
      save
      data init /0/
      logical icc
      dimension p1(4),p2(4),p3(4),p4(4),qeff1(4),qeff2(4)
      REAL*4          cmin,cmax,spt2,scut,scutee,spt2ee
      COMMON /KWCTE0/ cmin,cmax,spt2,scut,scutee,spt2ee              !cav
      SAVE   /KWCTE0/
      COMMON /SELCTO/ nkwcte0,   nkwcte1,   nkwcte2                  !cav
      SAVE   /SELCTO/
C
C From selecto from koralw03
! to prevent double counting of channels which belong simultaneously
! to WW and ZZ final states corresponding ZZ statets will be set to zero.
      if(ifwon.eq.1.and.ifzon.eq.1.and.ifWnow.eq.0) then
         if((iflav(2).eq.-iflav(3)).and.(iflav(1).ne.-iflav(2))) wt=0d0
      endif
! introduce statistical factor for identical particles whenever necessary.
! this was a bug (but in test versions only) ZW. 1.05.97
!!!!!         if(iflav(1).eq.iflav(3)) wt=wt/4
! kill double counting in different zz final states.
      if(ifWnow.eq.0.and.iflav(1).gt.iflav(3))wt=0d0
C
C
c 1. /KWCTE0/ cuts
c ----------------
c Cuts
c
      angarb=1.-amax1(abs(cmin),abs(cmax))**2
c
C     all 4 final fermions have to be within acceptance
      ICC=.TRUE.
      do ii=1,4
       if(p3(ii)/sqrt(p1(ii)**2+p2(ii)**2+p3(ii)**2).gt.cmax)
     & icc=.false.
       if(p3(ii)/sqrt(p1(ii)**2+p2(ii)**2+p3(ii)**2).lt.cmin)
     & icc=.false.
      enddo
      if(.not.icc) wt=0.d0
C
C Cut on sum of PT^2 except neutrino_e
C
      pt2=0.
      if(abs(iflav(1)).ne.12.and.(p1(1)**2+p1(2)**2)/
     $ (p1(1)**2+p1(2)**2+p1(3)**2).gt.angarb) pt2=pt2+p1(1)**2+p1(2)**2

      if(abs(iflav(2)).ne.12.and.(p2(1)**2+p2(2)**2)/
     $ (p2(1)**2+p2(2)**2+p2(3)**2).gt.angarb) pt2=pt2+p2(1)**2+p2(2)**2

      if(abs(iflav(3)).ne.12.and.(p3(1)**2+p3(2)**2)/
     $ (p3(1)**2+p3(2)**2+p3(3)**2).gt.angarb) pt2=pt2+p3(1)**2+p3(2)**2

      if(abs(iflav(4)).ne.12.and.(p4(1)**2+p4(2)**2)/
     $ (p4(1)**2+p4(2)**2+p4(3)**2).gt.angarb) pt2=pt2+p4(1)**2+p4(2)**2
C
      if(pt2.lt.spt2) wt=0d0
C
      if(wt.eq.0d0) then
       nkwcte0=nkwcte0+1
       return
      endif
C
c 2. /KWCTE1/ cuts
c ----------------
C
      if(iflav(2).eq.-11.or.iflav(3).eq.11) then
C Cut on sum of invariant mass
       if(qadra(p1,p2)+qadra(p3,p4).lt.scut) wt=0.d0
      endif
C Cut on other sum of masses if eeee
      if(iflav(2).eq.-11.and.iflav(3).eq.11) then
       if(qadra(p1,p4)+qadra(p2,p3).lt.scutee) wt=0.d0
      endif
C
      if(wt.eq.0d0) then
       nkwcte1=nkwcte1+1
       return
      endif
C
c 3. /KWCTE2/ cuts
c ----------------
! this is dirty trick to get rid of events with high p_t photons
! which spoil e+e-xx final states.
! begin =========================
      if(abs(iflav(2)).eq.11.or.abs(iflav(3)).eq.11) then
       pt3=0.d0
       pt3=pt3+qeff1(1)**2+qeff1(2)**2
       pt3=pt3+qeff2(1)**2+qeff2(2)**2
       if(pt3.gt.spt2ee) wt=0.d0
      endif
      if(wt.eq.0d0) then
       nkwcte2=nkwcte2+1
       return
      endif
! end   =========================
c
      if (wt.eq.0d0) return
C
      end
C------------------------------------------------------------------------
      SUBROUTINE INIHER(XPAR)
C
C Routine to initialise HERWIG
C
C common blocks for HERWIG
C
C          ****COMMON BLOCK FILE FOR HERWIG VERSION 5.9****
C
      IMPLICIT NONE
      DOUBLE PRECISION ZERO,ONE,TWO,THREE,FOUR,HALF
      PARAMETER (ZERO =0.D0, ONE =1.D0, TWO =2.D0,
     &           THREE=3.D0, FOUR=4.D0, HALF=0.5D0)
C
      DOUBLE PRECISION
     & ACCUR,AFCH,ALPFAC,ALPHEM,ANOMSC,ASFIXD,AVWGT,B1LIM,BETAF,BRFRAC,
     & BRHIG,BTCLM,CAFAC,CFFAC,CLDKWT,CLMAX,CLPOW,CLQ,CLSMR,CMMOM,COSS,
     & COSTH,CSPEED,CTHRPW,CTMAX,DECPAR,DECWT,DISF,DKLTM,EBEAM1,EBEAM2,
     & EMLST,EMMAX,EMMIN,EMPOW,EMSCA,ENHANC,ENSOF,EPOLN,ETAMIX,EVWGT,
     & EXAG,F0MIX,F1MIX,F2MIX,GAMH,GAMMAX,GAMW,GAMWT,GAMZ,GAMZP,GCOEF,
     & GEV2NB,GEV2MM,GPOLN,H1MIX,HBAR,HARDST,OMEGA0,PBEAM1,PBEAM2,PDIQK,
     & PGSMX,PGSPL,PHEP,PHIMIX,PHIPAR,PHOMAS,PIFAC,PLTCUT,PPAR,PPOLN,
     & PRECO,PRSOF,PSPLT,PTINT,PTMAX,PTMIN,PTPOW,PTRMS,PXRMS,PWT,Q2MAX,
     & Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QCDL3,QCDL5,QCDLAM,QDIQK,QEV,QFCH,QG,
     & QLIM,QSPAC,QV,QWT,REPWT,RESN,RHOHEP,RHOPAR,RLTIM,RMASS,RMIN,
     & RSPIN,SCABI,SINS,SNGWT,SWEIN,SWTEF,SUD,THMAX,TLOUT,TMTOP,TMNISR,
     & TQWT,VCKM,VFCH,VGCUT,VHEP,VMIN2,VPAR,VPCUT,VQCUT,VTXPIP,VTXQDK,
     & WBIGST,WGTMAX,WGTSUM,WHMIN,WSQSUM,XFACT,XLMIN,XMIX,XMRCT,XX,
     & XXMIN,YBMAX,YBMIN,YJMAX,YJMIN,YMIX,YMRCT,YWWMAX,YWWMIN,ZBINM,
     & ZJMAX,ZMXISR
C
      INTEGER
     & CLDIR,IAPHIG,IBRN,IBSH,ICHRG,ICO,IDCMF,IDHEP,IDHW,IDK,IDKPRD,IDN,
     & IDPAR,IDPDG,IERROR,IFLAV,IFLMAX,IFLMIN,IHPRO,IMQDK,INHAD,INTER,
     & IOPDKL,IOPHIG,IOPREM,IPART1,IPART2,IPRINT,IPRO,IPROC,ISLENT,
     & ISPAC,ISTAT,ISTHEP,ISTPAR,JCOPAR,JDAHEP,JDAPAR,JMOHEP,JMOPAR,
     & JNHAD,LNEXT,LOCN,LOCQ,LRSUD,LSTRT,LWEVT,LWSUD,MAPQ,MAXER,MAXEV,
     & MAXFL,MAXPR,MODBOS,MODMAX,MODPDF,NBTRY,NCLDK,NCOLO,NCTRY,NDKYS,
     & NDTRY,NETRY,NEVHEP,NEVPAR,NFLAV,NGSPL,NHEP,NME,NMODES,NMXCDK,
     & NMXDKS,NMXHEP,NMXJET,NMXMOD,NMXPAR,NMXQDK,NMXRES,NMXSUD,NPAR,
     & NPRODS,NQDK,NQEV,NRES,NRN,NSPAC,NSTRU,NSTRY,NSUD,NUMER,NUMERU,
     & NWGTS,NZBIN,SUDORD
C
      LOGICAL
     & AZSOFT,AZSPIN,BGSHAT,BREIT,CLRECO,COLISR,DKPSET,FROST,FSTEVT,
     & FSTWGT,GENEV,GENSOF,HARDME,HVFCEN,MAXDKL,MIXING,NOSPAC,NOWGT,
     & PRNDEC,PIPSMR,PRVTX,RSTAB,SOFTME,TMPAR,TPOL,USECMF,VTOCDK,VTORDK,
     & ZPRIME
C
      real*8 xpar(100)
      CHARACTER*4
     & BDECAY
      CHARACTER*8
     & PART1,PART2,RNAME
      CHARACTER*20
     & AUTPDF
C
C New standard event common
      PARAMETER (NMXHEP=4000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     & JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C
C Beams, process and number of events
      COMMON/HWBEAM/IPART1,IPART2
      COMMON/HWBMCH/PART1,PART2
      COMMON/HWPROC/EBEAM1,EBEAM2,PBEAM1,PBEAM2,IPROC,MAXEV
C
C Basic parameters (and quantities derived from them)
      COMMON/HWPRAM/AFCH(16,2),ALPHEM,B1LIM,BETAF,BTCLM,CAFAC,CFFAC,
     & CLMAX,CLPOW,CLSMR,CSPEED,ENSOF,ETAMIX,F0MIX,F1MIX,F2MIX,GAMH,
     & GAMW,GAMZ,GAMZP,GEV2NB,H1MIX,PDIQK,PGSMX,PGSPL(4),PHIMIX,PIFAC,
     & PRSOF,PSPLT,PTRMS,PXRMS,QCDL3,QCDL5,QCDLAM,QDIQK,QFCH(16),QG,
     & QSPAC,QV,SCABI,SWEIN,TMTOP,VFCH(16,2),VCKM(3,3),VGCUT,VQCUT,
     & VPCUT,ZBINM,IOPREM,IPRINT,ISPAC,LRSUD,LWSUD,MODPDF(2),NBTRY,
     & NCOLO,NCTRY,NDTRY,NETRY,NFLAV,NGSPL,NSTRU,NSTRY,NZBIN,AZSOFT,
     & AZSPIN,CLDIR,HARDME,NOSPAC,PRNDEC,PRVTX,SOFTME,ZPRIME
C
      COMMON/HWPRCH/AUTPDF(2),BDECAY
C
C Parton shower common (same format as /HEPEVT/)
      PARAMETER (NMXPAR=500)
      COMMON/HWPART/NEVPAR,NPAR,ISTPAR(NMXPAR),IDPAR(NMXPAR),
     & JMOPAR(2,NMXPAR),JDAPAR(2,NMXPAR),PPAR(5,NMXPAR),VPAR(4,NMXPAR)
C
C Parton polarization common
      COMMON/HWPARP/DECPAR(2,NMXPAR),PHIPAR(2,NMXPAR),RHOPAR(2,NMXPAR),
     & TMPAR(NMXPAR)
C
C Electroweak boson common
      PARAMETER (MODMAX=5)
      COMMON/HWBOSC/ALPFAC,BRHIG(12),ENHANC(12),GAMMAX,RHOHEP(3,NMXHEP),
     & IOPHIG,MODBOS(MODMAX)
C
C Parton colour common
      COMMON/HWPARC/JCOPAR(4,NMXPAR)
C
C other HERWIG branching, event and hard subprocess common blocks
      COMMON/HWBRCH/ANOMSC(2,2),HARDST,PTINT(3,2),XFACT,INHAD,JNHAD,
     & NSPAC(7),ISLENT,BREIT,FROST,USECMF
C
      COMMON/HWEVNT/AVWGT,EVWGT,GAMWT,TLOUT,WBIGST,WGTMAX,WGTSUM,WSQSUM,
     & IDHW(NMXHEP),IERROR,ISTAT,LWEVT,MAXER,MAXPR,NOWGT,NRN(2),NUMER,
     & NUMERU,NWGTS,GENSOF
C
      COMMON/HWHARD/ASFIXD,CLQ(7,6),COSS,COSTH,CTMAX,DISF(13,2),EMLST,
     & EMMAX,EMMIN,EMPOW,EMSCA,EPOLN(3),GCOEF(7),GPOLN,OMEGA0,PHOMAS,
     & PPOLN(3),PTMAX,PTMIN,PTPOW,Q2MAX,Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QLIM,
     & SINS,THMAX,TMNISR,TQWT,XX(2),XLMIN,XXMIN,YBMAX,YBMIN,YJMAX,
     & YJMIN,YWWMAX,YWWMIN,WHMIN,ZJMAX,ZMXISR,IAPHIG,IBRN(2),IBSH,
     & ICO(10),IDCMF,IDN(10),IFLMAX,IFLMIN,IHPRO,IPRO,MAPQ(6),MAXFL,
     & BGSHAT,COLISR,FSTEVT,FSTWGT,GENEV,HVFCEN,TPOL
C
C Arrays for particle properties (NMXRES = max no of particles defined)
      PARAMETER(NMXRES=400)
      COMMON/HWPROP/RLTIM(0:NMXRES),RMASS(0:NMXRES),RSPIN(0:NMXRES),
     & ICHRG(0:NMXRES),IDPDG(0:NMXRES),IFLAV(0:NMXRES),NRES,
     & VTOCDK(0:NMXRES),VTORDK(0:NMXRES)
C
      COMMON/HWUNAM/RNAME(0:NMXRES)
C
C Arrays for particle decays (NMXDKS = max total no of decays,
C                             NMXMOD = max no of modes for a particle)
      PARAMETER(NMXDKS=4000,NMXMOD=200)
      COMMON/HWUPDT/BRFRAC(NMXDKS),CMMOM(NMXDKS),DKLTM(NMXRES),
     & IDK(NMXDKS),IDKPRD(5,NMXDKS),LNEXT(NMXDKS),LSTRT(NMXRES),NDKYS,
     & NME(NMXDKS),NMODES(NMXRES),NPRODS(NMXDKS),DKPSET,RSTAB(0:NMXRES)
C
C Weights used in cluster decays
      COMMON/HWUWTS/REPWT(0:3,0:4,0:4),SNGWT,DECWT,QWT(3),PWT(12),
     & SWTEF(NMXRES)
C
C Parameters for cluster decays (NMXCDK = max total no of cluster
C                                         decay channels)
      PARAMETER(NMXCDK=4000)
      COMMON/HWUCLU/CLDKWT(NMXCDK),CTHRPW(12,12),PRECO,RESN(12,12),
     & RMIN(12,12),LOCN(12,12),NCLDK(NMXCDK),CLRECO
C
C Variables controling mixing and vertex information
      COMMON/HWDIST/EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,VTXPIP(4),XMIX(2),
     & XMRCT(2),YMIX(2),YMRCT(2),IOPDKL,MAXDKL,MIXING,PIPSMR
C
C Arrays for temporarily storing heavy-b,c-hadrons decaying partonicaly
C (NMXBDK = max no such b-hadron decays in an event)
      PARAMETER (NMXQDK=20)
      COMMON/HWQDKS/VTXQDK(4,NMXQDK),IMQDK(NMXQDK),LOCQ(NMXQDK),NQDK
C
C Parameters for Sudakov form factors
C (NMXSUD= max no of entries in lookup table)
      PARAMETER (NMXSUD=1024)
      COMMON/HWUSUD/ACCUR,QEV(NMXSUD,6),SUD(NMXSUD,6),INTER,NQEV,NSUD,
     & SUDORD
C
      PARAMETER (NMXJET=200)
C also the following include files from the interface
C

      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
      INTEGER JPARGN,JPARNA,JPARCO,JPARMA,JPARCH,JPARLT,JPARMW,JPARAN,
     +          LPARTA
      PARAMETER(JPARGN=1,JPARNA=2,JPARCO=5,JPARMA=6,JPARCH=7,JPARLT=8,
     +          JPARMW=9,JPARAN=10,LPARTA=10)
C some declarations in KMACRO
      INTEGER NAPAR,JPA,NLINK
      REAL PARMAS,TIMLIF
C Flags and counters for the Herwig interface
CBB      INTEGER NCOL
C
CBB      PARAMETER (NCOL = 41)
C
      INTEGER IOUT,IDEBB,IDEBE
CBB      REAL SDVRT,TABL
C
CBB      COMMON / DTMILL / SDVRT(3),TABL(NCOL)
      COMMON / INPOUT / IOUT
      COMMON / DTBUG / IDEBB,IDEBE
C flag for the initial process and whether DIS process
      INTEGER IHARD,IFL,IDIS
C
      COMMON /INIPRO/ IHARD,IFL,IDIS
      character*20 datemod
      parameter (datemod=' June 27th 1997')
C
      CHARACTER*4 CHAINT,CHBDEC(3)
C
      INTEGER NAMIND
      INTEGER DUMMY,NOSOFT
      REAL*4 HWRGET
C
CBB      INTEGER ALTABL,ALRLEP
CBB      EXTERNAL ALTABL,ALRLEP
C
      INTEGER HEDE,IGCO
C
      PARAMETER (IGCO = 5136)
      PARAMETER (HEDE = 52)
C
      INTEGER HEDEC(HEDE)
C Standard variables
      INTEGER IGCOD,NDAT,IMAXER,IPROCI
      INTEGER IPART,IKLIN,IPPART,JGHRW,IQUARK,IIBDEC
      INTEGER JGSPAR,NAMI,JDEBU,JGHRC
      INTEGER JGPAR,JGMAS,MNUM,INPART,IANTI,JGSTA
      INTEGER MXDEC,NROW
      INTEGER KGPART,KNODEC,I,JSVRT,JKPAR
CBB   ,IEBEAM,JRLEP,JANTI
C
C
      REAL RAZSOF,RAZSPI,XTOT
      REAL*8 ECMS
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
C - mass of ALEPH particle# JPA
      PARMAS(JPA) = RTABL(IW(NAPAR),JPA,6)
C - time of life of ALEPH particle# JPA
      TIMLIF(JPA) = RTABL(IW(NAPAR),JPA,8)
C
C
      DATA CHBDEC /'HERW','EURO','CLEO'/
      DATA NAPAR /0/
C
C
      IF (NAPAR.EQ.0) NAPAR = NAMIND('PART')
C
C   Return the generator code as defined in the KINGAL library
C
      IGCOD = IGCO
      IOUT = IW(6)
      WRITE(IOUT,101) IGCOD,DATEMOD
 101  FORMAT(/,10X,
     & 'HRWG10 - Code Number = ',I4,' Last Modifications ',A20
     & ,/,10X,'***********************************************',
     &'********************',//)
C
C initialization
C
      NDAT = 0
      CALL HWIGIN
      DUMMY=HWRGET(NRN)
      IBRN(1)=12348765
      IBRN(2)=0
C
C create the KLIN bank and complete the PART  bank
C
      CALL KXHEPA(IPART,IKLIN)
C
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN
         WRITE (IOUT,'(1X,''+++ASKUSI+++ IPART IKLIN '',2I5)')
     &          IPART,IKLIN
         WRITE(IOUT,'('' ASKUSI error filling PART or KLIN -STOP-'')')
         CALL EXIT
C
      ENDIF
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN
         WRITE (IOUT,'(1X,''+++ASKUSI+++ IPART IKLIN '',2I5)')
     &          IPART,IKLIN
         WRITE(IOUT,'('' ASKUSI error filling PART or KLIN -STOP-'')')
         CALL EXIT
      ENDIF
C Adjust masses when decays have become forbidden
C K0s K0l
       rmass(42)  = rmass(60)
       rmass(50)  = rmass(60)
C F0(980)
       rmass(293) = 0.996
C A0(980)
       rmass(290) = 0.996
C XIb s
       rmass(227) = 5.800
       rmass(228) = rmass(227)
       rmass(251) = rmass(227)
       rmass(252) = rmass(227)
C
      PART1 = 'E-      '
      PART2 = 'E+      '
C
C beam momentum
C
      PBEAM1 = xpar(1)/2.0D0
C
      IHARD = 0
C
C print PART bank per default
C
      IPPART = 1
      iproc = 200
C      hardme = .false.
      modpdf(1)=-1
      modpdf(2)=-1
C
C the default values can be changed by the DATA CARD GHRW
C
      JGHRW = NLINK('GHRW',0)
C
      IF(JGHRW.NE.0) THEN
        NDAT = IW(JGHRW)
C       IF (NDAT.GE.1) PBEAM1 = DBLE(RW(JGHRW+1))
C       IF (NDAT.GE.2) IPROC = IW(JGHRW+2)
        IF (NDAT.GE.3) IPRINT = IW(JGHRW+3)
C       IF (NDAT.GE.4) IHARD = IW(JGHRW+4)
        IF (NDAT.GE.5) IPPART = IW(JGHRW+5)
        IF (NDAT.GE.6) IMAXER = IW(JGHRW+6)
      ENDIF
C
      PBEAM2 = PBEAM1
      ECMS   = PBEAM1 + PBEAM2
C lamda-QCD, Weinberg-angle, width of the Z0
C
      QCDLAM = 0.18D0
      SWEIN = 0.2293D0
      GAMZ = 2.56D0
C
C here the user can change more standard model parameters
C
      JGPAR = NLINK('GGSW',0)
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) QCDLAM = DBLE(RW(JGPAR+1))
        IF (NDAT.GE.2) NFLAV = IW(JGPAR+2)
        IF (NDAT.GE.3) GAMZ = DBLE(RW(JGPAR+3))
        IF (NDAT.GE.4) SWEIN = DBLE(RW(JGPAR+4))
        IF (NDAT.GE.5) SCABI = DBLE(RW(JGPAR+5))
      ENDIF
C
C here the user can change B-decay parameter and choices
C
      JGPAR = NLINK('GBDE',0)
      IIBDEC = 1
C
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) B1LIM = DBLE(RW(JGPAR+1))
        IF (NDAT.GE.2) THEN
          BDECAY = CHAINT(IW(JGPAR+2))
          DO I=3,1,-1
            IF(BDECAY.EQ.CHBDEC(I)) IIBDEC=I
          ENDDO
        ENDIF
      ENDIF
C choose B mixing and mass and width differences
      JGPAR = NLINK('GBMI',0)
C
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) MIXING = IW(JGPAR+1).EQ.1
        IF (NDAT.GE.2) XMIX(1) = DBLE(RW(JGPAR+2))
        IF (NDAT.GE.3) XMIX(2) = DBLE(RW(JGPAR+3))
        IF (NDAT.GE.4) YMIX(1) = DBLE(RW(JGPAR+4))
        IF (NDAT.GE.5) YMIX(2) = DBLE(RW(JGPAR+5))
      ENDIF
C
C Here the user can set the max lifetime for an unstable particle or
C if it outside a specified volume
      JGPAR = NLINK('GMLT',0)
      MAXDKL=.FALSE.
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF(NDAT.GE.1) PLTCUT = DBLE(RW(JGPAR+1))
        IF(NDAT.GE.2) MAXDKL = IW(JGPAR+2).EQ.1
        IF(NDAT.GE.3) PRVTX  = IW(JGPAR+3).EQ.1
      ENDIF
C here the user can change the most important
C parameters for the cluster fragmentation
C
      JGPAR = NLINK('GPRM',0)
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) CLMAX = DBLE(RW(JGPAR+1))
        IF (NDAT.GE.2) CLPOW = DBLE(RW(JGPAR+2))
        IF (NDAT.GE.3) PSPLT = DBLE(RW(JGPAR+3))
        IF (NDAT.GE.4) THMAX = DBLE(RW(JGPAR+4))
        IF (NDAT.GE.5) VQCUT = DBLE(RW(JGPAR+5))
        IF (NDAT.GE.6) VGCUT = DBLE(RW(JGPAR+6))
        IF (NDAT.GE.7) QDIQK = DBLE(RW(JGPAR+7))
        IF (NDAT.GE.8) PDIQK = DBLE(RW(JGPAR+8))
        IF (NDAT.GE.9) IOPREM = IW(JGPAR+9)
      ENDIF
C
C Now some further parameters controlling the program
C
      RAZSOF = 1.
      RAZSPI = 1.
      JGPAR = NLINK('GHRC',0)
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) THEN
          IF (RW(JGPAR+1).EQ.0.) THEN
            AZSOFT = .FALSE.
            RAZSOF = 0.D0
          ENDIF
        ENDIF
        IF (NDAT.GE.2) THEN
          IF (RW(JGPAR).EQ.0.) THEN
            AZSPIN = .FALSE.
            RAZSPI = 0.D0
          ENDIF
        ENDIF
        IF (NDAT.GE.3) CLDIR = IW(JGPAR+3)
        IF (NDAT.GE.4) CLSMR = DBLE(RW(JGPAR+4))
        IF (NDAT.GE.5) BTCLM = DBLE(RW(JGPAR+5))
        IF (NDAT.GE.6) ETAMIX = DBLE(RW(JGPAR+6))
        IF (NDAT.GE.7) PRSOF  = DBLE(RW(JGPAR+7))
      ENDIF
C
C change here the parameters governing final state radiation
C
      JGPAR = NLINK('GFSR',0)
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) VPCUT = DBLE(RW(JGPAR+1))
        IF (NDAT.GE.2) ALPFAC = DBLE(RW(JGPAR+2))
      ENDIF
C
C change here the parameters governing initial state radiation
C
CBB      JGPAR = NLINK('GISR',0)
C      IF(JGPAR.NE.0) THEN
C        NDAT = IW(JGPAR)
C        IF (NDAT.GE.1) TMNISR = DBLE(RW(JGPAR+1))
C        IF (NDAT.GE.2) ZMXISR = DBLE(RW(JGPAR+2))
C        IF (NDAT.GE.3) COLISR = IW(JGPAR+3).eq.1
CBB      ENDIF
C
C Sudakov form factor options
C
      LWSUD = 0
      LRSUD = 0
      JGPAR = NLINK('GSUD',0)
C
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) SUDORD = IW(JGPAR+1)
        IF (NDAT.GE.2) LWSUD  = IW(JGPAR+2)
        IF (NDAT.GE.3) LRSUD  = IW(JGPAR+3)
      ENDIF
C
C W and Z decay options
C
CBB      JGPAR = NLINK('GHMB',0)
C      IF(JGPAR.NE.0) THEN
C        NDAT = IW(JGPAR)
C        IF(NDAT.GE.1) THEN
C          DO I=1,NDAT
C            MODBOS(I) = IW(JGPAR+I)
C          ENDDO
C        ENDIF
CBB      ENDIF
C
C Allow some relative weight parameters to vary
C
      JGPAR = NLINK('GHWT',0)
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) SNGWT = DBLE(RW(JGPAR+1))
        IF (NDAT.GE.2) DECWT = DBLE(RW(JGPAR+2))
        IF (NDAT.GE.3) REPWT(0,1,0) = DBLE(RW(JGPAR+3))
        IF (NDAT.GE.4) REPWT(0,2,0) = DBLE(RW(JGPAR+4))
        IF(NDAT.GT.12) NDAT=12
        IF(NDAT.GT.4) THEN
          DO I=5,NDAT
            PWT(I-4) = DBLE(RW(JGPAR+I))
          ENDDO
        ENDIF
      ENDIF
C
C Colour rearrangement model
C
      JGPAR = NLINK('GCLR',0)
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) CLRECO = IW(JGPAR+1).EQ.1
        IF (NDAT.GE.2) PRECO  = DBLE(RW(JGPAR+2))
        IF (NDAT.GE.3) EXAG   = DBLE(RW(JGPAR+3))
      ENDIF
C finally we redefine some steering parameters of the
C HERWIG program:
C don't print out event listings
C increase number of possible HERWIG errors
C
      MAXPR = 0
      MAXER = 100
C
      IDEBB = 0
      IDEBE = 0
C
C switch off time check in HERWIG
C
      TLOUT = 0.D0
C
C the user can debug specific events with the
C data card DEBU
C
      NAMI = NAMIND('DEBU')
C
      JDEBU = IW(NAMI)
C
      IF(JDEBU.NE.0) THEN
C
        NDAT = IW(JDEBU)
C
        IF (NDAT.EQ.1) THEN
          IDEBB = IW(JDEBU+1)
          IDEBE = IDEBB
        ENDIF
C
        IF (NDAT.EQ.2) THEN
          IDEBB = IW(JDEBU+1)
          IDEBE = IW(JDEBU+2)
        ENDIF
C
      ENDIF
C
C user can reset parameters at
C this point by data cards, otherwise values
C set in HWIGIN/HWUINC will be used.
C
C stop smearing of primary vertex (done in ASKUSE)
      PIPSMR = .FALSE.
C
C the user can define different values with the data cards GMAS
C the masses of the t-mesons, t-baryons and the diquarks should
C be redefined after the call to HWUINC
C
      NAMI = NAMIND ('GMAS')
C
      IF (IW(NAMI).NE.0) THEN
C
        JGMAS = NAMI + 1
C
C loop over all GMAS banks
C
   5    JGMAS = IW(JGMAS-1)
        IF (JGMAS.EQ. 0) GOTO 6
C
        MNUM = IW(JGMAS-2)
        IF ((MNUM.LT.109.OR.MNUM.GT.120).AND.
     &      (MNUM.LT.232.OR.MNUM.GT.244).AND.
     &      (MNUM.LT.255.OR.MNUM.GT.264)) THEN
          RMASS(MNUM) = RW(JGMAS+1)
          INPART = IW(NAPAR)
          IPART = KGPART(MNUM)
          IANTI = ITABL(INPART,IPART,JPARAN)
          RW(KROW(INPART,IPART)+JPARMA) = RW(JGMAS+1)
          IF (IANTI.NE.IPART) RW(KROW(INPART,IANTI)+JPARMA)=RW(JGMAS+1)
        ENDIF
        GOTO 5
C
   6    CONTINUE
C
      ENDIF
C
C create the KLIN bank and complete the PART  bank
C
C compute parameter-dependent constants
C HWUINC overwrites the masses of the t-mesons, t-baryons
C and the diquarks
C
      rmass(42)  = rmass(60) + 0.00000
      rmass(50)  = rmass(60) + 0.00000

      CALL HWUINC
C
C reset the overwritten masses back to the ALEPH values
C (if available)
C
      RMASS(232) = DBLE(PARMAS(140))
      RMASS(233) = DBLE(PARMAS(133))
      RMASS(234) = DBLE(PARMAS(131))
      RMASS(235) = DBLE(PARMAS(135))
      RMASS(236) = DBLE(PARMAS(239))
      RMASS(237) = DBLE(PARMAS(271))
      RMASS(238) = DBLE(PARMAS(243))
C
      RMASS(241) = DBLE(PARMAS(249))
      RMASS(242) = DBLE(PARMAS(137))
      RMASS(243) = DBLE(PARMAS(139))
C
      RMASS(255) = DBLE(PARMAS(134))
      RMASS(256) = DBLE(PARMAS(132))
      RMASS(257) = DBLE(PARMAS(136))
      RMASS(258) = DBLE(PARMAS(240))
      RMASS(259) = DBLE(PARMAS(272))
      RMASS(260) = DBLE(PARMAS(244))
C
      RMASS(263) = DBLE(PARMAS(250))
      RMASS(264) = DBLE(PARMAS(138))
C
C now we we have to set the user defined masses of the
C t-mesons, t-baryons and diquarks
C
      NAMI = NAMIND ('GMAS')
C
      IF (IW(NAMI).NE.0) THEN
C
        JGMAS = NAMI + 1
C
C loop over all GMAS banks
C
  15    JGMAS = IW(JGMAS-1)
        IF (JGMAS.EQ. 0) GOTO 16
C
        MNUM = IW(JGMAS-2)
        IF ((MNUM.GE.109.AND.MNUM.LE.120).OR.
     &      (MNUM.GE.232.AND.MNUM.LE.244).OR.
     &      (MNUM.GE.255.AND.MNUM.LE.264)) THEN
          RMASS(MNUM) = RW(JGMAS+1)
          INPART = IW(NAPAR)
          IPART = KGPART(MNUM)
          RW(KROW(INPART,IPART)+JPARMA) = RW(JGMAS+1)
        ENDIF
        GOTO 15
C
  16    CONTINUE
C now rest Z amd W masses
      rmass(200) = xpar(4)
      rmass(198) = xpar(6)
      rmass(199) = xpar(6)
C
      ENDIF
C
C the user can set any particle stable
C with the data cards GSTA
C
      NAMI = NAMIND ('GSTA')
C
      IF (IW(NAMI).NE.0) THEN
C
        JGSTA = NAMI + 1
C
C loop over all GSTA banks
C
 105    JGSTA = IW(JGSTA-1)
        IF (JGSTA.EQ. 0) GOTO 106
C
        MNUM = IW(JGSTA-2)
C
        IF (CHAINT(IW(JGSTA+1)).EQ.'OFF'.AND.NMODES(MNUM).NE.0)
     &    CALL HWUSTA(RNAME(MNUM))
        IF (CHAINT(IW(JGSTA+1)).EQ.'ON'.AND.NMODES(MNUM).EQ.0)
     &    WRITE(IOUT,1001)MNUM
 1001     FORMAT(1X,'+++ASKUSI+++ Herwig particle # = ',I5,
     &           ' no decay modes available - left stable')
C
        GOTO 105
C
 106    CONTINUE
C
      ENDIF
C
C get particles which should not be decayed by the generator
C
      MXDEC = KNODEC (HEDEC,HEDE)
      MXDEC = MIN (MXDEC,HEDE)
C
C now set them stable, unless the user has provided a data
C card for them
C
      DO 200 IPART = 1,MXDEC
C
C look if data card is present
C
         JGSTA = NLINK('GSTA',HEDEC(IPART))
C
C if not, inhibit decay
C
         IF (JGSTA.LE.0) THEN
           IF (HEDEC(IPART).GT.0.AND.NMODES(HEDEC(IPART)).NE.0)
     &       CALL HWUSTA(RNAME(HEDEC(IPART)))
         ENDIF
C
  200 CONTINUE
C
C user's initial calculations, redefine any other parameter in HERWIG
C
      CALL USTART
C
C  Print PART and KLIN bank
C
      IF (IPPART.EQ.1) CALL PRPART
C
      RETURN
      END
      SUBROUTINE TOHAD(IFHDM,IFHDP)
      implicit none
C AST - modified to interface with herwig
C AST - note use of 'standard' hepevt
C BB  - add calls to create the KSHO bank
      integer ijoin(2),IFHDM,IFHDP
      integer iflav(4),ireco,id1,id2
      real*8 sqrs1,sqrs2
      double precision  amdec(4), brrat(2), brel
      common / decays / iflav, amdec, brrat, brel
! This common can be everywhere, contains various switches
      INTEGER  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp,Key4f
      COMMON / KeyKey /  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
      INTEGER JAK1,JAK2,ITDKRC,IFPHOT,IFHADM,IFHADP,ifhrwg
      COMMON / LIBRA  / JAK1,JAK2,ITDKRC,IFPHOT,IFHADM,IFHADP
      COMMON/IHRWG/ifhrwg
C
      integer IDEBB,IDEBE
      COMMON / DTBUG / IDEBB,IDEBE
      REAL*8 ASFIXD,CLQ,COSS,COSTH,CTMAX,DISF,EMLST,
     & EMMAX,EMMIN,EMPOW,EMSCA,EPOLN,GCOEF,GPOLN,OMEGA0,PHOMAS,
     & PPOLN,PTMAX,PTMIN,PTPOW,Q2MAX,Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QLIM,
     & SINS,THMAX,TMNISR,TQWT,XX,XLMIN,XXMIN,YBMAX,YBMIN,YJMAX,
     & YJMIN,YWWMAX,YWWMIN,WHMIN,ZJMAX,ZMXISR,
     & BGSHAT,COLISR,FSTEVT,FSTWGT,GENEV,HVFCEN,TPOL
C
C
      INTEGER IAPHIG,IBRN,IBSH,
     & ICO,IDCMF,IDN,IFLMAX,IFLMIN,IHPRO,IPRO,MAPQ,MAXFL
C
      COMMON/HWHARD/ASFIXD,CLQ(7,6),COSS,COSTH,CTMAX,DISF(13,2),EMLST,
     & EMMAX,EMMIN,EMPOW,EMSCA,EPOLN(3),GCOEF(7),GPOLN,OMEGA0,PHOMAS,
     & PPOLN(3),PTMAX,PTMIN,PTPOW,Q2MAX,Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QLIM,
     & SINS,THMAX,TMNISR,TQWT,XX(2),XLMIN,XXMIN,YBMAX,YBMIN,YJMAX,
     & YJMIN,YWWMAX,YWWMIN,WHMIN,ZJMAX,ZMXISR,IAPHIG,IBRN(2),IBSH,
     & ICO(10),IDCMF,IDN(10),IFLMAX,IFLMIN,IHPRO,IPRO,MAPQ(6),MAXFL,
     & BGSHAT,COLISR,FSTEVT,FSTWGT,GENEV,HVFCEN,TPOL
C
      INTEGER  NMXHEP,NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP
      real*8 phep,vhep
      PARAMETER (NMXHEP=4000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C
      integer ish,icr,ksho,kp6sho
      external kp6sho
C
      real*4 amarset,amar
      external amarset
C
      if (ifhadm.ne.0 .or. ifhadp.ne.0) then
        IRECO=0
!--------------------------
        Key4f  = MOD(KeyMis,100)/10
        if (key4f.ne.0) then
!--- routine spdetx can make a random choice of two colour recombination
!--- pattern either WW or ZZ like. It is necessary only for uudd ccss f.
          if ( (abs(iflav(1)).eq.1).and.(abs(iflav(4)).eq.1)
     $    .and.(abs(iflav(2)).eq.2).and.(abs(iflav(3)).eq.2)
     $       )  call spdetx(ireco)
          if ( (abs(iflav(1)).eq.3).and.(abs(iflav(4)).eq.3)
     $    .and.(abs(iflav(2)).eq.4).and.(abs(iflav(3)).eq.4)
     $       )  call spdetx(ireco)
        endif

!--------------------
C switch to another random 
        amar=amarset(1)
C
C lets first go to LUND and later hadronize
C
        if(ifhrwg.eq.0) then
          call pyhepc(2)

          DO 7 ID1=5,NHEP
            IF (IDHEP(ID1).NE.22) THEN
              goto 8
            ENDIF
  7       CONTINUE
  8       CONTINUE
          DO 17 ID2=ID1+2,NHEP
            IF (IDHEP(ID2).NE.22) THEN
              goto 18
            ENDIF
  17      CONTINUE
  18      CONTINUE

          ish = 0
          IF(IRECO.eq.0) then
            ICR = 0
            if (abs(iflav(1)).lt.10) then
              ijoin(1) = ID1
              ijoin(2) = ID1+1
              call pyjoin(2,ijoin)
              ish = ish + 1
              ksho = KP6SHO(ISH,NHEP+1,ID1,ID1+1,ICR)
            endif
            if (abs(iflav(3)).lt.10) then
              ijoin(1) = ID2
              ijoin(2) = ID2+1
              call pyjoin(2,ijoin)
              ish = ish + 1
              ksho = KP6SHO(ISH,NHEP+2,ID2,ID2+1,ICR)
            endif
C That's it, shower and hadronize now
            SQRS1=PHEP(5,JMOHEP(1,ID1))
            IF (ABS(IFLAV(1)).LT.10) CALL pySHOW(ID1,ID1+1,SQRS1)
            SQRS2=PHEP(5,JMOHEP(1,ID2))
            IF (ABS(IFLAV(3)).LT.10) CALL pySHOW(ID2,ID2+1,SQRS2)
            call pyexec
          else
            ICR = 1
            ijoin(1) = ID1
            ijoin(2) = ID2+1
            call pyjoin(2,ijoin)
              ish = ish + 1
              ksho = KP6SHO(ISH,NHEP+1,ID1,ID2+1,ICR)

            ijoin(1) = ID1+1
            ijoin(2) = ID2
            call pyjoin(2,ijoin)
              ish = ish + 1
              ksho = KP6SHO(ISH,NHEP+2,ID1+1,ID2,ICR)

C That's it, shower and hadronize now

            SQRS1=(PHEP(4,ID1)+PHEP(4,ID2+1))**2
     $       -(PHEP(3,ID1)+PHEP(3,ID2+1))**2
     $       -(PHEP(2,ID1)+PHEP(2,ID2+1))**2
     $       -(PHEP(1,ID1)+PHEP(1,ID2+1))**2
            sqrs1=sqrt(abs(sqrs1))
            CALL pySHOW(ID1,ID2+1,SQRS1)
            SQRS2=(PHEP(4,ID1+1)+PHEP(4,ID2))**2
     $       -(PHEP(3,ID1+1)+PHEP(3,ID2))**2
     $       -(PHEP(2,ID1+1)+PHEP(2,ID2))**2
     $       -(PHEP(1,ID1+1)+PHEP(1,ID2))**2
            sqrs2=sqrt(abs(sqrs2))
            CALL pySHOW(ID1+1,ID2,SQRS2)
            call pyexec

          endif
        else ! herwig
          EMSCA = PHEP(5,3)
          DO  ID1=5,NHEP
            IF (ISTHEP(ID1).EQ.113) THEN
              goto 28
            ENDIF
          ENDDO
  28      CONTINUE
          DO ID2=ID1+2,NHEP
            IF (ISTHEP(ID2).EQ.113) THEN
              goto 38
            ENDIF
          ENDDO
  38      CONTINUE
          IF(IRECO.EQ.0) THEN
              JDAHEP(2,ID1) = ID1 + 1
              JDAHEP(2,ID1+1) = ID1
              JDAHEP(2,ID2) = ID2 + 1
              JDAHEP(2,ID2+1) = ID2
              ICR=0
              ish = ish + 1
              ksho = KP6SHO(ISH,NHEP+1,ID1,ID1+1,ICR)
              ish = ish + 1
              ksho = KP6SHO(ISH,NHEP+2,ID2,ID2+1,ICR)
          ELSE
C AST colour reconnect - not sure if this is valid
              JDAHEP(2,ID1) = ID2 + 1
              JDAHEP(2,ID1+1) = ID2
              JDAHEP(2,ID2) = ID1 + 1
              JDAHEP(2,ID2+1) = ID1
              ICR=1
              ish = ish + 1
              ksho = KP6SHO(ISH,NHEP+1,ID1,ID2+1,ICR)
              ish = ish + 1
              ksho = KP6SHO(ISH,NHEP+2,ID2,ID1+1,ICR)
          ENDIF
C AST OK let herwig do the business
C
C generate parton cascades
C
          call hwuepr
          CALL HWBGEN
C          call hwuepr
C
C do heavy quark decays
C
          CALL HWDHQK
C          call hwuepr
C
C do cluster formation
C
          CALL HWCFOR
C          call hwuepr
C
C do cluster decays
C
          CALL HWCDEC
C          call hwuepr
C
C do unstable particle decays
C
          CALL HWDHAD
C          call hwuepr
C
C do heavy flavour decays
C
          CALL HWDHVY
C          call hwuepr
C
C add soft underlying event if required
C
          CALL HWMEVT
C          call hwuepr
C
C finish event
C
          CALL HWUFNE
C          call hwuepr
C
        endif
      endif
      end
      integer function KP6SHO(ISH,ISHO,IP1,IP2,ICR)
C--------------------------------------------------------------------
C!  BOOK and fill bank KSHO with fragmentation info
C      B. Bloch -Devaux November 1997
C     structure : integer function
C
C     input     : ISH  index of shower in event (1,2,...)
C                 ISHO current line number of shower  in LUJETS common
C                 IP1  current line number of parton1 in LUJETS common
C                 IP2  current line number of parton2 in LUJETS common
C                 ICR  Color reconnection flag I= CR happened in scheme I
C                                              0= No CR happened
C
C     output    : index of KSHO bank ( should be >0 if OK)
C                 KSHO bank is written to Event list
C     comdecks referenced : LUN7COM
C--------------------------------------------------------------------
C#ifndef DOC
C#include "pyt6com.h"
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
      COMMON /PYDAT3/MDCY(L2PAR,3),MDME(LJNPAR,2),BRAT(LJNPAR),
     &               KFDP(LJNPAR,5)
      COMMON/PYDAT4/CHAF(L2PAR,2)
      CHARACTER CHAF*16
C
      COMMON/PYSUBS/MSEL,MSELPD,MSUB(500),KFIN(2,-40:40),CKIN(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
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
C#include "bcs.h"

      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      INTEGER IBSH(4)
Cinclude "kshojj.h"
      INTEGER JKSHKI,JKSHK1,JKSHK2,JKSHCR, LKSHOA
      PARAMETER(JKSHKI=1,JKSHK1=2,JKSHK2=3,JKSHCR=4,LKSHOA=4)
C#include "bmacro.h"
C
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
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
      KP6SHO = -1
C       remove beam particle from numbering if needed
      IBEA = 0
      DO 27 ITR=1,N7LU
        KS = K7LU(ITR,1)
        IF ( KS.EQ.21 .AND. ABS(K7LU(ITR,2)).EQ.11 ) IBEA = IBEA +1
 27   CONTINUE
      IBSH(1) = ISHO - IBEA
      IBSH(2) = IP1  - IBEA
      IBSH(3) = IP2  - IBEA
      IBSH(4) = ICR
C   Get KSHO index
      JKSHO = IW(NAMIND('KSHO'))
      IF ( JKSHO.LE.0) THEN
C   Create KSHO bank
         CALL AUBOS('KSHO',0,LKSHOA+LMHLEN,JKSHO,IGARB)
         IF ( JKSHO.LE.0) GO TO 999
         IW(JKSHO+LMHCOL) = LKSHOA
         IW(JKSHO+LMHROW) = 1
         CALL BKFMT('KSHO','I')
         CALL BLIST(IW,'E+','KSHO')
      ELSE
C  KSHO EXISTS, TEST THE LENGTH AND EXTEND IF NEEDED
         NKSHO=LROWS(JKSHO)
         IF ( ISH.GT.NKSHO) THEN
           CALL AUBOS('KSHO',0,LKSHOA*ISH+LMHLEN,JKSHO,IGARB)
           IF ( JKSHO.LE.0) THEN
              KSHOBK= -ISH
              GO TO 999
           ELSE
              IW(JKSHO+LMHROW) = ISH
           ENDIF
         ENDIF
      ENDIF
C  Fill KSHO BANK
      KKSHO = KROW(JKSHO,ISH)
      DO 10 II=1,4
         IW(KKSHO+II) = IBSH(II)
  10  CONTINUE
      KP6SHO = JKSHO
C
 999  RETURN
      END

