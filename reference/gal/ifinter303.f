C*HE 06/09/95 12:14:38 C,D                                                      
C*DK GALEPH                                                                     
C*DF INTER                                                                      
      PROGRAM GALEPH                                                            
C ----------------------------------------------------------------------        
C! MonteCarlo main routine                                                      
C. - F.RANJARD - 850222                                                         
C*CA VERSION                                                                    
C - GALEPH   30.3   950928  18:17:44                                            
      PARAMETER (GALVER=30.3)                                                   
      PARAMETER (CORVER=0.0)                                                    
C                                                                               
C*IF DOC                                                                        
C*CC VERSION                                                                    
C ---------------------------------------------------------------------         
C*IF .NOT.DOC                                                                   
C*IF INTER                                                                      
      PARAMETER (LGB = 920000, LHB = 50000)                                     
C*EL                                                                            
C*EI                                                                            
      COMMON /PAWC/   HB(LHB)                                                   
      COMMON /GCBANK/   GB(LGB)                                                 
C                                                                               
C                                                                               
C ----------------------------------------------------------------------        
C*IF INTER                                                                      
      CALL GPAW (LGB,LHB)                                                       
C*EL                                                                            
C*EI                                                                            
      STOP                                                                      
      END                                                                       
C*EI                                                                            
C*DK ASCEVE                                                                     
C*DF INTER                                                                      
      SUBROUTINE ASCEVE                                                         
C ----------------------------------------------------------------------        
C. - F.RANJARD - 850328                 modified - 861009                       
C! Close the current event                                                      
C.   do analysis                                                                
C.   save event on tape                                                         
C.   drop BOS banks and release space                                           
C. - called by    ASPRUN                      from this .HLB                    
C. - calls        USCEVE, ASEVST, ASWRTP      from this .HLB                    
C.                RDMOUT                      from KERNLIB                      
C---------------------------------------------------------------                
C*IF .NOT.DOC                                                                   
      SAVE                                                                      
C*CA JQCOM                                                                      
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
C*IF DOC                                                                        
C*EI                                                                            
C*CC JQCOM                                                                      
C*CA IOCOM                                                                      
      PARAMETER (LFIL=6)                                                        
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO                
      DIMENSION LUNIIO(LFIL)                                                    
      EQUIVALENCE (LUNIIO(1),LGETIO)                                            
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)                                
      CHARACTER TFILIO*60, TFORIO*4                                             
C                                                                               
C*IF DOC                                                                        
C*CC IOCOM                                                                      
C*CA JOBCOM                                                                     
      PARAMETER (LOFFMC = 1000)                                                 
      PARAMETER (LHIS=20, LPRI=20, LTIM=6, LPRO=6, LRND=3)                      
      PARAMETER (LBIN=20, LST1=LBIN+3, LST2=3)                                  
      PARAMETER (LSET=15, LTCUT=5, LKINP=20)                                    
      PARAMETER (LDET=9,  LGEO=LDET+4, LBGE=LGEO+5)                             
      PARAMETER (LCVD=10, LCIT=10, LCTP=10, LCEC=15, LCHC=10, LCMU=10)          
      PARAMETER (LCLC=10, LCSA=10, LCSI=10)                                     
      COMMON /JOBCOM/   JDATJO,JTIMJO,VERSJO                                    
     &                 ,NEVTJO,NRNDJO(LRND),FDEBJO,FDISJO                       
     &                 ,FBEGJO(LDET),TIMEJO(LTIM),NSTAJO(LST1,LST2)             
     &                 ,IDB1JO,IDB2JO,IDB3JO,IDS1JO,IDS2JO                      
     &                 ,MBINJO(LST2),MHISJO,FHISJO(LHIS)                        
     &                 ,IRNDJO(LRND,LPRO)                                       
     &                 ,IPRIJO(LPRI),MSETJO,IRUNJO,IEXPJO,AVERJO                
     3                 ,MPROJO,IPROJO(LPRO),MGETJO,MSAVJO,TIMLJO,IDATJO         
     5                 ,TCUTJO(LTCUT),IBREJO,NKINJO,BKINJO(LKINP),IPACJO        
     6                 ,IDETJO(LDET),IGEOJO(LGEO),LVELJO(LGEO)                  
     7                 ,ICVDJO(LCVD),ICITJO(LCIT),ICTPJO(LCTP)                  
     8                 ,ICECJO(LCEC),ICHCJO(LCHC),ICLCJO(LCLC)                  
     9                 ,ICSAJO(LCSA),ICMUJO(LCMU),ICSIJO(LCSI)                  
     &                 ,FGALJO,FPARJO,FXXXJO,FWRDJO,FXTKJO,FXSHJO,CUTFJO        
     &                 ,IDAFJO,IDCHJO,TVERJO                                    
      LOGICAL FDEBJO,FDISJO,FHISJO,FBEGJO,FGALJO,FPARJO,FXXXJO,FWRDJO           
     &       ,FXTKJO,FXSHJO                                                     
      COMMON /JOBKAR/   TITLJO,TSETJO(LSET),TPROJO(LPRO)                        
     1                 ,TKINJO,TGEOJO(LBGE),TRUNJO                              
      CHARACTER TRUNJO*60                                                       
      CHARACTER*4 TKINJO,TPROJO,TSETJO,TITLJO*40                                
      CHARACTER*2 TGEOJO                                                        
C                                                                               
      PARAMETER (LERR=20)                                                       
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)                              
      COMMON /JOBCAR/   TACTJO                                                  
      CHARACTER*6 TACTJO                                                        
C                                                                               
C*IF DOC                                                                        
C*EI                                                                            
C*IF DOC                                                                        
C*EI                                                                            
C*CC JOBCOM                                                                     
C*CA AFIDJJ,EVEHJJ                                                              
      PARAMETER(JAFIAR=1,JAFIAZ=2,JAFIMF=3,JAFIBE=4,LAFIDA=4)                   
      PARAMETER(JEVEEN=1,JEVERN=2,JEVERT=3,JEVEDA=4,JEVETI=5,JEVEEV=6,          
     +          JEVEM1=7,JEVEM2=8,JEVEM3=9,JEVEM4=10,JEVETY=11,                 
     +          JEVEES=12,JEVETE=13,LEVEHA=13)                                  
C*CC AFIDJJ,EVEHJJ                                                              
      CHARACTER*4 NAME,NLIST                                                    
      DATA IFI /0/                                                              
C*CA BMACRO                                                                     
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
C*CC BMACRO                                                                     
C ----------------------------------------------------------------------        
C                                                                               
C - Call USER routine                                                           
      CALL USCEVE                                                               
C                                                                               
C - Build event statistics bank 'ASEV'                                          
      CALL ASEVST                                                               
C                                                                               
C - Update AFID beam energy with EVEH beam energy at 1st entry                  
      IF (IFI .EQ. 0) THEN                                                      
        IF(IW(NAMIND('RLEP')) .EQ. 0) THEN                                      
          JAFID = IW(NAMIND('AFID'))                                            
          IF (JAFID .GT. 0) THEN                                                
            JEVEH = IW(NAEVEH)                                                  
            IF (JEVEH .GT. 0) THEN                                              
              RW(JAFID+LMHLEN+JAFIBE) = REAL(IW(JEVEH+JEVETE))*1.E-6            
            ENDIF                                                               
          ENDIF                                                                 
        ENDIF                                                                   
        IFI = 1                                                                 
      ENDIF                                                                     
C                                                                               
C Produce friendly Fxxx DST if asked for.                                       
      IF (FXXXJO) THEN                                                          
        CALL ASFXXX                                                             
        IF (FDEBJO .AND. IPRIJO(16).EQ.1) CALL PRFKIN                           
      ENDIF                                                                     
C                                                                               
C - Save event onto unit # LSAVIO if required                                   
C                                                                               
      IF (MSAVJO.NE.0) CALL ASWRTP ('E')                                        
C                                                                               
      IF (FDEBJO .AND. IPRIJO(14).EQ.1) CALL AUBLIS ('E')                       
C                                                                               
C*IF DRAW                                                                       
C*EI                                                                            
C                                                                               
C - Save last random number which will be the root of the next evt.             
      CALL RDMOUT (NRNDJO(1))                                                   
      CALL VZERO (IRNDJO,LPRO*LRND)                                             
C                                                                               
C - Debug                                                                       
C                                                                               
      IF (FDEBJO .OR. KERRJO.NE.0) THEN                                         
        CALL TIMAD (TIMEJO(6))                                                  
        TIMEJO(4) = TIMEJO(4) + TIMEJO(6)                                       
        WRITE (LOUTIO,801) NEVTJO,TIMEJO(4),TIMEJO(6)                           
  801   FORMAT (/1X,'+++ASCEVE+++ event# ',I5,'  total time spent = '           
     &             ,F9.3,' sec , during digitization + trigger = '              
     &             ,F9.3,' sec')                                                
        TIMEJO(4) = 0.                                                          
      ENDIF                                                                     
C                                                                               
      RETURN                                                                    
      END                                                                       
C*DK ASIEVE                                                                     
C*DF INTER                                                                      
      SUBROUTINE ASIEVE                                                         
C ----------------------------------------------------------------------        
C. - F.RANJARD - 850328                                                         
C! event initialization                                                         
C. - Increment event # .                                                        
C. - Reset all flags, variables and banks for this event                        
C. - Reset random generator if required.                                        
C. - Set DEBUG flag.                                                            
C. - Initialize ZBOOK event partition .                                         
C                                                                               
C. - modified by : F.Ranjard - 911002                                           
C                  supress reference to LMOD which is replaced by LDET          
C                  in the definition of FBEGJO                                  
C                                                                               
C. - called from    ASPRUN                             from this .HLB           
C. - calls          RDMIN, RDMOUT, TIMAD, TIMAL        from KERNLIB             
C.                  MZBOOK                             from ZEBRA lib           
C.                  ASREVE                             from this .HLB           
C.                  ABRUEV                             from ALEPHLIB            
C -----------------------------------------------------------------------       
C*IF .NOT.DOC                                                                   
C*CA GCBANK                                                                     
      PARAMETER (KGWBK=69000,KGWRK=5200)                                        
      COMMON /GCBANK/   NGZEBR,GVERSN,GZVERS,IGXSTO,IGXDIV,IGXCON,              
     &                  GFENDQ(16),LGMAIN,LGR1,GWS(KGWBK)                       
      DIMENSION IGB(1),GB(1),LGB(8000),IGWS(1)                                  
      EQUIVALENCE (GB(1),IGB(1),LGB(9)),(LGB(1),LGMAIN),(IGWS(1),GWS(1))        
C                                                                               
      COMMON/GCLINK/JGDIGI,JGDRAW,JGHEAD,JGHITS,JGKINE,JGMATE,JGPART            
     +        ,JGROTM,JGRUN,JGSET,JGSTAK,JGGSTA,JGTMED,JGTRAC,JGVERT            
     +        ,JGVOLU,JGXYZ,JGPAR,JGPAR2,JGSKLT                                 
C                                                                               
C*CC GCBANK                                                                     
C*CA GCFLAG                                                                     
      COMMON/GCFLAG/IGDEBU,IGDEMI,IGDEMA,IGTEST ,IGDRUN ,IGDEVT                 
     +             ,IGEORU,IGEOTR,IGEVEN                                        
     +             ,IGSWIT(10),IGFINI(20),NGEVEN,NGRND(2)                       
C                                                                               
C*CC GCFLAG                                                                     
C*CA GCNUM                                                                      
      COMMON/GCNUM/NGMATE,NGVOLU,NGROTM,NGTMED,NGTMUL,NGTRAC,NGPART             
     +     ,NGSTMA,NGVERT,NGHEAD,NGBIT                                          
      COMMON/GCNUMX/ NGALIV,NGTMST                                              
C                                                                               
C*CC GCNUM                                                                      
C*CA JQCOM                                                                      
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
C*CC JQCOM                                                                      
C*CA JOBCOM                                                                     
      PARAMETER (LOFFMC = 1000)                                                 
      PARAMETER (LHIS=20, LPRI=20, LTIM=6, LPRO=6, LRND=3)                      
      PARAMETER (LBIN=20, LST1=LBIN+3, LST2=3)                                  
      PARAMETER (LSET=15, LTCUT=5, LKINP=20)                                    
      PARAMETER (LDET=9,  LGEO=LDET+4, LBGE=LGEO+5)                             
      PARAMETER (LCVD=10, LCIT=10, LCTP=10, LCEC=15, LCHC=10, LCMU=10)          
      PARAMETER (LCLC=10, LCSA=10, LCSI=10)                                     
      COMMON /JOBCOM/   JDATJO,JTIMJO,VERSJO                                    
     &                 ,NEVTJO,NRNDJO(LRND),FDEBJO,FDISJO                       
     &                 ,FBEGJO(LDET),TIMEJO(LTIM),NSTAJO(LST1,LST2)             
     &                 ,IDB1JO,IDB2JO,IDB3JO,IDS1JO,IDS2JO                      
     &                 ,MBINJO(LST2),MHISJO,FHISJO(LHIS)                        
     &                 ,IRNDJO(LRND,LPRO)                                       
     &                 ,IPRIJO(LPRI),MSETJO,IRUNJO,IEXPJO,AVERJO                
     3                 ,MPROJO,IPROJO(LPRO),MGETJO,MSAVJO,TIMLJO,IDATJO         
     5                 ,TCUTJO(LTCUT),IBREJO,NKINJO,BKINJO(LKINP),IPACJO        
     6                 ,IDETJO(LDET),IGEOJO(LGEO),LVELJO(LGEO)                  
     7                 ,ICVDJO(LCVD),ICITJO(LCIT),ICTPJO(LCTP)                  
     8                 ,ICECJO(LCEC),ICHCJO(LCHC),ICLCJO(LCLC)                  
     9                 ,ICSAJO(LCSA),ICMUJO(LCMU),ICSIJO(LCSI)                  
     &                 ,FGALJO,FPARJO,FXXXJO,FWRDJO,FXTKJO,FXSHJO,CUTFJO        
     &                 ,IDAFJO,IDCHJO,TVERJO                                    
      LOGICAL FDEBJO,FDISJO,FHISJO,FBEGJO,FGALJO,FPARJO,FXXXJO,FWRDJO           
     &       ,FXTKJO,FXSHJO                                                     
      COMMON /JOBKAR/   TITLJO,TSETJO(LSET),TPROJO(LPRO)                        
     1                 ,TKINJO,TGEOJO(LBGE),TRUNJO                              
      CHARACTER TRUNJO*60                                                       
      CHARACTER*4 TKINJO,TPROJO,TSETJO,TITLJO*40                                
      CHARACTER*2 TGEOJO                                                        
C                                                                               
      PARAMETER (LERR=20)                                                       
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)                              
      COMMON /JOBCAR/   TACTJO                                                  
      CHARACTER*6 TACTJO                                                        
C                                                                               
C*CC JOBCOM                                                                     
C*CA IOCOM                                                                      
      PARAMETER (LFIL=6)                                                        
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO                
      DIMENSION LUNIIO(LFIL)                                                    
      EQUIVALENCE (LUNIIO(1),LGETIO)                                            
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)                                
      CHARACTER TFILIO*60, TFORIO*4                                             
C                                                                               
C*CC IOCOM                                                                      
      DATA IFI/0/ , NANEVT, NASEVT/2*0/                                         
C ----------------------------------------------------------------------        
      IF (NANEVT.EQ.0) NANEVT = NAMIND('NEVT')                                  
C                                                                               
C - get one record or initialize BOS                                            
C                                                                               
    1 IF (MGETJO .GT.0) THEN                                                    
   10   CALL ABRSEL ('E','    ',IRET)                                           
        IF (IRET.EQ.1) GOTO 100                                                 
        IF (IRET.GT.3) THEN                                                     
          IF (IRET.EQ.9) THEN                                                   
            CALL ALTELL ('ASIEVE: Time limit reached',0,'END')                  
          ELSEIF (IRET.EQ.8) THEN                                               
            CALL ALTELL ('ASIEVE:  End of selected events',0,'END')             
          ELSEIF (IRET.EQ.7) THEN                                               
            CALL ALTELL ('ASIEVE:  Last event reached',0,'END')                 
          ELSEIF (IRET.EQ.6) THEN                                               
            CALL ALTELL ('ASIEVE:  No more input file',0,'END')                 
          ELSEIF (IRET.LE.16) THEN                                              
            CALL ALTELL ('ASIEVE:  Cannot open input or output file'            
     &                              ,0,'END')                                   
          ELSEIF (IRET.EQ.19) THEN                                              
            CALL ALTELL ('ASIEVE:  Not enough space for unpacking',0            
     &                              ,'RETURN')                                  
          ELSEIF (IRET.EQ.17) THEN                                              
            CALL ALTELL ('ASIEVE:  Read error-try again',0,'RETURN')            
          ELSEIF (IRET.EQ.18) THEN                                              
            CALL ALTELL ('ASIEVE: Error in decompressing-next',0                
     &                              ,'RETURN')                                  
          ENDIF                                                                 
          GOTO 10                                                               
        ENDIF                                                                   
C                                                                               
        IF (IRET.EQ.3) THEN                                                     
C        unknown record                                                         
          IF (IPRIJO(15).EQ.1) CALL AUBLIS ('E')                                
          IF (MSAVJO.GT.0) CALL ASWRTP('E')                                     
C                                                                               
        ELSEIF (IRET.EQ.2) THEN                                                 
          IF (IPRIJO(15).EQ.1) CALL AUBLIS ('C')                                
C        run record : header or end                                             
          IF (IW(NARUNE).NE.0) THEN                                             
            CALL ASCRUN                                                         
          ELSEIF (IW(NARUNH).NE.0) THEN                                         
            CALL ASIRUN                                                         
          ENDIF                                                                 
        ENDIF                                                                   
        GOTO 10                                                                 
      ELSE                                                                      
C      no input file                                                            
        CALL BDROP (IW,'T')                                                     
        CALL BDROP (IW,'E')                                                     
        CALL BLIST (IW,'T=','0')                                                
        CALL BLIST (IW,'E=','EVEH')                                             
        CALL BGARB (IW)                                                         
        IF (NEVTJO.GE.IW(IW(NANEVT)+1)) THEN                                    
           CALL ALTELL ('ASIEVE: last event reached ',0,'END')                  
        ENDIF                                                                   
        CALL TIMAL(TIMEJO(2))                                                   
        IF (TIMEJO(2).LT.TIMLJO) THEN                                           
           CALL ALTELL ('ASIEVE: time limit ',0,'END')                          
        ENDIF                                                                   
      ENDIF                                                                     
C                                                                               
C - Initialize event : reset flags and variables for this event                 
C   get input event if any.                                                     
C   set random generator if required, reset banks.                              
C                                                                               
 100  CONTINUE                                                                  
      IF (NASEVT.EQ.0) NASEVT = NAMIND('SEVT')                                  
      NEVTJO = NEVTJO + 1                                                       
C                                                                               
C - get trigger # from input file if any or from serial number                  
      IF (MGETJO.GT.0) THEN                                                     
         CALL ABRUEV (IRUNJO,ITRIG)                                             
      ELSE                                                                      
         ITRIG = NEVTJO                                                         
      ENDIF                                                                     
C                                                                               
C - if not 1st entry reset ZEBRA partition                                      
      IF (IFI .NE. 0 ) CALL GTRIGC                                              
C                                                                               
C - reset GEANT3 banks and counters                                             
      CALL MZBOOK (IGXDIV,JGHEAD,JGHEAD,1,'HEAD',1,1,NGHEAD,2,0)                
      NGTRAC=0                                                                  
      NGVERT=0                                                                  
      IGEOTR=0                                                                  
      IGDEBU=0                                                                  
      IGDEVT = NEVTJO                                                           
C                                                                               
C - reset counters and flags                                                    
      KERRJO = 0                                                                
      FDEBJO=.FALSE.                                                            
      DO 3 I =1,LDET                                                            
 3    FBEGJO(I) = .TRUE.                                                        
C                                                                               
C - this event has to be processed:  set debug flag                             
      IF(ITRIG.GE.IDB1JO.AND.ITRIG.LE.IDB2JO) FDEBJO=.TRUE.                     
                                                                                
C                                    set display flag                           
C*IF INTER                                                                      
      FDISJO = .TRUE.                                                           
C*EL                                                                            
C*EI                                                                            
C                                    set random generator and time              
      IF(IFI.EQ.0) THEN                                                         
         IFI=1                                                                  
         DO 4 I=1,LPRO                                                          
            IF (IRNDJO(1,I) .EQ. 0) GOTO 4                                      
               CALL RDMIN (IRNDJO(1,I))                                         
               GOTO 5                                                           
 4       CONTINUE                                                               
 5       CONTINUE                                                               
         CALL RDMOUT (NRNDJO(1))                                                
         CALL TIMAD (TIMEJO(5))                                                 
      ENDIF                                                                     
C                                    output random generator root               
      IF (FDEBJO .AND. MGETJO.EQ.0) THEN                                        
        WRITE (LOUTIO,802) NEVTJO,IRUNJO,ITRIG,NRNDJO                           
      ELSEIF (IDB3JO.GT.0) THEN                                                 
        IF (MOD(NEVTJO,IDB3JO).EQ.0) THEN                                       
          WRITE (LOUTIO,802) NEVTJO,IRUNJO,ITRIG,NRNDJO                         
  802     FORMAT (/3X,'+++ASIEVE+++ EVENT# ',I5,'  (run ',I5,                   
     &                  ' trigger ',I5,')  random number =',3I12,               
     &                  ' ++++++++')                                            
        ENDIF                                                                   
      ENDIF                                                                     
C                                                                               
C - print E-list and decode EVEH bank if any                                    
      IF (MGETJO.GT.0) THEN                                                     
         IF (FDEBJO .AND. IPRIJO(15).EQ.1) CALL AUBLIS ('E')                    
         CALL ASREVE                                                            
      ENDIF                                                                     
C                                                                               
C - Call USER routine                                                           
      CALL USIEVE                                                               
C                                                                               
      RETURN                                                                    
      END                                                                       
C*DK ASIPAC                                                                     
C*DF INTER                                                                      
      SUBROUTINE ASIPAC                                                         
C ----------------------------------------------------------------------        
C. - F.RANJARD - 850326                                                         
C! Initialize all packages used by the job                                      
C. - called by    ASIRUN                           from this .HLB               
C. - calls        GPHYSI, GHEINI                   from GEANT3.pam              
C.                ASBOOK                            from this .HLB              
C ---------------------------------------------------------------------         
C*IF .NOT.DOC                                                                   
C*CA IOCOM                                                                      
      PARAMETER (LFIL=6)                                                        
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO                
      DIMENSION LUNIIO(LFIL)                                                    
      EQUIVALENCE (LUNIIO(1),LGETIO)                                            
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)                                
      CHARACTER TFILIO*60, TFORIO*4                                             
C                                                                               
C*CC IOCOM                                                                      
C*CA JOBCOM                                                                     
      PARAMETER (LOFFMC = 1000)                                                 
      PARAMETER (LHIS=20, LPRI=20, LTIM=6, LPRO=6, LRND=3)                      
      PARAMETER (LBIN=20, LST1=LBIN+3, LST2=3)                                  
      PARAMETER (LSET=15, LTCUT=5, LKINP=20)                                    
      PARAMETER (LDET=9,  LGEO=LDET+4, LBGE=LGEO+5)                             
      PARAMETER (LCVD=10, LCIT=10, LCTP=10, LCEC=15, LCHC=10, LCMU=10)          
      PARAMETER (LCLC=10, LCSA=10, LCSI=10)                                     
      COMMON /JOBCOM/   JDATJO,JTIMJO,VERSJO                                    
     &                 ,NEVTJO,NRNDJO(LRND),FDEBJO,FDISJO                       
     &                 ,FBEGJO(LDET),TIMEJO(LTIM),NSTAJO(LST1,LST2)             
     &                 ,IDB1JO,IDB2JO,IDB3JO,IDS1JO,IDS2JO                      
     &                 ,MBINJO(LST2),MHISJO,FHISJO(LHIS)                        
     &                 ,IRNDJO(LRND,LPRO)                                       
     &                 ,IPRIJO(LPRI),MSETJO,IRUNJO,IEXPJO,AVERJO                
     3                 ,MPROJO,IPROJO(LPRO),MGETJO,MSAVJO,TIMLJO,IDATJO         
     5                 ,TCUTJO(LTCUT),IBREJO,NKINJO,BKINJO(LKINP),IPACJO        
     6                 ,IDETJO(LDET),IGEOJO(LGEO),LVELJO(LGEO)                  
     7                 ,ICVDJO(LCVD),ICITJO(LCIT),ICTPJO(LCTP)                  
     8                 ,ICECJO(LCEC),ICHCJO(LCHC),ICLCJO(LCLC)                  
     9                 ,ICSAJO(LCSA),ICMUJO(LCMU),ICSIJO(LCSI)                  
     &                 ,FGALJO,FPARJO,FXXXJO,FWRDJO,FXTKJO,FXSHJO,CUTFJO        
     &                 ,IDAFJO,IDCHJO,TVERJO                                    
      LOGICAL FDEBJO,FDISJO,FHISJO,FBEGJO,FGALJO,FPARJO,FXXXJO,FWRDJO           
     &       ,FXTKJO,FXSHJO                                                     
      COMMON /JOBKAR/   TITLJO,TSETJO(LSET),TPROJO(LPRO)                        
     1                 ,TKINJO,TGEOJO(LBGE),TRUNJO                              
      CHARACTER TRUNJO*60                                                       
      CHARACTER*4 TKINJO,TPROJO,TSETJO,TITLJO*40                                
      CHARACTER*2 TGEOJO                                                        
C                                                                               
      PARAMETER (LERR=20)                                                       
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)                              
      COMMON /JOBCAR/   TACTJO                                                  
      CHARACTER*6 TACTJO                                                        
C                                                                               
C*CC JOBCOM                                                                     
C*CA ALFGEO                                                                     
      COMMON/ALFGEO/ALRMAX,ALZMAX,ALFIEL,ALECMS                                 
C                                                                               
C*IF DOC                                                                        
C*EI                                                                            
C*CC ALFGEO                                                                     
C ----------------------------------------------------------------------        
C                                                                               
C - Book histograms                                                             
C                                                                               
      CALL ASBOOK                                                               
C                                                                               
C - Get cross-section and energy loss tables                                    
C                                                                               
      CALL GPHYSI                                                               
C                                                                               
C - initialize GHEISHA                                                          
C                                                                               
      IPACJO = IPACJO +1                                                        
      CALL GHEINI                                                               
C                                                                               
C - initialize LUND  (beam energy)                                              
C                                                                               
      IF(TKINJO.EQ.'LUND' .OR. TKINJO.EQ.'JET') THEN                            
         IF(BKINJO(4).LE.0.) BKINJO(4) = ALECMS                                 
      ENDIF                                                                     
C                                                                               
C*IF DRAW                                                                       
C*EI                                                                            
      RETURN                                                                    
      END                                                                       
C*EI                                                                            
C*DK ASPRUN                                                                     
C*DF INTER                                                                      
      SUBROUTINE ASPRUN                                                         
C ---------------------------------------------------------------------         
C - F.RANJARD - 850326                                                          
C! Loop over events                                                             
C. - called by    GALEPH                                 from this .HLB         
C. - calls        ASIEVE, ASPEVE, ASCEVE, ASCRUN, ASRRUN from this .HLB         
C.                ABRSEL, ABWSEL                         from ALEPHLIB          
C -------------------------------------------------------------------           
C*IF .NOT.DOC                                                                   
C ----------------------------------------------------------------------        
C                                                                               
C - initialize event in GEANT framework                                         
C                                                                               
 1    CONTINUE                                                                  
      CALL ASIEVE                                                               
C                                                                               
C - Process this event : following the flags set                                
C   Go to next event if End Of Event                                            
C   Stop if End Of Run.                                                         
C                                                                               
      CALL ASPEVE                                                               
C                                                                               
C - Close this event : statistics, trigger, analysis                            
C                                                                               
      CALL ASCEVE                                                               
      GOTO 1                                                                    
C                                                                               
       END                                                                      
C*EI                                                                            
C*DK AGTPCH                                                                     
C*DF INTER                                                                      
      SUBROUTINE AGTPCH                                                         
C-----------------------------------------------------------------              
C! Implement TPC geometry                                                       
C   Author :B. Bloch-Devaux                       18 March 85                   
C.              M.E.Mermikides                    25/11/85                      
C.                    add  Detailed endplate geometry                           
C.               B. Bloch and M. Mermikides        sept. 87                     
C.                    Data base connection                                      
C.               B.Bloch update materials for central membrane,supports         
C.               gas mix and inner cage.              april 1991                
C.                                                                              
C.  -Called by AGEOME                  from this .HLB                           
C.  -Calls GSTMED,GSVOLU,GSPOS  from  GEANT3                                    
C.                                                                              
C. -Stores extra Tracking Media needed                                          
C. -Builds geometry levels from  'TPCR'  and downwards                          
C ----------------------------------------------------------------------------  
C*IF .NOT.DOC                                                                   
      EXTERNAL JHOCHA                                                           
C*CA IOCOM                                                                      
      PARAMETER (LFIL=6)                                                        
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO                
      DIMENSION LUNIIO(LFIL)                                                    
      EQUIVALENCE (LUNIIO(1),LGETIO)                                            
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)                                
      CHARACTER TFILIO*60, TFORIO*4                                             
C                                                                               
C*CC IOCOM                                                                      
C*CA BCS                                                                        
      INTEGER LMHLEN, LMHCOL, LMHROW                                            
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                                  
C                                                                               
      COMMON /BCS/   IW(1000)                                                   
      INTEGER IW                                                                
      REAL RW(1000)                                                             
      EQUIVALENCE (RW(1),IW(1))                                                 
C                                                                               
C*CC BCS                                                                        
C*CA AGCONS                                                                     
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12                 
      REAL RADEG, DEGRA                                                         
      REAL CLGHT, ALDEDX                                                        
      INTEGER NBITW, NBYTW, LCHAR                                               
      PARAMETER (PI=3.141592653589)                                             
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)                                  
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)                  
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                                 
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                                 
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)                         
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)                      
C                                                                               
C*IF DOC                                                                        
C*EI                                                                            
      PARAMETER  (D360=TWOPI*RADEG   ,D180=PI*RADEG   ,D90=0.5*D180)            
      PARAMETER  (D45=0.5*D90   ,D15=D45/3.   ,D210=7.*D90/3.)                  
      PARAMETER  (D225=D180+D45   ,D270=D180+D90  ,D7P5=0.5*D15)                
      PARAMETER(LSENV=30)                                                       
      PARAMETER (LIMVOL=17)                                                     
C                                                                               
      COMMON/AGCONS/ IAGROT,IAGMAT,IAGMED,IAGFHB,IAGSLV,IAGSEN(LSENV,2)         
     2      , NAGIMP,LAGIMP(3,LIMVOL)                                           
C                                                                               
C*IF DOC                                                                        
C*EI                                                                            
       COMMON /WRKSPC/ WSPACE(88320)                                            
C*IF DOC                                                                        
      PARAMETER (LPTAB=50)                                                      
      DIMENSION PTAB(LPTAB),JTAB(LPTAB)                                         
      EQUIVALENCE (PTAB(1),JTAB(1),WSPACE(1))                                   
C                                                                               
C*IF DOC                                                                        
C*EI                                                                            
C*CC AGCONS                                                                     
C*CA ALFGEO                                                                     
      COMMON/ALFGEO/ALRMAX,ALZMAX,ALFIEL,ALECMS                                 
C                                                                               
C*CC ALFGEO                                                                     
C*CA AGECOM                                                                     
      PARAMETER ( NLIMR = 9 , NLIMZ = 6)                                        
      COMMON / AGECOM / AGLIMR(NLIMR),AGLIMZ(NLIMZ),IAGFLD,IAGFLI,IAGFLO        
C*IF DOC                                                                        
C*EI                                                                            
C*CC AGECOM                                                                     
C*CA JOBCOM                                                                     
      PARAMETER (LOFFMC = 1000)                                                 
      PARAMETER (LHIS=20, LPRI=20, LTIM=6, LPRO=6, LRND=3)                      
      PARAMETER (LBIN=20, LST1=LBIN+3, LST2=3)                                  
      PARAMETER (LSET=15, LTCUT=5, LKINP=20)                                    
      PARAMETER (LDET=9,  LGEO=LDET+4, LBGE=LGEO+5)                             
      PARAMETER (LCVD=10, LCIT=10, LCTP=10, LCEC=15, LCHC=10, LCMU=10)          
      PARAMETER (LCLC=10, LCSA=10, LCSI=10)                                     
      COMMON /JOBCOM/   JDATJO,JTIMJO,VERSJO                                    
     &                 ,NEVTJO,NRNDJO(LRND),FDEBJO,FDISJO                       
     &                 ,FBEGJO(LDET),TIMEJO(LTIM),NSTAJO(LST1,LST2)             
     &                 ,IDB1JO,IDB2JO,IDB3JO,IDS1JO,IDS2JO                      
     &                 ,MBINJO(LST2),MHISJO,FHISJO(LHIS)                        
     &                 ,IRNDJO(LRND,LPRO)                                       
     &                 ,IPRIJO(LPRI),MSETJO,IRUNJO,IEXPJO,AVERJO                
     3                 ,MPROJO,IPROJO(LPRO),MGETJO,MSAVJO,TIMLJO,IDATJO         
     5                 ,TCUTJO(LTCUT),IBREJO,NKINJO,BKINJO(LKINP),IPACJO        
     6                 ,IDETJO(LDET),IGEOJO(LGEO),LVELJO(LGEO)                  
     7                 ,ICVDJO(LCVD),ICITJO(LCIT),ICTPJO(LCTP)                  
     8                 ,ICECJO(LCEC),ICHCJO(LCHC),ICLCJO(LCLC)                  
     9                 ,ICSAJO(LCSA),ICMUJO(LCMU),ICSIJO(LCSI)                  
     &                 ,FGALJO,FPARJO,FXXXJO,FWRDJO,FXTKJO,FXSHJO,CUTFJO        
     &                 ,IDAFJO,IDCHJO,TVERJO                                    
      LOGICAL FDEBJO,FDISJO,FHISJO,FBEGJO,FGALJO,FPARJO,FXXXJO,FWRDJO           
     &       ,FXTKJO,FXSHJO                                                     
      COMMON /JOBKAR/   TITLJO,TSETJO(LSET),TPROJO(LPRO)                        
     1                 ,TKINJO,TGEOJO(LBGE),TRUNJO                              
      CHARACTER TRUNJO*60                                                       
      CHARACTER*4 TKINJO,TPROJO,TSETJO,TITLJO*40                                
      CHARACTER*2 TGEOJO                                                        
C                                                                               
      PARAMETER (LERR=20)                                                       
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)                              
      COMMON /JOBCAR/   TACTJO                                                  
      CHARACTER*6 TACTJO                                                        
C                                                                               
C*CC JOBCOM                                                                     
C*CA TPGPAR                                                                     
      PARAMETER (LTPDRO=21,LTTROW=19,LTSROW=12,LTWIRE=200,LTSTYP=3,             
     +           LTSLOT=12,LTCORN=6,LTSECT=LTSLOT*LTSTYP,LTTPAD=4,              
     +           LMXPDR=150,LTTSRW=11)                                          
C*IF DOC                                                                        
C*EI                                                                            
C*CC TPGPAR                                                                     
C*CA TPGEOM                                                                     
      COMMON /TPGEOM/RTPCMN,RTPCMX,ZTPCMX,DRTPMN,DRTPMX,DZTPMX,                 
     &               TPFRDZ,TPFRDW,TPAVDZ,TPFOF1,TPFOF2,TPFOF3,                 
     &               TPPROW(LTPDRO),TPTROW(LTTROW),NTSECT,NTPROW,               
     &               NTPCRN(LTSTYP),TPCORN(2,LTCORN,LTSTYP),                    
     &               TPPHI0(LTSECT),TPCPH0(LTSECT),TPSPH0(LTSECT),              
     &               ITPTYP(LTSECT),ITPSEC(LTSECT),IENDTP(LTSECT)               
C                                                                               
C*IF DOC                                                                        
C*EI                                                                            
C*CC TPGEOM                                                                     
C*CA PMG1JJ                                                                     
      PARAMETER(JPMGID=1,JPMGVR=2,JPMGNA=4,JPMGRI=8,JPMGRO=9,JPMGZN=10,         
     +          JPMGZX=11,JPMGPN=12,JPMGPX=13,JPMGNM=14,LPMG1A=14)              
C*CC PMG1JJ                                                                     
C                                                                               
      PARAMETER (D52P5 = 52.5)                                                  
C                                                                               
      PARAMETER (LTAB1=5, LTAB2=5,  LMED=5)                                     
      DIMENSION TAB1(LTAB1,LMED),TAB2(LTAB2,LMED)                               
      REAL ATEP(2),ZTEP(2),WTEP(2)                                              
      REAL ATEP2(2),ZTEP2(2),WTEP2(2)                                           
C                                                                               
C ========================================================================      
C                                                                               
C   Volumes:  TPC     Active volume       (TUBE) positioned in TPCR             
C             TPWI    Inner wall of TPC   (TUBE)      "      "  "               
C             TPWO    Outer wall of TPC   (TUBE)      "      "  "               
C             TPEP    Endplate (x2)       (TUBE)      "      "  "               
C             TPHF    Half active volume (x2)(TUBE)   "      "  "               
C             TPIS    inner cage support ring(TUBE)   "      " TPHF             
C             TPMB    central mylar membrane(*3)(TUBE)   "     TPHF             
C             TPMI    central membrane inner support(TUBE)   " TPHF             
C             TPMO    central membrane outer support(TUBE)   " TPHF             
C                                                                               
C      --->   TPFR    Endplate frame      (TUBE)      "      " TPEP             
C     |       TPAV    Endframe material   (TUBE)      "      " TPEP             
C     |       TPKA    K-Sector sub-shape  (TRD1)      "      " TPFR             
C     |       TPKB     "     "      "     (TRD1)      "      "  "               
C     |       TPKC     "     "      "     (TRD1)      "      " TPKA             
C  Detail     TPMA    M-sector      "     (TRD1)      "      " TPFR             
C  Level 2    TPMB     "            "     (TRD1)      "      "  "               
C     |       TPMC     "            "     (TRD1)      "      "  "               
C     |       TPMD     "            "     (TRD1)      "      "  "               
C     |       TPWA    W-sector      "     (TRD1)      "      "  "               
C     |       TPWB     "            "     (TRD1)      "      "  "               
C     |       TPWC     "            "     (TRD1)      "      "  "               
C      --->   TPWD     "            "     (TRD1)      "      "  "               
C                                                                               
C     Medium are defined as:                                                    
C    1- TPC inner wall volume           material: IMTWI                         
C    2- TPC active gas volume           material: IMTGA                         
C    3- TPC outer wall volume           material: IMTWO                         
C    4- TPC endplate volume             material: IMTEP   mixture               
C    5- TPC endplate av. mat. vol.      material: IMTAV                         
C            (Level 2 geom)                                                     
C                                                                               
C   For each medium the two following rows are filled:                          
C     TMXFD , DMXMS , DEEMX , EPSIL , STMIN                                     
      DATA TAB1 /                                                               
     1  0.1, 0.05 , 0.1 , 0.02 , .1 ,                                           
C*IF INTER                                                                      
     2   3., 0.2  , 0.2 , 0.02 , .1 ,                                           
C*EL                                                                            
C*EI                                                                            
     3  0.1, 0.05 , 0.1 , 0.02 , .1 ,                                           
     4  20., 0.10 , 0.1 , 0.2  ,  0.1 ,                                         
     5  10., 0.05 , 0.1 , 0.1  ,  0.1 /                                         
C                                                                               
C        A    , Z    , Dens  , Radl  ,Absl                                      
C  for inner wall,Rhoacell,outer wall,gas mixture(Ar-CH4)                       
      DATA TAB2 /                                                               
     1 18.175,8.869  ,0.929   ,34.84  ,114.66 ,                                 
     2  6.7  ,3.6    ,0.07    ,805.8  ,1204.79,                                 
     3 22.96 ,11.09  ,0.62    ,44.07  ,190.15 ,                                 
     4 38.774,17.492,1.684E-03,11866.9,79875.3,5*0./                            
C                                                                               
C  Al/air mixture for TPC endplate frame section                                
C                                                                               
      DATA NTEP,ATEP,ZTEP,WTEP,DTEP/                                            
     1       2 ,26.98 ,14.61,                                                   
     2          13.0  ,7.3  ,                                                   
     3          .9973 ,0.0027 , 0.405     /                                     
C  Mixture for detailed level                                                   
      DATA NTEP2, ATEP2, ZTEP2, WTEP2, DTEP2/                                   
     1       2  , 26.98, 14.61,                                                 
     2            13.0,   7.3,                                                  
     3            .9992, .0008, .9305 /                                         
C*CA BMACRO                                                                     
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
C*CC BMACRO                                                                     
C --------------------------------------------------------------------          
      IAGMAT=IAGMAT+1                                                           
      IMTWI=IAGMAT                                                              
      CALL GSMATE(IMTWI,'TPC Inner cage matter',TAB2(1,1),TAB2(2,1),            
     1            TAB2(3,1),TAB2(4,1),TAB2(5,1),0,0)                            
      IAGMAT=IAGMAT+1                                                           
      IMTRH = IAGMAT                                                            
      CALL GSMATE(IMTRH,'TPC Rohacell support',TAB2(1,2),TAB2(2,2),             
     1            TAB2(3,2),TAB2(4,2),TAB2(5,2),0,0)                            
      IAGMAT=IAGMAT+1                                                           
      IMTWO=IAGMAT                                                              
      CALL GSMATE(IMTWO,'TPC Outer cage matter',TAB2(1,3),TAB2(2,3),            
     1            TAB2(3,3),TAB2(4,3),TAB2(5,3),0,0)                            
      IAGMAT=IAGMAT+1                                                           
      IMTGA = IAGMAT                                                            
      CALL GSMATE(IMTGA,'TPC GAS mixture Ar/CH4',TAB2(1,4),TAB2(2,4),           
     1            TAB2(3,4),TAB2(4,4),TAB2(5,4),0,0)                            
      IMTMY = 26                                                                
      IMTAV = 9                                                                 
      MDAIR = 1                                                                 
C =====================================================================         
C                                                                               
C              DEFINE TRACKING MEDIA                                            
C                                                                               
      IAGMED=IAGMED+1                                                           
      IMWAL = IAGMED                                                            
      CALL GSTMED(IMWAL,'TPC inner wall med',IMTWI,0,IAGFLD,ALFIEL,             
     1         TAB1(1,1),TAB1(2,1),TAB1(3,1),TAB1(4,1),TAB1(5,1),0,0)           
C                                                                               
      IAGMED = IAGMED+1                                                         
      IMACT = IAGMED                                                            
      CALL GSTMED (IMACT,'TPC GAS medium',IMTGA,IDETJO(3),IAGFLD,ALFIEL         
     &  , TAB1(1,2),TAB1(2,2),TAB1(3,2),TAB1(4,2),TAB1(5,2),0,0)                
C                                                                               
C    Overall TPC volume                                                         
C                                                                               
      PTAB(1)=AGLIMR(3)                                                         
      PTAB(2)=AGLIMR(5)                                                         
      PTAB(3)=AGLIMZ(1)                                                         
      CALL GSVOLU('TPCR','TUBE',1,PTAB,3,IVOL)                                  
      CALL GSPOS('TPCR',1,'CDET',0.,0.,0.,0,'ONLY')                             
C                                                                               
C  Active TPC volume, place in TPCR                                             
C                                                                               
      PTAB(1) = RTPCMN                                                          
      PTAB(2) = RTPCMX                                                          
      PTAB(3) = ZTPCMX                                                          
      CALL GSVOLU('TPC ','TUBE',IMACT, PTAB, 3, IVOL)                           
C                                                                               
C  Divide TPC volume along HV plane to split track elements crossing it         
C                                                                               
      PTAB(1) = RTPCMN                                                          
      PTAB(2) = RTPCMX                                                          
      PTAB(3) = 0.5*ZTPCMX                                                      
      CALL GSVOLU('TPHF','TUBE',IMACT,PTAB,3,IVOL)                              
      Z = 0.5 * ZTPCMX                                                          
      CALL GSPOS('TPHF',2,'TPC ',0.,0.,Z,0,'ONLY')                              
      CALL GSPOS('TPHF',1,'TPC ',0.,0.,-Z,2,'ONLY')                             
C                                                                               
C   Define where to find slot number                                            
C                                                                               
      IF (IAGSLV.GE.LSENV) GOTO 998                                             
      IAGSLV=IAGSLV+1                                                           
      IAGSEN(IAGSLV,1) = JHOCHA('TPHF')                                         
      IAGSEN(IAGSLV,2) = 5                                                      
C                                                                               
      CALL GSPOS('TPC ',1,'TPCR',0.,0.,0.,0,'ONLY')                             
C                                                                               
C   Inner wall, place in TPCR                                                   
C                                                                               
      PTAB(1)=RTPCMN-DRTPMN                                                     
      PTAB(2)=RTPCMN                                                            
      PTAB(3)=ZTPCMX                                                            
      CALL GSVOLU('TPWI','TUBE',IMWAL, PTAB, 3, IVOL)                           
      CALL GSPOS('TPWI',1,'TPCR',0.,0.,0., 0,  'ONLY')                          
C                                                                               
C   OUTER WALL                                                                  
C                                                                               
      IAGMED=IAGMED+1                                                           
      CALL GSTMED (IAGMED,'TPC OUTER WALL med',IMTWO,0,IAGFLD,ALFIEL            
     &  , TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,0)                
C                                                                               
C  Outer wall, place in TPC                                                     
C                                                                               
      PTAB(1)=RTPCMX                                                            
      PTAB(2)=RTPCMX+DRTPMX                                                     
      PTAB(3)=ZTPCMX                                                            
      CALL GSVOLU('TPWO','TUBE',IAGMED,PTAB,3,IVOL)                             
      CALL GSPOS('TPWO',1,'TPCR',0.,0.,0., 0,  'ONLY')                          
C                                                                               
C   TPC inner cage supports                                                     
C                                                                               
      IAGMED = IAGMED +1                                                        
      CALL GSTMED(IAGMED,'TPC INNERCAGE SUPPORT',IMTAV,0,IAGFLD,ALFIEL,         
     $  TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,0)                  
      KPM = IW(NAMIND('PMG1'))                                                  
      IF (KPM.GT.0) THEN                                                        
          IF (LROWS(KPM).GT.80) THEN                                            
             PTAB(1) = RTABL(KPM,81,JPMGRI)                                     
             PTAB(2) = RTABL(KPM,81,JPMGRO)                                     
             PTAB(3) = 0.5 *(ZTPCMX-RTABL(KPM,81,JPMGZN))                       
             CALL GSVOLU('TPIS','TUBE',IAGMED,PTAB,3,IVOL)                      
             Z = 0.5 * ZTPCMX-PTAB(3)                                           
             CALL GSPOS('TPIS',1,'TPHF',0.,0.,Z,0,'ONLY')                       
          ENDIF                                                                 
C                                                                               
C  Central membrane elements                                                    
C                                                                               
          IF (LROWS(KPM).GT.81) THEN                                            
C   INNER SUPPORT RING  : rhoacell ( + some glass fiber and holes)              
             IAGMED = IAGMED +1                                                 
             CALL GSTMED(IAGMED,'TPC Mylar inner sup',IMTRH,0,IAGFLD,           
     $       ALFIEL,TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,        
     $       0)                                                                 
             PTAB(1) = RTABL(KPM,82,JPMGRI)                                     
             PTAB(2) = RTABL(KPM,82,JPMGRO)                                     
             PTAB(3) = .25 *(RTABL(KPM,82,JPMGZX)-RTABL(KPM,82,JPMGZN))         
             CALL GSVOLU('TPMI','TUBE',IAGMED,PTAB,3,IVOL)                      
             Z =PTAB(3)-0.5 *ZTPCMX                                             
             CALL GSPOS('TPMI',1,'TPHF',0.,0.,Z,0,'ONLY')                       
C   OUTER SUPPORT RING  :Aluminum                                               
             IAGMED = IAGMED +1                                                 
             CALL GSTMED(IAGMED,'TPC Mylar outer sup',IMTAV,0,IAGFLD,           
     $       ALFIEL,TAB1(1,5),TAB1(2,5),TAB1(3,5),TAB1(4,5),TAB1(5,5),0,        
     $       0)                                                                 
             PTAB(1) = RTABL(KPM,83,JPMGRI)                                     
             PTAB(2) = RTABL(KPM,83,JPMGRO)                                     
             PTAB(3) = .25 *(RTABL(KPM,83,JPMGZX)-RTABL(KPM,83,JPMGZN))         
             CALL GSVOLU('TPMO','TUBE',IAGMED,PTAB,3,IVOL)                      
             Z =PTAB(3)-0.5 *ZTPCMX                                             
             CALL GSPOS('TPMO',1,'TPHF',0.,0.,Z,0,'ONLY')                       
C   MYLAR MEMBRANE                                                              
             IAGMED = IAGMED +1                                                 
             CALL GSTMED(IAGMED,'TPC MYLAR MEMBRANE ',IMTMY,0,IAGFLD,           
     $       ALFIEL,TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,        
     $       0)                                                                 
             CALL GSVOLU('TPMB','TUBE',IAGMED,PTAB,0,IVOL)                      
             PTAB(1) = RTABL(KPM,84,JPMGRI)                                     
             PTAB(2) = RTABL(KPM,84,JPMGRO)                                     
             PTAB(3) = .25 *(RTABL(KPM,84,JPMGZX)-RTABL(KPM,84,JPMGZN))         
             Z =PTAB(3)-0.5 *ZTPCMX                                             
             CALL GSPOSP('TPMB',1,'TPHF',0.,0.,Z,0,'ONLY',PTAB,3)               
C   INNER MYLAR REINFORCMENT                                                    
             PTAB(1) = RTABL(KPM,85,JPMGRI)                                     
             PTAB(2) = RTABL(KPM,85,JPMGRO)                                     
             PTAB(3) = .25 *(RTABL(KPM,85,JPMGZX)-RTABL(KPM,85,JPMGZN))         
             Z =PTAB(3)-0.5 *ZTPCMX                                             
             CALL GSPOSP('TPMB',2,'TPHF',0.,0.,Z,0,'ONLY',PTAB,3)               
C   OUTER MYLAR REINFORCMENT                                                    
             PTAB(1) = RTABL(KPM,86,JPMGRI)                                     
             PTAB(2) = RTABL(KPM,86,JPMGRO)                                     
             PTAB(3) = .25 *(RTABL(KPM,86,JPMGZX)-RTABL(KPM,86,JPMGZN))         
             Z =PTAB(3)-0.5 *ZTPCMX                                             
             CALL GSPOSP('TPMB',3,'TPHF',0.,0.,Z,0,'ONLY',PTAB,3)               
          ENDIF                                                                 
      ENDIF                                                                     
C                                                                               
C  END PLATES                                                                   
C                                                                               
C   Simple ENDPLATE description, fill volume with average                       
C   air/aliminium mixture                                                       
C                                                                               
      IAGMAT = IAGMAT+1                                                         
      IMTEP = IAGMAT                                                            
      CALL GSMIXT(IMTEP,'TPC ENDPLATE MATTER$',ATEP,ZTEP,DTEP,NTEP,WTEP)        
      IAGMED=IAGMED+1                                                           
      CALL GSTMED(IAGMED,'TP ENDPLATE VOLUME$',IMTEP,0,IAGFLD,ALFIEL,           
     1         TAB1(1,4),TAB1(2,4),TAB1(3,4),TAB1(4,4),TAB1(5,4),0,0)           
C                                                                               
      PTAB(1)=RTPCMN-DRTPMN                                                     
      PTAB(2)=RTPCMX+DRTPMX                                                     
      PTAB(3) = DZTPMX *0.5                                                     
      CALL GSVOLU('TPEP','TUBE',IAGMED,PTAB,3,IVOL)                             
C                                                                               
C  Place endplates in TPCR                                                      
C                                                                               
      Z=ZTPCMX+DZTPMX/2.                                                        
      CALL GSPOS('TPEP',1,'TPCR',0.,0., Z , 0, 'ONLY')                          
      CALL GSPOS('TPEP',2,'TPCR',0.,0.,-Z , 2, 'ONLY')                          
C                                                                               
C    Store volume name and level in the geometry tree which define              
C    entrance in detector                                                       
C                                                                               
C - the order of the CALLs is important because 'TPC' is inside 'TPCR'          
C   CALL them from inside to outside                                            
      CALL AGDIMP ('TPC ',4,'TPCO')                                             
      CALL AGDIMP ('TPCR',3,'TPCI')                                             
      GOTO 999                                                                  
C                                                                               
C - not enough space to save sensitive module                                   
C                                                                               
 998  CONTINUE                                                                  
      CALL ALTELL('AGTPCH: too many sensitive volumes ',0,'STOP')               
C                                                                               
C - end                                                                         
C                                                                               
  999 CONTINUE                                                                  
      END                                                                       
C*EI                                                                            
C*DK GAXEPH                                                                     
C*DF INTER                                                                      
C*IF INTER                                                                      
      SUBROUTINE GTRIG                                                          
C ----------------------------------------------------------------------        
C. -  B. BLOCH - 850910                                                         
C                                                                               
C  Event   tracking   control routine to replace Geant default                  
C   Calls   ASPEVE ,ASCEVE from this .HLB                                       
C   Called from <GXINT>GXCONT     from GEANT3                                   
C ---------------------------------------------------------------------         
C*IF .NOT.DOC                                                                   
      CALL ASPEVE                                                               
      END                                                                       
C*EI                                                                            
      SUBROUTINE GTRIGC                                                         
C ----------------------------------------------------------------------        
C. -  B. BLOCH - 850910                                                         
C                                                                               
C   event closing routine to replace Geant default                              
C                                                                               
C   Calls   ASCEVE        from this .HLB                                        
C   Called from <GXINT>GXCONT     from GEANT3                                   
C ---------------------------------------------------------------------         
C*IF .NOT.DOC                                                                   
      CALL ASCEVE                                                               
      END                                                                       
      SUBROUTINE GTRIGI                                                         
C ----------------------------------------------------------------------        
C. -  B. BLOCH - 850910                                                         
C                                                                               
C   event initialisation routine to replace Geant default                       
C                                                                               
C   Calls   ASIEVE        from this .HLB                                        
C   Called from <GXINT>GXCONT     from GEANT3                                   
C ---------------------------------------------------------------------         
C*IF .NOT.DOC                                                                   
      CALL ASIEVE                                                               
      END                                                                       
C*EI                                                                            
      SUBROUTINE GUKINE                                                         
C ----------------------------------------------------------------------        
C. -  B. BLOCH - 850910                                                         
C                                                                               
C  Event   generation control routine to replace Geant default                  
C   Calls   ASKINE        from this .HLB                                        
C   Called from <GXINT>GXCONT     from GEANT3                                   
C ---------------------------------------------------------------------         
C*IF .NOT.DOC                                                                   
C                                                                               
C  Generate kinematics                                                          
C                                                                               
      CALL ASKINE                                                               
      END                                                                       
C*EI                                                                            
      SUBROUTINE UGINIT                                                         
C ----------------------------------------------------------------------        
C. -  F.Ranjard - 950425                                                        
C                                                                               
C  job initialization to replace Geant default                                  
C   Calls   AIGAL        from this .HLB                                         
C   Called from <GXINT>GXCONT     from GEANT3                                   
C ---------------------------------------------------------------------         
C*IF .NOT.DOC                                                                   
C                                                                               
C  GALEPH initialization                                                        
C                                                                               
      CALL ASIGAL                                                               
      END                                                                       
C*EI                                                                            
      SUBROUTINE UGLAST                                                         
C ----------------------------------------------------------------------        
C. -  B. BLOCH - 850910                                                         
C                                                                               
C  Job termination  routine to replace Geant default                            
C   Calls   GLAST       from this .HLB                                          
C   Called from <GXINT>GXCONT     from GEANT3                                   
C -----------------------------------------------------------------------       
C*IF .NOT.DOC                                                                   
C                                                                               
C Termination routine                                                           
C                                                                               
      CALL GLAST                                                                
C                                                                               
      RETURN                                                                    
      END                                                                       
C*EI                                                                            
      SUBROUTINE GUVIEW ( NAME ,TXT1 ,TXT2 ,IV )                                
C------------------------------------------------------------------             
C B. BLOCH-DEVAUX  - 850910                                                     
C                                                                               
C   Draw some typical cut view of ALEF  with header and text                    
C   If option 'STAN' selected then draw standards plots (X-Y and R-Z views)     
C   If not,draw plot of detector 'NAME' in cut view  at cutval value            
C   determined by answers.A comment is added at the bottom                      
C   Replaces default GEANT GUVIEW routine                                       
C   Called by <GXINT>GINC1                                                      
C   Calls UCTOH,UHTOC                 from cernlib                              
C   Calls GDOPT,GDRAWT,GDRAWC,GDHEAD  from GEANT3                               
C   Calls TVNEXT,TVSHOW               from GD3                                  
C   Calls ZCGETI,ZCGETA,ZCGETR        from ZCEDEX                               
C   Calls ADXY,ADRZ,ADVIEW         from this .HLB                               
C -------------------------------------------------------------------           
C*IF .NOT.DOC                                                                   
C*CA IOCOM                                                                      
      PARAMETER (LFIL=6)                                                        
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO                
      DIMENSION LUNIIO(LFIL)                                                    
      EQUIVALENCE (LUNIIO(1),LGETIO)                                            
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)                                
      CHARACTER TFILIO*60, TFORIO*4                                             
C                                                                               
C*CC IOCOM                                                                      
C*CA GCDRAW                                                                     
      COMMON/GCDRAW/NGMNOD,MGXNOD,NGMND1,LGVVER,LGVHOR,MGAXV,IGPICK,            
     + MGLEVV,MGLEVH,NGWCUT,JGNAM,JGMOT,JGXON,JGBRO,JGDUP,JGSCA,                
     + JGDVM,JGPSM,                                                             
     + JGNAM1,JGMOT1,JGXON1,JGBRO1,JGDUP1,JGSCA1,JGULEV,JGVLEV,                 
     + LGOKTB(16),                                                              
     + GRMAT0(10),GTRAN0(3),IGRNUM,GSIN(41),GCOS(41),GSNPSI,GCSPSI,             
     + GTHETA,GPHI,GPSI,GU0,GV0,GSCU,GSCV,NGGVIE,                               
     + IGCTFL,IGCUT,GCTHET,GCPHI,GDCUT,NGSURF,IGSURF,                           
     + GZUA,GZVA,GZUB,GZVB,GZUC,GZVC,GPLTRX,GPLTRY,                             
     + LGINAT,LGINAP,IGXATT,IGTHRZ,IGPRJ,GDPERS,IGTR3D,IGKHIT,IGOBJ,            
     + LGINBU,                                                                  
     + MGAXGU,MGORGU,MGAXGS,MGORGS,MGAXTU,MGORTU,MGAXTS,MGORTS,                 
     + IGU,IGS,IGTU,IGTS,NGKVIE,IGVIEW,                                         
     + NGOPEN,IGMR,IGPION,IGTROP,                                               
     + GDUMMY(19)                                                               
C                                                                               
C*CC GCDRAW                                                                     
      CHARACTER*4 ISTAC                                                         
      CHARACTER*20 TEXT                                                         
      CALL UCTOH(TXT1,ISTA,4,4)                                                 
      CALL UHTOC(ISTA,4,ISTAC,4)                                                
      IF (ISTAC.NE.'STAN') THEN                                                 
      CALL GDOPT('THRZ','OFF ')                                                 
      IAXIS=IV                                                                  
      CALL KUPROI(' Give axis number   ',IAXIS)                                 
      CALL KUPROR(' Give cut value  ',CUTVA)                                    
      CALL KUPROC(', Give text comment with quotes   ',TEXT,NCHI)               
      CALL ICLRWK(0,0)                                                          
      XTEXT=1.*GPLTRX/20.                                                       
      YTEXT=1.*GPLTRY/20.                                                       
      CSIZE=0.4*GPLTRY/20.                                                      
      IF (IAXIS.EQ.1)                                                           
     1  CALL GDRAWT(XTEXT,YTEXT,'SIDE  VIEW $',CSIZE,0.,2,-1)                   
      IF (IAXIS.EQ.2)                                                           
     1  CALL GDRAWT(XTEXT,YTEXT,'TOP   VIEW $',CSIZE,0.,2,-1)                   
      IF (IAXIS.EQ.3)                                                           
     1  CALL GDRAWT(XTEXT,YTEXT,'FRONT VIEW $',CSIZE,0.,2,-1)                   
      IF (IAXIS.EQ.4)     THEN                                                  
       CALL GDRAWT(XTEXT,YTEXT,'R-Z  VIEW $',CSIZE,0.,2,-1)                     
       CALL GDOPT('THRZ','ON  ')                                                
       IAXIS=1                                                                  
      ENDIF                                                                     
CC                                                                              
      XTEXT=10.*GPLTRX/20.                                                      
      IOPT=0                                                                    
      CALL GDRAWT(XTEXT,YTEXT,TEXT ,CSIZE,0.,2,IOPT)                            
      CALL GDRAWC(NAME ,IAXIS,CUTVA,GU0,GV0,GSCU,GSCV)                          
      CALL GDXYZ(0)                                                             
      ISEL=111110                                                               
      CALL GDHEAD(ISEL,'$',CSIZE)                                               
      ELSE                                                                      
      CALL ADXY(1)                                                              
      CALL ADRZ(2)                                                              
      CALL ADVIEW                                                               
      ENDIF                                                                     
      RETURN                                                                    
      END                                                                       
C*EI                                                                            
C*EI                                                                            
