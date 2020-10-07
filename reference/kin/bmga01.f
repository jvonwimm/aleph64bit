       SUBROUTINE ASKUSI(ICODE)                                         
C -----------------------------------------------------------------     
C - arranged for BEAMGA  by E. Lange - 880426                           
C - last update by E. Lange - 881214                                    
C! Initialize BEAMGA parameters                                         
C - Calls      BKFMT, BLIST               from BOS77 hlb                
C - Calls      BEAMIN                     from BMGA01 .HLB              
C                                                                       
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2 , LBCS = 1000)            
C                                                                       
      INTEGER IW                                                        
      REAL RW(LBCS)                                                     
C                                                                       
      COMMON /BCS/ IW(LBCS)                                             
      EQUIVALENCE (RW(1),IW(1))                                         
C                                                                       
      COMMON /LUSTAT/ ICOULU(10)                                        
C                                                                       
      PARAMETER (L1MST=40, L1PAR=80)                                    
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40) 
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)           
      PARAMETER (LEMSTE=40, LEPARE=80)                                  
      PARAMETER (LJNPAR=2000)                                           
C                                                                       
      COMMON /LUDAT1/   MSTLU1(L1MST),PARLU1(L1PAR)                     
      COMMON /LUDAT2/   KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID)    
     &                , KFRLU2(L2KFR),CFRLU2(L2CFR)                     
      COMMON /LUDAT3/   DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR)      
     &                , KDPLU3(L3KDP)                                   
      COMMON /LUDATE/   MSTELE(LEMSTE),PARELE(LEPARE)                   
      COMMON /LUJETS/   NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)        
C                                                                       
       COMMON /EVTCOM/ IDB1,IDB2,NIT,SDVRT(3)                           
C                                                                       
C                                                                       
      INTEGER ALTABL                                                    
      EXTERNAL ALTABL                                                   
C                                                                       
      PARAMETER(MAXTM=2,MAXCUT=14,MAXNP=3)                              
C                                                                       
      COMMON /BEALUN/ LOUTBE                                            
      COMMON /BEADEF/ TM(MAXTM)                                         
      COMMON /LEPTOU/ CUT(MAXCUT),LST(40),PARL(30),X,Y,W2,Q2,U          
      COMMON /BEARAN/ BRANGE                                            
      COMMON /XINT/ NP(MAXNP)                                           
C                                                                       
      PARAMETER (LPDEC=48)                                              
      DIMENSION NODEC(LPDEC)                                            
      DIMENSION TABL(30)                                                
C                                                                       
C -------------------------------------------------------------------   
C                                                                       
      LOUTBE = IW(6)                                                    
C                                                                       
      DO 5 I=1,10                                                       
        ICOULU(I)=0                                                     
   5  CONTINUE                                                          
C                                                                       
C set code for BEAMGA                                                   
C                                                                       
      ICODE = 8002                                                      
C                                                                       
C - Set LUND parameters by data cards                                   
C                                                                       
      CALL KXLUCO (IPRO)                                                
C                                                                       
C - Create the KLIN bank and complete the PART bank                     
C                                                                       
      CALL KXLUPA (IPART,IKLIN)                                         
C                                                                       
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN                              
        WRITE (LOUTBE,900) IPART,IKLIN                                  
  900   FORMAT (1X,'+++ASKUSI+++ error filling PART or KLIN--STOP',2I5) 
        STOP                                                            
      ENDIF                                                             
C                                                                       
C - Get list of LUND particle# which should not be decayed              
C                                                                       
      MXDEC = KNODEC (NODEC,LPDEC)                                      
      MXDEC = MIN (MXDEC,LPDEC)                                         
      CALL PRPART                                                       
C                                                                       
C - Inhibit LUND decays                                                 
C                                                                       
      DO 11 I=1,MXDEC                                                   
        IF (NODEC(I).GT.0) THEN                                         
          JIDB = NLINK('IDB',NODEC(I))                                  
          IF (JIDB.EQ.0)  IDBLU3(NODEC(I)) = 0                          
        ENDIF                                                           
   11 CONTINUE                                                          
C                                                                       
C get defaults and cuts from data cards                                 
C                                                                       
C                                                                       
      JBMGA = IW(NAMIND('GENE'))                                        
C                                                                       
      IF (JBMGA.GT.0) THEN                                              
C                                                                       
        EBEAM = RW(JBMGA+1)                                             
        LEPIN = NINT(RW(JBMGA+2))                                       
        IOPT = NINT(RW(JBMGA+3))                                        
        IHIST = NINT(RW(JBMGA+4))                                       
        A = RW(JBMGA+5)                                                 
        Z = RW(JBMGA+6)                                                 
        BRANGE = RW(JBMGA+7)                                            
      ELSE                                                              
        EBEAM = 46.1                                                    
        LEPIN = 7                                                       
        IOPT = 1                                                        
        IHIST = 2                                                       
        A = 1.                                                          
        Z = 1.                                                          
        BRANGE = 20.                                                    
      ENDIF                                                             
C                                                                       
C initialize BEAMGA event generator                                     
C                                                                       
      CALL BEAMIN(EBEAM,LEPIN,IOPT,IHIST,A,Z)                           
C                                                                       
      IF (JBMGA.GT.0) THEN                                              
        TM(1) = RW(JBMGA+8)                                             
        TM(2) = RW(JBMGA+9)                                             
      ENDIF                                                             
        TABL(1) = EBEAM                                                 
        TABL(2) = LEPIN                                                 
        TABL(3) = IOPT                                                  
        TABL(4) = IHIST                                                 
        TABL(5) = A                                                     
        TABL(6) = Z                                                     
        TABL(7) = BRANGE                                                
        TABL(8) = TM(1)                                                 
        TABL(9) = TM(2)                                                 
C                                                                       
        JBLEP = IW(NAMIND('GLEP'))                                      
C                                                                       
        IF (JBLEP.GT.0) THEN                                            
          NPAR = IW(JBLEP)                                              
          NPAR = MIN0(NPAR,MAXCUT)                                      
C                                                                       
          DO 100 I=1,NPAR                                               
            CUT(I) = RW(JBLEP+I)                                        
  100     CONTINUE                                                      
        ENDIF                                                           
C                                                                       
C                                                                       
      DO 103 I=1,MAXCUT                                                 
        TABL(9+I) = CUT(I)                                              
  103 CONTINUE                                                          
C                                                                       
      JXINT = IW(NAMIND('GINT'))                                        
C                                                                       
      IF (JXINT.GT.0) THEN                                              
        NPAR = IW(JXINT)                                                
        NPAR = MIN0(NPAR,MAXNP)                                         
C                                                                       
        DO 115 I = 1, NPAR                                              
          NP(I) = IW(JXINT + I)                                         
  115   CONTINUE                                                        
      ENDIF                                                             
C                                                                       
      DO 116 I = 1,MAXNP                                                
        TABL(9+MAXCUT+I) = FLOAT (NP(I))                                
  116 CONTINUE                                                          
C                                                                       
C vertex smearing                                                       
C                                                                       
      SDVRT(1) = 0.035                                                  
      SDVRT(2) = 0.0012                                                 
      SDVRT(3) = 1.28                                                   
C                                                                       
      JSVRT = NLINK('SVRT',0)                                           
C                                                                       
      IF (JSVRT.NE.0) THEN                                              
        SDVRT(1) = RW(JSVRT+1)                                          
        SDVRT(2) = RW(JSVRT+2)                                          
        SDVRT(3) = RW(JSVRT+3)                                          
      ENDIF                                                             
C                                                                       
      DO 102 I=1,3                                                      
 102  TABL(9+MAXCUT+MAXNP+I)=SDVRT(I)                                   
C                                                                       
      NCOL=9+MAXCUT+MAXNP+3                                             
      NROW=1                                                            
      JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'2I,(F)','C')                
      CALL PRTABL('KPAR',0)                                             
C                                                                       
      WRITE(LOUTBE,*)                                                   
      WRITE(LOUTBE,1010)BRANGE                                          
      WRITE(LOUTBE,*)                                                   
C                                                                       
C maybe we want a LULIST of some events                                 
C                                                                       
      NIT = 0                                                           
      IDB1 = 0                                                          
      IDB2 = 0                                                          
C                                                                       
      JDEBU = IW(NAMIND('DEBU'))                                        
C                                                                       
      IF (JDEBU.GT.0) THEN                                              
        IDB1 = IW(JDEBU+1)                                              
        IDB2 = IW(JDEBU+2)                                              
      ENDIF                                                             
C                                                                       
C compute cross section for selected process                            
C                                                                       
      CALL XCROSS(SIG)                                                  
C                                                                       
      WRITE(LOUTBE,*)                                                   
      WRITE(LOUTBE,1000)SIG                                             
      WRITE(LOUTBE,*)                                                   
C                                                                       
      RETURN                                                            
C                                                                       
 1000 FORMAT(/,1X,' CROSS SECTION IN NANOBARN = ',F13.0)                
C                                                                       
 1010 FORMAT(/,1X,' RANGE IN Z POSITION FOR BEAMGAS EVENTS IN CM =',    
     &       F13.1)                                                     
C                                                                       
      END                                                               
      SUBROUTINE ASKUSE (IOPX,ISTA,NITR,NIVX,ECMS,WEIT)                 
C ----------------------------------------------------------------------
C. - E. Lange - 880425 arranged for BEAMGA event generator              
C. - last update - E. Lange - 881010                                    
C! Generate 1 event with BEAMGA and fill KINE/VERT banks                
C - Output arguments:                                                   
C          IOPX     = selected porcess in BEAMGA                        
C          ISTA     = status word  ( 0 means OK )                       
C          NITR     = # of generated tracks                             
C          NIVX     = # of generated vertices                           
C          ECMS     = beam energy                                       
C          WEIT     = event weight                                      
C. - calls        RANNOR                             from KERNLIB       
C                                                                       
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2 , LBCS = 1000)            
C                                                                       
      INTEGER IW                                                        
      REAL RW(LBCS)                                                     
C                                                                       
      COMMON /BCS/ IW(LBCS)                                             
      EQUIVALENCE (RW(1),IW(1))                                         
C                                                                       
       COMMON /EVTCOM/ IDB1,IDB2,NIT,SDVRT(3)                           
C                                                                       
      REAL VRTX(4),PLAB(4)                                              
C                                                                       
      COMMON /BEAINT/ PLX,PLY,PLZ,PPX,PPY,PPZ,EBEAM,LEPIN,INTER,        
     &                CROMAX(10),EPSMIN,EPSMAX,COSMIN,COSMAX,DIRECT     
      COMMON /BEAFLA/ IOPT,IHIST,INUC,IFIRST                            
      COMMON /BEARAN/ BRANGE                                            
      COMMON /LUSTAT/ ICOULU(10)                                        
C                                                                       
C ----------------------------------------------------------------------
C                                                                       
      NIT=NIT+1                                                         
C                                                                       
C - process beamgas event, event is stored in COMMON /LUJETS/           
C                                                                       
 1    CALL BEAMGA(IERR)                                                 
C                                                                       
      ICOULU(9) = ICOULU(9) +  1                                        
C                                                                       
      IF (NIT.GE.IDB1 .AND. NIT.LE.IDB2) CALL LULIST(11)                
C                                                                       
      IF (IERR.GT.0) THEN                                               
        ICOULU(1) = ICOULU(1) +  1                                      
        GO TO 1                                                         
      ENDIF                                                             
C                                                                       
      IOPX = IOPT                                                       
      ECMS = 2.*EBEAM                                                   
      WEIT = 1.                                                         
C                                                                       
C - create vertex position                                              
C                                                                       
      ZPOS = BRANGE*(1.-2.*RNDM(DUM))                                   
C                                                                       
C - smear vertex position                                               
C                                                                       
      CALL RANNOR(RN1,RN2)                                              
      CALL RANNOR(RN3,DUM)                                              
      VRTX(1)=RN1*SDVRT(1)                                              
      VRTX(2)=RN2*SDVRT(2)                                              
      VRTX(3)=RN3*SDVRT(3) + ZPOS                                       
      VRTX(4)=0.                                                        
      NIVX = 1                                                          
C                                                                       
      IF (NIT.GE.IDB1 .AND. NIT.LE.IDB2) THEN                           
        WRITE(6,1000)VRTX(1),VRTX(2),VRTX(3)                            
      ENDIF                                                             
C                                                                       
C - store BEAMGA  event in ALEPH banks                                  
C                                                                       
      CALL KXLUAL(VRTX,ISTA,NIVX,NITR)                                  
C                                                                       
      IF (ISTA.EQ.0)   THEN                                             
        ICOULU(10)= ICOULU(10)+1                                        
      ELSEIF ( ISTA .GT. 0) THEN                                        
        ICOULU(2)= ICOULU(2)+1                                          
      ELSEIF ( ISTA.LT.0) THEN                                          
        ICOULU(-ISTA+1) = ICOULU(-ISTA+1) +1                            
      ENDIF                                                             
C                                                                       
      RETURN                                                            
C                                                                       
 1000 FORMAT(/,1X,' VERTEX POSITION AT (X,Y,Z) = ',                     
     &      3F10.5)                                                     
C                                                                       
       END                                                              
       SUBROUTINE USCJOB                                                
C -----------------------------------------------------------------     
C   E.Lange - 880425           B. Bloch - 880510                        
C!                                                                      
C! Terminate BEAMGA beamgas event generator                             
C!                                                                      
C                                                                       
C -------------------------------------------------------------------   
      COMMON /BEALUN/ LOUTBE                                            
      COMMON/LUSTAT/ ICOULU(10)                                         
C.......................................................................
C                                                                       
       WRITE(LOUTBE,101)                                                
  101  FORMAT(//20X,'EVENTS STATISTICS',                                
     &         /20X,'*****************')                                
       WRITE(LOUTBE,102) ICOULU(9),ICOULU(10),ICOULU(9)-ICOULU(10)      
  102  FORMAT(/5X,'# OF GENERATED EVENTS                = ',I10,        
     &        /5X,'# OF ACCEPTED  EVENTS                = ',I10,        
     &        /5X,'# OF REJECTED  EVENTS                = ',I10)        
       WRITE(LOUTBE,103)                                                
  103  FORMAT(//20X,'REJECT STATISTICS',                                
     &         /20X,'*****************')                                
       WRITE(LOUTBE,104) (ICOULU(I),I=1,8)                              
  104  FORMAT(/10X,'IR= 1 BEAMGA generator rejected  # OF REJECT =',I10,
     $        /10X,'IR= 2 LUND ERROR unknown part    # OF REJECT =',I10,
     &        /10X,'IR= 3 BOS  ERROR KINE/VERT       # OF REJECT =',I10,
     &        /10X,'IR= 4 LUND ERROR too many tracks # OF REJECT =',I10,
     &        /10X,'IR= 5 LUND ERROR Beam wrong pos  # OF REJECT =',I10,
     &        /10X,'IR= 6 LUND ERROR status code >5  # OF REJECT =',I10,
     &        /10X,'IR= 7 free for user              # OF REJECT =',I10,
     &        /10X,'IR= 8 free for user              # OF REJECT =',I10)
      RETURN                                                            
      END                                                               
