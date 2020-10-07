      SUBROUTINE QUEFLO                                                         
C----------------------------------------------------------------------         
C!  Call the energy flow                                                        
C----------------------------------------------------------------------         
      INTEGER LMHLEN, LMHCOL, LMHROW                                            
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                                  
      INTEGER IW                                                                
      REAL RW(10000)                                                            
      COMMON /BCS/ IW(10000)                                                    
      EQUIVALENCE (RW(1),IW(1))                                                 
C----------------------------------------------------------------------         
      IVERS=0                                                                   
      JALPV=NLINK('ALPV',0)                                                     
      IF (JALPV.EQ.0) GO TO 100                                                 
      IVERS=IW(JALPV+1)                                                         
C                                                                               
C Do nothing if ALPHA version < 115. Will lead to unpredictable                 
C results with ENFLW...                                                         
C                                                                               
      IF ( ivers .LT. 115 ) GO TO 100                                           
C                                                                               
C Only for ALPHA version 115-116-117-118 :                                      
C                                                                               
      IF ( ivers .LE. 118 ) THEN                                                
C                                                                               
C  Create PGPC # 0 if needed                                                    
C                                                                               
        ipgpc  = NLINK('PGPC',0)                                                
        IF ( ipgpc .LE. 0 ) THEN                                                
           CALL gapgpc                                                          
           CALL BLIST(iw,'S+','PGPC')                                           
        ENDIF                                                                   
C                                                                               
C Only for ALPHA version 120 and after :                                        
C                                                                               
      ELSE                                                                      
C                                                                               
C  Create PGAC # 0 if needed                                                    
C                                                                               
        ipgac  = NLINK('PGAC',0)                                                
        IF ( ipgac .LE. 0 ) THEN                                                
           CALL gapgac                                                          
           CALL BLIST(iw,'S+','PGAC')                                           
        ENDIF                                                                   
C                                                                               
      ENDIF                                                                     
C                                                                               
 100  CALL creflw(irejet)                                                       
C                                                                               
  999 RETURN                                                                    
      END                                                                       
      SUBROUTINE QUPEC1                                                         
C----------------------------------------------------------------------         
C!  Create bank PECO/1 if requested                                             
C                                                                               
C P. Janot -- 9 June 1993                                                       
C----------------------------------------------------------------------         
      INTEGER LMHLEN, LMHCOL, LMHROW                                            
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                                  
      INTEGER IW                                                                
      REAL RW(10000)                                                            
      COMMON /BCS/ IW(10000)                                                    
      EQUIVALENCE (RW(1),IW(1))                                                 
C----------------------------------------------------------------------         
      IVERS=0                                                                   
      JALPV=NLINK('ALPV',0)                                                     
      IF (JALPV.EQ.0) GO TO 999                                                 
      IVERS=IW(JALPV+1)                                                         
      IF (IVERS.LT.115) GO TO 999                                               
C Only for ALPHA version 115 and after :                                        
C                                                                               
C  Create PECO # 1 if needed                                                    
C                                                                               
      ipeco1 = NLINK('PECO',1)                                                  
      IF ( ipeco1 .LE. 0 ) THEN                                                 
         CALL GAPECO(IER)                                                       
         CALL BLIST(IW,'S+','PECO')                                             
      ENDIF                                                                     
C                                                                               
  999 RETURN                                                                    
      END                                                                       
      SUBROUTINE QMUNEW                                                         
C----------------------------------------------------------------------         
C!  Initialize the muon identification                                          
C----------------------------------------------------------------------         
      CALL MUNEWR                                                               
  999 RETURN                                                                    
      END                                                                       
      SUBROUTINE QUMUPR                                                         
C----------------------------------------------------------------------         
C!  Prepare the muon identification                                             
C----------------------------------------------------------------------         
C...Swap between FRFT #0 an #3 ALWAYS to perform the muon extrap with           
C...FRFT #2 as in JULIA                                                         
      IND=NSWAP('FRFT',0,'FRFT',3)                                              
C                                                                               
      CALL MUREDO(IER)                                                          
C                                                                               
C...Restore the original situation                                              
      IND=NSWAP('FRFT',3,'FRFT',0)                                              
C                                                                               
  999 RETURN                                                                    
      END                                                                       
      SUBROUTINE FIXMUCH                                                        
C----------------------------------------------------------------------         
C! routine to control the deletion of hits in                                   
C! the mu-ch 4c and 4d in 1993 MC                                               
C!                                   A.Gregorio and A.Venturi .02.94            
C----------------------------------------------------------------------         
      INTEGER LMHLEN, LMHCOL, LMHROW                                            
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                                  
      INTEGER IW                                                                
      REAL RW(10000)                                                            
      COMMON /BCS/ IW(10000)                                                    
      EQUIVALENCE (RW(1),IW(1))                                                 
C - # of words/row in bank with index KI                                        
      LCOLS(KI) = IW(KI+LMHCOL)                                                 
C - # of rows in bank with index KI                                             
      LROWS(KI) = IW(KI+LMHROW)                                                 
C - index of next row in the bank with index KI                                 
      KNEXT(KI) = KI + LMHLEN + IW(KI+1)*IW(KI+2)                               
C - index of row # KI1 in the bank with index KI                                
      KROW(KI,KI1) = KI + LMHLEN + IW(KI+1)*(KI1-1)                             
C - # of free words in the bank with index KI                                   
      LFRWRD(KI) = KI + IW(KI) - KNEXT(KI)                                      
C - # of free rows in the bank with index KI                                    
      LFRROW(KI) = LFRWRD(KI) / LCOLS(KI)                                       
C - KI2th integer element of the KI1th row of the bank with index KI            
      ITABL(KI,KI1,KI2) = IW(KI+LMHLEN+(KI1-1)*IW(KI+1)+KI2)                    
C - KI2th real element of the KI1th row of the bank with index KI               
      RTABL(KI,KI1,KI2) = RW(KI+LMHLEN+(KI1-1)*IW(KI+1)+KI2)                    
C----------------------------------------------------------------------         
C                                                                               
C++ Perform this deletion only on MC events and not MINI                        
C                                                                               
      JMCHO=IW(NAMIND('MCHO'))                                                  
      NAD4CD=NAMIND('D4CD')                                                     
C                                                                               
C++ If the bank D4CD exists do not repeat the mapping                           
C                                                                               
      IF(NAD4CD.LE.0) GO TO 11                                                  
      IF(IW(NAD4CD).GT.0) GO TO 19                                              
C                                                                               
C++ Look for the year simulated in the MC                                       
C                                                                               
 11   KASIM=IW(NAMIND('ASIM'))                                                  
      IF(KASIM.LE.0) THEN                                                       
        WRITE(6,*) 'ERROR : FIXMUCH - NO ASIM BANK ON THIS MC'                  
        GO TO 19                                                                
      ENDIF                                                                     
C                                                                               
C++ If we are simulating 1993 data or the MCHO bank exists, correct             
C++ for the muon chamber 4c and 4d                                              
C                                                                               
      IF (JMCHO.GT.0.OR.(ITABL(KASIM,1,1)/100).EQ.93) THEN                      
        CALL DELMUCH(IER)                                                       
      ENDIF                                                                     
 19   CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END                                                                       
      SUBROUTINE DELMUCH(IER)                                                   
C----------------------------------------------------------------------         
C! routine to delete hits in the mu-ch                                          
C! 4c and 4d in a fixed fraction of events                                      
C!                                   A.Gregorio and A.Venturi .02.94            
C----------------------------------------------------------------------         
      INTEGER LMHLEN, LMHCOL, LMHROW                                            
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                                  
      INTEGER IW                                                                
      REAL RW(10000)                                                            
      COMMON /BCS/ IW(10000)                                                    
      EQUIVALENCE (RW(1),IW(1))                                                 
      PARAMETER(JMHISC=1,JMHISN=2,JMHILN=3,JMHIXL=4,JMHIYL=5,JMHIRH=6,          
     +          JMHITH=7,JMHIPH=8,LMHITA=8)                                     
      LOGICAL BADEVNT                                                           
      DATA BADEVNT /.FALSE./                                                    
      DATA FRACT /0.256/                                                        
C - # of words/row in bank with index KI                                        
      LCOLS(KI) = IW(KI+LMHCOL)                                                 
C - # of rows in bank with index KI                                             
      LROWS(KI) = IW(KI+LMHROW)                                                 
C - index of next row in the bank with index KI                                 
      KNEXT(KI) = KI + LMHLEN + IW(KI+1)*IW(KI+2)                               
C - index of row # KI1 in the bank with index KI                                
      KROW(KI,KI1) = KI + LMHLEN + IW(KI+1)*(KI1-1)                             
C - # of free words in the bank with index KI                                   
      LFRWRD(KI) = KI + IW(KI) - KNEXT(KI)                                      
C - # of free rows in the bank with index KI                                    
      LFRROW(KI) = LFRWRD(KI) / LCOLS(KI)                                       
C - KI2th integer element of the KI1th row of the bank with index KI            
      ITABL(KI,KI1,KI2) = IW(KI+LMHLEN+(KI1-1)*IW(KI+1)+KI2)                    
C - KI2th real element of the KI1th row of the bank with index KI               
      RTABL(KI,KI1,KI2) = RW(KI+LMHLEN+(KI1-1)*IW(KI+1)+KI2)                    
C----------------------------------------------------------------------         
C                                                                               
      IER=0                                                                     
C                                                                               
C++ Determine if the event has to be considered as a simulation of              
C++ the "bad" period                                                            
C                                                                               
      BADEVNT=.FALSE.                                                           
      CALL ALPROB(1,1,PROB)                                                     
      IF(PROB.LE.FRACT) BADEVNT=.TRUE.                                          
      IND=NDROP('D4CD',0)                                                       
      JD4CD=NBANK('D4CD',0,LMHLEN+1)                                            
      IF (JD4CD.EQ.0) THEN                                                      
         WRITE (6,*) ' **** FIXMUCH **** : Unable to create D4CD bank'          
         IER=1                                                                  
         GO TO 999                                                              
      ENDIF                                                                     
      IW(JD4CD+LMHCOL)=1                                                        
      IW(JD4CD+LMHLEN)=1                                                        
      IF(.NOT.BADEVNT) THEN                                                     
C Fill the bank D4CD for good events : =1                                       
        IW(JD4CD+LMHLEN+1)=1                                                    
        GO TO 999                                                               
      ENDIF                                                                     
C                                                                               
C++ The events is a "bad" one, so the hits in chambers 4c and 4d                
C++ have to be deleted                                                          
C                                                                               
C Fill the bank D4CD for bad  events : =-1                                      
      IW(JD4CD+LMHLEN+1)=-1                                                     
C                                                                               
      KMHIT=NSWAP('MHIT',0,'MHIT',2)                                            
      IF (KMHIT.LE.0) THEN                                                      
        IER=2                                                                   
        GO TO 999                                                               
      ENDIF                                                                     
      IND=NDROP('MHIT',0)                                                       
C                                                                               
      KMHIT0=NBANK('MHIT',0,IW(KMHIT))                                          
      IF(KMHIT0.EQ.0) THEN                                                      
        IDUM=NSWAP('MHIT',0,'MHIT',2)                                           
        IER=2                                                                   
        GO TO 999                                                               
      ENDIF                                                                     
      IW(KMHIT0+1)=IW(KMHIT+1)                                                  
      IW(KMHIT0+2)=IW(KMHIT+2)                                                  
C                                                                               
C++ drop rows to be deleted in MHIT bank                                        
C                                                                               
      NGOOD=0                                                                   
      DO 10 IHIT=1,LROWS(KMHIT)                                                 
        NSUB= ITABL(KMHIT,IHIT,JMHISC)                                          
        NSLOT=ITABL(KMHIT,IHIT,JMHISN)                                          
C           generate a vector of random numbers                                 
        IF(NSUB.EQ.3.AND.                                                       
     +  (NSLOT.EQ.4.OR.NSLOT.EQ.5.OR.                                           
     +   NSLOT.EQ.21.OR.NSLOT.EQ.22)) GOTO 10                                   
        NGOOD=NGOOD+1                                                           
        IPNTO = KROW(KMHIT,IHIT)+1                                              
        IPNTN = KROW(KMHIT0,NGOOD)+1                                            
        CALL UCOPY(IW(IPNTO),IW(IPNTN),LMHITA)                                  
   10 CONTINUE                                                                  
C                                                                               
      IW(KMHIT0+LMHROW) = NGOOD                                                 
      IDUM= NBANK('MHIT',0,NGOOD*LMHITA+LMHLEN)                                 
      IF (IDUM.EQ.0) THEN                                                       
         IER=2                                                                  
         GO TO 999                                                              
      ENDIF                                                                     
C                                                                               
      IDUM=NDROP('MHIT',2)                                                      
C++ Drop the old banks of track-hit association                                 
      CALL BDROP(IW,'MTHRMCADMUID')                                             
C++ Repeat the hit-track association                                            
      CALL MUASS                                                                
      CALL MUIDO(IERT)                                                          
C                                                                               
      IER=IER+IERT*10                                                           
C                                                                               
  999 RETURN                                                                    
      END                                                                       
