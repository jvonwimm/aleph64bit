      SUBROUTINE SELEMU (IFLAHM,IFLAMC,IFLACA,CLAS19)                           
C----------------------------------------------------------------------         
CKEY EDIR DEF CLASS19                                                           
C! Muon identification class 19 code.                                           
C-                                                                              
C   Input  : None                                                               
C   Output : IFLAHM = Muon flag from HMAD bank                                  
C            IFLAMC = Muon flag from MCAD bank                                  
C            IFLACA = Muon flag based on QMUIDO analysis                        
C            CLAS19 = class 19 logical flag                                     
C-                                                                              
C   Called by   : SELEVT                                                        
C   Calls  : MUCATR                                                             
C   Input banks : HMAD,MCAD,MUID                                                
C-                                                                              
C                                       Author: R. Tenchini January 90          
C                                       Modified : G. Bagliesi April 96
C----------------------------------------------------------------------         
#ifndef DOC
      SAVE                                                                      
C --                                                                            
#include "bcs.h"
#include "hmadjj.h"
#include "mcadjj.h"
#include "muidjj.h"
C --                                                                            
      COMMON/MUEFF/NHMAD,NHMAMB,NHMPOS,NMCAD,NMBOTH,NMSING,                     
     &             NMCALO,NHMEVT,NMCEVT,NCAEVT,NMUEVT,NMUAND                    
      PARAMETER(LENMAX=1000)                                                    
      DIMENSION IPLIS(LENMAX),ICLIS(LENMAX),ITLIS(LENMAX)                       
      PARAMETER(MAXL=100)                                                       
      DIMENSION NCAVEC(MAXL)                                                    
      DIMENSION ENER(1000),PT(1000)                                             
      DIMENSION LISTEJ(300)                                                     
      CHARACTER*8 CNAM                                                          
      LOGICAL CLAS19                                                            
C --                                                                            
#include "bmacro.h"
C --                                                                            
      IFLAHM=0                                                                  
      IFLAMC=0                                                                  
      IFLACA=0                                                                  
      CLAS19 = .FALSE.                                                          
C --                                                                            
      KHMAD=IW(NAMIND('HMAD'))                                                  
      IF(KHMAD.LE.0) GOTO 888                                                   
C --                                                                            
      NMATRK = LROWS(KHMAD)                                                     
C --                                                                            
C   Loop on charged tracks linked to HMAD                                       
C --                                                                            
      DO 10 NMT = 1,NMATRK                                                      
        IFMU = ITABL(KHMAD,NMT,JHMAIF)                                          
        IF(IFMU.EQ.1) THEN                                                      
          IFLAHM = 1                                                            
          GOTO 999                                                              
        ENDIF                                                                   
   10 CONTINUE                                                                  
C --                                                                            
  888 CONTINUE                                                                  
      KMCAD=IW(NAMIND('MCAD'))                                                  
      IF(KMCAD.LE.0) GOTO 889                                                   
C --                                                                            
      NMATRK = LROWS(KMCAD)                                                     
C --                                                                            
C   Loop on charged tracks linked to MCAD                                       
C --                                                                            
      DO 20 NMT = 1,NMATRK                                                      
        MCAD1 = ITABL(KMCAD,NMT,JMCANH)                                         
        MCAD2 = ITABL(KMCAD,NMT,JMCANH+1)                                       
        IF(MCAD1.GE.1 .OR. MCAD2.GE.1) THEN                                     
          IFLAMC = 1                                                            
          GOTO 999                                                              
        ENDIF                                                                   
   20 CONTINUE                                                                  
C --                                                                            
  889 CONTINUE                                                                  
C --
C     QMUIDO
C --
      KMUID=IW(NAMIND('MUID'))
      IF(KMUID.LE.0) GOTO 999
C
      NMUID = LROWS(KMUID)
      DO 30 NMT = 1,NMUID
        IFLAG = ITABL(KMUID,NMT,JMUIIF)
        IF(IFLAG.GE.1.AND.IFLAG.LE.15) THEN
          IFLACA = 1
          GOTO 999
        ENDIF
   30 CONTINUE
C --
999   CONTINUE
C --
      IF(IFLAHM.EQ.1 .OR. IFLAMC.EQ.1 .OR. IFLACA.EQ.1)
     &  CLAS19 = .TRUE.
C --
      RETURN                                                                    
      END                                                                       
#endif

