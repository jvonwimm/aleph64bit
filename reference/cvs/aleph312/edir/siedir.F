      SUBROUTINE SIEDIR(SIARM,SIBHA)                                   
C----------------------------------------------------------------------
CKEY EDIR CLASS SICAL                                                  
C! Compute EDIR class from SICAL                                       
C-                                                                     
C   Input  : None                                                      
C   Output : SIARM = Class 23 logical flag  single arm  A or B         
C            SIBHA = Class 22 logical flag  coincidence A and B        
C-                                                                     
C   Called by   : SELEVT                                               
C   Calls  : None                                                      
C   Input banks : SCLS                                                 
C-                                                                     
C                                   Author: B.Bloch-Devaux September 92
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "bcs.h"
#include "sclsjj.h"
#include "evehjj.h"

      LOGICAL SIARM,SIBHA                                              
      PARAMETER ( E0=20.,ENLEP0=91.2)
      DATA NRUNO /0/
      DATA NSCLS /0/
C
#include "bmacro.h"
C ------------------------------------------------------------------   
      SICALA = 0.
      SICALB = 0.
      SIARM = .FALSE.
      SIBHA = .FALSE.
      KEVEH = IW(NAMIND('EVEH'))
      IF ( KEVEH.LE.0) GO TO 999
      NRUN = IW(KEVEH+JEVERN )
      IF ( NRUN.NE.NRUNO) THEN
         ENLEP = ALELEP(NRUN)
         ECUT= E0 * ENLEP / ENLEP0
         NRUNO = NRUN
      ENDIF
C --                                                                   
      IF (NSCLS.EQ.0) NSCLS = NAMIND('SCLS')                           
      KSCLS=IW(NSCLS)                                                  
      IF(KSCLS.LE.0) GOTO 999                                          
C --                                                                   
C   Loop on SCAL clusters and get separately the energy                
C   of the SCAL sides A and B                                          
C --                                                                   
      NCLU  = LROWS(KSCLS)                                             
      DO 40 NLT = 1,NCLU                                               
        ZCLU = RTABL(KSCLS,NLT,JSCLZC)                                 
        IF ( ZCLU.GT.0.) THEN                                          
           SICALA = SICALA+RTABL(KSCLS,NLT,JSCLEC)                     
        ELSE IF ( ZCLU.LT.0.) THEN                                     
           SICALB = SICALB+RTABL(KSCLS,NLT,JSCLEC)                     
        ENDIF                                                          
   40 CONTINUE                                                         
C --                                                                   
      IF(SICALA.GT.ECUT .AND. SICALB.GT.ECUT) SIBHA = .TRUE.
      IF(SICALA.GT.ECUT .OR.  SICALB.GT.ECUT) SIARM = .TRUE.
C --                                                                   
  999 RETURN                                                           
      END                                                              
#endif
