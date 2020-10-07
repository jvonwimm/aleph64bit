#ifdef UNIX
C*HE 05/12/95 15:43:48 C,D      
C*DK DAFILM         
C*DF UNIX           
      SUBROUTINE DAFILM (STRING,FNAME,FTYPE,FDEVI,IER)  
C -------------------------------------------------------------------   
CKEY DALI   
C! simulate AOPERD for DALI 
C - F.Ranjard - 950303  
C - Input   : STRING  / A = character string containing an input
C                           filename
C   
C - Output  : FNAME   / A = filename to be input to AOPEN   
C             FTYPE   / A = filetype
C             FDEVI   / A = file device 
C             IER     / I = AFILMI return code  
C ----------------------------------------------------------------- 
*#ifndef DOC
*#include "bcs.h"   
      INCLUDE 'A_BCS.INC'
C*IF VAX        
      SAVE ALDAT,LDAT   
      CHARACTER*80  ALDAT   
      CHARACTER*(*) STRING, FNAME, FTYPE, FDEVI 
C --------------------------------------------------------------------- 
C   
C*IF VAX        
C - fill CARD bank  
      CALL ALCARD ('FILM',0,STRING) 
      CALL AFILMI (FNAME,FTYPE,FDEVI,IRET)  
      IF (IRET.EQ.0) THEN   
C*IF VAX        
C*IF UNIX       
C              replace ALWS directory by ALUNIX pathname if it is   
C              a  VAX FILM card 
               IVAX = INDEX(FNAME,':')  
               IF (IVAX.NE.0) THEN  
C               FILM card comes from VAX
                  LNAM = LNBLNK(FNAME)  
                  IF (INDEX(FNAME,'AL$DATA').NE.0 .OR.  
     &                INDEX(FNAME,'AL$MCDATA') .NE.0 ) THEN 
C                 IBM and RS6K compilers do not understand  
C                          FNAME = 'blabala'//FNAME 
                     CALL GETENV ('ALDATA',ALDAT)
                     LDAT = LNBLNK(ALDAT)  
                     FNAME = ALDAT(1:LDAT)//'/'//FNAME(IVAX+1:LNAM) 
                  ENDIF 
                  CALL CUTOL (FNAME)
               ELSEIF (INDEX(FDEVI,'GIME') .GT. 0) THEN 
C               FILM card comes from IBM
                  FDEVI = 'CART '//FNAME(1:6)//'.1.SL'  
                  FNAME = 'ALDATA'  
               ENDIF
C*EI        
      ENDIF 
C   
      END   
C*EI        
#endif
#ifdef VMS
      SUBROUTINE DAFILM (STRING,FNAME,FTYPE,FDEVI,IER)
C ------------------------------------------------------------------- 
CKEY DALI                                                             
C! simulate AOPERD for DALI                                           
C - F.Ranjard - 950303                                                
C - Input   : STRING  / A = character string containing an input      
C                           filename                                  
C                                                                     
C - Output  : FNAME   / A = filename to be input to AOPEN             
C             FTYPE   / A = filetype                                  
C             FDEVI   / A = file device                               
C             IER     / I = AFILMI return code                        
C -----------------------------------------------------------------   
      INCLUDE 'A_BCS.INC'
      INCLUDE '($LNMDEF)'                                             
      INCLUDE '($SSDEF)'                                              
      INTEGER*2 ITMLS(8) /10,LNM$_STRING,6*0/                         
      EQUIVALENCE (ITMLS(3),IADDR)                                    
      EQUIVALENCE (ITMLS(5),LEN)                                      
      INTEGER SYS$TRNLNM                                              
      INTEGER STATUS                                                  
      CHARACTER*10 NODE                                               
      SAVE ALDAT,LDAT                                                 
      CHARACTER*80  ALDAT                                             
      CHARACTER*(*) STRING, FNAME, FTYPE, FDEVI                       
      LOGICAL FIRST                                                   
      DATA FIRST /.TRUE./                                             
C -------------------------------------------------------------------
C                                                                     
      IF (FIRST) THEN                                                 
C        get the VAX node                                             
        IADDR = %LOC(NODE)                                            
        LEN = %LOC(LENNA)                                             
        STATUS = SYS$TRNLNM(,'LNM$SYSTEM_TABLE','SYS$CLUSTER_NODE',   
     &                      ,ITMLS)                                   
        CALL GETENVF ('ALDATA',ALDAT)                                 
        LDAT = LNBLNK(ALDAT)                                          
        IF (LDAT.NE.0) THEN                                           
           ALDAT = ALDAT(1:LDAT)//'/'                                 
           LDAT = LDAT+1                                              
        ENDIF                                                         
      ENDIF                                                           
C - fill CARD bank                                                    
      CALL ALCARD ('FILM',0,STRING)                                   
      CALL AFILMI (FNAME,FTYPE,FDEVI,IRET)                            
      IF (IRET.EQ.0) THEN                                             
C              On ALWS and if IBM FILM card replace GIME by CART      
C              to get the file through SHIFT alstagein                
               IF (NODE(1:4) .EQ. 'ALWS') THEN                        
                 IVAX = INDEX(FNAME,':')                              
                 IF (IVAX.EQ.0) THEN                                  
                    IGIME = INDEX(FDEVI,'GIME')                       
                    IF (IGIME.GT.0) THEN                              
C                    FILM card comes from IBM                         
                      FDEVI = 'CART '//FNAME(1:6)//'.1.SL'            
                      FNAME = 'ALDATA'                                
                    ENDIF                                             
                 ENDIF                                                
               ENDIF                                                  
      ENDIF                                                           
C                                                                     
      END                                                             
#endif /* VMS */
      SUBROUTINE ALCARD (CARD,NR,STRING)                              
C --------------------------------------------------------------------
CKEY ALREAD FILE ENVIRONMENT                                          
C! fill bank CARD,NR with character STRING
C - F.Ranjard - 950116                                                
C - Input   : STRING   / A = character string to be put into a bank   
C             CARD     / A4= bank name                                
C             NR       / I = bank number                              
C                                                                     
C - Description : if STRING contains quotes only characters between   
C                 quotes are put into the bank created with CARD name.
C                 if the bank already exists, it is dropped and booked
C                 again                                               
C --------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      CHARACTER*(*) STRING, CARD
      CHARACTER     STR*80
C ---------------------------------------------------------------------
C                                                                     
      J1 = INDEX(STRING,'''')                                         
      IF (J1.GT.0) THEN                                               
         J2 = INDEX(STRING(J1+1:),'''') + J1 - 1                      
      ELSE                                                            
         J2 = INDEX(STRING,'!') + 1                                   
         IF (J2.EQ.1) J2 = LNBLNK(STRING)                             
      ENDIF                                                           
      J1 = J1+1                                                       
      STR = STRING(J1:J2)                                             
C                                                                     
      JCARD = NDROP (CARD,NR)                                         
      NW = (LNBLNK(STR)+3)/4                                          
      IF (NW.EQ.0) RETURN                                             
      JCARD = NBANK (CARD,NR,NW)                                      
      IF (JCARD.EQ.0) RETURN                                          
      CALL ALINST (STR(1:LNBLNK(STR)),IW(JCARD+1),NC)                 
C                                                                     
      END                                                             
