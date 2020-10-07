      SUBROUTINE JNUCLE (NTK,TK,DP,JNP)                                         
C -------------------------------------------------------
C - Author - D.Rousseau
C -------------------------------------------------------
      IMPLICIT NONE                                                             
#include "qdecl.h"
#include "qcde.h"
      INTEGER MAXNTK                                                            
      PARAMETER (MAXNTK=10)                                                     
      INTEGER NTK,TK(MAXNTK)                                                    
      REAL JNP(4),DP(4)                                                         
      REAL MOM_OBJ,ETRACK                                                       
      REAL JNDIST,JNINVM,JNMOM,JNMASS                                           
      EXTERNAL JNDIST,JNINVM,JNMOM,JNMASS                                       
                                                                                
      INTEGER ITRK,MAXTRK,ICH,ICHT,IQUL,IJUL,IALP,INAT,NTRK                     
      INTEGER NLOOP,NLOOPMAX,NSAY                                               
                                                                                
C common                                                                        
      PARAMETER (MAXTRK=100)                                                    
      PARAMETER (NLOOPMAX=MAXTRK*2)                                             
      REAL TRACK(MAXTRK+1,4)                                                    
      COMMON/TRACK/TRACK                                                        
      INTEGER FLAG(MAXTRK)                                                      
                                                                                
      LOGICAL STOP,FIRST                                                        
      INTEGER NFOUND                                                            
      INTEGER IOBJ,I,J,ITRKMIN                                                  
      REAL DIST,DISTMIN,DMOM(4),OLDMASS,ECUT,MASSCUT,MASS                       
      REAL MPI2                                                                 
                                                                                
      INTEGER   ENFLOW                                                          
      INTEGER KCHT,NAMIND                                                       
      REAL QPMASS                                                               
                                                                                
      SAVE FIRST,ENFLOW                                                         
                                                                                
#include "qmacro.h"
                                                                                
Cthese essential parameters are roughly tuned for Bs-->Ds X                     
      DATA MASSCUT/7./                                                          
      DATA ECUT/1./                                                             
                                                                                
C for Psi the tuning is the following                                           
C      DATA MASSCUT/6./                                                         
C      DATA ECUT/0.5/                                                           
                                                                                
      DATA NSAY/0/                                                              
      DATA FIRST/.TRUE./                                                        
                                                                                
                                                                                
C find out if PCPA or Janor ENFLW to be used                                    
      IF (FIRST) THEN                                                           
                  FIRST=.FALSE.                                                 
                  MPI2=QPMASS('Pi+')**2                                         
                                                                                
                  IF (IW(NAMIND('EFLJ')).NE.0                                   
     &            .OR.IW(NAMIND('EFLW')).NE.0) THEN                             
            WRITE(*,*)'JNUCLE : EFLOW WANTED'                                   
            ENFLOW=1                                                            
                                               ELSE                             
            WRITE(*,*)'JNUCLE : PCPA WANTED'                                    
            ENFLOW=0                                                            
                                               ENDIF                            
                                                                                
            IF (ENFLOW.EQ.1.AND.KNEFT.EQ.0) THEN                                
               WRITE(*,*)'WARNING JNUCLE : EFLOW NOT AVAILABLE'                 
               ENFLOW=0                                                         
                            ENDIF                                               
                                                                                
            IF (ENFLOW.EQ.0.AND.KNNET.EQ.0) THEN                                
               WRITE(*,*)'WARNING JNUCLE : PCPA NOT AVAILABLE'                  
               ENFLOW=1                                                         
                            ENDIF                                               
                                                                                
            IF (ENFLOW.EQ.1.AND.KNEFT.EQ.0) THEN                                
               WRITE(*,*)'WARNING JNUCLE : ONLY',                               
     &                             ' CHARGED TRACKS WILL BE USED'               
               ENFLOW=0                                                         
                            ENDIF                                               
                                                                                
                                                                                
               ENDIF   !FIRST                                                   
                                                                                
      IF (NTK.GT.MAXNTK) THEN                                                   
         CALL QWMESE('WARNING JNUCLE:too many charged track from D')            
         GOTO 999                                                               
                         ENDIF                                                  
                                                                                
cinitialize jet momentum to default values                                      
         DO I=1,4                                                               
           JNP(I)=0.                                                            
         ENDDO                                                                  
                                                                                
Calculate the D 4-momentum                                                      
      IF (DP(4).LE.0.) THEN                                                     
         DO I=1,4                                                               
           DP(I)=0.                                                             
         ENDDO                                                                  
         DO I=1,NTK                                                             
           DP(1)=DP(1)+QX(TK(I))                                                
           DP(2)=DP(2)+QY(TK(I))                                                
           DP(3)=DP(3)+QZ(TK(I))                                                
           DP(4)=DP(4)+QE(TK(I))                                                
         ENDDO                                                                  
                       ENDIF                                                    
                                                                                
                                                                                
C build an array of usable tracks with the proper mass,                         
Ci.e all charged track good for energy flow                                     
Cexcept the one from the D + all neutrals                                       
      ITRK=0                                                                    
      NFOUND=0                                                                  
                                                                                
                                                                                
cif  PCPA energy flow wanted                                                    
      IF (ENFLOW.EQ.0) THEN                                                     
        DO 111 ICH=KFCHT,KLCHT                                                  
C check that it is a good track                                                 
          IQUL=KFRIQF(ICH)                                                      
          IF (IQUL.NE.1.AND.IQUL.NE.3) GOTO 111                                 
C check it is not from the D                                                    
          DO J=1,NTK                                                            
             IF (KCHT(TK(J)).EQ.KCHT(ICH)) THEN                                 
                NFOUND=NFOUND+1                                                 
                GOTO 111                                                        
                             ENDIF                                              
          ENDDO                                                                 
          ETRACK=SQRT(QP(ICH)**2+MPI2)                                          
          IF (ETRACK.LT.ECUT) GOTO 111                                          
                                                                                
Cit is a good track to save with mass pion                                      
          IF (ITRK.LT.MAXTRK) THEN                                              
                              ITRK=ITRK+1                                       
                              TRACK(ITRK,1)=QX(ich)                             
                              TRACK(ITRK,2)=QY(ich)                             
                              TRACK(ITRK,3)=QZ(ich)                             
                              TRACK(ITRK,4)=ETRACK                              
                            ELSE                                                
      CALL QWMESE('WARNING:JNUCLE MAX # OF TRACKS REACHED')                     
      GOTO 999                                                                  
                            ENDIF                                               
 111    CONTINUE                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
C fill all neutrals                                                             
        DO 112 ICH=KFNET,KLNET                                                  
C check it is not from the D                                                    
          DO J=1,NTK                                                            
             IF (TK(J).EQ.ICH) THEN                                             
                NFOUND=NFOUND+1                                                 
                GOTO 112                                                        
                               ENDIF                                            
          ENDDO                                                                 
                                                                                
          INAT=KPCQNA(ICH)                                                      
C keep Ecal object  with mass 0                                                 
          IF (INAT.GT.0.AND.INAT.LE.14)   THEN                                  
                                                                                
            ETRACK=QP(ICH)                                                      
            IF (ETRACK.LT.ECUT) GOTO 112                                        
            IF (ITRK.LT.MAXTRK) THEN                                            
                              ITRK=ITRK+1                                       
                              TRACK(ITRK,1)=QX(ICH)                             
                              TRACK(ITRK,2)=QY(ICH)                             
                              TRACK(ITRK,3)=QZ(ICH)                             
                              TRACK(ITRK,4)=ETRACK                              
                                ELSE                                            
          CALL QWMESE('WARNING:JNUCLE MAX # OF TRACKS REACHED')                 
          GOTO 999                                                              
                               ENDIF                                            
                                          ELSE                                  
            ETRACK=SQRT(QP(ICH)**2+MPI2)                                        
            IF (ETRACK.LT.ECUT) GOTO 112                                        
                                                                                
C keep the rest with mass pion                                                  
            IF (ITRK.LT.MAXTRK) THEN                                            
                              ITRK=ITRK+1                                       
                              TRACK(ITRK,1)=QX(ICH)                             
                              TRACK(ITRK,2)=QY(ICH)                             
                              TRACK(ITRK,3)=QZ(ICH)                             
                              TRACK(ITRK,4)=ETRACK                              
                                ELSE                                            
          CALL QWMESE('WARNING:JNUCLE MAX # OF TRACKS REACHED')                 
          GOTO 999                                                              
                                ENDIF                                           
                                          ENDIF                                 
 112   CONTINUE                                                                 
                                                                                
C if Janot energy flow wanted                                                   
            ELSE                                                                
        DO 113 ICH=KFEFT,KLEFT                                                  
Cif a charged track check it is not from the D                                  
          INAT=KEFOTY(ICH)                                                      
          IF (INAT.LT.4) THEN                                                   
            ICHT=50+KEFOLT(ICH)                                                 
            DO J=1,NTK                                                          
              IF (KCHT(TK(J)).EQ.ICHT) THEN                                     
                NFOUND=NFOUND+1                                                 
                GOTO 113                                                        
                               ENDIF                                            
            ENDDO                                                               
                         ELSE                                                   
            DO J=1,NTK                                                          
              IF (TK(J).EQ.ICH) THEN                                            
                NFOUND=NFOUND+1                                                 
                GOTO 113                                                        
                               ENDIF                                            
            ENDDO                                                               
                                                                                
                                 ENDIF                                          
C keep Ecal object  with mass 0                                                 
          IF (INAT.EQ.4)   THEN                                                 
            ETRACK=QP(ICH)                                                      
            IF (ETRACK.LT.ECUT) GOTO 113                                        
            IF (ITRK.LT.MAXTRK) THEN                                            
                              ITRK=ITRK+1                                       
                              TRACK(ITRK,1)=QX(ICH)                             
                              TRACK(ITRK,2)=QY(ICH)                             
                              TRACK(ITRK,3)=QZ(ICH)                             
                              TRACK(ITRK,4)=QP(ICH)                             
                                ELSE                                            
          CALL QWMESE('WARNING:JNUCLE MAX # OF TRACKS REACHED')                 
          GOTO 999                                                              
                                ENDIF                                           
                                          ELSE                                  
C keep the rest with mass pion                                                  
            ETRACK=SQRT(QP(ICH)**2+MPI2)                                        
            IF (ETRACK.LT.ECUT) GOTO 113                                        
            IF (ITRK.LT.MAXTRK) THEN                                            
                              ITRK=ITRK+1                                       
                              TRACK(ITRK,1)=QX(ICH)                             
                              TRACK(ITRK,2)=QY(ICH)                             
                              TRACK(ITRK,3)=QZ(ICH)                             
                              TRACK(ITRK,4)=ETRACK                              
                                ELSE                                            
          CALL QWMESE('WARNING:JNUCLE MAX # OF TRACKS REACHED')                 
          GOTO 999                                                              
                                ENDIF                                           
                                          ENDIF                                 
 113   CONTINUE                                                                 
                                                                                
            ENDIF                                                               
                                                                                
CArray TRACK has been filled                                                    
                                                                                
                                                                                
Check that all the tracks from the D have been found                            
      IF (NTK.NE.NFOUND) THEN                                                   
         NSAY=NSAY+1                                                            
         IF (NSAY.LE.10)                                                        
     &    CALL QWMESE('WARNING JNUCLE all tracks from D not found')             
          GOTO 999                                                              
                         ENDIF                                                  
                                                                                
                                                                                
                                                                                
      NTRK=MIN(ITRK,MAXTRK)                                                     
                                                                                
Cinitialize object to the D                                                     
      IOBJ=MAXTRK+1                                                             
      DISTMIN=0.                                                                
                                                                                
      DO I=1,4                                                                  
        TRACK(IOBJ,I)=DP(I)                                                     
      ENDDO                                                                     
                                                                                
C initialize mass of the object                                                 
      OLDMASS=JNMASS(IOBJ)                                                      
                                                                                
                                                                                
                                                                                
Cinitialize flag array                                                          
C (FLAG(ITRK)=0 -->  unusable because already used                              
C (FLAG(ITRK)=1 -->  usable                                                     
        DO ITRK=1,NTRK                                                          
          FLAG(ITRK)=1                                                          
        ENDDO                                                                   
                                                                                
                                                                                
        STOP=.FALSE.                                                            
        NLOOP=0                                                                 
        DO WHILE (.NOT.STOP.AND.NLOOP.LT.NLOOPMAX)                              
        NLOOP=NLOOP+1                                                           
                                                                                
C loop on all tracks save the "closest" to the current object                   
        DISTMIN=1.E30                                                           
        ITRKMIN=0                                                               
        DO ITRK=1,NTRK                                                          
                                                                                
C calculate distance if not flagged out                                         
         IF (FLAG(ITRK).EQ.1) THEN                                              
            DIST=JNDIST(IOBJ,ITRK)                                              
            IF (DIST.LT.DISTMIN) THEN                                           
                                  DISTMIN=DIST                                  
                                  ITRKMIN=ITRK                                  
                                 ENDIF                                          
                              ENDIF !IF FLAG                                    
        ENDDO !ITRK=1,NTRK                                                      
c case there are no particles left                                              
        IF (ITRKMIN.EQ.0) THEN                                                  
           STOP=.TRUE.                                                          
                       ENDIF                                                    
                                                                                
                                                                                
        IF (.NOT.STOP) THEN                                                     
C calculate invariant mass of object plus closest particle                      
          MASS=JNINVM(IOBJ,ITRKMIN)                                             
                       ELSE                                                     
C special case no particles left                                                
          MASS=OLDMASS                                                          
                      ENDIF !IF .NOT.STOP                                       
                                                                                
                                                                                
Cif mass of new object below MASSCUT merge the old object and the track         
C to form the new object, otherwise stop                                        
        IF (MASS.LT.MASSCUT) THEN                                               
                               CALL JNMERGE(IOBJ,ITRKMIN)                       
c    flag the track so we do not use it again                                   
                               FLAG(ITRKMIN)=0                                  
                               OLDMASS=MASS                                     
                             ELSE                                               
                               STOP=.TRUE.                                      
                             ENDIF                                              
                                                                                
                                                                                
       ENDDO !WHILE .not.STOP                                                   
                                                                                
Ccheck no infinite loop                                                         
      IF (NLOOP.GE.NLOOPMAX) THEN                                               
          CALL QWMESE('WARNING JNUCLE infinite loop')                           
          GOTO 999                                                              
                             ENDIF                                              
Cthe object is the one we want ; fill the output array                          
      DO I=1,4                                                                  
       JNP(I)=TRACK(IOBJ,I)                                                     
      ENDDO                                                                     
                                                                                
                                                                                
      RETURN                                                                    
                                                                                
 999  CONTINUE                                                                  
                                                                                
                                                                                
      END                                                                       
                                                                                
C*************************************************************                  
C sets of function to manipulate 4-vectors in array TRACK                       
C*************************************************************                  
Ccalculate invariant mass of two tracks                                         
      REAL FUNCTION JNINVM(I,J)                                                 
      IMPLICIT NONE                                                             
      INTEGER I,J                                                               
      REAL JNINVM2                                                              
                                                                                
Carray of tracks                                                                
      INTEGER MAXTRK                                                            
      PARAMETER (MAXTRK=100)                                                    
      REAL TRACK(MAXTRK+1,4)                                                    
      COMMON /TRACK/TRACK                                                       
                                                                                
      JNINVM2=(TRACK(I,4)+TRACK(J,4))**2-                                       
     &          (TRACK(I,1)+TRACK(J,1))**2-                                     
     &          (TRACK(I,2)+TRACK(J,2))**2-                                     
     &          (TRACK(I,3)+TRACK(J,3))**2                                      
      JNINVM=SIGN(SQRT(ABS(JNINVM2)),JNINVM2)                                   
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
Cmerge tracks ITK and JTK in track JTK                                          
      SUBROUTINE JNMERGE(ITK,JTK)                                               
      IMPLICIT NONE                                                             
                                                                                
      INTEGER ITK,JTK,I                                                         
Carray of tracks                                                                
      INTEGER MAXTRK                                                            
      PARAMETER (MAXTRK=100)                                                    
      REAL TRACK(MAXTRK+1,4)                                                    
      COMMON /TRACK/TRACK                                                       
                                                                                
      DO I=1,4                                                                  
      TRACK(ITK,I)=TRACK(ITK,I)+TRACK(JTK,I)                                    
      ENDDO                                                                     
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
Ccalculate distance (JADE definition : 2 E1 E2 ( 1 - COS Theta12 )              
C between   2 tracks                                                            
      REAL FUNCTION JNDIST (ITK,JTK)                                            
      IMPLICIT NONE                                                             
      INTEGER ITK,JTK                                                           
      real JNMOM,JNINVM                                                         
      external  JNMOM,JNINVM                                                    
Carray of tracks                                                                
      INTEGER MAXTRK                                                            
      PARAMETER (MAXTRK=100)                                                    
      REAL TRACK(MAXTRK+1,4),DIST2,MOMI,MOMJ                                    
      COMMON /TRACK/TRACK                                                       
                                                                                
                                                                                
      MOMI=JNMOM(ITK)                                                           
      IF (MOMI.EQ.0.) THEN                                                      
         CALL QWMESE('WARNING JNUCLE:DISTANCE WITH 0 MOMENTUM')                 
         RETURN                                                                 
                      ENDIF                                                     
                                                                                
      MOMJ=JNMOM(JTK)                                                           
      IF (MOMJ.EQ.0.) THEN                                                      
         CALL QWMESE('WARNING JNUCLE:DISTANCE WITH 0 MOMENTUM')                 
         RETURN                                                                 
                     ENDIF                                                      
                                                                                
Cstandard JADE algoritHme  DIST2=2 E1 E2*(1-COS12)                              
       DIST2=2*              TRACK(ITK,4)*TRACK(JTK,4)*                         
     &           (1.          -(TRACK(ITK,1)*TRACK(JTK,1)                       
     &                         +TRACK(ITK,2)*TRACK(JTK,2)                       
     &                         +TRACK(ITK,3)*TRACK(JTK,3))                      
     &                         /MOMI/MOMJ)                                      
                                                                                
                                                                                
      JNDIST=SIGN(SQRT(ABS(DIST2)),DIST2)                                       
                                                                                
      RETURN                                                                    
                                                                                
      END                                                                       
                                                                                
                                                                                
Cmomentum                                                                       
      REAL  FUNCTION   JNMOM(ITK)                                               
      IMPLICIT NONE                                                             
      INTEGER ITK                                                               
                                                                                
Carray of tracks                                                                
      INTEGER MAXTRK                                                            
      PARAMETER (MAXTRK=100)                                                    
      REAL TRACK(MAXTRK+1,4)                                                    
      COMMON /TRACK/TRACK                                                       
                                                                                
      REAL JNMOM2                                                               
      JNMOM2=TRACK(ITK,1)**2+TRACK(ITK,2)**2+TRACK(ITK,3)**2                    
      JNMOM=SIGN(SQRT(ABS(JNMOM2)),JNMOM2)                                      
      RETURN                                                                    
      END                                                                       
                                                                                
Cmass                                                                           
      REAL  FUNCTION   JNMASS(ITK)                                              
      IMPLICIT NONE                                                             
      INTEGER ITK                                                               
                                                                                
Carray of tracks                                                                
      INTEGER MAXTRK                                                            
      PARAMETER (MAXTRK=100)                                                    
      REAL TRACK(MAXTRK+1,4)                                                    
      COMMON /TRACK/TRACK                                                       
                                                                                
      REAL JNMASS2                                                              
      JNMASS2=TRACK(ITK,4)**2-TRACK(ITK,1)**2-TRACK(ITK,2)**2                   
     &                                        -TRACK(ITK,3)**2                  
      JNMASS=SIGN(SQRT(ABS(JNMASS2)),JNMASS2)                                   
      RETURN                                                                    
      END                                                                       
                                                                                
