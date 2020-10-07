      SUBROUTINE OLSPOU(KBLQP,KWORK,LLXIP,LBOMQ,SECS,DEBUG,LUNIT)
C----------------------------------------------------------------------
C!  - Store data in bank BLQP
C! 
C!   Input arguments:  KBLQP = index of BLQP bank
C!                     KWORK = index of work bank
C!                     LLXIP = .TRUE. if LXIP data can be used
C!                     LBOMQ = .TRUE. if BOMQ data can be used
C!                     SECS  = integer time of current slow control record 
C!                     DEBUG = logical (.TRUE. means debugging "ON")
C!                     LUNIT = unit for debug printout
C!
C!   Author:  O. Schneider
C!
C!   History: 07-JUN-1996 first version for private tests
C!            20-JUN-1996 incorporated in test version of JULIA
C!            19-JUL-1996 incorporated in JULIA 282 after fixing a bug 
C!                         (an empty line was not necessarily added if
C!                          no row existed yet in BLQP)
C!            22-AUG-1996 modified to run OBSPOT and save teh QS0 correction 
C!                         factors in first row of BLQP; error message 
C!                         'Database bank BLQC not found' temporarily disabled
C?
C!======================================================================
#ifndef DOC
C
C***********************************************************************
C
C     =========================
C     Non-executable statements
C     =========================
C
      IMPLICIT NONE
C
C     Arguments
C
      INTEGER KBLQP,KWORK,SECS,LUNIT
      LOGICAL LLXIP,LBOMQ,DEBUG
C
C     Externals
C
      INTEGER NAMIND,NPRNT,AGETDB
C
C     HAC parameter definitions
C
C#include "blqcjj.h"
*CD BLQCJJ
      INTEGER JBLQFR,JBLQCF,JBLQSY,JBLQAA,JBLQMA,JBLQCA,JBLQAB,JBLQMB,  
     +          JBLQCB,LBLQCA                                           
      PARAMETER(JBLQFR=1,JBLQCF=2,JBLQSY=3,JBLQAA=5,JBLQMA=7,JBLQCA=9,  
     +          JBLQAB=11,JBLQMB=13,JBLQCB=15,LBLQCA=16)                
#include "blqpjj.h"
C
C     Other parameters
C
      CHARACTER*(*) RNAME 
      PARAMETER(RNAME='OLSPOU') ! name of this routine
      INTEGER LRADD
      PARAMETER(LRADD=25)
      INTEGER NRBLQC
      PARAMETER(NRBLQC=0)
      INTEGER EMPTY
      PARAMETER(EMPTY='FFFFFFFF'X)
C
C     Non-static local variables
C
      INTEGER K
      INTEGER I,TIM,EVT,DELTA1,DELTA2
      INTEGER LENGTH,IRUN,IGARB,INDX,IRET
      LOGICAL OLD,NEW,OOLD,ONEW
      INTEGER KBLQC,IROW,NDIFF
      INTEGER CBITS
C
C     Static local variables (i.e. saved variables)
C
      INTEGER OSECS,ORUN
      SAVE    OSECS,ORUN
      INTEGER NABLQC
      SAVE    NABLQC
      INTEGER KUTS(LBLQCA)
      SAVE    KUTS
      REAL    CUTS(LBLQCA)
      SAVE    CUTS
      EQUIVALENCE(KUTS,CUTS)
      DATA    OSECS,ORUN/0,0/
      DATA    NABLQC/0/
      DATA    KUTS/LBLQCA*0/
C
C     BOS common block
C
#include "bcs.h"
C
C     Standard function definition statements
C
#include "bmacrod.h"
#include "bmacro.h"
C
C***********************************************************************
C
C     ==================
C     Initialize routine
C     ==================
C
      IF(NABLQC.LE.0) NABLQC=NAMIND('BLQC') ! Database bank with cuts
C
C***********************************************************************
C
C     ===========================
C     Get BLQC bank from database
C     ===========================
C
      IRUN=IW(KBLQP-2)
      IF(ORUN.EQ.0.OR.IRUN.NE.ORUN) THEN 
        IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &   'read BLQC from database for run ',IRUN
        IRET=AGETDB('BLQC',IRUN)
        IF(DEBUG) WRITE(LUNIT,2000) RNAME,'AGETDB return code =',IRET
        KBLQC=IW(NABLQC)
        IF(IRET.EQ.0.OR.KBLQC.LE.0) THEN
C******** for the time being the BLQC bank is not put in the database ...
C******** ... so we don't generate an error message if it is not found there
C***          CALL RERROR(RNAME,1,
C***     &     'Database bank BLQC not found, will use default cuts')
          IF(DEBUG) WRITE(LUNIT,3000) RNAME,'BLQC'
 3000     FORMAT(1X,A,': Database bank ',A,
     &           ' not found, will use default cuts')
C
C         Create default BLQC bank
C
          CALL BDROP(IW,'BLQC')
          LENGTH=LMHLEN+LBLQCA
          CALL AUBOS('BLQC',NRBLQC,LENGTH,KBLQC,IGARB)
          IF(IGARB.EQ.2) THEN 
            CALL RERROR(RNAME,-2,'No space to create BLQC bank')
            IF(DEBUG) WRITE(LUNIT,1000) RNAME,
     &       'No space to create BLQC bank'
            ORUN=0
            RETURN
          ENDIF
          IF(IGARB.EQ.1.AND.DEBUG) WRITE(LUNIT,1000)
     &      'garbage collection occurred'
          IW(KBLQC+LMHCOL)=LBLQCA
          IW(KBLQC+LMHROW)=1
          IW(KBLQC+LMHLEN+JBLQFR)=-1
          IW(KBLQC+LMHLEN+JBLQCF)='00030003'X
          IW(KBLQC+LMHLEN+JBLQSY)=100 
          IW(KBLQC+LMHLEN+JBLQSY+1)=0
          RW(KBLQC+LMHLEN+JBLQAA  )=0.  ! QS0 correction factor in x
          RW(KBLQC+LMHLEN+JBLQAA+1)=1.1 ! QS0 correction factor in y
          RW(KBLQC+LMHLEN+JBLQAB  )=0.  ! QS0 correction factor in x
          RW(KBLQC+LMHLEN+JBLQAB+1)=1.1 ! QS0 correction factor in y
C**          IF(DEBUG) LENGTH=NPRNT('BLQC',NRBLQC)
          IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &      'default BLQC bank created, NR = ',NRBLQC
        ELSE
          IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &     'BLQC bank found, NR = ',IW(KBLQC-2)
        ENDIF
C
C       Find appropriate row in BLQC bank
C
        IROW=0
        DO I=1,LROWS(KBLQC)
          IF(ITABL(KBLQC,1,JBLQFR).LE.IRUN) IROW=I
        ENDDO
        IF(IROW.EQ.0) THEN 
          CALL RERROR(RNAME,-3,'Requested run not found in BLQC')
          IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &     'run not found in BLQC:',IRUN
          ORUN=0
          RETURN
        ENDIF
C
C       Load cuts
C
        NDIFF=0
        DO I=1,LBLQCA
          IF(ITABL(KBLQC,IROW,I).NE.KUTS(I)) NDIFF=NDIFF+1
          KUTS(I)=ITABL(KBLQC,IROW,I)
        ENDDO
        IF(NDIFF.NE.0) THEN 
          WRITE(LUNIT,5000) RNAME,(KUTS(I),I=1,JBLQAA-1),
     &                        'A',(CUTS(I),I=JBLQAA,JBLQAB-1),
     &                        'B',(CUTS(I),I=JBLQAB,LBLQCA)
 5000     FORMAT(1X,A,': cuts used for runs starting at ',I6,':',/,
     &'   control flag = ',Z8.8,',',
     &'   synchronization cuts = ',2I8,' seconds',/,(
     &'   side ',A,':  x and y QS0 correction factors =',2F7.2,/,
     &    '            nominal average   e+e- difference =',
     &                                        2F7.1,' microns',/,
     &    '            maximum deviation e+e- difference =',
     &                                        2F7.1,' microns'))
        ENDIF
      ENDIF
      ORUN=IRUN
C
C***********************************************************************
C
C     ======================================================================
C     Create first row of BLQP to contain correction and alignment constants
C     ======================================================================
C
C     For the first row only, the meaning of the columns is modified as follows:
C
C     JBLQFE  = 1   ----> negative version number of bank
C     JBLQSF  = 2   ----> always -1 = 'FFFFFFFF'X
C     JBLQSF+1= 3   ----> always -1 = 'FFFFFFFF'X
C     JBLQBO  = 4   ----> beam spot position in x w.r.t to BOM+QS0 estimate
C     JBLQBO+1= 5   ----> beam spot position in y w.r.t to BOM+QS0 estimate
C     JBLQBO+2= 6   ----> error on beam spot position in x
C     JBLQBO+3= 7   ----> error on beam spot position in y
C     JBLQQS  = 8   ----> QS0 correction factor in x for side A
C     JBLQQS+1= 9   ----> QS0 correction factor in y for side A
C     JBLQQS+2=10   ----> QS0 correction factor in x for side A
C     JBLQQS+3=11   ----> QS0 correction factor in y for side A
C
      IF(LROWS(KBLQP).LE.0) THEN 
        IW(KBLQP+LMHROW)=1
        IW(KBLQP+LMHLEN+JBLQFE  )=-1
        IW(KBLQP+LMHLEN+JBLQSF  )=EMPTY
        IW(KBLQP+LMHLEN+JBLQSF+1)=EMPTY
        RW(KBLQP+LMHLEN+JBLQBO  )= 0. ! will be filled at end of run
        RW(KBLQP+LMHLEN+JBLQBO+1)= 0. ! will be filled at end of run
        RW(KBLQP+LMHLEN+JBLQBO+2)=-1. ! will be filled at end of run
        RW(KBLQP+LMHLEN+JBLQBO+3)=-1. ! will be filled at end of run
        RW(KBLQP+LMHLEN+JBLQQS  )=CUTS(JBLQAA)
        RW(KBLQP+LMHLEN+JBLQQS+1)=CUTS(JBLQAA+1)
        RW(KBLQP+LMHLEN+JBLQQS+2)=CUTS(JBLQAB)
        RW(KBLQP+LMHLEN+JBLQQS+3)=CUTS(JBLQAB+1)
        IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &   'Special row 1 created in BLQP bank for run ',IRUN
      ENDIF
C
C***********************************************************************
C
C     ======================================================
C     Loop on events stored in work bank to form mini-chunks
C     ======================================================
C
      IF(DEBUG) THEN 
        WRITE(LUNIT,2000) RNAME,'OSECS, SECS = ',OSECS,SECS
 2000   FORMAT(1X,A,': ',A,1X,3I10)
        IF(LROWS(KBLQP).LE.1) THEN 
          OOLD=.TRUE.
        ELSE
          OOLD=ITABL(KBLQP,LROWS(KBLQP),JBLQSF).NE.EMPTY.AND.
     &         ITABL(KBLQP,LROWS(KBLQP),JBLQSF+1).NE.EMPTY
        ENDIF
        ONEW=.FALSE.
        DO I=1,LROWS(KWORK)
          EVT=ITABL(KWORK,I,1)
          TIM=ITABL(KWORK,I,2)
          DELTA1=IABS(OSECS-KUTS(JBLQSY+1)-TIM)
          DELTA2=IABS( SECS-KUTS(JBLQSY+1)-TIM)
          OLD=DELTA1.LE.KUTS(JBLQSY)
          NEW=DELTA2.LE.KUTS(JBLQSY)
          IF(OLD.AND.NEW) THEN  
            OLD=DELTA1.LT.DELTA2
            NEW=.NOT.OLD
          ENDIF
          IF(I.LE.2.OR.I.EQ.LROWS(KWORK).OR.
     &       OOLD.NEQV.OLD.OR.ONEW.NEQV.NEW)  
     &     WRITE(LUNIT,1001) I,EVT,TIM,OLD,NEW
 1001     FORMAT(1X,2I7,I10,2(2X,L1))
          OOLD=OLD
          ONEW=NEW
        ENDDO
      ENDIF
C
      IF(LROWS(KBLQP).LE.1) THEN 
        OOLD=.TRUE.
      ELSE
        OOLD=ITABL(KBLQP,LROWS(KBLQP),JBLQSF).NE.EMPTY.AND.
     &       ITABL(KBLQP,LROWS(KBLQP),JBLQSF+1).NE.EMPTY
      ENDIF
      ONEW=.FALSE.
      DO I=1,LROWS(KWORK)
        EVT=ITABL(KWORK,I,1)
        TIM=ITABL(KWORK,I,2)
        DELTA1=IABS(OSECS-KUTS(JBLQSY+1)-TIM)
        DELTA2=IABS( SECS-KUTS(JBLQSY+1)-TIM)
        OLD=DELTA1.LE.KUTS(JBLQSY)
        NEW=DELTA2.LE.KUTS(JBLQSY)
        IF(OLD.AND.NEW) THEN  
          OLD=DELTA1.LT.DELTA2
          NEW=.NOT.OLD
        ENDIF
C
C       Add empty line to BLQP bank
C
        IF((OOLD.AND..NOT.OLD).OR.
     &     (.NOT.ONEW.AND.NEW)) THEN 
          IF(LROWS(KBLQP).LE.1.OR.
     &       EVT.NE.ITABL(KBLQP,LROWS(KBLQP),JBLQFE)) THEN
            IF(LFRROW(KBLQP).LT.1) THEN
              LENGTH=IW(KBLQP)+LRADD*LCOLS(KBLQP)
              CALL AUBOS('BLQP',IRUN,LENGTH,KBLQP,IGARB)
              IF(IGARB.EQ.2) THEN 
                CALL RERROR(RNAME,-4,'No space to extend BLQP bank')
                IF(DEBUG) WRITE(LUNIT,1000) RNAME,
     &            'No space to extend BLQP bank'
 1000           FORMAT(1X,A,': ',A)
                INDX=KROW(KBLQP,LROWS(KBLQP))
                IW(INDX+JBLQSF)=EMPTY
                IW(INDX+JBLQSF+1)=EMPTY
                RETURN
              ENDIF
              IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &         'bank BLQP extended for run ',IRUN
            ENDIF
            INDX=KNEXT(KBLQP)
            IW(KBLQP+LMHROW)=IW(KBLQP+LMHROW)+1
            IW(INDX+JBLQFE)=EVT
            IW(INDX+JBLQSF)=EMPTY
            IW(INDX+JBLQSF+1)=EMPTY
           IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &       'empty BLQP row added / row, event = ',IW(KBLQP+LMHROW),EVT
          ELSE 
            IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &       'last BLQP row will be overwritten / row, event = ',
     &       IW(KBLQP+LMHROW),EVT
          ENDIF
        ENDIF
C
C       Fill line in BLQP bank
C
        IF(.NOT.ONEW.AND.NEW) THEN 
          INDX=KROW(KBLQP,LROWS(KBLQP))
          CBITS=KUTS(JBLQCF)
          IF(.NOT.LLXIP) CBITS=IAND(CBITS,'FFFF0000'X)
          IF(.NOT.LBOMQ) CBITS=IAND(CBITS,'0000FFFF'X)
          CALL OLSPOD(IW(INDX+JBLQSF),RW(INDX+JBLQBO),RW(INDX+JBLQQS),
     &                CBITS,CUTS(JBLQMA),DEBUG,LUNIT)
          IF(DEBUG)
     &    WRITE(LUNIT,2001) RNAME,'status = ',IW(INDX+JBLQSF),
     &                                        IW(INDX+JBLQSF+1),
     &                                        IW(INDX+JBLQSF),
     &                                        IW(INDX+JBLQSF+1)
 2001     FORMAT(1X,A,': ',A,1X,2I10,2X,2Z9.8)
          IF(DEBUG) 
     &    WRITE(LUNIT,2002) RNAME,'BOM=',(RW(INDX+JBLQBO+K),K=0,3),
     &                          ', QS0=',(RW(INDX+JBLQQS+K),K=0,3)
 2002     FORMAT(1X,A,':',2(2X,A,4F8.1))
          IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &         'BLQP row filled for event ',EVT
        ENDIF
C
        OOLD=OLD
        ONEW=NEW
      ENDDO
C**      IF(DEBUG) LENGTH=NPRNT('BLQP',IRUN)
      OSECS=SECS
C
C     Reset work bank
C
      IW(KWORK+LMHROW)=0
      END
#endif
