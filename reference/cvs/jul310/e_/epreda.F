      SUBROUTINE EPREDA
C***********************************************************************
C! PREPARE ECAL DATA
C!     INPUT: 'ETDI','ECMC'
C!     OUPUT: 'ESDA'
C!     CREATED : 'ESDA'
C!     FUNCTIONAL DESCRIPTION:
C!             CLEAN UP LAST EVENT BANKS
C!             TRANSFORM STOREY BANK ,Insert online corrections
C!
C***********************************************************************
#ifndef DOC
C      EXTERNAL IBITS
      INTEGER NMOD1(36) , NMOD2(36) ,NMOD3(36)
      INTEGER NCL1(36) , NCL2(36) , NAUTO(36)
      INTEGER NAUTOW ( 36 )
       DIMENSION PLACO(36)
#include "bcs.h"
#include "rcurnt.h"
#include "rlunit.h"
#include "rmonco.h"
      INTEGER IRUN94
      PARAMETER ( IRUN94 = 25000 )
#include "rparac.h"
#include "rflags.h"
#include "ecnamc.h"
#include "esdajj.h"
#include "ecmcjj.h"
#include "egnrec.h"
#include "bmacro.h"
C
      CALL VZERO (NMOD1 , 36)
      CALL VZERO (NMOD2 , 36)
      CALL VZERO (NMOD3 , 36)
      CALL VZERO (NCL1 , 36)
      CALL VZERO (NCL2 , 36)
      CALL VZERO (NAUTO , 36)
      CALL VZERO ( NAUTOW , 36 )
C
C-    In case of raw data from run range 1994 (<25854)
C     swap gain map of endcap A
C
      CALL ECSW94
C
C-    In case of reprocessing rebuilt EWDI bank
C
      CALL ECERAS
C
C     Analyse Ecal read-out status
C
      CALL EFERRR(NAUTO)
C
C-    call to fix ETDI swapping in end-cap for runs 22793 - 22880
C
C
      CALL ECSW1093
C
C     count number of ECAL storeys before cleaning
C
      CALL ESCOUN(N1,NMOD1)
      NESTOR=N1
C
C   Correct corrupted data for known bugs
C   and apply absolute calibration per module on Pad and Wire Energies
C  - pad problems  EPADCL
C  - wire problems EWIRCL
      CALL ERCUPR
C
      CALL ESWPST
C
      CALL EPADCL
C
C    count number of ECAL storeys AFTER cleaning on pads
C
      CALL ESCOUN(N2,NMOD2)
      DO 30 I = 1, 36
          NCL1(I) = NMOD1(I) - NMOD2(I)
 30   CONTINUE
      NSUPEB=N2-N1
C
C
C
C-    Clean noisy amplificators
C
      CALL ECLAMP
C
C-    Cleaning from wires
C
      CALL EWIRCL
C  Prepare wire bank PEWI
      CALL ECFILS
C  remove more pad data below wire module threshold if required
      IF(IW(NAMIND('EWTH')).NE.0) CALL ETHRES
C
C    count number of ECAL storeys AFTER cleaning based on wire threshold
C
      CALL ESCOUN(N3,NMOD3)
      DO 32 I = 1, 36
          NCL2(I) = NMOD2(I) - NMOD3(I)
 32   CONTINUE
C*
C*    For 1994 data onwards fill the array NAUTO and NAUTOW from the
C*    NEW AUTOPEDS banks EHWI and EHPA. Unpack these in new routine
C*    EAUTOP.
      IF ( IRUNRC .GT. IRUN94 ) THEN
          CALL EAUTOP ( NAUTO , NAUTOW )
      END IF
C?
C?    Pack Run Quality info into ECRQ bank
C?
      CALL ECRUNQ ( NCL1 , NCL2 , NAUTO )
      NSUPEC=N2-N3
C?
C?         READ RAW MC_DATA
C?
      CALL BLIST(IW,'E+','ESDA')
      CALL BLIST(IW,'E+','ECLU')
C
      KETDI  = IW(NAETDI)
C? To build banks of zero length in case of no Ecal data
      IF (KETDI.EQ.0) THEN
      CALL AUBOS('ESDA',0,LMHLEN,KESDA,IGARB)
      IF (IGARB.EQ.2) GOTO 999
      IW(KESDA + LMHCOL) = LESDAA
      IW(KESDA + LMHROW) = 0
      GOTO 900
      ENDIF
      LETDIA = IW(KETDI + LMHCOL )
      NTOW   = IW(KETDI + LMHROW )
C? Verify on line bank consistency
      IF (IW(KETDI) .NE. LETDIA*NTOW+2) GOTO 990
      KETDI  = KETDI + LMHLEN
C?
C?    CREATE BANK ESDA
C?
      ND  = LESDAA*NTOW*3 + LMHLEN
      CALL AUBOS('ESDA',0,ND,KESDA,IGARB)
      IF (IGARB.EQ.2) GOTO 999
C? RECOMPUTE POINTER ETDI IN CASE OF GARBAGE COLLECTION
      KETDI = IW(NAETDI) + LMHLEN
      IW(KESDA + LMHCOL) = LESDAA
      IW(KESDA + LMHROW) = NTOW*3
      KESDA       = KESDA + LMHLEN
C? Get On_Line Calibrations
C? if ESCA present ,bank ECMC is read
C? If run number lower than 6000 (1989 data),
C?        do not apply platines corections
C
       IF (IW(NAMIND('ESCA')).EQ.0 .OR. IW(NAMIND('ECMC')).EQ.0.
     +  OR.IRUNRC.LT.6000) THEN
       DO 50 I=1,36
 50       PLACO(I)=1.
                        ELSE
      KECMC=IW(NAMIND('ECMC'))
       DO 51 I=1,36
 51       PLACO(I)=RTABL(KECMC,I,JECMCO)
                        ENDIF
C?-- FILL ESDA BOS_BANK
      DO 20 ITOW = 1,NTOW
C
C- Correct for gas flow calibration dependance
        IADD = IW(KETDI + 1)
        CALL ECGFLW (IADD,CFL)
        DO  52 IK=1,3
          ERN = FLOAT(IW(KETDI+1+IK))*CFL
          IW(KETDI+1+IK) = ERN
   52   CONTINUE
        DO 10 K = 1,3
            IW(KESDA + JESDTJ) = IBITS(IADD,16,8)
            IW(KESDA + JESDFI) = IBITS(IADD, 2,9)
            IW(KESDA + JESDDK) = K
C? ----- ENCODING FOR CLUSTERING
            J = IW(KESDA + JESDTJ)
            I = IW(KESDA + JESDFI)
C       protect against bad data
            IF(J.LT.1 .OR. J.GT.228)GOTO 990
            NRE = NUMREG(J)-INT(NUMREG(J)/5)
            IF(I.LT.1 .OR. I.GT.96*NRE)GOTO 990
            CALL ECINIT(I,J,K,IORG,NDIR)
            IW(KESDA + JESDIO) = IORG
            IW(KESDA + JESDDI) = NDIR
C? MODULE NUMBER (1..36)
            CALL ECMOD(I,J,IMOD)
      CALL ECPLM(IMOD,K,EWEI)
            IW(KESDA + JESDER) = IMOD
C? --- ENERGY IN GEV
            ESTO = 1.E-6*FLOAT(IW(KETDI+K+1))*PLACO(IMOD)
      RW(KESDA + JESDME)=ESTO*EWEI
C? REGION NUMBER (1..5)
            IW(KESDA + JESDSC) = NUMREG (J)
C? FLAG FOR DEAD STOREYS TREATMENT
            IW(KESDA+JESDED)=0
C
            KESDA = KESDA + LESDAA
   10    CONTINUE
         KETDI = KETDI + LETDIA
   20 CONTINUE
C
      RETURN
 900  IF (JDBDRF(JULEC) .NE. 0) THEN
      CALL RERROR(
     1  'EPREDA ',1,' ETDI bank missing. ECAL not processed')
      ENDIF
      RETURN
 990  CALL RERROR(
     1  'EPREDA ',3,' ETDI bank data inconsistent - drop it')
      CALL BDROP(IW,'ETDIESDA')
      RETURN
C? ERROR IN CASE OF BOS MEMORY FULL
 999  CALL RERROR('EPREDA ',
     1 2,' Bos memory full.ESDA not created.ECAL not processed')
      RETURN
      END
#endif
