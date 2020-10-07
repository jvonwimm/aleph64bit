      SUBROUTINE QFPTPX 
C ---------------------------------------------------------------------
C - Author: J. Boucrot   981207                     
C!    Rebuild the PTPX bank for pad dE/dX if necessary
C     Called from QFILL, not executed if data card NOPX is present
C-    Runs on POTs or DSTs only (needs full TPC coordinates)              
C ---------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "rhahjj.h"
      SAVE
      PARAMETER (JVRMI=300 , JVRC1=30803 , JVRC2=30804 , JVRC3=30806)
      PARAMETER (NRUMC=2000, NRF98=45000, NRL98=47920, ILPOT=3, ILDST=4)
      EXTERNAL CHAINT
      CHARACTER*4 CHAINT*4,PRNAM*8
      DATA IPASS,NPASS,IRUN / 0 , 0,  -1 /
      DATA NER1,NER2 / 0 , 0 /
#include "qmacro.h"
C ---------------------------------------------------------------------
C Whenever a new run is found, define the conditions for recreating PTPX
C (MPTPX=1) or not (MPTPX=0) for this run :
C      
      IF (KRUN.EQ.IRUN) GO TO 100
      IRUN=KRUN
      MPTPX=0
      IPASS=IPASS+1
C Data card NOPX intended to avoid the rebuilding of PTPX
C must be ignored if MINI official production : 
      INOPX=0
      IF (IW(NAMIND('MINP')).EQ.0) INOPX=IW(NAMIND('NOPX')) 
C The unpacking of all coordinates and their sorting is mandatory
C to be able to rebuild PTPX :
      IUNPK=1
      IF (INDEX(CQUNPK,'AL').EQ.0.AND.INDEX(CQUNPK,'SO').EQ.0)
     +    IUNPK=0
C Find which type of data/MC (LEP1 or LEP2) one is dealing with :
      CALL MINDTYP(ILEPD)
C
C- Find nature of data and JULIA version from RHAH bank:
C
      JRHAH = IW(NAMIND('RHAH'))
      IF (JRHAH.LE.0) GO TO 999
C
      NRHAH=LROWS(JRHAH)
      IPOTI = 0
      IJVER = 0
      ICVER = 0
      DO 10 IRHAH = 1, NRHAH
        PRNAM(1:4) = CHAINT(ITABL(JRHAH,IRHAH,JRHAPN))
        PRNAM(5:8) = CHAINT(ITABL(JRHAH,IRHAH,JRHAPN+1))
        CALL CLTOU(PRNAM)
C JULIA version :
        IF (INDEX(PRNAM,'JULIA').GT.0) THEN
          ICVER = ITABL(JRHAH,IRHAH,JRHACV)
          IJVER = ITABL(JRHAH,IRHAH,JRHAPV)
        ENDIF 
C Input stream is a POT if it is the last row of RHAH with 
C nature of data = ILPOT :
C Input stream is a DST if it is the last row of RHAH with
C nature of data = ILDST :
        IF (IRHAH.EQ.NRHAH) THEN
             IDAT=ITABL(JRHAH,IRHAH,JRHANO)
             IF (IDAT.EQ.ILPOT.OR.IDAT.EQ.ILDST) IPOTI=1
       ENDIF
 10   CONTINUE
C
C-   Determine if 'PTPX' has to be rebuilt :
C
C Input file is not a POT or a DST : 
      IF (IPOTI.EQ.0) GO TO 100
C 
C LEP1 Monte-Carlo : rebuild PTPX if Julia version >= 30806
C
      IF (ILEPD.EQ.1.AND.KRUN.LE.NRUMC) THEN
         IF (ICVER.GE.JVRC3) THEN
            IF (INOPX.GT.0) THEN 
               CALL QWMESS(
     +     '_QFPTPX_ Warning ! Pad dE/dX calibrations not standard !!')
            ELSE   
               MPTPX=1
            ENDIF
         ENDIF  
         GO TO 50
      ENDIF
C
C LEP1 Data : rebuild PTPX if JULIA version >= 300
C                                              and  <= 30803
C
      IF (ILEPD.EQ.1.AND.KRUN.GT.NRUMC) THEN
         IF (IJVER.GE.JVRMI.AND.ICVER.LE.JVRC1) THEN
            IF (INOPX.GT.0) THEN
               CALL QWMESE(
     +     '_QFPTPX_ Warning ! Pad dE/dX calibrations will be WRONG !')
            ELSE
               MPTPX=1
            ENDIF
         ENDIF
         GO TO 50 
      ENDIF
C
C LEP2 data of 1998 reprocessed with JULIA >= 30804 : do nothing   
C
      IF (ILEPD.NE.2) GO TO 100
      IY98=0
      IF (KRUN.GE.NRF98.AND.KRUN.LE.NRL98) IY98=1
      IF (IY98.EQ.1.AND.ICVER.GE.JVRC2) GO TO 100
C
C LEP2 MCarlo processed with JULIA >= 30806 : do nothing   
C
      IF (KRUN.LE.NRUMC.AND.ICVER.GE.JVRC3) GO TO 100
C
C For all other LEP2 data/MCarlo rebuild PTPX, unless the
C data card 'NOPX' is present :
C
      IF (INOPX.GT.0) GO TO 100
      MPTPX=1
C
C Print a warning if MPTPX = 1 and no UNPK 'AL SO' card:
C
 50   IF (MPTPX.EQ.1.AND.IUNPK.EQ.0.AND.IPASS.EQ.1) WRITE (IW(6),1001)
C -----------------------------------------------------------------------
C Rebuild the PTPX bank if needed :
C
 100  IF (MPTPX.EQ.0) GO TO 999 
      NPASS=NPASS+1
      IF (NPASS.EQ.1) THEN
         CALL QWMESE('_QFPTPX_ Rebuild PTPX bank from POT') 
      ENDIF
C
C Get the pad dE/dX from the POT banks :
C
      CALL QGTPDDX(KRUN,KEVT,IRE1)
      IF (IRE1.NE.0) THEN      
         NER1=NER1+1
         IF (NER1.LE.10) WRITE(IW(6),1002) KRUN,KEVT,IRE1
         GO TO 999
      ENDIF
C
C Create the TPXS bank of truncated means for pad dE/dX :
C
      CALL QDOTPXS
C
C Build the PTPX bank from the TPXS bank :
C
      CALL QWRPTPX(IRE2)
 200  IF (IRE2.NE.0) THEN 
         NER2=NER2+1
         IF (NER2.LE.10) WRITE(IW(6),1003) KRUN,KEVT,IRE2
      ENDIF
C
 999  RETURN
 1001 FORMAT (/' *** WARNING in QFPTPX ===> Pad dE/dX bank PTPX '/,
     +        1x,'*** required to be rebuilt without UNPK AL SO card'/
     +        1x,'*** Results will be WRONG ! ***'/)
 1002 FORMAT (' _QFPTPX_ run, event ',2I7,' Error # ',I5,' in ',
     +        ' QGTPDDX')
 1003 FORMAT (' _QFPTPX_ run, event ',2I7,' Error # ',I5,' in ',
     +        ' QWRPTPX') 
      END
#endif

