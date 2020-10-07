      SUBROUTINE HPRANA(NHVST)
C-------------------------------------------------------------------
C! prepare Hcal analog data
C
C      G.Capon               date : 861001
C      modified R.Tenchini
C!      fill HSDA bank
C!      output : NHVST = number of valid storeys
C!                     (over threshold KTHRE)
C!      called by HPREDA
C?
C!======================================================================
#ifndef DOC
      EXTERNAL HINTST
      LOGICAL TEST,HINTST,FIRST
#include "bcs.h"
#include "hcnamc.h"
#include "hstadd.h"
#include "rcurnt.h"
#include "rlunit.h"
#include "hdeach.h"
#include "hcjosu.h"
#include "hsdajj.h"
#include "hstojj.h"
#include "hcinjj.h"
      PARAMETER(IFFFF=65535)
      DATA KTHRE,NERR/1,0/
      DATA FIRST /.TRUE./
#include "bmacro.h"
      IF(FIRST) THEN
         KHOFF=NLINK('HOFF',0)
         FIRST=.FALSE.
      ENDIF
C
C?             KTHRE= adc threshold to validate a storey !!
C?             ( set to zero provisionally )
C
      NHVST=0
      JHTDI=IW(NAHTDI)
      IF (JHTDI.EQ.0) GO TO 99
      NTOW=IW(JHTDI+2)
      NHSTO=2*NTOW
      LHTDI=IW(JHTDI+1)
      NSTKS=LHTDI-1
C
C?             create HSDA bank
C
      LNHSD=2+NHSTO*LHSDAA
      CALL AUBOS('HSDA',0,LNHSD,JHSDA,IGARB)
      IF (IGARB.EQ.2) THEN
         CALL RERROR('HPRANA',1,'no space for HSDA bank')
         RETURN
      ENDIF
C
C?             create HSTO bank
C
      LNHST=2+NHSTO*LHSTOA
      CALL AUBOS('HSTO',0,LNHST,JHSTO,IGARB)
      IF (IGARB.EQ.2) THEN
         CALL RERROR('HPRANA',2,'no space for HSTO bank')
         RETURN
      ENDIF
      KHSTO=JHSTO+2
C
C     Access  HCIN (cable inversions)
C
      NAHCIN=NAMIND('HCIN')
      JHCIN=IW(NAHCIN)
      IF(JHCIN.EQ.0) THEN
         NHCIN=0
      ELSE
         NHCIN=LROWS(JHCIN)
      ENDIF
C
C?             loop on fired towers
C?             one tower = storey 1 + storey 2
C?             unpack address + adc data
C?             NHVST = number of valid (over threshold) storeys
C
      JHSDA=IW(NAHSDA)
      JHTDI=IW(NAHTDI)
      KHSDA=JHSDA+2
      KHTDI=JHTDI+2
      NAHNTO=NAMIND('HNTO')
      JHNTO=IW(NAHNTO)
      DO 10 N=1,NTOW
         IADDHS=IW(KHTDI+1)
         IADOLD=IADDHS
         DO 100 JIN=1,NHCIN
            INVW=ITABL(JHCIN,JIN,JHCIWA)
            IF(IADDHS.EQ.INVW) THEN
               INVC=ITABL(JHCIN,JIN,JHCICA)
               IF(INVC.EQ.0) GO TO 12
               IADDHS=INVC
               GOTO 101
            ENDIF
  100    CONTINUE
  101    CONTINUE
         IPHIHS=IBITS(IADDHS,0,16)
         ITETHS=IBITS(IADDHS,16,8)
         IREGHS=IBITS(IADDHS,24,8)
C?             check indices within expected range
         TEST=HINTST(ITETHS,IPHIHS,1)
         IF(.NOT.TEST)THEN
            NERR=NERR+1
            CALL RERROR('HPRANA',5,'hcal analog addr out of range')
            IF (NERR.GT.10) GO TO 12
            WRITE(LDEBRL,*)
     +      ' routine HPRANA:Hcal analog address out of range - ',
     +      'RUN,EV = ', IRUNRC,IEVTRC
            WRITE(LDEBRL,*)' nregio,indphi,inteta =',
     +      IREGHS,IPHIHS,ITETHS
            GO TO 12
         ENDIF
C?           get module #, subcomp #
         CALL HFNDMD(ITETHS,IPHIHS,ISTKHS,ISUBHS,IMODHS,IOVRHS)
C                 skip missing subcomponents 1=EA, 2=barrel, 3=EB
         IF (IHSUBC(ISUBHS).EQ.0) GO TO 12
         DO 11 ISTKHS=1,NSTKS
            IADC=IW(KHTDI+1+ISTKHS)
            IADC=IAND(IADC,IFFFF)
C                     handle noisy towers
            IF (JHNTO.GT.0) THEN
               DO 21 NR=1,LROWS(JHNTO)
               IF( IADOLD .EQ. ITABL(JHNTO,NR,1) ) THEN
                  INOIE=ITABL(JHNTO,NR,2)
                  IF(INOIE.GE.IECUT1) GO TO 11
                  IADC=IADC-INOIE
                  IF(IADC.LE.IECUT2) GO TO 11
                  GO TO 22
               ENDIF
 21            CONTINUE
            ENDIF
 22         EGEV=0.001*FLOAT(IADC)
            EGEV=EGEV*HCATOW(ITETHS,IPHIHS)
            IF(IADC.GT.KTHRE)THEN
               NHVST=NHVST+1
               NSTOHC(ISTKHS)=NSTOHC(ISTKHS)+1
               IW(KHSDA+JHSDTI)=ITETHS
               IW(KHSDA+JHSDPI)=IPHIHS
               IW(KHSDA+JHSDSN)=ISTKHS
               RW(KHSDA+JHSDDE)=EGEV
               ETOTHC(ISTKHS)=ETOTHC(ISTKHS)+EGEV
               IW(KHSDA+JHSDSC)=ISUBHS
               IW(KHSDA+JHSDMN)=IMODHS
               IW(KHSDA+JHSDRN)=IREGHS
               KHSDA=KHSDA+LHSDAA
               CALL HSTOFI(KHSTO)
            ENDIF
   11    CONTINUE
 12      KHTDI=KHTDI+LHTDI
   10 CONTINUE
C
C             drop banks HSDA,HSTO if bank HSDA is empty
C
      IF (NHVST.EQ.0) THEN
         IND=NDROP('HSDA',0)
         IND=NDROP('HSTO',0)
C         CALL RERROR('HPRANA',5,'bank HTDI exists but HSDA is empty')
         GO TO 99
       ENDIF
C
C?            fill bank headers, adjust bank length
C
      LNHSD=2+NHVST*LHSDAA
      CALL AUBOS('HSDA',0,LNHSD,JHSDA,IGARB)
      IF (IGARB.EQ.2) CALL RERROR('HPRANA',3,'no update of HSDA bank')
      IW(JHSDA+1)=LHSDAA
      IW(JHSDA+2)=NHVST
C
      LNHST=2+NHVST*LHSTOA
      CALL AUBOS('HSTO',0,LNHST,JHSTO,IGARB)
      IF(IGARB.EQ.2) CALL RERROR('HPRANA',4,'no update of HSTO bank')
      IW(JHSTO+1)=LHSTOA
      IW(JHSTO+2)=NHVST
C
C            apply single tower calibration factors
C
      IPOTG=0
      IF(KHOFF.EQ.0) CALL HCALIB(IPOTG)
 99   RETURN
      END
#endif
