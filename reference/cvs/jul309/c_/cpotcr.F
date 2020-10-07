      SUBROUTINE CPOTCR(IER)
C--------------------------------------------------------------------
C! Create bank for POT output
C!
C!   Author :  JP Albanese   20 06 88
C!   Modified: C Benchouk Aug. 89,Jan.90
C!
C!       Banks :
C!        Input : ESDA,ECOB,CALO,EHYP,EPAR,EBOS,COCR...
C!        Output: PCOB,PCRL,PCHY,PCPA,PEOB,PEHYP,PEST,PKST
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "ecnamc.h"
#include "hcnamc.h"
#include "ccnamc.h"
#include "pcnamc.h"
#include "esdajj.h"
#include "eclujj.h"
#include "ehypjj.h"
#include "eparjj.h"
#include "ebosjj.h"
#include "pcobjj.h"
#include "pcrljj.h"
#include "pestjj.h"
#include "pkstjj.h"
#include "pchyjj.h"
#include "pcpajj.h"
#include "peobjj.h"
#include "peotjj.h"
#include "pehyjj.h"
#include "ecobjj.h"
#include "etckjj.h"
#include "cocrjj.h"
#include "calojj.h"
#include "cct1jj.h"
#include "ctc2jj.h"
#include "crl3jj.h"
#include "bmacro.h"
C
C Clear previous events banks ?
      KPCRL = NDROP('PCRL',0)
      KPCOB = NDROP('PCOB',0)
      KPEOB = NDROP('PEOB',0)
      KPEOT = NDROP('PEOT',0)
      KPEHY = NDROP('PEHY',0)
      KPCHY = NDROP('PCHY',0)
      KPCHY = NDROP('PCPA',0)
C
      KCALO = NLINK('CALO',0)
      IER=0
      IF (KCALO.LE.0) GOTO 295
      NCALO=LROWS(KCALO)
C
C  Max number of pot_calobjects
C
      NPCOB=NCALO
C
C?  Create CalObjects bank  ( 0 column !)
      CALL AUBOS('PCOB',0,NPCOB*LPCOBA+LMHLEN,KPCOB,IGARB)
      IF(IGARB.EQ.2)GO TO 298
C?   Fill miniheader
      IW(KPCOB+LMHCOL)=LPCOBA
      IW(KPCOB+LMHROW)=NPCOB
      NCOCR=0
      NCCT1=0
      KCOCR=IW(NACOCR)
      IF (KCOCR.NE.0) NCOCR=LROWS(KCOCR)
      KCCT1=IW(NACCT1)
      IF (KCCT1.NE.0) NCCT1=LROWS(KCCT1)
C Copy PCRL from CHRL (if present)
      KCHRL=IW(NACHRL)
      IF(KCHRL.EQ.0)GO TO 297
      NDATA=LROWS(KCHRL)*LCOLS(KCHRL)+LMHLEN
      CALL AUBOS('PCRL',0,NDATA,KPCRL,IGARB)
      IF(IGARB.EQ.2)GO TO 296
      KCHRL=IW(NACHRL)
      CALL UCOPY(IW(KCHRL+1),IW(KPCRL+1),NDATA)
      GO TO 400
  296 CALL RERROR('CPOTCR',1,'Too little space to create PCRL bank')
      GO TO 290
  298 CALL RERROR('CPOTCR',3,'Too little space to create PCOB bank')
      GO TO 290
  297 CALL RERROR('CPOTCR',2,'CHRL bank missing')
      GO TO 290
  295 CALL RERROR('CPOTCR',4,'CALO bank missing')
  290 IER=1
  400 CONTINUE
C Create PEOB
      KECOB=IW(NAECOB)
      IF (KECOB.EQ.0) GOTO 393
      NECOB=LROWS(KECOB)
      NPEOB=NECOB
      CALL AUBOS('PEOB',0,NPEOB*LPEOBA+LMHLEN,KPEOB,IGARB)
      IF(IGARB.EQ.2)GO TO 394
      IW(KPEOB+LMHCOL)=LPEOBA
      IW(KPEOB+LMHROW)=NPEOB
      KECOB=IW(NAECOB)
C Fill PEOB data
      DO 20 IC=1,NPEOB
      RW(KROW(KPEOB,IC)+JPEOER)=RTABL(KECOB,IC,JECOEG)
      RW(KROW(KPEOB,IC)+JPEOE1)=RTABL(KECOB,IC,JECOEN)/
     1                          RTABL(KECOB,IC,JECOEG)
      RW(KROW(KPEOB,IC)+JPEOE2)=RTABL(KECOB,IC,JECOEN+1)/
     1                          RTABL(KECOB,IC,JECOEG)
      RW(KROW(KPEOB,IC)+JPEOEC)=RTABL(KECOB,IC,JECOEG)
      IW(KROW(KPEOB,IC)+JPEOKD)=ITABL(KECOB,IC,JECORC)
      RW(KROW(KPEOB,IC)+JPEOR1)=RTABL(KECOB,IC,JECOIT)
      RW(KROW(KPEOB,IC)+JPEOR2)=RTABL(KECOB,IC,JECOIT+1)
 20   CONTINUE
      NPEOT=NECOB
C Fill PEOT for all ECOB (to be updated)
      CALL AUBOS('PEOT',0,NPEOT*LPEOTA+LMHLEN,KPEOT,IGARB)
      IF(IGARB.EQ.2)GO TO 396
      IW(KPEOT+LMHCOL)=LPEOTA
      IW(KPEOT+LMHROW)=NPEOT
C Fill PEOT data
      KECLU=IW(NAECLU)
      IF (KECLU.EQ.0) GOTO 210
      DO 21 IC=1,NPEOT
C! JECLS1,2,3 not yet filled !
      RW(KROW(KPEOT,IC)+JPEOT1)=RTABL(KECLU,IC,JECLT1)
      RW(KROW(KPEOT,IC)+JPEOP1)=RTABL(KECLU,IC,JECLF1)
      RW(KROW(KPEOT,IC)+JPEOS1)=RTABL(KECLU,IC,JECLS1)
      RW(KROW(KPEOT,IC)+JPEOT2)=RTABL(KECLU,IC,JECLT2)
      RW(KROW(KPEOT,IC)+JPEOP2)=RTABL(KECLU,IC,JECLF2)
      RW(KROW(KPEOT,IC)+JPEOS2)=RTABL(KECLU,IC,JECLS2)
      RW(KROW(KPEOT,IC)+JPEOT3)=RTABL(KECLU,IC,JECLT3)
      RW(KROW(KPEOT,IC)+JPEOP3)=RTABL(KECLU,IC,JECLF3)
      RW(KROW(KPEOT,IC)+JPEOS3)=RTABL(KECLU,IC,JECLS3)
      IW(KROW(KPEOT,IC)+JPEOPE)=IC
 21   CONTINUE
 210  CONTINUE
      KEHYP=IW(NAEHYP)
      NPEHY=0
      IF (KEHYP.NE.0) THEN
        NEHYP=LROWS(KEHYP)
        NPEHY=NEHYP
        CALL AUBOS('PEHY',0,NPEHY*LPEHYA+LMHLEN,KPEHY,IGARB)
        IF(IGARB.EQ.2)GO TO 398
      IW(KPEHY+LMHCOL)=LPEHYA
      IW(KPEHY+LMHROW)=NPEHY
      ELSE
        GO TO 397
      ENDIF
C Fill PEHY data
      KEHYP=IW(NAEHYP)
      DO 22 ID=1,NPEHY
      IW(KROW(KPEHY,ID)+JPEHKT)=ITABL(KEHYP,ID,JEHYTY)
      IW(KROW(KPEHY,ID)+JPEHBH)=0
      IW(KROW(KPEHY,ID)+JPEHPE)=ITABL(KEHYP,ID,JEHYEC)
 22   CONTINUE
      GO TO 310
  394 CALL RERROR('CPOTCR',5,'Too little space to create PEOB bank')
      GO TO 390
  396 CALL RERROR('CPOTCR',5,'Too little space to create PEOT bank')
      GO TO 390
  398 CALL RERROR('CPOTCR',7,'Too little space to create PEHY bank')
      GO TO 390
  393 CONTINUE
C      CALL RERROR('CPOTCR',6,'ECOB bank missing')
      GO TO 390
  397 CALL RERROR('CPOTCR',8,'EHYP bank missing')
  390 IER=1
  310 CONTINUE
C
C? Copy PCHY from CHYP (if present)
      IF (KCALO.LE.0) GOTO 410
C
      KCHYP=NLINK('CHYP',0)
      IF(KCHYP.EQ.0)GO TO 493
      NDATA=LROWS(KCHYP)*LCOLS(KCHYP)+LMHLEN
      CALL AUBOS('PCHY',0,NDATA,KPCHY,IGARB)
      IF(IGARB.EQ.2)GO TO 494
      KCHYP=NLINK('CHYP',0)
      CALL UCOPY(IW(KCHYP+1),IW(KPCHY+1),NDATA)
C
C? Copy PCPA from CPAR (if present)
C
      KCPAR=NLINK('CPAR',0)
      IF(KCPAR.EQ.0)GO TO 495
      NDATA=LROWS(KCPAR)*LCOLS(KCPAR)+LMHLEN
      CALL AUBOS('PCPA',0,NDATA,KPCPA,IGARB)
      IF(IGARB.EQ.2)GO TO 496
      KCPAR=NLINK('CPAR',0)
      CALL UCOPY(IW(KCPAR+1),IW(KPCPA+1),NDATA)
C
C? Copy PPRL from CHPR (if present)
C
      KCHPR=NLINK('CHPR',0)
      IF(KCHPR.EQ.0)GO TO 497
      NDATA=LROWS(KCHPR)*LCOLS(KCHPR)+LMHLEN
      CALL AUBOS('PPRL',0,NDATA,KPPRL,IGARB)
      IF(IGARB.EQ.2)GO TO 498
      KCHPR=NLINK('CHPR',0)
      CALL UCOPY(IW(KCHPR+1),IW(KPPRL+1),NDATA)
C
      GO TO 410
  494 CALL RERROR('CPOTCR',9,'Too little space to create PCHY bank')
      GO TO 490
  496 CALL RERROR('CPOTCR',11,'Too little space to create PCPA bank')
      GO TO 490
  498 CALL RERROR('CPOTCR',13,'Too little space to create PPRL bank')
      GO TO 490
  493 CONTINUE
C  493 CALL RERROR('CPOTCR',10,'CHYP BANK MISSING')
      GO TO 490
  495 CONTINUE
C  495 CALL RERROR('CPOTCR',12,'CPAR BANK MISSING')
      GO TO 490
  497 CONTINUE
C  497 CALL RERROR('CPOTCR',14,'CHPR BANK MISSING')
  490 IER=1
 410  CONTINUE
C?   Update indices, in case of garbage collection
C
      KECOB=IW(NAECOB)
      KPEOB=IW(NAPEOB)
C?
C?    do for each ecob
C?      get (loop) ebos (iebos)
C?        get esda
C?             fill pest, pkst with esda infos
C?      endloop ebos
C?    endloop ecob
      KEBOS=IW(NAEBOS)
      IF (KEBOS.EQ.0) GOTO 595
      KESDA=IW(NAESDA)
C
      NESDA=LROWS(KESDA)
      NEBOS=LROWS(KEBOS)
      NPEST=NEBOS
      NPKST=NEBOS
C
C Get total number of towers from ETDI not to point from PEST
C to Dead storeys added to ESDA (JPESET word)
      KETDI=IW(NAETDI)
      NETDI=LROWS(KETDI)
C
      CALL AUBOS('PEST',0,NPEST*LPESTA+LMHLEN,KPEST,IGARB)
      IF(IGARB.EQ.2)GO TO 596
      IW(KPEST+LMHCOL)=LPESTA
      IW(KPEST+LMHROW)=NPEST
C
      CALL AUBOS('PKST',0,NPKST*LPKSTA+LMHLEN,KPKST,IGARB)
      IF(IGARB.EQ.2)GO TO 597
      IW(KPKST+LMHCOL)=LPKSTA
      IW(KPKST+LMHROW)=NPKST
C
C
C?  Restore pointers to other banks, in case of garbage collection
C
      KECOB=IW(NAECOB)
      KPEST=IW(NAPEST)
      KESDA=IW(NAESDA)
      KEBOS=IW(NAEBOS)
C
      NECOB=LROWS(KECOB)
      DO 300 IEC=1,NECOB
C? get ebos and esda to fill pest,pkst
      IEBOS=ITABL(KECOB,IEC,JECOEB)
  305 IF (IEBOS.EQ.0) GOTO 300
      IESDA=ITABL(KEBOS,IEBOS,JEBOES)
      IW(KROW(KPEST,IEBOS)+JPESKS)=ITABL(KESDA,IESDA,JESDDK)
      RW(KROW(KPEST,IEBOS)+JPESER)=RTABL(KEBOS,IEBOS,JEBOFR)*
     1                          RTABL(KESDA,IESDA,JESDME)
      IW(KROW(KPEST,IEBOS)+JPESPE)=IEC
C
      IW(KROW(KPKST,IEBOS)+JPKSAD)=
     +      ITABL(KESDA,IESDA,JESDDK)
     +    + ITABL(KESDA,IESDA,JESDFI) * 2**2
     +    + ITABL(KESDA,IESDA,JESDTJ) * 2**16
      RW(KROW(KPKST,IEBOS)+JPKSER)=RTABL(KEBOS,IEBOS,JEBOFR)*
     1                          RTABL(KESDA,IESDA,JESDME)
      IW(KROW(KPKST,IEBOS)+JPKSPE)=IEC
C
      IETDI=((IESDA-1)/3)+1
      IF(IETDI.LE.NETDI) THEN
        IW(KROW(KPEST,IEBOS)+JPESET)=IETDI
      ELSE
        IW(KROW(KPEST,IEBOS)+JPESET)=0
      ENDIF
C -- FOR RESCUED DEAD STOREYS
      IW(KROW(KPEST,IEBOS)+JPESED)=ITABL(KESDA,IESDA,JESDED)
      IF(ITABL(KESDA,IESDA,JESDED).GT.0) IW(KROW(KPKST,IEBOS)+JPKSAD)=
     +  IW(KROW(KPKST,IEBOS)+JPKSAD) + 2**26
      IEBOS=ITABL(KEBOS,IEBOS,JEBOEB)
      GOTO 305
 300  CONTINUE
      GO TO 301
  596 CALL RERROR('CPOTCR',16,'Too little space to create PEST bank')
      GO TO 590
  597 CALL RERROR('CPOTCR',17,'Too little space to create PKST bank')
      GO TO 590
  595 CALL RERROR('CPOTCR',15,'EBOS bank missing')
  590 IER=1
 301  CONTINUE
      RETURN
      END
#endif
