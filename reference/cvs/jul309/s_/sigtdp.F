      SUBROUTINE SIGTDP
C------------------------------------------------------------------
C!  - Get dead pad information from the header (SDPD and SKPD banks)
C!  and create SMPD bank ( Missing pad bank)
C!
C!  AUTHOR  :  B. Bloch-Devaux   june 1992
C!
C!  BANKS:
C!    INPUT  : SDPD , SKPD
C!    OUTPUT : SMPD
C!    CREATED: SMPD
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "rcurnt.h"
#include "sinamc.h"
#include "smpdjj.h"
#include "sdpdjj.h"
#include "skpdjj.h"
#include "rparac.h"
#include "rflags.h"
      INTEGER ALGTDB, GTSTUP
      EXTERNAL ALGTDB, GTSTUP
      DIMENSION IADD(16)
      PARAMETER ( IAMPX = 16384)
      PARAMETER ( IADMX = 28672)
      CHARACTER DET*2,LIST*4
      PARAMETER( NBK = 2 )
      CHARACTER*4 BKLIS (NBK)
C
      DATA DET / 'SI' /
C
C?   SDPD : SICAL permanent dead pads
C?   SKPD : SICAL killed pads in the Event builder
C
      DATA BKLIS /  'SDPD', 'SKPD' /
C
#include "bmacro.h"
C
      NTOTW  = LMHLEN
      NBAD   = 0
      NKILL  = 0
C
C?   bank present on run header ?
C
      IND = NDROP('SMPD',0)
      CALL BLIST(IW,'C+','SMPD')
C
      DO 20 IBK =  1,  NBK
        LIST  = BKLIS (IBK)
        KLIST = IW( NAMIND(LIST) )
        IF ( KLIST.EQ.0 ) THEN
#include "gtdbbk.h"
          KLIST = IW( NAMIND(LIST) )
          IF ( KLIST.EQ.0 ) THEN
            WRITE (LOUTRL,'(A)') ' No '//LIST//
     &        ' bank found for this run'
          ELSE
            WRITE (LOUTRL,'(A)') '    '//LIST//' taken from DataBase '
          ENDIF
        ELSE
          WRITE (LOUTRL,'(A)') '    '//LIST//' taken from Run Header '
C  if list found on file is empty or too short , try Data Base for SDPD
          IF (LIST.EQ.'SDPD' .AND. LROWS(KLIST).LE.8) THEN
#include "gtdbbk.h"
            WRITE (LOUTRL,
     &        '(A)') '    '//LIST//' had bad length ,try DB '
          ENDIF
        ENDIF
   20 CONTINUE
C
C
C?   Redefine bank indices
C
      KSDPD = IW( NAMIND('SDPD') )
      KSKPD = IW( NAMIND('SKPD') )
      IF ( KSDPD.GT.0) THEN
        IF (LCOLS(KSDPD).NE.1) NBAD   = LCOLS(KSDPD)
        IF (LCOLS(KSDPD).NE.1) WRITE(LOUTRL,'(A)') 'SDPD SWITCH COL-ROW'
        IF (LCOLS(KSDPD).EQ.1) NBAD   = LROWS(KSDPD)
      ENDIF
      IF ( KSKPD.GT.0) THEN
        IF (LCOLS(KSKPD).NE.1) NKILL = LCOLS(KSKPD)
        IF (LCOLS(KSKPD).NE.1) WRITE (LOUTRL,'(A)')'SKPD SWITCH COL-ROW'
        IF (LCOLS(KSKPD).EQ.1) NKILL = LROWS(KSKPD)
      ENDIF
      WRITE (LOUTRL,'(5X,I6,A)') NBAD  ,
     $                          '   Dead Elements (SDPD) Read In'
      WRITE (LOUTRL,'(5X,I6,A)') NKILL,
     $                          ' Killed Elements (SKPD) Read In'
C
C --- CREATE THE SMPD BANK
C
      NTOTW=LSMPDA*(NBAD  +NKILL)*16+LMHLEN
      CALL AUBOS('SMPD',0,NTOTW,KSMPD,IGARB)
      IF(IGARB.EQ.2) GOTO 997
      IW(KSMPD+LMHCOL)=LSMPDA
      IW(KSMPD+LMHROW)= 0
C
C IN CASE OF GARBAGE COLLECTION
C
      KSDPD = IW( NAMIND('SDPD') )
      IF(KSDPD.EQ.0) GO TO 15
      IF ( NBAD  .EQ.0 ) GO TO 15
C
      DO 10 IB=1,NBAD
        IADDS=ITABL (KSDPD,IB,JSDPAD)
        IF ( IADDS.GT.IADMX .OR. IADDS.LE.0) THEN
           WRITE ( LOUTRL,'(A)')  ' *** SIGTDP *** INVALID SDPD ADDRESS'
           GO TO 10
        ENDIF
        IADD(1) = IADDS
        NDA = 1
        IF (IADDS.GE.IAMPX) CALL SIDAMP(IADDS,IADD,NDA)
        DO 11 J = 1,NDA
          IND= KNEXT(KSMPD)
          IW(IND+JSMPAD)=IADD(J)
          IW(KSMPD+LMHROW)= IW(KSMPD+LMHROW)+1
   11   CONTINUE
   10 CONTINUE
C-
   15 CONTINUE
C! Read dead channels from  SKPD...
      KSKPD=IW(NAMIND('SKPD'))
      IF (KSKPD .EQ. 0) GOTO 12
      IF ( NKILL.EQ.0 ) GO TO 12
      KSKPD=IW(NAMIND('SKPD'))
C
      DO 13 IB=1,NKILL
        IADDS=ITABL (KSKPD,IB,JSKPAD)
        IF ( IADDS.GT.IADMX .OR. IADDS.LE.0) THEN
           WRITE ( LOUTRL,'(A)')  ' *** SIGTDP *** INVALID SKPD ADDRESS'
           GO TO 13
        ENDIF
        NDA = 1
        IADD(1) = IADDS
        IF (IADDS.GE.IAMPX) CALL SIDAMP(IADDS,IADD,NDA)
        DO 14 J = 1,NDA
          IND= KNEXT(KSMPD)
          IW(IND+JSMPAD)=IADD(J)
          IW(KSMPD+LMHROW)= IW(KSMPD+LMHROW)+1
   14   CONTINUE
   13 CONTINUE
C
   12 CONTINUE
C
      CALL AUBPRS ('SMPD')
      IF(JDBDRF(JULSI).GT.0) THEN
        KSMPD = IW(NAMIND('SMPD'))
        IF ( KSMPD.LE.0) GO TO 999
        WRITE (LOUTRL,1000)
        DO 21 II = 1 ,LROWS(KSMPD)
          IAD = ITABL(KSMPD,II,JSMPAD)
          CALL SIDCOZ(IAD,MODU,IPL,IPHI,IR)
          WRITE (LOUTRL,1001) IAD,MODU,IPL,IPHI,IR
   21   CONTINUE
      ENDIF
C
      GOTO 999
C
  997 CALL RERROR('SIGTDP',2,
     1  '   BOS memory full. SMPD bank not created')
C
  999 RETURN
 1000 FORMAT(5X,'missing pad address Module  Plane    Phi Radius')
 1001 FORMAT(14X,I10,4I7)
      END
#endif
