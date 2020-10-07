      SUBROUTINE EGETDS
C******************************************************************
C!  GET DEAD STOREY INFORMATION FROM THE DBASE   (EDDB BANK)
C!  AND CREATE EDST BANK
C!
C!  AUTHOR  :  C. BENCHOUK 890529
C!  MODIFIED:  JP ALBANESE 890915
C!   Modified :- E. Lancon              4-DEC-1990
C!   Modified :- E. Lancon              7-OCT-1992
C!     Use GTDBBK common deck to access run header/ daf setup dep. banks
C!
C!  BANKS:
C!    INPUT  : EDDB , EKLS , EGLO
C!    OUTPUT : EDST
C!    CREATED: EDST
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "rcurnt.h"
#include "ecnamc.h"
#include "edstjj.h"
#include "eddbjj.h"
#include "eglojj.h"
#include "eklsjj.h"
#include "edkbjj.h"
#include "egnrec.h"
#include "eclcom.h"
#include "rparac.h"
#include "rflags.h"
      INTEGER ALGTDB, GTSTUP
      EXTERNAL ALGTDB, GTSTUP
      PARAMETER( NBK = 3 , NBKL = 4*NBK )
      CHARACTER DET*2,LIST*(NBKL)
      DIMENSION IMIS (NBK)
      CHARACTER*4 BKLIS (NBK)
C
C?   EKLS : ECAL killed storeys in the ROC
C?   EDDB : ECAL Dead storeys
C?   EKPL : ECAL killed planes in the ROC
C
      DATA BKLIS /  'EKLS', 'EDDB', 'EKPL' /
      DATA IROLD / 0 /
      DATA DET   / 'EC' /
C
#include "bmacro.h"
C
      ENDEAD = 0.0001
      NTOTW  = LMHLEN
      NBAD   = 0
      NKILL  = 0
      NGLO   = 0
      CALL VZERO(IDRRAY,6144)
C
C-    For 94 running drop EKLS EKPL wrong on the header
C
      CALL EFIXI94
C
C
C?   Is there some banks missing on Run Header
C
      NMIS = 0
      DO 30 IB =  1,  NBK
        IF ( IW(NAMIND(BKLIS(IB))).EQ.0 ) THEN
          WRITE (LOUTRL,'(A)') '    '//BKLIS(IB)//
     &      ' NOT found on Run Header'
          NMIS = NMIS + 1
          IMIS (NMIS) = IB
        ELSE
          WRITE (LOUTRL,'(A)') '    '//BKLIS(IB)//
     &      '     found on Run Header'
        ENDIF
   30 CONTINUE
C
C?   Try to get missing banks from DAF
C
      IF ( NMIS.NE.0 ) THEN
        LIST = BKLIS( IMIS(1) )
        IF ( NMIS.GT.1 ) THEN
          DO 20 IBK =  2,  NMIS
            LIST  = LIST(1:LNBLNK(LIST)) // BKLIS ( IMIS(IBK) )
   20     CONTINUE
        ENDIF
#include "gtdbbk.h"
        DO 23 IB =  1,  NMIS
          KLIST = IW( NAMIND(BKLIS(IMIS(IB))) )
          IF ( KLIST.EQ.0 ) THEN
            WRITE (LOUTRL,'(A)') ' No '//BKLIS(IMIS(IB))//
     &          ' bank found on DataBase '
          ELSE
            WRITE (LOUTRL,'(A)') '    '//BKLIS(IMIS(IB))//
     &        ' taken from DataBase '
          ENDIF
   23   CONTINUE
      ENDIF
C
C?   Redefine bank indices
C
      KEDDB = IW( NAMIND('EDDB') )
      KEKLS = IW( NAMIND('EKLS') )
      KEKPL = IW( NAMIND('EKPL') )
C
      NRDDB = IW(KEDDB-2)
      NBAD  = LROWS (KEDDB)
      WRITE (LOUTRL,'(5X,I6,A)') NBAD  ,
     &  ' Dead   Storeys (EDDB) Read In'
C
C --- CREATE THE EDST BANK
C
      IF(KEDDB.NE.0) THEN
        NTOTW=LEDSTA*NBAD  +LMHLEN
        CALL AUBOS('EDST',0,NTOTW,KEDST,IGARB)
        IF(IGARB.EQ.2) GOTO 999
C
C IN CASE OF GARBAGE COLLECTION
        KEDDB = NLINK('EDDB',NRDDB)
        IW(KEDST+LMHCOL) = LEDSTA
        IW(KEDST+LMHROW) = NBAD
C
C
        DO 10 IB=1,NBAD
          INDSB = KROW (KEDST,IB)
          IADDI = ITABL (KEDDB,IB,JEDDAD)
          IADDS = ICASWT(IADDI,IRUNRC)
          JVAL = IBITS(IADDS,16,8)
          IVAL = IBITS(IADDS,2,9)
          KVAL = IBITS(IADDS,26,2)
          IW(INDSB+JEDSTJ) = JVAL
          IW(INDSB+JEDSFI) = IVAL
          IW(INDSB+JEDSDK) = KVAL
C
          CALL ECINIT(IVAL,JVAL,KVAL,IORG,NDIR)
          IW(INDSB+JEDSIO) = IORG
          IW(INDSB+JEDSDI) = NDIR
          CALL ECMOD(IVAL,JVAL,IMOD)
          IW(INDSB+JEDSER) = IMOD
          IW(INDSB+JEDSSC) = NUMREG(JVAL)
          IW(INDSB+JEDSES) = 0
          RW(INDSB+JEDSME) = ENDEAD
C
C
          IA    = (IORG-1) / 73728
          IR    =  IORG - 73728*IA
          IMT2  = (IR-1) / 6144
          IMT1  =  IR - 6144*IMT2
C
          IF(IDRRAY(IMT1).EQ.0) THEN
            IDRRAY(IMT1)=IB
            IW(INDSB+JEDSED)=0
          ELSE
C THERE IS ANOTHER STOREY AT THE SAME POSITION IN ITS BANANA
            IW(INDSB+JEDSED)=IDRRAY(IMT1)
            IDRRAY(IMT1)=IB
          ENDIF
   10   CONTINUE
C
      ENDIF
C! Read dead channels from  EDKB...(ROC)
      KEKLS = IW(NAMIND('EKLS'))
      NKILL = IW(KEKLS+LMHROW)
      WRITE (LOUTRL,'(5X,I6,A)') NKILL,
     &  ' Killed Storeys (EKLS) Read In'
      IF (KEKLS .EQ. 0) GOTO 12
C Expand EDST
      CALL AUBOS('EDST',0,NTOTW+NKILL*LEDSTA,KEDST,IGARB)
      IF (IGARB .EQ. 2) GOTO 999
      KEKLS = IW(NAMIND('EKLS'))
      IW(KEDST+LMHCOL) = LEDSTA
      IW(KEDST+LMHROW) = NBAD + NKILL
C
      DO 11 IB = NBAD + 1, NBAD + NKILL
        IB1 = IB - NBAD
        INDSB = KROW (KEDST,IB)
        IADDS = ITABL (KEKLS,IB1,JEKLAD)
        JVAL = IBITS(IADDS,16,8)
        IVAL = IBITS(IADDS,2,9)
        KVAL = IBITS(IADDS,26,2)
        IF(KVAL.EQ.0) KVAL = IBITS(IADDS,0,2)
        IW(INDSB+JEDSTJ) = JVAL
        IW(INDSB+JEDSFI) = IVAL
        IW(INDSB+JEDSDK) = KVAL
C
        CALL ECINIT(IVAL,JVAL,KVAL,IORG,NDIR)
        IW(INDSB+JEDSIO) = IORG
        IW(INDSB+JEDSDI) = NDIR
        CALL ECMOD(IVAL,JVAL,IMOD)
        IW(INDSB+JEDSER) = IMOD
        IW(INDSB+JEDSSC) = NUMREG(JVAL)
        IW(INDSB+JEDSES) = 0
        RW(INDSB+JEDSME) = ENDEAD
C
C
        IA    = (IORG-1) / 73728
        IR    =  IORG - 73728*IA
        IMT2  = (IR-1) / 6144
        IMT1  =  IR - 6144*IMT2
C
        IF(IDRRAY(IMT1).EQ.0) THEN
          IDRRAY(IMT1) = IB
          IW(INDSB+JEDSED) = 0
        ELSE
C THERE IS ANOTHER STOREY AT THE SAME POSITION IN ITS BANANA
          IW(INDSB+JEDSED) = IDRRAY(IMT1)
          IDRRAY(IMT1) = IB
        ENDIF
   11 CONTINUE
C
   12 CONTINUE
C
C-     Call routine to kill ECAL  boxes
C
      IREADB = ALGTDB (LRCONS,'EDKB',IRUNRC)
      CALL EBOXKI
      KEDKB=IW(NAMIND('EDKB'))
      IF (KEDKB .EQ. 0) GOTO 22
      NKILB = IW(KEDKB+LMHROW)
C Expand EDST
      CALL AUBOS('EDST',0,NTOTW+(NKILL+NKILB)*LEDSTA,KEDST,IGARB)
      IF (IGARB .EQ. 2) GOTO 999
      KEDKB = IW(NAMIND('EDKB'))
      IW(KEDST+LMHCOL) = LEDSTA
      IW(KEDST+LMHROW) = NBAD + NKILL
      NUM1 = NBAD   + NKILL
C
      IB0 = NUM1
      IBC = IB0 + 1
      DO 21 IB = NUM1+1, NUM1+NKILB
        IB1 = IB-NUM1
        INDDB = KEDKB+LMHLEN+(IB1-1)*LEDKBA
        INDSB = KEDST+LMHLEN+(IBC-1)*LEDSTA
        IADDS = IW(INDDB+JEDKAD)
        JVAL = IBITS(IADDS,16,8)
        IVAL = IBITS(IADDS,2,9)
        KVAL = IBITS(IADDS,26,2)
        IF(KVAL.EQ.0) KVAL = IBITS(IADDS,0,2)
C
C-    Check not yet in dead list
C
        CALL EDLIST (JVAL,IVAL,KVAL,IB0,IOK)
        IF ( IOK .NE. 0 )  GO TO 21
        IBC = IBC + 1
        IW(INDSB+JEDSTJ) = JVAL
        IW(INDSB+JEDSFI) = IVAL
        IW(INDSB+JEDSDK) = KVAL
C
        CALL ECINIT(IVAL,JVAL,KVAL,IORG,NDIR)
        IW(INDSB+JEDSIO) = IORG
        IW(INDSB+JEDSDI) = NDIR
        CALL ECMOD(IVAL,JVAL,IMOD)
        IW(INDSB+JEDSER) = IMOD
        IW(INDSB+JEDSSC) = NUMREG(JVAL)
        IW(INDSB+JEDSES) = 0
        RW(INDSB+JEDSME) = ENDEAD
C
C
        IA    = (IORG-1) / 73728
        IR    =  IORG - 73728*IA
        IMT2  = (IR-1) / 6144
        IMT1  =  IR - 6144*IMT2
C
        IF(IDRRAY(IMT1).EQ.0) THEN
          IDRRAY(IMT1) = IBC-1
          IW(INDSB+JEDSED) = 0
        ELSE
C THERE IS ANOTHER STOREY AT THE SAME POSITION IN ITS BANANA
          IW(INDSB+JEDSED) = IDRRAY(IMT1)
          IDRRAY(IMT1) = IBC-1
        ENDIF
   21 CONTINUE
      IW(KEDST+LMHROW) = IBC-1
      CALL AUBPRS ('EDST')
C
   22 CONTINUE
      RETURN
C
  999 CALL RERROR('EGETDS',2,

     1  '   BOS memory full. EDST bank not created')
      RETURN
      END
#endif
