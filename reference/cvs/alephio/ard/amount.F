      SUBROUTINE AMOUNT(LUN,ANAME,ATYPE,FDEVI,IER )
C----------------------------------------------------------------------
CKEY ALREAD MOUNT TAPE CARTRIDGE ALPHARUN IBM
C   Author: M.Rumpf                 February 1990
C! Purpose   : Mount tape/cartridge IBM only
C            : MOUNT requested in FILI card as option
C            : Same drive address used : 181 for tape - 288 for cart.
C            : Send MOUNT command for next medium
C            : Send FILEDEF  command
C            : Send LABELDEF command if Tape or cart. labelled
C  All SETUPs must be done previously in exec file (ALPHARUN)
C
C   Inputs    : LUN      logical unit
C
C               FNAME    DSN of tape file, can be ' ' if NL tape
C                        is used or if you wish to ignore the dsn
C
C               ATYPE    'NATIVE'  for native fortran
C                        'EPIO'    for EPIO
C               FDEVI     'TAPE tapeid options' or
C                         'CART tapeid options'
C
C               where 'tapeid' is 'vsn.fseq.label' and 'options' which
C               contain at least MOUNT (mandatory)
C
C   Outputs   : IER  = 0  successful opening
C                    = 1  File number /= 1 on TAPE/CART  (multifile)
C                    = 2  Error mounting next medium
C                    = 3  Error on FILEDEF
C                    = 4  Error on LABELDEF
C ==== IBM example =============== Multifile not allowed
C  FILI 'DSNAME |EPIO | CART AC0021.1.SL MOUNT' mount - no staging
C  FILI '       |NATIVE|TAPE 03558W.1.NL MOUNT'
C  FILI 'ALDATA |EPIO | CART AF0056.1.SL.AF0056 MOUNT'
C   Calls:    ACLOSE , VMCMS
C   Called by AOPTAP
C -------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "alrcom.h"
#include "albitw.h"
#include "gtchdecl.h"
      CHARACTER     BL
      CHARACTER*2   CHLAB ,CHSEQ
      CHARACTER*4   CHNAME
      CHARACTER*6   CHVOL
      CHARACTER*30  CHPARM
      CHARACTER*80  MESS
      LOGICAL FIRSC, FIRST
      DATA FIRSC /.TRUE./, FIRST /.TRUE./
      DATA BL/' '/
C----------------------------------------------------------------------
C
      IER   = 0
      SUBR  = ' AMOUNT '
      FNAME = ANAME
      MESS  = FNAME
#include "prfname.h"
#if defined(IBM)
C
C - get file type and record length, close previous file, send file
C   name to EPIO
#include "gtftyp.h"
C - get logical unit as character variable
#include "gtchunit.h"
C - get medium, tape no. and option list
#include "gtvsn.h"
C
C Don't process multifile CART/TAPE - fatal
C
      NPT = INDEX(CHTAPE,'.')
      CHSEQ = CHTAPE(NPT+1:NPT+1)
      IF(CHSEQ.NE.'1')  THEN
        MESS =' Error : file sequence # > 1 '
        IST = 1
        GO TO 98
      ENDIF
C
      NPT1 = INDEX (CHTAPE,'.')
      NPT2 = NPT1 + 2
C
C Check if requested tape/cart is labelled
C
      CHLAB = CHTAPE(NPT2+1:NPT2+2)
      CHVOL = CHTAPE(1:NPT1-1)
C
      CHRECL = ' '
      CALL CSETDI (KRECL,CHRECL,1,5)
      IF ( FTYP3.EQ.'EPI' ) THEN
           CHPARM = '(RECFM U BLKSIZE '//CHRECL
      ELSE
           CHPARM = '(RECFM VBS BLKSIZE '//CHRECL
      ENDIF
C
      IRC = 0
C
C     CARTRIDGE REQUEST - Mount on drive address 288  (tap8)
C
      IF(CHMED.EQ.'CART')     THEN
         CHNAME = 'TAP8'
         IF(.NOT.FIRSC)       THEN
           MSG = 'MOUNT ON 288'
#include "prmsg.h"
           CALL VMCMS(MSG,IST)
         ENDIF
      ENDIF
C
C     TAPE REQUEST      - Mount on drive address 181 (tap1)
C
      IF(CHMED.EQ.'TAPE')     THEN
         CHNAME = 'TAP1'
         IF (.NOT.FIRST)        THEN
           MSG = 'MOUNT ON 181'
#include "prmsg.h"
           CALL VMCMS(MSG,IST)
         ENDIF
      ENDIF
      IF(IST.NE.0) THEN
         MESS = 'Error mounting '//CHMED//BL//CHVOL
         GOTO 98
#if defined(ARDEB)
      ELSE
C In any case (SETUP or MOUNT)
         WRITE(IW(6),*) ' +++ AMOUNT +++ ',CHMED,BL,CHVOL,' mounted'
#endif
      ENDIF
C
C Send FILEDEF
C
      MSG = 'FILEDEF '//DSNAME//BL//CHNAME//BL//CHLAB
      LE = LNBLNK(MSG)
      MSG = MSG(1:LE)//BL//CHSEQ//BL//CHPARM
#include "prmsg.h"
      CALL VMCMS(MSG,IST)
      IF(IST.NE.0) THEN
          MESS = ' Error in FILEDEF for '//CHMED//BL//CHVOL
          GOTO 98
       ENDIF
C
C Send LABELDEF if SL
      IF (CHLAB.EQ.'SL')   THEN
         MSG = 'LABELDEF '//DSNAME//' VOLID '//CHVOL//' FID '//FNAME
#include "prmsg.h"
         CALL VMCMS(MSG,IST)
         IF(IST.NE.0) THEN
            MESS = ' Error in LABELDEF for '//CHMED//BL//CHVOL
            GOTO 98
         ENDIF
      ENDIF
C
C - prepare for next medium
      IF (CHMED.EQ.'CART') FIRSC = .FALSE.
      IF (CHMED.EQ.'TAPE') FIRST = .FALSE.
C
C----------------------------------------------------------------------
C                  define BOS file format  (FORT,EPIO,TEXT)
C----------------------------------------------------------------------
      BFORM2 = 'SEQ'
#include "stbforma.h"
C
      RETURN
C
C Errors
C
  98  CONTINUE
      WRITE(IW(6),*) ' +++ AMOUNT +++ ',IST,MESS
      IER = 2
#endif
      END
#endif
