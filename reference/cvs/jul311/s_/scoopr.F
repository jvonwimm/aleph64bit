      SUBROUTINE SCOOPR(LUN)
C----------------------------------------------------------------------
C! Print the SCOO bank
C!
C!    Author:     H. Meinhard       10-Feb-1988
C!    Modified:   H. Meinhard       09-Oct-1989  (4)
C!
C!    Input:      - LUN       / I   logical unit for output
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "sanamc.h"
#include "scoojj.h"
#include "scrpjj.h"
#include "sgeomc.h"
#include "srtdjj.h"
      CHARACTER*1 CAB(2)
      DATA CAB / 'A','B' /
#include "bmacro.h"
#include "smacro.h"
C----------------------------------------------------------------------
C link to banks
      KSCOO = IW(NASCOO)
      KSCRP = IW(NASCRP)
      KSRTD = IW(NASRTD)
      IF (KSCOO .EQ. 0)                                     GOTO 901
C
C if raw data and pointers available, choose full output format
      IF (KSCRP .NE. 0 .AND. KSRTD .NE. 0) THEN
C
C if banks exist, but no entries, print corresponding message
        IF (LROWS(KSCOO) .EQ. 0) THEN
          WRITE (LUN,500)
        ELSE
          WRITE (LUN,510)
          DO 300 ICO = 1, LROWS(KSCOO)
            IRP = ITABL(KSCRP,ICO,JSCRCR)
            IWORD = ITABL(KSRTD,IRP,JSRTIR)
            WRITE (LUN,520) ICO,CAB(ITABL(KSCOO,ICO,JSCOSI)),
     +      ITABL(KSCOO,ICO,JSCOLA),ITABL(KSCOO,ICO,JSCOSC),
     +      RTABL(KSCOO,ICO,JSCOTT),RTABL(KSCOO,ICO,JSCOTT+1),
     +      IRP,JCRAT(IWORD),JCARD(IWORD),JTDCN(IWORD),JTDCC(IWORD),
     +      REAL(ITABL(KSRTD,IRP,JSRTTR))/1000.
  300     CONTINUE
        ENDIF
C
      ELSE
C
C no raw data, no pointers. Print only SCOO bank
        IF (LROWS(KSCOO) .EQ. 0) THEN
          WRITE (LUN,500)
        ELSE
          WRITE (LUN,610)
          DO 310 ICO = 1, LROWS(KSCOO)
            WRITE (LUN,620) ICO,CAB(ITABL(KSCOO,ICO,JSCOSI)),
     +      ITABL(KSCOO,ICO,JSCOLA),ITABL(KSCOO,ICO,JSCOSC),
     +      RTABL(KSCOO,ICO,JSCOTT),RTABL(KSCOO,ICO,JSCOTT+1)
  310     CONTINUE
        ENDIF
C
      ENDIF
C
      GOTO 999
C----------------------------------------------------------------------
  901 IF (KSCOO .EQ. 0) WRITE (LUN,530) 'SCOO'
      GOTO 999
  999 CONTINUE
      RETURN
C
C Formats
  500 FORMAT (9X,'No coordinates found.')
  510 FORMAT (
     +  '  Nr   Side   Layer   Sector   L tan theta   H tan theta',
     +  '   RDWord   Crate   Card   TDC-Nr   TDC-Cont   TDC-Time(ns)')
  520 FORMAT (I4,5X,A1,I7,I9,E16.4,E14.4,I7,3I8,I11,F15.4)
  530 FORMAT (' --> SCOOPR: Bank ',A4,' not found')
  610 FORMAT (
     +  '  Nr   Side   Layer   Sector   L tan theta   H tan theta')
  620 FORMAT (I4,5X,A1,I7,I9,E16.4,E14.4)
      END
#endif
