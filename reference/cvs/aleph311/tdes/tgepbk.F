      SUBROUTINE TGEPBK
C
C------------------------------------------------------------------
C! Print content of BOS -banks referring to the TPC
C!
C!     Author:    R. Richter      2-06-87
C!      !!!   This S.R. will have to be changed for versions
C!      !!!   of ADBSDAF > 105 in order to show the additional
C!      !!!   constants in TCGD and in TMTY.  R. Richter 11.05.88
C!
C!     Called by USER program after call to TRDDAF
C!
C!--------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "bcs.h"
      CHARACTER*4 CHAINT
C
C   Now print the following BOS - banks  with PRTABL
C          ---- TGCD, TSLO, TMTY, TSGM ----
      WRITE (IW(6),'(/1X,''+++TGEPBK+++ TPC BOS bank printout''/)')
C
C     Dump 'TGCD'
C     -----------
      I1 = IW(NAMIND('TCGD'))
      IE = IW (I1) + I1
      NR = IW(I1-2)
      WRITE (IW(6),1001) NR
 1001 FORMAT(//1X,'-------------- START printout of TCGD',
     &          ', NR =',I5)
      CALL PRTABL ('TCGD',NR)
C
      WRITE (IW(6),191)
     +      (IW(II),II=I1,I1+5),CHAINT(IW(I1+6)),
     +      (IW(II),II=I1+7,I1+9),(RW(II),II=I1+10,IE)
C
C     Dump 'TSLO'
C     -----------
      I1 = IW(NAMIND('TSLO'))
      IE = IW (I1) + I1 - 4
      NR = IW(I1-2)
      WRITE (IW(6),1002) NR
 1002 FORMAT(//1X,'-------------- START printout of TSLO',
     &          ', NR =',I5)
      CALL PRTABL ('TSLO',NR)
C
      WRITE (IW(6),192) (IW(II),II=I1,IE)
C
C     Dump 'TMTY'
C     -----------
      IA    = IW(NAMIND('TMTY'))
      IREP  = IW (IA+1)
      IE    = IA + 2 * IREP
      ISTEP = IREP
      NR = IW(IA-2)
      WRITE (IW(6),1003) NR
 1003 FORMAT(//1X,'-------------- START printout of TMTY',
     &       ', NR =',I5)
      CALL PRTABL ('TMTY',NR)
C
          WRITE (IW(6),193)    (IW(II),II=IA,IA+2),
     +                         ((IW(II),II=I1+3,I1+5),CHAINT(IW(II+6)),
     +   (RW(II),II=I1+7,I1+8), (IW(II),II=I1+ 9,I1+21),
     +    RW(I1+22),            (IW(II),II=I1+23,I1+34),
     +    RW(I1+35),            (IW(II),II=I1+36,I1+39),
     +    RW(I1+40),             IW(I1+41), I1 = IA, IE, ISTEP)
C
C     Dump 'TSGM'
C     -----------
      IA    = IW(NAMIND('TSGM'))
      IE    = IA + 2 * 15
      ISTEP = 15
      NR = IW(IA-2)
      WRITE (IW(6),1004) NR
 1004 FORMAT(//1X,'-------------- START printout of TSGM',
     &       ', NR =',I5)
      CALL PRTABL ('TSGM',NR)
C
      WRITE (IW(6),194) (IW(II),II=IA,IA+2),
     +      ((IW(II),II=I1+3,I1+6), (RW(II),II=I1+7,I1+16),
     +        IW(I1+17), I1= IA, IE, ISTEP)
C
  190 FORMAT (3X,'\ Message fr S.R. TGEPBK: \ Content of BOS-BANK #',
     +        I3,' (',A4,'):'/3X,61('-')//)
C
C                                             TCGD:
C                                             -----
  191 FORMAT (5X,3I10//
     +        5X,3I10,6X,A4//
     +        7X,'# of subcomp.   crowns   half-pads'//
     +        5X,3I10//
     +        7X,'AngSpa    RadStp    PadHgt    PadSep    PadWid    ',
     +           'GapWid    EdgpWd    EdgSep    BorDis'//
     +        5X,9F10.6//
     +        7X,'TpdStp    TpdHgt    WirPit    WirDia'//
     +        5X,4F10.4//
     +        7X,'TPC outer dimensions...',8X,'and   wall thicknesses'//
     +        5X,3F10.4,5X,3F10.4//
     +        7X,'"WHEEL" - ribs: thickness, equiv. thickn.,  width'//
     +        5X,3F10.4//
     +        7X,'"WHEEL" - ribs: 3 constants for offset of kinks'//
     +        5X,3F10.4//)
C
C                                             TSLO:
C                                             -----
  192 FORMAT (5X,3I10//
     +        8X,'#',8X,'Date',8X,'SloNam      SlonmB   ',
     +           'SuperSect  rDsSlo   AdsSlo   RotSlo   ',
     +           'Modtyp   endpl#'//
     +   3(12(5X,I4,2X,2I8,4X,A4,2X,2I9,1X,3I9,1X,2I9/)/)//)
C
C                                             TMTY:
C                                             -----
  193 FORMAT (5X,3I10//:
     +      3(8X,'#',8X,'Date',8X,'Modtyp    PhiPos'//
     +        5X,I4,2X,2I8,4X,A4,2X,F10.4//
     +        8X,'RadPadrow ',3X,'NtPdrw   PadspR (1...12)'//
     +        5X,F10.4,I8,2X,4(2X,3I4)//
     +        8X,'RadTpadrow',3X,'NtpTrw   TppRow (1...11)'//
     +        5X,F10.4,I8,2X,3(2X,3I4),2X,2I4//
     +        8X,'Phi position of Tpad - boundary (1./2. pad):',F10.4//
     +        8X,'Constants for comput. of Tpad electr. channels',//
     +        5X,4I8//
     +        8X,'Rad 1.wire  NtWire'//
     +        5X,F10.4,I8//)//)
C
C                                             TSGM:
C                                             -----
  194 FORMAT (5X,3I10// 3(5X,3I10//
     +        5X,'CORNERS are: ( 2,',I2,' )',T60,'SECTOR-TYPE'/
     +     2(/5X,5F10.4),T60,I6//))
C
      RETURN
      END
#endif
