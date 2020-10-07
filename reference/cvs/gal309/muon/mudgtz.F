*DK mudgtz
      SUBROUTINE MUDGTZ
C
C*******************************************************************************
C
C T.Wang -860112
C
C           - modified for final dbase
C             by A. Antonelli, F. Bossi 1 July 89
C
C       Routine to digitize all signals ( track signals
C       and track uncorrelated noises) and fill digitization
C       information in BOS bank 'MUDG', fill digi-track cross reference
C       information in BOS bank 'MUDT' and 'MUTD'.
C
C       Called by MUDIGI
C       Calls ALBOS,MUFIDI,MUFLDT,MUFLTD       in this .HLB
C       Calls BKFMT,BLIST                      in BOS
C       Calls SORTZV                           in CERNLIB
C
C*******************************************************************************
C
#ifndef DOC
      SAVE
#include "mudgpr.h"
#include "muhtjj.h"
#include "mudgjj.h"
#include "mthtjj.h"
#include "munamc.h"
#include "bcs.h"
#include "jobcom.h"
#include "iocom.h"
      INTEGER ISTP(MXPHT),IHIT(MXPHT)
      INTEGER INDX(MXPHT),IADHT(MXPHT),IDTRK(MXPHT)
      PARAMETER (NBAR=34,NECA=116,NEMA=200)
      PARAMETER (NAS1=100,NAS2=166)
C
C
#include "bmacro.h"
C
C       Link to BOS bank 'MUHT'
C
      JMUHT = IW(NAMUHT)
      IF ( JMUHT .EQ. 0) RETURN
      IF (LROWS( JMUHT) .EQ. 0) RETURN
      NMUHT = LROWS( JMUHT)
C - Test the array length
      IF (NMUHT .GT. MXPHT) THEN
       CALL ALTELL('MUDGTZ: too many hits for local arrays',2,'RETURN')
       NMUHT = MXPHT
       IW(JMUHT+LMHROW) = NMUHT
      ENDIF
C
C       Create BOS bank  'MUDT'
C
      KMDT1 = IW(NAMUDT)
      IF( KMDT1 .EQ. 0 )THEN
         ND = NMUHT + LMHLEN
         CALL ALBOS ('MUDT',1,ND,KMDT1,IGARB)
         IW(KMDT1+1) = 1
         CALL BLIST(IW,'E+','MUDT')
         CALL ALBOS ('MUDT',2,ND,KMDT2,IGARB)
         IW(KMDT2+1) = 1
         CALL ALBOS ('MUDT',3,NMUHT*NAVTD+LMHLEN,KMDT3,IGARB)
         IW(KMDT3+1) = 1
      ENDIF
C
C       Create BOS bank 'MUDG and 'JDMTHT'
C
      JMUDG = IW(NAMUDG)
      IF( JMUDG .EQ. 0 )THEN
         ND = LMHLEN + NMUHT*LMUDGA
         CALL ALBOS ('MUDG',0,ND,JMUDG,IGARB)
         CALL BLIST(IW,'E+','MUDG')
         IW(JMUDG + LMHCOL) = LMUDGA
      ENDIF
C
      MXWRD = NMUHT*LMTHTA
      IF( JDMTHT .NE. 0 ) CALL WDROP(IW,JDMTHT)
      CALL WBANK(IW,JDMTHT,MXWRD+LMHLEN,*998)
      IW(JDMTHT-3) = INTCHA('MTHT')
      IW(JDMTHT+LMHCOL) = LMTHTA
C
C
C               Filling of temporary bank JDMTHT
C
      JMUHT = IW(NAMUHT)
      KMUHT = JMUHT + LMHLEN
      KMTHT = JDMTHT+ LMHLEN
      NHIT = 0
      DO 110 ISGNL = 1,NMUHT
         JEMD = IW(KMUHT + JMUHEL)
         JPLN = IW(KMUHT + JMUHSP)
         JSTP = IW(KMUHT + JMUHSA)
C
C      Find  bus channel and astros
C
C      Barrel
         IF(JEMD.LE.NBAR)
     &     CALL MUINCB(JEMD,JPLN,JSTP,NAST,NBUS,NSTP,IFLAG)
C
C      End-Cap
C
         IF(JEMD.GT.NBAR.AND.JEMD.LE.NECA)
     &     CALL MUINCE(JEMD,JPLN,JSTP,NAST,NBUS,NSTP,IFLAG)
C
C      Middle angle
C
         IF(JEMD.GT.NEMA)
     &     CALL MUINCM(JEMD,JPLN,JSTP,NAST,NBUS,NSTP,IFLAG)
         IF (IFLAG.EQ.0) GO TO 110
         NHIT = NHIT + 1
         IW(KMTHT+JMTHW1) = NAST
         IW(KMTHT+JMTHW2) = NBUS
         IW(KMTHT+JMTHW3) = NSTP
         KMTHT = KMTHT + LMTHTA
 110  KMUHT = KMUHT + LMUHTA
      IW(JDMTHT+LMHROW) = NHIT
C
C   Loop on Astros number
C
      DO 270 IEMD = NAS1,NAS2,2
      DO 260 IBUS = 0,25
C
C       Preset MHIT (# of hit in current plane) to 0
C
            MHIT = 0
C
C       Loop of signals in 'JDMTHT'
C
            KMTHT = JDMTHT + LMHLEN
            DO 210 ISGNL = 1,NHIT
               JEMD = IW(KMTHT + JMTHW1)
               IF( JEMD .NE. IEMD )GOTO 210
               JBUS = IW(KMTHT + JMTHW2)
               IF( JBUS .NE. IBUS )GOTO 210
               JSTP = IW(KMTHT + JMTHW3)
               MHIT = MHIT + 1
               ISTP(MHIT) = JSTP
               IHIT(MHIT) = ISGNL
 210        KMTHT = KMTHT + LMTHTA
C
            IF( MHIT .EQ.  0)GOTO 260
C
C       Sort array ISTP(*), ISTP(*) is not changed, the sorting
C       results are kept in INDX(*)
C
            CALL SORTZV(ISTP,INDX,MHIT,-1,0,0)
C
C       Find cluster
C
            LCLST = 1
            IFRST = ISTP(INDX(1))
            IOLST = IFRST
            NHTCL = 0
            DO 240 I=1,MHIT
               ID = INDX(I)
               IF( ID .EQ.  0)GOTO 240
               INWST = ISTP(ID)
               NJUMP = INWST - IOLST
               IF( NJUMP .EQ.  0)THEN
                  NHTCL = NHTCL + 1
                  IADHT(NHTCL) = IHIT(ID)
               ELSE IF( NJUMP .EQ.  1)THEN
                  NHTCL = NHTCL + 1
                  IADHT(NHTCL) = IHIT(ID)
                  LCLST = LCLST + 1
                  IF( LCLST .EQ. 8)THEN
                     J = I
 220                 IF( J .EQ. MHIT )GOTO 230
                     J = J + 1
                     ID = INDX(J)
                     INEXT = ISTP(ID)
                     IF( INEXT .EQ. INWST )THEN
                        NHTCL = NHTCL + 1
                        IADHT(NHTCL) = IHIT(ID)
                        INDX(J) = 0
                        GOTO 220
                     ELSE
                        CALL MUFLDI(IEMD,IBUS,LCLST,INWST)
                        CALL MUFLDT (LROWS(JMUDG),NHTCL,IADHT)
                        LCLST = 1
                        IFRST = INEXT
                        INWST = INEXT
                        NHTCL = 0
                     ENDIF
                  ENDIF
               ELSE
                  CALL MUFLDI(IEMD,IBUS,LCLST,IOLST)
                  CALL MUFLDT (LROWS(JMUDG),NHTCL,IADHT)
                  LCLST = 1
                  NHTCL = 1
                  IFRST = INWST
                  IADHT(1) = IHIT(ID)
               ENDIF
               IOLST = INWST
C
               IF( I .NE. MHIT )GOTO 240
 230           CALL MUFLDI(IEMD,IBUS,LCLST,IOLST)
               CALL MUFLDT (LROWS(JMUDG),NHTCL,IADHT)
 240        CONTINUE
 250        CONTINUE
 260     CONTINUE
 270   CONTINUE
C
C       Fill track-digi cross-reference BOS bank 'MUTD'
C
CCC   CALL MUFLTD
C
C
C
 998    CONTINUE
        RETURN
C
      END
#endif
