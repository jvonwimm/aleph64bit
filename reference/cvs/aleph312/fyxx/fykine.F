      SUBROUTINE FYKINE(DROP,GARB,IBFUL)
C ------------------------------------------------------
CKEY FYXX MCARLO KINE / USER
C - F.Ranjard - 871202             B.Bloch 901010,980316
C! translate KINGAL or GALEPH banks to equivalent Fxxx MC banks
C The banks are KINE,VERT,KHIS,KVOL --> FKIN,FVER 
C               KPOL,KZFR,KSHO      --> FPOL,FZFR,FSHO
C Drop tracks and vertices depending on flags set at beginning of run
C through a call to FYIRUN : A CALL TO FYIRUN IS MANDATORY.
C If there is a problem with space, the old banks are not dropped.
C
C - input arguments :
C        DROP     = character flag
C                   if = 'DROP' then drop KINE,VERT,KHIS,KVOL,KPOL 
C                               KSHO and KZFR banks
C        GARB     = character flag
C                   if = 'GARB' then make a garbage collection
C
C - output argument :
C        IBFUL    = -1 means not enough space in BOS array
C
C - this routine must be called once per event after reading
C - USER routine
#ifndef DOC
      SAVE
#include "bcs.h"
#include "fyrela.h"
      CHARACTER*(*) DROP,GARB
      DATA IONC /0/
C -------------------------------------------------------------
C
      IF (IONC.EQ.0) THEN
         NKINE = NAMIND('KINE')
         IONC = 1
      ENDIF
C Test if the KINE banks are present. Do nothing if not.
      IF (IW(NKINE).LE.0)  GOTO 991
C - initialize the event
      IBFUL = 0
      CALL FYINEV(IBFUL)
      IF (IBFUL .EQ. -1)  GOTO 990
C
C - Build a tree of mother and daugther tracks in JDMOTH,JDTREE
C   from KINE and VERT which content tracks and vertices to be kept
C   fill JDKOFN which gives the old KINE# for each new track# and
C        JDKNFO which gives the new track# for each old KINE#
C   JDMOTH and JDTREE are dropped before return
      CALL FYTREE (IBFUL)
      IF (IBFUL .EQ. -1) GOTO 990
C
C - Fill FKIN, FVER, FVOL,FPOI using KINE,VERT and all working banks
      CALL FYFKIN(IBFUL)
      IF (IBFUL .EQ. -1)  GOTO 990
C
C
C - tidy
C
C - Drop work banks and temporary banks
      IF (DROP.EQ.'DROP') CALL BDROP(IW,'KINEVERTKHISKVOLKPOLKSHOKZFR')
      IF (GARB .EQ. 'GARB') CALL BGARB (IW)
C
 990  CONTINUE
C
C - workbanks
      IW(1) = LFXWBK
      CALL WDROP(IW,JDFXWB)
C
  991 CONTINUE
      END
#endif
