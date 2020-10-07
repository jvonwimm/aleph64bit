      SUBROUTINE YHISTO
C
C----------------------------------------------------------*
C!    book debugging histograms for YTOP
CKEY YTOP HISTOGRAMS
C!    Author :     G. Lutz       /06/91
C!    Modified :   M. Bosman   24/07/91
C!
C!    Description
C!    ===========
C! find vd coordinates for track ITR
C! NCOM is no. of complete hits (ie rphi and z coordinates)
C----------------------------------------------------------*
#include "ypafma.h"
#include "yparto.h"
C
#ifndef DOC
      SAVE
C        START NUMBER FOR HISTOS
      DATA IHST/20000/
C
C----- last parameter was missing in hbookx calls
      CALL HBOOK2(IHST+1001,' CHISQ DIST.FROM PR.VTX. VS. MOMENTUM $',
     &   50,0.,25., 40,0.,200.,0.)
      CALL HBOOK1(IHST+1002,' # OF CALLS TO YADHVX $',100,0.,500.,0.)
      CALL HBOOK1(IHST+1003,' # OF CALLS TO YFTHVX $',100,0.,500.,0.)
      CALL HBOOK1(IHST+1011,' # UNIQ.ASS.TRKS TO PRIM.VTX $',50,0.,50.
     &    ,0.)
      CALL HBOOK1(IHST+1012,' # AMB.ASS.TRKS TO PRIM.VTX $',50,0.,50.
     &    ,0.)
      CALL HBOOK1(IHST+1013,' # UNIQ.ASS.TRKS TO SEC.VTX $',50,0.,50.
     &    ,0.)
      CALL HBOOK1(IHST+1014,' # AMB.ASS.TRKS TO SEC.VTX $',50,0.,50.
     &    ,0.)
C
C BOOK HISTOS FOR CONVERSION SEARCH
      IF(LCONVS) THEN
        CALL HBOOK1(IHST+1041,' CHI VTX. GAMMA CONV. SEARCH$',50,0.,50.
     &    ,0.)
        CALL HBOOK1(IHST+1061,
     &    ' RAD VERTEX GAMMA CON BEFORE RAD.&MASS CUT$',100,0.,50.,0.)
        CALL HBOOK1(IHST+1081,
     &    ' MASS GAMMA CONV. SEARCH BEFORE MASS CUT$',100,0.,5.,0.)
        CALL HBOOK1(IHST+1091,
     &    ' MASS GAMMA CONV. SEARCH AFTER MASS CUT$',100,0.,5.,0.)
        CALL HBOOK1(IHST+1071,
     &    ' RAD VERTEX GAMMA CON AFTER MASS CUT$',100,0.,50.,0.)
        CALL HBOOK1(IHST+1191,' CHI VTX. REC.GAMMA + BCRO$',50,0.,50.
     &    ,0.)
        CALL HBOOK1(IHST+1291,
     &    ' MASS GAMMA CONV. AFTER CUT CHISQ REFIT BCRO$',100,0.,5.,0.)
        CALL HBOOK1(IHST+1391,
     &    ' RAD VERTEX GAMMA AFTER CUT CHISQ REFIT BCRO$',100,0.,50.,0.)
      ENDIF
C
C  BOOK HISTOS FOR VZERO SEARCH
      IF(LVZERS) THEN
        CALL HBOOK1(IHST+1042,' CHI VTX. VZERO SEARCH$',50,0.,50.,0.)
        CALL HBOOK1(IHST+1043,' CHI DIST. VTX.-BCR. V0 SEARCH$',50,0.
     &    ,50.,0.)
        CALL HBOOK1(IHST+1050,' CHI VTX. REC. V0 + BCRO$',50,0.,50.
     &    ,0.)
        CALL HBOOK1(IHST+1052,' MASS K0 SEARCH$',100,0.,5.,0.)
        CALL HBOOK1(IHST+1152,' MASS K0 SEARCH$',100,.25,1.25,0.)
        CALL HBOOK1(IHST+1053,' MASS LAMBDA SEARCH$',100,0.,5.,0.)
        CALL HBOOK1(IHST+1153,' MASS LAMBDA SEARCH$',100,1.,2.,0.)
        CALL HBOOK1(IHST+1062,' RAD VERTEX K0 SEARCH$',100,0.,200.,0.)
        CALL HBOOK1(IHST+1063,' RAD VERTEX LAMBDA $',100,0.,200.,0.)
      ENDIF
C
C
      RETURN
      END
#endif
