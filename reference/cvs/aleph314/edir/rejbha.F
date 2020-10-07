       SUBROUTINE REJBHA(XTOW,EWTOT,IQQT1,IQQT2,IOK)
C----------------------------------------------------------------------
CKEY EDIR BHABHA REJECTION
C! Subroutine to reject Bhabhas keeping hadrons and most of qqbars.
C! 2 rejections available: 1 based on PECO
C!                         2 based on wires
C-
C   Input  : XTOW(1-7) = Parameters of the 2 most energetic cluster
C            EWTOT     = Wire energy
C   Output : IQQT1     = 1 qqbar candidate if IOK = 0
C            IQQT2     = 1 qqbar candidate if IOK = 0
C            IOK       = 1 not a qqbar
C-
C   Called by   : SELCAL
C   Calls  : None
C   Input banks : None
C-
C                                    Author: S. Dugey - 910400
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
      DIMENSION XTOW (7)
C --
       IOK = 0
       IQQT1  = 0
       IQQT2  = 0
       EHIGH1 = XTOW(1)
       EHIGH2 = XTOW(2)
       TH1    = XTOW(3)
       TH2    = XTOW(4)
       EMAX1  = XTOW(6)
       EMAX2  = XTOW(7)
       NMOD   = INT(XTOW(6))
C --
C   Angular selection
C --
       L1 = 0
       L2 = 0
       IF( TH1.GT.20..AND.TH1.LT.160. )  L1 = 1
       IF( TH1.GT.38..AND.TH1.LT.42. )   L1 = 0
       IF( TH1.GT.138..AND.TH1.LT.142. ) L1 = 0
       IF( TH2.GT.20..AND.TH2.LT.160. )  L2 = 1
       IF( TH2.GT.38..AND.TH2.LT.42. )   L2 = 0
       IF( TH2.GT.138..AND.TH2.LT.142. ) L2 = 0
C --
C   Ratio (EMAXI + EMAXI2) / EWTOT
C --
       RAT = 0.
       IF(EWTOT.GT.0.) RAT = (EMAX1 + EMAX2 ) / EWTOT
       RATT= 0.
       IF(EWTOT.GT.0.) RATT= (EHIGH1+ EHIGH2) / EWTOT
C --
       IF(EWTOT.LT.80.) THEN
C --
C   Selection using the 2 most energetic ECAL objects
C --
          IF(EHIGH1.LT.35..AND.EHIGH2.LT.25.) THEN
             IF(L1.EQ.1.AND.L2.EQ.1) THEN
               IQQT1= 1
             ELSE
               IF(EHIGH2.LT.20.0.AND.RAT.LT.0.84) IQQT1= 1
             ENDIF
          ENDIF
C --
C   Selection using the 2 most energetic ECAL modules
C --
          IF(EMAX1.LT.35..AND.EMAX2.LT.25.) THEN
             IF(L1.EQ.1.AND.L2.EQ.1) THEN
              IQQT2= 1
             ELSE
              IF(RAT.LT.0.84) IQQT2= 1
             ENDIF
          ENDIF
       ENDIF
C --
C Bhabha selection
C --
       IF(EMAX1.GT.38..AND.EMAX2.GT.25.) THEN
         IF(RAT.GT.0.95) IOK = 1
       ELSE
         IF(L1.EQ.0.AND.L2.EQ.0) THEN
           IF(EWTOT.GT.55..AND.RAT.GT.0.90) IOK = 1
         ENDIF
       ENDIF
C --
       IF(NMOD.GT.3..AND.IOK.EQ.0) THEN
         IQQT1 = 1
         IQQT2 = 1
       ENDIF
C --
       RETURN
       END
#endif
