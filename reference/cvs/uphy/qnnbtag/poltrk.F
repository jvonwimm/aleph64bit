      SUBROUTINE POLTRK(ITK,ITHRU,II)
C------------------------------------
C     II = 1 (lock) 2 (unlock)
#include "qcde.h"
#include "qmacro.h"
      CALL PR_SCAL(1,QX(ITK),QY(ITK),QZ(ITK),
     >             QX(ITHRU),QY(ITHRU),QZ(ITHRU),PICH)
      IF (ABS(PICH).LT.0.342) THEN
      IF (II.EQ.1) THEN
      CALL QLTRK  (ITK)
      ELSE
      CALL QLUTRK (ITK)
      ENDIF
      ENDIF
C
      RETURN
      END
C
       SUBROUTINE MLP_ANA
C--------------------------------------------------------------
C      Neural Net.
#include "comnnt.h"
#include "comlpt.h"
      NCLASS = ITQUA
      CALL EN_AVA
      IF (ILOOP1.EQ.1) THEN
      IF (NCLASS.EQ.1) CALL HFILL (IHNUM+161,RESULT,0.,POIDS)
      IF (NCLASS.EQ.2) CALL HFILL (IHNUM+162,RESULT,0.,POIDS)
      IF (NCLASS.EQ.3) CALL HFILL (IHNUM+163,RESULT,0.,POIDS)
                       CALL HFILL (IHNUM+164,RESULT,0.,POIDS)
      ELSE IF(ILOOP1.EQ.2) THEN
      IF (NCLASS.EQ.1) CALL HFILL (IHNUM+261,RESULT,0.,POIDS)
      IF (NCLASS.EQ.2) CALL HFILL (IHNUM+262,RESULT,0.,POIDS)
      IF (NCLASS.EQ.3) CALL HFILL (IHNUM+263,RESULT,0.,POIDS)
                       CALL HFILL (IHNUM+264,RESULT,0.,POIDS)
      ELSE IF(ILOOP1.EQ.3) THEN
      IF (NCLASS.EQ.1) CALL HFILL (IHNUM+361,RESULT,0.,POIDS)
      IF (NCLASS.EQ.2) CALL HFILL (IHNUM+362,RESULT,0.,POIDS)
      IF (NCLASS.EQ.3) CALL HFILL (IHNUM+363,RESULT,0.,POIDS)
                       CALL HFILL (IHNUM+364,RESULT,0.,POIDS)
      ELSE IF(ILOOP1.EQ.4) THEN
      IF (NCLASS.EQ.1) CALL HFILL (IHNUM+461,RESULT,0.,POIDS)
      IF (NCLASS.EQ.2) CALL HFILL (IHNUM+462,RESULT,0.,POIDS)
      IF (NCLASS.EQ.3) CALL HFILL (IHNUM+463,RESULT,0.,POIDS)
                       CALL HFILL (IHNUM+464,RESULT,0.,POIDS)
      ENDIF
      RETURN
      END
C
       SUBROUTINE EN_AVA
C---------------------------------------------
C     To compute the Neural Net. outputs from
C     the inputs and the weights W(i,j)
C     Author : J. Proriol
#include "comnnt.h"
#include "comlpt.h"
C
C number of neurons per layer
      IF (ILOOP1.EQ.1) THEN
c option 1
      NVAR      = 25
      NEURON(1) = 25
      NEURON(2) = 14
      NEURON(3) = 14
      ELSEIF (ILOOP1.EQ.2) THEN
c option 2
      NVAR      = 20
      NEURON(1) = 20
      NEURON(2) = 11
      NEURON(3) = 13
      ELSEIF (ILOOP1.EQ.3) THEN
c option 3
      NVAR      = 20
      NEURON(1) = 20
      NEURON(2) = 14
      NEURON(3) = 14
      ELSEIF (ILOOP1.EQ.4) THEN
c option 4
      NVAR      = 20
      NEURON(1) = 20
      NEURON(2) = 12
      NEURON(3) = 12
      ENDIF
      NEURON(4) =  2
C---- First layer ----
       DO 1 I  = 1 , NEURON(1)
       XEEV(I) = VTOT(I)
       IF (ILOOP1.EQ.1) THEN
       XMEAN  = (XMAX1(I) + XMIN1(I)) / 2.0
       XRANGE = (XMAX1(I) - XMIN1(I)) / 2.0
       ENDIF
       IF (ILOOP1.EQ.2) THEN
       XMEAN  = (XMAX2(I) + XMIN2(I)) / 2.0
       XRANGE = (XMAX2(I) - XMIN2(I)) / 2.0
       ENDIF
       IF (ILOOP1.EQ.3) THEN
       XMEAN  = (XMAX3(I) + XMIN3(I)) / 2.0
       XRANGE = (XMAX3(I) - XMIN3(I)) / 2.0
       ENDIF
       IF (ILOOP1.EQ.4) THEN
       XMEAN  = (XMAX4(I) + XMIN4(I)) / 2.0
       XRANGE = (XMAX4(I) - XMIN4(I)) / 2.0
       ENDIF
       X(1,I) = (XEEV(I) - XMEAN) / XRANGE
       Y(1,I)  = X(1,I)
    1  CONTINUE
C---- Another layers ----
       DO 2 LAYER = 1 , LAYERM - 1
       DO 2 J = 1 , NEURON(LAYER+1)
C
       X(LAYER+1,J) = 0.
       DO 3 I = 1 , NEURON(LAYER)
       IF (ILOOP1.EQ.1)
     > X(LAYER+1,J) = X(LAYER+1,J) + Y(LAYER,I) * W1(LAYER+1,J,I)
       IF (ILOOP1.EQ.2)
     > X(LAYER+1,J) = X(LAYER+1,J) + Y(LAYER,I) * W2(LAYER+1,J,I)
       IF (ILOOP1.EQ.3)
     > X(LAYER+1,J) = X(LAYER+1,J) + Y(LAYER,I) * W3(LAYER+1,J,I)
       IF (ILOOP1.EQ.4)
     > X(LAYER+1,J) = X(LAYER+1,J) + Y(LAYER,I) * W4(LAYER+1,J,I)
   3   CONTINUE
C
       IF (ILOOP1.EQ.1)
     > X(LAYER+1,J) = X(LAYER+1,J) + WW1(LAYER+1,J)
       IF (ILOOP1.EQ.2)
     > X(LAYER+1,J) = X(LAYER+1,J) + WW2(LAYER+1,J)
       IF (ILOOP1.EQ.3)
     > X(LAYER+1,J) = X(LAYER+1,J) + WW3(LAYER+1,J)
       IF (ILOOP1.EQ.4)
     > X(LAYER+1,J) = X(LAYER+1,J) + WW4(LAYER+1,J)
       Y(LAYER+1,J)=(1. -EXP(-X(LAYER+1,J)))/(1. + EXP(-X(LAYER+1,J)))
   2   CONTINUE
       RESULT = Y(LAYERM,1)
C
       RETURN
       END

      SUBROUTINE MLP_INI
C--------------------------------------------------------------
#include "qcde.h"
#include "comnnt.h"
#include "comlpt.h"
#include "qmacro.h"
C---- Initialize for the Neural Network ----
      NKCOUT = 0
      NKPRIS = 0
      NKTRUS = 0
      NGEN   = 0
      NB1    = 0
      NC1    = 0
      NUDS1  = 0
      NDAT1  = 0
      NB2    = 0
      NC2    = 0
      NUDS2  = 0
      NDAT2  = 0
      TTT = 0.
      DO 10 J = 0 , 3
      DO 10 I = 1 , 9
 10   NERR(I,J) = 0
C
      LCLASS    =  2
      LAYERM    =  4
C
C----------------------------------
      IF (IOPVA1.EQ.1) THEN
      IHNUM = 10000
c option 1
      CALL HBOOK1 (IHNUM + 101,'PCL(1)         opt 1',50,0.0,35.0,0.)
      CALL HBOOK1 (IHNUM + 102,'HJ5            opt 1',50,0.0,1.00,0.)
      CALL HBOOK1 (IHNUM + 103,'YMT            opt 1',50,0.0,55.0,0.)
      CALL HBOOK1 (IHNUM + 104,'XMIN(2)        opt 1',50,0.0,7.00,0.)
      CALL HBOOK1 (IHNUM + 105,'XJFM           opt 1',50,0.0,5.00,0.)
      CALL HBOOK1 (IHNUM + 106,'PCT(2)         opt 1',50,0.0,3.00,0.)
      CALL HBOOK1 (IHNUM + 107,'PCT(1)         opt 1',50,0.0,3.00,0.)
      CALL HBOOK1 (IHNUM + 108,'N_Y  p > 1 GeV opt 1',20,0.,20.,0.)
      CALL HBOOK1 (IHNUM + 109,'M_X  p > 1 GeV opt 1',50,0.,10.,0.)
      CALL HBOOK1 (IHNUM + 110,'M_Y  CONE      opt 1',50,0.,15.,0.)
      CALL HBOOK1 (IHNUM + 111,'SPT CONE       opt 1',50,0.0,10.0,0.)
      CALL HBOOK1 (IHNUM + 112,'Econe/ EHEMI   opt 1',51,0.0,1.02,0.)
      CALL HBOOK1 (IHNUM + 113,'FRAC ITKMAX    opt 1',50,0.0,1.0,0.)
      CALL HBOOK1 (IHNUM + 114,'M_Y  itkm      opt 1',50,0.,15.,0.)
      CALL HBOOK1 (IHNUM + 115,'Eitkm/ EHEMI   opt 1',51,0.0,1.02,0.)
      CALL HBOOK1 (IHNUM + 116,'SDIR(2)        opt 1',50,0.0,1.00,0.)
      CALL HBOOK1 (IHNUM + 117,'SDIR(3)        opt 1',50,0.0,1.00,0.)
      CALL HBOOK1 (IHNUM + 118,'SDIR(7)        opt 1',50,0.0,1.00,0.)
      CALL HBOOK1 (IHNUM + 119,'SDIR(8)        opt 1',50,0.0,1.00,0.)
      CALL HBOOK1 (IHNUM + 120,'XMIN(7)        opt 1',50,0.0,6.00,0.)
      CALL HBOOK1 (IHNUM + 121,'EMINV(2)       opt 1',20,0.,40.,0.)
      CALL HBOOK1 (IHNUM + 122,'EMINV(3)       opt 1',20,0.,40.,0.)
      CALL HBOOK1 (IHNUM + 123,'EMINV(6)       opt 1',20,0.,40.,0.)
      CALL HBOOK1 (IHNUM + 124,'EMINV(8)       opt 1',20,0.,40.,0.)
      CALL HBOOK1 (IHNUM + 125,'ENUCL          opt 1',20,0.,40.,0.)
c option 2
      CALL HBOOK1 (IHNUM + 201,'JTLJET         opt 2',50,0.0,3.00,0.)
      CALL HBOOK1 (IHNUM + 202,'BJS            opt 2',50,0.0,1.00,0.)
      CALL HBOOK1 (IHNUM + 203,'PTJETS         opt 2',50,0.0,0.40,0.)
      CALL HBOOK1 (IHNUM + 204,'SPT            opt 2',50,0.0,10.0,0.)
      CALL HBOOK1 (IHNUM + 205,'PTLJET         opt 2',50,0.0,0.03,0.)
      CALL HBOOK1 (IHNUM + 206,'PCL(1)         opt 2',50,0.0,35.0,0.)
      CALL HBOOK1 (IHNUM + 207,'PCL(2)         opt 2',50,0.0,20.0,0.)
      CALL HBOOK1 (IHNUM + 208,'PCT(1)         opt 2',50,0.0,3.00,0.)
      CALL HBOOK1 (IHNUM + 209,'YMT            opt 2',50,0.0,55.0,0.)
      CALL HBOOK1 (IHNUM + 210,'REVIS          opt 2',50,0.0,0.80,0.)
      CALL HBOOK1 (IHNUM + 211,'SDIR(1)        opt 2',50,0.0,1.00,0.)
      CALL HBOOK1 (IHNUM + 212,'XJFM           opt 2',50,0.0,5.00,0.)
      CALL HBOOK1 (IHNUM + 213,'PCT(2)         opt 2',50,0.0,3.00,0.)
      CALL HBOOK1 (IHNUM + 214,'MJET           opt 2',50,0.0,15.0,0.)
      CALL HBOOK1 (IHNUM + 215,'SMJETS         opt 2',50,0.0,20.0,0.)
      CALL HBOOK1 (IHNUM + 216,'Probhe         opt 2',50,-9.8,0.2,0.)
      CALL HBOOK1 (IHNUM + 217,'N_Y            opt 2',40,0.,40.,0.)
      CALL HBOOK1 (IHNUM + 218,'N_Y  P > 1 GEV opt 2',40,0.,40.,0.)
      CALL HBOOK1 (IHNUM + 219,'M_Y  p > 1 GeV opt 2',50,0.,10.,0.)
      CALL HBOOK1 (IHNUM + 220,'Missing Energy opt 2',50,-15.,35.,0.)
c option 3
      CALL HBOOK1 (IHNUM + 301,'PTJETS          opt 3',50,0.0,0.40,0.)
      CALL HBOOK1 (IHNUM + 302,'SPT             opt 3',50,0.0,10.0,0.)
      CALL HBOOK1 (IHNUM + 303,'PTLJET          opt 3',50,0.0,0.03,0.)
      CALL HBOOK1 (IHNUM + 304,'PCL(1)          opt 3',50,0.0,35.0,0.)
      CALL HBOOK1 (IHNUM + 305,'PCL(2)          opt 3',50,0.0,20.0,0.)
      CALL HBOOK1 (IHNUM + 306,'PCT(1)          opt 3',50,0.0,3.00,0.)
      CALL HBOOK1 (IHNUM + 307,'YMT             opt 3',50,0.0,55.0,0.)
      CALL HBOOK1 (IHNUM + 308,'REVIS           opt 3',50,0.0,0.80,0.)
      CALL HBOOK1 (IHNUM + 309,'SDIR(1)         opt 3',50,0.0,1.00,0.)
      CALL HBOOK1 (IHNUM + 310,'PCL(3)          opt 3',50,0.0,15.0,0.)
      CALL HBOOK1 (IHNUM + 311,'XJFM            opt 3',50,0.0,5.00,0.)
      CALL HBOOK1 (IHNUM + 312,'MJET            opt 3',50,0.0,15.0,0.)
      CALL HBOOK1 (IHNUM + 313,'SMJETS          opt 3',50,0.0,20.0,0.)
      CALL HBOOK1 (IHNUM + 314,'Probhe          opt 3',50,-9.8,0.2,0.)
      CALL HBOOK1 (IHNUM + 315,'N_X             opt 3',20,0.,20.,0.)
      CALL HBOOK1 (IHNUM + 316,'N_X  p > 1 GeV  opt 3',20,0.,20.,0.)
      CALL HBOOK1 (IHNUM + 317,'N_Y             opt 3',40,0.,40.,0.)
      CALL HBOOK1 (IHNUM + 318,'N_Y  p > 1 GeV  opt 3',40,0.,40.,0.)
      CALL HBOOK1 (IHNUM + 319,'M_Y  p > 1 GeV  opt 3',50,0.,10.,0.)
      CALL HBOOK1 (IHNUM + 320,'Nbre of leptons opt 3',10,0.,10.,0.)
c option 4
      CALL HBOOK1 (IHNUM + 401,'HJ5             opt 4',50,0.0,1.00,0.)
      CALL HBOOK1 (IHNUM + 402,'PCL(1)          opt 4',50,0.0,35.0,0.)
      CALL HBOOK1 (IHNUM + 403,'PCL(2)          opt 4',50,0.0,20.0,0.)
      CALL HBOOK1 (IHNUM + 404,'PCT(1)          opt 4',50,0.0,3.00,0.)
      CALL HBOOK1 (IHNUM + 405,'SDIR(8)         opt 4',50,0.0,1.00,0.)
      CALL HBOOK1 (IHNUM + 406,'YMT             opt 4',50,0.0,55.0,0.)
      CALL HBOOK1 (IHNUM + 407,'XMIN(2)         opt 4',50,0.0,7.00,0.)
      CALL HBOOK1 (IHNUM + 408,'M_Y  itkm       opt 4',50,0.,15.,0.)
      CALL HBOOK1 (IHNUM + 409,'SDIR(1)         opt 4',50,0.0,1.00,0.)
      CALL HBOOK1 (IHNUM + 410,'Eitkm/ EHEMI    opt 4',51,0.0,1.02,0.)
      CALL HBOOK1 (IHNUM + 411,'WALTER          opt 4',50,0.0,5.00,0.)
      CALL HBOOK1 (IHNUM + 412,'PCT(2)          opt 4',50,0.0,3.00,0.)
      CALL HBOOK1 (IHNUM + 413,'N_X  p > 1 GeV  opt 4',20,0.,20.,0.)
      CALL HBOOK1 (IHNUM + 414,'N_Y  p > 1 GeV  opt 4',20,0.,20.,0.)
      CALL HBOOK1 (IHNUM + 4415,'M_X  p > 1 GeV opt 4',50,0.,10.,0.)
      CALL HBOOK1 (IHNUM + 416,'PCT(3)          opt 4',50,0.0,2.00,0.)
      CALL HBOOK1 (IHNUM + 417,'M_Y  CONE       opt 4',50,0.,15.,0.)
      CALL HBOOK1 (IHNUM + 418,'SPT CONE        opt 4',50,0.0,10.0,0.)
      CALL HBOOK1 (IHNUM + 419,'Econe/ EHEMI    opt 4',51,0.0,1.02,0.)
      CALL HBOOK1 (IHNUM + 420,'FRAC ITKMAX     opt 4',50,0.0,1.0,0.)
C Neural Net. outputs
      IF (XMCEV) THEN
      CALL HBOOK1 (IHNUM+161,'SORTIE bb(bar) 1: HEMI',50,-1.0,1.0,0.)
      CALL HBOOK1 (IHNUM+162,'SORTIE cc(bar) 1: HEMI',50,-1.0,1.0,0.)
      CALL HBOOK1 (IHNUM+163,'SORTIE uds     1: HEMI',50,-1.0,1.0,0.)
      CALL HBOOK1 (IHNUM+261,'SORTIE bb(bar) 2: HEMI',50,-1.0,1.0,0.)
      CALL HBOOK1 (IHNUM+262,'SORTIE cc(bar) 2: HEMI',50,-1.0,1.0,0.)
      CALL HBOOK1 (IHNUM+263,'SORTIE uds     2: HEMI',50,-1.0,1.0,0.)
      CALL HBOOK1 (IHNUM+361,'SORTIE bb(bar) 3: HEMI',50,-1.0,1.0,0.)
      CALL HBOOK1 (IHNUM+362,'SORTIE cc(bar) 3: HEMI',50,-1.0,1.0,0.)
      CALL HBOOK1 (IHNUM+363,'SORTIE uds     3: HEMI',50,-1.0,1.0,0.)
      CALL HBOOK1 (IHNUM+461,'SORTIE bb(bar) 4: HEMI',50,-1.0,1.0,0.)
      CALL HBOOK1 (IHNUM+462,'SORTIE cc(bar) 4: HEMI',50,-1.0,1.0,0.)
      CALL HBOOK1 (IHNUM+463,'SORTIE uds     4: HEMI',50,-1.0,1.0,0.)
      ENDIF
      CALL HBOOK1 (IHNUM+164,'SORTIE qq(bar) 1: HEMI',50,-1.0,1.0,0.)
      CALL HBOOK1 (IHNUM+264,'SORTIE qq(bar) 2: HEMI',50,-1.0,1.0,0.)
      CALL HBOOK1 (IHNUM+364,'SORTIE qq(bar) 3: HEMI',50,-1.0,1.0,0.)
      CALL HBOOK1 (IHNUM+464,'SORTIE qq(bar) 4: HEMI',50,-1.0,1.0,0.)
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE MLP_END
C------------------------------------
C     Neural Net. output
#include "qcde.h"
#include "comnnt.h"
#include "comlpt.h"
#include "qmacro.h"
C
      CALL MLP_STA
      RETURN
      END
C
      SUBROUTINE MLP_VAR
C--------------------------------------------------------------
#include "comnnt.h"
#include "comlpt.h"
      IF (ILOOP1.EQ.1) THEN
      CALL HFILL (IHNUM + 101,VTOT(1),0.,1.)
      CALL HFILL (IHNUM + 102,VTOT(2),0.,1.)
      CALL HFILL (IHNUM + 103,VTOT(3),0.,1.)
      CALL HFILL (IHNUM + 104,VTOT(4),0.,1.)
      CALL HFILL (IHNUM + 105,VTOT(5),0.,1.)
      CALL HFILL (IHNUM + 106,VTOT(6),0.,1.)
      CALL HFILL (IHNUM + 107,VTOT(7),0.,1.)
      CALL HFILL (IHNUM + 108,VTOT(8),0.,1.)
      CALL HFILL (IHNUM + 109,VTOT(9),0.,1.)
      CALL HFILL (IHNUM + 110,VTOT(10),0.,1.)
      CALL HFILL (IHNUM + 111,VTOT(11),0.,1.)
      CALL HFILL (IHNUM + 112,VTOT(12),0.,1.)
      CALL HFILL (IHNUM + 113,VTOT(13),0.,1.)
      CALL HFILL (IHNUM + 114,VTOT(14),0.,1.)
      CALL HFILL (IHNUM + 115,VTOT(15),0.,1.)
      CALL HFILL (IHNUM + 116,VTOT(16),0.,1.)
      CALL HFILL (IHNUM + 117,VTOT(17),0.,1.)
      CALL HFILL (IHNUM + 118,VTOT(18),0.,1.)
      CALL HFILL (IHNUM + 119,VTOT(19),0.,1.)
      CALL HFILL (IHNUM + 120,VTOT(20),0.,1.)
      CALL HFILL (IHNUM + 121,VTOT(21),0.,1.)
      CALL HFILL (IHNUM + 122,VTOT(22),0.,1.)
      CALL HFILL (IHNUM + 123,VTOT(23),0.,1.)
      CALL HFILL (IHNUM + 124,VTOT(24),0.,1.)
      CALL HFILL (IHNUM + 125,VTOT(25),0.,1.)
      ELSE IF (ILOOP1.EQ.2) THEN
      CALL HFILL (IHNUM + 201,VTOT(1),0.,1.)
      CALL HFILL (IHNUM + 202,VTOT(2),0.,1.)
      CALL HFILL (IHNUM + 203,VTOT(3),0.,1.)
      CALL HFILL (IHNUM + 204,VTOT(4),0.,1.)
      CALL HFILL (IHNUM + 205,VTOT(5),0.,1.)
      CALL HFILL (IHNUM + 206,VTOT(6),0.,1.)
      CALL HFILL (IHNUM + 207,VTOT(7),0.,1.)
      CALL HFILL (IHNUM + 208,VTOT(8),0.,1.)
      CALL HFILL (IHNUM + 209,VTOT(9),0.,1.)
      CALL HFILL (IHNUM + 210,VTOT(10),0.,1.)
      CALL HFILL (IHNUM + 211,VTOT(11),0.,1.)
      CALL HFILL (IHNUM + 212,VTOT(12),0.,1.)
      CALL HFILL (IHNUM + 213,VTOT(13),0.,1.)
      CALL HFILL (IHNUM + 214,VTOT(14),0.,1.)
      CALL HFILL (IHNUM + 215,VTOT(15),0.,1.)
      CALL HFILL (IHNUM + 216,VTOT(16),0.,1.)
      CALL HFILL (IHNUM + 217,VTOT(17),0.,1.)
      CALL HFILL (IHNUM + 218,VTOT(18),0.,1.)
      CALL HFILL (IHNUM + 219,VTOT(19),0.,1.)
      CALL HFILL (IHNUM + 220,VTOT(20),0.,1.)
      ELSE IF (ILOOP1.EQ.3) THEN
      CALL HFILL (IHNUM + 301,VTOT(1),0.,1.)
      CALL HFILL (IHNUM + 302,VTOT(2),0.,1.)
      CALL HFILL (IHNUM + 303,VTOT(3),0.,1.)
      CALL HFILL (IHNUM + 304,VTOT(4),0.,1.)
      CALL HFILL (IHNUM + 305,VTOT(5),0.,1.)
      CALL HFILL (IHNUM + 306,VTOT(6),0.,1.)
      CALL HFILL (IHNUM + 307,VTOT(7),0.,1.)
      CALL HFILL (IHNUM + 308,VTOT(8),0.,1.)
      CALL HFILL (IHNUM + 309,VTOT(9),0.,1.)
      CALL HFILL (IHNUM + 310,VTOT(10),0.,1.)
      CALL HFILL (IHNUM + 311,VTOT(11),0.,1.)
      CALL HFILL (IHNUM + 312,VTOT(12),0.,1.)
      CALL HFILL (IHNUM + 313,VTOT(13),0.,1.)
      CALL HFILL (IHNUM + 314,VTOT(14),0.,1.)
      CALL HFILL (IHNUM + 315,VTOT(15),0.,1.)
      CALL HFILL (IHNUM + 316,VTOT(16),0.,1.)
      CALL HFILL (IHNUM + 317,VTOT(17),0.,1.)
      CALL HFILL (IHNUM + 318,VTOT(18),0.,1.)
      CALL HFILL (IHNUM + 319,VTOT(19),0.,1.)
      CALL HFILL (IHNUM + 320,VTOT(20),0.,1.)
      ENDIF
      RETURN
      END
      SUBROUTINE MLP_STA
C--------------------------------------
C     Statistics printout
#include "qcde.h"
#include "comnnt.h"
#include "comlpt.h"
#include "qmacro.h"
C
      WRITE(KUPRNT,1)
 1    FORMAT(//20X,'<> <> <> <> <> <> <> <> <> <> <> '//
     >         20X,'<>  NEURAL NETWORK: QNNBTAG   <> '//
     >         20X,'<>    Statistics printout     <> '//
     >         20X,'<>  Authors : A.Bencheikh     <> '//
     >         20X,'<>            P.Henrard       <> '//
     >         20X,'<>            J.Proriol       <> '//
     >         20X,'<>  LPC Clermont-Ferrand      <> '//
     >         20X,'<> <> <> <> <> <> <> <> <> <> <> '//)
C
      IF(XMCEV) THEN
C
      WRITE(KUPRNT,2)NKCOUT
 2    FORMAT(//5X,'Number of Monte Carlo QQbar processed   ',I10)
      WRITE(KUPRNT,3)NB1,NC1,NUDS1
 3    FORMAT(  5X,'Number of Monte Carlo BBbar processed   ',I10/
     >         5X,'Number of Monte Carlo CCbar processed   ',I10/
     >         5X,'Number of Monte Carlo UDS   processed   ',I10)
      WRITE(KUPRNT,4)NGEN
 4    FORMAT(//5X,'Number of Monte Carlo QQbar successful  ',I10)
      WRITE(KUPRNT,5)NB2,NC2,NUDS2
 5    FORMAT(  5X,'Number of Monte Carlo BBbar successful  ',I10/
     >         5X,'Number of Monte Carlo CCbar successful  ',I10/
     >         5X,'Number of Monte Carlo UDS   successful  ',I10)
C
      ELSE
      WRITE(KUPRNT,6)NKCOUT
 6    FORMAT(//5X,'Number of Data QQbar processed          ',I10)
      WRITE(KUPRNT,7)NGEN
 7    FORMAT(//5X,'Number of Data QQbar successful         ',I10)
C
      ENDIF
C
      WRITE(KUPRNT,8)NKCOUT-NKPRIS,NKPRIS-NKTRUS,NKTRUS-NGEN
 8    FORMAT(//5X,'Number of events : class 16 failed     ',I10/
     >         5X,'Number of events : thrust failed       ',I10/
     >         5X,'Number of events : variables failed    ',I10)

      IF (.NOT.XMCEV) WRITE(KUPRNT,21)(NERR(1,0))
 21   FORMAT(//5X,'Number of Run select failed  Data      ',I10)
C
      IF (XMCEV) THEN
      WRITE(KUPRNT,9)
 9    FORMAT(//20X,'<  <  <  <  <  <  <  <  <  <  < '//
     >         20X,'< Details for each flavour <  < '//
     >         20X,'<  <  <  <  <  <  <  <  <  <  < '//)
C

      WRITE(KUPRNT,11)(NERR(2,J),J=1,3)
 11   FORMAT(//5X,'Number of Class 16   failed  bbbar ccbar uds',3I6)
      WRITE(KUPRNT,13)(NERR(9,J),J=1,3)
 13   FORMAT(  5X,'Number of thrust cut failed  bbbar ccbar uds',3I6)
      WRITE(KUPRNT,12)(NERR(3,J)+NERR(4,J)+NERR(5,J)+NERR(6,J)+
     >NERR(7,J)+NERR(8,J),J=1,3)
 12   FORMAT(5X,'Number of variables  failed  bbbar ccbar uds',3I6//)
C
      ENDIF
      RETURN
      END
C======================================================================C
