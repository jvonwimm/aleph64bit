      SUBROUTINE YV1INI(LCONS,IER,NRYV1)
C-----------------------------------------------------------------------
C! Read the cuts for the V0 reconstruction, YV1C (DBASE)
CKEY YV0 INIT CUTS /INTERNAL
C   Author:  M.A.Ciocci 20/2/90
C   Modified: J.Sedgbeer 09/04/91. YV1C bank and common/YV0CUT/ extended
C   Modified: M.A.Ciocci 20/1/93. YV1C bank have now several values of NR
C     The  different cuts used in  different periods
C     of Aleph data tacking (89-90 91-92 93...) are sumarized
C     by the different NR (1, 2, 3) of the yv1c bank. So on the data base
C     the yv1c is present with 3 different value for NR.
C     If the user decide to change cuts will be sufficient to give
C     by cards a bank called yv1c with NR greater than 3 and change
C     the cuts value
C
C  Standard YV1C on dbase
C  ----------------------
CYV1C 1/ 25  1  1  1  99999999
C*1989-1990
C*      IV     O1     O2     CT     PS         RV    ZV    DI
C        1      0.0    0.    2.     0.         180. 220. 0.06
C*      CF     CS     CO     MA     PM    NT   CB    NC    MD
C      13.    0.92    0.    50.    0.     4    0    0      0.
C *     D0     Z0     WK        WL         WG
C      999.   999.   0.1E+05  0.1E+05     0.1E+05
CYV1C 2/ 25  1  1  1  99999999
C*1991-1992
C*      IV     O1     O2     CT     PS         RV    ZV    DI
C       1      0.0    0.     3.    0.0        180.  220. 0.06
C*      CF     CS     CO     MA     PM    NT   CB    NC    MD
C       25.    0.5    0.0    9999.  0.1   4     2    10    1.5
C*      D0     Z0     WK     WL     WG
C       999.   999.   9999. 9999.   9999.
CYV1C 3/ 25  1  1  1  99999999
C*1993 and later i hope...
C*      IV     O1     O2     CT     PS         RV    ZV    DI
C       1      0.0    0.     2.    0.0        180.  180. 0.06
C*      CF     CS     CO     MA     PM    NT   CB    NC    MD
C       13.    0.92   0.0    9999.  0.    5     0    33    0.
C*      D0     Z0     WK     WL     WG
C       999.   999.   0.5    0.50  0.50
CYV1C 4/ 25  1  1  1  99999999
C*user private
C*      IV     O1     O2     CT     PS         RV    ZV    DI
C        0      0.0    0.    5.     0.         180. 220. 0.06
C*      CF     CS     CO     MA     PM    NT   CB    NC    MD
C      20.    0.5     0.    50.    0.     4    0    0      0.
C *     D0     Z0     WK        WL         WG
C      999.   999.   0.1E+05  0.1E+05     0.1E+05
C------------------------------------------------------------------------
C   Input:  LCONS     /I      Logical unit for the data base
C   Output: IER       /I      Error return
C                                 0: successful initialization
C                                 1: yv1c bank not found
C
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "yv1cjj.h"
#include "bcs.h"
C
C
      EXTERNAL NLINK,NAMIND,NBANK,CHAINT,INTCHA,NDROP
      CHARACTER*4 CHAINT
      INTEGER NLINK,NAMIND,NBANK,INTCHA,NDROP
C
      COMMON/YV0CUT/IYV0IV,YV0CO1,YV0CO2,YV0CCT,YV0CPS,YV0CRV,YV0CZV,
     $              YV0DIP,YV0CC2,YV0CCS,YV0CC0,YV0CMA,YV0CPM,IYV0NT,
     $              IYV0CB,IYV0NC,YV0CMD,YV0CDZ,YV0CZZ,YV0CKW,YV0CLW,
     $              YV0CGW
CIF DOC
C
C     This common contains the cuts for V0 finding. It is filled
C     in YV0INI from the direct acces  bank 'YV0C'
C     IYV0IV  = Flag for take reconstructed vertex from JSUM bank
C               (IYV0IV=2),or from PYER bank (IYV0IV=1)
C                or vertex in (0.,0.,0.) (IYV0CV=0)
C
C     YV0CO1  = Min value of chi square increase constraining only
C                                     one track to the main vertex
C     YV0CO2  = Min value of chi square increase constraining both
C                                        tracks to the main vertex
C     YV0CCT  = Max value of distance between
C               starting points of fit
C     YV0CPS  = Minimum value for the psi angle
C     YV0CRV  = Max value of the V0 vertex radius
C     YV0CZV  = Max value of the V0 vertex abs(Z)
C     YV0DIP  = Max value of difference between tg of dip
C               (for test on parallelism between two tracks)
C     YV0CC2  = Max value of the V0 fit chi square
C     YV0CCS  = Minimum value of the cosinus of the angle between
C                                       V0 vertex and V0 momentum
C     YV0CC0  = Minimum value of the chi square increase constraining
C                                     V0 vertex to the primary vertex
C     YV0CMA  = Maximum value of the chi square of the mass constraint
C     YV0CPM  = Minimum value of daughter track momenta
C     IYV0NT  = Minimum no. of TPC hits on each daughter track
C     IYV0CB  = Flag for testing on coords before vx.
C                =0 no test; =1 at least one track to have less than NC
C                coords before vx; =2 both tracks to have less than NC
C                coords before vx.
C     IYV0NC  = Max. no. of coords. before vx.
C     YV0CMD  = Minimum distance of vx from primary vx (cm)
C     YV0CDZ  = Maximum d0 of V0
C     YV0CZZ  = Maximum z0 of V0
C     YV0CKW  = Keep candidate if pi-pi mass within YV0CKW GeV of k0
C     YV0CLW  =  "       "      "  p-pi  "     "    YV0CLW GeV of lambda
C     YV0CGW  =  "       "      "  e-e   "     "    YV0CGW GeV of zero
C
C---------------------------------------------------------------------
CEI
      INTEGER ALGTDB
      DATA NYV1C /0/
C
#include "bmacro.h"
C
          IER=1
C
C+First we check if there are cards that overwrite the Database
C
      IF (NYV1C.EQ.0) THEN
         NYV1C = NAMIND ('YV1C')
         IROLD = 0
         KYV0C = IW(NYV1C)
      ENDIF
      IF(KYV0C.NE.0)THEN
      IER=0
      GO TO 30
      ENDIF
C
C  Database reading
C
      IER=0
C
C+    first run found
c
c choosen value of nr for the yv1c bank is nryv1
      NRYV1=NDANR(LCONS,'YV1C','LE',100)

C
C+    IF YV1C has been found
C
      KYV0C = MDARD(IW,LCONS,'YV1C',NRYV1)
      IF (KYV0C.EQ.0) THEN
        IER=1
        RETURN
      ENDIF
 30   CONTINUE
      NRYV1=IW(KYV0C-2)
      IYV0IV=ITABL(KYV0C,LROWS(KYV0C),JYV1IV)
      YV0CO1=RTABL(KYV0C,LROWS(KYV0C),JYV1O1)
      YV0CO2=RTABL(KYV0C,LROWS(KYV0C),JYV1O2)
      YV0CCT=RTABL(KYV0C,LROWS(KYV0C),JYV1CT)
      YV0CPS=RTABL(KYV0C,LROWS(KYV0C),JYV1PS)
      YV0CRV=RTABL(KYV0C,LROWS(KYV0C),JYV1RV)
      YV0CZV=RTABL(KYV0C,LROWS(KYV0C),JYV1ZV)
      YV0DIP=RTABL(KYV0C,LROWS(KYV0C),JYV1DI)
      YV0CC2=RTABL(KYV0C,LROWS(KYV0C),JYV1CF)
      YV0CCS=RTABL(KYV0C,LROWS(KYV0C),JYV1CS)
      YV0CC0=RTABL(KYV0C,LROWS(KYV0C),JYV1CO)
      YV0CMA=RTABL(KYV0C,LROWS(KYV0C),JYV1MA)
C
C If new extended YV1C bank ......
      IF(LYV1CA.GT.15) THEN
        YV0CPM=RTABL(KYV0C,LROWS(KYV0C),JYV1PM)
        IYV0NT=ITABL(KYV0C,LROWS(KYV0C),JYV1NT)
        IYV0CB=ITABL(KYV0C,LROWS(KYV0C),JYV1CB)
        IYV0NC=ITABL(KYV0C,LROWS(KYV0C),JYV1NC)
        YV0CMD=RTABL(KYV0C,LROWS(KYV0C),JYV1MD)
        YV0CDZ=RTABL(KYV0C,LROWS(KYV0C),JYV1DZ)
        YV0CZZ=RTABL(KYV0C,LROWS(KYV0C),JYV1ZZ)
        YV0CKW=RTABL(KYV0C,LROWS(KYV0C),JYV1KW)
        YV0CLW=RTABL(KYV0C,LROWS(KYV0C),JYV1LW)
        YV0CGW=RTABL(KYV0C,LROWS(KYV0C),JYV1GW)
C Else use default values corresponding to no extra cuts .....
      ELSE
        YV0CPM= 0.0
        IYV0NT= 4
        IYV0CB= 0
        IYV0NC= 99
        YV0CMD= 0.0
        YV0CDZ= 999.
        YV0CZZ= 999.
        YV0CKW= 999.
        YV0CLW= 999.
        YV0CGW= 999.
      ENDIF
C
      IF (IW(6).GT.0) THEN
       WRITE(IW(6),1000)NRYV1
       WRITE(IW(6),1001)IYV0IV,YV0CO1,YV0CO2,YV0CCT,YV0CPS,
     $              YV0CRV,YV0CZV,YV0DIP,YV0CC2,YV0CCS,
     $              YV0CC0,YV0CMA,YV0CPM,IYV0NT,IYV0CB,
     $              IYV0NC,YV0CMD,YV0CDZ,YV0CZZ,YV0CKW,
     $              YV0CLW,YV0CGW
C
 1000 FORMAT(/3X,'+++ YV1INI +++ CUTS FOR V0 RECONSTRUCTION:',
     $ '  NR =',1X,I2)
 1001 FORMAT(5X,' IV',5X,' O1',5X,' O2',5X,' CT',5X,' PS',5X,' RV',
     $       5X,' ZV',5X,' DI',5X,' C2',5X,' CS',5X,' C0',5X,' MA',/
     $       1X,I7,6F8.2,F8.3,F8.1,F8.3,F8.2,F8.3,/
     $       5X,' PM',5X,' NT',5X,' CB',5X,' NC',5X,' MD',5X,' DZ',
     $       5X,' ZZ',5X,' KW',5X,' LW',5X,' GW',/
     $       1X,F7.3,3I8,3F8.2,3F8.3)
      ENDIF
      END
#endif
