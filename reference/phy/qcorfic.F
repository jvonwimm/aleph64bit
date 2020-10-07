      REAL FUNCTION QCORFIC(ENE,ITH,IPH)

#include "qcde.h"
      REAL FIC(36), IM96O(36), IM96N(36), IM97O(36), IM97N(36)
      REAL GC96N(2), GC97N(2)
      REAL IMO(2,36), IMN(2,36), GCN(2,2)
      INTEGER INEVT / -1 /
      LOGICAL FIRST / .TRUE. /
      DATA FIC / 0.599, 0.477, 0.466, 0.460, 0.518, 0.472,
     &           0.508, 0.547, 0.536, 0.500, 0.527, 0.623,
     &           0.624, 0.692, 0.773, 0.753, 0.706, 0.722,
     &           0.819, 0.741, 0.778, 0.799, 0.653, 0.699,
     &           0.625, 0.621, 0.591, 0.566, 0.631, 0.667,
     &           0.674, 0.676, 0.751, 0.660, 0.698, 0.645 /
      DATA IM96O / 1., 1., 1., 1., 1., 1.,
     &             1., 1., 1., 1., 1., 1.,
     &             1., 1., 1., 1., 1., 1.,
     &             1., 1., 1., 1., 1., 1.,
     &             1., 1., 1., 1., 1., 1.,
     &             1., 1., 1., 1., 1., 1. /
      DATA IM96N / 1.014, 1.040, 1.010, 1.001, 0.978, 1.001,
     &             0.998, 0.986, 0.976, 0.963, 0.939, 0.990,
     &             1.021, 1.034, 1.018, 0.984, 0.957, 1.031,
     &             1.001, 1.020, 1.035, 0.984, 0.968, 1.049,
     &             0.959, 0.987, 0.963, 0.927, 0.987, 1.060,
     &             1.003, 1.009, 1.129, 0.982, 1.057, 0.938 /
      DATA IM97O / 0.956, 0.993, 1.008, 1.028, 0.979, 0.972,
     &             0.906, 1.089, 1.105, 0.929, 0.954, 0.908,
     &             0.929, 0.987, 1.033, 1.062, 0.969, 0.931,
     &             0.994, 1.091, 1.067, 1.005, 1.038, 1.067, 
     &             0.892, 0.912, 0.936, 0.976, 0.981, 1.071,
     &             0.967, 1.044, 1.191, 1.041, 0.987, 1.002 /
      DATA IM97N / 0.961, 1.050, 0.992, 1.002, 1.065, 0.964,
     &             1.029, 1.016, 1.027, 0.996, 0.986, 0.968,
     &             0.972, 0.988, 0.957, 0.968, 0.993, 1.014,
     &             0.954, 1.003, 1.051, 1.008, 0.988, 1.050,
     &             0.971, 0.997, 0.952, 0.980, 0.977, 1.023,
     &             0.939, 1.073, 1.119, 0.979, 1.014, 0.972 /
      DATA GC96N / 0.663, 0.487 /
      DATA GC97N / 0.665, 0.504 /

      SAVE
#include "qmacro.h"

C     To do once per job
      IF (FIRST) THEN
         WRITE(KUPRNT,*)
     &   '++++++ HCAL ONLINE BUG CORRECTION ENABLED ++++++'
         
C     Filling some matrices
         DO I = 1, 36
            IMO(1,I) = IM96O(I)
            IMO(2,I) = IM97O(I)
            IMN(1,I) = IM96N(I)
            IMN(2,I) = IM97N(I)
         ENDDO
         DO I = 1, 2
            GCN(1,I) = GC96N(I)
            GCN(2,I) = GC97N(I)
         ENDDO
         IND = MDARD(IW,KUCONS,'HNGR',0)
         FIRST = .FALSE.
      ENDIF

      QCORFIC = ENE

C     To do once per event
      IF (KNEVT.NE.INEVT) THEN
         
C     Let's begin with the correction of the online bug...
         IBUG = 0
         IF (KRUN.GE.41298.AND.KRUN.LE.45289) IBUG = 1

C     Loads online calibration constant
         JHCCV = IW(NAMIND('HCCV'))
         IF (JHCCV.LE.0.AND.IBUG.EQ.1) THEN
            CALL QWMESS('++++ HCCV is missing: HCAL online '//
     &           'bug cannot be corrected!!')
            RETURN
         ENDIF

C     Recalibrate only 1996 and 1997 data!
         IF (KRUN.GE.41000.AND.KRUN.LT.43000) THEN
            IYEAR = 1
         ELSEIF (KRUN.GE.43000.AND.KRUN.LT.45000) THEN
            IYEAR = 2
         ELSE
            IYEAR = 0
         ENDIF
         JHT0C = IW(NAMIND('HT0C'))
         JHSTD = IW(NAMIND('HSTD'))
         IF ((JHT0C.LE.0.OR.JHSTD.LE.0).AND.IYEAR.GT.0) THEN
            CALL QWMESE('+++++ HT0C or HSTD banks missing: '//
     &           'HCAL cannot be recalibrated!!')
            RETURN
         ENDIF
         INEVT = KNEVT
      ENDIF

C     Determines the module number (1-24 barrel, 25-36 endcaps)
      JHNGR = IW(NAMIND('HNGR'))
      IF (JHNGR.LE.0) THEN
         CALL QWMESE('+++++ HNGR bank missing: '//
     &        'HCAL cannot be recalibrated!!')
         RETURN
      ENDIF
      IHNGR = ITABL(JHNGR,ITH,IPH)
      IHBA = MOD(IHNGR,1000)
      IHEC = IHNGR/1000
      IF (IHBA .GT. 0) THEN
         ISUB = 1
         IMD = (IPH-1)/4 + 1
      ELSE
         ISUB = 2
         IMD = (IHEC-1)/10 + 1 + 24
      ENDIF

C     Rescales the tower energy to the right value
      IF (IBUG.EQ.1) THEN
         TUB = SNGL(IW(JHCCV+3))
         CAL = 10000.*FIC(IMD)/TUB
         WRG = SNGL(INT(CAL))
         QCORFIC = ENE * CAL / WRG
      ENDIF

C     Now we have to remove the old calibration and to put in
C     the new one...
      IF (IYEAR.GT.0) THEN
         GCO = RTABL(JHT0C,1,2*ISUB)
         GCTO = RTABL(JHSTD,1,ISUB)
         CORR = GCO * GCTO * IMO(IYEAR,IMD) /
     &        ( GCN(IYEAR,ISUB) * IMN(IYEAR,IMD) )
         QCORFIC = QCORFIC * CORR
      ENDIF

      RETURN
      END

