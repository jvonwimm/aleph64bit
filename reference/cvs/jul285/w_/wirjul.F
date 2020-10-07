      SUBROUTINE WIRJUL
C---------------------------------------------------------
C!    Correct wire energy for run 4001-4095
C-    Banks EWDI EWHE updated
C                                M.N Minard 1/10/89
C?
C!======================================================================
#ifndef DOC
#include "pewijj.h"
#include "bcs.h"
#include "rcurnt.h"
      EXTERNAL NAMIND
      DIMENSION  EOWIR (53)  ,IT0Z (36) ,INADC(8,2)
      DIMENSION INWIRF ( 53)
      DIMENSION S(8) , T (8) , SVAL (2)
      DATA SVAL / 0.17 , 0.24 /
      DATA T/0.,512.,1024.,1536.,2048.,2560.,3072.,3584./
      DATA ADCOV,XNMIN/2880.,-800./
      DATA IOVFA , IADWI / 4090 , 10000 /
      DATA TOFSET/1708./
      DATA IADMX / 800. /
      DATA XVAL / 0.19/
#include "bmacro.h"
C
C-    Check if correction bank is there
C
C
C-    Module / module recalculate gains using solution 1
C
      IRUNC = IRUNRC
      NAGAIN = NAMIND ('EWGA')
      KGAIN = IW ( NAGAIN)
      IF (KGAIN.LE.0) GO TO 990
      NGAIN = LROWS ( KGAIN )
      NAEWDI = NAMIND ('EWDI')
      KEWDI = IW ( NAEWDI)
      IF ( KEWDI .EQ.0 ) GO TO 990
      NEWDI = LROWS ( KEWDI )
      NAEWHE = NAMIND ('EWHE')
      KEWHE = IW ( NAEWHE)
C
C-   Recalculate pedestal for this module
C
      ISUP = 0
      DO 100 IEWDI = 1,NEWDI
      IMOD = ITABL ( KEWDI,IEWDI,1)
      IF ( IMOD.LT.1.OR.IMOD.GT.36 ) GO TO 990
      JGAIN = KROW (KGAIN,IMOD)
C
C-    Reset flag at beginning of each roc
C
      IF (IMOD.EQ.13.OR.IMOD.EQ.25 ) ISUP = 0
      IF ( IMOD.GT.12.AND.IMOD.LT.25.AND.MOD(IMOD,2).EQ.1) ISUP =0
      IF (IRUNC.LT.4089.AND.(IMOD.LT.13.OR.IMOD.GT.24)) GO TO 100
      ISUPC = ISUP
C
C-    Treat gain determination
C
      IF ( IMOD.EQ.36 ) GO TO 95
      IF ( IMOD.GT.12.AND.IMOD.LT.25.AND.MOD(IMOD,2).EQ.0) GO TO 95
      IF ( ISUP.EQ.0) THEN
      IADC0 = ITABL ( KEWDI , IEWDI , 47)
      IADCS = ITABL (KEWDI,IEWDI,54)
      INAS = 53
      JJGAIN = JGAIN + (INAS-1)*4
      XNADC1 = FLOAT(IADCS)/FLOAT(IW(JJGAIN+3))+FLOAT(IW(JJGAIN+4))
      XNADC8 = FLOAT(IADCS)/FLOAT(IW(JJGAIN+1))+FLOAT(IW(JJGAIN+2))
      IF ( XNADC8.GT.ADCOV ) THEN
C
C-    Verify  next channel compatible with gain change
C
       KJGAIN = KROW ( KGAIN,IMOD+1)
       IADCN = ITABL(KEWDI,IEWDI+1,2)
       XNNDC8 = FLOAT (IADCN)*FLOAT(IW(KJGAIN+1))
     *  /FLOAT(IW(KJGAIN+3))-(FLOAT(IW(KJGAIN+2)-IW(KJGAIN+4)))*
     *  FLOAT (IW(KJGAIN+1))
        IF ( XNNDC8.GT.XNMIN ) ISUP = 1
        IF ( XNADC8.GT.ADCOV+1000 ) ISUP= 1
      ENDIF
      GO TO 100
      ENDIF
C
C-    Now do the suppression
C
 95   CONTINUE
      IF ( ISUP.EQ.0) GO TO 100
      ENSUM = 0.
      DO 50 IADC = 1, 53
      JTGAIN = JGAIN + (IADC-1)*4
      JFGAIN = JGAIN + (IADC-1)*4 + ISUP*2
C
C-    Calculate signal for the 2 gains hypothesis
C
      GAINFA = FLOAT ( IW( JFGAIN + 3 ))
      IF ( IMOD.EQ.36.AND.IADC.EQ.53 ) THEN
           GAINFA = FLOAT ( IW( JFGAIN-1))
      ENDIF
      XNWIR8 = FLOAT (ITABL( KEWDI,IEWDI,1+IADC))*FLOAT(IW(JTGAIN+1))
     */FLOAT(IW(JFGAIN+1))-(FLOAT(IW(JTGAIN+2)-IW(JFGAIN+2)))*
     * FLOAT (IW(JTGAIN+1))
      XNWIR1 = FLOAT (ITABL( KEWDI,IEWDI,1+IADC))*FLOAT(IW(JTGAIN+3))
     */GAINFA-(FLOAT(IW(JTGAIN+4)-IW(JFGAIN+4)))*
     * FLOAT (IW(JTGAIN+3))
C
C-     In wire plane choose gain level
C
      IF ( IADC . LT . 46 ) THEN
C
C-      Store wire info
C
        INWIRT = XNWIR8
        IF ( IADC.GT.1 ) THEN
C
C-      Calculate the number of adc count
C
        IADCC = XNWIR1/FLOAT(IW (JTGAIN+1))+ IW ( JTGAIN + 2 )
        IF ( IADCC.GT.IOVFA.AND.IADCC.LT.8*IOVFA-100) THEN
         IF ( INWIRF(IADC-1) .GT. IADWI) THEN
         IF(ABS(INWIRF(IADC-1)-XNWIR8).GT.10*ABS(INWIRF(IADC-1)))THEN
            INWIRT= XNWIR1
          ENDIF
         ENDIF
        ENDIF
        ENDIF
        INWIRF ( IADC ) = INWIRT
        ENSUM  = ENSUM + INWIRT/1000.
      ENDIF
      IF ( IADC.GE.46) THEN
        INADC ( IADC-45,1) = XNWIR8
        INADC ( IADC-45,2) = XNWIR1
      ENDIF
C
C-    Now check level for pastis
C
 50   CONTINUE
C
C-    Pastis gain
C
      JPGAIN = JGAIN + 52 * 4
      IPADC = INADC(8,2)/IW(JPGAIN+1)+IW(JPGAIN+2)
       IGPG = 1
       IF (IMOD.EQ.36 ) INADC (8,1 ) = ENSUM*0.2
       XDIF = ENSUM-FLOAT (INADC(8,1)*5)
       IF ( ABS(XDIF).GT.2000.) THEN
       IF (IPADC.GT.2880) ISUP = ISUP + 1
       IF ( IPADC.GT.2880) IGPG = 2
       ENDIF
       CALL UCOPY ( INADC(1,IGPG),INWIRF(46),8)
C
C-     Update banks
C
       JEWHE = KROW (KEWHE,IEWDI)
       JEWDI = KROW (KEWDI,IEWDI)
       CALL UCOPY ( INWIRF(1),IW(JEWDI+2),53)
       IF ( KEWHE . GT. 0 ) THEN
       JEWHE = KROW (KEWHE,IEWDI)
       IW ( JEWHE+1 ) = ENSUM * 1000.
       ENDIF
 100   CONTINUE
 990   CONTINUE
 980   CONTINUE
       RETURN
       END
#endif
