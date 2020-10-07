      SUBROUTINE NREPYE(IRET)
CKEY NANO IN ALPHA /INTERNAL
C----------------------------------------------------------------------
C! NANO unpacking : create PYER bank from NDHE or NEHE bank
C!
C!   Author   :- Gerrit Graefe         15-FEB-1994
C!   Modified :- Gerrit Graefe         27-FEB-1995  Link to NDHE if
C!                                                  NAVERS>115
C!
C!   Inputs:
C!        - none
C!
C!   Outputs:
C!        - IRET / I        return code  0=created PYER from NDHE
C!                                       1=can't find NDHE
C!                                       2=no space to book PYER
C!                                       3=unknown error
C!
C!   Libraries required: ALPHA,BOS77
C!
C!   Description
C!   ===========
C!
C?   This subroutine creates the bank PYER from the NDHE bank. The only
C?   entry in this bank will be the main vertex stored in the NDHE bank.
C?
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
#include "nbpdcl.h"
#include "nbnkpo.h"
#include "ndhejj.h"
#include "nehejj.h"
#include "pyerjj.h"
      INTEGER NLINK,NVERS,NAMIND,NAVERS
      INTEGER I,J,INDNTK,INDFRT,IGARB,IROWN,IROWP,IBKLEN,KOPYER
      INTEGER IRET,INDNHE,INDPYE,INOCOL
      INTEGER MASK2,MASK4,MASK5
      DATA MASK2,MASK4,MASK5/3,15,31/
#include "qmacro.h"
C----------------------------------------------------------------------
C
C!..LINK TO BANK NDHE
C
      IRET=3
      CALL GETNAV(NAVERS)
      IF(NAVERS.EQ.115)THEN
        INDNHE=IW(NAMIND('NDHE'))
        IF(INDNHE.EQ.0)THEN
          IRET=1
          RETURN
        ENDIF
        KONDHE=INDNHE
        KCNDHE=IW(KONDHE+1)
        IBKLEN=IW(KONDHE+2)
        INOCOL=LNDHEA
        IROWN=KONDHE+LMHLEN
      ELSE
        INDNHE=IW(NAMIND('NEHE'))
        IF(INDNHE.EQ.0)THEN
          IRET=1
          RETURN
        ENDIF
        KONEHE=INDNHE
        KCNEHE=IW(KONEHE+1)
        IBKLEN=IW(KONEHE+2)
        INOCOL=LNEHEA
        IROWN=KONEHE+LMHLEN
      ENDIF
C
C!..CREATE BANK PYER
C
      CALL AUBOS('PYER',0,LMHLEN+LPYERA,INDPYE,IGARB)
      IF(IGARB.EQ.2)THEN
        CALL QWMESE('### NRECPG ### No space to book PYER')
        IRET=2
        RETURN
      ENDIF
      CALL BKFMT('PYER','2I,(I,5F,I)')
      NAPYER=NAMIND('PYER')
      KOPYER=INDPYE
      IW(KOPYER+1)=LPYERA
      IW(KOPYER+2)=IBKLEN
C
C!..FILL BANK PYER
C
      IROWP=KOPYER+LMHLEN
      DO 100 I=1,IBKLEN
C!..VERTEX COORDINATES
        IF(NAVERS.EQ.115)THEN
          RW(IROWP+JPYEVX)=FLOAT(IW(IROWN+JNDHVX)) / 10000.0
          RW(IROWP+JPYEVY)=FLOAT(IW(IROWN+JNDHVY)) / 10000.0
          RW(IROWP+JPYEVZ)=FLOAT(IW(IROWN+JNDHVZ)) / 10000.0
        ELSE
          RW(IROWP+JPYEVX)=FLOAT(IW(IROWN+JNEHVX)) / 10000.0
          RW(IROWP+JPYEVY)=FLOAT(IW(IROWN+JNEHVY)) / 10000.0
          RW(IROWP+JPYEVZ)=FLOAT(IW(IROWN+JNEHVZ)) / 10000.0
        ENDIF
C!..VERTEX TYPE
        IW(IROWP+JPYETY)=1
        IROWN=IROWN+INOCOL
        IROWP=IROWP+LPYERA
  100 CONTINUE
      CALL BLIST(IW,'S+','PYER')
      IRET=0
  999 RETURN
      END
#endif
