      SUBROUTINE DEFOVE
C.----------------------------------------------------------------------
CKEY GAMPACK ECAL GAMPEX CONSTANT / INTERNAL
C   J.C.Brient      Creation  1/10/91
C! define overlap region fron dbase bank ECGN and basic parameters
C   Input : None
C   Output: STORE ECOXA and  COMCUX common
C   Calls : EPLMST , ERWIDT
C   Called by GAMPEX
C.----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      PARAMETER ( NPLWA = 45 )
#include "ecoxa.h"
#include "ecgnjj.h"
#include "comcux.h"
C
      DIMENSION PLL(4)
      DIMENSION RLONG(NPLWA)  , RLARG(NPLWA)
      EXTERNAL AGETDB
#include "bmacro.h"

C
C read in data base the bank EGRP which contains the reconstruction
C parameters of GAMPEX
C
      LBASE = JUNIDB (0)
      JEGRP = MDARD (IW,LBASE,'EGRP',0 )
      IF(JEGRP .GT. 0) THEN
        DO I = 1 , LCOLS(JEGRP)
          RECPAR(I) = RTABL(JEGRP,1,I)
        ENDDO
      ELSE
        RECPAR(1) = 0.030
        RECPAR(2) = 0.075
        RECPAR(3) = 0.150
        RECPAR(4) = 0.200
        RECPAR(5) = 0.25
        RECPAR(6) = 2.0
        RECPAR(7) = 1.
        RECPAR(8) = 1.
      ENDIF
C
C E4/Etot mean value from J.Badier , for the Barrel storeys width
C-----------------------------------------------------------------
      E4ETB = 0.851
C
C R and Z middle stack1 ECAL
C---------------------------
      CALL EPLMST('ALEPH',1,1,1,PLL)
      ZESTK1 = ABS( PLL(4) )
      CALL EPLMST('ALEPH',2,1,1,PLL)
      RESTK1 = ABS( PLL(4) )
C
C width of storey from H.Videau
C -------------------------------
      CALL ERWIDT(RLONG,RLARG)
      SUMD= 0.
      DO I = 1 , NPLWA
        SUMD = SUMD + RLONG(I)
      ENDDO
      STWIDT = SUMD/FLOAT(NPLWA)
      DO I = 1 , NPLWA
        STRPHI(I) = RLARG(I)
      ENDDO
      JECGN  = IW(NAMIND('ECGN'))
      I17    = ITABL(JECGN,1,JECGPR)
      I12    = ITABL(JECGN,1,JECGRW)
      I13    = ITABL(JECGN,1,JECGXW)
      NOV    = (I13-I12)/2
C
      ITOV1  = I17+1
C --itov1=46
      ITOV2  = I17+NOV
C --itov2=50
      ITOV3  = I13+1-ITOV1
C --itov3=179
      ITOV4  = I13+1-ITOV2
C --itov4=183
      ITHTO  = I13
C --ithto=228
      RETURN
      END
#endif
