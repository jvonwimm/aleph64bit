      SUBROUTINE VGWCNS(IMOD,IWAF,VDEPL,VMU,VTE,VDTHL,VFPOS,VDAPP)
C----------------------------------------------------------------------
C! Get wafer constants from DAF
CKEY VDET DBASE
C!
C!  Author         A. Bonissent 15-Jan-1994
C!
C!  Description
C!  ===========
C! Get Vdet constants for one wafer
C! Input : IMOD module number
C!         IWAF wafer number
C! Output : VDEPL Depletion voltage
C!          VMU   Electron or holes mobility
C!          VTE   Electron or holes temperature
C!          VDTHL Electron or holes mobility in silicon
C!          VFPOS Face position in local coordinates
C!          VDAPP Applied voltage
C
C-----------------------------------------------------------------------
C
C
      DIMENSION VMU(*),VTE(*),VDTHL(*),VFPOS(2)
      PARAMETER(JVDEEM=1,JVDEHM=2,JVDEET=3,JVDEHT=4,JVDEHE=5,JVDEHH=6,
     +          JVDEAV=7,JVDEDV=8,JVDEST=9,JVDEEC=11,JVDENL=13,
     +          JVDENC=14,JVDESN=16,JVDENE=18,JVDEPN=20,JVDEIC=22,
     +          JVDEMF=24,JVDEEG=25,LVDEPA=25)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C
      SAVE NAVDEP
C
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
      IF (FIRST) THEN
         FIRST = .FALSE.
         NAVDEP = NAMIND('VDEP')
      ENDIF
      KVDEP=IW(NAVDEP)
      VMU(1) = RTABL(KVDEP,1,JVDEEM)
      VMU(2) = RTABL(KVDEP,1,JVDEHM)
      VTE(1) = RTABL(KVDEP,1,JVDEET)
      VTE(2) = RTABL(KVDEP,1,JVDEHT)
      VDTHL(1) = RTABL(KVDEP,1,JVDEHE) * ALFIEL(DUMM) *1.E-5
      VDTHL(2) = RTABL(KVDEP,1,JVDEHH) * ALFIEL(DUMM) *1.E-5
      VFPOS(1) = -VWTHCK()*0.5
      VFPOS(2) = +VWTHCK()*0.5
      VDEPL = RTABL(KVDEP,1,JVDEDV)
      VDAPP = RTABL(KVDEP,1,JVDEAV)
      RETURN
      END
