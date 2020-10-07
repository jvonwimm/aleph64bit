*DK everif
      SUBROUTINE EVERIF(ILG,ICL,IMODU,IOK)
C --------------------------------------------------
CKEY ECAL
C! check consistency of ecal address
C - MNM - 960404
C - Input   : ILG    / I = ecal row no.
C             ICL    / I = ecal column no.
C             IMODU  / I = ecal module no.
C - Output  : IOK    / I = new row number if .ne. 0
C --------------------------------------------------
      DIMENSION IBZONE(4) , IPZONE (4)
      DATA IBZONE / 8,24,40,50 /
      DATA IPZONE / 8, 16, 24, 32 /
C --------------------------------------------------
C
C     Verify ILG/ICL consistency in ENDCAP
C
      IOK = 0
      ILG0 = ILG
      IDEBUG = 0
      IF (ILG.LT.50.OR.ILG.GT.178) THEN
         IF ( ILG.GT.178) ILG0 = 228-ILG+1
         IMOD = MOD (IMODU-1,12)+1
         IZON =1
         DO IL =1,3
         IF(ILG0.GT.IBZONE(IL)) IZON = IL+1
         ENDDO
         IMOR =((ICL+(0.5*IPZONE(IZON))-1)/IPZONE(IZON))+1
         IMOR0 = IMOR
         IF (IMOR.GT.12) IMOR =1
         IF ( IMOR .NE.IMOD) THEN
           IF (IMOR0.LT.IMOD) THEN
            IOK = ILG0 -1
           ELSE
            IOK = ILG0 +1
           ENDIF
         ENDIF
       ENDIF
       IF ( ILG.NE.ILG0.AND.IOK.NE.0) IOK = 228-IOK +1
       RETURN
       END
