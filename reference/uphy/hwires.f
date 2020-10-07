      SUBROUTINE HPLAND
C********************************************************************
C!HCAL wire planes decoding routine.                                *
C                                                                   *
C    Author : R.Tenchini                                            *
C                  18.7.91                                          *
C                  MODIFIED 11.1.93                                 *
C                  Modified 13.9.94 (S.Salomone)
C    Input bank : HPDI                                              *
C                                                                   *
C    Ouput in common HDPLA containig the calibrated                 *
C          energy in each of the 12 planes of the                   *
C          24 barrel modules, of the 6 modules of                   *
C          endcap A and the 6 of encap B                            *
C                                                                   *
C********************************************************************
       INCLUDE '/aleph/phy/qcde.inc'
       INCLUDE '/aleph/phy/qhac.inc'
      COMMON/HDPLA/EPLANE(24,12),EPLAEA(6,12),EPLAEB(6,12)
      PARAMETER (CAL90=1.83)
      PARAMETER (C91EA0=1.43)
      PARAMETER (C91BA=2.18,C91EA=3.40,C91EB=3.40)
      PARAMETER (C92BA=2.16,C92EA=2.19,C92EB=2.22)
      PARAMETER (C93BA=2.14,C93EA=2.17,C93EB=2.21)
C CALIBRATION WITH DIMUONS
C     PARAMETER (C91BA=1.88,C91EA=3.98,C91EB=3.98)
C     PARAMETER (C92BA=1.90,C92EA=2.14,C92EB=2.14)
       INCLUDE '/aleph/phy/qmacro.inc'
C-----------------------------------------------------------------------
      KHPDI=NLINK('HPDI',0)
      IF(KHPDI.EQ.0) RETURN
      NHPDI=LROWS(KHPDI)
      LHPDI=LCOLS(KHPDI)
      CALL VZERO(EPLANE,288)
      CALL VZERO(EPLAEA,72)
      CALL VZERO(EPLAEB,72)
      IF(XMCEV) THEN
        PLCBA=1.
        PLCEA=1.
        PLCEB=1.
        KAJOB=NLINK('AJOB',0)
        IGVER=ITABL(KAJOB,1,9)
      ELSE
        PLCBA=C91BA
        PLCEA=C91EA
        PLCEB=C91EB
        IF(KRUN.LT.10000) PLCBA=CAL90
        IF(KRUN.LT.11671) PLCEA=C91EA0
        IF(KRUN.GT.14207) THEN
           PLCBA=C92BA
           PLCEA=C92EA
           PLCEB=C92EB
        ENDIF
        IF(KRUN.GT.20164) THEN
           PLCBA=C93BA
           PLCEA=C93EA
           PLCEB=C93EB
        ENDIF
      ENDIF
      DO 100 I=1,NHPDI
        IADD=ITABL(KHPDI,I,JHPDPA)
        IF(.NOT.XMCEV.AND.KRUN.LT.10000) THEN
          IF(IADD.LE.10000) THEN
            IMODUL=(IADD-1)/12+1
            ICHK  =MOD(IMODUL,2)
            IF(ICHK.EQ.0) THEN
              IMODUL=IMODUL-1
            ELSE
              IMODUL=IMODUL+1
            ENDIF
            IPLAN=MOD(IADD,12)
            IF(IPLAN.EQ.0) IPLAN=12
            IPLAN=13-IPLAN
            ICHK  =MOD(IPLAN,2)
            IF(ICHK.EQ.0) THEN
              IPLAN=IPLAN-1
            ELSE
              IPLAN=IPLAN+1
            ENDIF
            ISUBD=2
          ELSE
            IPLAN=IBITS(IADD,0,8)
            IMODUL=IBITS(IADD,8,8)
            IMODUL=IMODUL+1
            ISUBD=IBITS(IADD,16,8)
          ENDIF
        ELSE
          IPLAN=IBITS(IADD,0,8)
          IMODUL=IBITS(IADD,8,8)
          ISUBD=IBITS(IADD,16,8)

          IF((XMCEV.AND.IGVER.LT.242.AND.ISUBD.NE.2).OR.
     &       (.NOT.XMCEV.AND.KRUN.GE.14209.AND.ISUBD.NE.2).OR.
     &       (XMCEV.AND.IGVER.GE.253.AND.ISUBD.NE.2)) THEN
            IF(IPLAN.GE.2.AND.IPLAN.LE.21) THEN
              IPLAN=IPLAN/2+1
            ENDIF
            IF(IPLAN.EQ.22) IPLAN=12
          ENDIF
        ENDIF
        IF(ISUBD.EQ.2) THEN
          ENER=ITABL(KHPDI,I,JHPDED)/(1000.*PLCBA)
          IF(IMODUL.GE.1.AND.IMODUL.LE.24.
     &      AND.IPLAN.GE.1.AND.IPLAN.LE.12) THEN
            EPLANE(IMODUL,IPLAN)=ENER
          ENDIF
        ELSEIF(ISUBD.EQ.1) THEN
          ENER=ITABL(KHPDI,I,JHPDED)/(1000.*PLCEA)
          IF(IMODUL.GE.1.AND.IMODUL.LE.6.
     &      AND.IPLAN.GE.1.AND.IPLAN.LE.12) THEN
            EPLAEA(IMODUL,IPLAN)=ENER+EPLAEA(IMODUL,IPLAN)
          ENDIF
        ELSEIF(ISUBD.EQ.3) THEN
          ENER=ITABL(KHPDI,I,JHPDED)/(1000.*PLCEB)
          IF(IMODUL.GE.1.AND.IMODUL.LE.6.
     &      AND.IPLAN.GE.1.AND.IPLAN.LE.12) THEN
            EPLAEB(IMODUL,IPLAN)=ENER+EPLAEB(IMODUL,IPLAN)
          ENDIF
        ENDIF
  100 CONTINUE
      RETURN
      END
      SUBROUTINE HPLTRK(ITRA,EVECB,EVECE,NEXB,NEXE)
C********************************************************************
C!HCAL wire plane energy for a given ALPHA charged track            *
C                                                                   *
C    Author : R.Tenchini                                            *
C                  08.8.91                                          *
C                                                                   *
C      You have to call HPLAND for each event                       *
C                                                                   *
C    Input  :                                                       *
C             ITRA =  ALPHA charged track (ADD 50 if JULIA track)   *
C    Ouput :                                                        *
C             EVECB = Real vector 12 elements. Energy in each       *
C                     barrel plane for that track                   *
C             NEXB  = Integer vector 12 elements. If element        *
C                     equal to 1 than that plane is geometrically   *
C                     allowed to fire (barrel).                     *
C             EVECE = Real vector 12 elements. Energy in each       *
C                     endcap plane for that track                   *
C             NEXE  = Integer vector 12 elements. If element        *
C                     equal to 1 than that plane is geometrically   *
C                     allowed to fire (endcaps).                    *
C                                                                   *
C********************************************************************
       INCLUDE '/aleph/phy/qcde.inc'
       INCLUDE '/aleph/phy/qhac.inc'
      COMMON/HDPLA/EPLANE(24,12),EPLAEA(6,12),EPLAEB(6,12)
      DIMENSION    NPLANE(24,12),NPLAEA(6,12),NPLAEB(6,12)
      DIMENSION    EVECB(12),EVECE(12),NEXB(12),NEXE(12)
      PARAMETER(LENMAX=1000)
      DIMENSION ICLIS(LENMAX)
      EXTERNAL NUMTOW
      EXTERNAL LASTBA
      EXTERNAL LASTEC
      EXTERNAL FIRSEC
      INTEGER FIRSEC
       INCLUDE '/aleph/phy/qmacro.inc'
      CALL VZERO(NPLANE,288)
      CALL VZERO(NPLAEA,72)
      CALL VZERO(NPLAEB,72)
      CALL VZERO(EVECB,12)
      CALL VZERO(EVECE,12)
      CALL VZERO(NEXB,12)
      CALL VZERO(NEXE,12)
      KPHST=NLINK('PHST',0)
      IF(KPHST.EQ.0) RETURN
      NPHST=LROWS(KPHST)
      IPCRL=NLINK('PCRL',0)
      IF(IPCRL.EQ.0) RETURN
      ITR=ITRA-50
      CALL MAKLIS(IPCRL,JPCRPF,JPCRPH,ITR,NCLUS,ICLIS,IER)
      IF(IER.NE.0) THEN
         IF(IER.GT.0) WRITE(76,*) ' ERRORE IN MAKLIS',IER
         GO TO 990
      ENDIF
      IF(NCLUS.GT.LENMAX) THEN
         IF(IER.GT.0) WRITE(76,*) ' NTRAK.GT.LENMAXIS'
         GO TO 990
      ENDIF
      DO 6 K=1,NPHST
         ITHE = ITABL(KPHST,K,1)
         IPHI = ITABL(KPHST,K,2)
         IPHC = ITABL(KPHST,K,4)
         DO 7 J=1,NCLUS
            ICL=ICLIS(J)
            IF(IPHC.EQ.ICL) THEN
C
C BARREL
C
               IF(ITHE.GE.14.AND.ITHE.LE.49) THEN
                  IPRIM=1
                  IMOD=(IPHI-1)/4+1
                  JROW=IPHI-4*(IMOD-1)
                  IULTI=LASTBA(ITHE,JROW)
                  DO 8 JJ=IPRIM,IULTI
                     NEXB(JJ)=1
                     NPLANE(IMOD,JJ)=1
 8                CONTINUE
               ENDIF
C
C  ENDCAP A
C
               IF(ITHE.LE.17) THEN
                  NUHAR=NUMTOW(ITHE,IPHI,IMOD)
                  IPRIM=FIRSEC(NUHAR)
                  IULTI=LASTEC(NUHAR)
                  DO 18 JJ=IPRIM,IULTI
                     IF(NUHAR.GE.57.AND.NUHAR.LE.62) THEN
                        IF(JJ.EQ.3.OR.JJ.EQ.4) GO TO 18
                     ENDIF
                     NEXE(JJ)=1
                     NPLAEA(IMOD,JJ)=1
 18               CONTINUE
               ENDIF
C
C  ENDCAP B
C
               IF(ITHE.GE.46) THEN
                  NUHAR=NUMTOW(ITHE,IPHI,IMOD)
                  IPRIM=FIRSEC(NUHAR)
                  IULTI=LASTEC(NUHAR)
                  DO 28 JJ=IPRIM,IULTI
                     IF(NUHAR.GE.57.AND.NUHAR.LE.62) THEN
                        IF(JJ.EQ.3.OR.JJ.EQ.4) GO TO 28
                     ENDIF
                     NEXE(JJ)=1
                     NPLAEB(IMOD,JJ)=1
 28               CONTINUE
               ENDIF
            ENDIF
 7       CONTINUE
 6    CONTINUE
      DO 10 I=1,24
         DO 20 J=1,12
            IF(NPLANE(I,J).EQ.1) EVECB(J)=EVECB(J)+EPLANE(I,J)
 20      CONTINUE
 10   CONTINUE
      DO 11 I=1,6
         DO 21 J=1,12
            IF(NPLAEA(I,J).EQ.1) EVECE(J)=EVECE(J)+EPLAEA(I,J)
 21      CONTINUE
 11   CONTINUE
      DO 12 I=1,6
         DO 22 J=1,12
            IF(NPLAEB(I,J).EQ.1) EVECE(J)=EVECE(J)+EPLAEB(I,J)
 22      CONTINUE
 12   CONTINUE
C     WRITE(6,*) ' <<<<< PATT PIANI BARREL >>>>>'
C     DO I=1,24
C        WRITE(6,2) (NPLANE(I,J),J=1,12)
C     ENDDO
C     WRITE(6,*) ' <<<<< PATT PIANI ENDCAP A >>>>>'
C     DO I=1,6
C        WRITE(6,2) (NPLAEA(I,J),J=1,12)
C     ENDDO
C     WRITE(6,*) ' <<<<<< PATT PIANI ENDCAP B >>>>>'
C     DO I=1,6
C        WRITE(6,2) (NPLAEB(I,J),J=1,12)
C     ENDDO
2     FORMAT(12(2X,I4))
 990  RETURN
      END
      INTEGER FUNCTION NUMTOW(II,JJ,N)
C********************************************************************
C!Hcal endcap tower conversion from software to hardware            *
C                                                                   *
C    Authors : R.Tenchini + Unknown                                 *
C                  18.7.91                                          *
C    Input:                                                         *
C        II      (I)     theta index of tower                       *
C        JJ      (I)     phi index of tower                         *
C                                                                   *
C    Output:                                                        *
C        N       (I)     submodule number                           *
C        NUMTOW  (I)     hardware number                            *
C                                                                   *
C********************************************************************
      INTEGER PHILIM(17) /4*4,6*8,6*16,15/
      INTEGER PH2LIM(17) /4*4,6*8,6*16,16/
      NUMTOW = 0
      J = JJ
      I = II
      IF(I.GT.17)I=63-I
      DO 10 K=1,I-1
         NUMTOW = NUMTOW + PH2LIM(K)
 10   CONTINUE
      N = INT ((J-1)/PH2LIM(I))
      J = J - N * PH2LIM(I)
      N = N + 1
      IF (II.GT.17)THEN
         IF(N.NE.1.AND.N.NE.4) THEN
            NUMTOW = NUMTOW + PH2LIM(I)-J+1
         ELSE
            NUMTOW = NUMTOW + J
         ENDIF
      ELSE
         IF(N.EQ.3.OR.N.EQ.6) THEN
            NUMTOW = NUMTOW + PH2LIM(I)-J+1
         ELSE
            NUMTOW = NUMTOW + J
         ENDIF
      ENDIF
C WRITE(6,*) 'NUMTOW, II, JJ, N,I,J',NUMTOW,II,JJ,N,I,J
      RETURN
      END
      INTEGER FUNCTION LASTBA(ITHE,JROW)
C********************************************************************
C!Hcal barrel last expected wire plane                              *
C                                                                   *
C    Author : R.Tenchini                                            *
C                  08.8.91                                          *
C    Input:                                                         *
C        ITHE    (I)     theta index of tower                       *
C        IROW    (I)     row in barrel module (from 1 to 4)         *
C                                                                   *
C    Output:                                                        *
C        LASTBA  (I)     last wire plane                            *
C                                                                   *
C********************************************************************
      LASTBA=12
      IF(ITHE.GE.19.AND.ITHE.LE.44) RETURN
      IF(ITHE.LE.13.OR.ITHE.GE.50) THEN
         LASTBA=0
         RETURN
      ENDIF
      IF(JROW.EQ.1.OR.JROW.EQ.3) THEN
         IF(ITHE.EQ.14) LASTBA=2
         IF(ITHE.EQ.15) LASTBA=4
         IF(ITHE.EQ.16) LASTBA=6
         IF(ITHE.EQ.17) LASTBA=8
         IF(ITHE.EQ.49) LASTBA=2
         IF(ITHE.EQ.48) LASTBA=4
         IF(ITHE.EQ.47) LASTBA=6
         IF(ITHE.EQ.46) LASTBA=10
      ENDIF
      IF(JROW.EQ.2) THEN
         IF(ITHE.EQ.14) LASTBA=2
         IF(ITHE.EQ.15) LASTBA=4
         IF(ITHE.EQ.16) LASTBA=6
         IF(ITHE.EQ.17) LASTBA=8
         IF(ITHE.EQ.49) LASTBA=4
         IF(ITHE.EQ.48) LASTBA=6
         IF(ITHE.EQ.47) LASTBA=6
         IF(ITHE.EQ.46) LASTBA=10
      ENDIF
      IF(JROW.EQ.4) THEN
         IF(ITHE.EQ.14) LASTBA=2
         IF(ITHE.EQ.15) LASTBA=4
         IF(ITHE.EQ.16) LASTBA=6
         IF(ITHE.EQ.17) LASTBA=8
         IF(ITHE.EQ.18) LASTBA=10
         IF(ITHE.EQ.49) LASTBA=2
         IF(ITHE.EQ.48) LASTBA=4
         IF(ITHE.EQ.47) LASTBA=6
         IF(ITHE.EQ.46) LASTBA=8
         IF(ITHE.EQ.45) LASTBA=10
      ENDIF
      RETURN
      END
      INTEGER FUNCTION LASTEC(ITOW)
C********************************************************************
C!Hcal endcap last expected wire plane                              *
C                                                                   *
C    Author : R.Tenchini                                            *
C                  08.8.91                                          *
C    Input:                                                         *
C        ITOW    (I)     hardware index of tower                    *
C                                                                   *
C    Output:                                                        *
C        LASTEC  (I)     last wire plane                            *
C                                                                   *
C********************************************************************
      LASTEC=12
      IF(ITOW.GT.127) LASTEC=11
      IF(ITOW.GT.128) LASTEC=10
      IF(ITOW.GT.143) LASTEC=9
      IF(ITOW.GT.144) LASTEC=8
      IF(ITOW.GT.159) LASTEC=6
      IF(ITOW.GT.174) LASTEC=5
      RETURN
      END
      INTEGER FUNCTION FIRSEC(ITOW)
C********************************************************************
C!Hcal endcap first expected wire plane                             *
C                                                                   *
C    Author : R.Tenchini                                            *
C                  08.8.91                                          *
C    Input:                                                         *
C        ITOW    (I)     hardware index of tower                    *
C                                                                   *
C    Output:                                                        *
C        FIRSEC  (I)     first wire plane                           *
C                                                                   *
C********************************************************************
      FIRSEC=1
      IF(ITOW.LE.4) FIRSEC=2
      IF(ITOW.GT.63) FIRSEC=5
      RETURN
      END
      SUBROUTINE HPLENE(EPLTOT,EPLTBA,EPLTEA,EPLTEB)
C********************************************************************
C!Hcal wire planes total energy                                     *
C                                                                   *
C    Author : R.Tenchini                                            *
C                  08.8.91                                          *
C    Input:                                                         *
C        needs HPLAND to be called before                           *
C                                                                   *
C    Output:                                                        *
C        EPLTOT Total HCAL energy from wire planes                  *
C        EPLTBA Total HCAL wire planes barrel energy                *
C        EPLTEA Total HCAL wire planes endcap A energy              *
C        EPLTEB Total HCAL wire planes endcap B energy              *
C                                                                   *
C********************************************************************
      COMMON/HDPLA/EPLANE(24,12),EPLAEA(6,12),EPLAEB(6,12)
      EPLTBA=0.
      EPLTEA=0.
      EPLTEB=0.
      DO 501 I=1,24
         DO 502 J=1,12
            EPLTBA=EPLTBA+EPLANE(I,J)
  502    CONTINUE
  501 CONTINUE
      DO 511 I=1,6
         DO 512 J=1,12
            EPLTEA=EPLTEA+EPLAEA(I,J)
  512    CONTINUE
  511 CONTINUE
      DO 521 I=1,6
         DO 522 J=1,12
            EPLTEB=EPLTEB+EPLAEB(I,J)
  522    CONTINUE
  521 CONTINUE
      EPLTOT=EPLTBA+EPLTEA+EPLTEB
      RETURN
      END
