*DK DGT
CH..............+++
CH
CH
CH
CH
CH
CH
CH & GT +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DGT
CH
      SUBROUTINE DGT
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *1 TLAST/' '/
      CHARACTER *2 TA,TCW
      DATA TCW/'cw'/
      CHARACTER *3  THLP,TBACK,TCR
      CHARACTER *20 TIST
      DATA TIST/' '/
      CHARACTER *49 T
      CHARACTER *1 T0789(-4:-1),TPRG
      DATA T0789/'0','7','8','9'/
C      CHARACTER BACKSLASH
C      PARAMETER (BACKSLASH=CHAR(92))
C      DATA TPRG/BACKSLASH/
      LOGICAL NYES,F0,FPRG,FHEAD,FSC
      DATA FPRG/.FALSE./

      CALL DQH_ON(' ')
      TPRG=CHAR(92) ! Backslash
      FHEAD=.FALSE.
      THLP='GT '
      IARE=IAREDO
      IF(TPICDO.NE.'GG'.AND.TPICDO.NE.'GT') THEN
        T='GT:W1: Non-existent processor. Type a correct one.'
        TPICDO='GT'
      END IF
    1 IF(TPICDO.EQ.'GG') GO TO 5
      THLPDO='GT'
      TNAMDO='GT'
      CALL DO_NEW_WINDOW(' ',0.,0.)
C        123456789 123456789 123456789
      T='GT:W1:     Select a processor.'
      TPICDO='GT'
      CALL DWRT('.................................................')
      CALL DWR_HL_AR(T)
      FHEAD=.TRUE.
C      CALL DGZOOM(6,IAREDO,0,0)
    2 THLP(3:3)=TPRGDH
      TPRGDH=' '
      CALL DQHL_MENU(THLP)

    5 CALL DWRT_END(2)
  935 CALL DO_BAR_DRAW(0,0,
     &  1,0,' ',0.,
     &  1,0,' ',0.)

  936 CALL DGZOOM(6,IAREDO,0,0)
      CALL DOP2LT(TLAST,NTYPE,NEXEC,TA,F)
C     ................... 28.5.97 intrinsic macros give "  " at the end ??
      CALL DGZOOM(6,-1,0,0)

      IF(NTYPE.EQ.8) THEN
        CALL DO_BAR_OP(IACT,TA)
        IF(     IACT.EQ.1) THEN
          GO TO 2                  ! window change or intrinsic macro
        ELSE IF(IACT.EQ.2) THEN
          THLP='GT0'
          CALL DQHL_MENU(THLP)
          GO TO 936                ! TANSW='GT' go to top level
        ELSE IF(IACT.EQ.4) THEN  
          NTYPE=2                  ! new 2 letter command with ntype=2
        ELSE IF(IACT.EQ.0.AND.TA.EQ.'GT') THEN
          THLP='GT0'
          CALL DQHL_MENU(THLP)
          GO TO 936
        ELSE
          GO TO 1                  ! <CR>
        END IF
      END IF

      CALL DO_RENAME_COMMAND(TA)

      IF(TA.EQ.'  ') GO TO 936
      IF(TA.EQ.'**') GO TO 936
      IF(TA.EQ.'T ') THEN
        TPICDO='GT'
        GO TO 1
      END IF
      IF(     TA(1:1).EQ.TPRG) THEN
        THLP='GT'//TA(2:2)
        CALL DQHL_MENU(THLP)
        TA=' '
        IF(.NOT.FHEAD) THEN
          T='GT:W1:     Select a processor.'
          TPICDO='GT'
          CALL DWRT('.................................................')
          CALL DWR_HL_AR(T)
          FHEAD=.TRUE.
        END IF
        GO TO 936
      END IF
      IF(FPRG) THEN
        FPRG=.FALSE.
      ELSE
        TPRGDH=' '
      END IF
      IF(TA.EQ.'X?'.OR.TA.EQ.'Y?'.OR.TA.EQ.'C?') 
     &  CALL DO_RESET_COMMAND_LIST(TA)
C                                               DO
      CALL DO_STR('DO: get selected window')
      IF(NEXEC.EQ.2) THEN
        NARE=IAREDO
        GO TO 3
      END IF
C                                               RETURN
      IF(NTYPE.EQ.5) GO TO 8
      IF(NTYPE.EQ.2) THEN
C                                               P2
c        CALL DO_STR('P2: go to page 2')
c        IF(TA.EQ.'P2') THEN
c          THLP='G2 '
c          TPICDO='GT'
c          GO TO 2
c        END IF
C                                               GT,GO
        CALL DO_STR('GT: no action')
        IF(TA.EQ.'GT') THEN
          THLP='GT '
          TPICDO='GT'
          GO TO 2
C                                               GG
        ELSE IF(TA.EQ.'GG: no action') THEN
          GO TO 2
        END IF
C                                               GB
        CALL DO_STR('GB: go back')
        IF(TA.EQ.'GB') GO TO 8

      IF(TA(1:1).EQ.'_') THEN
C       ........................................ user level
        READ(TA(2:2),1029,ERR=2) LEVUS
 1029   FORMAT(I1)
        IF(LEVUS.LE.7) THEN
          CALL DPARSV(81,'USL',2,FLOAT(LEVUS))
          GO TO 2
        END IF
      END IF

C       ....................................... TI
        CALL DO_STR('TI')
        IF(TA.EQ.'TI') THEN
          CALL DATE4(TIST(1:11))
          CALL TIME(TIST(13:20))
          CALL DWRT(TIST)
          GO TO 2
        END IF
C                                               W1 ...
        CALL DO_STR('W?: change windows')
        IF(TA(1:1).EQ.'W'.OR.TA(1:1).EQ.'w') THEN
          CALL DAREA('W',TA,0,12,IAREDO,NYES)
          IF(NYES) THEN
            CALL DQHL_W
C            CALL DGZOOM(6,IAREDO,0,0)
            IF(TA(1:1).EQ.'w') GO TO 936
            GO TO 2
          END IF
        END IF
        CALL DO_STR('WA: select window set 1')
        IF(TA.EQ.'WA') THEN
          WISUDW=1.
          CALL DQWSU
          IF(TPICDO.EQ.'GG') GO TO 8
          GO TO 2
        END IF
        CALL DO_STR('WB: select window set 2')
        IF(TA.EQ.'WB') THEN
          WISUDW=0.
          CALL DQWSU
          IF(TPICDO.EQ.'GG') GO TO 8
          GO TO 2
        END IF
        CALL DO_STR('G.: get processor of selected window.')
        IF(TA.EQ.'G.') THEN
C                                               G.
          NARE=IAREDO
          GO TO 3
        END IF
        CALL DO_STR('G?: get data of selected window')
        IF(TA(1:1).EQ.'G') THEN
C                                               G1 ...
          CALL DAREA('G',TA,0,12,NARE,NYES)
          IF(NYES) GO TO 3
          DO LARE=-4,-1
            IF(TA(2:2).EQ.T0789(LARE)) THEN
              CALL DPCGVI(LARE,F0,IZOMDO)
              IF(.NOT.F0) THEN
                FGETDO=.FALSE.
                RETURN
              END IF
            END IF
          END DO
        END IF
C                                               GW
        CALL DO_STR('GC: get data of current window')
        IF(TA.EQ.'GC') THEN
          CALL DGCURG(HW,VW)
          CALL DPOAR(HW,VW,NARE)
          IF(NARE.LT.0) GO TO 4
          GO TO 3
        END IF
        IF(TA.EQ.'QW: only used for wizzard else no action') THEN
          FWIZDW=.FALSE.
          CALL DQ_HEADER
          CALL DQHL_R
          GO TO 1
        END IF
        IF(TA.EQ.TCW) THEN
          FCONDW=.TRUE.
          GO TO 1
        END IF
        IF(TA.EQ.'ba') THEN
          CALL DQHL_LAST_PAGE(TBACK,TCR)
          IF(TBACK.NE.TCR) THEN
            IF(TBACK(1:2).EQ.TCR(1:2)) THEN
              THLP=TBACK
              TBACK=' '
              CALL DQHL_MENU(THLP)
              CALL DWRT_END(2)
              GO TO 936
            ELSE
              CALL DOPER_HELP_BACK(TBACK)
              TA=TBACK(1:2)
            END IF
          END IF
        END IF
        CALL DO_STR('??: search commnd or text')
        IF(TA.EQ.'??') THEN
          CALL DO_INDEX
          GO TO 1
        END IF
        IF(TA.EQ.'X?'.OR.TA.EQ.'Y?'.OR.TA.EQ.'C?') THEN 
          DO K=1,MAXPDP
            IF(TPICDP(K).NE.'  ') CALL DO_STR(TPICDP(K))
          END DO
          CALL DO_TY_COMMAND_LIST('DGT')
          GO TO 936
        END IF
        TPICDO=TA
        GO TO 9
      ELSE
        IF(TA.EQ.'X?'.OR.TA.EQ.'Y?'.OR.TA.EQ.'C?') THEN 
          DO K=1,MAXPDP
            IF(TPICDP(K).NE.'  ') CALL DO_STR(TPICDP(K))
          END DO
          CALL DO_TY_COMMAND_LIST('DGT')
        ELSE
          T='GT:W1: Invalid command. Select a processor.'
        END IF
      END IF
      IF(TPICDO.EQ.'GG') THEN
        CALL DWRT('.................................................')
        CALL DWR_HL_AR(T)
        TPICDO='GT'
      ELSE
        T(1:6)=' '
        CALL DWRT(T)
      END IF
      CALL DQHLP(THLP)
      GO TO 2
    3 CALL DPCGAR(NARE,F0,IZOMDO)
      IF(.NOT.F0) THEN
        FGETDO=.FALSE.
        GO TO 9
      END IF
    4 CALL DWRT('No picture defined in this window?')
      GO TO 2
    8 IAREDO=IARE
      TPICDO='GB'
      FGETDO=.FALSE.
C    9 CALL DGZOOM(6,-1,0,0)
    9 END
