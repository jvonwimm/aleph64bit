      SUBROUTINE QHFN(ID,A1 ,A2 ,A3 ,A4 ,A5 ,A6 ,A7 ,A8 ,A9 ,A10,
     1                   A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,
     2                   A21,A22,A23,A24,A25,A26,A27,A28,A29,A30,
     3                   A31,A32,A33,A34,A35,A36,A37,A38,A39,A40,
     4                   A41,A42,A43,A44,A45,A46,A47,A48,A49,A50,
     2                   A51,A52,A53,A54,A55,A56,A57,A58,A59,A60,
     3                   A61,A62,A63,A64,A65,A66,A67,A68,A69,A70,
     4                   A71,A72,A73,A74,A75,A76,A77,A78,A79,A80)
CKEY HIST /USER
C------------------------------------------------------------------
C!  fill N-tuple with variable number of arguments
C   Input:          ID           N-tuple  identifier
C                   A1,...,A80   elements of N-tuple
C
C   Calls           HGIVEN  from HBOOK
C
C   Author:     D.Schlatter    1.89
C   Modified:   F.Ranjard      3.93
C               use HGIVEN instead of NOARG on all machines
C   Modified:   J.Boucrot     11.94
C               extend to 80 arguments
C------------------------------------------------------------------
#ifndef DOC
      REAL PV(80),RLOW(80),RHIGH(80)
      CHARACTER CHTITL,TAGS(80)
      NA = 80
      CALL HGIVEN(ID,CHTITL,NA,TAGS,RLOW,RHIGH)
      IF(NA.GT.80) THEN
        CALL QWMESS('_QHFN_  more than 80 arguments. Call ignored ')
        GO TO 90
      ENDIF
C
      IF (NA.LE.0) GO TO 90
      GO TO
     & ( 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,
     &  21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
     &  41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,
     &  61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80), NA
   80 PV(80) = A80
   79 PV(79) = A79
   78 PV(78) = A78
   77 PV(77) = A77
   76 PV(76) = A76
   75 PV(75) = A75
   74 PV(74) = A74
   73 PV(73) = A73
   72 PV(72) = A72
   71 PV(71) = A71
   70 PV(70) = A70
   69 PV(69) = A69
   68 PV(68) = A68
   67 PV(67) = A67
   66 PV(66) = A66
   65 PV(65) = A65
   64 PV(64) = A64
   63 PV(63) = A63
   62 PV(62) = A62
   61 PV(61) = A61
   60 PV(60) = A60
   59 PV(59) = A59
   58 PV(58) = A58
   57 PV(57) = A57
   56 PV(56) = A56
   55 PV(55) = A55
   54 PV(54) = A54
   53 PV(53) = A53
   52 PV(52) = A52
   51 PV(51) = A51
   50 PV(50) = A50
   49 PV(49) = A49
   48 PV(48) = A48
   47 PV(47) = A47
   46 PV(46) = A46
   45 PV(45) = A45
   44 PV(44) = A44
   43 PV(43) = A43
   42 PV(42) = A42
   41 PV(41) = A41
   40 PV(40) = A40
   39 PV(39) = A39
   38 PV(38) = A38
   37 PV(37) = A37
   36 PV(36) = A36
   35 PV(35) = A35
   34 PV(34) = A34
   33 PV(33) = A33
   32 PV(32) = A32
   31 PV(31) = A31
   30 PV(30) = A30
   29 PV(29) = A29
   28 PV(28) = A28
   27 PV(27) = A27
   26 PV(26) = A26
   25 PV(25) = A25
   24 PV(24) = A24
   23 PV(23) = A23
   22 PV(22) = A22
   21 PV(21) = A21
   20 PV(20) = A20
   19 PV(19) = A19
   18 PV(18) = A18
   17 PV(17) = A17
   16 PV(16) = A16
   15 PV(15) = A15
   14 PV(14) = A14
   13 PV(13) = A13
   12 PV(12) = A12
   11 PV(11) = A11
   10 PV(10) = A10
    9 PV( 9) = A 9
    8 PV( 8) = A 8
    7 PV( 7) = A 7
    6 PV( 6) = A 6
    5 PV( 5) = A 5
    4 PV( 4) = A 4
    3 PV( 3) = A 3
    2 PV( 2) = A 2
    1 PV( 1) = A 1
C
      CALL HFIND(ID,'HGIVEN')
      CALL HFN (ID,PV)
   90 END
#endif
