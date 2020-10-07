*DK DGETMR
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DGETMR
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C   This routine lists active macros, reads in a new set of macros or
C   saves current macros.
C   Björn S. Nilsson, June 1989 and Feb 1990.
C   This routine almost behaves
C   like a processor. For a processor proper, input must be taken via
C   DOPER. However, DGETMR will list the definition of (not execute) a
C   function key, and therefore this non-standard solution has been
C   chosen. The main limitation is, that on exit from DGETMR only GB, GG
C   GT and QU have meaning.
C
C   X version for HP !
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DM
      LOGICAL FMACDM,FMOPDM,FMINDM,FANIDM,FNTADM,FINMDM
      COMMON /DMACRC/ FMACDM,NLETDM,FMOPDM,FMINDM,FANIDM,FNTADM,
     &  FINMDM
C------------------------------------------------------------------  DC
      COMMON /DCFTVT/ TFILDC,TITLDC,TDEFDC
      CHARACTER*8 TFILDC
      CHARACTER*10 TITLDC
      CHARACTER*3 TDEFDC
C------------------------------------------------------------------- DU
      PARAMETER (MPNPDU=60)
      COMMON /DUSDAC/ NUNIDU, PARADU(MPNPDU)
C------------------------------------------------------------------- DO
      COMMON /DOPR1T/ TPLNDO(-2:11)
      COMMON /DOPR2T/ TAREDO( 0:14)
      COMMON /DOPR3T/ TZOODO( 0: 1)
      COMMON /DOPR4T/ TPICDO
      CHARACTER*2 TPLNDO,TAREDO,TZOODO,TPICDO
C
C-------------------End of DALI_CF commons----------------
      INTEGER L2
      INTEGER ISTAT,LATEXT,I,ILEN,K,KFIRST,KLAST,NUMMAC,L1
      CHARACTER ATEXT*2,T*49
      CHARACTER*80 TSCR,TSCR1
      LOGICAL LENTRY, LFIRST
      CHARACTER TFNAM*80/' '/
      INTEGER MA_CNT/0/

      INTEGER MACMX, MACLEVMAX
      PARAMETER (MACMX=500, MACLEVMAX=8)
      CHARACTER TMNAME*20,TMAC*80,MACFIL*80,TMSAVE*20
      INTEGER LSCRN,KMAX,MACLEN,MACLEV,MACSAVE
      LOGICAL FMSAVE
      COMMON /MACCMN/ LSCRN,KMAX,MACLEN(MACMX),MACFIL,TMNAME(MACMX),
     & TMAC(MACMX),MACLEV,MACSAVE(3,MACLEVMAX),FMSAVE(2,MACLEVMAX),
     & TMSAVE(MACLEVMAX)

      LOGICAL FLGOOB,FLGDOP
      COMMON /OOBCMN/ FLGOOB(0:31),FLGDOP

      LFIRST=.TRUE.
      IF(FMACDM) THEN
        MA_CNT = MA_CNT+1
        CALL DWRT('DGETMR was called while macro was active. Ignored.')
        IF (MA_CNT.GE.100) THEN
          CALL DWRT_SETUP('TERMINAL=ON')
          CALL DWRT_SETUP('DEFAULT')
          CALL DWRT('Loop over DGETMR. Probably DALI internal error.')
          CALL DWRT('Please report this to Hans Drevermann.')
          CALL EXIT
        END IF
        GO TO 999
      END IF
      CALL DQHLP('MA')
      LENTRY=.TRUE.
      ATEXT='LB'
  100 CALL DWRT('.................................................')
      IF(LENTRY) THEN
         T='MA: Present macro definitions.'
         CALL DWR_HIGH_LIGHT_FROM_N1_TO_N2(T,1,2)
         LENTRY=.FALSE.
         GO TO 200
      ELSE
         T='MA: LB LF CM RM WM for macro handling or GT'
         CALL DWR_HIGH_LIGHT_FROM_N1_TO_N2(T,1,2)
      END IF

  110 CALL DWRT_END(2)
      ATEXT(1:1)=CHAR(0)
      CALL DGETST(ATEXT,LATEXT,2,-1)

      IF(LATEXT.EQ.-26) ATEXT='QU'
      IF(LATEXT.LT.-255) GO TO 100

      IF(LATEXT.EQ.-13) THEN
         LATEXT=2
         ATEXT='LB'
      END IF
  200 CONTINUE
      IF(ATEXT.EQ.'LB'.OR.LFIRST) THEN
         LFIRST=.FALSE.
C
C   List all macros except for the start macro presently defined.
C   LB = List Brief; First line only.
C
         ILINE=0
         FLGOOB(3)=.FALSE.
         CALL DWRT_SETUP('END CHARACTER= ')
         DO K=1,KMAX
            L1=LENOCC(TMNAME(K))
            ILEN=LENOCC(TMAC(K))
            I=MIN(79-L1,ILEN)
            IF(TMNAME(K).NE.' '.AND.TMNAME(K).NE.'ST '.AND.I.GT.0) THEN
               CALL DWRT(TMNAME(K)(1:L1)//'='//TMAC(K)(1:ILEN))
               ILINE=ILINE+1
               IF(ILINE.GE.LSCRN-1.AND.K.NE.KMAX) THEN
                  IF(FLGOOB(3)) THEN
                     FLGOOB(3)=.FALSE.
                     CALL DWRT_SETUP('DEFAULT')
                     CALL DWRT('Output terminated...')
                     GO TO 100
                  END IF
                  CALL DTYANS('Hit Return to continue.. ', 'N', NANSW)
                  IF (NANSW .NE. 0) GOTO 100
                  ILINE=0
               END IF
            END IF
         END DO
         CALL DWRT_SETUP('DEFAULT')
         GO TO 100
      ELSE IF(ATEXT.EQ.'LF') THEN
C
C   List all macros presently defined.
C   LF = List Full;  All lines.
C
         KLAST=1
         FLGOOB(3)=.FALSE.
  300    ILINE=0
         KFIRST=KLAST
         CALL DWRT_SETUP('END CHARACTER= ')
         DO 310 K=KFIRST,KMAX
            L1=LENOCC(TMNAME(K))
            ILEN=LENOCC(TMAC(K))
            I=MIN(79-L1,ILEN)
            IF(I.LE.0) GO TO 310
            IF(TMNAME(K).NE.' '.AND.I.GT.0) THEN
               CALL DWRT(TMNAME(K)(1:L1)//'='//TMAC(K)(1:ILEN))
               KLAST=K
            ELSE
               CALL DWRT('  '//TMAC(K)(1:I))
            END IF
            ILINE=ILINE+1
            IF(ILINE.GE.LSCRN-1.AND.K.NE.KMAX) THEN
               IF(FLGOOB(3)) THEN
                  FLGOOB(3)=.FALSE.
                  CALL DWRT_SETUP('DEFAULT')
                  CALL DWRT('Listing terminated.')
                  GO TO 100
               END IF
               CALL DTYANS('Hit Return to continue.. ', 'N', NANSW)
               IF (NANSW .NE. 0) GOTO 100
               ILINE=0
C
C   Ensure that we do not break a macro in too many displays.
C
               IF(TMNAME(K+1).NE.' ') THEN
                  KLAST=K+1
               ELSE IF(K-KLAST.GT.LSCRN-3) THEN
                  KLAST=K
                  IF(KMAX-K.LE.LSCRN-2) KLAST=K+1
               END IF
               GO TO 300
            END IF
  310    CONTINUE
         CALL DWRT_SETUP('DEFAULT')
         GO TO 100
      ELSE IF(ATEXT.EQ.'CM') THEN
C
C   Clear existing macros.
C
         CALL DTYANS('Do you really want to clear all macros? [Y]',
     &    'Nn', NANSW)
         IF (NANSW .GT. 0) THEN
            CALL DWRT('No action taken, macros preserved.')
            GO TO 100
         END IF
  320    KMAX=0
         CALL DWRT('All macros were cleared.')
         GO TO 100
      ELSE IF(ATEXT.EQ.'RM') THEN
C
C   Read a new set of macros from a file.
C
  400    TSCR1=TFNAM
         IF(TFNAM.EQ.' ') TSCR1 = TFILDC(1:LENOCC(TFILDC))//'MAC'
         CALL DWRT('Enter filename for macro input ['//
     &     TSCR1(1:LENOCC(TSCR1))//']: ')
         Call DGTLNX_LC
         CALL DGETLN(TSCR,L1,80)
         Call DGTLNX_LC
         IF(L1.EQ.-13.OR.TSCR.EQ.' ') THEN
            MACFIL=TSCR1
            TSCR=TSCR1
         ELSE IF(L1.LT.0) THEN
            GO TO 100
         ELSE
            MACFIL=TSCR
         END IF
         CALL DGOPEN(NUNIDU,MACFIL,-2,*410,ISTAT)
         GO TO 450
  410    MACFIL=TSCR(1:LENOCC(TSCR))//'.MAC'
         CALL DGOPEN(NUNIDU,MACFIL,-2,*440,ISTAT)
         GOTO 450
  440    L1=LENOCC(MACFIL)
         CALL DWRT('File '//MACFIL(1:L1)//' cannot be opened!')
         GO TO 100
  450    CLOSE(UNIT=NUNIDU)
         TFNAM=MACFIL
         CALL DGETM0('   ',-2)
         GO TO 100
C
C   Write the present macros on a file.
C
      ELSE IF(ATEXT.EQ.'WM') THEN
         TSCR1=TFILDC(1:LENOCC(TFILDC))//'MAC'
         CALL DWRT('Enter filename for macro output ['//
     &     TSCR1(1:LENOCC(TSCR1))//']: ')
         Call DGTLNX_LC
         CALL DGETLN(TSCR,L1,80)
         Call DGTLNX_LC
         IF(L1.EQ.-13.OR.TSCR.EQ.' ') THEN
            TSCR=TSCR1
         ELSE IF(L1.LT.0) THEN
            GO TO 100
         END IF
         CLOSE(UNIT=NUNIDU)
         L1=LENOCC(TSCR)
         CALL DGOPEN(NUNIDU,TSCR(1:L1),1,*460,ISTAT)
         GOTO 470
  460    CALL DWRT('File '//TSCR(1:L1)//' cannot be opened !')
         GO TO 100
  470    NUMMAC=0
         DO K=1,KMAX
            L1=MAX(LENOCC(TMNAME(K)),3)
            I=LENOCC(TMAC(K))
            IF(TMNAME(K).NE.' ') THEN
               WRITE(NUNIDU,'(A,A,A)') TMNAME(K)(1:L1),'=',TMAC(K)(1:I)
               NUMMAC=NUMMAC+1
            ELSE
               WRITE(NUNIDU,'(3X,A)') TMAC(K)(1:I)
            END IF
         END DO
         CLOSE(UNIT=NUNIDU,STATUS='KEEP')
         I1 = 1
         IF(NUMMAC.GT.9) I1 = 2
         IF(NUMMAC.GT.99) I1 = 3
         WRITE(TSCR1,'(I<I1>,A)') NUMMAC, ' macros with '
         L2 = LENOCC(TSCR1)
         I1 = 1
         IF(KMAX.GT.9) I1 = 2
         IF(KMAX.GT.99) I1 = 3
         WRITE(TSCR1(L2+2:),'(I<I1>,A)')  KMAX,
     &    ' lines were written to '
         L2 = LENOCC(TSCR1)
         WRITE(TSCR1(L2+2:),'(A)') TSCR(1:LENOCC(TSCR))
         L2 = LENOCC(TSCR1)
         CALL DWRT(TSCR1(1:L2))
         GO TO 100
      ELSE IF(ATEXT(1:1).EQ.';') THEN
         CALL DGPOP('FLIP')
         GO TO 100
      ELSE IF(ATEXT(1:1).EQ.'^') THEN
         CALL DGPOP('POP')
         GO TO 100
      ELSE IF(ATEXT(1:1).EQ.'<') THEN
         CALL DWR_ADD_SEMICOLON_DELET
         CALL DWR_ADD_SEMICOLON_DELET
         CALL DQHLP('<<')
         GO TO 110
      ELSE IF(ATEXT(1:1).EQ.'@') THEN
C        CALL DWRT_END(24)
         CALL DGETST(TSCR(2:),K,24,0)
         TSCR(1:1)=ATEXT(2:2)
         CALL DGETM0(TSCR(1:K+1),-1)
         GO TO 100

      ELSE IF(ATEXT.EQ.'QU'.OR.
     &        ATEXT.EQ.'GG'.OR.
     &        ATEXT.EQ.'GT'.OR.
     &        ATEXT.EQ.'GB') THEN
        TPICDO=ATEXT

      ELSE
         CALL DWR_IC(ATEXT)
         GO TO 100
      END IF
  999 RETURN
      END

*DK DGETMD
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DGETMD
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C   This routine reformats the TMACDM buffer to fit in the present line
C   layout. It then (re)nominates a macro with the name entered. This
C   can be a key or a name of 20 characters or less.
C   Björn S. Nilsson, June 1989 and Feb 1990.
C
C   X version !
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DU
      COMMON /DUSD2C/NUN6DU,NUN9DU
      PARAMETER (MUN5DU=5,MUN6DU=6,MUN9DU=9)
C
C-------------------End of DALI_CF commons----------------
      CHARACTER ANAME*24,TNAME*24,TCOM*80,TBLANK*24/'   '/,A1*1
      INTEGER K,LENCOM,LENMAC,LBLANK,L1,L2

      INTEGER IPNTDM
      CHARACTER TMACDM*2000
      COMMON/DMMACR/ IPNTDM,TMACDM

      INTEGER MACMX, MACLEVMAX
      PARAMETER (MACMX=500, MACLEVMAX=8)
      CHARACTER TMNAME*20,TMAC*80,MACFIL*80,TMSAVE*20
      INTEGER LSCRN,KMAX,MACLEN,MACLEV,MACSAVE
      LOGICAL FMSAVE
      COMMON /MACCMN/ LSCRN,KMAX,MACLEN(MACMX),MACFIL,TMNAME(MACMX),
     & TMAC(MACMX),MACLEV,MACSAVE(3,MACLEVMAX),FMSAVE(2,MACLEVMAX),
     & TMSAVE(MACLEVMAX)

      LOGICAL FLGOOB,FLGDOP
      COMMON /OOBCMN/ FLGOOB(0:31),FLGDOP

      IF(IPNTDM.LE.1.OR..NOT.FLGOOB(12)) GO TO 999

      CALL DWRT('Input recording finished.')
      FLGOOB(12)=.FALSE.
  100 CALL DWRT('Press a key or enter a name to nominate this macro.')
      CALL DGETST(TNAME,L1,24,0)
      IF((L1.GT.0.AND.L1.LE.20).OR.L1.LE.-255) GO TO 102
      IF(L1.GT.20) THEN
         CALL DWRT(
     &    ' That name ('//TNAME(1:L1)//') is too long (> 20).')
         GO TO 100
      ELSE IF(L1.GT.-255.OR.TNAME.EQ.'  ') THEN
         L2=LENOCC(TNAME)
         CALL DWRT(
     &   'That key or name ('//TNAME(1:L2)//') cannot be used.')
         GO TO 100
      END IF
  102 IF(TNAME(1:1).EQ.'@') TNAME=TNAME(2:)
      L2=LENOCC(TNAME)
      DO K=1,L2
C
C  Check that characters are alphabetic, digits or _.
C
         A1=TNAME(K:K)
         IF(.NOT.((A1.EQ.'$').OR.(A1.GE.'0'.AND.A1.LE.'9').OR.
     &    (A1.GE.'A'.AND.A1.LE.'Z').OR.(A1.EQ.'_').OR.(A1.GE.'À'))) THEN 
            CALL DWRT('A macro name must not contain a '//A1//
     &       ' character.')
            GO TO 100
         END IF
      END DO
      LENMAC=LENOCC(TNAME)
      ANAME=TNAME
C      WRITE(*,'($,A)') ' Enter a Macro comment if any: '
C      READ(*,'(A)',END=105,ERR=105) TCOM
C  105 LENCOM=LENOCC(TCOM)
C      IF(LENCOM.GT.0) WRITE(MUN9DU,'(A,A)') ' Macro comment: ',
C     & TCOM(1:LENCOM)
C
C  Blank out a possible previous definition.
C
      DO K=1,KMAX
         IF(TMNAME(K).EQ.TNAME) THEN
            CALL DWRT('Macro '//TNAME(1:LENMAC)//' was replaced.')
            K1=K
            GO TO 110
         END IF
      END DO
      CALL DWRT('Macro '//TNAME(1:LENMAC)//' was inserted.')
      GO TO 130
  110 DO K=K1+1,KMAX
         IF(TMNAME(K).NE.' ') THEN
            K2=K
            GO TO 120
         END IF
      END DO
      KMAX=K1-1
      GO TO 130
  120 DO K=K2,KMAX
         TMNAME(K1+K-K2)=TMNAME(K)
         TMAC(K1+K-K2)=TMAC(K)
         MACLEN(K1+K-K2)=MACLEN(K)
      END DO
      KMAX=K1+KMAX-K2
C
C  Finally store the new macro at the end of the list.
C
  130 K3=0
      LENCOM=LENOCC(TCOM)
      IF(LENCOM.GT.0) THEN
         KMAX=KMAX+1
         TMNAME(KMAX)=TNAME
         LBLANK=MAX(1,15-LENMAC)
         TMAC(KMAX)='-'//TBLANK(1:LBLANK)//'! '//TCOM(1:LENCOM)
         MACLEN(KMAX)=1
         TNAME='   '
         LENMAC=3
      END IF
  140 K1=K3+1
      K2=MIN(IPNTDM-1,K1+79-LENMAC-3)
      KMAX=KMAX+1
      IF(K2.EQ.IPNTDM-1) GO TO 160
      DO K=K2,K1,-1
         IF(TMACDM(K:K).EQ.':') THEN
            K3=K
            GO TO 150
         END IF
      END DO
      GO TO 160
  150 TMNAME(KMAX)=TNAME
      TMAC(KMAX)=TMACDM(K1:K3)//'-'
      MACLEN(KMAX)=K3-K1+2
      TNAME='   '
      LENMAC=3
      GO TO 140
  160 TMNAME(KMAX)=TNAME
      TMAC(KMAX)=TMACDM(K1:K2)
      MACLEN(KMAX)=K2-K1+1
      CALL DGETM0(ANAME,-1)
      IPNTDM=0

  999 FLGOOB(12)=.FALSE.
      FLGOOB(18)=.FALSE.
      END

      INTEGER FUNCTION DGOOBA(NADDR)
C  This function handles Out-of-Band keyboard ASTs.
C  Björn S. Nilsson, May-June 1989 and March 1990.
C
C  X version !
C
      IMPLICIT NONE

      CHARACTER TSCR*128
      INTEGER LENSTR
      INTEGER I,NADDR,NKEY,NCHAR
      LOGICAL FMOP

      LOGICAL FLGOOB,FLGDOP
      COMMON /OOBCMN/ FLGOOB(0:31),FLGDOP

      INTEGER NLETDM
      LOGICAL FMACDM,FMOPDM,FMINDM,FANIDM,FNTADM,FINMDM
      COMMON /DMACRC/ FMACDM,NLETDM,FMOPDM,FMINDM,FANIDM,FNTADM,
     &  FINMDM

      INTEGER BUTSTA,KEYBF1
      LOGICAL FBOX
      REAL LOWX,LOWY,PXX,PYY,HPOPOS,VPOPOS
      COMMON /ASTCM1/ BUTSTA,KEYBF1(4),FBOX,LOWX,LOWY,PXX(5),PYY(5),
     & HPOPOS,VPOPOS

      INTEGER IPNTDM
      CHARACTER TMACDM*2000
      COMMON/DMMACR/ IPNTDM,TMACDM

      INTEGER   IPTBUF,LENBUF
      CHARACTER INPBUF*12
      COMMON /CMIOBF/ IPTBUF,LENBUF,INPBUF

      INTEGER MACMX, MACLEVMAX
      PARAMETER (MACMX=500, MACLEVMAX=8)
      CHARACTER TMNAME*20,TMAC*80,MACFIL*80,TMSAVE*20
      INTEGER LSCRN,KMAX,MACLEN,MACLEV,MACSAVE
      LOGICAL FMSAVE
      COMMON /MACCMN/ LSCRN,KMAX,MACLEN(MACMX),MACFIL,TMNAME(MACMX),
     & TMAC(MACMX),MACLEV,MACSAVE(3,MACLEVMAX),FMSAVE(2,MACLEVMAX),
     & TMSAVE(MACLEVMAX)

      INTEGER IW
      COMMON/BCS/IW(6)    ! BOS

      NKEY=%LOC(NADDR)

      FMOP = FMOPDM
      IF(FMOP) THEN
         CALL DWRT_SETUP('TERMINAL=ON')
         FMOPDM=.FALSE.
      END IF
      IF(NKEY.EQ.3.OR.NKEY.EQ.8) THEN
C   CTRL-C. Action when running macros or when listing BOS banks.
C   CTRL-H. Action when running macros.
C   IW(6) is the output unit of BOS. I hope I do not muck up anything here.
         IF(NKEY.EQ.3) IW(6)=90
         IF(BUTSTA.EQ.1.OR.BUTSTA.EQ.2) CALL DGSTEV(4)
         FLGOOB(NKEY)=.TRUE.
      ELSE IF(NKEY.EQ.5) THEN
C   CTRL-E will clear some conditions, e.g. setting BUTSTA = 0 and clearing
C   the internal input buffer.
         BUTSTA=0
         IPTBUF=0
         LENBUF=0
         CALL DWRT('Input buffer cleared and BUTSTA reset to zero.')
      ELSE IF(NKEY.EQ.9.OR.NKEY.EQ.31) THEN
C   CTRL-I or CTRL-? Give some info.
         CALL DWR_LINE(10, '-----')
         CALL DWRT_SETUP('END CHARACTER= ')
         IF(FLGOOB(3)) CALL DWRT('CTRL-C is pending.')
         IF(FLGOOB(8)) CALL DWRT('CTRL-H is pending.')
         IF(FLGOOB(12).AND..NOT.FLGOOB(18)) THEN
            I = 1
            IF(IPNTDM.GE.  10) I=2
            IF(IPNTDM.GE. 100) I=3
            IF(IPNTDM.GE.1000) I=4
            WRITE(TSCR,'(A,I<I>,A)') 'Macro learning is ON, ',IPNTDM-1,
     &       ' characters buffered'
            CALL DWRT(TSCR)
         END IF
         IF(.NOT.FLGOOB(12).AND..NOT.FMACDM)
     &     CALL DWRT('Macro learning is OFF.')
         IF(FMACDM) THEN
            DO I=1,MACLEV
               CALL DGTRIM(TMSAVE(I),TMSAVE(I),LENSTR)
               WRITE(TSCR,'(A,A,A,I2)') 'Macro ',
     &          TMSAVE(I)(1:LENSTR),' is executing at Level',I
               CALL DWRT(TSCR)
            END DO
         ELSE
            CALL DWRT('Normal (non-macro) input mode.')
         END IF
         TSCR = ' '
         CALL DGQUME(NCHAR,TSCR)
         IF(NCHAR.GT.0) THEN
            LENSTR = Index(TSCR,Char(0)) - 1
            CALL DWRT('Metafile')
            CALL DWRT(TSCR(1:LENSTR))
            CALL DWRT('open.')
            I=3
            IF(NCHAR.GT.     99) I=4
            IF(NCHAR.GT.    999) I=5
            IF(NCHAR.GT.   9999) I=6
            IF(NCHAR.GT.  99999) I=7
            IF(NCHAR.GT. 999999) I=8
            IF(NCHAR.GT.9999999) I=15
            WRITE(TSCR,'(I<I>,A)') NCHAR, ' characters written so far '
            CALL DWR_ADD(TSCR)
         EndIf
         CALL DWRT('Use < to toggle Help menu.')
         FLGOOB(NKEY)=.FALSE.
         CALL DWRT_SETUP('END CHARACTER=|')
         CALL DWR_LINE(10, '-----')
      ELSE IF(NKEY.EQ.12) THEN
C
C   CTRL-L  Start recording keystrokes for macro save, or, if CTRL-R
C   was "active", cancel it. Thus, if CTRL-R was issued but not acted
C   upon, another CTRL-L removes the CTRL-R indication. If, however,
C   macro saving was active and CTRL-L was issued, it serves as a toggle
C   and switches saving off.
C
         IF(FLGOOB(12).AND.FLGOOB(18)) THEN
            TSCR=' '
            IF(LENBUF.GT.0) TSCR=INPBUF(:LENBUF)
            CALL DGTRIM(TSCR,TSCR,LENSTR)
            CALL DWRT(
     &       'Macro recording resumed. Press CTRL-R to finish.')
            CALL DWR_ADD(':')
            IF(.NOT.(LENSTR.EQ.1.AND.TSCR(1:1).EQ.' '))
     &       CALL DWR_ADD(TSCR(1:LENSTR))
            FLGOOB(18)=.FALSE.
            CALL DGCLEV(5)
         ELSE IF(.NOT.FLGOOB(12)) THEN
            TSCR=' '
            IF(LENBUF.GT.0) TSCR=INPBUF(:LENBUF)
            IPNTDM=1
            CALL DGTRIM(TSCR,TSCR,LENSTR)
            CALL DWRT('Start Macro learning. Press CTRL-R to finish.')
            CALL DWR_ADD(':')
            IF(.NOT.(LENSTR.EQ.1.AND.TSCR(1:1).EQ.' '))
     &       CALL DWR_ADD(TSCR(1:LENSTR))
            FLGOOB(NKEY)=.TRUE.
         ELSE IF(FLGOOB(12)) THEN
            CALL DWRT('Macro learning is cancelled.')
            FLGOOB(12)=.FALSE.
         END IF
      ELSE IF(NKEY.EQ.18) THEN
C   CTRL-R End keystroke recording.
         IF(FLGOOB(12)) THEN
            TSCR=' '
            IF(LENBUF.GT.0) TSCR=INPBUF(:LENBUF)
            CALL DGTRIM(TSCR,TSCR,LENSTR)
            IF(TSCR.NE.' ') THEN
               CALL DWRT(
     &          'Macro learning ends. The last, incomplete command')
               CALL DWRT(
     &          'is not included in the macro definition.')
               CALL DWR_ADD(':')
               CALL DWR_ADD(TSCR(1:LENSTR))
            END IF
            FLGOOB(NKEY)=.TRUE.
            IF(FLGDOP) CALL DGSTEV(5)
         ELSE
            CALL DWRT('Macro learning is NOT active.')
         END IF
      END IF
      IF(FMOP) THEN
         CALL DWRT_SETUP('TERMINAL=OFF')
         FMOPDM=.TRUE.
      END IF
      DGOOBA=1
      END

*DK DGETST
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DGETST(ICHSTR, LCHSTR, NCHSET, ICNTRL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C   This routine reads at most NCHSET characters  from the keyboard if
C   not terminated before.
C   It returns the LCHSTR character(s) read in ICHSTR.
C   ICNTRL controls the control key action further:
C         +2  ICHSTR must contain a macro name. If found, the macro
C             will start executing, returning output as normal.
C         +1  If a macro is defined for this key, execute the macro.
C             Otherwise return character string.
C          0  Return information on what keys were pressed, no macro
C             execution. Input may be taken from a macro, however.
C         -1  List macro definition if found, no execution.
C   Special cases:
C     For ICNTRL>0 DELETE is a terminator. For all strings terminated
C     with a function key, ICHSTR will contain the keyname name as de-
C     fined in $SMGDEF, but without the SMG$K_TRM_ prefix. LCHSTR will
C     then be the negative of the number for that key.
C
C   Present drawbacks:
C     The routine by necessity contains many VAX-Fortran features.
C
C   This version is by B.S. Nilsson from June 1989 and Feb 1990.
C
C   X version !
C
      CHARACTER*(*) ICHSTR
      INTEGER       LCHSTR,NCHSET
      CHARACTER JCHSTR*80, AMAC*20
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DM
      LOGICAL FMACDM,FMOPDM,FMINDM,FANIDM,FNTADM,FINMDM
      COMMON /DMACRC/ FMACDM,NLETDM,FMOPDM,FMINDM,FANIDM,FNTADM,
     &  FINMDM
C------------------------------------------------------------------- DU
      COMMON /DUSD2C/NUN6DU,NUN9DU
      PARAMETER (MUN5DU=5,MUN6DU=6,MUN9DU=9)
C
C-------------------End of DALI_CF commons----------------
      CHARACTER COMMND*8,TON*1,TSCR*1
      COMMON /CMFLAG/ COMMND,TON
      INTEGER   IPTBUF,LENBUF
      CHARACTER INPBUF*12
      COMMON /CMIOBF/ IPTBUF,LENBUF,INPBUF

      INTEGER IPNTDM
      CHARACTER TMACDM*2000
      COMMON/DMMACR/ IPNTDM,TMACDM

      INTEGER*2 IOSB(4), INT2
      INTEGER L1
C   FLTHLP is set by one of the mouse routines to toggle HELP window on/off
C   is -> was. This is not needed any more it seems.
      LOGICAL FLTHLP/.FALSE./

      LOGICAL FLGOOB,FLGDOP
      COMMON /OOBCMN/ FLGOOB(0:31),FLGDOP

      INTEGER BUTSTA,KEYBF1
      LOGICAL FBOX
      REAL LOWX,LOWY,PXX,PYY,HPOPOS,VPOPOS
      COMMON /ASTCM1/ BUTSTA,KEYBF1(4),FBOX,LOWX,LOWY,PXX(5),PYY(5),
     & HPOPOS,VPOPOS

      LOGICAL FCLIC/.FALSE./,FCANCL/.FALSE./,FHLPLC/.FALSE./

      IF(ICNTRL.EQ.0) ICHSTR=' '
      IF(BUTSTA.GT.0.OR.ICNTRL.LT.0.OR.ICNTRL.EQ.2) GO TO 3
      IF(ICNTRL.EQ.0) GO TO 2
C
C  First check whether HELP is switched on or off.
C
      IF(FLTHLP) THEN
         CALL DGCLEV(3)
         CALL DQHLP('<<')
         FLTHLP=.FALSE.
      END IF
C
C  Then check if there already is something in the terminal IO buffer.
C  If something, return it.
C
      IF(IPTBUF.LT.LENBUF) THEN
         ILEN=MIN(LENBUF-IPTBUF,NCHSET)
         ICHSTR=INPBUF(IPTBUF+1:IPTBUF+ILEN)
         LCHSTR=ILEN
         IPTBUF=IPTBUF+ILEN
         GO TO 999
      END IF

      IF(FCLIC.OR.FCANCL) THEN
    1    ICOM = ICOM + 1
         IF(FCANCL.AND.(COMMND(1:1).LT.' ')) THEN
C   A control sequence was stored.
            LCHSTR=-(ICHAR(COMMND(1:1))*256+ICHAR(COMMND(2:2)))
            FCANCL=.FALSE.
            IF(LCHSTR.LT.-255) GO TO 10
            GO TO 999
         END IF
         IF(ICOM.GT.8) GO TO 2
         IF((COMMND(ICOM:ICOM) .EQ. ' '.AND.(FCLIC.OR.ICOM.GT.1)) .OR.
     &      COMMND(ICOM:ICOM) .EQ. ',' .OR.
     &      COMMND(ICOM:ICOM) .EQ. '|') GO TO 2
         IF(COMMND(ICOM:ICOM) .EQ. '=' .OR.
     &      COMMND(ICOM:ICOM) .EQ. ':' .OR.
     &      COMMND(ICOM:ICOM) .EQ. '/' .OR.
     &      COMMND(ICOM:ICOM) .EQ. 'y' .OR.
     &      COMMND(ICOM:ICOM) .EQ. 'x') GO TO 1
         ICHSTR(1:1) = COMMND(ICOM:ICOM)
         IF(ICHSTR(1:1) .EQ. '!'.AND..NOT.FCANCL) THEN
            LCHSTR = 0
            FCLIC = .FALSE.
            FCANCL= .FALSE.
         ELSE
            IF(FCLIC.AND.(.NOT.FHLPLC)) CALL DWR_ADD(ICHSTR(1:1))
            FHLPLC=.FALSE.
            LCHSTR = 1
         END IF
         NLETDM = NLETDM + 1
         GO TO 9
      END IF
    2 FCLIC = .FALSE.
      FCANCL= .FALSE.
      LCHSTR=0
      IF(FMACDM) THEN
   21    TSCR=' '
         CALL DGETM(TSCR,LCH,ICNTRL)
CMIP Place to process events - modify keyinput ......
         IF(LCH.LT.0) THEN
            IF(.NOT.(ICNTRL.EQ.0.AND.TSCR(1:1).EQ.':')) THEN
               ICHSTR=TSCR
               LCHSTR=LCH
               GO TO 999
            ENDIF
         ELSE
            LCHSTR=LCHSTR+1
            ICHSTR(LCHSTR:)=TSCR
            IF(LCHSTR.LT.NCHSET) GO TO 21
         END IF
         IF(FMACDM.OR.ICNTRL.EQ.0) THEN
           NLETDM = NLETDM + LCHSTR
           GO TO 9
         END IF
      END IF
    3 NLETDM = 0
      IF(ICNTRL.EQ.2) THEN
         AMAC=ICHSTR
         ICHSTR=' '
         LCHSTR=-1
         FNTADM=.TRUE.
         GO TO 10
      END IF
C
C   Setup terminal I/O and wait for an event flag to be set:
C   EVFLAG(1) terminal input.
C   EVFLAG(2) input from HELP window.
C   EVFLAG(3) HELP window toggling.
C   EVFLAG(4) set when rubberband, Pick or Point request terminates normally
C  or is cancelled.
C   EVFLAG(5) set if a CTRL-R was issued when waiting for first character in
C  DOP2LT.
C
C   The following event flags are used in other routines.
C   EVFLAG(6) is used with simultaneous cursors.
C   EVFLAG(7) is used for some waiting conditions that are cancelled by any
C   keyboard input.
C
    5 CALL DGHMRK
      TON = 'Y'

      CALL DGWTLP(NCHSET)
      JCHSTR=' '
      FHLPLC=.FALSE.
      CALL DGTFLG(ISTAT,IOSB,JCHSTR)
      IF(ISTAT .EQ. 5) THEN
C
C   A CTRL-R was hit while DOP2LT was waiting for the first character.
C
         CALL DGCLEV(5)
         LCHSTR=-1001
         RETURN
      END IF

      IF(ISTAT .EQ. 3) THEN
C
C   HELP window toggling.
C
         CALL DGCLEV(3)
         CALL DQHLP('<<')
         FLTHLP=.FALSE.
         GO TO 5
      END IF

      TON = 'N'
      IF(ISTAT .EQ. 2) THEN
C
C   Input from HELP window.
C
         CALL DGCLEV(2)
         DO ICOM = 1,7
            IF(COMMND(ICOM:ICOM) .NE. ' ' .AND.
     &         COMMND(ICOM:ICOM) .NE. 'x' .AND.
     &         COMMND(ICOM:ICOM) .NE. '=')  GO TO 8
         END DO
         ICHSTR(1:1) = '*'
    8    IF(COMMND(ICOM:ICOM) .NE. '?') THEN
            FCLIC = .TRUE.
            NLETDM = 1
            LCHSTR = MIN(NCHSET,9-ICOM)
            IF(BUTSTA.GT.0) THEN
               LCHSTR = 0
               ICOM = 0
               GO TO 1000
            END IF
            ICHSTR(1:LCHSTR) = COMMND(ICOM:ICOM+LCHSTR-1)
            CALL DWR_ADD(' '//ICHSTR(1:LCHSTR))
            ICOM=ICOM+LCHSTR-1
         ELSE
            CALL DQHLP(COMMND(ICOM:ICOM+1))
            GO TO 3
         END IF
      END IF

      IF(ISTAT .EQ. 4) THEN
C
C   Rubberband, Pick or Point request is terminated.
C
         CALL DGCLEV(4)
         LCHSTR = 0
         GO TO 1000
      END IF

      IF(ISTAT .EQ. 1) THEN
C
C   Terminal input.
C
         CALL DGCLEV(1)
         FCLIC=.FALSE.
C
C   Was input an escape sequence (function key)?
C
         IF(JCHSTR(1:1) .EQ. CHAR(27) .OR. JCHSTR(1:1) .EQ. CHAR(155))
     &     THEN
            CALL DGPESC(JCHSTR(1:MOD(IOSB(4),256)), LCHSTR, AMAC)
            LCHSTR=-LCHSTR
            L1=LENOCC(AMAC)
            CALL DWR_ADD('@'//AMAC(1:L1)//':')
            IF(FLGOOB(12).AND..NOT.FMACDM) THEN
C  Record macro invokation for macro learning (CTRL-L is ON).
C  Allow only if there are enough characters free in the buffer.
               IF(IPNTDM+L1+1.GT.2000) THEN
                  CALL DWRT('Overflow in macro save buffer.')
                  FLGOOB(12)=.FALSE.
               ELSE
                  IF(IPNTDM.EQ.2.AND.TMACDM(1:1).EQ.':') IPNTDM=1
                  TMACDM(IPNTDM:IPNTDM+L1+1)='@'//AMAC(1:L1)//':'
                  IPNTDM=IPNTDM+L1+2
               END IF
            END IF
            IF(ICNTRL.EQ.0) THEN
               ICHSTR=AMAC
               GO TO 999
            END IF
            IF(BUTSTA.GT.0) GO TO 1010
            GO TO 10
         END IF
C
C   Was input Delete or a CTRL-KEY sequence?
C
         IF((IOSB(1).EQ.1) .AND. (IOSB(2).EQ.0). AND.
     &     (MOD(IOSB(4),256).EQ.1)) THEN
            INT2 = -(IOSB(3)-Z'FF00')
            LCHSTR = INT2
            IF(LCHSTR .LE. 0 .AND. LCHSTR .GE. -31) THEN
               ICHSTR = 'CTRL-'//CHAR(64-LCHSTR)
            ELSE IF(LCHSTR .EQ. -127) THEN
               ICHSTR = 'DELETE'
            ELSE
               ICHSTR = ' '
            END IF
            IF(LCHSTR.EQ.-26) THEN
C   CTRL-Z. Echo Exit in inverse.
               CALL DWR_ADD(CHAR(27)//'[7m Exit '//CHAR(27)//'[m')
            END IF
            IF(BUTSTA.GT.0) GO TO 1010
            GO TO 999
         END IF
         LCHSTR = IOSB(2)
         IF(BUTSTA.GT.0.AND.(JCHSTR(1:LCHSTR).NE.'<')) GO TO 1010
         ICHSTR(1:LCHSTR) = JCHSTR(1:LCHSTR)
         IF(JCHSTR(1:1).EQ.'='.AND.LCHSTR.EQ.1) THEN
C
C   Echo a '+' instead of '='.
C
            WRITE(*,'(A,$)') '+'//CHAR(8)//'+'
            ICHSTR(1:1) = '+'
         END IF
      END IF
    9 IF((ISTAT.NE.2).AND.(.NOT.FCANCL).AND.(.NOT.FMACDM))
     & WRITE(MUN9DU,'(A,$)') ICHSTR(1:LCHSTR)
      ICHSTR(LCHSTR+1:) = ' '
      IF(ICNTRL.LE.0) GO TO 999
      IF(ICHSTR(1:1) .EQ. '=') ICHSTR(1:1) = '+'
      IF(ICHSTR .EQ. ';' .AND. LCHSTR .EQ. 1) THEN
         CALL DGPOP('FLIP')
         GO TO 2
      END IF
      IF(ICHSTR .EQ. '^' .AND. LCHSTR .EQ. 1) THEN
         CALL DGPOP('POP')
         GO TO 2
      END IF
      IF(ICHSTR .EQ. '<' .AND. LCHSTR .EQ. 1) THEN
         CALL DWR_ADD_SEMICOLON_DELET
         CALL DQHLP('<<')
         GO TO 2
      END IF
      N = ICHAR(ICHSTR(1:1))
C
C   Put last characters in terminal IO buffer. Flush it first if too much.
C
      IF(IPTBUF+LCHSTR.LE.LEN(INPBUF)) THEN
         INPBUF(IPTBUF+1:)=ICHSTR(1:LCHSTR)
         IPTBUF=IPTBUF+LCHSTR
         LENBUF=LENBUF+LCHSTR
      ELSE
         IPTBUF=LCHSTR
         LENBUF=LCHSTR
         INPBUF=ICHSTR(1:LCHSTR)
      END IF
      GO TO 999
   10 CONTINUE
C
C  Start macro execution if key is defined.
C
      CALL DGETM0(AMAC,ICNTRL)
      IF(FMACDM) GO TO 2
C
C   Error. Return negative of function key number
C
      LCHSTR=-IABS(LCHSTR)
  999 IF(FLTHLP) THEN
C  Check HELP on/off a final time.
         CALL DGCLEV(3)
         CALL DQHLP('<<')
         FLTHLP=.FALSE.
      END IF
 1000 RETURN

 1010 CONTINUE
C
C   Keyboard input terminates a rubberband, Pick or Point request. Store
C   the character(s) to be processed next time DGETST is entered. We use
C   a similar setup as for HELP window input for this.
C   Specials:
C     For a CTRL-x or escape sequence  the value is written in the first
C   two bytes of COMMND. The first byte is therefore either 0 or 1.
C
      IF(LCHSTR.GE.0) THEN
         IF(JCHSTR(1:1).EQ.'='.AND.LCHSTR.EQ.1) THEN
C
C   Echo a '+' instead of '='.
C
            WRITE(*,'(A,$)') '+'//CHAR(8)//'+'
            JCHSTR(1:1) = '+'
         END IF
         COMMND=JCHSTR(1:LCHSTR)
      ELSE
         COMMND(1:1)=CHAR((-LCHSTR)/256)
         COMMND(2:)=CHAR(MOD(-LCHSTR,256))
      END IF
      ICOM=0
      FCANCL=.TRUE.
      GO TO 1000

      END

      SUBROUTINE DGTINT(INTEG,LINTEG)
C  Read a string with DGETLN and convert it, if possible, to an integer.
C  If nothing is read, INTEG and LINTEG are set to zero.
C
C   X version only!
C
      Implicit None
      Integer Integ, LInteg
      Character T80*80

      Call DGETLN(T80,LInteg,Len(T80))
      Integ = 0
      If(LInteg .LE. 0) Then
         LInteg = 0
         Return
      EndIf

      Read(T80(1:LInteg),'(BN,I20)',Err=100) Integ
      Return

  100 CALL DWRT('Format conversion error, input not integer.')
      LInteg=-1
      Return
      End

      SUBROUTINE DGPESC (INC,NKEY,CHKEY)
C   This routine processes a standard escape sequence.
C
C  INC      Character sequence starting with ESC or CSI.
C  NKEY     If the escape sequence is identified NKEY is the code de-
C           fined in the $SMGDEF macro , otherwise zero.
C  CHKEY    If NKEY is determined CHKEY will be set to the name of the
C           corresponding key.
C
C   Björn S. Nilsson, 26-Apr-1989.
C   In the X version ETAB contains X key names in the first part and
C   standard VT200+ escape sequencies in the second part.
C   A third set of defs was added on 10-Jun-1993. DECs VMS motif servers
C   now names E3 as DRemove, not _Remove.
C
      IMPLICIT NONE
      CHARACTER INC*(*), CHKEY*(*)
      INTEGER   NKEY

      CHARACTER ETAB(256:316,3)*8/
     & 'KP_F1','KP_F2','KP_F3','KP_F4',
     & 'KP0','KP1','KP2','KP3','KP4',
     & 'KP5','KP6','KP7','KP8','KP9',
     & 'Enter','Minus','Comma','Period',
     & 'Up','Down','Left','Right',
     & 3*' ',
     & 'F1','F2','F3','F4','F5',
     & 'F6','F7','F8','F9','F10',
     & 'F11','F12','F13','F14',
     & 'Help','Menu','F17','F18','F19','F20',
     & 10*' ',
     & 'Find','Insert','_Remove','Select','Prior','Next',

     & 'OP  ','OQ  ','OR  ','OS  ',
     & 'Op  ','Oq  ','Or  ','Os  ','Ot  ',
     & 'Ou  ','Ov  ','Ow  ','Ox  ','Oy  ',
     & 'OM  ','Om  ','Ol  ','On  ',
     & 'A   ','B   ','D   ','C   ',
     & 3*'    ',
     & '12~ ','13~ ','14~ ','15~ ','16~ ',
     & '17~ ','18~ ','19~ ','20~ ','21~ ',
     & '23~ ','24~ ','25~ ','26~ ',
     & '28~ ','29~ ','31~ ','32~ ','33~ ','34~ ',
     & 10*'    ',
     & '1~  ','2~  ','3~  ','4~  ','5~  ','6~  ',

     & 'KP_F1','KP_F2','KP_F3','KP_F4',
     & 'KP0','KP1','KP2','KP3','KP4',
     & 'KP5','KP6','KP7','KP8','KP9',
     & 'Enter','Minus','Comma','Period',
     & 'Up','Down','Left','Right',
     & 3*' ',
     & 'F1','F2','F3','F4','F5',
     & 'F6','F7','F8','F9','F10',
     & 'F11','F12','F13','F14',
     & 'Help','Menu','F17','F18','F19','F20',
     & 10*' ',
     & 'Find','Insert','DRemove','Select','Prior','Next'/

      INTEGER I, I1, I2
      CHARACTER KEYTMP*8
      CHARACTER KEYNAM(256:316)*12/
     1 'PF1','PF2','PF3','PF4',
     2 'KP0','KP1','KP2','KP3','KP4','KP5','KP6','KP7','KP8','KP9',
     3 'ENTER','MINUS','COMMA','PERIOD',
     4 'UP','DOWN','LEFT','RIGHT',3*' ',
     5 'F1','F2','F3','F4','F5','F6','F7','F8','F9','F10',
     6 'F11','F12','F13','F14','F15','F16','F17','F18','F19','F20',
     7 10*' ',
     8 'E1','E2','E3','E4','E5','E6'/
C ALT8 'FIND','INSERT_HERE','REMOVE','SELECT','PREV_SCREEN','NEXT_SCREEN'/

      NKEY = 0
      CHKEY = ' '
      I1 = 2
      IF(INC(2:2) .EQ. '[') I1 = 3
      DO I = I1, LEN(INC)
         IF(INC(I:I) .NE. ' ' .AND. INC(I:I) .NE. CHAR(0)) I2 = I
      END DO
      KEYTMP = INC(I1:I2)
      DO I1 = 1, 3
         DO I = 256, 316
            IF(KEYTMP .EQ. ETAB(I,I1)) THEN
               NKEY = I
               CHKEY = KEYNAM(I)
               RETURN
            END IF
         END DO
      END DO

      RETURN
      END

*DK DGETM
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DGETM(A,LCH,ICNTRL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C   This routine and the following entries control the execution of
C   DALI macros.
C
C   DGETM controls the reading of macros and returns next character
C   in a macro string.
C
C   X version !
C   FNTADM = NoTypeAhead. Disable some typeahead while macro is
C   executing. (20-Aug-1993)
C
      CHARACTER*(*) A,AMAC,TINMAC
      INTEGER LCH
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- D0
      COMMON /D0PFLG/ FTRMD0,FLOGD0,FPROD0,FSOLD0
      LOGICAL FTRMD0,FLOGD0,FPROD0,FSOLD0
C------------------------------------------------------------------- DM
      LOGICAL FMACDM,FMOPDM,FMINDM,FANIDM,FNTADM,FINMDM
      COMMON /DMACRC/ FMACDM,NLETDM,FMOPDM,FMINDM,FANIDM,FNTADM,
     &  FINMDM
C------------------------------------------------------------------- DU
      COMMON /DUSD2C/NUN6DU,NUN9DU
      PARAMETER (MUN5DU=5,MUN6DU=6,MUN9DU=9)
C------------------------------------------------------------------- DU
      PARAMETER (MPNPDU=60)
      REAL DACUDU
      COMMON /DUSDAC/ NUNIDU,
     1 WAITDU,ECCHDU,BOXSDU,ECSQDU,HCSQDU,
     1 HIMXDU,RESCDU,SLOWDU,WISUDU,SETTDU,
     1 USLVDU,SYLRDU,SDATDU,RZDHDU,ROTADU,
     1 SCMMDU,CLMMDU,CUMMDU,WIMIDU,CCMIDU,
     1 ECMIDU,HCMIDU,SHTFDU(2)    ,FTDZDU,
     1 FRNLDU,DACUDU,SHECDU,SHHCDU,DRLIDU,
     1 SQLCDU,CHLCDU,HEADDU,VRDZDU,CHHCDU,
     1 AREADU,DFWIDU(0:1)  ,DFLGDU,PBUGDU,
     1 CHTFDU,RNCLDU,PR43DU,PR44DU,PR45DU,
     1 PR46DU(4),                  PR50DU,
     1 PR51DU(4),                  PR55DU,
     1 PR56DU(4),                  PR60DU
      DIMENSION PARADU(MPNPDU)
      EQUIVALENCE (PARADU,WAITDU)
      COMMON /DUSDA2/PLIMDU(2,MPNPDU)
      COMMON /DUSDAT/ TPR1DU(MPNPDU)
      CHARACTER*2 TPR1DU
C------------------------------------------------------------------  DC
      COMMON /DCFTVT/ TFILDC,TITLDC,TDEFDC
      CHARACTER*8 TFILDC
      CHARACTER*10 TITLDC
      CHARACTER*3 TDEFDC
C
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER*80 TXTADW
      CHARACTER*1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C-------------------End of DALI_CF commons----------------
      CHARACTER*2 DT2
      CHARACTER*80 TSCR,TSCR1
      CHARACTER TCOM*50, T3C*3
      INTEGER L2
      INTEGER ICNTRL,NUMMAC,NRLINE,I1,LMACNF,L1,KMAC
      LOGICAL FSTRT,FCONT,FNUM,FABORT
      DATA FSTRT/.TRUE./,FCONT/.FALSE./,FNUM/.FALSE./

C
C  TMNAME & TMAC store macro line names and contents.
C  MACSAVE stores KMAC, MMAC and KREP for previous macro level.
C  FMSAVE stores FMINDM and FMOPDM (Intrinsic resp. OPEN output) flags.
C  TMSAVE stores macro name of presently executing macro.
C
      INTEGER MACMX, MACLEVMAX
      PARAMETER (MACMX=500, MACLEVMAX=8)
      CHARACTER TMNAME*20,TMAC*80,MACFIL*80,TMSAVE*20
      INTEGER LSCRN,KMAX,MACLEN,MACLEV,MACSAVE
      LOGICAL FMSAVE
      COMMON /MACCMN/ LSCRN,KMAX,MACLEN(MACMX),MACFIL,TMNAME(MACMX),
     & TMAC(MACMX),MACLEV,MACSAVE(3,MACLEVMAX),FMSAVE(2,MACLEVMAX),
     & TMSAVE(MACLEVMAX)

      LOGICAL FLGOOB,FLGDOP
      COMMON /OOBCMN/ FLGOOB(0:31),FLGDOP

      LOGICAL FLSTMA/.FALSE./

      LCH=1
C
C  Get next character.
C
CCC 1 CALL DGCHKX
    1 CONTINUE
      MMAC=MMAC+1
      IF(MMAC.GT.MACLEN(KMAC)) GO TO 2

      IF(TMAC(KMAC)(MMAC:MMAC).EQ.'>') GO TO 1

      IF(TMAC(KMAC)(MMAC:MMAC).EQ.':') THEN
C
C   Colon is equivalent to a return if following a numeric field, or if
C   ICNTRL is 0.
C
         IF(FNUM.OR.ICNTRL.EQ.0) THEN
            LCH=-13
            FNUM=.FALSE.
            A=TMAC(KMAC)(MMAC:MMAC)
            GO TO 4
         ELSE
            GO TO 1
         END IF
      END IF

      IF(TMAC(KMAC)(MMAC:MMAC).EQ.'=') THEN
         FNUM=.TRUE.
         GO TO 1
      END IF

      IF(TMAC(KMAC)(MMAC:MMAC).EQ.'(') THEN
         IF((MACLEV.GT.1).AND.(.NOT.FMINDM)) GO TO 1
         IF(.NOT.FMOPDM) CALL DWRT_SETUP('TERMINAL=OFF')
         FMOPDM=.TRUE.
         GO TO 1
      END IF

      IF(TMAC(KMAC)(MMAC:MMAC).EQ.')') THEN
         IF((MACLEV.GT.1).AND.(.NOT.FMINDM)) GO TO 1
         IF(FMOPDM) THEN
            CALL DWRT_SETUP('TERMINAL=ON')
            FMOPDM=.FALSE.
         END IF
         GO TO 1
      END IF

      IF(TMAC(KMAC)(MMAC:MMAC).EQ.'[') THEN
         IF(MACLEV.GT.1) GO TO 1
         CALL DQHLP('[[')
         GO TO 1
      END IF

      IF(TMAC(KMAC)(MMAC:MMAC).EQ.']') THEN
         IF(MACLEV.GT.1) GO TO 1
         CALL DQHLP(']]')
         GO TO 1
      END IF

      A=TMAC(KMAC)(MMAC:MMAC)

      FNUM=(((A(1:1).GE.'0') .AND. (A(1:1).LE.'9')) .OR. (A(1:1).EQ.'.')
     & .OR. (A(1:1).EQ.'+') .OR. (A(1:1).EQ.'-')) .AND. FNUM

      IF(A.EQ.'{') THEN
         IF(MACLEV.GT.1) GO TO 1
C
C  Start of a loop.
C
         IF(KREP.EQ.0) THEN
            KREP=KMAC+1
            CALL DWRT(
     &       'Start of macro loop. Hit Ctrl-C to hold the loop')
            CALL DWRT(
     &       ' at watch-points, Ctrl-H to hold before next pass.')
            WRITE(TSCR1,1002) DT2(WAITDU)
 1002       FORMAT('Waiting-time = ',A,' sec. Start ? [Y] ')
            CALL DTYANS(TSCR1, 'Yy', NANSW)
  101       FLGOOB(3)=.FALSE.
            FLGOOB(8)=.FALSE.
            IF(NANSW .GE. 0) THEN
               CALL DTYANS('Do you want the terminal output ? [N]',
     &          'Nn',NANSW)
               IF(NANSW .GE. 0) THEN
                  IF(.NOT.FMOPDM) THEN
                     CALL DWRT_SETUP('TERMINAL=OFF')
                     FMOPDM=.TRUE.
                  END IF
               END IF
               GO TO 2
            ELSE
               CALL DWRT('|')
               GO TO 3
            END IF
         END IF
         GO TO 2
      END IF

      IF(A.EQ.'}') THEN
C
C   End of loop.
C
         IF(FLGOOB(3).OR.FLGOOB(8)) THEN
C
C   CTRL-C and CTRL-H hold the loop before next iteration.
C
            FLGOOB(3)=.FALSE.
            FLGOOB(8)=.FALSE.
            IF(FMOPDM) CALL DWRT_SETUP('TERMINAL=ON')
            CALL DTYANS('Loop ends. Do you want to continue? [Y] ',
     &       'Yy', NANSW)
            IF(NANSW .LT. 0) THEN
               KREP=0
               FMOPDM=.FALSE.
               CALL DWRT('|')
               GO TO 3
            END IF
  102       IF(FMOPDM) CALL DWRT_SETUP('TERMINAL=OFF')
         END IF
         KMAC=KREP-1
         GO TO 2
      END IF

      IF(A.EQ.'&') THEN
C
C   Text string which must fill the rest of the line. This should be
C   improved later so that text always must be delimited by ampersands.
C   Not foolproof at present.
C
         LTXT=LENOCC(TMAC(KMAC)(MMAC+1:))
         IF(TMAC(KMAC)(MMAC+LTXT:MMAC+LTXT).EQ.'-') LTXT=LTXT-1
         IF(TMAC(KMAC)(MMAC+LTXT:MMAC+LTXT).EQ.'&') LTXT=LTXT-1
         CALL DQTIC(LTXT,TMAC(KMAC)(MMAC+1:))
         GO TO 2
      END IF

      IF(A.EQ.'"'.OR.(A.EQ.'-'.AND.MMAC.EQ.MACLEN(KMAC))) THEN
C
C   Watch point, also at continuation of a line.
C
         IF(FLGOOB(3)) THEN
C
C   CTRL-C holds the macro.
C
            IF(FMOPDM) CALL DWRT_SETUP('TERMINAL=ON')
            CALL DTYANS('Macro pauses. Do you want to continue? [Y] ',
     &       'Nn', NANSW)
            FLGOOB(3)=.FALSE.
            IF(NANSW .GT. 0) THEN
               FMOPDM=.FALSE.
               CALL DWRT('Macro terminated.')
               GO TO 3
            END IF
  103       IF(FMOPDM) CALL DWRT_SETUP('TERMINAL=OFF')
            IF(A.EQ.'-') GO TO 2
            GO TO 1
         END IF
         CALL DGWAIT(WAITDU)
         IF(A.EQ.'-') GO TO 2
         GO TO 1
      END IF

      GO TO 4

    2 CONTINUE
C
C   End of line handling.
C
      IF(A.EQ.'-') THEN
         KMAC=KMAC+1
         MMAC=0
         GO TO 1
      END IF
      IF(KREP.NE.0) THEN
         IF(KMAC.LT.KMAX) THEN
            KMAC=KMAC+1
         ELSE
            KMAC=KREP
         END IF
         MMAC=0
         GO TO 1
      END IF
      GO TO 3

      ENTRY DMCTRM
C   Entry to abort macro execution.
      FABORT=.TRUE.

C
C   Terminating the macro.
C
    3 IF(MACLEV.EQ.1.OR.FABORT) THEN
         CALL DQHLP(']]')
         CALL DGZOOM(6,99,0,0)
         IF(FMOPDM) THEN
            CALL DWRT_SETUP('TERMINAL=ON')
            FMOPDM=.FALSE.
         END IF
      END IF
      IF(TMNAME(KMAC).EQ.'ST '.AND.IFIX(USLVDU).GE.7) THEN
         CALL DWRT(' ++++ End of start Macro ++++')
      ELSE IF(FABORT) THEN
         CALL DWRT('End of Macro execution.')
      ELSE IF(MACLEV.GT.0.AND.IFIX(USLVDU).EQ.9) THEN
         I1=LENOCC(TMSAVE(MACLEV))
         WRITE(TXTADW,1006) TMSAVE(MACLEV)(1:I1),MACLEV
 1006    FORMAT('End of Macro ',A,' at level',I2,'.')
         CALL DWRC
      END IF
      MACLEV=MAX(MACLEV-1,0)
      IF(FABORT) MACLEV=0
      IF(MACLEV.GT.0) THEN
         KMAC=MACSAVE(1,MACLEV)
         MMAC=MACSAVE(2,MACLEV)
         KREP=MACSAVE(3,MACLEV)
         FMACDM=.TRUE.
         FMINDM=FMSAVE(1,MACLEV)
         FMOPDM=FMSAVE(2,MACLEV)
         LCH=1
         GO TO 1
      ELSE
         FMACDM=.FALSE.
         FMINDM=.FALSE.
         FNTADM=.FALSE.
         FLGOOB(3)=.FALSE.
         FLGOOB(8)=.FALSE.
      END IF
      RETURN

    4 IF(TMAC(KMAC)(MMAC+1:MMAC+1).NE.'('.AND.FTRMD0)
     & CALL DWR_ADD(A(1:1)) 
      RETURN
CH --------------------------------------------------------------------
CH
      ENTRY DGETM0(AMAC,ICNTRL)
CH
CH --------------------------------------------------------------------
CH
C   Depending on
C   ICNTRL   >=0   Execute macro AMAC
C             -1   List macro AMAC
C             -2   Read in a new macro file. Insert new definitions instead
C                  of possible existing, keep keywords not in the file.
C
      IF(ICNTRL.EQ.-2) FSTRT=.TRUE.
      FABORT=.FALSE.
   20 IF(FSTRT) THEN
C
C   Read in predefined macros from file.
C   Note. As of May 1989 the format is:
C      1. An input line is at most 80 characters.
C      2. A key is defined by the first 3 characters of it's "normal"
C         name. To bind a macro to a key, start the line by KEY=macro,
C         e.g., F17=macro. A start macro is defined by ST=macro.  Com-
C         mands may span several lines if - (hyphen) is used as a con-
C         tinuation character.
C      3. Loops are initialised by '{' and ended by '}' as the last
C         command on a line. Note that a many-lines-loop is terminated
C         after the first line not ending with - (hyphen).
C      4. Lines containing header text should start with '&' (ampersand).
C      5. Anything after '!' (exclamation point) is a comment (except in
C         text delimited by ampersands).
C
         FSTRT=.FALSE.
         IF(MACFIL.NE.' ') THEN
            CALL DGOPEN(NUNIDU,MACFIL,2,*21,ISTAT)
            GO TO 23
         END IF
   21    MACFIL=TFILDC(1:LENOCC(TFILDC))//'MAC'
         CALL DGOPEN(NUNIDU,MACFIL,2,*34,ISTAT)
   23    NUMMAC=0
         NRLINE=0
   25    READ(NUNIDU,'(A)',END=29) TSCR
         NRLINE=NRLINE+1
         LSTR=LENOCC(TSCR)
         IF(LSTR.EQ.0) GO TO 25
         DO I = 1,LSTR
            I1=I
            IF(TSCR(I:I).EQ.'!'.OR.TSCR(1:1).EQ.'$') GO TO 25
            IF(TSCR(I:I).NE.' ') GO TO 26
         END DO
   26    I=I1-1
         KMAX=KMAX+1
         IF(KMAX.GT.MACMX-1) THEN
C
C   Last entry in the table is reserved for the 'intrinsic' macro.
C
            CALL DWRT_SETUP('TERMINAL=ON')
            CALL DWRT('DALI''s Macro table is full. Redimension!')
            CALL DWRT('Macro reading stopped.')
            GO TO 29
         END IF
         FCONT=.FALSE.
         IF(KMAX.GT.1) THEN
            K=KMAX-1
            FCONT=TMAC(K)(MACLEN(K):MACLEN(K)).EQ.'-'.OR.
     &        TMAC(K)(MACLEN(K):MACLEN(K)).EQ.'{'
         END IF
         IF(.NOT.FCONT) THEN
C
C   New macro.
C
            I=INDEX(TSCR,'=')
            IF(I.EQ.0) THEN
               WRITE(T3C,'(I3)') NRLINE
               CALL DWRT('Error in macro line number '//T3C)
               CALL DWRT(TSCR)
               CALL DWRT(' Line will be ignored.')
               KMAX=KMAX-1
               GO TO 25
            END IF
            LMACNF=LENOCC(TSCR(1:I-1))
            CALL DGUPCS(TSCR(1:LMACNF),TSCR(1:LMACNF))
            IF(LMACNF.GT.20) THEN
               CALL DWRT_SETUP('TERMINAL=ON')
               CALL DWRT('Macro name'//TSCR(1:LMACNF)//
     &          ' is too long. Truncated to '//TSCR(1:20)//'.')
               LMACNF=20
            END IF
            TMNAME(KMAX)=TSCR(1:LMACNF)
C
C   Try to help the user with a few nicknames.
C
            IF(TMNAME(KMAX)(2:2).EQ.'0') TMNAME(KMAX)=
     &       TMNAME(KMAX)(1:1)//TMNAME(KMAX)(3:3)//' '
            IF(TMNAME(KMAX).EQ.'ESC') TMNAME(KMAX)='F11'
            IF(TMNAME(KMAX).EQ.'BS') TMNAME(KMAX)='F12'
            IF(TMNAME(KMAX).EQ.'LF') TMNAME(KMAX)='F13'
            IF(TMNAME(KMAX).EQ.'HLP'.OR.TMNAME(KMAX).EQ.'HEL'.OR.
     &       TMNAME(KMAX).EQ.'HELP') TMNAME(KMAX)='F15'
            IF(TMNAME(KMAX).EQ.'DO') TMNAME(KMAX)='F16'
            IF(TMNAME(KMAX).EQ.'FND'.OR.TMNAME(KMAX).EQ.'FIND'.OR.
     &       TMNAME(KMAX).EQ.'FIN') TMNAME(KMAX)='E1'
            IF(TMNAME(KMAX).EQ.'INS'.OR.TMNAME(KMAX).EQ.'INSE'.OR.
     &       TMNAME(KMAX).EQ.'INSER'.OR.TMNAME(KMAX).EQ.'INSERT'.OR.
     &       TMNAME(KMAX)(1:8).EQ.'INSERT H') TMNAME(KMAX)='E2'
            IF(TMNAME(KMAX).EQ.'REM'.OR.TMNAME(KMAX).EQ.'REMO'.OR.
     &       TMNAME(KMAX).EQ.'REMOV'.OR.TMNAME(KMAX).EQ.'REMOVE')
     &       TMNAME(KMAX)='E3'
            IF(TMNAME(KMAX).EQ.'SEL'.OR.TMNAME(KMAX).EQ.'SELE'.OR.
     &       TMNAME(KMAX).EQ.'SELEC'.OR.TMNAME(KMAX).EQ.'SELECT')
     &       TMNAME(KMAX)='E4'
            IF(TMNAME(KMAX).EQ.'PRE'.OR.TMNAME(KMAX).EQ.'P S'.OR.
     &       TMNAME(KMAX).EQ.'PRV'.OR.TMNAME(KMAX).EQ.'PREV'.OR.
     &       TMNAME(KMAX)(1:6).EQ.'PREV S') TMNAME(KMAX)='E5'
            IF(TMNAME(KMAX).EQ.'NEX'.OR.TMNAME(KMAX).EQ.'N S'.OR.
     &       TMNAME(KMAX).EQ.'NXT'.OR.TMNAME(KMAX).EQ.'NEXT'.OR.
     &       TMNAME(KMAX)(1:6).EQ.'NEXT S') TMNAME(KMAX)='E6'
            IF(TMNAME(KMAX).EQ.'COM'.OR.TMNAME(KMAX).EQ.'COMM')
     &       TMNAME(KMAX)='COMMA'
            IF(TMNAME(KMAX).EQ.'MIN'.OR.TMNAME(KMAX).EQ.'MINU')
     &       TMNAME(KMAX)='MINUS'
            IF(TMNAME(KMAX).EQ.'ENT'.OR.TMNAME(KMAX).EQ.'ENTE')
     &       TMNAME(KMAX)='ENTER'
            IF(TMNAME(KMAX).EQ.'PER'.OR.TMNAME(KMAX).EQ.'PERI'.OR.
     &       TMNAME(KMAX).EQ.'PERIO') TMNAME(KMAX)='PERIOD'
            IF(LMACNF.LT.LENOCC(TMNAME(KMAX))) THEN
               DO K=LMACNF+1,LENOCC(TMNAME(KMAX))
                  I1=INDEX(TSCR(I+1:),' !')
                  IF(I1.NE.0) TSCR(I+I1:)=TSCR(I+I1+1:)
               END DO
            ELSE IF(LMACNF.GT.LENOCC(TMNAME(KMAX))) THEN
               DO K=LENOCC(TMNAME(KMAX))+1,LMACNF
                  I1=INDEX(TSCR(I+1:),' !')
                  IF(I1.NE.0) TSCR(I+I1:)=' '//TSCR(I+I1:)
               END DO
            END IF
            LMACNF=LENOCC(TMNAME(KMAX))
            NUMMAC=NUMMAC+1
C
C  Remove a possible previous definition.
C
            DO K=1,KMAX-1
               IF(TMNAME(K).EQ.TMNAME(KMAX)) THEN
                  CALL DWRT(
     &             'Macro '//TMNAME(K)(1:LMACNF)//' was replaced.')
                  K1=K
                  GO TO 110
               END IF
            END DO
            IF(FLSTMA)
     &       CALL DWRT('Macro '//TMNAME(K)(1:LMACNF)//' was inserted.')
            GO TO 130
  110       DO K=K1+1,KMAX
               IF(TMNAME(K).NE.' ') THEN
                  K2=K
                  GO TO 120
               END IF
            END DO
  120       DO K=K2,KMAX
               TMNAME(K1+K-K2)=TMNAME(K)
               TMAC(K1+K-K2)=TMAC(K)
               MACLEN(K1+K-K2)=MACLEN(K)
            END DO
            KMAX=K1+KMAX-K2
  130       CONTINUE
         ELSE
C
C  Continuation.
C
            TMNAME(KMAX)=' '
         END IF
C
C   Then work on the macro, strip off blanks in the beginning and
C   deactivate possible comments at the end.
C
         TMAC(KMAX)=TSCR(I+1:)
         DO I=1,80
            IF(TMAC(KMAX)(1:1).NE.' ') GO TO 27
            TMAC(KMAX)=TMAC(KMAX)(2:)
         END DO
   27    IF(TMAC(KMAX)(1:1).EQ.'&') THEN
C
C   Special treatment of text strings delimited by ampersands.
C
            I1=INDEX(TMAC(KMAX)(2:),'&')
            IF(I1.GT.0) THEN
               I=INDEX(TMAC(KMAX)(I1+1:),'!')
               IF(I.NE.0) TMAC(KMAX)(I1+I:)=' '
            END IF
            MACLEN(KMAX)=LENOCC(TMAC(KMAX))
            GO TO 25
         END IF
         I1=INDEX(TMAC(KMAX),'!')
         IF(I1.EQ.0) I1=81
         MACLEN(KMAX)=LENOCC(TMAC(KMAX)(1:I1-1))
         I1=MACLEN(KMAX)
         CALL DGUPCS(TMAC(KMAX)(1:I1),TMAC(KMAX)(1:I1))
         GO TO 25
   29    CLOSE(UNIT=NUNIDU)
         FLSTMA=.TRUE.
         IF(ICNTRL.EQ.-2) THEN
            CALL DWRT_SETUP('TERMINAL=ON')
            I1 = 1
            IF(NRLINE.GT.9) I1 = 2
            IF(NRLINE.GT.99) I1 = 3
            WRITE(TSCR,'(I<I1>,A)') NRLINE, ' lines with'
            L2 = LENOCC(TSCR)
            I1 = 1
            IF(NUMMAC.GT.9) I1 = 2
            IF(NUMMAC.GT.99) I1 = 3
            WRITE(TSCR(L2+2:),'(I<I1>,A)')  NUMMAC,
     .       ' macros were read from'
            L2 = LENOCC(TSCR)
            WRITE(TSCR(L2+2:),'(A)') MACFIL(1:LENOCC(MACFIL))
            L2 = LENOCC(TSCR)
            CALL DWRT(TSCR(1:L2))
            RETURN
         END IF
         IF(AMAC.EQ.'***') RETURN
      END IF
      IF(MACLEV.EQ.0) FMOPDM=.FALSE.
      KREP=0
      IF(AMAC.EQ.' ') GO TO 30
      DO K=1,KMAX
         IF(TMNAME(K).EQ.AMAC) THEN
           IF(MACLEN(K).EQ.0) GO TO 30
           GO TO 31
         END IF
      END DO
   30 FABORT=.TRUE.
      L1=LENOCC(AMAC)
      IF(FMOPDM) THEN
         CALL DWRT_SETUP('TERMINAL=ON')
         FMOPDM=.FALSE.
      END IF
      CALL DWRT('Macro '//AMAC(1:L1)//' is not defined.')
      FNTADM=.FALSE.
      CALL DWR_BELL(3, .FALSE.)
      IF(FMACDM) GO TO 3
      CALL DWRT_END(2)
      RETURN
   31 IF(ICNTRL.EQ.-1) THEN
C
C   We are just listing a macro.
C
         L1=MAX(LENOCC(TMNAME(K)),3)
         ILEN=LENOCC(TMAC(K))
         I=MIN(79-L1,ILEN)
         CALL DWRT_SETUP('TERMINAL=ON')
         CALL DWRT(TMNAME(K)(1:L1)//'='//TMAC(K)(1:I))
         ILINE=1
         FLGOOB(3)=.FALSE.
         DO M=K+1,KMAX
            IF(TMNAME(M).NE.' ') RETURN
            ILEN=LENOCC(TMAC(M))
            I=MIN(76,ILEN)
            CALL DWRT_SETUP('TERMINAL=ON')
            CALL DWRT('  '//TMAC(M)(1:I))
            ILINE=ILINE+1
            IF(ILINE.GE.LSCRN-1.AND.TMNAME(M+1).EQ.' '.AND.M.NE.KMAX)
     &       THEN
               IF(FLGOOB(3)) THEN
                  CALL DWRT('Output terminated...')
                  GO TO 32
               END IF
               CALL DTYANS('Hit Return to continue... ','N',NANSW)
               IF (NANSW.NE.0) GO TO 32
               ILINE=0
            END IF
         END DO
   32    FLGOOB(3)=.FALSE.
         RETURN
      END IF
C
C   The macro can be executed if the level is not to high.
C
   33 IF(MACLEV.EQ.MACLEVMAX) THEN
         FABORT=.TRUE.
         IF(FMOPDM) THEN
            CALL DWRT_SETUP('TERMINAL=ON')
            FMOPDM=.FALSE.
         END IF
         WRITE(TCOM,'(A,I2,A)') 'Attempt to nest macros beyond '//
     &    ' configured limit', MACLEV, '.'
         CALL DWRT_SETUP('TERMINAL=ON')
         CALL DWRT(TCOM)
         GO TO 3
      END IF
C
C   Or, if it is not called recursively.
C
      DO I=1,MACLEV
         IF(TMNAME(K).EQ.TMSAVE(I)) THEN
            I1=LENOCC(TMNAME(K))
            CALL DWRT('Attempt to call macro '//TMNAME(K)(1:I1)//
     &       ' recursively.')
            FABORT=.TRUE.
            GO TO 3
         END IF
      END DO
      IF(MACLEV.GT.0) THEN
         MACSAVE(1,MACLEV)=KMAC
         MACSAVE(2,MACLEV)=MMAC
         MACSAVE(3,MACLEV)=KREP
         FMSAVE(1,MACLEV)=FMINDM
         FMSAVE(2,MACLEV)=FMOPDM
      END IF
      MACLEV=MACLEV+1
      TMSAVE(MACLEV)=TMNAME(K)
      MMAC=0
      KMAC=K
      FNUM=.FALSE.
      IF(TMNAME(KMAC).EQ.'ST '.AND.IFIX(USLVDU).GE.7) THEN
         CALL DWRT('++++ Beginning of Start Macro ++++')
      ELSE IF(IFIX(USLVDU).EQ.9) THEN
         I1=LENOCC(TMNAME(K))
         WRITE(TCOM,'(A,I2,A)') 'Start executing Macro '//
     &    TMNAME(K)(1:I1)//' at Level', MACLEV, '.'
         CALL DWRT(TCOM)
      END IF
      IF((TMAC(KMAC)(1:1).EQ.'(').AND.(MACLEV.EQ.1)) THEN
         CALL DWRT_SETUP('TERMINAL=OFF')
         FMOPDM=.TRUE.
         MMAC=1
      END IF
      FMACDM=.TRUE.
      IF(MACLEV.EQ.1) CALL DQHLP('[[')
      RETURN
   34 FABORT=.TRUE.
      MACFIL=' '
      CALL DWRT('The macro file cannot be opened!')
      GO TO 3

      ENTRY DGINMA(TINMAC)
C
C   TINMAC is a string of commands that will be executed as a macro.
C   This "intrinsic" macro will not be displayed or written to a file.
C   It will be placed at the last line of the macro array.
C   First determine if an intrinsic macro is at it's end.
C
      IF(FMINDM.AND.KMAC.EQ.MACMX.AND.MMAC.GE.MACLEN(MACMX)) THEN
         FMINDM=.FALSE.
         MACLEV=MAX(MACLEV-1,0)
      END IF
      TMNAME(MACMX)='DALI_INTRINSIC_MACRO'
      TMAC(MACMX)=TINMAC
      I1=LENOCC(TINMAC)
      IF(I1.GT.0) THEN
         IF(TINMAC(I1:I1).EQ.':') I1=I1-1
      END IF
      IF(I1.LE.0) THEN
         CALL DWRT('+++ Zero length intrinsic macro +++')
         RETURN
      END IF
      MACLEN(MACMX)=I1
      K=MACMX
      FMINDM=.TRUE.
      GO TO 33
      END

      Subroutine DGOPEN(Nunit,FileName,Icase,*,Istat)
C   This routine should provide for platform independent fileopening.
C   The fileargument may be given in VMS syntax.
C   Nunit = Logical unit
C   Filename
C   Icase = File open mode. If negative, do not print opening info.
C      1    Write New file.
C      2    Read Old file, open readonly if possible.
C      3    Append to an existing file.
C      5    Write to a file with STATUS='UNKNOWN'
C     12    Interface to AOPEN with 'CARDS' and 'DISK' arguments
C     14    Interface to AOPEN with blank arguments.
C   * label to return to when ERR in opening.
C    Istat  Return status from OPEN
C   Bjorn S. Nilsson, February-1993.
C
      Implicit None
      Integer Nunit, Icase, Istat
      Character*(*) FileName

      Character FullName*120, DFName*120
      Integer LenOcc

      GoTo (100, 200, 300, 999, 500, 999, 999, 999, 999, 999,
     &      999,1200, 999,1400), Iabs(Icase)
      GoTo 999
  100 Continue
C
C  Open a new file for writing.
C
      FullName = DFName(FileName)
      Open(Unit=Nunit,File=FullName,Status='NEW',Err=110,IOstat=Istat)
      Return
  110 Return 1

  200 Continue
C
C  Open an existing file for reading.
C  First try with the filename only.
C
      FullName = DFName(FileName)
      Open(Unit=Nunit,File=FullName,Status='OLD',Err=210,READONLY,
     & IOstat=Istat)
      GoTo 230
C
C  Here we prepend the environmental variable DAL if it makes sense.
C
  210 If (Index(FileName,':') .NE. 0) Return 1
      FullName = DFName('DAL:'//FileName)
      Open(Unit=Nunit,File=FullName,Status='OLD',Err=220,READONLY,
     & IOstat=Istat)
      GoTo 230
  220 Return 1
  230 If(Icase .GT. 0) Call DWR_INPUT(FullName)
      Return

  300 Continue
C
C  Open an existing file for writing by appending.
C
      FullName = DFName(FileName)
      Open(Unit=Nunit,File=FullName,Status='OLD',Access='APPEND',
     & Err=310,IOstat=Istat)
      Return
  310 Return 1

  500 Continue
C
C  Open a file for writing with status unknown.
C
      FullName = DFName(FileName)
      Open(Unit=Nunit,File=FullName,Status='UNKNOWN',Err=510,
     & IOstat=Istat)
      Return
  510 Return 1

 1200 Continue
C
C  Interface to AOPEN for reading with 'CARDS' and 'DISK' arguments.
C
      FullName = DFName(FileName)
      Call AOPEN(Nunit,FullName,'CARDS','DISK',Istat)
      If (Istat .EQ. 0) Then
         Call DWR_INPUT(FullName)
         Return
      EndIf
      If (Index(FileName,':') .NE. 0) Return
      FullName = DFName('DAL:'//FileName)
      Call AOPEN(Nunit,FullName,'CARDS','DISK',Istat)
      If (Istat .EQ. 0) Call DWR_INPUT(FullName)
      Return

 1400 Continue
C
C  Interface to AOPEN for reading with blank arguments.
C
      FullName = DFName(FileName)
      Call AOPEN(Nunit,FullName,'    ','    ',Istat)
      Return

  999 Write(FullName,'(A,I3)') 'DGOPEN called with illegal index ',
     & Icase
      Call DWRT(FullName(1:LenOcc(FullName)))
      Return
      End

      Character*(*) Function DFName(FileName)
C  Convert a pseudo filename to the platform specific name, here UNIX.
C  The input filename may be a VMS file name. It may also start with
C  a UNIX environmental name. This could either be name:more_components
C  or $name/rest_of_the_name
C  Some logicals/environmentals may be mapped:
C    AL$DATA   <->  ALDATA
C    AL$EDIR   <->  ALEDIR
C    AL$STAGE  <->  ALSTAGE
C     BOOK     <->  ALBOOK
C  If this mapping is applied, the filename and extension will be
C  returned in lower case.
C
C  Bjorn S. Nilsson, March 1993
C
      Implicit None
      Character*(*) FileName
      Character Ename*120
      Integer I, I1, I2, LenOcc

      Ename = ' '
      DFName = ' '
      If(FileName(1:1).EQ.'$') Then
C
C  The filename starts with a $. This is interpreted as a UNIX environmental
C  name. The end delimiter must be '/' or ':'
C
         I = Len(FileName) + 1
         I1 = Index(FileName,':')
         If(I1.NE.0) I = I1
         I1 = Index(FileName,'/')
         If(I1.NE.0.AND.I1.LT.I) I = I1
         Ename = FileName(2:I-1)
      Else
         I = Index(FileName,':')
         If(I.NE.0) Then
            Ename = FileName(1:I-1)
            If(Ename.EQ.'AL$DATA')  Ename = 'ALDATA'
            If(Ename.EQ.'AL$EDIR')  Ename = 'ALEDIR'
            If(Ename.EQ.'AL$STAGE') Ename = 'ALSTAGE'
            If(Ename.EQ.'BOOK')     Ename = 'ALBOOK'
         EndIf
      EndIf

      If(Ename.NE.' ') Then
         Call getenv(Ename,DFName)
         If(DFName.NE.' ') Then
            I1 = LenOcc(DFName)
            If(FileName(I+1:).NE.' ')
     &       DFName(I1+1:) = '/'//FileName(I+1:)
         Else
            DFName = FileName
         EndIf
      Else
         DFName = FileName
      EndIf
C
C   There may be VMS-type directories in the name.
C
      I1 = Index(DFName,'[')
      If(I1.NE.0) Then
         I2 = Index(DFName,']')
         If(I2.EQ.0) GoTo 100
         DFName(I1:I1) = '/'
         DFName(I2:I2) = '/'
         Do I = I1,I2
            If(DFName(I:I).EQ.'.') DFName(I:I) = '/'
         EndDo
      EndIf
  100 Do I2 = 1, 100
         I1 = Index(DFName,'//')
         If(I1.EQ.0) GoTo 110
         DFName = DFName(1:I1)//DFName(I1+2:)
      EndDo

  110 If((Ename.EQ.'ALDATA').OR.(Ename.EQ.'ALEDIR').OR.
     & (Ename.EQ.'ALSTAGE').OR.(Ename.EQ.'ALBOOK')) Then
C
C  Lowercase the part of the filename that follows the last '/'
C
         I2 = LenOcc(DFName)
         Do I = I2,1,-1
            If(DFName(I:I).EQ.'/') Then
               I1 = I + 1
               GoTo 120
            EndIf
         EndDo
 120     Do I = I1, I2
            If(DFName(I:I).GE.'A'.AND.DFName(I:I).LE.'Z')
     &       DFName(I:I) = Char(Ichar(DFName(I:I))+32)
         EndDo
      EndIf
      Return
      End

      Subroutine DSPAWN
C  Create a sub-process
C  Bjorn S. Nilsson, 19-Mar-1993.
C
      Implicit None
      Character T72*72
      Integer Istat, estat, tty_reset, tty_set, system

      Call DGIFOC(1,0)
      Call getenv('SHELL', T72)
      If(T72 .EQ. ' ') T72 = '/bin/csh'
      Call DWR_HIGH_LIGHT_FROM_N1_TO_N2(
     & 'SP: Creating subprocess... Return by typing exit',1,2)
      Istat = tty_reset()
      Write(*,'(a)') ' '//Char(13)//Char(10)//Char(10)
      estat = system(T72)
      Istat = tty_set()
      Call DGIFOC(0,-1)
      Call DWR_HIGH_LIGHT_FROM_N1_TO_N2(
     & 'SP: Returned to DALI. Continue...',1,2)
      End

      Subroutine DSPAPR(LPR,TMETA)
C  Print a metafile.
C  Bjorn S. Nilsson, 30-Jan-1994
C
      Implicit None
      Character*(*) LPR, TMETA
      Character*72 T72, LPRPR
      Integer Istat, system, Len, Lenocc

      Call getenv('DALI_PRINTER',LPRPR)
      If(LPRPR.EQ.' ') LPRPR = LPR
      T72 = LPRPR(1:LENOCC(LPRPR))//' '//TMETA(1:LENOCC(TMETA))
      Len = LENOCC(T72)
      Istat = system(T72(1:Len))
      CALL DWRT('Printing with '//T72(1:Len))
      End

      Subroutine DGTRIM(Dest, Src, Length)
      Implicit None
      Character*(*) Dest, Src
      Integer Length
      Character*1 C1
      Integer L1, L2, L3

      L2 = LEN(Src)
      Do L1 = 1,L2
        L3 = L2+1-L1
        C1 = Src(L3:L3)
        If(.NOT.(C1.EQ.' ' .OR. C1.EQ.Char(9))) GoTo 100
      EndDo

  100 Length = L3
      Dest = Src(1:L3)
      End

      Subroutine DGUPCS(Dest, Src)
      Character*(*) Dest, Src
      Integer L1, L2

      L2 = LEN(Src)
      Do L1 = 1,L2
        If((Src(L1:L1).GE.'a' .AND. Src(L1:L1).LE.'z') .OR.
     .   (Ichar(Src(L1:L1)).GE.225 .AND. Ichar(Src(L1:L1)).LE.254)) Then
          Dest(L1:L1) = Char(Ichar(Src(L1:L1))-32)
        Else
          Dest(L1:L1) = Src(L1:L1)
        EndIf
      EndDo
      End

      Integer Function LENOCC(TEXT)
C  A private LENOCC function that discard all characters < 33
C  Björn S. Nilsson, 29-May-1994
C
      Character*(*) Text
      Integer Ilen, I1, I2

      Ilen = Len(Text)
      Do I1 = Ilen, 1, -1
         I2 = I1
         If (Text(I1:I1) .GT. ' ') GoTo 100
      EndDo
      I2 = 0
  100 LENOCC = I2
      End

      Subroutine DGWAIT(Secnds)
      Real Secnds
C Next UNIX specific!
COSF  Integer*8 Itime
      Integer*4 Itime

      Itime = Secnds + 0.7
      Call sleep(Itime)
      Return
      End

      Logical Function And(L1, L2)
C  We have problems with and on HP. From where is it called?
      Logical L1, L2

      And = IAnd(L1, L2)
      Write(*,*) 'If you see this, please report and and problem'//
     & 'to B.S.Nilsson'
      Return
      End

*DK DGETIN
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DGETIN
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C   This routine initialises terminal I/O and sets up a few things that
C   DGETST depends on. Heavily VAX dependent!
C   Modified by B.S. Nilsson, April - June 1989.
C   New event flag scheme introduced on 22-Jan-1991.
C
C   Heavily hacked version to work with X windows. Not so heavily VAX
C   dependent. M. Parsons Jan 1991.
C
C   Now (8-Feb-1993) this version is heavily UNIX-dependent! /BSN
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DU
      COMMON /DUSD2C/NUN6DU,NUN9DU
      PARAMETER (MUN5DU=5,MUN6DU=6,MUN9DU=9)
C
C-------------------End of DALI_CF commons----------------
C
C  The next include is not needed here. However, it serves as a space
C  allocator for the corresponding variables used in the C-routines
C  for DALI_X
C
C     INCLUDE 'DALI_UIS.INC'
C-------------------------------------------------------------DUISCC
      INTEGER MWINDU,MPNWDU,MERSDU
      PARAMETER (MWINDU=6,MPNWDU=13,MERSDU=10)
      INTEGER INDVCM,IDCMDU,IDVDDU,IDWDDU,IDSGDU,IDESDU,NERSDU,
     & IWESDU,LERSDU,IOCLDU
      LOGICAL FUISDU,FERSDU,FPUIDU,FOPSDU,FUIXDU,FNRZDU,FMETDU,FPSWDU
      COMMON /DUISCC/ INDVCM,IDCMDU,IDVDDU(MWINDU),IDWDDU(MWINDU),
     &                FUISDU,
     &                IDSGDU(0:MPNWDU),IDESDU(MERSDU),NERSDU,
     &                IWESDU(MERSDU),LERSDU,
     &                IOCLDU(7,0:MPNWDU),FERSDU,FPUIDU,FOPSDU,FUIXDU,
     &                FNRZDU,FMETDU,FPSWDU
C-------------------------------------------------------------DXPNTR
      INTEGER NXCUDX,MXCUDX
      PARAMETER (MXCUDX=5)
      LOGICAL FUPODX
C
C  POINDX is a 16 bit quantity, but we allocate it here as 32 bit
C  integers with half the amount.
C
      INTEGER POINDX
      INTEGER HSPHDX,HSPVDX
      COMMON /DXPNTR/ NXCUDX,FUPODX(MXCUDX,2),HSPHDX(MXCUDX),
     & HSPVDX(MXCUDX),POINDX(32,MXCUDX)
C-------------------------------------------------------------
      INTEGER WSCRN
      INTEGER MACMX, MACLEVMAX
      PARAMETER (MACMX=500, MACLEVMAX=8)
      CHARACTER TMNAME*20,TMAC*80,MACFIL*80,TMSAVE*20
      INTEGER LSCRN,KMAX,MACLEN,MACLEV,MACSAVE
      LOGICAL FMSAVE
      COMMON /MACCMN/ LSCRN,KMAX,MACLEN(MACMX),MACFIL,TMNAME(MACMX),
     & TMAC(MACMX),MACLEV,MACSAVE(3,MACLEVMAX),FMSAVE(2,MACLEVMAX),
     & TMSAVE(MACLEVMAX)

      COMMON /D0PLAF/ TPLFD0,TWRTD0
      CHARACTER *6 TPLFD0
      CHARACTER *1 TWRTD0

      Integer Nfile/0/
      Character File_Name*12/'DALI.LOG.0'/,Form*6/'(a,i1)'/

      LOGICAL FLGOOB,FLGDOP
      COMMON /OOBCMN/ FLGOOB(0:31),FLGDOP

      INTEGER BUTSTA,KEYBF1
      LOGICAL FBOX
      REAL LOWX,LOWY,PXX,PYY,HPOPOS,VPOPOS
      COMMON /ASTCM1/ BUTSTA,KEYBF1(4),FBOX,LOWX,LOWY,PXX(5),PYY(5),
     & HPOPOS,VPOPOS

      CHARACTER TNUM*16

      LSCRN = 24
      KMAX=0
      MACLEV=0
      MACFIL=' '
      TPLFD0 = 'HP'
      TWRTD0 = CHAR(10)
      Call getenv('LINES',TNUM)
      If(TNUM.NE.' ') THEN
        IL = Lenocc(TNUM)
        READ(TNUM,'(I<IL>)') LSCRN
      EndIf
      WSCRN = 80
      Call getenv('COLUMNS',TNUM)
      If(TNUM.NE.' ') THEN
        IL = Lenocc(TNUM)
        READ(TNUM,'(I<IL>)') WSCRN
      EndIf
C
C Clear the event flags 1 - 7
C
      DO I = 1,7
         CALL DGCLEV(I)
      END DO
C
C Clear the immediate action key flag array
C
      BUTSTA = 0
      DO I=0,31
         FLGOOB(I)=.FALSE.
      END DO
      FLGDOP = .FALSE.
      GO TO 2
CH
CH --------------------------------------------------------------------
CH
      ENTRY DGETCD(NPIC, NZ, NCPC)
CH
CH --------------------------------------------------------------------
CH
      NPIC = 0
      If(File_Name(10:10).EQ.'0') Then
        CALL DWRT('Close DALI.LOG')
      Else
        Call DWRT('Close '//File_Name)
      EndIf
      CLOSE(UNIT=MUN9DU)
      CALL DCONLO
    2 NFile = NFile + 1
      Form(5:5) = '1'
      If(NFile.GT.9) Form(5:5) = '2' 
      If(NFile.GT.99) Form(5:5) = '3'
      Write(File_Name,Form) 'DALI.LOG.', NFile
      OPEN(UNIT=MUN9DU,FILE=File_Name,STATUS='NEW',
     & ERR=2)
      CALL DWRT('Output on file '//File_Name(1:12))
      RETURN

      END

      SUBROUTINE DGETLN(ILNSTR, LCHLIN, LINM)
C  This subrotine reads a text string, converting to uppercase characters.
C  The alternative entry, DGETLM, reads without conversion.
C
C   X version !
C
      IMPLICIT NONE
      CHARACTER*(*) ILNSTR
      INTEGER       LCHLIN,LINMAX,LINM

      INTEGER MUN9DU
      PARAMETER (MUN9DU=9)

      CHARACTER JCHSTR*80
      INTEGER*2 IOSB(4), INT2
      LOGICAL FUCASE

      FUCASE = .TRUE.
      GOTO 100

      ENTRY DGETLM(ILNSTR, LCHLIN, LINM)
      FUCASE = .FALSE.
      CALL DGTLNX_LC

  100 CONTINUE
      CALL DWRT_END(LINM)
      LINMAX=MAX(1,LINM)
      CALL DGTLNX(LINMAX, IOSB, JCHSTR)
      IF((IOSB(1).EQ.1) .AND. (IOSB(2).EQ.0). AND.
     &  (MOD(IOSB(4),256).EQ.1)) THEN
         INT2 = -(IOSB(3)-Z'FF00')
         LCHLIN = INT2
         IF(LCHLIN .LE. 0 .AND. LCHLIN .GE. -31) THEN
            ILNSTR = CHAR(0)
         ELSE
            ILNSTR = CHAR(0)
         END IF
         IF(LCHLIN.EQ.-26) THEN
C   CTRL-Z. 
            WRITE(MUN9DU,'(A)') ' Exit ' ! HP
         END IF
         GO TO 1000
      END IF

      LCHLIN = IOSB(2)
      ILNSTR = JCHSTR(1:LCHLIN)
      WRITE(MUN9DU,'(A)') ILNSTR(1:LCHLIN)       ! HP
 1000 IF(.NOT.FUCASE) CALL DGTLNX_LC
      RETURN

      END

      SUBROUTINE DGMMCB(IH, IV, IHCO, IVCO, NDS, NNS, FBUT, KCHAR)
C  Callback routine for DGMRKM calling dglimn
C
C  Björn S. Nilsson, 14-April-1992
C
CCC   INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
C------------------------------------------------------------------- DO
C             IF MPNPDO IS CHANGED CHANGE ALSO MPNPDS
      PARAMETER (MPNPDO=190)
C       If IDLSDO is changed, it must be changed also in DOPRIN!
      COMMON /DOPR2C/
     1  IDLSDO,IPDDDO,IYDPDO,IPCCDO,IPALDO,  !   1   5 
     1  IPAFDO,IPDFDO,IFRFDO,IPCFDO,IHZPDO,  !   6  10
     1  IPATDO,IPDTDO,IFRTDO,IPCTDO,IFDGDO,  !  10  15
     1  IPPLDO,IPP1DO,IPP2DO,IPFRDO,IPTODO,  !  16  20
     1  IPPUDO,IPSKDO,IFCHDO,IPSCDO,IYNBDO,  !  21  25
     1  IPASDO,IPINDO,IPDADO,IPSHDO,IROMDO,  !  26  30
     1  IBMODO,IPSTDO,IPSLDO,IPSWDO,IHFMDO,  !  31  35
     1  IRDYDO,IPHMDO,IRESDO,IREYDO,IPFSDO,  !  36  40
     1  IDUMDO,IMULDO,IRDHDO,IPL1DO,IPL2DO,  !  41  45
     1  IELADO,IEABDO,ICUTDO,IHDUDO,IHNIDO,  !  46  50
     1  IHNTDO,IPCMDO,IHP0DO,IHDZDO,IHDRDO,  !  51  55
     1  IHF1DO,IHF2DO,IHT1DO,IHT2DO,IHSTDO,  !  56  60
     1  IHSHDO,IHDEDO,IPZ0DO,IHSEDO,IHCNDO,  !  61  65
     1  IHCHDO,IHLIDO,IHCEDO,IHCLDO,IRADDO,  !  66  70
     1  IRTCDO,IRDFDO,IRDZDO,IRVDDO,IRITDO,  !  71  75
     1  IHEODO,IROSDO,IRDUDO,IYMWDO,IYZHDO,  !  76  80
     1  IYZWDO,IYZFDO,IYZTDO,IYZDDO,IYZMDO,  !  81  85
     1  IYZPDO,IYZSDO,IYZCDO,IYZIDO,IETYDO,  !  86  90
     1  IENBDO,IENDDO,IESZDO,IEDFDO,IEDTDO,  !  91  95
     1  IHTBDO,IHELDO,IHSVDO,IRB1DO,IRB2DO,  !  96 100
     1  IDNSDO,IWQWDO,IYALDO,IRB3DO,IRBPDO,  ! 101 105
     1  ILDZDO,ILDLDO,ILNTDO,ILDIDO,ILFIDO,  ! 106 110
     1  IAHMDO,IADHDO,IAVMDO,IADVDO,IAALDO,  ! 111 115
     1  IAHVDO,IAVVDO,IATHDO,IATVDO,IADUDO,  ! 116 120
     1  I121DO(5),                           ! 121 125
     1  I126DO,I127DO,I128DO,I129DO,ISFODO,  ! 126 130
     1  ISL1DO,ISL2DO,ISSXDO,ISSRDO,ISSHDO,  ! 131 135
     1  ISSFDO,ISFHDO,ISFMDO,ISFRDO,ISMIDO,  ! 136 140
     1  ISMADO,ISCLDO,ISNRDO,ISNPDO,ISMCDO,  ! 141 145
     1  ISCGDO,ISCHDO,ISPRDO,ISMODO,ISZZDO,  ! 146 150
     1  I151DO(10),
     1  I161DO(10),
     1  I171DO(10),
     1  I181DO(10)
      EQUIVALENCE(IPMIDO,IPFRDO),(IPMADO,IPTODO)
C------------------------------------------------------------------- DP
      PARAMETER (MAXPDP=120)
      COMMON /DPICT/ TPICDP(-10:MAXPDP)
      CHARACTER*2 TPICDP
C------------------------------------------------------------------- DS
      PARAMETER (MPARDS=12,MVIRDS=-4,MPNPDS=190)
      COMMON /DSTRC1/ IAUTDS,ISTODS(8,MVIRDS:MPARDS),
     &  IPICDS,IZOMDS,IAREDS
      COMMON /DSTRC2/PSTODS(2,MPNPDS,MVIRDS:MPARDS)
      COMMON /DSTRC2/TVRUDS(6,MVIRDS:MPARDS)
C------------------------------------------------------------------- DU
      PARAMETER (MPNPDU=60)
      REAL DACUDU
      COMMON /DUSDAC/ NUNIDU,
     1 WAITDU,ECCHDU,BOXSDU,ECSQDU,HCSQDU,
     1 HIMXDU,RESCDU,SLOWDU,WISUDU,SETTDU,
     1 USLVDU,SYLRDU,SDATDU,RZDHDU,ROTADU,
     1 SCMMDU,CLMMDU,CUMMDU,WIMIDU,CCMIDU,
     1 ECMIDU,HCMIDU,SHTFDU(2)    ,FTDZDU,
     1 FRNLDU,DACUDU,SHECDU,SHHCDU,DRLIDU,
     1 SQLCDU,CHLCDU,HEADDU,VRDZDU,CHHCDU,
     1 AREADU,DFWIDU(0:1)  ,DFLGDU,PBUGDU,
     1 CHTFDU,RNCLDU,PR43DU,PR44DU,PR45DU,
     1 PR46DU(4),                  PR50DU,
     1 PR51DU(4),                  PR55DU,
     1 PR56DU(4),                  PR60DU
      DIMENSION PARADU(MPNPDU)
      EQUIVALENCE (PARADU,WAITDU)

      INTEGER IH, IV, IHCO(*), IVCO(*), NDS(2,*), NNS
      LOGICAL FBUT(4)
      CHARACTER*(*) KCHAR

      INTEGER IMRKC, IAREA
      REAL PHI, THETA
      COMMON /MRKCM/ IMRKC, IAREA, PHI, THETA

      CHARACTER TWCU(12)*2, TW*2
      INTEGER NWCU, IAR, IARL, IARH, IWCU(12), NPIC, IHC, IVC, IW, ILIM,
     & I, IHN, IVN, NCNT
      LOGICAL FIN
      REAL SCEX, SCSC, SLOW, SHIGH, PHI1, THET1, RDEG, DH, DV, DHV2,
     & COST, SINT, COSF, SINF, PHSTP, PHSTR, PH, H(2), V(2),
     & HH(2), VV(2), FIMID, FIYZ, CFIYZ, SFIYZ, RYZ, ZYZ, XYZ, YYZ,
     & Z, R, X1, Y1, Z1, FI3D, CF, SF, TE3D, CT, ST, G3D, CG, SG,
     & X2, Y2, Z3
      Character Window_Id(0:13)*1/
     & 'W','1','2','3','4','5','6','U','D','L','M','R','S','A'/
C  TPC dimensions.
      REAL TPCT1/38.9/,TPCT2/141.1/,TPCR0/177.48/,TPCZ1/219.96/

Cnow  DATA SCEX/0./       ! Exponent for scaling factor.
Cnow  DATA SCSC/0.10/     ! Overall scaling factor for cursor movement.
      DATA SCEX/0.5/
Cnow  DATA SCSC/0.02/
Cnow  DATA SLOW/1.5/
Cnow  DATA SHIGH/20./

C
C  Do not react on keyboard input.
C
      IF(.NOT.FBUT(4)) RETURN
C
C  Entering first time?
C
      IF(IMRKC.GE.0) GOTO 200

      IMRKC = 1
      PHI1  = PHI
      THET1 = THETA
      SCSC  = SCMMDU
      SLOW  = CLMMDU
      SHIGH = CUMMDU
      RDEG  = ASIN(1.0)/90.
      NWCU  = 0
C
C  Make a list of windows.
C
      IARL = 0
      IARH = MPARDS
      IF(IAREA.GE.0) THEN
         IARL = IAREA
         IARH = IAREA
      END IF
      DO IAR = IARL,IARH
         IF(ISTODS(4,IAR).GT.0) THEN
            NWCU       = NWCU+1
            IWCU(NWCU) = IAR
            NPIC       = ISTODS(5,IAR)
            TWCU(NWCU)=TPICDP(NPIC)
            IF(TWCU(NWCU).EQ.'XY') TWCU(NWCU) = 'YX'
            IF(TWCU(NWCU).EQ.'TF') TWCU(NWCU) = 'FT'
            IF(TWCU(NWCU).EQ.'ZR') TWCU(NWCU) = 'RZ'
            IF(TWCU(NWCU).EQ.'ZF') TWCU(NWCU) = 'FZ'
            IF(TWCU(NWCU).EQ.'RF') TWCU(NWCU) = 'FR'
            IF(TWCU(NWCU).EQ.'ZY') TWCU(NWCU) = 'YZ'
            IF(TWCU(NWCU).EQ.'RO') TWCU(NWCU) = '3D'
            TW = TWCU(NWCU)
            IF(.NOT.(TW.EQ.'FT' .OR. TW.EQ.'YX' .OR. TW.EQ.'EC'
     &          .OR. TW.EQ.'RZ' .OR. TW.EQ.'HZ' .OR. TW.EQ.'FZ'
     &          .OR. TW.EQ.'FR' .OR. TW.EQ.'YZ' .OR. TW.EQ.'3D')) THEN
              CALL DWRT(' Markers not coded for projection '//TW//
     &         ' (window W'//Window_Id(IAR)//').')
            END IF
         END IF
      END DO
      IF(NWCU .LE. 0) GOTO 999
      WRITE(*,1000) PHI, THETA
 1000 FORMAT('      phi=',F4.0,' theta=',F4.0)
      IHC = 0.5*(HMINDG(0)+HHGHDG(0))
      IVC = 0.5*(VMINDG(0)+VHGHDG(0))

C
C  Calculate new (PHI,THETA) position and coordinates for markers.
C
  200 DH   = IH - IHC
      DV   = IV - IVC
      DHV2 = SCSC
      IF(SCEX .GT. 0.)
     & DHV2 = SCSC*MAX(SLOW,MIN(SHIGH,(DH*DH+DV*DV)**SCEX))
      IF(FBUT(1)) PHI   = PHI   + DV*DHV2
      IF(FBUT(2)) THETA = THETA - DH*DHV2
      IF(.NOT.FBUT(3)) THEN
        PHI   = PHI1
        THETA = THET1
      END IF
C
C   New theta and phi values.
C
      THETA = MIN(180.,MAX(THETA,0.))
      PHI   = MOD(PHI+36000.,360.)
      WRITE(*,1001) PHI, THETA
 1001 FORMAT('+     phi=',F4.0,' theta=',F4.0)

      COST = COS(THETA*RDEG)
      SINT = SIN(THETA*RDEG)
      COSF = COS(PHI*RDEG)
      SINF = SIN(PHI*RDEG)

C
C   Loop over the different windows. Display a cross marker for Theta-Phi
C  projection and a line for the others.
C
      NNS  = 0
      NCNT = 0
      DO 300 IW = 1,NWCU
        IAR = IWCU(IW)
        CALL DQSET(IAR,0.,0.)
        IF(TWCU(IW).EQ.'FT') THEN
          IF(IAREA.EQ.IAR) THEN
C  Only PHI-lines if this window is the only selected.
            ILIM  = 4
            PHSTP = 180.
            PHSTR = MOD(PHI+36000.,180.) - 180.
            GOTO 280
          END IF
          DO I = 1,3
            PH = PHI+(I-2)*360.
            CALL DQPOC(-THETA, PH, H, V, FIN)
            IF(FIN) THEN
              IHN             = H(1)
              IVN             = V(1)
              NDS(1, NNS + 1) = NCNT + 1
              NDS(2, NNS + 1) = 2
              NDS(1, NNS + 2) = NCNT + 3
              NDS(2, NNS + 2) = 2
              NNS             = NNS + 2
              IHCO(NCNT + 1)  = IHN - 7
              IVCO(NCNT + 1)  = IVN
              IHCO(NCNT + 2)  = IHN + 7
              IVCO(NCNT + 2)  = IVN
              IHCO(NCNT + 3)  = IHN
              IVCO(NCNT + 3)  = IVN - 7
              IHCO(NCNT + 4)  = IHN
              IVCO(NCNT + 4)  = IVN + 7
              NCNT            = NCNT + 4
            END IF
          END DO
        ELSE
          IF(TWCU(IW).EQ.'YX' .OR. TWCU(IW).EQ.'EC') THEN
            IF(IAR.EQ.IAREA) THEN
               HH(1) =-10000.*COSF
               VV(1) =-10000.*SINF
            ELSE
               HH(1) = 0.
               VV(1) = 0.
            END IF
            HH(2) = 10000.*COSF
            VV(2) = 10000.*SINF
          ELSE IF(TWCU(IW).EQ.'RZ' .OR. TWCU(IW).EQ.'HZ') THEN
            HH(1) = 0.
            VV(1) = 0.
            HH(2) = 10000.*COST
            VV(2) = 10000.*SINT
            IF(PSTODS(2,IPATDO,IAR).NE.0) THEN
               FIMID = PSTODS(1,IPAFDO,IAR)
C              IF(.NOT.(PHI.GE.FIMID-90. .AND. PHI.LE.FIMID+90.))
               IF((PHI.GT.FIMID-270..AND.PHI.LE.FIMID- 90.).OR.
     &            (PHI.GT.FIMID+ 90..AND.PHI.LE.FIMID+270.)) 
     &          VV(2) =-VV(2)
            END IF
          ELSE IF(TWCU(IW).EQ.'FZ' .OR. TWCU(IW).EQ.'FR') THEN
            ILIM  = 3
            PHSTP = 360.
            PHSTR = PHI - 360.
            GOTO 280
          ELSE IF(TWCU(IW).EQ.'YZ') THEN
            FIYZ  = PSTODS(1,IPAFDO,IAR)*RDEG
            CFIYZ = COS(FIYZ)
            SFIYZ = SIN(FIYZ)
            RYZ   = 10000.*SINT
            ZYZ   = 10000.*COST
            XYZ   = RYZ*COSF
            YYZ   = RYZ*SINF
            HH(1) = 0.
            VV(1) = 0.
            HH(2) = ZYZ
            VV(2) =-SFIYZ*XYZ+CFIYZ*YYZ
          ELSE IF(TWCU(IW).EQ.'3D') THEN
            IF(THETA.LT.TPCT1) THEN
              Z = TPCZ1
              R = Z*SINT/COST
            ELSE IF(THETA.LE.TPCT2) THEN
              R = TPCR0
              Z = R*COST/SINT
            ELSE
              Z =-TPCZ1
              R = Z*SINT/COST
            END IF
            X1    = R*COSF
            Y1    = R*SINF
            Z1    = Z
            FI3D  = PSTODS(1,IPAFDO,IAR)*RDEG
            CF    = COS(FI3D)
            SF    = SIN(FI3D)
            TE3D  = (90.-PSTODS(1,IPATDO,IAR))*RDEG
            CT    = COS(TE3D)
            ST    = SIN(TE3D)
            G3D   = PSTODS(1,IPALDO,IAR)*RDEG
            CG    = COS(G3D)
            SG    = SIN(G3D)
            X2    = CF*X1+SF*Y1
            Y2    =-SF*X1+CF*Y1
            Z3    =-ST*X2+CT*Z1
            HH(1) = 0.
            VV(1) = 0.
            HH(2) = CT*X2+ST*Z1
            VV(2) = CG*Y2+SG*Z3
          ELSE
            GOTO 300
          END IF
          CALL DQPOC(HH(1), VV(1), H(1), V(1), FIN)
          CALL DQPOC(HH(2), VV(2), H(2), V(2), FIN)
          CALL DQCLP( H(1),  V(1), H(2), V(2), FIN)
          IF(FIN) THEN
            NDS(1, NNS + 1) = NCNT + 1
            NDS(2, NNS + 1) = 2
            NNS             = NNS + 1
            IHCO(NCNT + 1)  = H(1)
            IVCO(NCNT + 1)  = V(1)
            IHCO(NCNT + 2)  = H(2)
            IVCO(NCNT + 2)  = V(2)
            NCNT            = NCNT + 2
          END IF
          GOTO 300

  280     DO I = 1,ILIM
            HH(1) =-10000.
            VV(1) = PHSTR+PHSTP*(I-1)
            HH(2) = 10000.
            VV(2) = PHSTR+PHSTP*(I-1)
            CALL DQPOC(HH(1), VV(1), H(1), V(1), FIN)
            CALL DQPOC(HH(2), VV(2), H(2), V(2), FIN)
            CALL DQCLP( H(1),  V(1), H(2), V(2), FIN)
            IF(FIN) THEN
              NDS(1, NNS + 1) = NCNT + 1
              NDS(2, NNS + 1) = 2
              NNS             = NNS + 1
              IHCO(NCNT + 1)  = H(1)
              IVCO(NCNT + 1)  = V(1)
              IHCO(NCNT + 2)  = H(2)
              IVCO(NCNT + 2)  = V(2)
              NCNT            = NCNT + 2
            END IF
          END DO
        END IF
  300 CONTINUE

      IH = IHC
      IV = IVC
      RETURN

C
C   Error return.
C
  999 CALL DWRT('Error return from DGMMCB.')
      NNS = -1
      RETURN
      END

*DK DGMRKM
      SUBROUTINE DGMRKM(PHI0, THET0, IARIN)
C ---------------------------------------------------------------------
C
C     Created by B.S. Nilsson                 April 1990 and April 1992
C
C ---------------------------------------------------------------------
C This routine runs a process, where the theta/phi position is read from
C an (almost) invisible pointer position and then displayed in all
C selected windows.
C  If IAREA < 0 all displayed windows are selected. Otherwise only the
C one corresponding to IAREA is chosen. Note that IARIN == IAREA here.
C  The markers can be switched OFF/ON temporarily with the space bar.
C The process is terminated by a Return.
C  Movement can be limited to vertical (Phi) or horizontal (Theta) direc-
C tion by depressing M2 or M1 resp. If M3 is depressed the markers will
C return to the original Phi/Theta position.
C
C  This version is for X. It calls dglinm and dglinf, resp. Auxilliary
C call-back routine needed: dgmmcb 
C
C Björn S. Nilsson, 14-April-1992
C
      EXTERNAL DGMMCB

      REAL PHI0, THET0
      INTEGER IARIN
      INTEGER IHC, IVC, IH(72), IV(72), NDS(2,36), NNS
      REAL RH(72), RV(72)

      INTEGER IMRKC
      REAL PHI, THETA
      COMMON /MRKCM/ IMRKC, IAREA, PHI, THETA

      IHC   = -1
      IVC   = -1
      IMRKC = -1
      PHI   = PHI0
      THETA = THET0
      IAREA = IARIN

      Call DGLINM(IHC, IVC, RH, RV, IH, IV, 0, DGMMCB, .TRUE., NDS, NNS)

      WRITE(*,1000)
 1000 FORMAT(' ',T51,'..:',$)
      PHI0 = PHI
      THET0 = THETA

      RETURN

      ENTRY DGMRKF(LEVL)
C
C   Fix the markers at the last position with colour determined by LEV.
C

      CALL DGLINF(RH, RV, IH, IV, NDS, NNS, LEVL)
      RETURN

      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++ DW_GETLN
CH
      SUBROUTINE DW_GETLN(T,LNIN,LMAX)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      CHARACTER *(*) T
      LM=MIN(3,LMAX)
      CALL DGTLNX_LC
      CALL DGETLN(T,LNIN,LMAX)
      CALL DGTLNX_LC
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++ DW_ERROR_HANDLING_SETUP
CH
      SUBROUTINE DW_ERROR_HANDLING_SETUP
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C
C  Initialise error handler, error messages are suppressed on the
C  terminal, however a notification is sent to DALI.LOG
C  No message to unit 9, since it is NOT yet assigned to DALI.LOG
CVMS  IF (LERON) THEN
CVMS     CALL DWRT_SETUP('TERMINAL=OFF')
CVMS     CALL DWRT('DALI> Error handler established.')
CVMS     CALL DWRT_SETUP('TERMINAL=LAST')
CVMS     CALL LIB$ESTABLISH(DW_ERRLG)
CVMS     CALL DW_ERROR(IRFLG,IRTRC)
CVMS  END IF
      RETURN
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++ DW_ERROR_HANDLING_STOP
CH
      ENTRY DW_ERROR_HANDLING_STOP
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH  
CVMS  IF (LERON) THEN
CVMS    CALL DWRT_SETUP('TERMINAL=OFF')
CVMS    CALL DW_ERROR(-1,0)
CVMS    CALL DWRT('DALI> Closing error handler ....')
CVMS    CALL DWRT_SETUP('TERMINAL=ON')
CVMS  ENDIF
      END
