      Program Dali_Conv
C
C   This program  looks through FORTRAN-source-code-files assigned to
C   the logical unit number 21, extracts all identifiers matching the
C   ALEPH-common-naming-conventions  (6 characters,  for DALI-commons
C   5th character = 'D')  and copies the common-blocks they originate
C   from in file  DALI_CF.FOR  into either the subroutine or function
C   they were found in.  The INCLUDE 'DALI_CF.FOR'  statement is com-
C   mented out.
C
C   Created  18-SEP-1989   R.Vogl
C
C   Björn S. Nilsson added code so the program runs as a foreign com-
C   mand on 4-Feb-1990. Checks on array boundaries also added.
C
C   Conditional inclusion added on 3-May-1993 with simple C-like
C   constructs:
C   #ifdef ident  (or #ifndef ident)
C   ...
C   #else
C   ...
C   #endif comment
C   The operators must be lower case while ident is case-insensitive
C
C   This is a Linux version first prepared on 25-Sep-1995.
C   f77 -fdollar-ok -fbackslash -o ic ic.f
C

      IMPLICIT NONE

C-------------------------------------------------------------------------
C
C  Parameters
C
C   max_lin_rout : max. number of lines in a subroutine
C   max_lin_dcf  : max. number of lines in file DALI_CF.INC
C   max_com_bl   : max. number of common UNITS in file DALI_CF.INC
C
C  IMPORTANT: These parameters are chosen in order to satisfy current
C             requirements!
C             It is possible that the limits given here will once be
C             too low! So adjust these PARAMETERs here AND in all sub-
C             routines and functions when needed.
C

      INTEGER max_lin_rout, max_lin_dcf, max_com_bl
      PARAMETER (max_lin_rout = 3000)
      PARAMETER (max_lin_dcf  =  500)
      PARAMETER (max_com_bl   =  200)

C-------------------------------------------------------------------------
C
C  Variable declarations
C

C
C  file and log.unit variables
C

      INTEGER lu_message, lu_dali_cf, lu_file_in, lu_file_out
      COMMON / log_unit / lu_message, lu_dali_cf, lu_file_in,
     &                    lu_file_out
      DATA lu_message  /  6 /
      DATA lu_dali_cf  / 20 /
      DATA lu_file_in  / 21 /
      DATA lu_file_out / 22 /
      INTEGER iargc, Status, I2
      INTEGER Ilen
      CHARACTER Arg_str*200, File1*100, File2*100, File3*100

C
C  general variables
C

      CHARACTER*80 text_dali_cf, text_rout
      CHARACTER*2  com_bl_id

      INTEGER rows_dali_cf, rows_rout, Inline, IPstat
      DATA IPstat/0/
      INTEGER cols_dali_cf, cols_rout
      INTEGER numb_com_bl , com_bl_lin

      COMMON /text_common/ text_dali_cf(max_lin_dcf),
     &                     text_rout  (max_lin_rout),
     &                     com_bl_id  (max_com_bl)

      LOGICAL LBlock
      COMMON /int_common/ rows_dali_cf, rows_rout,
     &             cols_dali_cf(max_lin_dcf), cols_rout(max_lin_rout),
     &             com_bl_lin (max_com_bl), numb_com_bl,
     &             LBlock

      LOGICAL rout_found, end_found, prog_found
      LOGICAL LInclude, CheckIfdef
      INTEGER Lenocc

      CHARACTER*80 a

C
C   Get filenames from command line.
C
      If (iargc() .NE. 3) Then
        Write(*,'(a)') ' Incorrect program call.'
        IPstat = 1
        Goto 999
      Endif
      Call GetArg(1,File1)
      Call GetArg(2,File2)
      Call GetArg(3,File3)

C--------------------------------------------------------------------------
C
C  Formats
C

 1000 Format(A)

C--------------------------------------------------------------------------
C
C  Open files to be processed
C

      Open (unit=lu_dali_cf, status='OLD', File = File1,
     &      Iostat=Status, ERR= 910)

      rows_dali_cf = 0
      numb_com_bl  = 0
      LInclude = .True.

  100 Continue                          ! next line of DALI_CF.INC

      rows_dali_cf = rows_dali_cf + 1
      If (rows_dali_cf .GT. max_lin_dcf) Goto 950

      Read(lu_dali_cf, 1000, END = 199)
     &     text_dali_cf(rows_dali_cf)
      cols_dali_cf(rows_dali_cf) = Lenocc(text_dali_cf(rows_dali_cf))
      a = text_dali_cf(rows_dali_cf)
      If (a(1:1) .eq. '#') Then
        LInclude = CheckIfdef(a, LInclude, rows_dali_cf)
        rows_dali_cf = rows_dali_cf - 1
        GoTo 100
      ElseIf (.NOT. LInclude) Then
        rows_dali_cf = rows_dali_cf - 1
        GoTo 100
      EndIf
      If ((a(1:3) .EQ. 'C--') .OR. (a(1:3) .EQ. 'c--')) Then
        numb_com_bl = numb_com_bl + 1
        if(numb_com_bl .GT. max_com_bl) Goto 940

        com_bl_lin(numb_com_bl) = rows_dali_cf
        com_bl_id (numb_com_bl) = a(70:71)
       Endif
      Goto 100

  199 Continue                          ! eof_dali_cf

      Open (unit=lu_file_in, status='OLD', File = File2,
     &       err = 920)
      Open (unit=lu_file_out, File = File3, err = 930)
      LInclude = .True.
      Inline = 0


  130 Continue                          ! read next routine

      rows_rout  = 0
      rout_found = .false.
      LBlock     = .false.
      end_found  = .false.
      prog_found = .false.

  140 Continue                          ! read next line

      rows_rout = rows_rout + 1
      If (rows_rout .GT. max_lin_rout) Goto 960

      Read(lu_file_in, 1000, END = 299)
     &     text_rout(rows_rout)
      cols_rout(rows_rout) = Lenocc(text_rout(rows_rout))
      Inline = Inline + 1

      a = text_rout(rows_rout)
      If (a(1:1) .eq. '#') Then
        LInclude = CheckIfdef(a, LInclude, Inline)
        rows_rout = rows_rout - 1
        GoTo 140
      ElseIf (.NOT. LInclude) Then
        rows_rout = rows_rout - 1
        GoTo 140
      EndIf

      If ((a(1:1) .NE. 'C' .AND. a(1:1) .NE. 'c' .AND.
     & a(1:1) .NE. '*')) Then

        rout_found = (((index(a,'SUBROUTINE').NE. 0)
     &          .OR.   (index(a,'BLOCK DATA').NE. 0)
     &          .OR.   (index(a,'FUNCTION')  .NE. 0))
     &          .OR.    rout_found)

        LBlock = (index(a,'BLOCK DATA').NE.0) .OR. LBlock

        end_found  = (a(7:12) .eq. 'END   ')   ! END always at col. 7

        prog_found = ((index(a,'PROGRAM').ne.0) .OR. prog_found)

       Endif

      If (end_found) Then

        If (prog_found) Then          ! Do not process main program
          Call Write_noprocess
         Elseif (rout_found) Then
          Call process_routine
         Else
          Write(lu_message, *)
     &      ' Syntactical error encountered in source file !'
          Write(lu_message, *)
     &      ' ---> END without routine-declaration ! '
          IPstat = 1
          Goto 999
         Endif

       Else

        Goto 140

       Endif

      Goto 130

  299 Continue                          ! eof_file_in

      If ((rout_found) .AND. (.NOT. end_found)) Then
        Write(lu_message, *)
     &    ' Syntactical error encountered in source file !'
        Write(lu_message, *)
     &    ' ---> no END found !'
        IPstat = 1
        Goto 999
       Else
        Goto 999
       Endif

C------------------------------------------------------------------------
C
C  Errors opening the files
C

  910 Call STR$Trim (File1, File1, Ilen)
      Write(lu_message, '(a,a,a,i3)') ' Error opening include file "',
     & File1(1:Ilen), '", Status=', Status
      IPstat = 1
      Goto 999

  920 Call STR$Trim (File2, File2, Ilen)
      Write(lu_message, '(a,a,a,i3)') ' Error opening input file "',
     & File2(1:Ilen), '", Status=', Status
      IPstat = 1
      Goto 999

  930 Call STR$Trim (File3, File3, Ilen)
      Write(lu_message, '(a,a,a,i3)')' Error opening new output '//
     & 'file "', File3(1:Ilen), '", Status=', Status
      IPstat = 1
      Goto 999

C-------------------------------------------------------------------------
C
C  Dimension overflows
C

  940 Write(lu_message, '(a,a,i4,a)') ' IC limitation:',
     & ' Increase max_com_bl (', max_com_bl, ' at present).'
      IPstat = 1
      Goto 999

  950 Write(lu_message, '(a,a,i4,a)') ' IC limitation:',
     & ' Increase max_lin_dcf (', max_lin_dcf, ' at present).'
      IPstat = 1
      Goto 999

  960 Write(lu_message, '(a,a,i5,a)') ' IC limitation:',
     & ' Increase max_lin_rout (', max_lin_rout, ' at present).'
      IPstat = 1
      Goto 999

C-------------------------------------------------------------------------
C
C  exit_prog
C
  999 Continue

      Close (unit = lu_dali_cf)
      Close (unit = lu_file_in)
      Close (unit = lu_file_out)

      Call Exit(IPstat)

      End

C######################################################################
C
      SUBROUTINE Write_noprocess
C
C-----------------------------------------
C
C   Author   :- R.Vogl                22-SEP-1989
C
C=========================================
C
C   Purpose   :
C   Inputs    :
C   Outputs   :
C
C=========================================
C +
C Declarations.
C -
      IMPLICIT NONE

      INTEGER max_lin_rout, max_lin_dcf, max_com_bl
      PARAMETER (max_lin_rout = 3000)
      PARAMETER (max_lin_dcf  =  500)
      PARAMETER (max_com_bl   =  200)

      INTEGER lu_message, lu_dali_cf, lu_file_in, lu_file_out
      COMMON / log_unit / lu_message, lu_dali_cf, lu_file_in,
     &                    lu_file_out

      CHARACTER*80 text_dali_cf, text_rout
      CHARACTER*2  com_bl_id

      INTEGER rows_dali_cf, rows_rout
      INTEGER cols_dali_cf, cols_rout
      INTEGER numb_com_bl , com_bl_lin

      COMMON /text_common/ text_dali_cf(max_lin_dcf),
     &                     text_rout  (max_lin_rout),
     &                     com_bl_id  (max_com_bl)

      LOGICAL LBlock
      COMMON /int_common/ rows_dali_cf, rows_rout,
     &             cols_dali_cf(max_lin_dcf), cols_rout(max_lin_rout),
     &             com_bl_lin (max_com_bl), numb_com_bl,
     &             LBlock

      CHARACTER*80 a

      INTEGER i

C-----------------------------------------------------------------------
C
C  Format
C

 1200 Format(A)

C-----------------------------------------------------------------------
C
C  Write main-program block without processing
C

      Write(lu_file_out, 1200)
     &  'C-------------------- BEGIN of MAIN-program -----------------'
      Write(lu_file_out, 1200) 'C'
      Write(lu_file_out, 1200) 'C'


      Do i=1, rows_rout

        a = text_rout(i)
        Write(lu_file_out,1200) a(1:cols_rout(i))

       Enddo

      Write(lu_file_out, 1200) 'C'
      Write(lu_file_out, 1200) 'C'
      Write(lu_file_out, 1200)
     &  'C---------------------- END of MAIN-program ----------------'


      END
C
C  end of Write_noprocess
C


C######################################################################
C
      SUBROUTINE process_routine
C
C-----------------------------------------
C
C   Author   :- R.Vogl                17-SEP-1989
C
C=========================================
C
C   Purpose   :
C   Inputs    :
C   Outputs   :
C
C=========================================
C +
C Declarations.
C -
      IMPLICIT NONE

      INTEGER lu_message, lu_dali_cf, lu_file_in, lu_file_out
      COMMON / log_unit / lu_message, lu_dali_cf, lu_file_in,
     &                    lu_file_out

      INTEGER max_lin_rout, max_lin_dcf, max_com_bl
      PARAMETER (max_lin_rout = 3000)
      PARAMETER (max_lin_dcf  =  500)
      PARAMETER (max_com_bl   =  200)

      INTEGER lenid
      INTEGER max_ident
      PARAMETER (lenid = 6)
      PARAMETER (max_ident = 2000)


      CHARACTER*80 text_dali_cf, text_rout
      CHARACTER*2  com_bl_id

      INTEGER rows_dali_cf, rows_rout
      INTEGER cols_dali_cf, cols_rout
      INTEGER numb_com_bl , com_bl_lin

      COMMON /text_common/ text_dali_cf(max_lin_dcf),
     &                     text_rout  (max_lin_rout),
     &                     com_bl_id  (max_com_bl)

      LOGICAL LBlock
      COMMON /int_common/ rows_dali_cf, rows_rout,
     &             cols_dali_cf(max_lin_dcf), cols_rout(max_lin_rout),
     &             com_bl_lin (max_com_bl), numb_com_bl,
     &             LBlock

      INTEGER i,j,k,l

      LOGICAL   new_identifier, contains_ident, yet_extracted

      CHARACTER*80 a
      CHARACTER*6  xx
      CHARACTER*5  yy

      COMMON /ident_common/ ident_number, ident_found(max_ident)
      CHARACTER*6  ident_found
      INTEGER      ident_number

      COMMON /extract_common/ extr_bl(max_com_bl), numb_extr_bl
      INTEGER    extr_bl, numb_extr_bl

C-------------------------------------------------------------------------
C  Format
C

 1100 Format(A)

C-------------------------------------------------------------------------
C  Extract common-identifiers from routine
C
      ident_number = 0

      Do i=1, rows_rout

        a = text_rout(i)
        l = cols_rout(i)
        If (.NOT. (l .EQ. 0 .OR. a(1:1) .EQ. 'C' .OR.
     &             a(1:1) .EQ. 'c' .OR. a(1:1) .EQ. '*')) Then

          k=0
          Do j=1, cols_rout(i)
            If ((a(j:j).ge.'A' .AND. a(j:j).le.'Z') .OR.
     &          (a(j:j).ge.'0' .AND. a(j:j).le.'9' .AND. k.GT.0)) Then

              k=k+1
              If (k .EQ. lenid) Then
                xx=a(j-lenid+1:j)
                yy=a(j-lenid-4:j-lenid)
                If (new_identifier(yy,xx)) Then
                  ident_number = ident_number + 1
                  If (ident_number .GT. max_ident) Then
                    Write(*, '(a,i4,a)') ' Increase max_ident (',
     &               max_ident, ' at present).'
                    Call exit(1)
                   Endif
                  ident_found(ident_number) = xx
                 Endif
                k = 0
               Endif
             Else
              k = 0
             Endif
           Enddo

         Endif

       Enddo
C------------------------------------------------------------------------
C  Extract corresponding common-blocks from DALI_CF.INC
C

      numb_extr_bl = 0

      Do i=1, ident_number

        Do j=1, numb_com_bl

          If (ident_found(i)(5:6) .EQ. com_bl_id(j)) Then
            If ((contains_ident(j,ident_found(i))) .AND.
     &          (.NOT. yet_extracted(j)))  Then
              numb_extr_bl = numb_extr_bl + 1
              If (numb_extr_bl .GT. max_com_bl) Then
                Write(*, '(a,i4,a)') ' Increase max_com_bl (',
     &           max_com_bl, ' at present).'
                Call exit(1)
               Endif
              extr_bl(numb_extr_bl) = j
              Goto 100
             Endif
           Endif

         Enddo

  100   Continue

       Enddo

C-------------------------------------------------------------------------
C  Write modified fortran-source-file
C

      Do i=1, rows_rout

        a = text_rout(i)
        If ((index(a,'INCLUDE').ne.0) .AND.
     &      (index(a,'DALI_CF.INC') .NE. 0) .AND.
     &      (a(1:1) .NE. 'C' .AND. a(1:1) .NE. 'c' .AND.
     &       a(1:1) .NE. '*')) Then

          a(1:1) = 'C'
          Write(lu_file_out,1100) a(1:cols_rout(i))

          Do j=1, numb_extr_bl
            Do k= com_bl_lin(extr_bl(j)), com_bl_lin(extr_bl(j)+1)-1
              a = text_dali_cf(k)
              Write(lu_file_out, 1100) a(1:cols_dali_cf(k))
             Enddo
           Enddo

          Write(lu_file_out, 1100) 'C'
          Write(lu_file_out, 1100)
     &      'C-------------------End of DALI_CF commons----------------'
C
C  Add EXTERNAL declarations for SIND, COSD etc. which are not yet
C  implemented in g77 / 7-Oct-1997
C
          If (.NOT.LBlock) Write(lu_file_out, 1100)
     &      '      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D'//
     &      '! Not implemented in g77'

         Else

          Write(lu_file_out,1100) a(1:cols_rout(i))

         Endif

       Enddo

      END
C
C  End of process_routine
C


C######################################################################
C
      LOGICAL FUNCTION contains_ident (j, c)
C
C-----------------------------------------
C
C   Author   :- R.Vogl                17-SEP-1989
C
C=========================================
C
C   Purpose   :
C   Returned value  :
C   Inputs    :
C   Outputs   :
C
C=========================================
C +
C Declarations.
C -
      IMPLICIT NONE

      INTEGER     j
      CHARACTER*(*) c

      INTEGER max_lin_rout, max_lin_dcf, max_com_bl
      PARAMETER (max_lin_rout = 3000)
      PARAMETER (max_lin_dcf  =  500)
      PARAMETER (max_com_bl   =  200)

      CHARACTER*80 text_dali_cf, text_rout
      CHARACTER*2  com_bl_id

      INTEGER rows_dali_cf, rows_rout
      INTEGER cols_dali_cf, cols_rout
      INTEGER numb_com_bl , com_bl_lin

      COMMON /text_common/ text_dali_cf(max_lin_dcf),
     &                     text_rout  (max_lin_rout),
     &                     com_bl_id  (max_com_bl)

      LOGICAL LBlock
      COMMON /int_common/ rows_dali_cf, rows_rout,
     &             cols_dali_cf(max_lin_dcf), cols_rout(max_lin_rout),
     &             com_bl_lin (max_com_bl), numb_com_bl,
     &             LBlock



      LOGICAL  flag_cont
      CHARACTER*80 a
      INTEGER i,l

      flag_cont = .false.

      Do i=com_bl_lin(j), com_bl_lin(j+1)-1
        a = text_dali_cf(i)
        If (a(1:1) .NE. 'C' .AND. a(1:1) .NE. 'c' .AND. a(1:1) .NE. '*')
     &   flag_cont = (flag_cont .OR. (index(a,c).ne.0))
       Enddo

      contains_ident = flag_cont

      END




C######################################################################
C
      LOGICAL FUNCTION yet_extracted (j)
C
C-----------------------------------------
C
C   Author   :- R.Vogl                17-SEP-1989
C
C=========================================
C
C   Purpose   :
C   Returned value  :
C   Inputs    :
C   Outputs   :
C
C=========================================
C +
C Declarations.
C -
      IMPLICIT NONE

      INTEGER j

      INTEGER max_lin_rout, max_lin_dcf, max_com_bl
      PARAMETER (max_lin_rout = 3000)
      PARAMETER (max_lin_dcf  =  500)
      PARAMETER (max_com_bl   =  200)

      COMMON / extract_common / extr_bl(max_com_bl), numb_extr_bl
      INTEGER    extr_bl, numb_extr_bl

      INTEGER i
      LOGICAL  extr_flag

      extr_flag = .false.

      Do i=1, numb_extr_bl
        extr_flag = (extr_flag .OR. (extr_bl(i) .EQ. j))
       Enddo

      yet_extracted = extr_flag

      END



C######################################################################
C
      LOGICAL FUNCTION new_identifier (b, a)
C
C-----------------------------------------
C
C   Author   :- R.Vogl                17-SEP-1989
C
C=========================================
C
C   Purpose   :
C   Returned value  :
C   Inputs    :
C   Outputs   :
C
C=========================================
C +
C Declarations.
C -
      IMPLICIT NONE

      CHARACTER*6  a
      CHARACTER*5  b

      LOGICAL resw_match, already_found

      INTEGER i

      INTEGER max_ident
      PARAMETER (max_ident = 2000)

      COMMON / ident_common / ident_number, ident_found(max_ident)
      CHARACTER*6  ident_found
      INTEGER      ident_number

C      INTEGER   nrw
C      PARAMETER (nrw = 32)
C      CHARACTER*12 resw(nrw)
C
C
C      DATA resw /'CHARACTER','DIMENSION','INTEGER','FORMAT','ACCEPT',
C     &           'ACCESS','ALLOCATE','PARAMETER','ASSIGN','REWIND',
C     &           'BACKSPACE','LOGICAL','EXTERNAL','INTRINSIC','RETURN',
C     &           'EQUIVALENCE','COMMON','COMPLEX','CONTINUE','DELETE',
C     &           'DISPOSE','DOUBLE','ELSEIF','ENDFILE','STRUCTURE',
C     &           'UNLOCK','INCLUDE','IMPLICIT','REWRITE','OPTIONS',
C     &           'SUBROUTINE','FUNCTION'/

C
C
C the following loop is not required here because none of the reserved FORTRAN
C words has a 'D' for a fifth character!
C Should other fifth characters than the 'D' be searched for it may be possible
C that this loop has to be utilized!
C

      resw_match = .false.
C      Do i=1, nrw
C        If (a .EQ. resw(i)(1:6)) Then
C          resw_match = .true.
C          Goto 10
C        Endif
C      Enddo

   10 Continue

      already_found = .false.
      Do i=ident_number, 1, -1
        If (a .EQ. ident_found(i)) Then
          already_found = .true.
          Goto 20
         Endif
       Enddo

   20 Continue

      new_identifier = ((.NOT. resw_match) .AND. (.NOT. already_found)
     &          .AND. (b(1:4) .NE. 'CALL') .AND. (a(5:5) .EQ. 'D'))

      END

      Logical Function CheckIfdef(string, LInclude, LineNo)
      Implicit None
C  string should be
C   "#ifdef ident"
C   "#ifndef ident"
C   "#else"
C   "#endif comment"
C This routine determines whether lines should be accepted from here on.
C
      Integer NIdents
      Parameter (NIdents = 2)
      Logical LInclude
      Character*(*) string
      Integer LineNo
      Character*8 Cond_Ident(NIdents) /'UNIX    ', 'LINUX  '/, TC
      Integer I, L1, L2
      Integer I2
      Logical LPhase /.False./

      TC = ' '
      CheckIfdef = LInclude
      Call STR$Trim (string, string, I2)
      If (string(1:6) .eq. '#endif') Then
        If (.NOT. LPhase) Then
          Write(*,'(a,a,a,i5)') ' Incorrect nesting, unexpected ',
     .     string(1:I2),' in line ', LineNo
          Call Exit(1)
        EndIf
        CheckIfdef = .True.
        LPhase = .False.
        Return
      EndIf

      If (string(1:5) .eq. '#else') Then
        If (.NOT. LPhase) Then
          Write(*,'(a,a,a,i5)') ' Incorrect nesting, unexpected ',
     .     string(1:I2), ' in line ', LineNo
          Call Exit(1)
        EndIf
        CheckIfdef = .NOT. LInclude
        Return
      EndIf

      If ((string(1:6) .eq. '#ifdef') .OR.
     . (string(1:7) .eq. '#ifndef')) Then
        If (LPhase) Then
          Write(*,'(a,a,a,i5)') ' Incorrect nesting, unexpected ',
     .     string(1:I2), ' in line ', LineNo
          Call Exit(1)
        EndIf
        Do I = Index(string, ' '), Len(string)
          If (string(I:I) .NE. ' ') Then
            L1 = I
            GoTo 100
          EndIf
        EndDo
  100   Do I = L1, Len(string)
          If (string(I:I) .EQ. ' ') Then
            L2 = I
            GoTo 110
          EndIf
        EndDo
  110   LPhase = .True.
        If (L2 .LE. L1) Return
        Call STR$UpCase (TC, string(L1:L2))
        If (string(1:6) .eq. '#ifdef') Then
          Do I = 1, NIdents
            If (TC .EQ. Cond_Ident(I)) Then
              CheckIfdef = .True.
              Return
            EndIf
          EndDo
          CheckIfdef = .False.
          Return
        Else
          Do I = 1, NIdents
            If (TC .EQ. Cond_Ident(I)) Then
              CheckIfdef = .False.
              Return
            EndIf
          EndDo
          CheckIfdef = .True.
          Return
        EndIf
      EndIf

      Write(*,'(a,a,a,i5)') ' Incorrect keyword ',string(1:I2),
     . ' in line ', LineNo
      Call Exit(1)

      End

      Subroutine STR$Trim(Str1, Str2, Len0)
C  This routine mimics the corresponding RTL VAX routine.
      Implicit None
      Character*(*) Str1, Str2
      Integer Len0, I
      Character*1 Ch
      Integer Len1

      Len1 = Len(Str2)
      Do I = Len1,1,-1
        Ch = Str2(I:I)
        If (.NOT. (Ch .EQ. ' ' .OR. Ch .EQ. Char(9))) Then
          Len0 = I
          Str1(1:I) = Str2(1:I)
          Return
        EndIf
      EndDo

      Len0 = 0
      End

      Subroutine STR$UpCase(Dest, Src)

      Character*(*) Dest, Src
      Integer L1, L2
C  This routine mimics the corresponding RTL VAX routine.
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
