 
      PROGRAM ALWHO_CALL
C
C-----------------------------------------
C
C   Author   :- Olivier Callot        14-NOV-1991
C
C=========================================
C   13/12/93 : F.Blin      adapted for unix
C   11/01/96 : M.Cattaneo  modified for new database format
C=========================================
C
C   Purpose   : Display the database content for one or several users
C   Inputs    : on the DCL command line
C   Outputs   : on the screen
C
C=========================================
C +
C Declarations.
C -
      INTEGER ier
      INCLUDE 'export_aloha.inc'
      INCLUDE 'institute.inc'
      CHARACTER*5 FS
      CHARACTER*80 alwho_file
      CHARACTER*20 last_name, first_name
      INTEGER l_l,l_f, nb_found
      CHARACTER*40 key
      CHARACTER*1 archive_test
      LOGICAL full_flag
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C Entry Point.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      archive_test = " "
      last_name = " "
      full_flag = .FALSE.
      CALL GETENV ('PAR1',FS)
      CALL GETENV ('PAR2',last_name)
      CALL GETENV ('PAR3',first_name)
      CALL GETENV ('ALEPH', alwho_file )
      alwho_file = alwho_file(1:LENOCC(alwho_file))//alwho_file_path
      CALL CLTOU(FS(1:LENOCC(FS)))
      IF (FS.EQ."FULL ") full_flag = .TRUE.
      CALL CLTOU(last_name(1:LENOCC(last_name)))
      CALL CLTOU(first_name(1:LENOCC(first_name)))
      IF (INDEX(first_name," ") .LT. 1) first_name = " "
      IF (first_name .EQ. "WXYZ") first_name = " "
      l_f = LENOCC(first_name)
      l_l = LENOCC(last_name)
C
      OPEN( member_unit, file=alwho_file, status='old', readonly,
     &      shared )
 
      IF ( last_name .EQ. '?' .OR.
     &     ( last_name .eq. ' ' .AND. archive_test .eq. ' ' ) ) THEN
        WRITE( 6, 3000 )
      ELSE
        nb_found = 0
        nb_inst  = 0
        READ( member_unit, fmt=member_fmt, iostat=ier ) user.all
        DO WHILE ( ier .EQ. 0 )
          IF ( user.all(1:20) .ne. ' ' ) THEN
            IF ( nb_inst .lt. max_inst ) THEN
              nb_inst = nb_inst + 1
              all_inst(nb_inst).short_name = user.all(1:20)
              all_inst(nb_inst).all(421:)  = user.all(21:)
              if( user.all(1:20) .eq. 'CERN' ) k_cern = nb_inst
            ENDIF
            READ( member_unit, fmt=member_fmt, iostat=ier ) user.all
          ELSE
            ier = 1
          ENDIF
        ENDDO
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C   Scan the user part of the file
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        READ( member_unit, fmt=member_fmt, iostat=ier ) user.all
        DO WHILE ( ier .EQ. 0 )
          key = user.all(1:40)
          CALL CLTOU (key)
          IF ( index( key, last_name(1:l_l) ) .ne. 0 ) THEN
            IF ( l_f .LE. 0 .OR.
     &           index( key, first_name(1:l_f)) .ne. 0) THEN
              IF ( user.archived .EQ. archive_test ) THEN
                nb_found = nb_found + 1
                CALL print_info( full_flag, nb_found )
              ENDIF
            ENDIF
          ENDIF
          READ( member_unit, fmt=member_fmt, iostat=ier ) user.all
        ENDDO
        IF ( nb_found .EQ. 0 ) THEN
          WRITE( 6, 1020 ) last_name(1:l_l)
        ENDIF
      ENDIF
 1020 FORMAT(' No user with ',a,' in its name.' )
 3000 FORMAT(
     &  ' ALWHO requires an argument, ( part of ) a user name.'/
     &  10x,'It will search the complete user name for this string,'/
     &  10x,'or these two strings if two are given.'//
     &  ' You can specify qualifiers:'/
     &  10x,'FULL will display all information on the selected users.'/
     &  20x,'Default is one line with CERN phone and Login.'/
     &  ' Please report problems/suggestions to VXCERN::BLIN')
      END
C#######################################################################
      SUBROUTINE print_info( full_flag, nb_found )
C
C-----------------------------------------
C
C   Author   :- Olivier Callot         3-DEC-1991
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
      LOGICAL full_flag
      INCLUDE 'export_aloha.inc'
      INCLUDE 'institute.inc'
      INTEGER nb_found, i, j_ins, j_fax
      CHARACTER*132 line
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C Entry Point.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF ( nb_found .EQ. 1 ) THEN
        IF ( full_flag ) THEN
          WRITE( 6, 1030 )
        ELSE
          WRITE( 6, 1010 )
        ENDIF
      ELSE
        IF( full_flag ) WRITE( 6, 1030 )
      ENDIF
      line = user.last_name(1:LENOCC(user.last_name)+1)//
     &             user.first_name
      IF ( full_flag ) THEN
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C   CERN Info
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        line = line(1:30)//' '//user.cern_division//' '//
     &               user.CERN_office
        IF ( user.at_CERN .ne. ' ' ) THEN
          line(29:30) = ' *'
        ENDIF
        line = line(1:47)//user.CERN_mailbox
        j_ins = 0
        IF ( user.institute .NE. 'CERN' ) THEN
          line = line(1:56)//user.institute
          DO i = 1 , nb_inst
            if( user.institute .eq. all_inst(i).short_name ) j_ins = i
          ENDDO
          IF ( user.at_CERN .eq. ' ' ) THEN
                line = line(1:LENOCC(line))//' *'
          ENDIF
        ENDIF
        WRITE( 6, 1000 ) line(1:LENOCC(line))
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C   Phone number
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        line = ' '
        line = line( 1:18)//'Phone : '//user.cern_phone_1
        IF ( user.cern_phone_2 .NE. ' ' ) THEN
          line = line(1:LENOCC(line))//','//user.cern_phone_2
        ENDIF
        IF ( user.cern_port .NE. ' ' ) THEN
          line = line(1:LENOCC(line))//',16'//user.cern_port
        ENDIF
        IF ( j_ins .ne. 0 ) THEN
          IF ( all_inst(j_ins).direct .ne. 'none' ) THEN
            line = line(1:47)//all_inst(j_ins).direct
            i = LENOCC(line)
            DO WHILE ( line(i:i) .eq. '.' .OR. line(i:i) .eq. ' ' )
              i = i - 1
            ENDDO
            line = line(1:i)//' '//user.inst_phone
          ELSE
            line = line(1:47)//all_inst(j_ins).inst_phone
            line = line(1:LENOCC(line)+1)//'Ext '//user.inst_phone
          ENDIF
        ENDIF
        WRITE( 6, 1000 ) line(1:LENOCC(line))
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C   Fax info
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        read(user.cern_fax,'(i1)' ) j_fax
        line = ' '
        line = line( 1:20 ) //'Fax : '
        IF ( j_fax .gt. 0 ) THEN
          line = line( 1:26 ) //all_inst(k_cern).fax(j_fax)
        ENDIF
        IF ( j_ins .ne. 0 ) THEN
          line = line(1:47)//all_inst(j_ins).fax(1)
        ENDIF
        WRITE( 6, 1000 ) line(1:LENOCC(line))
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C   Telex info
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        line = ' '
        line = line( 1:18 ) //'Telex : '
        line = line( 1:26 ) //all_inst(k_cern).telex
        IF ( j_ins .ne. 0 ) THEN
          line = line(1:47)//all_inst(j_ins).telex
        ENDIF
        WRITE( 6, 1000 ) line(1:LENOCC(line))
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C   Login info
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        IF ( user.login .NE. ' ' ) THEN
          line = line(1:18)//'Login : '//user.login
          IF ( user.login2 .NE. ' ' ) THEN
             line = line(1:LENOCC(line))//' or '//user.login2
          ENDIF
          WRITE( 6, 1000 ) line(1:LENOCC(line))
        ENDIF
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C   CERN home address
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        line = ' '
        IF ( user.cern_home_add(1) .NE. ' ' ) THEN
          line = line(1:19)//'Home : '
          DO i = 1 , 4
            line = line(1:26)//user.cern_home_add(i)
            IF ( line .NE. ' ' ) THEN
              WRITE( 6, 1000 ) line(1:LENOCC(line))
              line = ' '
            ENDIF
          ENDDO
        ENDIF
        IF ( user.cern_home_phone .NE. ' ' ) THEN
          line = line(1:20)//'Tel : '//user.CERN_home_phone
          WRITE( 6, 1000 ) line(1:LENOCC(line))
        ENDIF
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C   Institute home address
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        IF ( user.inst_home_add(1) .NE. ' ' ) THEN
          line = line(1:19)//'Home : '
          DO i = 1 , 4
            line = line(1:26)//user.inst_home_add(i)
            IF ( line .NE. ' ' ) THEN
              WRITE( 6, 1000 ) line(1:LENOCC(line))
              line = ' '
            ENDIF
          ENDDO
        ENDIF
        IF ( user.inst_home_phone .NE. ' ' ) THEN
          line = line(1:20)//'Tel : '//user.INST_home_phone
          WRITE( 6, 1000 ) line(1:LENOCC(line))
        ENDIF
      ELSE
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C   Short form : CERN phone, and login.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        line = line(1:25)//user.cern_phone_1
        IF ( user.cern_phone_2 .NE. ' ' ) THEN
          line = line(1:30)//','//user.cern_phone_2
        ENDIF
        IF ( user.cern_port .NE. ' ' ) THEN
          line = line(1:37)//'16'//user.cern_port
        ENDIF
        line = line(1:46)//user.CERN_mailbox(1:9)
        line = line(1:55)//user.login
        IF ( user.login2 .ne. ' ' ) THEN
          line = line(1:LENOCC(line))//','//user.login2
        ENDIF
        WRITE( 6, 1000 ) line(1:LENOCC(line))
      ENDIF
  999 RETURN
 1000 FORMAT(1x,a )
 1010 FORMAT(1x,'User name',16x,'CERN phone, Portable,',
     &       'Mailbox  E-mail'/
     &       1x,78('='))
 1020 FORMAT(1x)
 1030 FORMAT(1x,'User name',17x,'CERN Div Office',6x,
     &      'Mailbox  Institute'/
     &       1x,78('='))
      END
