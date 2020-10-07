
      PROGRAM ALWHO_CALL
C
C-----------------------------------------
C
C   Author   :- Olivier Callot        14-NOV-1991
C
C=========================================
C   13/12/93 : F.Blin adapted for unix
C   13/09/95 : M.Cattaneo used as template for WWW version
C=========================================
C
C   Purpose   : Display the database content for one or several users
C   Inputs    : From WWW via environment variables:
C                 FORM_switch  = 'FULL '  for single user info
C                              = 'INST1'  for single institute info
C                              = 'INSTL'  for list of institutes
C                 FORM_surname = (part of) user's surname
C                 FORM_name    = (part of) user's first name or institute name
C   Outputs   : on the WWW client
C
C=========================================
C +
C Declarations.
C -
#include "export_aloha.h"
#include "institute.h"
      INTEGER l,lx,ier,value
C      INCLUDE 'export_aloha.inc'
C      INCLUDE 'institute.inc'
      PARAMETER max_usr = 1000
      RECORD / member / all_usr(max_usr)
      CHARACTER*5 FS
      CHARACTER*80 alwho_file
      CHARACTER*20 last_name, first_name
      INTEGER l_l,l_f,len, i
      INTEGER ind_f, ind_l
      INTEGER k_match, nb_match, nb_found
      CHARACTER*40 key
      CHARACTER*1 archive_test
      CHARACTER*1024 mail_URL
      CHARACTER*400 line
      CHARACTER*25 buffer
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C Entry Point.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      archive_test = " "
      last_name = " "
      mail_URL = "mailto:"
      CALL GETENV ('FORM_switch',FS)
      CALL GETENV ('FORM_surname',last_name)
      CALL GETENV ('FORM_name',first_name)
      CALL CLTOU(FS(1:LENOCC(FS)))

C      CALL GETENV ('ALEPH', alwho_file )
C      alwho_file = alwho_file(1:LENOCC(alwho_file))//alwho_file_path
      alwho_file = "/afs/cern.ch/aleph"//alwho_file_path

      l_l = LENOCC(last_name)
      IF( l_l .EQ. 0 ) THEN
        last_name  = first_name
        first_name = " "
        l_l = LENOCC(last_name)
      ENDIF
      IF( INDEX(first_name," ") .LT. 1 ) first_name = " "
      l_f = LENOCC(first_name)

      CALL CLTOU(last_name(1:l_l))
      CALL CLTOU(first_name(1:l_f))

      IF( FS .EQ. 'CONF ' ) THEN
        CALL write_html_from_src(
     &              '/www/aleph/ALWHO/src/conference.src', 6 )
        GOTO 999

      ELSE IF( FS .EQ. 'ALIGN' ) THEN
        CALL write_html_from_src(
     &              '/www/aleph/ALWHO/src/alignment.src', 6 )
        GOTO 999

      ELSE IF( FS .EQ. 'DATAQ' ) THEN
        CALL write_html_from_src(
     &              '/www/aleph/ALWHO/src/data_quality.src', 6 )
        GOTO 999

      ELSE IF( FS .EQ. 'COORD' ) THEN
        CALL write_html_from_src(
     &              '/www/aleph/ALWHO/src/detector_coord.src', 6 )
        GOTO 999

      ELSE IF( FS .EQ. 'ECHEG' ) THEN
        CALL write_html_from_src(
     &              '/www/aleph/ALWHO/src/echenevex_group.src', 6 )
        GOTO 999

      ELSE IF( FS .EQ. 'ECHEP' ) THEN
        CALL write_html_from_src(
     &              '/www/aleph/ALWHO/src/echenevex_phone.src', 6 )
        GOTO 999

      ELSE IF( FS .EQ. 'LEPWG' ) THEN
        CALL write_html_from_src(
     &              '/www/aleph/ALWHO/src/lep_wg.src', 6 )
        GOTO 999

      ELSE IF( FS .EQ. 'MAILS' ) THEN
        CALL write_html_from_src(
     &              '/www/aleph/ALWHO/src/mailing.src', 6 )
        GOTO 999

C      ELSE IF( FS .EQ. 'MONTE' ) THEN
C        CALL write_html_from_src(
C     &              '/www/aleph/ALWHO/src/montecarlo_contact.src', 6 )
C        GOTO 999

      ELSE IF( FS .EQ. 'OFFLC' ) THEN
        CALL write_html_from_src(
     &              '/www/aleph/ALWHO/src/offline.src', 6 )
        GOTO 999

      ELSE IF( FS .EQ. 'ONLC ' ) THEN
        CALL write_html_from_src(
     &              '/www/aleph/ALWHO/src/online_contact.src', 6 )
        GOTO 999

      ELSE IF( FS .EQ. 'ONLG ' ) THEN
        CALL write_html_from_src(
     &              '/www/aleph/ALWHO/src/online_group.src', 6 )
        GOTO 999

      ELSE IF( FS .EQ. 'ORG  ' ) THEN
        CALL write_html_from_src(
     &              '/www/aleph/ALWHO/src/organization.src', 6 )
        GOTO 999

      ELSE IF( FS .EQ. 'PHYSG' ) THEN
        CALL write_html_from_src(
     &              '/www/aleph/ALWHO/src/physics_group.src', 6 )
        GOTO 999

C      ELSE IF( FS .EQ. 'PHYST' ) THEN
C        CALL write_html_from_src(
C     &              '/www/aleph/ALWHO/src/physics_tools.src', 6 )
C        GOTO 999

      ELSE IF( FS .EQ. 'PUBPL' ) THEN
        CALL write_html_from_src(
     &              '/www/aleph/ALWHO/src/public_places.src', 6 )
        GOTO 999

      ELSE IF( FS .EQ. 'RECCO' ) THEN
        CALL write_html_from_src(
     &           '/www/aleph/ALWHO/src/reconstruction_contact.src', 6 )
        GOTO 999

      ELSE IF( FS .EQ. 'SECR ' ) THEN
        CALL write_html_from_src(
     &              '/www/aleph/ALWHO/src/secretariat.src', 6 )
        GOTO 999

      ENDIF
C
      OPEN( member_unit, file=alwho_file, status='old', readonly,
     &      shared )
 
      nb_found = 0
      nb_inst  = 0
      k_match  = 0
      nb_match = 0
      READ( member_unit, fmt=member_fmt, iostat=ier ) user.all
      DO WHILE ( ier .EQ. 0 )
        IF ( user.all(1:20) .ne. ' ' ) THEN
          IF( user.all(1:1) .EQ. ' ' ) THEN
            DO i=1,nb_inst
              IF( all_inst(i).short_name .EQ. user.all(2:21) ) THEN
                all_inst(i).all(21:420) = user.all(22:)
                IF( FS .EQ. 'INST1' ) THEN
                  key = all_inst(i).short_name
                  CALL CLTOU( key )
                  IF( index(key,last_name(1:l_l)) .NE. 0 )
     &              CALL print_inst(i)
                ELSE IF( FS .EQ. 'INSTL' ) THEN
                  IF( all_inst(i).deleted .EQ. ' ' )
     &              CALL print_inst_name(i)
                ENDIF
              ENDIF
            ENDDO
          ELSE IF ( nb_inst .lt. max_inst ) THEN
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

      IF( FS .EQ. 'INSTL' ) THEN
        WRITE( 6,'(a)' ) '</PRE>'
        GOTO 999
      ENDIF
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C   Scan the user part of the file
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      READ( member_unit, fmt=member_fmt, iostat=ier ) user.all
      DO WHILE ( ier .EQ. 0 )
        IF ( user.archived .EQ. archive_test ) THEN
          nb_found = nb_found + 1
          all_usr(nb_found) = user
          IF ( FS .EQ. 'FULL ' ) THEN
            key = user.all(1:40)
            CALL CLTOU (key)
            ind_l = index( key, last_name(1:l_l) )
            IF ( ind_l .NE. 0 ) THEN
              ind_f = index( key, first_name(1:l_f) )
C Deal with special case: name is substring of surname or vice versa
              IF( (ind_f .GE. 21 .AND. ind_l .GE. 21) .OR.
     &            (ind_f .LE. 20 .AND. ind_l .LE. 20) .AND.
     &            (ind_f .GT. 0)                            ) THEN
                IF( l_f .GT. l_l ) THEN
                  IF( ind_f .GT. 20 ) THEN
                    key = user.all(1:20)
                  ELSE
                    key = user.all(21:40)
                  ENDIF
                  CALL CLTOU (key)
                  IF( index( key, last_name(1:l_l) ) .NE. 0 ) THEN
                    nb_match = nb_match + 1
                    IF( nb_match .EQ. 1 ) k_match = nb_found
                  ENDIF
                ELSE
                  IF( ind_l .GT. 20 ) THEN
                    key = user.all(1:20)
                  ELSE
                    key = user.all(21:40)
                  ENDIF
                  CALL CLTOU (key)
                  IF( index( key, first_name(1:l_f) ) .NE. 0 ) THEN
                    nb_match = nb_match + 1
                    IF( nb_match .EQ. 1 ) k_match = nb_found
                  ENDIF
                ENDIF
C Usual case
              ELSE IF ( l_f .LE. 0 .OR. ind_f .NE. 0 ) THEN
                nb_match = nb_match + 1
                IF( nb_match .EQ. 1 ) k_match = nb_found
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        READ( member_unit, fmt=member_fmt, iostat=ier ) user.all
      ENDDO

C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C All read in, now create the pages
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      IF ( FS .EQ. 'ALL ' ) THEN
        WRITE(6,'(a)') 
     &      '<H3>List of all Aleph members</H3><PRE>'
        DO i=1,nb_found
          IF( all_usr(i).institute .NE. ' ' ) THEN
            CALL print_user_name( all_usr(i).first_name,
     &                            all_usr(i).last_name,
     &                            all_usr(i).login )
          ENDIF
        ENDDO
        WRITE( 6,'(a)' ) '</PRE>'

      ELSE IF ( FS .EQ. 'FULL ' ) THEN
        IF( nb_match .EQ. 1 ) THEN
          user = all_usr( k_match )
          CALL print_user( )
        ELSE IF( nb_match .GT. 1 ) THEN
          WRITE(6,'(a)') 
     &      '<H3>More than one user matches your search</H3><PRE>'
          DO i=1,nb_found
            key = all_usr(i).all(1:40)
            CALL CLTOU (key)
            ind_l = index( key, last_name(1:l_l) )
            IF ( ind_l .NE. 0 ) THEN
              ind_f = index( key, first_name(1:l_f) )
C Deal with special case: name is substring of surname or vice versa
              IF( (ind_f .GE. 21 .AND. ind_l .GE. 21) .OR.
     &            (ind_f .LE. 20 .AND. ind_l .LE. 20) .AND.
     &            (ind_f .GT. 0)                            ) THEN
                IF( l_f .GT. l_l ) THEN
                  IF( ind_f .GT. 20 ) THEN
                    key = user.all(1:20)
                  ELSE
                    key = user.all(21:40)
                  ENDIF
                  CALL CLTOU (key)
                  IF( index( key, last_name(1:l_l) ) .NE. 0 ) THEN
                    CALL print_user_name( all_usr(i).first_name,
     &                                    all_usr(i).last_name,
     &                                    all_usr(i).login )
                  ENDIF
                ELSE
                  IF( ind_l .GT. 20 ) THEN
                    key = user.all(1:20)
                  ELSE
                    key = user.all(21:40)
                  ENDIF
                  CALL CLTOU (key)
                  IF( index( key, first_name(1:l_f) ) .NE. 0 ) THEN
                    CALL print_user_name( all_usr(i).first_name,
     &                                    all_usr(i).last_name,
     &                                    all_usr(i).login )
                  ENDIF
                ENDIF
C Usual case
              ELSE IF ( l_f .LE. 0 .OR. ind_f .NE. 0 ) THEN
                CALL print_user_name( all_usr(i).first_name,
     &                                all_usr(i).last_name,
     &                                all_usr(i).login )
              ENDIF
            ENDIF
          ENDDO
          WRITE( 6,'(a)' ) '</PRE>'
        ELSE
          WRITE( 6, 1020 ) first_name(1:l_f), last_name(1:l_l)
        ENDIF

      ELSE IF( FS .EQ. 'INST1' ) THEN
        DO i=1,nb_found
          key = all_usr(i).institute
          CALL CLTOU (key)
          IF ( index( key, last_name(1:l_l) ) .ne. 0 ) THEN
            CALL print_user_name( all_usr(i).first_name,
     &                            all_usr(i).last_name,
     &                            all_usr(i).login )
          ENDIF
        ENDDO
        WRITE( 6,'(a)' ) '</PRE>'

      ELSE IF( FS .EQ. 'STEER' ) THEN
        WRITE(6,'(a)') '<H1>Steering Committee</H1><PRE>'
        DO i=1,nb_found
          IF( all_usr(i).function(1) .NE. ' ' )
     &        CALL PRINT_COMMITTEE( 1, all_usr(i), mail_URL )
        ENDDO
        IF( mail_URL(1:1) .EQ. '*' ) THEN
          WRITE( 6,'(a)' ) '</PRE>'
        ELSE
          WRITE( 6, 1040 ) mail_URL(1:LENOCC(mail_URL))
        ENDIF

      ELSE IF( FS .EQ. 'SAFE ' ) THEN
        WRITE(6,'(a)') '<H1>Safety Committee</H1><PRE>'
        DO i=1,nb_found
          IF( all_usr(i).function(2) .NE. ' ' )
     &        CALL PRINT_COMMITTEE( 2, all_usr(i), mail_URL )
        ENDDO
        IF( mail_URL(1:1) .EQ. '*' ) THEN
          WRITE( 6,'(a)' ) '</PRE>'
        ELSE
          WRITE( 6, 1040 ) mail_URL(1:LENOCC(mail_URL))
        ENDIF

      ELSE IF( FS .EQ. 'SPEAK' ) THEN
        WRITE(6,'(a)') '<H1>Speakers Bureau</H1><PRE>'
        DO i=1,nb_found
          IF( all_usr(i).function(3) .NE. ' ' )
     &        CALL PRINT_COMMITTEE( 3, all_usr(i), mail_URL )
        ENDDO
        IF( mail_URL(1:1) .EQ. '*' ) THEN
          WRITE( 6,'(a)' ) '</PRE>'
        ELSE
          WRITE( 6, 1040 ) mail_URL(1:LENOCC(mail_URL))
        ENDIF

      ELSE IF( FS .EQ. 'EDIT ' ) THEN
        WRITE(6,'(a)') '<H1>Editorial board</H1><PRE>'
        DO i=1,nb_found
          IF( all_usr(i).function(4) .NE. ' ' )
     &        CALL PRINT_COMMITTEE( 4, all_usr(i), mail_URL )
        ENDDO
        IF( mail_URL(1:1) .EQ. '*' ) THEN
          WRITE( 6,'(a)' ) '</PRE>'
        ELSE
          WRITE( 6, 1040 ) mail_URL(1:LENOCC(mail_URL))
        ENDIF

      ELSE IF( FS .EQ. 'RUNCO' ) THEN
        WRITE(6,'(a)') '<H1>Run Coordinators</H1><PRE>'
        DO i=1,nb_found
          IF( all_usr(i).function(5) .NE. ' ' )
     &        CALL PRINT_COMMITTEE( 5, all_usr(i), mail_URL )
        ENDDO
        IF( mail_URL(1:1) .EQ. '*' ) THEN
          WRITE( 6,'(a)' ) '</PRE>'
        ELSE
          WRITE( 6, 1040 ) mail_URL(1:LENOCC(mail_URL))
        ENDIF

      ELSE IF( FS .EQ. 'INCON' ) THEN
        WRITE(6,'(a)') '<H1>General Institute contacts</H1><PRE>'
        DO i=1,nb_inst
          IF( all_inst(i).deleted .EQ. ' ' )
     &       CALL print_contact_name( all_inst(i).short_name//':', 
     &                                all_inst(i).contact )
        ENDDO
        WRITE( 6,'(a)' ) '</PRE>'

      ELSE IF( FS .EQ. 'INONL' ) THEN
        WRITE(6,'(a)') '<H1>Institute Online contacts</H1><PRE>'
        DO i=1,nb_inst
          IF( all_inst(i).deleted .EQ. ' ' )
     &       CALL print_contact_name( all_inst(i).short_name//':', 
     &                                all_inst(i).contact )
        ENDDO
        WRITE( 6,'(a)' ) '</PRE>'

      ELSE IF( FS .EQ. 'INOFL' ) THEN
        WRITE(6,'(a)') '<H1>Institute Software contacts</H1><PRE>'
        DO i=1,nb_inst
          IF( all_inst(i).deleted .EQ. ' ' )
     &       CALL print_contact_name( all_inst(i).short_name//':', 
     &                                all_inst(i).contact )
        ENDDO
        WRITE( 6,'(a)' ) '</PRE>'

      ELSE IF( FS .EQ. 'INMEL' ) THEN
        WRITE(6,'(a)') '<H1>Institute Mail contacts</H1><PRE>'
        DO i=1,nb_inst
          IF( all_inst(i).deleted .EQ. ' ' )
     &       CALL print_contact_name( all_inst(i).short_name//':', 
     &                                all_inst(i).contact )
        ENDDO
        WRITE( 6,'(a)' ) '</PRE>'

      ELSE IF( FS .EQ. 'BEEPE' ) THEN
        WRITE(6,'(a)') 
     &    '<H1>Experts Mobile Phones and Tatoos</H1><PRE>'
        WRITE(6,'(a)') 'Mobile'
        WRITE(6,'(a)') '-------'
        DO i=1,nb_found
          IF( all_usr(i).institute .EQ. ' ' ) THEN
            line ='<FORM METHOD="POST"'//
     &            ' ACTION="http://axaonl.cern.ch:8000/htbin/tatoo">'
            IF( all_usr(i).CERN_port .NE. ' ' ) THEN
              line = line(1:LENOCC(line))//'16+'//all_usr(i).CERN_port
            ELSE
              line = line(1:LENOCC(line))//'       <!>'
            ENDIF

            line = line(1:LENOCC(line))//' '//
     &        all_usr(i).last_name(1:LENOCC(all_usr(i).last_name))//' '
     &        //all_usr(i).first_name(1:LENOCC(all_usr(i).first_name))
     
            IF( all_usr(i).tatoo .NE. ' ' ) THEN
              l_l = LENOCC(all_usr(i).last_name)
              l_f = LENOCC(all_usr(i).first_name)
              len = MAX( 1, 25-l_l-l_f)
              buffer = ' ........................'
              line=line(1:LENOCC(line))//buffer(1:len)//
     &         '<INPUT TYPE="hidden" NAME="experiment" VALUE="ALEPH">'
     &         //'<INPUT TYPE="hidden" NAME="recipient" VALUE="'//
     &          all_usr(i).last_name(1:LENOCC(all_usr(i).last_name))//
     &          '">'//
     &          '  <INPUT TYPE=submit value="Send to Tatoo:" size=25>'
     &          //'<INPUT name="message" value="" size=12>'
            ENDIF

            line = line(1:LENOCC(line))//'</FORM>'
            IF( all_usr(i).CERN_port .NE. ' '        .OR.
     &          all_usr(i).tatoo     .NE. ' ' ) THEN
              WRITE(6,'(a)') line(1:LENOCC(line))
            ENDIF

          ENDIF
        ENDDO
        WRITE( 6,'(a)' ) '</PRE>'

      ELSE IF( FS .EQ. 'BEEPP' ) THEN
        WRITE(6,'(a)')
     &       '<H1>Personal Mobile Phones and Tatoos</H1>'
        WRITE(6,'(a)') 'You may send a short message (SMS) to a GSM'
        WRITE(6,'(a)') 'portable phone via E-mail, with your message'
        WRITE(6,'(a)') 'as the <B>subject</B>'
        WRITE(6,'(a)') '<PRE>Mobile'
        WRITE(6,'(a)')      '-------'
        DO i=1,nb_found
          IF( all_usr(i).institute .NE. ' ' ) THEN
            line ='<FORM METHOD="POST"'//
     &            ' ACTION="http://axaonl.cern.ch:8000/htbin/tatoo">'
            IF( all_usr(i).CERN_port .NE. ' ' ) THEN
              line = line(1:LENOCC(line))//'<A HREF="mailto:'//
     &          all_usr(i).first_name(1:LENOCC(all_usr(i).first_name))
     &          //'.'//
     &          all_usr(i).last_name(1:LENOCC(all_usr(i).last_name))//
     &          '@16'//all_usr(i).CERN_port//'.gsm.cern.ch">16+'//
     &          all_usr(i).CERN_port//'</A>'
            ELSE
              line = line(1:LENOCC(line))//'       <!>'
            ENDIF

            line = line(1:LENOCC(line))//' '//
     &        all_usr(i).last_name(1:LENOCC(all_usr(i).last_name))//' '
     &        //all_usr(i).first_name(1:LENOCC(all_usr(i).first_name))
     
            IF( all_usr(i).tatoo .NE. ' ' ) THEN
              l_l = LENOCC(all_usr(i).last_name)
              l_f = LENOCC(all_usr(i).first_name)
              len = MAX( 1, 25-l_l-l_f)
              buffer = ' ........................'
              line=line(1:LENOCC(line))//buffer(1:len)//
     &         '<INPUT TYPE="hidden" NAME="experiment" VALUE="ALEPH">'
     &         //'<INPUT TYPE="hidden" NAME="recipient" VALUE="'//
     &          all_usr(i).last_name(1:LENOCC(all_usr(i).last_name))//
     &          '">'//
     &          '  <INPUT TYPE=submit value="Send to Tatoo:" size=25>'
     &          //'<INPUT name="message" value="" size=12>'
            ENDIF

            line = line(1:LENOCC(line))//'</FORM>'

            IF( all_usr(i).CERN_port .NE. ' '        .OR.
     &          all_usr(i).tatoo     .NE. ' ' ) THEN
              WRITE(6,'(a)') line(1:LENOCC(line))
            ENDIF
          ENDIF
        ENDDO
        WRITE( 6,'(a)' ) '</PRE>'

      ELSE 
        WRITE(6,'(a)') '<H1>Invalid keyword</H1>'
      ENDIF

 1020 FORMAT(' No user with ',a,' ',a,' in the name.' )
 1030 FORMAT(15x,'13+',a,2x,a,1x,a)
 1031 FORMAT(5x,'16+',a,3x,'13+',a,2x,a,1x,a)
 1032 FORMAT(5x,'16+',a,12x,a,1x,a)
 1040 FORMAT('</PRE>'/,'<A HREF="',a,
     &       '">Send mail</A> to everyone in this list<BR>')
 999  CONTINUE
      END
C#######################################################################
      SUBROUTINE print_committee( id_com, memb, mail_URL )
C
C-----------------------------------------
C
C   Author   :- Marco Cattaneo
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
#include "export_aloha.h"
C      INCLUDE 'export_aloha.inc'
      RECORD / member / memb
      INTEGER id_com
      CHARACTER*120 line
      CHARACTER*(*) mail_URL
      INTEGER l_URL
      CHARACTER*60 login_line
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C Entry Point.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF( memb.function(id_com) .EQ. 'C' ) THEN
          line = 'Chairman'
      ELSE IF( memb.function(id_com) .EQ. 'S' ) THEN
          line = 'Secretary'
      ELSE IF( memb.function(id_com) .EQ. 'T' ) THEN
          line = 'Tech. Coordinator'
      ELSE IF( memb.function(id_com) .EQ. 'B' ) THEN
          line = 'Spokesman'
      ELSE
          line = ' '
      ENDIF

      CALL print_user_name_t( line, memb.institute, memb.first_name,
     &                              memb.last_name, memb.login )

      l_URL = LENOCC(mail_URL)
      CALL make_valid_email( memb.login, login_line )
      IF( mail_URL(l_URL:l_URL) .EQ. ':' ) THEN
        mail_URL = mail_URL(1:l_URL)//login_line
      ELSEIF(l_URL .LE. 964) THEN
        mail_URL = mail_URL(1:l_URL)//','//login_line
      ELSE
        mail_URL(1:1) = '*'
      ENDIF

      RETURN
      END
C#######################################################################
      SUBROUTINE print_user_name( name, surname, login )
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
      CHARACTER*(*) name, surname, login
      CHARACTER*400 line
      CHARACTER*260 login_line
      INTEGER k_len
      INTEGER i
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C Entry Point.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      line = '<A HREF="/manager/alwho_query/?switch=FULL&name='
     &       //name
      line = line(1:LENOCC(line))//'&surname='//surname
      DO i=48,LENOCC(line)
        IF( line(i:i) .EQ. ' ' ) line(i:i) = '+'
      ENDDO
      k_len = LENOCC(line)+2+4
      line = line(1:LENOCC(line))//'">'//name
      line = line(1:LENOCC(line)+1)//surname
      line = line(1:LENOCC(line))//'</A>'
      CALL make_email_href( login, ' ', login_line )
      line = line(1:k_len+31)//login_line(1:LENOCC(login_line))
      WRITE( 6, '(a)' ) line(1:LENOCC(line))

 999  RETURN
      END
C#######################################################################
      SUBROUTINE print_user_name_t( text, inst, name, surname, login )
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
      CHARACTER*(*) text, inst, name, surname, login
      CHARACTER*400 line
      CHARACTER*260 login_line
      INTEGER k_len, i
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C Entry Point.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      line = text(1:20)//
     &       '<A HREF="/manager/alwho_query/?switch=FULL&name='
     &       //name
      line = line(1:LENOCC(line))//'&surname='//surname
      DO i=48,LENOCC(line)
        IF( line(i:i) .EQ. ' ' ) line(i:i) = '+'
      ENDDO
      k_len = LENOCC(line)+2+4
      line = line(1:LENOCC(line))//'">'//name
      line = line(1:LENOCC(line)+1)//surname
      line = line(1:LENOCC(line))//'</A>'

      line = line(1:k_len+26)//
     &       '(<A HREF="/manager/alwho_query/?switch=INST1&name='
     &       //inst
      k_len = LENOCC(line)+2+4
      line = line(1:LENOCC(line))//'">'//inst
      line = line(1:LENOCC(line))//'</A>)'

      CALL make_email_href( login, ' ', login_line )
      line = line(1:k_len+18)//login_line
      WRITE( 6, '(a)' ) line(1:LENOCC(line))

 999  RETURN
      END
C#######################################################################
      SUBROUTINE print_contact_name( text, name )
C
C-----------------------------------------
C
C   Author   :
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
      CHARACTER*(*) text, name
      CHARACTER*200 line
      CHARACTER*20  first_name, last_name
      INTEGER i,k,j,l
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C Entry Point.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      i = INDEX( name, ' ' )
      last_name = name(1:i-1)

      IF( i.LE.1) THEN
        WRITE( 6, 1000 ) text(1:LENOCC(text))
        GOTO 999
      ENDIF

      k = 40
      DO WHILE( name(k:k) .EQ. ' ' )
        k=k-1
      ENDDO

      j = i
      DO WHILE( j .LE. k )
        IF( name(j:j) .EQ. ' ' ) i = j
        j = j+1
      END DO
      first_name = name(i+1:k)

      line = text(1:LENOCC(text))//
     &       ' <A HREF="/manager/alwho_query/?switch=FULL&name='
     &       //first_name
      line = line(1:LENOCC(line))//'&surname='//last_name
      DO l=48,LENOCC(line)
        IF( line(l:l) .EQ. ' ' ) line(l:l) = '+'
      ENDDO
      line = line(1:LENOCC(line))//'">'//name
      line = line(1:LENOCC(line))//'</A>'
      WRITE( 6, 1000 ) line(1:LENOCC(line))

 999  RETURN
 1000 FORMAT(1x,a )
      END
C#######################################################################
      SUBROUTINE print_user( )
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
#include "export_aloha.h"
#include "institute.h"
C      INCLUDE 'export_aloha.inc'
C      INCLUDE 'institute.inc'
      INTEGER i, j_ins, j_fax
      CHARACTER*260 line
      INTEGER j_decnet, j_at, j_dot
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C Entry Point.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      line = '<H1>'//user.last_name
      line = line(1:LENOCC(line)+1)//user.first_name
      line = line(1:LENOCC(line))//'</H1>'
      WRITE( 6, 1000 ) line(1:LENOCC(line))

C Institute
      line = 'Home institute: <A HREF="/manager/alwho_query/'//
     &       '?switch=INST1&name='//user.institute
      line = line(1:LENOCC(line))//'">'//user.institute
      IF ( user.institute .NE. 'CERN' ) THEN
        IF ( user.at_CERN .ne. ' ' ) THEN
          line = line(1:LENOCC(line))//'</A> but based at CERN'
        ELSE
          line = line(1:LENOCC(line))//'</A>'
        ENDIF
      ELSE
        line = line(1:LENOCC(line))//'</A>'
      ENDIF
      line = line(1:LENOCC(line))//'<BR>'
      WRITE( 6, 1000 ) line(1:LENOCC(line))

C Email
      CALL make_email_href( user.login, user.login2, line )
      line = 'Email address : '//line(1:LENOCC(line))//'<HR>'
      WRITE( 6, 1000 ) line(1:LENOCC(line))

C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C   CERN Info
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      line = '<H3>Address at CERN</H3>'
      WRITE( 6, 1000 ) line(1:LENOCC(line))
      line = '<PRE>'
      WRITE( 6, 1000 ) line(1:LENOCC(line))
      line = 'Division: '//user.cern_division
      WRITE( 6, 1000 ) line(1:LENOCC(line))
      line = 'Office:   '//user.CERN_office
      WRITE( 6, 1000 ) line(1:LENOCC(line))
      line = 'Mailbox:  '//user.CERN_mailbox
      WRITE( 6, 1000 ) line(1:LENOCC(line))
      IF( user.cern_phone_1 .NE. ' ' ) THEN
        line = 'Phone:    [41] (22) 76 <B>'//user.cern_phone_1
        IF( user.cern_phone_2 .NE. ' ' ) THEN
          line = line(1:LENOCC(line))//','//user.cern_phone_2
        ENDIF
        line = line(1:LENOCC(line))//'</B>'
      ELSE
        line = 'Phone:'
      ENDIF
      IF ( user.cern_beep .NE. ' ' ) THEN
        line = line(1:LENOCC(line))//'   Beep: 13+'//user.cern_beep
      ENDIF
      WRITE( 6, 1000 ) line(1:LENOCC(line))

      read(user.cern_fax,'(i1)' ) j_fax
      line = 'Fax:'
      IF ( j_fax .gt. 0 ) THEN
        line = line(1:LENOCC(line))//
     &         '      '//all_inst(k_cern).fax(j_fax)
      ENDIF
      WRITE( 6, 1000 ) line(1:LENOCC(line))

      line = 'Telex:    '//all_inst(k_cern).telex
      WRITE( 6, 1000 ) line(1:LENOCC(line))

      IF ( user.cern_port .NE. ' ' ) THEN
        WRITE(6,1132)
     &           user.first_name(1:LENOCC(user.first_name)),
     &           user.last_name(1:LENOCC(user.last_name)),
     &           user.CERN_port,
     &           user.CERN_port
 1132 FORMAT(' Portable: <A HREF="mailto:'a,'.',a,'@16',a,
     &       '.gsm.cern.ch">16+',a,'</A></PRE>')
        WRITE(6,'(a)') '(You may send a short message (SMS) to a GSM'
        WRITE(6,'(a)') 'portable phone via E-mail, with your message'
        WRITE(6,'(a)') 'as the <B>subject</B>)<P>'
      ENDIF

C Tatoo
      IF ( user.tatoo .NE. ' ' ) THEN
        WRITE(6,1001) user.last_name(1:LENOCC(user.last_name))
 1001   FORMAT(/,1x,
     &    'You may send a numeric message to the Tatoo of this user',
     &    /,'<FORM METHOD="POST"',
     &    ' ACTION="http://axaonl.cern.ch:8000/htbin/tatoo">',
     &    '<INPUT TYPE="hidden" NAME="experiment" VALUE="ALEPH">',
     &    '<INPUT TYPE="hidden" NAME="recipient" VALUE="',a,'">',
     &    ' (usually the phone where you wish to be called back) ',
     &    /,1x,'<INPUT name="message" value="" size=12>',1x,
     &    '<INPUT TYPE=submit value="Send these digits to the Tatoo">',
     &    '</FORM>')
      ELSE
        line = '</PRE>'
        WRITE( 6, 1000 ) line(1:LENOCC(line))
      ENDIF

      IF( user.ccid .NE. '00000' ) THEN
        line = 'See also the information in the <A HREF="http:/'
        line = line(1:LENOCC(line))//'/consult.cern.ch/xwho/people/'
        line = line(1:LENOCC(line))//user.ccid
        line = line(1:LENOCC(line))//'">CERN telephone directory</A>'
        WRITE( 6, 1000 ) line(1:LENOCC(line))
      ENDIF

C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C   Institute Info
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF ( user.institute .NE. 'CERN' ) THEN
        line = '<HR><H3>Address at '//user.institute
        line = line(1:LENOCC(line))//'</H3>'
        WRITE( 6, 1000 ) line(1:LENOCC(line))
        line = '<PRE>'
        WRITE( 6, 1000 ) line(1:LENOCC(line))

        j_ins = 0
        DO i = 1 , nb_inst
          if( user.institute .eq. all_inst(i).short_name ) j_ins = i
        ENDDO

        IF ( all_inst(j_ins).direct .NE. 'none' .AND.
     &       LENOCC(user.inst_phone) .NE. 0 ) THEN
          line = 'Direct phone:    '//all_inst(j_ins).direct
          i = LENOCC(line)
          DO WHILE ( line(i:i) .eq. '.' .OR. line(i:i) .eq. ' ' )
            i = i - 1
          ENDDO
          line = line(1:i)//' '//user.inst_phone
        ELSEIF ( LENOCC(user.inst_phone) .NE. 0 ) THEN
          line = 'Phone:           '//all_inst(j_ins).inst_phone
          line = line(1:LENOCC(line)+1)//'  Ext '//user.inst_phone
        ELSE
          line = 'Institute phone: '//all_inst(j_ins).inst_phone
          WRITE( 6, 1000 ) line(1:LENOCC(line))
          line = 'Group phone:     '//all_inst(j_ins).group_phone
        ENDIF
        WRITE( 6, 1000 ) line(1:LENOCC(line))

        line = 'Fax:             '//all_inst(j_ins).fax(1)
        WRITE( 6, 1000 ) line(1:LENOCC(line))

        line = 'Telex:           '//all_inst(j_ins).telex
        WRITE( 6, 1000 ) line(1:LENOCC(line))

        line = '</PRE>'
        WRITE( 6, 1000 ) line(1:LENOCC(line))

      ENDIF

      IF( user.last_modif .NE. ' ' ) THEN
        line = '<HR><EM>Information last updated on '//user.last_modif
        line = line(1:LENOCC(line))//'</EM>'
        WRITE( 6, 1000 ) line(1:LENOCC(line))
      ENDIF

  999 RETURN
 1000 FORMAT(1x,a )
      END
C#######################################################################
      SUBROUTINE make_email_href( addr1, addr2, line )
C
C-----------------------------------------
C
C   Author   :- 
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
      CHARACTER*(*) addr1, addr2, line
      INTEGER j_decnet, j_at, j_dot
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C Entry Point.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF( addr1 .EQ. ' ' ) THEN
        line = ' '
        GOTO 999
      ENDIF

      line = '<A HREF="mailto:'
      j_decnet = index( addr1, '::' )
      IF( j_decnet .NE. 0 ) THEN
        line = line(1:LENOCC(line))//addr1(j_decnet+2:60)
        line = line(1:LENOCC(line))//'@'//addr1(1:j_decnet-1)
        line = line(1:LENOCC(line))//'.decnet.cern.ch">'//addr1
        line = line(1:LENOCC(line))//'</A> (DECNET)'
      ELSE
        j_at  = index( addr1, '@' )
        j_dot = index( addr1(j_at:60), '.' )
        line  = line(1:LENOCC(line))//addr1
        IF( j_dot .EQ. 0 ) THEN
          line = line(1:LENOCC(line))//'.cern.ch'
        ENDIF
        line = line(1:LENOCC(line))//'">'//addr1
        line = line(1:LENOCC(line))//'</A>'
      ENDIF

      IF ( addr2 .NE. ' ' ) THEN
        line = line(1:LENOCC(line))//' or <A HREF="mailto:'
        j_decnet = index( addr2, '::' )
        IF( j_decnet .NE. 0 ) THEN
          line = line(1:LENOCC(line))//addr2(j_decnet+2:60)
          line = line(1:LENOCC(line))//'@'//addr2(1:j_decnet-1)
          line = line(1:LENOCC(line))//'.decnet.cern.ch">'//addr2
          line = line(1:LENOCC(line))//'</A> (DECNET)'
        ELSE
          j_at  = index( addr2, '@' )
          j_dot = index( addr2(j_at:60), '.' )
          line  = line(1:LENOCC(line))//addr2
          IF( j_dot .EQ. 0 ) THEN
            line = line(1:LENOCC(line))//'.cern.ch'
          ENDIF
          line = line(1:LENOCC(line))//'">'//addr2
          line = line(1:LENOCC(line))//'</A>'
        ENDIF
      ENDIF


 999  RETURN
      END
C#######################################################################
      SUBROUTINE make_valid_email( addr1, line )
C
C-----------------------------------------
C
C   Author   :- 
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
      CHARACTER*(*) addr1, line
      INTEGER j_decnet, j_at, j_dot
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C Entry Point.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF( addr1 .EQ. ' ' ) THEN
        line = ' '
        GOTO 999
      ENDIF

      j_decnet = index( addr1, '::' )
      IF( j_decnet .NE. 0 ) THEN
        line = addr1(j_decnet+2:60)
        line = line(1:LENOCC(line))//'@'//addr1(1:j_decnet-1)
        line = line(1:LENOCC(line))//'.decnet.cern.ch'
      ELSE
        j_at  = index( addr1, '@' )
        j_dot = index( addr1(j_at:60), '.' )
        line  = addr1
        IF( j_dot .EQ. 0 ) THEN
          line = line(1:LENOCC(line))//'.cern.ch'
        ENDIF
      ENDIF

 999  RETURN
      END
C#######################################################################
      SUBROUTINE print_inst(i)
C
C-----------------------------------------
C
C   Author   :
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
#include "export_aloha.h"
#include "institute.h"
C      INCLUDE 'export_aloha.inc'
C      INCLUDE 'institute.inc'

      INTEGER i
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C Entry Point.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      WRITE( 6, 1001 )
     &    all_inst(i).short_name,
     &    all_inst(i).full_name,
     &    all_inst(i).mail_add,
     &    all_inst(i).inst_phone,
     &    all_inst(i).group_phone,
     &    all_inst(i).telex,
     &    all_inst(i).fax,
     &    all_inst(i).direct
 1001   FORMAT(
     &    '<H1> ',a,' </H1> <PRE>'/
     &    2(1x,a/)/' Address for mail : ',a/
     &    5(20x,a/)/
     &    ' Institute phone  : ',a/
     &    ' Group phone      : ',a/
     &    ' Telex            : ',a/
     &    ' Fax              : ',5a/
     &    ' Direct phone     : ',a)


      IF ( all_inst(i).home_page_URL .ne. ' ' ) THEN
          write( 6, 1003 ) all_inst(i).home_page_URL(1:
     &      LENOCC(all_inst(i).home_page_URL)),
     &      all_inst(i).home_page_URL(1:
     &      LENOCC(all_inst(i).home_page_URL))
 1003 format(' WWW Home page    : <A HREF="',A,'">',A,'</A>' )
      ENDIF

      CALL print_contact_name( '<HR><PRE> Contact person   :',
     &                         all_inst(i).contact )
      CALL print_contact_name( 'Online contact   :',
     &                         all_inst(i).onl_contact )
      CALL print_contact_name( 'Software contact :',
     &                         all_inst(i).ofl_contact )
      CALL print_contact_name( 'Group secretary  :',
     &                         all_inst(i).Secretariat )
      CALL print_contact_name( 'Mail contact     :',
     &                         all_inst(i).mail_contact )

      WRITE(6,1999)
 1999 FORMAT('</PRE><HR><PRE>')

  999 RETURN
      END
C#######################################################################
      SUBROUTINE print_inst_name(i)
C
C-----------------------------------------
C
C   Author   :
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
#include "institute.h"
C      INCLUDE 'institute.inc'

      INTEGER i
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C Entry Point.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      IF( i .EQ. 1 ) WRITE(6,'(a)') '<H1>List of institutes</H1><PRE>'

      WRITE( 6, 1002 ) 
     &      all_inst(i).short_name(1:20), all_inst(i).full_name(1)
 1002 FORMAT(a, 9x, a)

      IF ( all_inst(i).home_page_URL .ne. ' ' ) THEN

        WRITE( 6, 1004 ) all_inst(i).short_name, 
     &   all_inst(i).home_page_URL(1:LENOCC(all_inst(i).home_page_URL)),
     &   all_inst(i).full_name(2)
 1004   FORMAT('[<A HREF="/manager/alwho_query?switch=INST1&name=',a,
     &    '">ALWHO page</A>] [<A HREF="',a,'">WWW home page</A>] ',
     &    a,'<BR>')

      ELSE

        WRITE( 6, 1005 ) all_inst(i).short_name, 
     &   all_inst(i).full_name(2)
 1005   FORMAT('[<A HREF="/manager/alwho_query?switch=INST1&name=',a,
     &    '">ALWHO page</A>]',17x,a,'<BR>')

      ENDIF

  999 RETURN
      END

C#######################################################################
      SUBROUTINE write_html_from_src( file_name, lunout )
C
C-----------------------------------------
C
C   Author   :- Olivier Callot        17-JAN-1995
C
C=========================================
C
C   Purpose   : Write the .TeX processing of the .SRC file on Lunout
C   Inputs    : file_name [C*] : Input file
C               lunout [I]     : Output unit
C   Outputs   :
C
C=========================================
C +
C Declarations.
C -
      IMPLICIT NONE
      CHARACTER*(*) file_name
      INTEGER lunout
      INTEGER lun, leng, ier
      CHARACTER*180 line
      INTEGER j_tag, k_tag, j
      CHARACTER*40 tag
      LOGICAL table_mode
      CHARACTER*80 arg
      INTEGER l_arg
      CHARACTER*200  user_name
      INTEGER LENOCC
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C Entry Point.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      lun = 20
      table_mode = .false.
      open( lun,
     &      file = file_name,
     &      status = 'OLD',
     &      readonly )
   10 read( lun, '(q,a)', end=900 ) leng, line
      line = line(1:leng)
   20 j_tag = index(line,'<')
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C   Handle possible <A or </A anchor...
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      DO WHILE ( j_tag .ne. 0 .AND.
     &           ( line(j_tag:j_tag+1) .eq. '<A' .OR.
     &             line(j_tag:j_tag+2) .eq. '</A' ) ) 
        k_tag = index(line(j_tag+1:),'<')
        IF ( k_tag .ne. 0 ) THEN
          j_tag = j_tag + k_tag
        ELSE
          j_tag = 0
        ENDIF
      ENDDO
      leng = LENOCC(line)
      IF ( j_tag .ne. 0 ) THEN
        k_tag = index(line(j_tag+1:leng), '>' )
        IF ( k_tag .eq. 0 ) THEN
          write( 6, * ) 'Incomplete tag : '//line(1:leng)
          goto 900
        ENDIF
        k_tag = j_tag + k_tag
        tag = line(j_tag:k_tag)
        IF ( tag .eq. '<SIZE=12>' ) THEN
          line = ' '
        ELSEIF ( tag .eq. '<SIZE=10>' ) THEN
          line = ' '
        ELSEIF ( tag .eq. '<P>' ) THEN
          CALL single_tag_write( line, lunout, '<P>' )
        ELSEIF ( tag .eq. '<RULE>' ) THEN
          CALL single_tag_write( line, lunout, '<HR>' )
        ELSEIF ( tag .eq. '<TABLE>' ) THEN
          IF ( table_mode ) THEN
            write( 6, * ) '<TABLE> while in table mode '
          ENDIF
          table_mode = .true.
          CALL single_tag_write( line, lunout, '<PRE>' )
          read( lun, '()', end=900 )    ! Skip table description
        ELSEIF ( tag .eq. '<ENDTABLE>' ) THEN
          IF ( .NOT. table_mode ) THEN
            write( 6, * ) '<ENDTABLE> while not in table mode'
          ENDIF
          table_Mode = .false.
          CALL single_tag_write( line, lunout, '</PRE>' )
        ELSEIF ( tag .eq. '<PAGE>' ) THEN
          CALL single_tag_write( line, lunout, ' ' )
        ELSEIF ( tag .eq. '<SKIP>' ) THEN
          IF ( j_tag .ne. 1 ) THEN
            CALL write_line( line(1:j_tag-1), table_mode, lunout )
            line = line(j_tag:)
          ENDIF
          CALL single_tag_write( line, lunout, ' ' )
        ELSEIF ( tag .eq. '<SPACE>' ) THEN
          IF ( j_tag .ne. 1 ) THEN
            CALL write_line( line(1:j_tag-1), table_mode, lunout )
            line = line(j_tag:)
          ENDIF
          CALL single_tag_write( line, lunout, ' ' )
        ELSEIF ( tag .eq. '<LIST>' ) THEN
          line = ' '
        ELSEIF ( tag .eq. '<ENDLIST>' ) THEN
          line = ' '
        ELSEIF ( tag .eq. '<LE>' ) THEN
          line = '  - '//line(5:)
        ELSEIF ( tag .eq. '<BOLD>' ) THEN
          Call get_arg( line(1:leng), k_tag+1, arg, l_arg)
          line = line(1:j_tag-1)//arg(1:l_arg)//line(k_tag+l_arg+3:)
        ELSEIF ( tag .eq. '<BOLD12>' ) THEN
          Call get_arg( line(1:leng), k_tag+1, arg, l_arg)
          line = line(1:j_tag-1)//arg(1:l_arg)//line(k_tag+l_arg+3:)
        ELSEIF ( tag .eq. '<CENTER>' ) THEN
          Call get_arg( line(1:leng), k_tag+1, arg, l_arg)
          line = line(1:j_tag-1)//arg(1:l_arg)//line(k_tag+l_arg+3:)
        ELSEIF ( tag .eq. '<NAME>' ) THEN
          CALL get_arg( line(1:leng), k_tag+1, arg, l_arg )
          j = index(arg, '_')
          IF ( j .ne. 0 ) THEN
            CALL make_user_name( arg(1:j-1), arg(j+1:), user_name )
          ELSE
            user_name = 'Invalid name : '//arg
          ENDIF
          line = line(1:j_tag-1)//user_name(1:LENOCC(user_name))//
     &           line(k_tag+l_arg+3:)
        ELSEIF ( tag .eq. '<TITLE>' ) THEN
          Call get_arg( line(1:leng), k_tag+1, arg, l_arg)
          CALL single_tag_write( line, lunout, 
     &      '<HR> <H1> '//arg(1:l_arg)//' </H1> <HR>' )
        ELSE
          write( 6, * ) 'Unknown tag : '//tag
        ENDIF
        if( line .eq. ' ' ) goto 10
        goto 20
      ELSE
        CALL write_line( line(1:leng), table_mode, lunout )
      ENDIF
      goto 10
  900 close( lun )
  999 RETURN
      END
C#######################################################################
      SUBROUTINE write_line( line, table_mode, lunout )
C
C-----------------------------------------
C
C   Author   :- Olivier Callot        17-JAN-1995
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
      CHARACTER*(*) line
      INTEGER table_Mode, lunout, length, i
      CHARACTER*180 out
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C Entry Point.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      length = len( line )
      IF ( table_mode ) THEN
        out = line(1:length)
        DO i = 1 , length
          IF ( out(i:i) .eq. '|' ) THEN
            IF ( i .eq. 1 ) THEN
              out = out(2:)
            ELSE
              out = out(1:i-1)//out(i+1:)
            ENDIF
            length = length - 1
          ENDIF
        ENDDO
        write( lunout, '(A)' ) out(1:length)
      ELSE
        write( lunout, '(A)' ) line(1:length)
      ENDIF
  999 RETURN
      END
C#######################################################################
      SUBROUTINE single_tag_write( line, lunout, text )
C
C-----------------------------------------
C
C   Author   :- Olivier Callot        17-JAN-1995
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
      CHARACTER*(*) line, text
      INTEGER lunout
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C Entry Point.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF ( line(1:1) .ne. '<' ) THEN
        write( 6, * ) 'This tag should start on column 1 : '//line
      ENDIF
      line = ' '
      write( lunout, '(A)' ) text
  999 RETURN
      END
C#######################################################################
      SUBROUTINE get_arg( line, ip, arg, l_arg )
C
C-----------------------------------------
C
C   Author   :- Olivier Callot        17-JAN-1995
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
      CHARACTER*(*) line, arg
      INTEGER ip, l_arg, leng
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C Entry Point.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      leng = len( line )
      IF ( line(ip:ip) .ne. '(' ) THEN
        write( 6, * ) 'Missing argument : '//line(1:leng)
        l_arg = 0
        return
      ELSE
        arg = line(ip+1:)
        l_arg = index( arg, ')') - 1
        IF ( l_arg .lt. 0 ) THEN
          write( 6, * ) 'Missing closing parenthese : '//line(1:leng)
          l_arg = 0
        ELSE
          arg = arg(1:l_arg)
        ENDIF
      ENDIF
  999 RETURN
      END
C#######################################################################
      SUBROUTINE make_user_name( last_name, first_name, line )
C
C-----------------------------------------
C
C   Author   :
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
      CHARACTER*(*) first_name, last_name, line
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C Entry Point.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      line = ' <A HREF="/manager/alwho_query/?switch=FULL&name='
     &       //first_name
      line = line(1:LENOCC(line))//'&surname='//last_name
      DO i=43,LENOCC(line)
        IF( line(i:i) .EQ. ' ' ) line(i:i) = '+'
      ENDDO
      line = line(1:LENOCC(line))//'">'//first_name
      line = line(1:LENOCC(line))//' '//last_name
      line = line(1:LENOCC(line))//'</A>'

 999  RETURN
 1000 FORMAT(1x,a )
      END

