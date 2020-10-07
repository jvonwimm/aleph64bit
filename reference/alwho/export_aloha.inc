C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C   Structure to hold information about one member ( exported info )
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CHARACTER*31 alwho_file_path
C      PARAMETER (alwho_file_path = '/src/alwho/aleph.database')
      PARAMETER (alwho_file_path = '/reference/alwho/aleph.database')
      INTEGER member_unit
      PARAMETER (member_unit     = 41)
      INTEGER member_rec_len
      PARAMETER (member_rec_len  = 600)
      CHARACTER*6 member_fmt
      PARAMETER (member_fmt      = '(A600)')
      STRUCTURE / member /  
        UNION
          MAP
            CHARACTER*20 last_name        ! Uppercase surname                 20
            CHARACTER*20 First_name       ! Capitalized first-name(s)         40
            CHARACTER*20 Institute        ! Uppercase institute short name    60
            CHARACTER*3  CERN_division    ! PPE, ECP, ...                     63
            CHARACTER*1  at_cern          ! M for Mail, blank = not at CERN   64
            CHARACTER*12 CERN_office      ! CERN building-floor-room          76
            CHARACTER*5  CERN_phone_1     ! CERN phone number                 81
            CHARACTER*5  CERN_phone_2     ! Other CERN phone number           86
            CHARACTER*1  CERN_fax         ! Reference to the CERN fax         87
            CHARACTER*1  archived         ! non blank if non active           88
            CHARACTER*4  CERN_beep        ! Beep, or space                    92
            CHARACTER*4  CERN_port        ! portable phone                    96
            CHARACTER*12 INST_phone       ! Institute phone number(s)        108
            CHARACTER*32 CERN_home_add(4) ! Home address around Geneva       236
            CHARACTER*20 CERN_home_phone  ! Home phone number ( Geneva )     256
            CHARACTER*32 INST_home_add(4) ! home home                        384
            CHARACTER*20 INST_home_phone  ! home phone                       404
            CHARACTER*60 login            ! Preferred login                  464
            CHARACTER*60 login2           ! Preferred login                  524
            CHARACTER*6  CERN_ID          ! Cern ID                          530
            CHARACTER*12 last_modif       ! Last modification date           542
            CHARACTER*1  function(8)      ! Functions and commitee           550
            CHARACTER*5  ccid             ! CERN CC ID ( for Who )           555
            CHARACTER*12 tatoo            ! France Telecom Tatoo             567
            CHARACTER*12 CERN_mailbox     ! CERN internal mailbox            579
          END MAP
          MAP
            CHARACTER*(member_rec_len) all
          END MAP
        END UNION
      END STRUCTURE
      RECORD / member /  user
      COMMON / WHOSWHO_MEMBER / user
