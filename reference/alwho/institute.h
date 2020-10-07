C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C   This describes an Institute
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      INTEGER    institute_unit
      PARAMETER (institute_unit    = 42)
      INTEGER    institute_rec_len
      PARAMETER (institute_rec_len = 1024)
      CHARACTER*7 institute_fmt
      PARAMETER (institute_fmt     = '(A1024)')
      STRUCTURE / institution /  
        UNION
          MAP
            CHARACTER*20 short_name     ! used in the member database     20
            CHARACTER*50 full_name(2)   ! Official full name             120
            CHARACTER*50 Mail_add(6)    ! Name and mail address          420
            CHARACTER*20 Inst_phone     ! Institute phone                440
            CHARACTER*20 Group_phone    ! Group phone or extension       460
            CHARACTER*20 Telex          ! Telex address                  480
            CHARACTER*20 Fax(5)         ! Various faxes                  580
            CHARACTER*20 direct         ! How to call directly           600
            CHARACTER*40 Contact        ! Contact person                 640
            CHARACTER*40 Onl_contact    !                                680
            CHARACTER*40 Ofl_contact    !                                720
            CHARACTER*40 Secretariat    !                                760
            CHARACTER*40 Mail_contact   !                                800
            CHARACTER*80 home_page_URL  !                                880
            CHARACTER*1  deleted        ! non blank if deleted           881
          END MAP
          MAP
            CHARACTER*(institute_rec_len) all
          END MAP
        END UNION
      END STRUCTURE
      INTEGER max_inst
      PARAMETER (max_inst = 50)
      RECORD / institution /  inst, all_inst(max_inst)
      INTEGER nb_inst, k_cern
      COMMON / WHOSHO_INST / inst, all_inst, nb_inst, k_cern
