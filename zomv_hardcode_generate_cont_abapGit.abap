*&---------------------------------------------------------------------*
*& Report ZSMASH_HARDCODE_GENERATE_CONT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zomv_hardcode_generate_cont_abapGit. "TESTING1
*&---------------------------------------------------------------------*
*& Report ZSMASH_HARDCODE_GENERATE_CONT_abapGit
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

PARAMETERS: lot1 RADIOBUTTON GROUP r1 USER-COMMAND sy-ucomm DEFAULT 'X'.
PARAMETERS: lot2 RADIOBUTTON GROUP r1 .
SELECTION-SCREEN BEGIN OF BLOCK lot1 WITH FRAME TITLE T_lot1.
  PARAMETERS: p_file   TYPE localfile MODIF ID lt1,
              p_fildwn TYPE localfile MODIF ID lt1.
  SELECTION-SCREEN SKIP.
  PARAMETERS: p_b_file TYPE localfile MODIF ID lt1,   "BUKRS mapping file
              p_e_file TYPE localfile MODIF ID lt1.   "EKORG mapping file
SELECTION-SCREEN END OF BLOCK lot1.

SELECTION-SCREEN BEGIN OF BLOCK lot2 WITH FRAME TITLE T_Lot2.
  SELECTION-SCREEN:
  BEGIN OF LINE,
  COMMENT 1(31) c_upf FOR FIELD p_upfile MODIF ID lt2.
  PARAMETERS: p_upfile   TYPE localfile MODIF ID lt2.
  SELECTION-SCREEN: END OF LINE,
  BEGIN OF LINE,
  COMMENT 1(31) c_dwf FOR FIELD p_dwfile MODIF ID lt2.
  PARAMETERS: p_dwfile TYPE localfile MODIF ID lt2.
  SELECTION-SCREEN: END OF LINE, SKIP.

  PARAMETERS: p_l2_buk TYPE localfile MODIF ID lt2.  "Lot 2 BUKRS Mapping file.
  PARAMETERS: p_l2_afa TYPE localfile MODIF ID lt2.  "Lot 2 AFAPL Mapping file.
  PARAMETERS: p_l2_anl TYPE localfile MODIF ID lt2.  "Lot 2 ANLKL Mapping file.
  PARAMETERS: p_l2_mws TYPE localfile MODIF ID lt2.  "Lot 2 MWSKZ Mapping file.
  PARAMETERS: p_l2_sak TYPE localfile MODIF ID lt2.  "Lot 2 SAKNR Mapping file.
  PARAMETERS: p_l2_zls TYPE localfile MODIF ID lt2.  "Lot 2 ZLSCH Mapping file.
SELECTION-SCREEN END OF BLOCK lot2.

INITIALIZATION.
  t_lot1 = |Lot 1|.
  t_lot2 = |Lot 2|.
  c_upf = |Input file path|.
  c_dwf = |Output file download path|.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF lot1 EQ abap_true.
      IF screen-group1 EQ 'LT1'.
        screen-active = 1.
      ELSEIF screen-group1 EQ 'LT2'.
        screen-active = 0.
      ENDIF.
*      CLEAR: p_l2_afa,p_l2_anl,p_l2_buk,p_l2_mws,p_l2_sak,p_l2_zls,p_upfile,p_dwfile.
    ENDIF.
    IF lot2 EQ abap_true.
      IF screen-group1 EQ 'LT2'.
        screen-active = 1.
      ELSEIF screen-group1 EQ 'LT1'.
        screen-active = 0.
      ENDIF.
*      CLEAR: p_file,p_fildwn,p_b_file,p_e_file.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
  IF lot1 IS NOT INITIAL.
    CLEAR: p_l2_afa,p_l2_anl,p_l2_buk,p_l2_mws,p_l2_sak,p_l2_zls,p_upfile,p_dwfile.
  ENDIF.
  IF lot2 IS NOT INITIAL.
    CLEAR: p_file,p_fildwn,p_b_file,p_e_file.
  ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM value_help CHANGING p_file.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_b_file.
  PERFORM value_help CHANGING p_b_file.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_e_file.
  PERFORM value_help CHANGING p_e_file.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_upfile.
  PERFORM value_help CHANGING p_upfile.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_l2_afa.
  PERFORM value_help CHANGING p_l2_afa.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_l2_anl.
  PERFORM value_help CHANGING p_l2_anl.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_l2_buk.
  PERFORM value_help CHANGING p_l2_buk.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_l2_mws.
  PERFORM value_help CHANGING p_l2_mws.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_l2_sak.
  PERFORM value_help CHANGING p_l2_sak.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_l2_zls.
  PERFORM value_help CHANGING p_l2_zls.

START-OF-SELECTION.

  TYPES :BEGIN OF ty_input,
           domname TYPE string,
           value   TYPE string,
           ctype   TYPE char20,
           cid     TYPE char30,
           objname TYPE string,
           objtype TYPE string,
           sflag   TYPE c LENGTH 1,
           sincl   TYPE string,
         END OF ty_input.

  TYPES : BEGIN OF ty_output,
            progname  TYPE rvari_vnam,
            contextid TYPE char30,
            type      TYPE rsscr_kind,
            counter   TYPE tvarv_numb,
            sign      TYPE tvarv_sign,
            opti      TYPE tvarv_opti,
            low       TYPE char30,
            high      TYPE char30,
            datadesc  TYPE char50,
          END OF ty_output.

  TYPES:BEGIN OF ty_range,
          value TYPE string,
        END OF ty_range.
  TYPES:BEGIN OF ty_bukrs,
          old_bukrs TYPE bukrs,
          new_bukrs TYPE bukrs,
        END OF ty_bukrs.
  TYPES:BEGIN OF ty_ekorg,
          old_ekorg TYPE ekorg,
          new_ekorg TYPE ekorg,
        END OF ty_ekorg,
        BEGIN OF ty_afapl,
          old_afapl TYPE afapl,
          new_afapl TYPE afapl,
        END OF ty_afapl,
        BEGIN OF ty_anlkl,
          old_anlkl TYPE anlkl,
          new_anlkl TYPE anlkl,
        END OF ty_anlkl,
        BEGIN OF ty_mwskz,
          old_mwskz TYPE mwskz,
          new_mwskz TYPE mwskz,
        END OF ty_mwskz,
        BEGIN OF ty_saknr,
          old_saknr TYPE saknr,
          new_saknr TYPE saknr,
        END OF ty_saknr,
        BEGIN OF ty_zlsch,
          old_zlsch TYPE char1,
          new_zlsch TYPE char1,
        END OF ty_zlsch.


  DATA: gt_input     TYPE TABLE OF ty_input,
        gs_input     TYPE ty_input,
        gt_bukrs_map TYPE  TABLE OF ty_bukrs,
        gt_ekorg_map TYPE TABLE OF ty_ekorg.

  DATA:gt_output TYPE TABLE OF ty_output,
       gs_output TYPE ty_output,
       gt_range  TYPE TABLE OF string,
       gs_range  LIKE LINE OF gt_range.

  "Lot2 declarations
  DATA: gt_lot2_input TYPE TABLE OF ty_input,
        gs_lot2_input TYPE ty_input,
        gt_l2_afapl   TYPE TABLE OF ty_afapl,
        gt_l2_anlkl   TYPE TABLE OF ty_anlkl,
        gt_l2_bukrs   TYPE TABLE OF ty_bukrs,
        gt_l2_mwskz   TYPE TABLE OF ty_mwskz,
        gt_l2_saknr   TYPE TABLE OF ty_saknr,
        gt_l2_zlsch   TYPE TABLE OF ty_zlsch.
  DATA: gt_lot2_output TYPE TABLE OF ty_output,
        gs_lot2_output TYPE ty_output.

  IF lot1 EQ abap_true.
    PERFORM get_input_list.


    IF gt_input IS NOT INITIAL.
      PERFORM populate_final_value.
    ENDIF.

    IF gt_output IS NOT INITIAL.
      PERFORM get_download.
    ENDIF.
  ENDIF.
  IF lot2 EQ abap_true.
    PERFORM read_input_files.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  GET_INPUT_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_input_list .

  DATA: lv_file_str TYPE string.
  lv_file_str = p_file.
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = lv_file_str
*     filetype                = 'ASC'
      has_field_separator     = cl_abap_char_utilities=>horizontal_tab
    CHANGING
      data_tab                = gt_input
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  IF sy-subrc <> 0.
*    ERROR HANDLING
    WRITE :/ 'Error uploading file to intenal table'.
    STOP.
  ENDIF.

  DATA: lv_file_b TYPE string.
  DATA: lv_file_e TYPE string.
  lv_file_b = p_b_file.
  lv_file_e = p_e_file.
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = lv_file_b
*     filetype                = 'ASC'
      has_field_separator     = cl_abap_char_utilities=>horizontal_tab
    CHANGING
      data_tab                = gt_bukrs_map
*     isscanperformed         = SPACE
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
  IF sy-subrc <> 0.
*    ERROR HANDLING
    WRITE :/ 'Error uploading file to intenal table-BUKRS'.
    STOP.
  ENDIF.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = lv_file_e
*     filetype                = 'ASC'
      has_field_separator     = cl_abap_char_utilities=>horizontal_tab
    CHANGING
      data_tab                = gt_ekorg_map
*     isscanperformed         = SPACE
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.

  IF sy-subrc <> 0.
*    ERROR HANDLING
    WRITE :/ 'Error uploading file to intenal table-EKORG'.
    STOP.
  ENDIF.
  SORT: gt_bukrs_map, gt_ekorg_map.


ENDFORM.                    " GET_INPUT_LIST

*&---------------------------------------------------------------------*
*&      Form  GET_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_download .

  DATA: lv_dwn_str TYPE string.
  lv_dwn_str = p_fildwn.
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename              = lv_dwn_str
      write_field_separator = cl_abap_char_utilities=>horizontal_tab
    TABLES
      data_tab              = gt_output.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " GET_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  POPULATE_FINAL_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM populate_final_value .

  DATA: lv_count     TYPE tvarv_numb,
        lv_new_value TYPE ty_output-low.
  CLEAR: gs_input,gs_output,gt_output.

  LOOP AT gt_input INTO gs_input.
    CLEAR: lv_new_value.
    CASE gs_input-domname .
      WHEN 'BUKRS'.
        READ TABLE gt_bukrs_map INTO DATA(gs_bukrs_map)
          WITH KEY old_bukrs = gs_input-value BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          lv_new_value = gs_bukrs_map-new_bukrs.
        ELSE.
          lv_new_value = gs_input-value.
        ENDIF.
      WHEN 'EKORG'.
        READ TABLE gt_ekorg_map INTO DATA(gs_ekorg_map)
          WITH KEY old_ekorg = gs_input-value BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          lv_new_value = gs_ekorg_map-new_ekorg.
        ELSE.
          lv_new_value = gs_input-value.
        ENDIF.
      WHEN OTHERS.
        lv_new_value = gs_input-value.
    ENDCASE.

*     lv_new_value = SWITCH #( gs_input-domname
*                               WHEN 'BUKRS' THEN gt_bukrs_map[ old_bukrs = gs_input-value ]-new_bukrs
*                               WHEN 'EKORG' THEN gt_ekorg_map[ old_ekorg = gs_input-value ]-new_ekorg
*                               ELSE gs_input-value
*                               ).

    CASE gs_input-ctype.

        "For single value
      WHEN 'SINGLE'.

        IF gs_input-sflag = 'N'.       "if entry is not for shared include

          gs_output-progname  = gs_input-objname.
          gs_output-contextid = gs_input-cid.
          gs_output-type      = 'S'.
          gs_output-counter   = 1.
*          gs_output-low       = gs_input-value.
          gs_output-low       = lv_new_value.
          CLEAR: gs_output-sign, gs_output-opti,gs_output-high, gs_output-datadesc.
          APPEND gs_output TO gt_output.

        ELSEIF gs_input-sflag = 'Y'.   "entry for shared include

          gs_output-progname  = gs_input-sincl.
          gs_output-contextid = gs_input-cid.
          gs_output-type      = 'S'.
          gs_output-counter   = 1.
*          gs_output-low       = gs_input-value.
          gs_output-low       = lv_new_value.
          CLEAR: gs_output-sign, gs_output-opti,gs_output-high, gs_output-datadesc.
          APPEND gs_output TO gt_output.

        ENDIF.

      WHEN 'RANGE'.

        IF gs_input-value CA ','.

          CLEAR gt_range.
          SPLIT gs_input-value AT ',' INTO TABLE gt_range.

          lv_count = 0.

          LOOP AT gt_range INTO gs_range.

            lv_count = lv_count + 1.

            IF gs_input-sflag = 'N'.       "if entry is not for shared include
              gs_output-progname  = gs_input-objname.
            ELSEIF gs_input-sflag = 'Y'.   "entry for shared include
              gs_output-progname  = gs_input-sincl.
            ENDIF.

            gs_output-contextid = gs_input-cid.
            gs_output-type      = 'R'.
            gs_output-counter   = lv_count.
            gs_output-sign      = 'I'.
            gs_output-opti      = 'EQ'.
*            gs_output-low       = gs_range.
            CLEAR: lv_new_value,gs_bukrs_map,gs_ekorg_map.
            CASE gs_input-domname .
              WHEN 'BUKRS'.
                READ TABLE gt_bukrs_map INTO gs_bukrs_map
                  WITH KEY old_bukrs = gs_range BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  lv_new_value = gs_bukrs_map-new_bukrs.
                ELSE.
                  lv_new_value = gs_range.
                ENDIF.
              WHEN 'EKORG'.
                READ TABLE gt_ekorg_map INTO gs_ekorg_map
                  WITH KEY old_ekorg = gs_range BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  lv_new_value = gs_ekorg_map-new_ekorg.
                ELSE.
                  lv_new_value = gs_range.
                ENDIF.
              WHEN OTHERS.
                lv_new_value = gs_range.
            ENDCASE.
            gs_output-low       = lv_new_value.

            APPEND gs_output TO gt_output.

            CLEAR:gs_output-high, gs_output-datadesc,gs_output,gs_range.

          ENDLOOP.

        ELSE.

          IF gs_input-sflag = 'N'.       "if entry is not for shared include
            gs_output-progname  = gs_input-objname.
          ELSEIF gs_input-sflag = 'Y'.   "entry for shared include
            gs_output-progname  = gs_input-sincl.
          ENDIF.
          gs_output-contextid = gs_input-cid.
          gs_output-type      = 'R'.
          gs_output-counter   = 1.
          gs_output-sign      = 'I'.
          gs_output-opti      = 'EQ'.
*          gs_output-low       = gs_input-value.
          gs_output-low       = lv_new_value.

          CLEAR:gs_output-high, gs_output-datadesc.
          APPEND gs_output TO gt_output.


        ENDIF.

    ENDCASE.

    CLEAR: gs_input,gs_output,gt_range,gs_range,lv_count.
    CLEAR: lv_new_value,gs_bukrs_map,gs_ekorg_map.
  ENDLOOP.


ENDFORM.                    " POPULATE_FINAL_VALUE
*&---------------------------------------------------------------------*
*& Form value_help
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- P_FILE
*&---------------------------------------------------------------------*
FORM value_help  CHANGING p_path TYPE localfile.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = p_path.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form read_input_files
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM read_input_files .
*  CLEAR
  PERFORM read_file TABLES gt_lot2_input USING p_upfile.
  PERFORM read_file TABLES gt_l2_afapl USING p_l2_afa.
  PERFORM read_file TABLES gt_l2_anlkl USING p_l2_anl.
  PERFORM read_file TABLES gt_l2_bukrs USING p_l2_buk.
  PERFORM read_file TABLES gt_l2_mwskz USING p_l2_mws.
  PERFORM read_file TABLES gt_l2_saknr USING p_l2_sak.
  PERFORM read_file TABLES gt_l2_zlsch USING p_l2_zls.

ENDFORM.

FORM read_file TABLES p_tab USING p_file_in TYPE localfile.
  DATA: lv_file_string TYPE string.
  lv_file_string = p_file_in.
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = lv_file_string
      has_field_separator     = cl_abap_char_utilities=>horizontal_tab
    CHANGING
      data_tab                = p_tab
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
  IF sy-subrc <> 0.
    MESSAGE 'Error reading Lot2 Input files' TYPE 'E'.
  ENDIF.

ENDFORM.
