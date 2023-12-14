*&---------------------------------------------------------------------*
*& Modulpool Z_ASC_EXTRACTOR_CHECK
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
PROGRAM z_asc_extractor_check.
*deleting the line for git test
TABLES: roosource, roosfield, rodeltamt.

DATA: okcode_100 TYPE sy-ucomm,
      okcode_200 TYPE sy-ucomm.

DATA: o_container TYPE REF TO cl_gui_custom_container.

DATA: o_alv TYPE REF TO  cl_gui_alv_grid.

TYPES:          " create working table to store file/upload content
  BEGIN OF asc_extractor_import,
    asc_extractor_import_raw(1024) TYPE c,
  END OF asc_extractor_import.
DATA: asc_extractor_import_raw_table TYPE asc_extractor_import OCCURS 0
    WITH HEADER LINE.

TYPES:          " create table with converted uploaded data
  BEGIN OF asc_extractor_raw_list,
    asc_extractor_field1(256) TYPE c,
    asc_extractor_field2(256) TYPE c,
  END OF asc_extractor_raw_list.
DATA asc_extractor_import_table TYPE asc_extractor_raw_list OCCURS 0
      WITH HEADER LINE.

" define final internal table that will hold the Extractors ASC and the infos to the extractors
TYPES: BEGIN OF asc_extractor_list,
         asc_extractor                       TYPE roosourcer,
         asc_extractor_status                TYPE roobjvers,
         asc_extractor_status_long(256)      TYPE c,
         asc_extractor_delta_mthd            TYPE rogendelta,
         asc_extractor_delta_txtlg           TYPE rstxtlg,
         " field parts of the table
         asc_extractor_field                 TYPE fieldname,
*TBD  asc_extractor_field_status TYPE roobjvers, "no activation info on field level!
         asc_extractor_field_status(1)       TYPE c, "info for Field found / Not found
         asc_extractor_field_status_lng(256) TYPE c, "long text field. no activation info on field level!
       END OF asc_extractor_list.
DATA asc_extractor_table TYPE asc_extractor_list OCCURS 0
      WITH HEADER LINE.

CLASS lcl_event DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING io_alv TYPE REF TO cl_gui_alv_grid.
    METHODS handle_double_click
        FOR EVENT double_click OF cl_gui_alv_grid IMPORTING sender es_row_no e_row e_column.
ENDCLASS.


CLASS lcl_event IMPLEMENTATION.
  METHOD constructor.
    SET HANDLER handle_double_click FOR io_alv.
  ENDMETHOD.
  METHOD handle_double_click .
  ENDMETHOD.
ENDCLASS.

DATA: o_event TYPE REF TO lcl_event.

DATA: PBO_is_INITIAL(1) TYPE c VALUE 'Y'. "needed because AlV events my lead to PBO 200 Re-run and data duplication

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  "routine reused for other screens.
  "Make sure that you change okcode in module and screen
  IF okcode_100 = 'FTC_EXIT'.
    LEAVE PROGRAM.
  ELSEIF okcode_100 = 'FTC_BACK'. "at first screen, back should have the same behavior as exit.
    IF sy-dynnr = 100.
      LEAVE PROGRAM.
    ELSE.
      LEAVE TO CURRENT TRANSACTION.
    ENDIF.
  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  FILE_UPLOAD_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE file_upload_0100 INPUT.

  DATA: p_upload TYPE rlgrap-filename.
  " structure to convert types betten F4_Filename and GUI_Upload
  DATA: in_file TYPE string.

  IF okcode_100 = 'FTC_PB1'.
    CALL FUNCTION 'F4_FILENAME'
*    Exporting
*      program_name = sy-repid
*      dynpro_number = sy-dynnrb
*     field_name = <set the default folder in the file select dialog
      IMPORTING
        file_name = p_upload.
  ENDIF.

*TBD Error Handling if p_upload is inital
* raise message
  IF p_upload IS INITIAL.
    MESSAGE 'No upload file selected.' TYPE 'S' DISPLAY LIKE 'W'.
    CALL SCREEN 100.
  ENDIF.


  " needed type conversion because type from F4_Filename/Importing is different to Gui_Upload/Exporting
  in_file = p_upload.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      Filename                = in_file
      Filetype                = 'ASC'
*     HAS_FIELD_SEPARATOR     = 'X'
*     read_by_line            = 'X'
* Repleacement
    TABLES
      data_tab                = asc_extractor_import_raw_table
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
      OTHERS                  = 17.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    CALL SCREEN 100.
  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'MENU_100'.
  SET TITLEBAR 'SCR_100_T1'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'MENU_100'.
  SET TITLEBAR 'SCR_200_T1'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CONVERT_UPLOAD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE convert_upload_100 INPUT.

  DATA asc_intext(256) TYPE c.
  DATA asc_outtext(256) TYPE c.

  " make sure that semicolon is the coloum separator
  LOOP AT asc_extractor_import_raw_table.
    IF asc_extractor_import_raw_table CA ','.
      REPLACE ALL OCCURRENCES OF ',' IN asc_extractor_import_raw_table WITH ';'.
    ENDIF.
    "in case the field contains a Tab Character, GUI Upload can not convert it to to a symbol and replaces it with with # (HEX09).
    " This leades to wrong handling and download later on
    " Single Tab in a field are consideres as spelling mistake and handled as such.

    asc_intext = asc_extractor_import_raw_table.
    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        intext  = asc_intext
      IMPORTING
        outtext = asc_outtext.

    asc_extractor_import_raw_table = asc_outtext.
    MODIFY asc_extractor_import_raw_table.

  ENDLOOP.


*-----------------------------------------------------------------------------------------------------------------------------------------
*  Split content of asc_extractor_import_RAW_table and prepare for move into CLEAN asc_extractor_import table
*-----------------------------------------------------------------------------------------------------------------------------------------

  LOOP AT asc_extractor_import_raw_table .
    SPLIT asc_extractor_import_raw_table AT ';' INTO
       asc_extractor_import_table-asc_extractor_field1
       asc_extractor_import_table-asc_extractor_field2.
    " all letters need to be upper case, because later select is case senitive. lower case names/letters will not be found.
    TRANSLATE asc_extractor_import_table-asc_extractor_field1 TO UPPER CASE.
    TRANSLATE asc_extractor_import_table-asc_extractor_field2 TO UPPER CASE.
*TBD: check sy-subrc for split
* 0  The segments were passed to the target fields or the internal table without being truncated.
* 4  At least one of the segments was truncated on the right when being passed to the target fields or internal table.
    APPEND asc_extractor_import_table.
  ENDLOOP.
  IF sy-subrc <> 0.
    WRITE: 'asc_extractor_import_raw_table is empty' .
* TBD: END PROGRAM here if the import is empty
  ENDIF.

  " delete empty rows. empty rows can come from csv concersion, for example last row.
  DELETE asc_extractor_import_table WHERE asc_extractor_field1 IS INITIAL.

* TBD: potentially collect/count fields where extractor was missing and raise message to screen.
* TBD: only call screen if no prior erros

  CALL SCREEN 200.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module READ_EXTRACTOR_INFOS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE read_extractor_infos_0200 OUTPUT.
  SET PF-STATUS 'MENU_100'.
  SET TITLEBAR 'SCR_200_T1'.

  IF PBO_is_Initial = 'Y'. "see definition of variable


    " fill new, final itab with clean input from upload
    LOOP AT asc_extractor_import_table.
      asc_extractor_table-asc_extractor = asc_extractor_import_table-asc_extractor_field1.
      asc_extractor_table-asc_extractor_field = asc_extractor_import_table-asc_extractor_field2.
      APPEND asc_extractor_table.
    ENDLOOP.

    " fill asc extractor itab with details from the system
    " here starts core of logic
    LOOP AT asc_extractor_table.
      " selecting with 'A' make the selection faster AND catches the case that the table contains A/D version of the extractor, unsorted.
      " initialy selected without A, and then D was found first, while A came later. Alternative: fetch roosource into itab and sort first
      SELECT * FROM roosource WHERE oltpsource = asc_extractor_table-asc_extractor AND objvers = 'A'.
      ENDSELECT.
      IF sy-subrc = 0.     "  straight forward case. activate ASC extractor found in SAP System (AND selection!)
        " reading extration data to the extractor. INFO ONLY! NOT checking if the extraction type is what ASC wants/needs
        asc_extractor_table-asc_extractor_status = roosource-objvers.
        asc_extractor_table-asc_extractor_status_long = 'Extractor found and activated'.
        asc_extractor_table-asc_extractor_delta_mthd = roosource-delta.

      ELSE.                " extractor not found OR not 'A'. Thus, need to find out now what it is, so we check for ASC extractor only

        SELECT * FROM roosource WHERE oltpsource = asc_extractor_table-asc_extractor.
        ENDSELECT.
        IF sy-subrc = 0. " so extractor was found. set flags and leave loop
          CASE roosource-objvers.
              " when 'A'. "this case should not come up, because we searched for 'A' before and did not find
              " asc_extractor_table-asc_extractor_status_long = 'Extractor found and activated'.
            WHEN 'M'.
              asc_extractor_table-asc_extractor_status_long = 'Extractor modified, not activated'.
            WHEN 'D'.
              asc_extractor_table-asc_extractor_status_long = 'Extractor delivered from this system, not activated'.
            WHEN 'P'.
              asc_extractor_table-asc_extractor_status_long = 'Extractor delivered with patch, not activated'.
            WHEN OTHERS.
              asc_extractor_table-asc_extractor_status_long = 'Edge case. Pls check coding'.
          ENDCASE.
          asc_extractor_table-asc_extractor_status = roosource-objvers.
        ELSE.          " so the ASC extractor name itself was not found. set flags and leave loop
          " potentially spelling mistake OR xls colum header, thus marked with X
          " also mark field status with X as this is an invalid combo of extractor and field
          asc_extractor_table-asc_extractor_status = 'X'.
          asc_extractor_table-asc_extractor_status_long = 'Extractor not found, pls check spelling' .
          asc_extractor_table-asc_extractor_delta_mthd = 'X' .
          asc_extractor_table-asc_extractor_delta_txtlg = 'Extractor not found, pls check spelling'.
*            asc_extractor_table-asc_extractor_field_status = 'X'.
          IF asc_extractor_table-asc_extractor_field IS INITIAL.
            asc_extractor_table-asc_extractor_field_status_lng = 'Extractor Field empty, pls check input'.
            asc_extractor_table-asc_extractor_field_status = 'X'.
          ELSE. " so there is something in the extractor-field / so need to check if this is a valid field name .
            SELECT * FROM roosfield WHERE field =  asc_extractor_table-asc_extractor_field.
            ENDSELECT.
            IF sy-subrc = 0. "so field existits, but extractor is wrong
              asc_extractor_table-asc_extractor_field_status = 'X'.
              asc_extractor_table-asc_extractor_field_status_lng = 'Field found, but not valid combo. pls check Extractor name'.
            ELSE. "not found, so the field must be a spelling mistake
              asc_extractor_table-asc_extractor_field_status = 'X'.
              asc_extractor_table-asc_extractor_field_status_lng = 'Field not found, pls check spelling'.
            ENDIF.
*                   asc_extractor_table-asc_extractor_field_status_lng = 'Not valid combo. pls check Extractor name'.
          ENDIF.
          " spezialfall 'not extractor' ist JETZT abgearbeitet. keine weitere details dazulesen daher: modify itab and go to next step of loop.
          MODIFY asc_extractor_table.
          CONTINUE. "extractor not found. leave current loop (so no rodeltamt or roosfields assessment) and go to next loop entry
        ENDIF.
      ENDIF.

      " get long text describing the delta mechanism, this is for A as well as some D versions, thus at this place of the loop
      " https://sap.erpref.com/?schema=ERP6EHP7&module_id=73&table=RODELTAMT
      SELECT * FROM rodeltamt WHERE delta = roosource-delta AND langu = 'E' .
        asc_extractor_table-asc_extractor_delta_txtlg = rodeltamt-txtlg.
      ENDSELECT.

* --------------------------
* ROOSFIELD starts here / only if extractor exists (regardless of actionat status)
* --------------------------

      IF asc_extractor_table-asc_extractor_field IS NOT INITIAL. " check if fields are filled
        " get extractor field data / also extractors status D have fields!
        SELECT * FROM roosfield
           WHERE oltpsource = asc_extractor_table-asc_extractor AND
           field =  asc_extractor_table-asc_extractor_field.
        ENDSELECT.
        IF sy-subrc = 0.
          " combo extractor name and field was found / best case
          asc_extractor_table-asc_extractor_field_status = 'Y'.
          asc_extractor_table-asc_extractor_field_status_lng = 'Field found and belongs to Extractor'.

        ELSE.
          " combo extractor name and field was not found. must extractor or field wrong field
          " the exception of "wrong extrator" was handled above (incl. modify and continue). We are not getting to this lines if the extractor is wrong. THUS, here it can only be wrong field.
          asc_extractor_table-asc_extractor_field_status = 'N'.
          asc_extractor_table-asc_extractor_field_status_lng = 'Field not found, pls check spelling'.
        ENDIF.
      ELSE. " extractor field is empty
        asc_extractor_table-asc_extractor_field_status = 'X'.
        asc_extractor_table-asc_extractor_field_status_lng = 'Extractor Field empty, pls check input'.
      ENDIF.

      MODIFY asc_extractor_table.

    ENDLOOP.


  ENDIF. " PBO_is_Initial still "y" because first run through

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
* only handle key events here, not screen interactions like "PB_DWNLD"

  IF okcode_200 = 'FTC_EXIT'.
    CLEAR: o_alv.
    LEAVE PROGRAM.
  ELSEIF okcode_200 = 'FTC_BACK'.
    CLEAR: o_alv.
    LEAVE TO CURRENT TRANSACTION.
  ENDIF.


ENDMODULE.


*&---------------------------------------------------------------------*
*& Module ALV_DISPLAY_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE alv_display_0200 OUTPUT.
  SET PF-STATUS 'MENU_100'.
  SET TITLEBAR 'SCR_200_T1'.

  IF PBO_is_INITIAL = 'Y'.

    CHECK o_alv IS INITIAL.

    CREATE OBJECT o_container EXPORTING container_name = 'CONTAINER'. "name aus dem screenpainter

* https://codezentrale.de/abap-alv-grid-einfaches-beispiel-zur-anzeige-von-daten-in-einem-alv-grid-cl_gui_alv_grid/
* ALV-Gitter-Objekt erzeugen
    o_alv = NEW cl_gui_alv_grid( i_parent      = o_container  " in default container einbetten
                                       i_appl_events = abap_true ).                     " Ereignisse als Applikationsevents registrieren


* Feldkatalog automatisch durch SALV-Objekte erstellen lassen
    DATA: o_salv TYPE REF TO cl_salv_table.

    cl_salv_table=>factory( IMPORTING
                              r_salv_table = o_salv
                            CHANGING
                              t_table      = asc_extractor_table[] ).

    DATA(it_fcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = o_salv->get_columns( )
                                                                       r_aggregations = o_salv->get_aggregations( ) ).


    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'asc_extractor_list'
      CHANGING
        ct_fieldcat      = it_fcat.

    LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat_i>).
      CASE <ls_fcat_i>-fieldname.
        WHEN 'ASC_EXTRACTOR'.
          <ls_fcat_i>-reptext = 'ASC Extractor'.
        WHEN 'ASC_EXTRACTOR_STATUS'.
          <ls_fcat_i>-reptext = 'Activation Status'.
        WHEN 'ASC_EXTRACTOR_STATUS_LONG'.
          <ls_fcat_i>-reptext = 'Activation Long Text'.
        WHEN 'ASC_EXTRACTOR_DELTA_MTHD'.
          <ls_fcat_i>-reptext = 'Delta Extraction Type'.
        WHEN 'ASC_EXTRACTOR_DELTA_TXTLG'.
          <ls_fcat_i>-reptext = 'Delta Extraction Long Text'.
        WHEN 'ASC_EXTRACTOR_FIELD'.
          <ls_fcat_i>-reptext = 'Extracted Field'.
        WHEN 'ASC_EXTRACTOR_FIELD_STATUS'.
          <ls_fcat_i>-reptext = 'Status of Extracted Field'.
        WHEN 'ASC_EXTRACTOR_FIELD_STATUS_LNG'.
          <ls_fcat_i>-reptext = 'Long Text Extracted Field'.
      ENDCASE.

      <ls_fcat_i>-scrtext_s = <ls_fcat_i>-reptext.
      <ls_fcat_i>-scrtext_m = <ls_fcat_i>-reptext.
      <ls_fcat_i>-scrtext_l = <ls_fcat_i>-reptext.
      CLEAR <ls_fcat_i>-key.

    ENDLOOP.


* Layout des ALV setzen
    DATA(lv_layout) = VALUE lvc_s_layo( zebra      = abap_true             " ALV-Control: Alternierende Zeilenfarbe (Zebramuster)
                                        cwidth_opt = 'A'                   " ALV-Control: Spaltenbreite optimieren
                                        grid_title = 'Analysis of mandatory Extractors for ASC' ). " ALV-Control: Text der Titelzeile

* ALV anzeigen
    o_alv->set_table_for_first_display( EXPORTING
                                          i_bypassing_buffer = abap_false  " Puffer ausschalten
                                          i_save             = 'A'         " Anzeigevariante sichern
                                          is_layout          = lv_layout   " Layout
                                        CHANGING
                                          it_fieldcatalog    = it_fcat     " Feldkatalog
                                          it_outtab          = asc_extractor_table[] ). " Ausgabetabelle

* Focus auf ALV setzen
    cl_gui_alv_grid=>set_focus( control = o_alv ).



    o_event = NEW lcl_event( o_alv ).

    PBO_is_INITIAL = 'N'. " set to N after first complete runthrough of PBO modules.
  ENDIF.

ENDMODULE.
