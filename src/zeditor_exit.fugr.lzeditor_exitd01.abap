
*----------------------------------------------------------------------*
*       CLASS lcl_messages DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_messages DEFINITION INHERITING FROM cx_static_check FINAL.
  PUBLIC SECTION.
    DATA: lv_text         TYPE string,
          lv_type         TYPE char1,
          lv_display_like TYPE char1.

    METHODS:
      constructor      IMPORTING iv_text         TYPE string OPTIONAL
                                 iv_type         TYPE char1
                                 iv_display_like TYPE char1  OPTIONAL,
      display_msg.
ENDCLASS.                    "lcl_messages DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_alv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv DEFINITION ABSTRACT .

  PUBLIC SECTION.

*** Methods***
    METHODS :
      constructor         IMPORTING iv_container         TYPE c
                                    iv_tabname           TYPE tabname
                                    iv_tabtype           TYPE string
                                    is_layout            TYPE lvc_s_layo     OPTIONAL
                                    iv_set_toolbar       TYPE char1          OPTIONAL
                                    iv_set_toolbar_click TYPE char1          OPTIONAL
                                    iv_reg_edit_enter    TYPE char1          OPTIONAL
                                    iv_reg_edit_event    TYPE char1          OPTIONAL,

      first_display       ABSTRACT,

      refresh_alv         ABSTRACT
        IMPORTING it_data         TYPE STANDARD TABLE OPTIONAL
                  iv_soft_refresh TYPE char1          OPTIONAL
                    PREFERRED PARAMETER  it_data,

      check_ddictype      IMPORTING iv_typename TYPE typename
                          EXPORTING ev_type     TYPE char1
                          RAISING   lcl_messages,

      fill_tab_data       ABSTRACT
        IMPORTING it_data TYPE tty_dd03vt,
      sync_alv_data       ,

      clean_up.

  PROTECTED SECTION.
*  ***  Object Reference***
    DATA: o_ref_cont      TYPE REF TO cl_gui_custom_container,
          o_ref_alv       TYPE REF TO cl_gui_alv_grid,
          o_ref_alv_tbmgr TYPE REF TO cl_alv_grid_toolbar_manager.

*** Currenct Alv Class Table and Type Reference
    DATA : lv_my_alvtabname     TYPE tabname,
           lv_my_alvtabtype     TYPE string,
           ls_layout            TYPE lvc_s_layo,
           lv_set_toolbar       TYPE char1,
           lv_set_toolbar_click TYPE char1,
           lv_reg_edit_enter    TYPE char1,
           lv_reg_edit_event    TYPE char1
           .
    METHODS:
      initialize_display  CHANGING ct_outtab            TYPE STANDARD TABLE,

      on_toolbar          FOR EVENT toolbar             OF cl_gui_alv_grid
        IMPORTING e_object,

      on_toolbar_click    FOR EVENT after_user_command  OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      fill_tmp_data       IMPORTING io_chg_data TYPE REF TO cl_alv_changed_data_protocol
                          CHANGING  ct_data     TYPE STANDARD TABLE,

      move_itab_records   IMPORTING i_ucomm    TYPE syucomm
                                    it_row     TYPE lvc_t_row
                                    iv_tabname TYPE tabname
                                    iv_tabtype TYPE string,

      display_error       IMPORTING io_chg_data TYPE REF TO cl_alv_changed_data_protocol
                                    it_alv_msgs TYPE tty_alv_protocol_msgs,

      alv_data_validation IMPORTING it_data         TYPE STANDARD TABLE
                                    ir_data_changed TYPE REF TO cl_alv_changed_data_protocol
                          EXPORTING et_alv_msg      TYPE tty_alv_protocol_msgs,

      handle_data_changed FOR EVENT data_changed        OF cl_gui_alv_grid
        IMPORTING er_data_changed                                  .


ENDCLASS.                    "lcl_alv DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_types_alv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_types_alv DEFINITION INHERITING FROM lcl_alv FINAL.

  PUBLIC SECTION .
    DATA:   t_types      TYPE STANDARD TABLE OF ty_type READ-ONLY,
            t_types_meta TYPE STANDARD TABLE OF ty_types_meta READ-ONLY.
*** Methods
    METHODS :
      first_display        REDEFINITION ,

      refresh_alv          REDEFINITION,

      fill_tab_data        REDEFINITION,

      move_to_types_meta,

      move_frm_types_meta,

      del_frm_types_meta.

  PROTECTED SECTION.

    METHODS :

      alv_data_validation  REDEFINITION,

      handle_data_changed  REDEFINITION.

ENDCLASS.                    "lcl_types_alv DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_data_alv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_data_alv DEFINITION INHERITING FROM lcl_alv  FINAL.

  PUBLIC SECTION.
    DATA : t_data        TYPE STANDARD TABLE OF ty_data READ-ONLY.
*** Methods
    METHODS :
      first_display       REDEFINITION,

      refresh_alv         REDEFINITION,

      fill_tab_data       REDEFINITION,

      set_dropdown  .

  PROTECTED SECTION.

    METHODS :

      alv_data_validation REDEFINITION,

      handle_data_changed REDEFINITION.

ENDCLASS.                    "lcl_data_alv DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_selscr_alv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_selscr_alv DEFINITION INHERITING FROM lcl_alv FINAL.

  PUBLIC SECTION.

    DATA : t_selscr      TYPE STANDARD TABLE OF ty_selscr READ-ONLY.
*** Methods
    METHODS :
      first_display       REDEFINITION,

      refresh_alv         REDEFINITION,

      fill_tab_data       REDEFINITION,

      set_dropdown        .

  PROTECTED SECTION.

    METHODS :

      alv_data_validation REDEFINITION,

      handle_data_changed REDEFINITION.
ENDCLASS.                    "lcl_data_alv DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_sqlfields_alv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_sqlfields_alv DEFINITION INHERITING FROM lcl_alv FINAL.

  PUBLIC SECTION.

    DATA :t_sqlfields     TYPE STANDARD TABLE OF ty_sqlfields READ-ONLY.

    METHODS:

      first_display        REDEFINITION,

      refresh_alv          REDEFINITION,

      fill_tab_data        REDEFINITION,
      fill_tab_frm_type    IMPORTING iv_typename   TYPE char20
                                     it_types_meta TYPE tty_types_meta.

  PROTECTED SECTION.
    METHODS:
      on_toolbar           REDEFINITION.

ENDCLASS.                    "lcl_sqlfields_alv DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_sqlwhr_alv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_sqlwhr_alv DEFINITION INHERITING FROM lcl_alv FINAL.

  PUBLIC SECTION.
    DATA : t_sqlwhr       TYPE STANDARD TABLE OF ty_sqlwhr READ-ONLY.

    METHODS :

      first_display         REDEFINITION,

      refresh_alv           REDEFINITION,

      fill_tab_data         REDEFINITION,

      set_dropdown,
      fill_tab_frm_type     IMPORTING iv_typename   TYPE char20
                                      it_types_meta TYPE tty_types_meta.

  PROTECTED SECTION.
    METHODS :
      on_toolbar          REDEFINITION,

      handle_data_changed REDEFINITION.

ENDCLASS.                    "lcl_sqlwhr_alv DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_main DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.
**  Constants**
    CONSTANTS :
      BEGIN OF c,
        x                   TYPE char1   VALUE 'X',
*** Table and types.
        my_fieldstabname    TYPE tabname VALUE 'T_TYPES',
        my_fieldstabtype    TYPE string  VALUE 'TY_TYPE',
        my_datatabname      TYPE tabname VALUE 'T_DATA',
        my_datatabtype      TYPE string  VALUE 'TY_DATA',
        my_selscrtabtype    TYPE string  VALUE 'TY_SELSCR',
        my_selscrtabname    TYPE tabname VALUE 'T_SELSCR',
        my_sqlfieldstabname TYPE tabname VALUE 'T_SQLFIELDS',
        my_sqlfieldstype    TYPE string  VALUE 'TY_SQLFIELDS',
        my_sqlwhrtabname    TYPE tabname VALUE 'T_SQLWHR',
        my_sqlwhrtype       TYPE string  VALUE 'TY_SQLWHR',
****  COde Related Constants***
        data                TYPE char4   VALUE 'DATA',
        colon               TYPE char1   VALUE ':',
        types               TYPE char5   VALUE 'TYPES',
        include             TYPE char7   VALUE 'INCLUDE',
        comma               TYPE char1   VALUE ',',
        type                TYPE char4   VALUE 'TYPE',
        standard_tab        TYPE string  VALUE 'STANDARD TABLE OF',
        period              TYPE char1   VALUE '.',
        begin               TYPE string  VALUE 'Begin OF',
        end                 TYPE string  VALUE 'END OF',
        ref_to              TYPE string  VALUE 'REF TO',
        parameters          TYPE string  VALUE 'PARAMETERS',
        seloption           TYPE string  VALUE 'SELECT-OPTIONS',
        select              TYPE string  VALUE 'SELECT',
        single              TYPE string  VALUE 'SINGLE',
        from                TYPE string  VALUE 'FROM',
        into                TYPE string  VALUE 'INTO',
        table               TYPE string  VALUE 'TABLE',
        where               TYPE string  VALUE 'WHERE',
        upto                TYPE string  VALUE 'UP TO',
        rows                TYPE string  VALUE 'ROWS',
        and                 TYPE string  VALUE 'AND',
        or                  TYPE string  VALUE 'OR',
      END OF c.

*** ALV Class References
    DATA :
      o_ref_types_alv     TYPE REF TO lcl_types_alv,
      o_ref_data_alv      TYPE REF TO lcl_data_alv,
      o_ref_selscr_alv    TYPE REF TO lcl_selscr_alv,
      o_ref_sqlfields_alv TYPE REF TO lcl_sqlfields_alv,
      o_ref_sqlwhr_alv    TYPE REF TO lcl_sqlwhr_alv.

** Methods
    METHODS:

      constructor,
      gui_init            EXPORTING et_buffer   TYPE rswsourcet,
      process_ucomm       IMPORTING iv_ucomm TYPE ok
                                    iv_dynnr TYPE sydynnr
                          RAISING   lcl_messages,
      validation          IMPORTING i_ucomm  TYPE ok
                                    iv_dynnr TYPE sydynnr
                          RAISING   lcl_messages,

      build_code          RAISING   lcl_messages,

      set_prefixes,

      build_fieldcat      IMPORTING iv_tabname  TYPE tabname
                          EXPORTING et_fieldcat TYPE lvc_t_fcat,

      get_tabfields       IMPORTING iv_tabname TYPE tabname
                          EXPORTING et_data    TYPE tty_dd03vt,

      field_select        IMPORTING it_data   TYPE tty_dd03vt
                          CHANGING  ct_fields TYPE tty_fields ,

      get_tabname         CHANGING cv_tabname TYPE tabname
                          RAISING  lcl_messages,

      set_values_ddown    IMPORTING iv_dynnr    TYPE sydynnr,

      copy_to_clipboard   RAISING   lcl_messages,

      clean_up ,
      ret_actual_typname  IMPORTING iv_is_global       TYPE char1
                                    iv_typename        TYPE typename
                          RETURNING VALUE(ev_typename) TYPE string.
  PRIVATE SECTION.
    DATA :
      lt_typenames TYPE STANDARD TABLE OF string,
      lt_varnames  TYPE STANDARD TABLE OF string.
    METHODS :
      recalibrate_ucomm   CHANGING  cv_ucomm          TYPE ok,
      maintain_types_data IMPORTING iv_ucomm          TYPE ok,
      get_alv_data        IMPORTING iv_ucomm          TYPE ok
                                    iv_tabname        TYPE tabname OPTIONAL,
      add_type_code       RAISING   lcl_messages,
      has_inctype         IMPORTING it_types_meta     TYPE tty_types_meta
                                    iv_index          TYPE syindex
                          RETURNING VALUE(ev_is_inc)  TYPE abap_bool,

      has_autodata_dec    IMPORTING it_types_meta     TYPE tty_types_meta
                          RETURNING VALUE(ev_autodec) TYPE abap_bool,

      ret_typ_begcode     IMPORTING iv_types_meta     TYPE ty_types_meta
                                    iv_hasinc         TYPE abap_bool
                          RETURNING VALUE(ev_code)    TYPE LINE OF rswsourcet,
      ret_warea_begcode   RETURNING VALUE(ev_code)    TYPE LINE OF rswsourcet,
      ret_warea_endcode   RETURNING VALUE(ev_code)    TYPE LINE OF rswsourcet,
      ret_typ_endcode     IMPORTING iv_types_meta     TYPE ty_types_meta
                                    iv_hasinc         TYPE abap_bool
                          RETURNING VALUE(ev_code)    TYPE LINE OF rswsourcet,

      ret_typ_pf          IMPORTING iv_is_global      TYPE char1
                          RETURNING VALUE(ev_pf)      TYPE char4,


      ret_actual_itabname IMPORTING iv_is_global      TYPE char1
                                    iv_tabname        TYPE typename
                          RETURNING VALUE(ev_tabname) TYPE string,
      ret_actual_objname  IMPORTING iv_is_global      TYPE char1
                                    iv_objname        TYPE typename
                          RETURNING VALUE(ev_objname) TYPE string,
      ret_actual_varname  IMPORTING iv_is_global      TYPE char1
                                    iv_varname        TYPE char30
                          RETURNING VALUE(ev_varname) TYPE string,

      ret_var_pf          IMPORTING iv_is_global      TYPE char1

                          RETURNING VALUE(ev_pf)      TYPE char4,

      ret_itab_pf         IMPORTING iv_is_global      TYPE char1
                          RETURNING VALUE(ev_pf)      TYPE char4,

      ret_obj_pf          IMPORTING iv_is_global      TYPE char1
                          RETURNING VALUE(ev_pf)      TYPE char4,

      add_data_code         RAISING lcl_messages,

      add_selscr_code       RAISING lcl_messages,

      add_sql_code          RAISING lcl_messages,

      launch_help,

      get_types_and_vars  EXPORTING et_types          TYPE string_table
                                    et_vars           TYPE string_table
                                    et_objs           TYPE string_table
                                    et_tables         TYPE string_table ,
      set_sqltab_ddown,
      set_fr_all_entries_ddown
                          IMPORTING it_tables         TYPE string_table,
      set_in_var_ddown    IMPORTING it_vars           TYPE string_table
                                    it_tables         TYPE string_table,
      set_local_types_ddown
                          IMPORTING it_types          TYPE string_table,

      set_scr_ddown_val   IMPORTING iv_id             TYPE vrm_id
                                    it_val            TYPE vrm_values.

ENDCLASS.                    "lcl_main DEFINITION
