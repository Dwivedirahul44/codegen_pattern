*----------------------------------------------------------------------*
*       CLASS lcl_alv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv IMPLEMENTATION .
  METHOD constructor.

    CHECK me->o_ref_cont IS INITIAL.

    CREATE OBJECT me->o_ref_cont
      EXPORTING
        container_name              = iv_container "'O_FIELD_CON'  " Name of the Screen CustCtrl Name to Link Container To
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF sy-subrc <> 0.

    ELSE.

      CREATE OBJECT me->o_ref_alv
        EXPORTING
          i_parent          = me->o_ref_cont   " Parent Container
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4.

    ENDIF.

**** Set Class Variable.***
    lv_my_alvtabname     = iv_tabname.
    lv_my_alvtabtype     = iv_tabtype.
    lv_set_toolbar       = iv_set_toolbar     .
    lv_set_toolbar_click =  iv_set_toolbar_click.
    lv_reg_edit_enter    = iv_reg_edit_enter   .
    lv_reg_edit_event    = iv_reg_edit_event   .
    ls_layout            = is_layout.


  ENDMETHOD.                    "constructor
  METHOD initialize_display.
*   Internal Table*
    DATA : lt_fcat TYPE lvc_t_fcat.

**<<< Main Logic >>>
    CHECK  me->o_ref_alv IS NOT INITIAL.

    CALL METHOD o_ref_main->build_fieldcat
      EXPORTING
        iv_tabname  = lv_my_alvtabname "'T_FIELDS'
      IMPORTING
        et_fieldcat = lt_fcat.

** Set tool Bar event
    IF lv_set_toolbar IS NOT INITIAL.
      SET HANDLER me->on_toolbar FOR me->o_ref_alv.
    ENDIF.

*   Set tool Bar click.
    IF lv_set_toolbar_click IS NOT INITIAL.

      IF me->o_ref_alv_tbmgr IS INITIAL.
        CREATE OBJECT me->o_ref_alv_tbmgr
          EXPORTING
            io_alv_grid = me->o_ref_alv.
      ENDIF.

      SET HANDLER me->on_toolbar_click FOR me->o_ref_alv.
    ENDIF.

    IF lv_reg_edit_enter IS NOT INITIAL
    OR lv_reg_edit_event IS NOT INITIAL.
* set editable cells to ready for input
      CALL METHOD me->o_ref_alv->set_ready_for_input
        EXPORTING
          i_ready_for_input = 1.
*    Set Enter as edit event.
      IF lv_reg_edit_enter IS NOT INITIAL.
        CALL METHOD me->o_ref_alv->register_edit_event
          EXPORTING
            i_event_id = cl_gui_alv_grid=>mc_evt_enter
          EXCEPTIONS
            error      = 1
            OTHERS     = 2.
      ENDIF.

*    Set Modification as edit event.
      IF lv_reg_edit_event IS NOT INITIAL.
        CALL METHOD me->o_ref_alv->register_edit_event
          EXPORTING
            i_event_id = cl_gui_alv_grid=>mc_evt_modified
          EXCEPTIONS
            error      = 1
            OTHERS     = 2.
      ENDIF.

    ENDIF.

*   Call alv
    CALL METHOD me->o_ref_alv->set_table_for_first_display
      EXPORTING
        is_layout                     = ls_layout
      CHANGING
        it_outtab                     = ct_outtab
        it_fieldcatalog               = lt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ENDMETHOD.                    "initialize_display

  METHOD on_toolbar.
**  Work Area**
    DATA ls_toolbar TYPE stb_button.

**<<< Main Logic >>>
    REFRESH e_object->mt_toolbar.

    ls_toolbar-icon      = icon_create.
    ls_toolbar-butn_type = 0.
    ls_toolbar-function  = '&LOCAL&APPEND'.
    ls_toolbar-quickinfo = 'Append Row'.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    ls_toolbar-icon      = icon_insert_row.
    ls_toolbar-butn_type = 0.
    ls_toolbar-function  = '&LOCAL&INSERT_ROW'.
    ls_toolbar-quickinfo = 'Insert Row'.

    APPEND ls_toolbar TO e_object->mt_toolbar.

    ls_toolbar-icon      = icon_delete_row.
    ls_toolbar-butn_type = 0.
    ls_toolbar-function  = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    ls_toolbar-quickinfo = 'Delete Row'.

    APPEND ls_toolbar TO e_object->mt_toolbar.

    ls_toolbar-icon      = icon_previous_value.
    ls_toolbar-butn_type = 0.
    ls_toolbar-function  = 'DOWN'.
    ls_toolbar-quickinfo = 'Move Down'.

    APPEND ls_toolbar TO e_object->mt_toolbar.

    ls_toolbar-icon      = icon_total_down.
    ls_toolbar-butn_type = 0.
    ls_toolbar-function  = 'LAST'.
    ls_toolbar-quickinfo = 'Move to last'.

    APPEND ls_toolbar TO e_object->mt_toolbar.

    ls_toolbar-icon      = icon_next_value.
    ls_toolbar-butn_type = 0.
    ls_toolbar-function  = 'UP'.
    ls_toolbar-quickinfo = 'Move Up'.

    APPEND ls_toolbar TO e_object->mt_toolbar.

    ls_toolbar-icon      = icon_total_up.
    ls_toolbar-butn_type = 0.
    ls_toolbar-function  = 'FIRST'.
    ls_toolbar-quickinfo = 'Move to first'.

    APPEND ls_toolbar TO e_object->mt_toolbar.
  ENDMETHOD.                    "on_toolbar
  METHOD on_toolbar_click.
** Variable **
    DATA: lv_row_index TYPE i.

**  Work Area**
    DATA: ls_row       TYPE lvc_s_row.

*   Internal Table*
    DATA: lt_row       TYPE lvc_t_row.

**<<< Main Logic >>>

    REFRESH lt_row[].

    CALL METHOD me->o_ref_alv->get_selected_rows
      IMPORTING
        et_index_rows = lt_row[].

    IF lt_row IS INITIAL.
      CALL METHOD me->o_ref_alv->get_current_cell
        IMPORTING
          e_row = lv_row_index. " Row on Grid

      IF lv_row_index <> 0.
        ls_row-index = lv_row_index.
        APPEND ls_row TO lt_row[].
      ENDIF.

    ENDIF.

    CHECK lt_row[] IS NOT INITIAL.

    IF e_ucomm = 'UP'
    OR e_ucomm = 'DOWN'
    OR e_ucomm = 'LAST'
    OR e_ucomm = 'FIRST'.

      move_itab_records(
      EXPORTING
        i_ucomm    = e_ucomm
        it_row     = lt_row
        iv_tabname = me->lv_my_alvtabname
        iv_tabtype = me->lv_my_alvtabtype
        ).

    ENDIF.

    me->o_ref_alv->refresh_table_display( ).

  ENDMETHOD.                    "on_toolbar_click

  METHOD alv_data_validation.
** Meant to Be Blank and implemented suitably by Subclass
  ENDMETHOD.                    "alv_data_validation

  METHOD handle_data_changed.
** Meant to Be Blank and implemented suitably by Subclass
  ENDMETHOD.                    "handle_data_changed

  METHOD fill_tmp_data.

** Variable **
    DATA: lv_row     TYPE syindex.

**  Work Area**
    DATA: ls_good    TYPE lvc_s_modi.

**  Field Symbols**

    FIELD-SYMBOLS :
         <lfs_any>   TYPE any,
         <lfs_data>  TYPE any.

**<<< Main Logic >>>

    LOOP AT io_chg_data->mt_good_cells INTO ls_good.

      IF lv_row <> ls_good-row_id .

        READ TABLE ct_data ASSIGNING <lfs_data> INDEX ls_good-row_id.

        IF syst-subrc IS NOT INITIAL.

          INSERT INITIAL LINE INTO  ct_data INDEX ls_good-row_id ASSIGNING <lfs_data>.

        ENDIF.

        lv_row = ls_good-row_id.

      ENDIF.

      ASSIGN COMPONENT ls_good-fieldname
          OF STRUCTURE <lfs_data>
                    TO <lfs_any>.

      CHECK syst-subrc IS INITIAL.

      <lfs_any> = ls_good-value.

    ENDLOOP.


  ENDMETHOD.                    "fill_tmp_data
  METHOD display_error.
**  Work Area**

    DATA: ls_alv_protocol_msg TYPE ty_alv_protocol_msg.

**<<< Main Logic >>>

    LOOP AT it_alv_msgs INTO ls_alv_protocol_msg.

      CALL METHOD io_chg_data->add_protocol_entry
        EXPORTING
          i_msgid     = ls_alv_protocol_msg-msgid
          i_msgno     = ls_alv_protocol_msg-msgno
          i_msgty     = ls_alv_protocol_msg-msgty
          i_msgv1     = ls_alv_protocol_msg-msg
          i_fieldname = ls_alv_protocol_msg-fieldname
          i_row_id    = ls_alv_protocol_msg-row_id.

    ENDLOOP.


  ENDMETHOD.                    "display_error
  METHOD check_ddictype.

**  Work Area**
    DATA: ls_header TYPE x030l.

** object Reference**
    DATA: lo_desc TYPE REF TO cl_abap_typedescr.

    cl_abap_typedescr=>describe_by_name(
     EXPORTING
       p_name         = iv_typename    " Type name
       RECEIVING
       p_descr_ref   = lo_desc   " Dictionary Header
     EXCEPTIONS
       type_not_found = 1
       OTHERS         = 2
       ).

    IF syst-subrc IS NOT INITIAL.

      RAISE EXCEPTION TYPE lcl_messages
        EXPORTING
          iv_text = 'Input Not Found in Repository'
          iv_type = 'E'.

    ENDIF.

    ev_type = 'X'.

    lo_desc->get_ddic_header(
    RECEIVING
      p_header     = ls_header    " Dictionary Header
    EXCEPTIONS
      not_found    = 1
      no_ddic_type = 2
      OTHERS       = 3
      ).

    CHECK syst-subrc IS INITIAL.

    IF  ls_header-tabtype = 'J'
    AND ls_header-tabform = 'J' .

      ev_type = 'S'.

    ELSEIF (    ls_header-tabtype = 'T'
             OR ls_header-tabform = 'T' ) .

      ev_type = 'T'.

    ENDIF.


  ENDMETHOD.                    "check_ddictype
  METHOD move_itab_records.

** Variable **
    DATA : lv_next_index     TYPE sytabix,
           lv_curr_index     TYPE sytabix.

**  Work Area**
    DATA : ls_row            TYPE lvc_s_row.

** object Reference**
    DATA : lo_var            TYPE REF TO data,
           lo_vartab         TYPE REF TO data.

**  Field Symbols**
    FIELD-SYMBOLS :
           <lfs_curr_fields> TYPE any,
           <lfs_var>         TYPE any,
           <lfs_tab>         TYPE STANDARD TABLE,
           <lfs_vartab>      TYPE STANDARD TABLE.


**<<< Main Logic >>>

    ASSIGN (iv_tabname) TO <lfs_tab>.
    CHECK <lfs_tab> IS ASSIGNED.

    CREATE DATA lo_var TYPE (iv_tabtype).
    CHECK lo_var IS BOUND.

    ASSIGN lo_var->* TO <lfs_var>.

    CREATE DATA lo_vartab TYPE STANDARD TABLE OF (iv_tabtype).

    CHECK lo_vartab IS BOUND.

    ASSIGN lo_vartab->* TO <lfs_vartab>.

    CHECK <lfs_vartab> IS ASSIGNED.

    <lfs_vartab>  = <lfs_tab>.

    LOOP AT it_row INTO ls_row.

      READ TABLE <lfs_vartab> ASSIGNING <lfs_curr_fields> INDEX ls_row-index.

      CHECK syst-subrc IS INITIAL.

      lv_curr_index = ls_row-index.

      CASE i_ucomm.
        WHEN 'UP'.
          CHECK lv_curr_index > 1.
          lv_next_index = lv_curr_index - 1.

        WHEN 'DOWN'.
          CHECK lv_curr_index < lines( <lfs_tab> ).
          lv_next_index = lv_curr_index + 1.

        WHEN 'LAST'.
          CHECK lv_curr_index <> lines( <lfs_tab> ).
          lv_next_index = lines( <lfs_tab> ).

        WHEN 'FIRST'.
          CHECK lv_curr_index > 1.
          lv_next_index = 1.
      ENDCASE.

      <lfs_var> = <lfs_curr_fields>.

      UNASSIGN <lfs_curr_fields>.

      DELETE <lfs_vartab>  INDEX lv_curr_index.

      INSERT <lfs_var> INTO <lfs_vartab> INDEX lv_next_index.

    ENDLOOP.

    me->refresh_alv( <lfs_vartab> ).

  ENDMETHOD.                    "move_itab_records
  METHOD sync_alv_data.

**<<< Main Logic >>>
    CALL METHOD o_ref_alv->check_changed_data( ).

  ENDMETHOD.                    "sync_alv_data

  METHOD clean_up.

**<<< Main Logic >>>
    CALL METHOD me->o_ref_alv->finalize.
    CALL METHOD me->o_ref_alv->free.
    CALL METHOD me->o_ref_cont->finalize.
    CALL METHOD me->o_ref_cont->free.

  ENDMETHOD.                    "clean_up
ENDCLASS.                    "lcl_alv IMPLEMENTATION

"lcl_alv IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS lcl_types_alv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_types_alv IMPLEMENTATION.

  METHOD first_display.

    me->initialize_display(
    CHANGING
      ct_outtab = t_types[]
      ).

    SET HANDLER handle_data_changed FOR o_ref_alv.

  ENDMETHOD.                    "first_display

  METHOD refresh_alv.

**<<< Main Logic >>>

    IF iv_soft_refresh IS INITIAL.
      t_types[] = it_data[].
    ENDIF.
    o_ref_alv->refresh_table_display( ).

  ENDMETHOD.                    "refresh_alv

  METHOD fill_tab_data.

**  Work Area**
    DATA : ls_row      TYPE LINE OF salv_t_row,
           ls_types    TYPE ty_type,
           ls_dd03vt   TYPE ty_dd03vt,
           ls_fields   TYPE ty_fields,
           ls_tabname  TYPE tabname.
    DATA : lt_fields   TYPE STANDARD TABLE OF ty_fields.
    DATA : lt_dd03vt   TYPE STANDARD TABLE OF ty_dd03vt.
**<<< Main Logic >>>


    LOOP AT t_types INTO ls_types.

      ls_fields-fieldname = ls_types-fieldname.
      ls_fields-rollname  = ls_types-rollname.
      ls_fields-tabname   = ls_types-tabname.

      APPEND ls_fields TO lt_fields.

    ENDLOOP.

    o_ref_main->field_select(
    EXPORTING
      it_data    = it_data
    CHANGING
      ct_fields   = lt_fields
      ).

    lt_dd03vt[] = it_data[].

    SORT lt_dd03vt BY tabname
                      fieldname
                      rollname.

*   Fill Fields table with selected recods.

    LOOP AT lt_fields INTO ls_fields.

      IF ls_tabname <> ls_fields-tabname.

        ls_tabname = ls_fields-tabname.

        READ TABLE t_types TRANSPORTING NO FIELDS WITH KEY tabname = ls_fields-tabname.

        IF  syst-subrc IS INITIAL.
          DELETE t_types WHERE tabname = ls_fields-tabname.
        ENDIF.

      ENDIF.

      CLEAR ls_types.
      READ TABLE lt_dd03vt INTO ls_dd03vt WITH KEY tabname   = ls_fields-tabname
                                                   fieldname = ls_fields-fieldname
                                                   rollname  = ls_fields-rollname
                                                   BINARY SEARCH.

      CHECK syst-subrc IS  INITIAL.

      ls_types-tabname   = ls_dd03vt-tabname.
      ls_types-rollname  = ls_dd03vt-rollname.
      ls_types-position  = ls_dd03vt-position.
      ls_types-fieldname = ls_dd03vt-fieldname.
      ls_types-ddtext    = ls_dd03vt-ddtext.

      APPEND ls_types TO t_types.

    ENDLOOP.


    IF t_types IS NOT INITIAL.
* Refresh ALV display with New data.
      me->refresh_alv( t_types ).
    ENDIF.

  ENDMETHOD.                    "fill_alv_data
  METHOD handle_data_changed .
** Variable **
    DATA: lv_tabix TYPE sytabix.

*   Internal Table*
    DATA: lt_alv_protocol_msg TYPE STANDARD TABLE OF ty_alv_protocol_msg,
          lt_fields           TYPE STANDARD TABLE OF ty_type.

**<<< Main Logic >>>

    lt_fields[] = t_types[].


    fill_tmp_data(
    EXPORTING
      io_chg_data = er_data_changed
    CHANGING
      ct_data     = lt_fields
      ).

    me->alv_data_validation(
    EXPORTING
      it_data         = lt_fields
      ir_data_changed = er_data_changed
    IMPORTING
      et_alv_msg      = lt_alv_protocol_msg
      ).

    CHECK lt_alv_protocol_msg IS NOT INITIAL.

    display_error(
    EXPORTING
      io_chg_data = er_data_changed
      it_alv_msgs = lt_alv_protocol_msg
      ).

    w-error = 'X'.

  ENDMETHOD.                    "handle_data_changed
  METHOD alv_data_validation.
** Variable **
    DATA: lv_tabix            TYPE sytabix,
          lv_row              TYPE syindex,
          lv_type             TYPE char1.

**  Work Area**
    DATA: ls_alv_protocol_msg TYPE ty_alv_protocol_msg,
          ls_good             TYPE lvc_s_modi,
          ls_types            TYPE ty_type.

** Internal Table
    DATA: lt_good_cells       TYPE lvc_t_modi.

**<<< Main Logic >>>
    ls_alv_protocol_msg-msgid = '0K'.
    ls_alv_protocol_msg-msgno = '000'.
    ls_alv_protocol_msg-msgty = 'E'.

    lt_good_cells[] = ir_data_changed->mt_good_cells[].

    SORT lt_good_cells BY fieldname
                          value.

    LOOP AT it_data INTO ls_types.

      lv_tabix = syst-tabix.

      IF  ls_types-fieldname IS INITIAL
      AND ls_types-include_type IS INITIAL.

        READ TABLE lt_good_cells INTO ls_good WITH KEY fieldname = 'FIELDNAME'
                                                       value     = ls_types-fieldname
                                                       BINARY SEARCH.
        IF syst-subrc IS INITIAL.
          lv_row = ls_good-row_id.
        ELSE.
          lv_row = lv_tabix.
        ENDIF.

        ls_alv_protocol_msg-fieldname = 'FIELDNAME'.
        ls_alv_protocol_msg-msg       = 'Fieldname is empty'.
        ls_alv_protocol_msg-row_id    = lv_row.

        APPEND ls_alv_protocol_msg TO et_alv_msg.
      ENDIF.

      IF ls_types-rollname IS NOT INITIAL.
        TRY.
            check_ddictype(
            EXPORTING
              iv_typename = ls_types-rollname
            IMPORTING
              ev_type     = lv_type
              ).

          CATCH lcl_messages INTO o_ref_msg.

            READ TABLE lt_good_cells INTO ls_good WITH KEY fieldname = 'ROLLNAME'
                                                           value = ls_types-rollname
                                                           BINARY SEARCH.

            IF syst-subrc IS INITIAL.
              lv_row = ls_good-row_id.
            ELSE.
              lv_row = lv_tabix.
            ENDIF.

            ls_alv_protocol_msg-fieldname = 'ROLLNAME'.
            ls_alv_protocol_msg-msg       = o_ref_msg->lv_text." 'Type Not found in DDIC repository'.
            ls_alv_protocol_msg-row_id    = lv_row.

            APPEND ls_alv_protocol_msg TO et_alv_msg.

        ENDTRY.

        IF  ls_types-include_type = 'X'
        AND (    lv_type <> 'S'
             AND lv_type <> 'T').

          READ TABLE lt_good_cells INTO ls_good WITH KEY fieldname = 'ROLLNAME'
                                                         value = ls_types-rollname
                                                         BINARY SEARCH.
          IF syst-subrc IS INITIAL.
            lv_row = ls_good-row_id.
          ELSE.
            lv_row = lv_tabix.
          ENDIF.

          ls_alv_protocol_msg-fieldname = 'ROLLNAME'.
          ls_alv_protocol_msg-msg       = 'Include type has no structure'.
          ls_alv_protocol_msg-row_id    = lv_row.

          APPEND ls_alv_protocol_msg TO et_alv_msg.

        ENDIF.


      ELSE.

        READ TABLE lt_good_cells INTO ls_good WITH KEY fieldname = 'ROLLNAME'
                                                       value = ls_types-rollname
                                                       BINARY SEARCH.

        IF syst-subrc IS INITIAL.
          lv_row = ls_good-row_id.
        ELSE.
          lv_row = lv_tabix.
        ENDIF.

        ls_alv_protocol_msg-fieldname = 'ROLLNAME'.
        ls_alv_protocol_msg-msg       = 'Type is Mandatory'.
        ls_alv_protocol_msg-row_id    = lv_row.
        APPEND ls_alv_protocol_msg TO et_alv_msg.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "alv_data_validation

  METHOD move_to_types_meta.

**  Work Area**
    DATA: ls_types_meta TYPE ty_types_meta,
          ls_types      TYPE ty_type,
          ls_pos        TYPE syindex.

**<<< Main Logic >>>

    DELETE t_types_meta WHERE index = w-type_index.

    LOOP AT t_types INTO ls_types.
      ls_pos = ls_pos + 1.
      CLEAR ls_types_meta.
      ls_types_meta-index        = w-type_index.
      ls_types_meta-position     = ls_pos.
      ls_types_meta-fieldname    = ls_types-fieldname.
      ls_types_meta-ddtext       = ls_types-ddtext.
      ls_types_meta-include_type = ls_types-include_type.
      ls_types_meta-rollname     = ls_types-rollname.
      ls_types_meta-typename     = w-typename.
      ls_types_meta-ditab        = w-ditab.
      ls_types_meta-dwarea       = w-dwarea.
      ls_types_meta-global       = w-global_chk.
      ls_types_meta-local        = w-local_chk.

      IF syst-tabix = lines( t_types ).
        ls_types_meta-last       = 'X'.
      ENDIF.

      APPEND ls_types_meta TO t_types_meta.

    ENDLOOP.

    SORT t_types_meta BY index ASCENDING position ASCENDING .

  ENDMETHOD.                    "move_to_types_meta

  METHOD move_frm_types_meta.
**  Work Area**
    DATA: ls_types_meta TYPE ty_types_meta,
          ls_types      TYPE ty_type.

**<<< Main Logic >>>

    REFRESH t_types[].

    READ TABLE t_types_meta INTO ls_types_meta
                        WITH KEY index =  w-type_index.

    CHECK syst-subrc IS INITIAL.

    w-typename   = ls_types_meta-typename.
    w-ditab      = ls_types_meta-ditab.
    w-dwarea     = ls_types_meta-dwarea.
    w-global_chk = ls_types_meta-global.
    w-local_chk  = ls_types_meta-local.

    IF ls_types_meta-global IS INITIAL.
      w-local_chk = 'X'.
    ENDIF.

    LOOP AT t_types_meta INTO ls_types_meta .

      CHECK ls_types_meta-index = w-type_index.

      ls_types-fieldname       = ls_types_meta-fieldname.
      ls_types-ddtext          = ls_types_meta-ddtext.
      ls_types-include_type    = ls_types_meta-include_type.
      ls_types-rollname        = ls_types_meta-rollname.

      APPEND ls_types TO t_types.
    ENDLOOP.

  ENDMETHOD.                    "move_frm_types_meta

  METHOD del_frm_types_meta.

** Variable **
    DATA : lv_index         TYPE syst-tabix.

**  Field Symbols**
    FIELD-SYMBOLS :
           <lfs_types_meta> TYPE ty_types_meta.

**<<< Main Logic >>>
    DELETE t_types_meta WHERE index = w-type_index.

    lv_index = w-type_index + 1.

    READ TABLE t_types_meta TRANSPORTING NO FIELDS WITH KEY index = lv_index.

    IF syst-subrc IS INITIAL.

      LOOP AT t_types_meta ASSIGNING <lfs_types_meta> FROM syst-tabix.

        <lfs_types_meta>-index = <lfs_types_meta>-index - 1.

      ENDLOOP.
    ENDIF.

    IF w-type_index = 1.
      EXIT.

    ELSE.
      CLEAR:w-typename,
            w-tabname.

      REFRESH t_types[].

      w-type_index = w-type_index - 1.

      me->move_frm_types_meta( ).

    ENDIF.

  ENDMETHOD.                    "del_frm_types_meta
ENDCLASS.                    "lcl_types_alv IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS lcl_data_alv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_data_alv IMPLEMENTATION.

  METHOD handle_data_changed .

*   Internal Table*
    DATA : lt_alv_protocol_msg TYPE STANDARD TABLE OF ty_alv_protocol_msg,
           lt_data             TYPE STANDARD TABLE OF ty_data.

**<<< Main Logic >>>

    lt_data[] = t_data[].

** Fill Temp Data to perform Validation.
    fill_tmp_data(
    EXPORTING
      io_chg_data = er_data_changed
    CHANGING
      ct_data     = lt_data
      ).
** perform Validation

    me->alv_data_validation(
    EXPORTING
      it_data         = lt_data
      ir_data_changed = er_data_changed
    IMPORTING
      et_alv_msg      = lt_alv_protocol_msg
      ).

    CHECK lt_alv_protocol_msg IS NOT INITIAL.
**  Display Error If needed.
    display_error(
    EXPORTING
      io_chg_data = er_data_changed
      it_alv_msgs = lt_alv_protocol_msg
      ).
**  Set Error.
    w-error = 'X'.
  ENDMETHOD.                    "handle_data_changed

  METHOD first_display.

**<<< Main Logic >>>

    set_dropdown( ).

    me->initialize_display(
    CHANGING
      ct_outtab = t_data
      ).

    SET HANDLER handle_data_changed FOR o_ref_alv.

  ENDMETHOD.                    "first_display

  METHOD refresh_alv.

**<<< Main Logic >>>
    IF iv_soft_refresh IS INITIAL.
      t_data[] = it_data[].
    ENDIF.
    o_ref_alv->refresh_table_display( ).

  ENDMETHOD.                    "refresh_alv

  METHOD fill_tab_data.

**  Work Area**
    DATA :ls_row    TYPE LINE OF salv_t_row,
          ls_data   TYPE ty_data,
          ls_dd03vt TYPE ty_dd03vt,
          ls_fields TYPE ty_fields.

    DATA :lt_fields TYPE STANDARD TABLE OF ty_fields.
**<<< Main Logic >>>

    o_ref_main->field_select(
    EXPORTING
      it_data    = it_data
    CHANGING
      ct_fields   = lt_fields
      ).

    LOOP AT lt_fields INTO ls_fields.

      CLEAR ls_data.
      READ TABLE it_data INTO ls_dd03vt WITH KEY tabname   = ls_fields-tabname
      fieldname = ls_fields-fieldname.

      CHECK syst-subrc IS INITIAL.

      READ TABLE t_data TRANSPORTING NO FIELDS
                                      WITH KEY varname  = ls_dd03vt-fieldname
                                               rollname = ls_dd03vt-rollname.

      CHECK syst-subrc IS NOT INITIAL.

      ls_data-varname  = ls_dd03vt-fieldname.
      ls_data-rollname = ls_dd03vt-rollname.
      ls_data-type     = 'TYPE'.

      APPEND ls_data TO t_data .

    ENDLOOP.


    IF t_data IS NOT INITIAL.
* Refresh ALV display with New data.
      refresh_alv(
      EXPORTING
            iv_soft_refresh = 'X'
        ).

    ENDIF.

  ENDMETHOD.                    "fill_alv_data

  METHOD set_dropdown.
**  Work Area**
    DATA: ls_dropdown TYPE lvc_s_drop.

*   Internal Table*
    DATA: lt_dropdown TYPE lvc_t_drop.

**<<< Main Logic >>>

* First listbox (handle '1').
    ls_dropdown-handle = '1'.
    ls_dropdown-value = 'Type'.
    APPEND ls_dropdown TO lt_dropdown.

    ls_dropdown-handle = '1'.
    ls_dropdown-value = 'Type Standard Table of'.
    APPEND ls_dropdown TO lt_dropdown.

    ls_dropdown-handle = '1'.
    ls_dropdown-value = 'Type Ref to'.
    APPEND ls_dropdown TO lt_dropdown.

    CLEAR ls_dropdown.

    CALL METHOD me->o_ref_alv->set_drop_down_table
      EXPORTING
        it_drop_down = lt_dropdown.

  ENDMETHOD.                    "set_dropdown

  METHOD alv_data_validation.

** Variable **
    DATA: lv_row   TYPE syindex,
          lv_tabix TYPE sytabix.

**  Work Area**
    DATA :ls_alv_protocol_msg TYPE ty_alv_protocol_msg,
          ls_good             TYPE lvc_s_modi,
          ls_data             TYPE ty_data.


**<<< Main Logic >>>

    ls_alv_protocol_msg-msgid = '0K'.
    ls_alv_protocol_msg-msgno = '000'.
    ls_alv_protocol_msg-msgty = 'E'.

    LOOP AT it_data INTO ls_data.

      lv_tabix = syst-tabix.

      IF  ls_data-varname IS INITIAL.

        READ TABLE ir_data_changed->mt_good_cells INTO ls_good
                                              WITH KEY fieldname = 'VARNAME'
                                                       value = ls_data-varname.

        IF syst-subrc IS INITIAL.
          lv_row = ls_good-row_id.
        ELSE.
          lv_row = lv_tabix.
        ENDIF.

        ls_alv_protocol_msg-fieldname = 'VARNAME'.
        ls_alv_protocol_msg-msg       = 'Variable is empty'.
        ls_alv_protocol_msg-row_id    = lv_row.

        APPEND ls_alv_protocol_msg TO et_alv_msg.

      ENDIF.

      IF ls_data-rollname IS INITIAL.

        READ TABLE ir_data_changed->mt_good_cells INTO ls_good
                                              WITH KEY fieldname = 'ROLLNAME'
                                                       value = ls_data-rollname.

        IF syst-subrc IS INITIAL.
          lv_row = ls_good-row_id.
        ELSE.
          lv_row = lv_tabix.
        ENDIF.

        ls_alv_protocol_msg-fieldname = 'ROLLNAME'.
        ls_alv_protocol_msg-msg       = 'Type is mandatory'.
        ls_alv_protocol_msg-row_id    = lv_row.

        APPEND ls_alv_protocol_msg TO et_alv_msg.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "alv_data_validation

ENDCLASS.                    "lcl_data_alv IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_selscr_alv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_selscr_alv  IMPLEMENTATION.

  METHOD first_display.

**<<< Main Logic >>>

    set_dropdown( ).

    me->initialize_display(
    CHANGING
      ct_outtab = t_selscr
      ).

    SET HANDLER handle_data_changed FOR o_ref_alv.

  ENDMETHOD.                    "first_display
  METHOD refresh_alv.

**<<< Main Logic >>>
    IF iv_soft_refresh IS INITIAL.
      t_selscr[] = it_data[].
    ENDIF.

    o_ref_alv->refresh_table_display( ).

  ENDMETHOD.                    "refresh_alv

  METHOD set_dropdown.

**  Work Area**
    DATA: ls_dropdown TYPE lvc_s_drop.

*   Internal Table*
    DATA: lt_dropdown TYPE lvc_t_drop.

**<<< Main Logic >>>

* First SLART listbox (handle '1').
    ls_dropdown-handle = '1'.
    ls_dropdown-value  = o_ref_main->c-parameters.
    APPEND ls_dropdown TO lt_dropdown.

    ls_dropdown-handle = '1'.
    ls_dropdown-value  = o_ref_main->c-seloption.
    APPEND ls_dropdown TO lt_dropdown.

    CLEAR ls_dropdown.

    CALL METHOD me->o_ref_alv->set_drop_down_table
      EXPORTING
        it_drop_down = lt_dropdown.


  ENDMETHOD.                    "set_dropdown
  METHOD handle_data_changed .

*   Internal Table*

    DATA : lt_alv_protocol_msg TYPE STANDARD TABLE OF ty_alv_protocol_msg,
           lt_selscr           TYPE STANDARD TABLE OF ty_selscr.

**<<< Main Logic >>>

    lt_selscr[] = t_selscr[].

    fill_tmp_data(
    EXPORTING
      io_chg_data = er_data_changed
    CHANGING
      ct_data     = lt_selscr
      ).

    me->alv_data_validation(
    EXPORTING
      it_data         = lt_selscr
      ir_data_changed = er_data_changed
    IMPORTING
      et_alv_msg      = lt_alv_protocol_msg
      ).

    CHECK lt_alv_protocol_msg IS NOT INITIAL.

    display_error(
    EXPORTING
      io_chg_data = er_data_changed
      it_alv_msgs = lt_alv_protocol_msg
      ).

    w-error = 'X'.

  ENDMETHOD.                    "handle_data_changed

  METHOD alv_data_validation.

** Variable **
    DATA: lv_row     TYPE syindex,
          lv_type    TYPE char1,
          lv_tabix   TYPE sytabix.

**  Work Area**
    DATA :ls_alv_msg TYPE ty_alv_protocol_msg,
          ls_good    TYPE lvc_s_modi,
          ls_selscr  TYPE ty_selscr.

**<<< Main Logic >>>

    ls_alv_msg-msgid = '0K'.
    ls_alv_msg-msgno = '000'.
    ls_alv_msg-msgty = 'E'.

    LOOP AT it_data INTO ls_selscr.

      lv_tabix = syst-tabix.
*** CHeck Parameter name is provided?
      IF  ls_selscr-name IS INITIAL.

        READ TABLE ir_data_changed->mt_good_cells INTO ls_good
                                              WITH KEY fieldname = 'NAME'
                                                       value     =  ls_selscr-name.

        IF syst-subrc IS INITIAL.
          lv_row = ls_good-row_id.
        ELSE.
          lv_row = lv_tabix.
        ENDIF.

        ls_alv_msg-fieldname = 'NAME'.
        ls_alv_msg-msg       = 'Name is Mandatory'.
        ls_alv_msg-row_id    = lv_row.

        APPEND ls_alv_msg TO et_alv_msg.

      ENDIF.

*    CHeck Declaration Type.
      IF ls_selscr-type IS INITIAL.
        READ TABLE ir_data_changed->mt_good_cells INTO ls_good
                                              WITH KEY fieldname = 'TYPE'
                                                       value     = ls_selscr-name.

        IF syst-subrc IS INITIAL.
          lv_row = ls_good-row_id.
        ELSE.
          lv_row = lv_tabix.
        ENDIF.

        ls_alv_msg-fieldname = 'TYPE'.
        ls_alv_msg-msg       = 'Type is Mandatory'.
        ls_alv_msg-row_id    = lv_row.

        APPEND ls_alv_msg TO et_alv_msg.
      ENDIF.

**** Check Parameter type***
      IF ls_selscr-rollname IS NOT INITIAL.

        TRY.
            check_ddictype(
            EXPORTING
              iv_typename = ls_selscr-rollname
            IMPORTING
              ev_type     = lv_type
              ).
          CATCH lcl_messages INTO o_ref_msg.

            READ TABLE ir_data_changed->mt_good_cells INTO ls_good
                                                  WITH KEY fieldname = 'ROLLNAME'
                                                           value     = ls_selscr-rollname.

            IF syst-subrc IS INITIAL.
              lv_row = ls_good-row_id.
            ELSE.
              lv_row = lv_tabix.
            ENDIF.

            ls_alv_msg-fieldname = 'ROLLNAME'.
            ls_alv_msg-msg       = o_ref_msg->lv_text."'Type Not found in DDIC repository'.
            ls_alv_msg-row_id    = lv_row.

            APPEND ls_alv_msg TO et_alv_msg.

        ENDTRY.

      ELSE.

        READ TABLE ir_data_changed->mt_good_cells INTO ls_good
                                              WITH KEY fieldname = 'ROLLNAME'
                                                       value     = ls_selscr-rollname.

        IF syst-subrc IS INITIAL.
          lv_row = ls_good-row_id.
        ELSE.
          lv_row = lv_tabix.
        ENDIF.

        ls_alv_msg-fieldname = 'ROLLNAME'.
        ls_alv_msg-msg       = 'Type is mandatory'.
        ls_alv_msg-row_id    = lv_row.

        APPEND ls_alv_msg TO et_alv_msg.

      ENDIF.

**  Parameter type check.
      IF ls_selscr-type IS INITIAL.
        READ TABLE ir_data_changed->mt_good_cells INTO ls_good
                                              WITH KEY fieldname = 'ROLLNAME'
                                                       value = ls_selscr-type.

        IF syst-subrc IS INITIAL.
          lv_row = ls_good-row_id.
        ELSE.
          lv_row = lv_tabix.
        ENDIF.

        ls_alv_msg-fieldname = 'TYPE'.
        ls_alv_msg-msg       = 'Type is mandatory'.
        ls_alv_msg-row_id    = lv_row.

        APPEND ls_alv_msg TO et_alv_msg.
      ENDIF.

*  CHeck Table validation.
      IF ls_selscr-check_table IS NOT INITIAL.

        TRY.
            check_ddictype(
            EXPORTING
              iv_typename = ls_selscr-check_table
            IMPORTING
              ev_type     = lv_type
              ).
          CATCH lcl_messages INTO o_ref_msg.

            READ TABLE ir_data_changed->mt_good_cells INTO ls_good
                                                  WITH KEY fieldname = 'CHECK_TABLE'
                                                           value = ls_selscr-check_table.

            IF syst-subrc IS INITIAL.
              lv_row = ls_good-row_id.
            ELSE.
              lv_row = lv_tabix.
            ENDIF.

            ls_alv_msg-fieldname = 'CHECK_TABLE'.
            ls_alv_msg-msg = o_ref_msg->lv_text."'Type Not found in DDIC repository'.
            ls_alv_msg-row_id = lv_row.

            APPEND ls_alv_msg TO et_alv_msg.
        ENDTRY.

        IF lv_type <> 'T'.
          READ TABLE ir_data_changed->mt_good_cells INTO ls_good
                                                WITH KEY fieldname = 'CHECKTABLE'
                                                         value     = ls_selscr-check_table.

          IF syst-subrc IS INITIAL.
            lv_row = ls_good-row_id.
          ELSE.
            lv_row = lv_tabix.
          ENDIF.

          ls_alv_msg-fieldname = 'CHECK_TABLE'.
          ls_alv_msg-msg       = 'Table Not found in DDIC repository'.
          ls_alv_msg-row_id    = lv_row.

          APPEND ls_alv_msg TO et_alv_msg.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "alv_data_validation

  METHOD fill_tab_data.

**  Work Area**

    DATA :ls_selscr   TYPE ty_selscr,
          ls_dd03vt   TYPE ty_dd03vt,
          ls_fields   TYPE ty_fields.

    DATA : lt_fields TYPE STANDARD TABLE OF ty_fields,
           lt_dd03vt TYPE STANDARD TABLE OF ty_dd03vt,
           lt_selscr TYPE STANDARD TABLE OF ty_selscr.
**<<< Main Logic >>>

    o_ref_main->field_select(
    EXPORTING
      it_data    = it_data
    CHANGING
      ct_fields   = lt_fields
      ).

    lt_dd03vt[] = it_data[].
    lt_selscr[] = t_selscr[].

    SORT: lt_dd03vt BY tabname
                       fieldname,
          lt_selscr BY name
                       rollname.

    LOOP AT lt_fields INTO ls_fields.

      CLEAR ls_selscr.
      READ TABLE lt_dd03vt INTO ls_dd03vt WITH KEY tabname   = ls_fields-tabname
                                                 fieldname = ls_fields-fieldname
                                                 BINARY SEARCH.

      CHECK syst-subrc IS INITIAL.

      READ TABLE lt_selscr TRANSPORTING NO FIELDS
                                        WITH KEY name     = ls_dd03vt-fieldname+0(8)
                                                 rollname = ls_dd03vt-rollname
                                                 BINARY SEARCH.

      CHECK syst-subrc IS NOT INITIAL.

      ls_selscr-name        = ls_dd03vt-fieldname+0(6).
      ls_selscr-type        = o_ref_main->c-parameters.
      ls_selscr-checkfield  = ls_dd03vt-fieldname.
      ls_selscr-rollname    = ls_dd03vt-rollname.
      ls_selscr-check_table = ls_dd03vt-checktable.

      APPEND ls_selscr TO lt_selscr.

    ENDLOOP.

    t_selscr[] = lt_selscr[].

    IF t_selscr IS NOT INITIAL.
* Refresh ALV display with New data.
      refresh_alv( iv_soft_refresh = 'X' ).
    ENDIF.

  ENDMETHOD.                    "get_param_fields

ENDCLASS.                    "lcl_selscr_alv IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS lcl_messages IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_messages IMPLEMENTATION.

  METHOD constructor.

**<<< Main Logic >>>

    super->constructor( ).

    CLEAR : lv_text,
            lv_type,
            lv_display_like.

    lv_text         = iv_text.
    lv_type         = iv_type.
    lv_display_like = iv_display_like.

  ENDMETHOD.                    "CONSTRUCTOR
  METHOD display_msg.

**<<< Main Logic >>>

*   CHeck Type and text is available.
    CHECK lv_type IS NOT INITIAL
      AND lv_text IS NOT INITIAL.

    IF lv_display_like IS NOT INITIAL.

      MESSAGE lv_text TYPE lv_type DISPLAY LIKE lv_display_like.
    ELSE.

      MESSAGE lv_text TYPE lv_type .
    ENDIF.

  ENDMETHOD.                    "display_msg

ENDCLASS.                    "lcl_messages IMPLEMENTATION
*----------------------------------------------------------------------*
***INCLUDE LZEDITOR_EXITF01 .
*----------------------------------------------------------------------*
CLASS lcl_main IMPLEMENTATION.
  METHOD constructor.

**<<< Main Logic >>>

    CLEAR tabstrip.
** Tabstrip active tab setup.
    tabstrip-activetab = 'TYPE'.

    REFRESH :t_buffer.

    w-type_index = '1'.

    set_prefixes( ).

  ENDMETHOD.                    "constructor

  METHOD gui_init.

**<<< Main Logic >>>
    REFRESH t_buffer[].
    CALL SCREEN 9100 STARTING AT 5 5 .

    CHECK t_buffer[] IS NOT INITIAL.


    et_buffer[] = t_buffer[].
* Clean Up.
    o_ref_main->clean_up( ).



  ENDMETHOD.                    "gui_init
  METHOD recalibrate_ucomm.

    CASE tabstrip-activetab .

      WHEN 'TYPE'.
        IF cv_ucomm = 'ENTER'.
          cv_ucomm = 'GET_FLD'.
        ENDIF.

        o_ref_types_alv->sync_alv_data( ).

      WHEN 'DATA'.
        IF cv_ucomm = 'ENTER'.
          cv_ucomm = 'GET_FLD2'.
        ENDIF.
        w-tabname = w-data_tabname.
        o_ref_data_alv->sync_alv_data( ).

      WHEN 'SELSCR'.
        IF cv_ucomm = 'ENTER'.
          cv_ucomm = 'GET_FLD3'.
        ENDIF.

        w-tabname = w-selscr_tabname.

        o_ref_selscr_alv->sync_alv_data( ).
      WHEN 'SQL'.
        w-tabname = w-sel_tabname.

        o_ref_sqlwhr_alv->sync_alv_data( ).

        o_ref_sqlfields_alv->sync_alv_data( ).

    ENDCASE.

  ENDMETHOD.
  METHOD process_ucomm.
    DATA : lv_ucomm TYPE ok.
**<<< Main Logic >>>

    lv_ucomm = iv_ucomm.

    me->recalibrate_ucomm(
    CHANGING
      cv_ucomm = lv_ucomm
      ).

* Validation.
    IF  lv_ucomm IS NOT INITIAL
    AND iv_dynnr IS NOT INITIAL.

      me->validation(
      EXPORTING
        i_ucomm  = lv_ucomm
        iv_dynnr = iv_dynnr
        ).

    ENDIF.

    CASE iv_dynnr.

      WHEN '9100'.

        CASE lv_ucomm .

          WHEN 'CANC'.
* Clean Up.
            me->clean_up( ).
* Leave.
            LEAVE TO SCREEN 0.

          WHEN 'OK'.

            me->build_code( ).

            IF w-copy_code IS NOT INITIAL.
              me->copy_to_clipboard( ).
              REFRESH t_buffer.
            ENDIF.
* Clean Up.
            me->clean_up( ).
            LEAVE TO SCREEN 0.

          WHEN 'TYPE'.
            tabstrip-activetab = 'TYPE'.

          WHEN 'DATA'.
            tabstrip-activetab = 'DATA'.

          WHEN 'CONFIG'.
            tabstrip-activetab = 'CONFIG'.

          WHEN 'SELSCR'.
            tabstrip-activetab = 'SELSCR'.

          WHEN 'SQL'.
            tabstrip-activetab = 'SQL'.

          WHEN 'LAUNCH_HELP'.

            me->launch_help( ).

        ENDCASE.

      WHEN '9101'.

        CASE lv_ucomm.
          WHEN 'GET_FLD'.

            me->get_alv_data( lv_ucomm ).
** Move or Delete Types.
          WHEN 'NEXT_ITEM'
            OR 'PREV_ITEM'
            OR 'DEL_ITEM'.

            me->maintain_types_data( lv_ucomm ).
        ENDCASE.

      WHEN '9102'.

        IF lv_ucomm =  'GET_FLD2'.

          me->get_alv_data( lv_ucomm ).

        ENDIF.

      WHEN '9103'.

        IF lv_ucomm =  'GET_FLD3'.

          me->get_alv_data( lv_ucomm ).
        ENDIF.

      WHEN '9104'.

        CASE lv_ucomm.
*  Get ALV data for SQL tab
          WHEN 'GET_FLD4'
            OR 'GET_FLD5'.

* Decide Table Name.
            IF w-sel_tabname IS INITIAL.
              me->get_tabname(
              CHANGING
                cv_tabname = w-sel_tabname
                ).
            ENDIF.
* Get Table ALV DATA
            IF w-sel_tabname IS NOT INITIAL.
              me->get_alv_data(
              EXPORTING
                iv_ucomm   = lv_ucomm
                iv_tabname = w-sel_tabname
                ).
            ENDIF.

          WHEN 'REF_INP'.
*     Refresh ALV table Data for SQL.
            o_ref_sqlwhr_alv->refresh_alv( ).
            o_ref_sqlfields_alv->refresh_alv( ).

          WHEN 'SQL_PFIELDS'.
            IF w-sql_types IS NOT INITIAL.

              o_ref_types_alv->move_to_types_meta( ).

              o_ref_sqlfields_alv->fill_tab_frm_type(
                EXPORTING
                  iv_typename   = w-sql_types
                  it_types_meta = o_ref_types_alv->t_types_meta
              ).

            ENDIF.

          WHEN 'SQL_PWHR'.

            IF w-sql_types IS NOT INITIAL.

              o_ref_types_alv->move_to_types_meta( ).

              o_ref_sqlwhr_alv->fill_tab_frm_type(
                EXPORTING
                  iv_typename   = w-sql_types
                  it_types_meta = o_ref_types_alv->t_types_meta
              ).

            ENDIF.

        ENDCASE.

    ENDCASE.

  ENDMETHOD.                    "process_ucomm

  METHOD validation.

**<<< Main Logic >>>

    CASE i_ucomm.

      WHEN 'GET_FLD'
        OR 'GET_FLD2'
        OR 'GET_FLD3'.

        IF ( i_ucomm = 'GET_FLD'  AND iv_dynnr = '9101')
        OR ( i_ucomm = 'GET_FLD2' AND iv_dynnr = '9102')
        OR ( i_ucomm = 'GET_FLD3' AND iv_dynnr = '9103').

          IF w-tabname IS INITIAL.

            RAISE EXCEPTION TYPE lcl_messages
              EXPORTING
                iv_text         = 'Table name can not be empty'
                iv_type         = 'S'
                iv_display_like = 'E'.

          ELSE.

            cl_abap_typedescr=>describe_by_name(
            EXPORTING
              p_name         = w-tabname    " Type name
            EXCEPTIONS
              type_not_found = 1
              OTHERS         = 2
              ).

            IF syst-subrc IS NOT INITIAL.

              RAISE EXCEPTION TYPE lcl_messages
                EXPORTING
                  iv_text         = 'Table does not exist'
                  iv_type         = 'S'
                  iv_display_like = 'E'.

            ENDIF.

          ENDIF.

        ENDIF.

      WHEN 'OK'.

        IF tabstrip-activetab = 'TYPE'.

          IF w-typename IS INITIAL.

            RAISE EXCEPTION TYPE lcl_messages
              EXPORTING
                iv_text         = 'typename can not be empty'
                iv_type         = 'S'
                iv_display_like = 'E'.

          ENDIF.

        ENDIF.

      WHEN 'NEXT_ITEM'.

        IF iv_dynnr = '9101'.

          IF w-typename IS INITIAL.

            RAISE EXCEPTION TYPE lcl_messages
              EXPORTING
                iv_text         = 'Typename is initial'
                iv_type         = 'S'
                iv_display_like = 'E'.

          ENDIF.

          IF o_ref_types_alv->t_types IS INITIAL.

            RAISE EXCEPTION TYPE lcl_messages
              EXPORTING
                iv_text         = 'No data in current item'
                iv_type         = 'S'
                iv_display_like = 'E'.

          ENDIF.

        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "validation
  METHOD launch_help.

    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
        document               = 'https://docs.google.com/document/d/1_fKQkUJb8nFCop3O_TTABFHldyMH741b/edit?usp=sharing&ouid=108776462233937115048&rtpof=true&sd=true'
        operation              = 'OPEN'
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.                    "launch_help
  METHOD get_alv_data.

** Variable **
    DATA : lv_tabname TYPE tabname.

*   Internal Table*
    DATA : lt_dd03vt  TYPE STANDARD TABLE OF ty_dd03vt.


**<<< Main Logic >>>

    IF iv_tabname IS INITIAL.
      lv_tabname = w-tabname.
    ELSE.
      lv_tabname = iv_tabname.
    ENDIF.

    me->get_tabfields(
    EXPORTING
      iv_tabname  = lv_tabname
    IMPORTING
      et_data     = lt_dd03vt
      ).

    CASE iv_ucomm.

      WHEN 'GET_FLD'.
*  when Types Tab.
        o_ref_types_alv->fill_tab_data(
        EXPORTING
          it_data     = lt_dd03vt
          ).

      WHEN 'GET_FLD2'.
*  When Data tab.
        o_ref_data_alv->fill_tab_data(
        EXPORTING
          it_data     = lt_dd03vt
          ).

      WHEN 'GET_FLD3'.
*  When Selection Screen Tab.
        o_ref_selscr_alv->fill_tab_data(
        EXPORTING
          it_data     = lt_dd03vt
          ).
      WHEN 'GET_FLD4'.

        o_ref_sqlfields_alv->fill_tab_data(
        EXPORTING
          it_data     = lt_dd03vt
          ).

      WHEN 'GET_FLD5'.

        o_ref_sqlwhr_alv->fill_tab_data(
        EXPORTING
          it_data     = lt_dd03vt
          ).
    ENDCASE.

  ENDMETHOD.                    "get_alv_data
  METHOD maintain_types_data.

**<<< Main Logic >>>

    CASE iv_ucomm.

      WHEN 'NEXT_ITEM'.

        o_ref_types_alv->move_to_types_meta( ).

        CLEAR: w-typename,
               w-tabname.

        w-type_index = w-type_index + 1.

        o_ref_types_alv->move_frm_types_meta( ).

        o_ref_types_alv->refresh_alv( iv_soft_refresh = 'X' ).

      WHEN 'PREV_ITEM'.

        IF  w-type_index = 1.
          RETURN.
        ENDIF.

        o_ref_types_alv->move_to_types_meta( ).

        CLEAR: w-typename,
               w-tabname.

        w-type_index = w-type_index - 1.

        o_ref_types_alv->move_frm_types_meta( ).
        o_ref_types_alv->refresh_alv( iv_soft_refresh = 'X' ).

      WHEN 'DEL_ITEM'.

        o_ref_types_alv->del_frm_types_meta( ).
        o_ref_types_alv->refresh_alv( iv_soft_refresh = 'X' ).

    ENDCASE.

  ENDMETHOD.                    "maintain_types_data
  METHOD has_inctype.
    DATA : lt_types_meta TYPE STANDARD TABLE OF ty_types_meta.

    ev_is_inc  = abap_false.

    lt_types_meta[] = it_types_meta[].

    SORT lt_types_meta BY index
                          include_type.

*  *       Check There is a Include type in the Types.
    READ TABLE lt_types_meta TRANSPORTING NO FIELDS WITH KEY index = iv_index
                                                             include_type = 'X'
                                                             BINARY SEARCH.

    CHECK syst-subrc IS INITIAL .
    ev_is_inc = abap_true.


  ENDMETHOD.                    "has_inctype
  METHOD ret_typ_begcode.
    DATA : lv_typname TYPE string.

    lv_typname =    me->ret_actual_typname( iv_is_global = iv_types_meta-global
                                            iv_typename  = iv_types_meta-typename ).

*       Add begin.( E.g. types : begin Of ty_table)
    CONCATENATE me->c-types
                me->c-colon
                me->c-begin
                lv_typname
                INTO ev_code
                SEPARATED BY space.

*       Add code based on Include type settings.
*( E.g. types : begin Of ty_table .)
    IF iv_hasinc = abap_true.
      ev_code = ev_code && me->c-period.
    ELSE.
      ev_code = ev_code && me->c-comma.
    ENDIF.

  ENDMETHOD.                    "add_beg_type
  METHOD ret_typ_endcode.

    DATA : lv_typname TYPE string.

    lv_typname =    me->ret_actual_typname( iv_is_global = iv_types_meta-global
                                            iv_typename  = iv_types_meta-typename ).

    IF iv_hasinc = abap_true.
      ev_code = me->c-types.
    ENDIF.

    CONCATENATE ev_code
                me->c-end
                lv_typname
                me->c-period"'.'
                INTO ev_code
                SEPARATED BY space.

  ENDMETHOD.                    "add_beg_type
  METHOD ret_typ_pf.

    IF iv_is_global IS NOT INITIAL.
      ev_pf = w-types_pf.
    ELSE.
      ev_pf = w-ltypes_pf.
    ENDIF.

  ENDMETHOD.                    "ret_typ_pf
  METHOD ret_var_pf.

    IF iv_is_global IS NOT INITIAL .
      ev_pf = w-warea_pf.
    ELSE.
      ev_pf = w-lwarea_pf.
    ENDIF.

  ENDMETHOD.                    "ret_var_pf
  METHOD ret_itab_pf.

    IF iv_is_global IS NOT INITIAL.
      ev_pf = w-itab_pf.
    ELSE.
      ev_pf = w-litab_pf.
    ENDIF.

  ENDMETHOD.                    "ret_itab_pf
  METHOD ret_obj_pf.

    IF iv_is_global IS NOT INITIAL.
      ev_pf = w-obj_pf.
    ELSE.
      ev_pf = w-lobj_pf.
    ENDIF.

  ENDMETHOD.                    "ret_obj_pf
  METHOD ret_actual_typname.

    ev_typename =    me->ret_typ_pf( iv_is_global = iv_is_global )
                 && iv_typename.

  ENDMETHOD.                    "ret_actual_typname
  METHOD add_type_code.

** Variable **
    DATA : lv_index       TYPE syindex,
           lv_hasinctyp   TYPE abap_bool,
*           lv_typename    TYPE string,
           lv_buffer      TYPE LINE OF rswsourcet.

**  Work Area**
    DATA ls_types_meta    TYPE ty_types_meta.

*   Internal Table*
    DATA : "lt_types       TYPE STANDARD TABLE OF ty_type,
           lt_types_meta  TYPE STANDARD TABLE OF ty_types_meta.

**<<< Main Logic >>>
    CHECK o_ref_types_alv IS NOT INITIAL.
*Call check_changed_data
    CALL METHOD o_ref_types_alv->sync_alv_data.

* Move all records to Meta table.
    o_ref_types_alv->move_to_types_meta( ).

    lt_types_meta[] = o_ref_types_alv->t_types_meta[].

* Proceed only if there is some Types data.
    CHECK lt_types_meta IS NOT INITIAL .

    LOOP AT lt_types_meta INTO ls_types_meta.
* On New Types.
      IF lv_index <> ls_types_meta-index.

*       Get Current Index.
        lv_index = ls_types_meta-index.

        lv_hasinctyp = me->has_inctype( it_types_meta = lt_types_meta
                                        iv_index      = lv_index     ).

        lv_buffer = me->ret_typ_begcode( iv_types_meta = ls_types_meta
                                         iv_hasinc     = lv_hasinctyp  ).

        APPEND lv_buffer TO t_buffer.

      ENDIF.
**** Add fields for The Type.
      CLEAR lv_buffer.
*       Add code based on Include type settings.
      IF lv_hasinctyp = abap_true.

        IF ls_types_meta-include_type = 'X'.
          lv_buffer = me->c-include.

          CLEAR ls_types_meta-fieldname.
        ELSE.
          lv_buffer = me->c-types.
        ENDIF.

      ENDIF.
*( E.g. Include zstructure.)
      CONCATENATE lv_buffer
                  ls_types_meta-fieldname
                  me->c-type
                  ls_types_meta-rollname
                  INTO lv_buffer
                  SEPARATED BY space.

      IF lv_hasinctyp = abap_true.
        lv_buffer = lv_buffer && me->c-period."'.'.
      ELSE.
        lv_buffer = lv_buffer && me->c-comma."','.
      ENDIF.

      CONCATENATE lv_buffer '"'
                  ls_types_meta-ddtext
                  INTO lv_buffer
                  SEPARATED BY space.

      CONDENSE lv_buffer.

      APPEND lv_buffer TO t_buffer.

****** Add End of Type.( E.g. End Of ty_table.)
      IF ls_types_meta-last = 'X'.

        lv_buffer = me->ret_typ_endcode( iv_types_meta = ls_types_meta
                                         iv_hasinc     =  lv_hasinctyp ).

        APPEND lv_buffer TO t_buffer.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "add_type_code
  METHOD has_autodata_dec.

    CHECK it_types_meta[] IS NOT INITIAL.
    ev_autodec = abap_true.
**    Check Work Area Declarations?
    READ TABLE it_types_meta TRANSPORTING NO FIELDS WITH KEY dwarea = 'X'.
***   If Not, Check Table declarations are required.
    CHECK syst-subrc IS NOT INITIAL.

    READ TABLE it_types_meta TRANSPORTING NO FIELDS WITH KEY ditab = 'X'.
***     If Not, Set Skip Variable.
    CHECK syst-subrc IS NOT INITIAL.
    ev_autodec = abap_false.


  ENDMETHOD.                    "has_autodata_dec
  METHOD ret_actual_varname.

    ev_varname =    me->ret_var_pf( iv_is_global = iv_is_global )
                 && iv_varname.

  ENDMETHOD.                    "ret_actual_varname
  METHOD ret_actual_itabname.

    ev_tabname =    me->ret_itab_pf( iv_is_global )
                 && iv_tabname.

  ENDMETHOD.                    "ret_actual_tabname
  METHOD ret_actual_objname.

    ev_objname =    me->ret_obj_pf( iv_is_global )
                 && iv_objname.

  ENDMETHOD.                    "ret_actual_tabname
  METHOD ret_warea_begcode.
    DATA : lv_pf TYPE char4.

    IF w-warea_pf CA '-'.
      lv_pf = w-warea_pf.
      REPLACE ALL OCCURRENCES OF '-' IN lv_pf WITH space.
      CONDENSE lv_pf.
    ELSE.
      lv_pf = w-warea_pf.
    ENDIF.

    CONCATENATE me->c-data
                 me->c-colon
                 me->c-begin
                 lv_pf
                 me->c-comma
                 INTO ev_code
                 SEPARATED BY space.

  ENDMETHOD.                    "ret_warea_begcode
  METHOD ret_warea_endcode.
    DATA : lv_pf TYPE char4.

    IF w-warea_pf CA '-'.
      lv_pf = w-warea_pf.
      REPLACE ALL OCCURRENCES OF '-' IN lv_pf WITH space.
      CONDENSE lv_pf.
    ENDIF.

    CONCATENATE me->c-end
    lv_pf
    me->c-period
    INTO ev_code
    SEPARATED BY space.

  ENDMETHOD.                    "ret_warea_endcode
  METHOD add_data_code.
**** Variables****
    DATA : lv_itab_pf        TYPE string,
           lv_types_pf       TYPE string,
           lv_typename       TYPE string,
           lv_varname        TYPE string,
           lv_end            TYPE char1,
           lv_index          TYPE syindex,
           lv_pf             TYPE string.

***** Work Areas****
    DATA : ls_code           TYPE rssource,
           ls_types_meta     TYPE ty_types_meta.

*** Internal Tables*****
    DATA : lt_tab_code       TYPE fkksourcebw_kk,
           lt_warea_code     TYPE fkksourcebw_kk,
           lt_types_meta     TYPE STANDARD TABLE OF ty_types_meta,
           lt_data           TYPE STANDARD TABLE OF ty_data.

**<<<<<<Main Logic>>>>>>>

***    First check If auto declration was set in any of the types.
** Get Types Meta Records.
    IF o_ref_types_alv IS NOT INITIAL.
      lt_types_meta[] = o_ref_types_alv->t_types_meta[].
    ENDIF.

* If not to Skip.
    IF me->has_autodata_dec( lt_types_meta[] ) = abap_true.
**      Go through Types Meta tab for data declarations.
      LOOP AT lt_types_meta INTO ls_types_meta.

*** Use index check to go through Each Types record only once.
        CHECK lv_index <> ls_types_meta-index.

        lv_index = ls_types_meta-index.
***   Check Work Area or Table declaration is needed.

        CHECK ls_types_meta-dwarea  = 'X'
           OR ls_types_meta-ditab   = 'X'.

***      Prepare Type name.
        lv_typename =   me->ret_actual_typname( iv_is_global = ls_types_meta-global
                                                iv_typename  = ls_types_meta-typename ).

* Check Work Area Declaration.
        IF  ls_types_meta-dwarea = 'X'.

          CLEAR ls_code.
          IF w-global_chk IS INITIAL.

            lv_varname = me->ret_actual_varname( iv_is_global = w-global_chk
                                                 iv_varname   = ls_types_meta-typename ).

            CONCATENATE me->c-data
                        lv_varname
                        INTO ls_code
                        SEPARATED BY space.

            lv_end = me->c-period.

          ELSE.

            ls_code = ls_types_meta-typename.
            lv_end = me->c-comma.

          ENDIF.

          CONCATENATE ls_code
                      me->c-type
                      lv_typename
                      lv_end
                      INTO ls_code
                      SEPARATED BY space.

          IF w-local_chk IS NOT INITIAL.
*         Append to Tables Code.
            APPEND ls_code TO lt_tab_code.

          ELSEIF w-global_chk IS NOT INITIAL.
**         Append to Work Area tab.
            APPEND ls_code  TO lt_warea_code.
          ENDIF.
        ENDIF.

*    Check Table declaration is required.
        IF ls_types_meta-ditab = 'X'.

          CLEAR ls_code.
          ls_code = me->ret_actual_itabname( iv_is_global = w-global_chk
                                             iv_tabname   = ls_types_meta-typename ).

*       Add declarations.
          CONCATENATE me->c-data
                      ls_code
                      me->c-type
                      me->c-standard_tab
                      lv_typename
                      me->c-period
                      INTO ls_code
                      SEPARATED BY space.

          APPEND ls_code TO lt_tab_code.

        ENDIF.
      ENDLOOP.
    ENDIF.

    IF me->o_ref_data_alv IS NOT INITIAL.

      o_ref_data_alv->sync_alv_data( ).

      lt_data[] = me->o_ref_data_alv->t_data[].

      LOOP AT lt_data INTO w-data.

        IF w-data-type CS 'TABLE'.
          ls_code = me->ret_actual_itabname( iv_is_global = w-global_chk
                                             iv_tabname   = w-data-varname ).
        ELSEIF w-data-type CS 'REF'.
          ls_code = me->ret_actual_objname( iv_is_global = w-global_chk
                                             iv_objname  = w-data-varname ).
        ELSEIF w-data-type = 'TYPE'.
          IF w-global_chk IS INITIAL.
            ls_code = me->ret_actual_varname( iv_is_global = w-global_chk
                                               iv_varname   = w-data-varname ).
          ELSE.
            ls_code = w-data-varname.
          ENDIF.
        ENDIF.

*   Add var name and type.
*      eg: Var type char1.
        CONCATENATE ls_code
                    w-data-type
                    w-data-rollname
                    INTO ls_code
                    SEPARATED BY space.

        IF w-global_data IS NOT INITIAL
      AND  w-data-type = 'TYPE'.

          CONCATENATE ls_code
                      me->c-comma
                      INTO ls_code
                      SEPARATED BY space.
          APPEND ls_code TO lt_warea_code.

        ELSE.

          CONCATENATE me->c-data
                ls_code
                me->c-period" '.'
                INTO ls_code
                SEPARATED BY space.
          APPEND ls_code TO lt_tab_code.

        ENDIF.

      ENDLOOP.
    ENDIF.

    IF lt_warea_code IS NOT INITIAL.

      ls_code = me->ret_warea_begcode( ).

      INSERT ls_code INTO lt_warea_code INDEX 1.

      CLEAR ls_code.

      ls_code = me->ret_warea_endcode( ).

      APPEND ls_code TO lt_warea_code.

      APPEND LINES OF lt_warea_code TO t_buffer.

    ENDIF.

    IF lt_tab_code IS NOT INITIAL.

      CLEAR ls_code.
      APPEND ls_code TO t_buffer.

      APPEND LINES OF lt_tab_code TO t_buffer.

    ENDIF.

  ENDMETHOD.                    "add_data_code
  METHOD add_selscr_code.
******** Variables*****
    DATA :lv_additive         TYPE char4,
          lv_type             TYPE string,
          lv_obligatory       TYPE char10,
          lv_string           TYPE string,
          lv_pname            TYPE char8.

**    Work Area**
    DATA ls_code              TYPE rssource.

** Internal tables**
    DATA : lt_selscr_code     TYPE fkksourcebw_kk,
           lt_validation_code TYPE fkksourcebw_kk,
           lt_selscr          TYPE STANDARD TABLE OF ty_selscr.

**<<<<<<<<< Main Logic>>>>

    CHECK o_ref_selscr_alv IS NOT INITIAL.

    CALL METHOD o_ref_selscr_alv->sync_alv_data.

*    Get Selection Screen Data.
    lt_selscr[] = o_ref_selscr_alv->t_selscr[].

    CHECK  lt_selscr IS NOT INITIAL.

    LOOP AT lt_selscr INTO w-selscr.
      CLEAR lv_additive.
* Get additive based on Parameter Type.
      CASE w-selscr-type.
*     Get Additive, Type and Parameter Name.
*      When Parameters
        WHEN me->c-parameters.

          lv_additive = me->c-type.
          lv_type     = w-selscr-rollname.
          lv_pname    = w-param_pf && w-selscr-name.

        WHEN me->c-seloption.

          lv_additive = 'FOR'.
          lv_type     = w-warea_pf  && '-' && w-selscr-rollname.
          lv_pname    = w-selopt_pf && w-selscr-name.

      ENDCASE.
* Check Obligatory Code need to be added.
      IF w-selscr-obligatory IS INITIAL.
        CLEAR lv_obligatory.
      ELSE.
        lv_obligatory = 'OBLIGATORY'.
      ENDIF.
* Generate COde for the Object.
      CONCATENATE w-selscr-type " Type of Sel screen object
                  lv_pname " Name
                  lv_additive    " For or Type
                  lv_type        " Type Name
                  lv_obligatory  " Obligatory ?
                  me->c-period   " Period
                  INTO ls_code
                  SEPARATED BY space.

      APPEND ls_code TO lt_selscr_code.

*    Provide Validation Query if Check Table and Field is Found.
      CHECK w-selscr-check_table IS NOT INITIAL
        AND w-selscr-checkfield  IS NOT INITIAL.

      CONCATENATE 'SELECT SINGLE'
                  w-selscr-checkfield
                  INTO ls_code
                  SEPARATED BY space.

      APPEND ls_code TO lt_validation_code.

      CONCATENATE 'FROM'
                   w-selscr-check_table
                   INTO ls_code
                   SEPARATED BY space.

      APPEND ls_code TO lt_validation_code.

      lv_string = w-warea_pf && '-' && w-selscr-checkfield.

      CONCATENATE 'INTO'
             lv_string
             INTO ls_code
             SEPARATED BY space.

      APPEND ls_code TO lt_validation_code.

      CONCATENATE 'WHERE'
                   w-selscr-checkfield
                   '='
                   lv_pname"w-selscr-name
                   me->c-period
                   INTO ls_code
                   SEPARATED BY space.

      APPEND ls_code TO lt_validation_code.

      ls_code = 'IF SYST-SUBRC IS NOT INITIAL.'.
      APPEND ls_code TO lt_validation_code.

      ls_code = 'Message ''Input is not Valid'' TYPE ''E''.' .

      APPEND ls_code TO lt_validation_code.
      ls_code = 'ENDIF.'.

      APPEND ls_code TO lt_validation_code.

    ENDLOOP.

    CLEAR ls_code.
    APPEND ls_code TO t_buffer.

    APPEND LINES OF lt_selscr_code TO t_buffer.
    CLEAR ls_code.
    APPEND ls_code TO t_buffer.

    APPEND LINES OF lt_validation_code TO t_buffer.

  ENDMETHOD.                    "add_selscr_code
  METHOD add_sql_code.

** Variable **
    DATA : lv_rows      TYPE string.

**  Work Area**
    DATA : ls_code      TYPE LINE OF fkksourcebw_kk,
           ls_sqlfields TYPE ty_sqlfields,
           ls_sqlwhr    TYPE ty_sqlwhr.

*   Internal Table*
    DATA : lt_sql_code  TYPE fkksourcebw_kk,
           lt_sqlfields TYPE STANDARD TABLE OF ty_sqlfields,
           lt_sqlwhr    TYPE STANDARD TABLE OF ty_sqlwhr.

**<<< Main Logic >>>
    IF o_ref_sqlwhr_alv IS NOT INITIAL.

      o_ref_sqlwhr_alv->sync_alv_data( ).

    ENDIF.

    CHECK o_ref_sqlfields_alv IS NOT INITIAL
      AND o_ref_sqlfields_alv->t_sqlfields[] IS NOT INITIAL.

    lt_sqlfields[] = o_ref_sqlfields_alv->t_sqlfields[].
*   Sort by Position
    SORT lt_sqlfields BY positon.

*     Decide select type
*   Check if its a select Single.
    IF  w-sel_qry_opt IS NOT INITIAL
    AND w-sel_single  IS NOT INITIAL.

      CONCATENATE me->c-select
                  me->c-single
                  INTO ls_code
                  SEPARATED BY space.
    ELSE.
      ls_code = me->c-select.
    ENDIF.

* Add select query fields.
    LOOP AT lt_sqlfields INTO ls_sqlfields.

      CONCATENATE ls_code
                  ls_sqlfields-fieldname
                  INTO ls_code
                  SEPARATED BY space.
      APPEND ls_code TO lt_sql_code.
      CLEAR ls_code.
    ENDLOOP.

** Add From Table
    CONCATENATE me->c-from
                w-sel_tabname
                INTO ls_code
                SEPARATED BY space.

    APPEND ls_code TO lt_sql_code.

** Add Into
    CLEAR ls_code.
    ls_code = me->c-into.

**    Attach table if needed.
    IF w-in_tab IS NOT INITIAL.
      CONCATENATE ls_code
                  me->c-table
                  INTO ls_code
                  SEPARATED BY space.
    ENDIF.

**    Add variable.
    CONCATENATE ls_code
                w-in_varname
                INTO ls_code
                SEPARATED BY space.

    APPEND ls_code TO lt_sql_code.


***   Add up to restiction if needed.
    IF  w-sel_qry_opt IS NOT INITIAL
    AND w-sel_rows  IS NOT INITIAL
    AND w-sel_rows_cnt IS NOT INITIAL.

      lv_rows = w-sel_rows_cnt.

      CONCATENATE me->c-upto
                  lv_rows
                  me->c-rows
                  INTO ls_code
                  SEPARATED BY space.

      APPEND ls_code TO lt_sql_code.
    ENDIF.

*** Add for entries if needed.
    IF  w-fr_al_entries IS NOT INITIAL
    AND w-fr_al_tabname IS NOT INITIAL.

      ls_code = 'FOR ALL ENTRIES IN'.
      CONCATENATE ls_code
      w-fr_al_tabname
      INTO ls_code
      SEPARATED BY space.
      APPEND ls_code  TO lt_sql_code.

    ENDIF.

    CLEAR ls_code.
**    Add whr condition ( Use and for now) replace with cloumn based logic
    CLEAR ls_code.

    ls_code = me->c-where.

    lt_sqlwhr[] = o_ref_sqlwhr_alv->t_sqlwhr[].

    LOOP AT lt_sqlwhr INTO ls_sqlwhr.

      CONCATENATE ls_code
                  ls_sqlwhr-para_begin
                  ls_sqlwhr-field1
                  ls_sqlwhr-opr
                  ls_sqlwhr-field2
                  ls_sqlwhr-para_end
                  ls_sqlwhr-and_or
                  INTO ls_code
                  SEPARATED BY space.

      APPEND ls_code TO lt_sql_code.
      CLEAR ls_code.

    ENDLOOP.
*** add period.
    CLEAR ls_code.
    ls_code = me->c-period.
    APPEND ls_code TO lt_sql_code.

***  Add End select if needed.
***   Add up to restiction if needed.
    IF  w-sel_qry_opt  IS NOT INITIAL
    AND w-sel_rows     IS NOT INITIAL
    AND w-sel_rows_cnt IS NOT INITIAL
    AND w-in_tab       IS INITIAL.

      CONCATENATE 'ENDSELECT'
                  me->c-period
                  INTO ls_code
                  SEPARATED BY space.

      APPEND ls_code TO lt_sql_code.
    ENDIF.

    CLEAR  ls_code.
    APPEND ls_code TO t_buffer.

    APPEND LINES OF lt_sql_code TO t_buffer.

  ENDMETHOD.                    "add_sql_code

  METHOD build_fieldcat.

** Variable **
    DATA : lv_pos      TYPE i VALUE 1.

**  Work Area**
    DATA : ls_fieldcat TYPE lvc_s_fcat.

*   Internal Table*
    DATA :lt_fieldcat  TYPE lvc_t_fcat.


    IF iv_tabname = 'T_TYPES'.
      CLEAR ls_fieldcat.
*Fieldname
      ls_fieldcat-col_pos    = lv_pos.
      ls_fieldcat-fieldname  = 'FIELDNAME'.
      ls_fieldcat-tabname    = iv_tabname.
      ls_fieldcat-edit       = 'X'.
      ls_fieldcat-scrtext_m  = 'Fieldname'.
      ls_fieldcat-colddictxt = 'M'.
      ls_fieldcat-tipddictxt = 'M'.

      APPEND ls_fieldcat TO lt_fieldcat.

      CLEAR ls_fieldcat.
*Data Element
      lv_pos = lv_pos + 1.
      ls_fieldcat-col_pos    = lv_pos.
      ls_fieldcat-fieldname  = 'ROLLNAME'.
      ls_fieldcat-tabname    = iv_tabname.
      ls_fieldcat-outputlen  = '30'.
      ls_fieldcat-datatype   = 'TYPENAME'.
      ls_fieldcat-edit       = 'X'.
      ls_fieldcat-scrtext_m  = 'TYPE'.

      APPEND ls_fieldcat TO lt_fieldcat.

      CLEAR ls_fieldcat.

      lv_pos = lv_pos + 1.
      ls_fieldcat-col_pos    = lv_pos.
      ls_fieldcat-fieldname  = 'INCLUDE_TYPE'.
      ls_fieldcat-tabname    = iv_tabname.
      ls_fieldcat-checkbox   = 'X'.
      ls_fieldcat-edit       = 'X'.
      ls_fieldcat-scrtext_m  = 'type include'.
      ls_fieldcat-colddictxt = 'M'.
      ls_fieldcat-tipddictxt = 'M'.

      APPEND ls_fieldcat TO lt_fieldcat.

      CLEAR ls_fieldcat.

*Description
      lv_pos = lv_pos + 1.
      ls_fieldcat-col_pos    = lv_pos.
      ls_fieldcat-fieldname  = 'DDTEXT'.
      ls_fieldcat-tabname    = iv_tabname.
      ls_fieldcat-key        = 'X'.
      ls_fieldcat-checkbox   = ' '.
      ls_fieldcat-edit       = ''.
      ls_fieldcat-outputlen  = '30'.
      ls_fieldcat-scrtext_m  = 'Description'.
      ls_fieldcat-colddictxt = 'M'.
      ls_fieldcat-tipddictxt = 'M'.

      APPEND ls_fieldcat TO lt_fieldcat.

    ELSEIF iv_tabname = 'T_DATA'.

      CLEAR ls_fieldcat.
*dname
      ls_fieldcat-col_pos    = lv_pos.
      ls_fieldcat-fieldname  = 'VARNAME'.
      ls_fieldcat-tabname    = iv_tabname.
      ls_fieldcat-edit       = 'X'.
      ls_fieldcat-scrtext_m  = 'Variable name'.

      APPEND ls_fieldcat TO lt_fieldcat.

      CLEAR ls_fieldcat.
*Data Element
      lv_pos = lv_pos + 1.
      ls_fieldcat-col_pos    = lv_pos.
      ls_fieldcat-tabname    = iv_tabname.
      ls_fieldcat-fieldname  = 'TYPE'.
      ls_fieldcat-edit       = 'X'.
      ls_fieldcat-scrtext_m  = 'TYPE'.
      ls_fieldcat-drdn_hndl  = '1'.
      APPEND ls_fieldcat TO lt_fieldcat.

      CLEAR ls_fieldcat.

      lv_pos = lv_pos + 1.
      ls_fieldcat-col_pos    = lv_pos.
      ls_fieldcat-tabname    = iv_tabname.
      ls_fieldcat-fieldname  = 'ROLLNAME'.
      ls_fieldcat-outputlen  = '30'.
      ls_fieldcat-edit       = 'X'.
      ls_fieldcat-scrtext_m  = 'Type Name'.
      ls_fieldcat-tipddictxt = 'M'.

      APPEND ls_fieldcat TO lt_fieldcat.

      CLEAR ls_fieldcat.

    ELSEIF iv_tabname = 'T_SELSCR'.

      CLEAR ls_fieldcat.
*Fieldname
      ls_fieldcat-col_pos    = lv_pos.
      ls_fieldcat-fieldname  = 'NAME'.
      ls_fieldcat-tabname    = iv_tabname.
      ls_fieldcat-outputlen  = '6'.
      ls_fieldcat-edit       = 'X'.
      ls_fieldcat-scrtext_m  = 'Name'.
      ls_fieldcat-colddictxt = 'M'.
      ls_fieldcat-tipddictxt = 'M'.

      APPEND ls_fieldcat TO lt_fieldcat.

      CLEAR ls_fieldcat.
*Data Element
      lv_pos = lv_pos + 1.
      ls_fieldcat-col_pos    = lv_pos.
      ls_fieldcat-fieldname  = 'TYPE'.
      ls_fieldcat-tabname    = iv_tabname.
      ls_fieldcat-datatype   = 'Parameter Type'.
      ls_fieldcat-edit       = 'X'.
      ls_fieldcat-drdn_hndl  = '1'.
      ls_fieldcat-scrtext_m  = 'TYPE'.

      APPEND ls_fieldcat TO lt_fieldcat.

      CLEAR ls_fieldcat.

      lv_pos = lv_pos + 1.
      ls_fieldcat-col_pos    = lv_pos.
      ls_fieldcat-fieldname  = 'ROLLNAME'.
      ls_fieldcat-tabname    = iv_tabname.
      ls_fieldcat-edit       = 'X'.
      ls_fieldcat-outputlen  = '30'.
      ls_fieldcat-scrtext_m  = 'TYPE'.
      ls_fieldcat-colddictxt = 'M'.
      ls_fieldcat-tipddictxt = 'M'.

      APPEND ls_fieldcat TO lt_fieldcat.

      CLEAR ls_fieldcat.
*Description
      lv_pos = lv_pos + 1.
      ls_fieldcat-col_pos    = lv_pos.
      ls_fieldcat-fieldname  = 'OBLIGATORY'.
      ls_fieldcat-tabname    = iv_tabname.
      ls_fieldcat-checkbox   = 'X'.
      ls_fieldcat-edit       = 'X'.
      ls_fieldcat-scrtext_m  = 'Obligatory'.
      ls_fieldcat-colddictxt = 'M'.
      ls_fieldcat-tipddictxt = 'M'.

      APPEND ls_fieldcat TO lt_fieldcat.

      CLEAR ls_fieldcat.
      lv_pos = lv_pos + 1.
      ls_fieldcat-col_pos    = lv_pos.
      ls_fieldcat-fieldname  = 'CHECKFIELD'.
      ls_fieldcat-tabname    = iv_tabname.
      ls_fieldcat-edit       = 'X'.
      ls_fieldcat-scrtext_m  = 'Check Field'.
      ls_fieldcat-colddictxt = 'M'.
      ls_fieldcat-tipddictxt = 'M'.

      APPEND ls_fieldcat TO lt_fieldcat.

      CLEAR ls_fieldcat.
      lv_pos = lv_pos + 1.
      ls_fieldcat-col_pos    = lv_pos.
      ls_fieldcat-fieldname  = 'CHECK_TABLE'.
      ls_fieldcat-tabname    = iv_tabname.
      ls_fieldcat-edit       = 'X'.
      ls_fieldcat-scrtext_m  = 'CheckTable'.
      ls_fieldcat-colddictxt = 'M'.
      ls_fieldcat-tipddictxt = 'M'.

      APPEND ls_fieldcat TO lt_fieldcat.

    ELSEIF iv_tabname = 'T_SQLFIELDS'.
      CLEAR ls_fieldcat.

*Data Element
      lv_pos = lv_pos + 1.
      ls_fieldcat-col_pos    = lv_pos.
      ls_fieldcat-fieldname  = 'FIELDNAME'.
      ls_fieldcat-tabname    = iv_tabname.
      ls_fieldcat-scrtext_m  = 'Fieldname'.
      APPEND ls_fieldcat TO lt_fieldcat.

      CLEAR ls_fieldcat.

      lv_pos = lv_pos + 1.
      ls_fieldcat-col_pos    = lv_pos.
      ls_fieldcat-fieldname  = 'POSITION'.
      ls_fieldcat-tabname    = iv_tabname.
      ls_fieldcat-no_out     = 'X'.
      ls_fieldcat-scrtext_m  = 'Position'.
      ls_fieldcat-colddictxt = 'M'.
      ls_fieldcat-tipddictxt = 'M'.

      APPEND ls_fieldcat TO lt_fieldcat.

      CLEAR ls_fieldcat.

    ELSEIF iv_tabname = 'T_SQLWHR'.
      lv_pos = lv_pos + 1.
      ls_fieldcat-col_pos    = lv_pos.
      ls_fieldcat-fieldname  = 'PARA_BEGIN'.
      ls_fieldcat-outputlen  = '2'.
      ls_fieldcat-dd_outlen  = '2'.
      ls_fieldcat-tabname    = iv_tabname.
      ls_fieldcat-edit       = 'X'.
      ls_fieldcat-scrtext_m  = '('.

      APPEND ls_fieldcat TO lt_fieldcat.

      CLEAR ls_fieldcat.
*Fieldname
      ls_fieldcat-col_pos    = lv_pos.
      ls_fieldcat-fieldname  = 'FIELD1'.
      ls_fieldcat-ref_table  = 'DD28V'.
      ls_fieldcat-ref_field  = 'FIELDNAME'.
      ls_fieldcat-f4availabl = 'X'.
      ls_fieldcat-tabname    = iv_tabname.
      ls_fieldcat-outputlen  = '15'.
      ls_fieldcat-dd_outlen  = '30'.
      ls_fieldcat-scrtext_m  = 'Field1'.
      ls_fieldcat-colddictxt = 'M'.
      ls_fieldcat-tipddictxt = 'M'.

      APPEND ls_fieldcat TO lt_fieldcat.

      CLEAR ls_fieldcat.
*Data Element
      lv_pos = lv_pos + 1.
      ls_fieldcat-col_pos    = lv_pos.
      ls_fieldcat-fieldname  = 'OPR'.
      ls_fieldcat-tabname    = iv_tabname.
      ls_fieldcat-outputlen  = '3'.
      ls_fieldcat-edit       = 'X'.
      ls_fieldcat-drdn_hndl  = '2'.
      ls_fieldcat-scrtext_m  = 'Opr'.

      APPEND ls_fieldcat TO lt_fieldcat.

      CLEAR ls_fieldcat.
      lv_pos = lv_pos + 1.
      ls_fieldcat-col_pos    = lv_pos.
      ls_fieldcat-fieldname  = 'FIELD2'.
      ls_fieldcat-outputlen  = '15'.
      ls_fieldcat-dd_outlen  = '30'.
      ls_fieldcat-tabname    = iv_tabname.
      ls_fieldcat-edit       = 'X'.
      ls_fieldcat-scrtext_m  = 'Field2'.

      APPEND ls_fieldcat TO lt_fieldcat.

      CLEAR ls_fieldcat.

      lv_pos = lv_pos + 1.
      ls_fieldcat-col_pos    = lv_pos.
      ls_fieldcat-fieldname  = 'POSITION'.
      ls_fieldcat-no_out     = 'X'.
      ls_fieldcat-tabname    = iv_tabname.

      APPEND ls_fieldcat TO lt_fieldcat.

      CLEAR ls_fieldcat.
      lv_pos = lv_pos + 1.
      ls_fieldcat-col_pos    = lv_pos.
      ls_fieldcat-fieldname  = 'PARA_END'.
      ls_fieldcat-outputlen  = '2'.
      ls_fieldcat-dd_outlen  = '2'.
      ls_fieldcat-tabname    = iv_tabname.
      ls_fieldcat-edit       = 'X'.
      ls_fieldcat-scrtext_m  = ')'.

      APPEND ls_fieldcat TO lt_fieldcat.

      CLEAR ls_fieldcat.
      lv_pos = lv_pos + 1.
      ls_fieldcat-col_pos    = lv_pos.
      ls_fieldcat-fieldname  = 'AND_OR'.
      ls_fieldcat-outputlen  = '3'.
      ls_fieldcat-dd_outlen  = '3'.
      ls_fieldcat-tabname    = iv_tabname.
      ls_fieldcat-drdn_hndl  = '1'.
      ls_fieldcat-edit       = 'X'.
      ls_fieldcat-scrtext_m  = 'AND_OR'.

      APPEND ls_fieldcat TO lt_fieldcat.

      CLEAR ls_fieldcat.

    ENDIF.

    et_fieldcat[] = lt_fieldcat[].

    CLEAR ls_fieldcat.
  ENDMETHOD.                    "build_fieldcat

  METHOD get_tabfields.

**<<< Main Logic >>>

    SELECT  tabname
            fieldname
            position
            rollname
            checktable
            ddtext
            FROM  dd03vt
            INTO TABLE et_data
            WHERE tabname    = iv_tabname
              AND as4local   = 'A'
              AND ddlanguage = 'E'.

    CHECK syst-subrc IS INITIAL.
* call salv
    SORT et_data BY position ASCENDING.

  ENDMETHOD.                    "DISPLAY_popup
  METHOD  build_code.

**<<< Main Logic >>>

*  Add Code for Types.
    add_type_code( ).
*  Add Code for Data
    add_data_code( ).
*  Add Code for selection Screen
    add_selscr_code( ).
*  Add Code for Select Query
    add_sql_code( ).

  ENDMETHOD.                    "build_code
  METHOD set_prefixes.

**<<< Main Logic >>>

** Set Prefixes.
    IF w-warea_pf IS INITIAL.
      w-warea_pf = 'W'.
    ENDIF.

    IF w-itab_pf IS INITIAL.
      w-itab_pf  = 'T_'.
    ENDIF.

    IF w-types_pf IS INITIAL.
      w-types_pf = 'TY_'.
    ENDIF.

    IF w-lwarea_pf IS INITIAL.
      w-lwarea_pf = 'LS_'.
    ENDIF.

    IF w-litab_pf IS INITIAL.
      w-litab_pf  = 'LT_'.
    ENDIF.

    IF w-ltypes_pf IS INITIAL.
      w-ltypes_pf = 'LTY_'.
    ENDIF.

    IF w-param_pf IS INITIAL.
      w-param_pf = 'P_'.
    ENDIF.

    IF w-selopt_pf IS INITIAL.
      w-selopt_pf = 'S_'.
    ENDIF.

  ENDMETHOD.                    "set_prefixes
  METHOD field_select.
    TYPES: BEGIN OF lty_fieldtab,
            tf TYPE dd03p,
            mark TYPE char1,
           END OF lty_fieldtab.

    DATA : lt_tabfields TYPE STANDARD TABLE OF lty_fieldtab.

    DATA : ls_tabfields TYPE lty_fieldtab,
           ls_data      TYPE ty_dd03vt,
           ls_fields    TYPE ty_fields.

    CHECK it_data IS NOT INITIAL.

    LOOP AT it_data INTO ls_data.
      ls_tabfields-tf-fieldname = ls_data-fieldname.
      ls_tabfields-tf-tabname  = ls_data-tabname.
      ls_tabfields-tf-rollname = ls_data-rollname.
      ls_tabfields-tf-ddtext   = ls_data-ddtext.

      READ TABLE ct_fields TRANSPORTING NO FIELDS WITH KEY tabname = ls_data-tabname
                                                           fieldname = ls_data-fieldname
                                                           rollname  = ls_data-rollname.
      IF  syst-subrc IS INITIAL.

        ls_tabfields-mark  = 'X'.
      ELSE.
        CLEAR ls_tabfields-mark.
      ENDIF.
      APPEND ls_tabfields TO lt_tabfields.

    ENDLOOP.

    CALL FUNCTION 'DD_LIST_TABFIELDS'
      TABLES
        fieldtab     = lt_tabfields
      EXCEPTIONS
        not_executed = 1
        OTHERS       = 2.
    IF sy-subrc = 0.

      DELETE lt_tabfields WHERE mark IS INITIAL.

      REFRESH ct_fields.
      LOOP AT lt_tabfields INTO ls_tabfields.
        ls_fields-fieldname = ls_tabfields-tf-fieldname.
        ls_fields-tabname   = ls_tabfields-tf-tabname.
        ls_fields-rollname  = ls_tabfields-tf-rollname.
        APPEND ls_fields TO ct_fields.

      ENDLOOP.

    ENDIF.


  ENDMETHOD.                    "Field_select
  METHOD get_tabname.

** Variable **
    DATA: lv_ret     TYPE char1.
**  Work Area**
    DATA: ls_fields  TYPE sval.

*   Internal Table*
    DATA: lt_fields  TYPE STANDARD TABLE OF sval.

**<<< Main Logic >>>

    ls_fields-tabname   = 'DD03VT'.
    ls_fields-fieldname = 'TABNAME'.
    ls_fields-value     = cv_tabname.
    ls_fields-field_obl = me->c-x.

    APPEND ls_fields TO lt_fields.

    CALL FUNCTION 'POPUP_GET_VALUES_DB_CHECKED'
      EXPORTING
        check_existence = me->c-x
        popup_title     = 'Input Table Name'
      IMPORTING
        returncode      = lv_ret
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
* Check Error*
    IF sy-subrc <> 0
    OR lv_ret IS NOT INITIAL.

      RAISE EXCEPTION TYPE lcl_messages
        EXPORTING
          iv_text         = 'Input Table incorrect'
          iv_type         = 'S'
          iv_display_like = 'E'.

    ELSE.

      READ TABLE lt_fields INTO ls_fields INDEX 1.

      IF syst-subrc IS INITIAL.
        cv_tabname = ls_fields-value.
      ENDIF.

    ENDIF.
  ENDMETHOD.                    "get_tabname
  METHOD clean_up.

**<<< Main Logic >>>

    IF o_ref_types_alv IS BOUND .
      o_ref_types_alv->clean_up( ).
    ENDIF.

    IF o_ref_data_alv IS BOUND.
      o_ref_data_alv->clean_up( ).
    ENDIF.

    IF o_ref_selscr_alv IS BOUND.
      o_ref_selscr_alv->clean_up( ).
    ENDIF.

    IF o_ref_sqlfields_alv IS BOUND.
      o_ref_sqlfields_alv->clean_up( ).
    ENDIF.

    IF o_ref_sqlwhr_alv IS BOUND.
      o_ref_sqlwhr_alv->clean_up( ).
    ENDIF.

    CLEAR w.

    FREE : o_ref_types_alv,
           o_ref_data_alv,
           o_ref_selscr_alv,
           o_ref_sqlfields_alv,
           o_ref_sqlwhr_alv.
  ENDMETHOD.                    "clean_up
  METHOD set_values_ddown.

    IF iv_dynnr = '9104'.

      set_sqltab_ddown( ).

    ENDIF.
  ENDMETHOD.                    "set_values_ddown
  METHOD copy_to_clipboard.

    DATA : lv_rc TYPE sysubrc .
    DATA : lt_buffer TYPE fkksourcebw_kk.

    lt_buffer[] = t_buffer[].

    cl_gui_frontend_services=>clipboard_export(
        IMPORTING
          data                 =  lt_buffer   " Data
      CHANGING
        rc                   =    lv_rc " Return code
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          no_authority         = 4
          OTHERS               = 5
      ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcl_messages
        EXPORTING
          iv_text         = 'Error Copying to Clipboard'
          iv_type         = 'S'
          iv_display_like = 'E'.
    ENDIF.

  ENDMETHOD.                    "copy_to_clipboard
  METHOD set_sqltab_ddown.
    DATA : lt_vars    TYPE string_table,
           lt_objs    TYPE string_table,
           lt_tables  TYPE string_table,
           lt_types   TYPE string_table.

    get_types_and_vars(
      IMPORTING
        et_types  = lt_types
        et_vars   = lt_vars
        et_objs   = lt_objs
        et_tables = lt_tables
    ).

    set_fr_all_entries_ddown( lt_tables ).

    set_in_var_ddown(
    EXPORTING
      it_vars   = lt_vars
      it_tables = lt_tables
      ).

    set_local_types_ddown( lt_types ).

  ENDMETHOD.                    "set_SQLTAB_ddown
  METHOD get_types_and_vars.

    DATA : lt_types_meta TYPE STANDARD TABLE OF ty_types_meta,
           lt_data       TYPE STANDARD TABLE OF ty_data.

    DATA : ls_string         TYPE string,
           ls_types_meta     TYPE ty_types_meta,
           ls_data           TYPE ty_data.

    IF o_ref_types_alv IS NOT INITIAL.

      TRY.
          o_ref_types_alv->sync_alv_data( ).

        CATCH lcl_messages INTO o_ref_msg.

      ENDTRY.
      lt_types_meta[] =  o_ref_types_alv->t_types_meta.
    ENDIF.

    SORT lt_types_meta BY index.

    DELETE ADJACENT DUPLICATES FROM lt_types_meta COMPARING index.
    SORT lt_types_meta BY typename.
    READ TABLE lt_types_meta TRANSPORTING NO FIELDS WITH KEY typename = w-typename
                                                    BINARY SEARCH.
    IF  syst-subrc IS NOT INITIAL
    AND w-typename IS NOT INITIAL.

      ls_types_meta-typename = w-typename.
      ls_types_meta-dwarea  = w-dwarea.
      ls_types_meta-ditab   = w-ditab.
      ls_types_meta-global  = w-global_chk.
      ls_types_meta-local   = w-local_chk.
      APPEND ls_types_meta TO lt_types_meta.

    ENDIF.

    LOOP AT lt_types_meta INTO ls_types_meta.

      ls_string = me->ret_actual_typname( iv_is_global = ls_types_meta-global
                                          iv_typename  = ls_types_meta-typename ).
      APPEND ls_string TO et_types.

      IF ls_types_meta-dwarea IS NOT INITIAL.
        ls_string = me->ret_actual_varname( iv_is_global = ls_types_meta-global
                                            iv_varname   = ls_types_meta-typename ).
        APPEND ls_string TO et_vars.
      ENDIF.

      IF ls_types_meta-ditab IS NOT INITIAL.
        ls_string = me->ret_actual_itabname( iv_is_global = ls_types_meta-global
                                             iv_tabname   = ls_types_meta-typename ).
        APPEND ls_string TO et_tables.
      ENDIF.

    ENDLOOP.

    IF o_ref_data_alv IS NOT INITIAL.

      TRY.
          o_ref_data_alv->sync_alv_data( ).

        CATCH lcl_messages INTO o_ref_msg.

      ENDTRY.
      lt_data = o_ref_data_alv->t_data[].

    ENDIF.

    LOOP AT lt_data INTO ls_data.

      IF ls_data-type CS 'TABLE'.

        ls_string = me->ret_actual_itabname( iv_is_global = w-global_data
                                             iv_tabname   = ls_data-varname ).

        APPEND ls_string TO et_tables.
      ELSEIF ls_data-type CS 'REF TO'.

        ls_string = me->ret_actual_objname( iv_is_global = w-global_data
                                            iv_objname   = ls_data-varname ).
        APPEND ls_string TO et_objs.
      ELSEIF ls_data-type = 'TYPE'.

        ls_string = me->ret_actual_typname( iv_is_global = w-global_data
                                            iv_typename  = ls_data-varname )."lv_data_pf && ls_data-varname.
        APPEND ls_string TO et_vars.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "get_types_and_vars
  METHOD set_fr_all_entries_ddown.

    DATA : lt_values TYPE STANDARD TABLE OF vrm_value,
           lt_data   TYPE STANDARD TABLE OF ty_data.

    DATA : ls_values TYPE vrm_value,
           ls_data   TYPE string.

    LOOP AT it_tables INTO ls_data.

      ls_values-key = ls_values-text = ls_data."-varname.

      APPEND ls_values TO lt_values.

    ENDLOOP.
    IF w-fr_al_tabname IS NOT INITIAL.
      READ TABLE lt_values TRANSPORTING NO FIELDS with KEY key = w-fr_al_tabname.
      IF syst-subrc IS NOT INITIAL.
CLEAR w-fr_al_tabname.
      ENDIF.
    ENDIF.
    set_scr_ddown_val(
    EXPORTING
      iv_id  = 'W-FR_AL_TABNAME'
      it_val = lt_values[]
      ).

  ENDMETHOD.                    "set_fr_all_entries_ddown

  METHOD set_in_var_ddown.

    DATA : lt_values TYPE STANDARD TABLE OF vrm_value,
           lt_string TYPE string_table.

    DATA : ls_values TYPE vrm_value,
           ls_string   TYPE string.

    IF w-in_var IS NOT INITIAL.
      lt_string[] = it_vars[].
    ELSE.
      lt_string[] = it_tables[].
    ENDIF.

    LOOP AT lt_string INTO ls_string.

      ls_values-key = ls_values-text = ls_string.
      APPEND ls_values TO lt_values.

    ENDLOOP.
    IF w-in_varname IS NOT INITIAL.
      READ TABLE lt_values TRANSPORTING NO FIELDS with KEY key = W-IN_VARNAME.
      IF syst-subrc IS NOT INITIAL.
CLEAR W-IN_VARNAME.
      ENDIF.
    ENDIF.
    set_scr_ddown_val(
    EXPORTING
      iv_id  = 'W-IN_VARNAME'
      it_val = lt_values[]
      ).

  ENDMETHOD.                    "set_in_var_ddown

  METHOD set_local_types_ddown.

    DATA : lt_values     TYPE STANDARD TABLE OF vrm_value,
           lt_types_meta TYPE STANDARD TABLE OF ty_types_meta.

    DATA : ls_values TYPE vrm_value,
           ls_types TYPE ty_type,
           ls_string TYPE string,
           ls_types_meta TYPE ty_types_meta.


    LOOP AT it_types INTO ls_string.

      ls_values-key = ls_values-text = ls_string.
      APPEND ls_values TO lt_values.
    ENDLOOP.

    set_scr_ddown_val(
    EXPORTING
      iv_id  = 'W-SQL_TYPES'
      it_val = lt_values[]
      ).

  ENDMETHOD.                    "set_local_types_ddown
  METHOD set_scr_ddown_val.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = iv_id
        values          = it_val[]
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.

    ENDIF.

  ENDMETHOD.                    "set_scr_ddown_val
ENDCLASS.                    "lcl_main IMPLEMENTATION
*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_sqlfields_alv
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_sqlfields_alv IMPLEMENTATION.

  METHOD first_display.

**<<< Main Logic >>>

    me->initialize_display(
    CHANGING
      ct_outtab = t_sqlfields
      ).

  ENDMETHOD.                    "first_display

  METHOD refresh_alv.

**<<< Main Logic >>>
    IF iv_soft_refresh IS INITIAL.
      t_sqlfields[] = it_data[].
    ENDIF.

    o_ref_alv->refresh_table_display( ).

  ENDMETHOD.                    "refresh_alv
  METHOD on_toolbar.

**<<< Main Logic >>>
    REFRESH e_object->mt_toolbar.

  ENDMETHOD.                    "on_toolbar

  METHOD fill_tab_data.

**  Work Area**

    DATA :ls_sqlfields TYPE ty_sqlfields,
          ls_dd03vt    TYPE ty_dd03vt,
          ls_fields    TYPE ty_fields.
    DATA : lt_fields TYPE STANDARD TABLE OF ty_fields.
**<<< Main Logic >>>

    LOOP AT t_sqlfields INTO ls_sqlfields.
      ls_fields-fieldname = ls_sqlfields-fieldname.
      ls_fields-tabname   = ls_sqlfields-tabname.
      ls_fields-rollname  = ls_sqlfields-rollname.
      APPEND ls_fields TO lt_fields.
    ENDLOOP.

    o_ref_main->field_select(
    EXPORTING
      it_data    = it_data
    CHANGING
      ct_fields   = lt_fields
      ).

*   Fill Fields table with selected recods.

    REFRESH t_sqlfields.
    LOOP AT lt_fields INTO ls_fields.

      CLEAR ls_sqlfields.
      READ TABLE it_data INTO ls_dd03vt WITH KEY tabname   = ls_fields-tabname
      fieldname = ls_fields-fieldname.

      CHECK syst-subrc IS INITIAL.

      ls_sqlfields-tabname = ls_dd03vt-tabname.
      ls_sqlfields-fieldname = ls_dd03vt-fieldname.
      ls_sqlfields-rollname  = ls_dd03vt-rollname.
      ls_sqlfields-positon = ls_dd03vt-position.

      APPEND ls_sqlfields TO t_sqlfields.

    ENDLOOP.


    IF t_sqlfields IS NOT INITIAL.
* Refresh ALV display with New data.
      refresh_alv( iv_soft_refresh = 'X'  ).

    ENDIF.
  ENDMETHOD.                    "fill_sqlfields
  METHOD fill_tab_frm_type.
    DATA : ls_types_meta TYPE ty_types_meta,
           ls_types      TYPE ty_type,
           ls_sqlfields  TYPE ty_sqlfields.

    DATA : lv_tabix      TYPE sytabix,
           lv_typename   TYPE char30.

    DATA : lt_types_meta TYPE STANDARD TABLE OF ty_types_meta.
    CHECK iv_typename IS NOT INITIAL.

    lv_typename = iv_typename.

    LOOP AT it_types_meta INTO ls_types_meta." FROM lv_tabix.

      lv_typename = o_ref_main->ret_actual_typname( iv_is_global = ls_types_meta-global
                                                    iv_typename  = ls_types_meta-typename ).

      IF lv_typename = iv_typename.
        ls_sqlfields-fieldname = ls_types_meta-fieldname.
        ls_sqlfields-positon   = ls_types_meta-position.
        APPEND ls_sqlfields TO t_sqlfields.
      ENDIF.

    ENDLOOP.


    IF t_sqlfields IS NOT INITIAL.
* Refresh ALV display with New data.
      refresh_alv( iv_soft_refresh = 'X'  ).

    ENDIF.
  ENDMETHOD.                    "fill_tab_frm_type
ENDCLASS.               "lcl_sqlfields_alv
*----------------------------------------------------------------------*
*       CLASS lcl_sqlwhr_alv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_sqlwhr_alv IMPLEMENTATION.
  METHOD first_display.

**<<< Main Logic >>>
    set_dropdown( ).

    me->initialize_display(
    CHANGING
      ct_outtab = t_sqlwhr
      ).

    SET HANDLER handle_data_changed FOR o_ref_alv.
  ENDMETHOD.                    "first_display
  METHOD on_toolbar.

**<<< Main Logic >>>

    REFRESH  e_object->mt_toolbar .

  ENDMETHOD.                    "on_toolbar
  METHOD handle_data_changed.
** Mandatory redifinition
  ENDMETHOD.                    "on_toolbar
  METHOD fill_tab_data.

    DATA :ls_row      TYPE LINE OF salv_t_row,
          ls_sqlwhr   TYPE ty_sqlwhr,
          ls_dd03vt   TYPE ty_dd03vt,
          ls_fields   TYPE ty_fields.

    DATA : lt_fields TYPE STANDARD TABLE OF ty_fields.
    DATA : lt_dd03vt TYPE STANDARD TABLE OF ty_dd03vt.
    DATA : lt_sqlwhr TYPE STANDARD TABLE OF ty_sqlwhr.
**<<< Main Logic >>>

    LOOP AT t_sqlwhr INTO ls_sqlwhr.
      ls_fields-fieldname = ls_sqlwhr-field1.
      ls_fields-rollname  = ls_sqlwhr-rollname.
      ls_fields-tabname   = w-sel_tabname.
      APPEND ls_fields TO lt_fields.
    ENDLOOP.

    o_ref_main->field_select(
    EXPORTING
      it_data    = it_data
    CHANGING
      ct_fields   = lt_fields
      ).

    lt_dd03vt[]  = it_data[].
    lt_sqlwhr[]  = t_sqlwhr[].

    SORT: lt_dd03vt BY fieldname,
          lt_sqlwhr BY field1.

*   Fill Fields table with selected recods.
    LOOP AT lt_fields INTO ls_fields.

      CLEAR ls_sqlwhr.

      READ TABLE lt_dd03vt INTO ls_dd03vt WITH KEY fieldname = ls_fields-fieldname.

      CHECK syst-subrc IS INITIAL.

      READ TABLE lt_sqlwhr TRANSPORTING NO FIELDS WITH KEY field1 = ls_dd03vt-fieldname.

      CHECK syst-subrc IS NOT INITIAL.

      ls_sqlwhr-field1 = ls_dd03vt-fieldname.
      ls_sqlwhr-tabname = w-sel_tabname.
      ls_sqlwhr-rollname = ls_dd03vt-rollname.
      APPEND ls_sqlwhr TO lt_sqlwhr.

    ENDLOOP.

    t_sqlwhr[] = lt_sqlwhr[].

    IF t_sqlwhr IS NOT INITIAL.
* Refresh ALV display with New data.
      refresh_alv( iv_soft_refresh = 'X' ).
    ENDIF.

  ENDMETHOD.                    "fill_sqlwhr

  METHOD set_dropdown.
**  Work Area**
    DATA: ls_dropdown TYPE lvc_s_drop.
    DATA: ls_dd07v    TYPE dd07v.
*   Internal Table*
    DATA: lt_dropdown TYPE lvc_t_drop.
    DATA: lt_dd07v    TYPE STANDARD TABLE OF dd07v.

**<<< Main Logic >>>

* First listbox (handle '1').
    ls_dropdown-handle = '1'.
    ls_dropdown-value = 'OR'.
    APPEND ls_dropdown TO lt_dropdown.

    ls_dropdown-handle = '1'.
    ls_dropdown-value = 'AND'.
    APPEND ls_dropdown TO lt_dropdown.

    CLEAR ls_dropdown.

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'DMC_WHERE_OPERATOR'
        langu          = syst-langu
      TABLES
        dd07v_tab      = lt_dd07v
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    IF sy-subrc = 0.

      LOOP AT lt_dd07v INTO ls_dd07v.

        ls_dropdown-handle = '2'.
        ls_dropdown-value = ls_dd07v-domvalue_l.
        APPEND ls_dropdown TO lt_dropdown.

        CLEAR ls_dropdown.
      ENDLOOP.
    ENDIF.

    CALL METHOD me->o_ref_alv->set_drop_down_table
      EXPORTING
        it_drop_down = lt_dropdown.

  ENDMETHOD.                    "set_dropdown
  METHOD refresh_alv.

***<<< Main Logic >>>

    IF iv_soft_refresh IS INITIAL.
      t_sqlwhr[] = it_data[].
    ENDIF.

    o_ref_alv->refresh_table_display( ).

  ENDMETHOD.                   "refresh_alv
  METHOD fill_tab_frm_type.

    DATA : ls_types_meta TYPE ty_types_meta,
           ls_sqlwhr     TYPE ty_sqlwhr.

    DATA : lv_tabix      TYPE sytabix,
           lv_typename   TYPE string.

    DATA : lt_types_meta TYPE STANDARD TABLE OF ty_types_meta.

    CHECK iv_typename IS NOT INITIAL.

    LOOP AT it_types_meta INTO ls_types_meta.

      lv_typename = o_ref_main->ret_actual_typname( iv_is_global = ls_types_meta-global
                                                    iv_typename  = ls_types_meta-typename ).
      IF lv_typename = iv_typename.
        ls_sqlwhr-field1    = ls_types_meta-fieldname.
        APPEND ls_sqlwhr TO t_sqlwhr.
      ENDIF.
    ENDLOOP.

    IF t_sqlwhr IS NOT INITIAL.
* Refresh ALV display with New data.
      refresh_alv( iv_soft_refresh = 'X'  ).

    ENDIF.
  ENDMETHOD.                    "fill_tab_frm_type
ENDCLASS.               "lcl_sqlfields_alv
