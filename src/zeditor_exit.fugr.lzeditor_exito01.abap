*----------------------------------------------------------------------*
***INCLUDE LZEDITOR_EXITO01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9100 OUTPUT.
** Set Pf Status.
  SET PF-STATUS 'BASIC1'.

ENDMODULE.                 " STATUS_9100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9101 OUTPUT.
** Initiate TYPES alv
  CHECK tabstrip-activetab = 'TYPE' .

  IF  o_ref_main->o_ref_types_alv IS INITIAL.

    CREATE OBJECT o_ref_main->o_ref_types_alv
      EXPORTING
        iv_container         = 'CONT'
        iv_tabname           = o_ref_main->c-my_fieldstabname
        iv_tabtype           = o_ref_main->c-my_fieldstabtype
        iv_set_toolbar       = 'X'
        iv_set_toolbar_click = 'X'
        iv_reg_edit_enter    = 'X'.

    o_ref_main->o_ref_types_alv->first_display( ).

  ENDIF.

*** Set Prefix based on Radio Button.
  IF w-global_chk IS NOT INITIAL.
    w-ty_prefix = w-types_pf.
  ELSEIF w-local_chk IS NOT INITIAL.
    w-ty_prefix = w-ltypes_pf.
  ELSE .
    CLEAR w-ty_prefix.
  ENDIF.


ENDMODULE.                 " STATUS_9101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9102 OUTPUT.

  CHECK tabstrip-activetab = 'DATA' .

** Initiate Data ALV
  IF  o_ref_main->o_ref_data_alv IS INITIAL .

    CREATE OBJECT o_ref_main->o_ref_data_alv
      EXPORTING
        iv_container         = 'CONT_DATA'
        iv_tabname           = o_ref_main->c-my_datatabname
        iv_tabtype           = o_ref_main->c-my_datatabtype
        iv_set_toolbar       = 'X'
        iv_set_toolbar_click = 'X'
        iv_reg_edit_enter    = 'X'.

    o_ref_main->o_ref_data_alv->first_display( ).


  ENDIF.

ENDMODULE.                 " STATUS_9102  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9103 OUTPUT.
  CHECK tabstrip-activetab = 'SELSCR'  .
*** Initiate Selection Screen ALV
  IF o_ref_main->o_ref_selscr_alv IS INITIAL.

    CREATE OBJECT o_ref_main->o_ref_selscr_alv
      EXPORTING
        iv_container         = 'CONT_SELSCR'
        iv_tabname           = o_ref_main->c-my_selscrtabname
        iv_tabtype           = o_ref_main->c-my_selscrtabtype
        iv_set_toolbar       = 'X'
        iv_set_toolbar_click = 'X'
        iv_reg_edit_enter    = 'X'.

    o_ref_main->o_ref_selscr_alv->first_display( ).

  ENDIF.

ENDMODULE.                 " STATUS_9103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9104 OUTPUT.
  CHECK tabstrip-activetab = 'SQL'.

*** Disable Input If required.
  IF ( w-sel_tabname IS NOT INITIAL
   AND w-ok_code <> 'REF_INP' )
       OR ( w-sql_types IS NOT INITIAL
      AND ( w-ok_code = 'SQL_PWHR'
         OR w-ok_code = 'SQL_PFIELDS' ) )  .

    LOOP AT SCREEN.
      IF screen-name = 'W-SEL_TABNAME'
      OR screen-name = 'W-SQL_TYPES'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.
  ENDIF.

** Display Sql Fields ALV.
  IF  o_ref_main->o_ref_sqlfields_alv IS INITIAL.
*  AND tabstrip-activetab = 'SQL'.

    CREATE OBJECT o_ref_main->o_ref_sqlfields_alv
      EXPORTING
        iv_container   = 'CONT_SQLFIELDS'
        iv_tabname     = o_ref_main->c-my_sqlfieldstabname
        iv_tabtype     = o_ref_main->c-my_sqlfieldstype
        iv_set_toolbar = 'X'.

    o_ref_main->o_ref_sqlfields_alv->first_display( ).

  ENDIF.

*** Display SQL Where COndition ALV.
  IF o_ref_main->o_ref_sqlwhr_alv IS INITIAL.

    CREATE OBJECT o_ref_main->o_ref_sqlwhr_alv
      EXPORTING
        iv_container         = 'CONT_SQLWHR'
        iv_tabname           = o_ref_main->c-my_sqlwhrtabname
        iv_tabtype           = o_ref_main->c-my_sqlwhrtype
        iv_set_toolbar       = 'X'
        iv_set_toolbar_click = 'X'
        iv_reg_edit_enter    = 'X'.
    o_ref_main->o_ref_sqlwhr_alv->first_display( ).

  ENDIF.

  o_ref_main->set_values_ddown( syst-dynnr ).

ENDMODULE.                 " STATUS_9004  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9109  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9109 OUTPUT.

  o_ref_main->set_prefixes( ).

ENDMODULE.                 " STATUS_9109  OUTPUT
