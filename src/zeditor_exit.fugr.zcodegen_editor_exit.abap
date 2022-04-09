FUNCTION ZCODEGEN_EDITOR_EXIT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      BUFFER TYPE  RSWSOURCET
*"----------------------------------------------------------------------
FREE buffer[].

  IF o_ref_main IS NOT INITIAL.
    CLEAR o_ref_main.
  ENDIF.

  CREATE OBJECT o_ref_main.

  o_ref_main->gui_init(
  IMPORTING
    et_buffer = buffer[]
  ).





ENDFUNCTION.
