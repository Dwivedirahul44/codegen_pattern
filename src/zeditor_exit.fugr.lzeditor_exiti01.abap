*----------------------------------------------------------------------*
***INCLUDE LZEDITOR_EXITI01 .
*----------------------------------------------------------------------*

MODULE exit_processing INPUT.
* CLean Up.
  o_ref_main->clean_up( ).

* Leave.
  LEAVE TO SCREEN 0.


ENDMODULE.                 " EXIT-PROCESSING  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9100 INPUT.
  TRY.
      o_ref_main->process_ucomm(
      EXPORTING
        iv_ucomm = w-ok_code
        iv_dynnr = syst-dynnr
        ).
    CATCH lcl_messages INTO o_ref_msg.
*      *  Clear Input COde if error.
      CLEAR w-ok_code.
      o_ref_msg->display_msg( ).

  ENDTRY.       "

ENDMODULE.                 " USER_COMMAND_9100  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9102 INPUT.

CHECK tabstrip-activetab = 'DATA'.
  TRY.
      o_ref_main->process_ucomm(
      EXPORTING
        iv_ucomm = w-ok_code
        iv_dynnr = syst-dynnr
        ).
    CATCH lcl_messages INTO o_ref_msg.
*    *  Clear Input COde if error.
      CLEAR w-ok_code.
      o_ref_msg->display_msg( ).

  ENDTRY.
ENDMODULE.                 " USER_COMMAND_9102  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9101 INPUT.

CHECK tabstrip-activetab = 'TYPE'.

  TRY.

      o_ref_main->process_ucomm(
      EXPORTING
        iv_ucomm = w-ok_code
        iv_dynnr = syst-dynnr
        ).
    CATCH lcl_messages INTO o_ref_msg.
*  Clear Input COde if error.
      CLEAR w-ok_code.
      o_ref_msg->display_msg( ).

  ENDTRY.

ENDMODULE.                 " USER_COMMAND_9101  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9103 INPUT.

CHECK tabstrip-activetab = 'SELSCR'.

  TRY.
      o_ref_main->process_ucomm(
      EXPORTING
        iv_ucomm = w-ok_code
        iv_dynnr = syst-dynnr
        ).

    CATCH lcl_messages INTO o_ref_msg.
      o_ref_msg->display_msg( ).
*  Clear Input COde if error.
      CLEAR w-ok_code.
  ENDTRY.

ENDMODULE.                 " USER_COMMAND_9103  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9104 INPUT.

  CHECK tabstrip-activetab = 'SQL'.

  TRY.
      o_ref_main->process_ucomm(
      EXPORTING
        iv_ucomm = w-ok_code
        iv_dynnr = syst-dynnr
        ).
    CATCH lcl_messages INTO o_ref_msg.
      o_ref_msg->display_msg( ).
*  Clear Input COde if error.
      CLEAR w-ok_code.
  ENDTRY.

ENDMODULE.                 " USER_COMMAND_9004  INPUT
