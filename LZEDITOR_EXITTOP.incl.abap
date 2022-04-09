FUNCTION-POOL ZEDITOR_EXIT.                 "MESSAGE-ID ..
*** Tabstrip***
CONTROLS tabstrip TYPE TABSTRIP.
TYPES :
BEGIN OF ty_type,
  tabname      TYPE tabname,    "Table name
  fieldname    TYPE fieldname,  "Fieldname
  position     TYPE tabfdpos,
  rollname     TYPE typename,   "Dataelement
  ddtext       TYPE as4text,    "Description
  include_type TYPE xfeld,
END OF ty_type,
* Types_Meta*
BEGIN OF ty_types_meta,
  index        TYPE syindex,
  position     TYPE tabfdpos,
  typename     TYPE typename,
  fieldname    TYPE fieldname,
  rollname     TYPE rollname,
  ddtext       TYPE as4text,
  include_type TYPE xfeld,
  last         TYPE char1,
  dwarea       TYPE char1,
  ditab        TYPE char1,
  global       TYPE char1,
  local        TYPE char1,
END OF ty_types_meta,
* Data*
BEGIN OF ty_data,
  varname    TYPE char30,
  type       TYPE char30,
  rollname   TYPE typename,
END OF ty_data,
* ALV Messages*
BEGIN OF ty_alv_protocol_msg,
  msgid     TYPE symsgid,
  msgty     TYPE symsgty,
  msgno     TYPE symsgno,
  msg       TYPE string,
  fieldname TYPE lvc_fname,
  row_id    TYPE int4,
END OF ty_alv_protocol_msg,
* DD03vt*
BEGIN OF ty_dd03vt,
  tabname    TYPE tabname, " Table Name
  fieldname  TYPE fieldname, " Field Name
  position   TYPE tabfdpos, " Position of the field in the table
  rollname   TYPE rollname,
  checktable TYPE checktable, " check table
  ddtext     TYPE as4text, " Short Description of Repository Objects
END OF ty_dd03vt,
* Selection Screen*
BEGIN OF ty_selscr,
  name        TYPE char8,
  type        TYPE char15,
  rollname    TYPE rollname,
  obligatory  TYPE char1,
  checkfield TYPE fieldname,
  check_table TYPE tabname,
END OF ty_selscr,
BEGIN OF ty_sqlfields,
  tabname    TYPE tabname,
  fieldname  TYPE fieldname,
  positon    TYPE tabfdpos,
  rollname   TYPE rollname,
END OF   ty_sqlfields,
BEGIN OF ty_sqlwhr,
  para_begin TYPE char1,
  field1     TYPE fieldname,
  opr        TYPE char3,
  field2     TYPE fieldname,
  positon    TYPE tabfdpos,
  para_end   TYPE char1,
  and_or     TYPE vsconj,
  TABNAME    TYPE TABNAME,
  rollname   TYPE rollname,
END OF   ty_sqlwhr,
BEGIN OF ty_fields,
  tabname    TYPE tabname,
  fieldname  TYPE fieldname,
  rollname   TYPE rollname,
END OF   ty_fields.
* Table Types*
TYPES :
  tty_dd03vt             TYPE STANDARD TABLE OF ty_dd03vt,
  tty_alv_protocol_msgs  TYPE STANDARD TABLE OF ty_alv_protocol_msg,
  tty_fields             TYPE STANDARD TABLE OF ty_fields,
  tty_types_meta         TYPE STANDARD TABLE OF ty_types_meta
  .
*Variables**
DATA:
BEGIN OF w,
  copy_code      TYPE char1,
  sql_types      TYPE char20,
  ty_prefix      TYPE char4,
  seltab_dis     TYPE char1,
  selscr         TYPE ty_selscr,
  data           TYPE ty_data,
  typename       TYPE char20,
  tabname        TYPE tabname,
  fields         TYPE ty_type,
  ok_code        TYPE ok,
  alv_error      TYPE char1,
  type_index     TYPE syindex,
  dwarea         TYPE xfeld,
  ditab          TYPE xfeld,
  data_tabname   TYPE char30,
  warea_pf       TYPE char4 VALUE 'W',
  itab_pf        TYPE char4 VALUE 'T_',
  types_pf       TYPE char4 VALUE 'TY_',
  lwarea_pf      TYPE char4 VALUE 'LS_',
  litab_pf       TYPE char4 VALUE 'LT_',
  ltypes_pf      TYPE char4 VALUE 'LTY_',
  param_pf       TYPE char4 VALUE 'P_',
  selopt_pf      TYPE char4 VALUE 'S_',
  obj_pf         TYPE char4 VALUE 'O_',
  lobj_pf        TYPE char4 VALUE 'LO_',
  global_chk     TYPE xfeld VALUE 'X',
  local_chk      TYPE xfeld,
  none           TYPE xfeld,
  global_data    TYPE xfeld ,
  local_data     TYPE xfeld,
  error          TYPE char1,
  sel_tabname    TYPE tabname,
  selscr_tabname TYPE tabname,
  fr_al_entries  TYPE char1,
  fr_al_tabname  TYPE tabname,
  in_tab         TYPE char1,
  in_var         TYPE char1,
  in_varname     TYPE fieldname,
  sel_qry_opt    TYPE char1,
  sel_single     TYPE char1,
  sel_rows       TYPE char1,
  sel_rows_cnt   TYPE num5,
END OF w.
*** Tables***
DATA :
  t_buffer       TYPE  rswsourcet.
INCLUDE LZEDITOR_EXITD01.
** Main Class
DATA : o_ref_main TYPE REF TO lcl_main.
DATA : o_ref_msg  TYPE REF TO lcl_messages.
