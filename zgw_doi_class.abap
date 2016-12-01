*----------------------------------------------------------------------*
*       CLASS ZGW_DOI_CLASS DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zgw_doi_class DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    "创建对象
    METHODS creat_excel
      EXCEPTIONS
        not_find_excel .
    "添加一个表格
    METHODS add_sheet
      IMPORTING
        !i_name TYPE any OPTIONAL .
    "打开文件
    METHODS open_file
      IMPORTING
        !i_path TYPE string .
    "保存文件
    METHODS document_save
      IMPORTING
        value(i_path) TYPE string OPTIONAL .
    "写入数据,用表
    METHODS write_table
      IMPORTING
        !i_startrow TYPE i
        !i_startcol TYPE i
        !it_tab TYPE ANY TABLE .
    "写入数据,单元格
    METHODS write_cell
      IMPORTING
        !i_row TYPE i
        !i_col TYPE i
        !i_val TYPE any .
    METHODS get_range
      IMPORTING
        !i_left TYPE i
        !i_top TYPE i
        !i_rows TYPE i OPTIONAL
        !i_cols TYPE i OPTIONAL
      RETURNING
        value(i_range) TYPE char20 .
    "设置边框线
    METHODS set_ranges_border
      IMPORTING
        !i_range TYPE char20 .
    "设置单元格颜色
    METHODS set_ranges_color
      IMPORTING
        !i_range TYPE char20
        !i_front_color TYPE i OPTIONAL
        !i_back_color TYPE i OPTIONAL .
    "设置单元格样式
    METHODS set_ranges_style
      IMPORTING
        !i_range TYPE char20
        !i_front TYPE i OPTIONAL
        !i_back TYPE i OPTIONAL
        !i_font TYPE char256 OPTIONAL
        !i_size TYPE i OPTIONAL
        !i_bold TYPE i OPTIONAL
        !i_italic TYPE i OPTIONAL
        !i_align TYPE i OPTIONAL
        !i_frametyp TYPE i OPTIONAL
        !i_framecolor TYPE i OPTIONAL
        !i_currency TYPE char3 OPTIONAL
        !i_number TYPE i OPTIONAL
        !i_decimals TYPE i OPTIONAL .
    "设置单元格合并
    METHODS set_ranges_merge
      IMPORTING
        !i_range TYPE char20 .
    METHODS set_ranges_autowrap
      IMPORTING
        !i_range TYPE any .
    METHODS insert_row_or_col
      IMPORTING
        !row TYPE i OPTIONAL
        !col TYPE i OPTIONAL .
protected section.

  methods CONVERT_EN_TO_NO
    importing
      !I_EN type ANY
    returning
      value(I_NO) type I .
  methods CONVERT_NO_TO_EN
    importing
      !I_NO type ANY
    returning
      value(I_EN) type STRING .
  methods CHANGE_RANGENAME_RANGE
    importing
      !I_RANGE type CHAR20 .
  PRIVATE SECTION.

    DATA gcl_splitter TYPE REF TO cl_gui_splitter_container .
    DATA gcl_container TYPE REF TO cl_gui_container .
    DATA gcl_control TYPE REF TO i_oi_container_control .
    DATA gcl_document TYPE REF TO i_oi_document_proxy .
    DATA gcl_spreadsheet TYPE REF TO i_oi_spreadsheet .
    DATA gcl_error TYPE REF TO i_oi_error .
    DATA:gcl_errors LIKE TABLE OF gcl_error .
    DATA g_range TYPE char20 VALUE 'RANGE'.                 "#EC NOTEXT
ENDCLASS.



CLASS ZGW_DOI_CLASS IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZGW_DOI_CLASS->ADD_SHEET
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_NAME                         TYPE        ANY(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_sheet.
    IF gcl_spreadsheet IS BOUND.
      CALL METHOD gcl_spreadsheet->add_sheet
        EXPORTING
          name     = i_name
          no_flush = ''
        IMPORTING
          error    = gcl_error.
    ELSE.
      CALL METHOD gcl_document->create_document
        EXPORTING
          open_inplace = 'X'
        IMPORTING
          error        = gcl_error.
      IF gcl_error->has_failed = 'X'.
        CALL METHOD gcl_error->raise_message
          EXPORTING
            type = 'I'.
        LEAVE LIST-PROCESSING.
      ENDIF.

      CALL METHOD gcl_document->get_spreadsheet_interface
        IMPORTING
          error           = gcl_error
          sheet_interface = gcl_spreadsheet.
      IF gcl_error->has_failed = 'X'.
        CALL METHOD gcl_error->raise_message
          EXPORTING
            type = 'I'.
        LEAVE LIST-PROCESSING.
      ENDIF.

      IF i_name IS NOT INITIAL.
        CALL METHOD gcl_spreadsheet->set_sheet_name
          EXPORTING
            newname = i_name
            oldname = 'Sheet1'
          IMPORTING
            error   = gcl_error.
        IF gcl_error->has_failed = 'X'.
          CALL METHOD gcl_error->raise_message
            EXPORTING
              type = 'I'.
          LEAVE LIST-PROCESSING.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.                    "add_sheet


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZGW_DOI_CLASS->CHANGE_RANGENAME_RANGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_RANGE                        TYPE        CHAR20
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD change_rangename_range.
  DATA: l_lstr TYPE string,
        l_rstr TYPE string,
        l_lcol TYPE string,
        l_lrow TYPE string,
        l_rcol TYPE string,
        l_rrow TYPE string.
  DATA: pos TYPE i.
  DATA: nums TYPE string VALUE '0123456789'.
  "EG I_RANGE LIKE "A1:AB50"
  "分离左边和右边的坐标点
  SPLIT i_range AT ':' INTO l_lstr l_rstr.
  "分离左边坐标点的英文和数字
  WHILE pos LT strlen( l_lstr ).
    IF l_lstr+pos(1) CA sy-abcde.
      CONCATENATE l_lcol l_lstr+pos(1) INTO l_lcol.
      CONDENSE l_lcol NO-GAPS.
    ENDIF.
    IF l_lstr+pos(1) CA nums.
      CONCATENATE l_lrow l_lstr+pos(1) INTO l_lrow.
      CONDENSE l_lrow NO-GAPS.
    ENDIF.
    ADD 1 TO pos.
  ENDWHILE.
  "分离右边坐标点的英文和数字
  IF l_rstr IS INITIAL.
    l_rstr = l_lstr.
  ENDIF.

  CLEAR pos.
  WHILE pos LT strlen( l_rstr ).
    IF l_rstr+pos(1) CA sy-abcde.
      CONCATENATE l_rcol l_rstr+pos(1) INTO l_rcol.
      CONDENSE l_rcol NO-GAPS.
    ENDIF.
    IF l_rstr+pos(1) CA nums.
      CONCATENATE l_rrow l_rstr+pos(1) INTO l_rrow.
      CONDENSE l_rrow NO-GAPS.
    ENDIF.
    ADD 1 TO pos.
  ENDWHILE.

  DATA: l_top TYPE i,
        l_left TYPE i,
        l_rows TYPE i,
        l_cols TYPE i.
  "转换英文成数字
  l_top = l_lrow.  "row
  l_left = me->convert_en_to_no( i_en = l_lcol ). "col
  l_rows = l_rrow.
  l_cols = me->convert_en_to_no( i_en = l_rcol ).
  "求出绝对行和绝对列
  l_rows = l_rows - l_top + 1.
  l_cols = l_cols - l_left + 1.

  CALL METHOD gcl_spreadsheet->insert_range_dim
    EXPORTING
      name     = g_range
      no_flush = 'X'
      top      = l_top
      left     = l_left
      rows     = l_rows
      columns  = l_cols
    IMPORTING
      error    = gcl_error.
  APPEND gcl_error TO gcl_errors.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZGW_DOI_CLASS->CONVERT_EN_TO_NO
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_EN                           TYPE        ANY
* | [<-()] I_NO                           TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_en_to_no.
    DATA: abc TYPE string VALUE '0ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
    DATA: l_offset TYPE i.
    DATA: LT_CHAR_TABLE TYPE TABLE OF STRING,
          LW_CHAR TYPE STRING.
    DATA: COUNT TYPE I VALUE 0.
"把字符串的每个字符分开,放到表LT_CHAR_TABLE里面
    DO STRLEN( I_EN ) TIMES.
      LW_CHAR = I_EN+COUNT(1).
      IF LW_CHAR CA SY-abcde.
        INSERT LW_CHAR INTO LT_CHAR_TABLE INDEX 1.
      ENDIF.
      ADD 1 TO COUNT.
    ENDDO.

    CLEAR COUNT.
    CHECK LT_CHAR_TABLE IS NOT INITIAL.
    LOOP AT LT_CHAR_TABLE INTO LW_CHAR.
      "把英文转中文A=>1,B=>2,Z=>26
      FIND FIRST OCCURRENCE OF LW_CHAR IN abc MATCH OFFSET l_offset.
      "看英文的位置,如果是最右边的,就乘26^0次方,中间的乘26^1次方
      "BA = B=>2*26^1  + A=>1*26^0 = 53
      L_OFFSET = L_OFFSET * ( 26 ** ( SY-TABIX - 1 ) ).
      ADD L_OFFSET TO COUNT.
    ENDLOOP.
    I_NO = COUNT.
  ENDMETHOD.                    "CONVERT_EN_TO_NO


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZGW_DOI_CLASS->CONVERT_NO_TO_EN
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_NO                           TYPE        ANY
* | [<-()] I_EN                           TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_no_to_en.
    DATA: abc TYPE string VALUE '0ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
    DATA: left TYPE i,
          right TYPE i,
          left_c TYPE c,
          right_c TYPE c,
          l_left TYPE i,
          l_left_c TYPE c.

    left = i_no DIV 26.
    right = i_no MOD 26.
    IF left > 26.
      l_left = left MOD 26.
      left = left DIV 26.
    ENDIF.
    left_c = abc+left(1).
    right_c = abc+right(1).
    IF l_left > 0.
      l_left_c = abc+l_left(1).
      CONCATENATE l_left_c left_c right_c INTO i_en.
    ELSEIF left > 0.
      CONCATENATE left_c right_c INTO i_en.
    ELSE.
      i_en = right_c.
    ENDIF.

  ENDMETHOD.                    "CONVERT_NO_COL


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZGW_DOI_CLASS->CREAT_EXCEL
* +-------------------------------------------------------------------------------------------------+
* | [EXC!] NOT_FIND_EXCEL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD creat_excel.
    CALL METHOD c_oi_container_control_creator=>get_container_control
      IMPORTING
        control = gcl_control
        error   = gcl_error.
    IF gcl_error->has_failed = 'X'.
      CALL METHOD gcl_error->raise_message
        EXPORTING
          type = 'E'.
    ENDIF.
* 创建CONTIANER
    CREATE OBJECT gcl_splitter
      EXPORTING
        parent  = cl_gui_container=>screen0
        rows    = 1
        columns = 1.

    CALL METHOD gcl_splitter->set_border
      EXPORTING
        border = cl_gui_cfw=>false.
    gcl_container  = gcl_splitter->get_container( row = 1 column = 1 ).


    CALL METHOD gcl_control->init_control
      EXPORTING
        r3_application_name      = 'OT_EXCEL'               "#EC NOTEXT
        inplace_enabled          = 'X'
        inplace_scroll_documents = 'X'
        parent                   = gcl_container
        register_on_close_event  = 'X'
        register_on_custom_event = 'X'
        no_flush                 = 'X'
      IMPORTING
        error                    = gcl_error
      EXCEPTIONS
*       javabeannotsupported     = 1
        OTHERS                   = 2.
    IF gcl_error->has_failed = 'X'.
      CALL METHOD gcl_error->raise_message
        EXPORTING
          type = 'E'.
    ENDIF.

    CALL METHOD gcl_control->get_document_proxy
      EXPORTING
        document_type  = soi_doctype_excel_sheet
      IMPORTING
        document_proxy = gcl_document
        error          = gcl_error.
    IF gcl_error->has_failed = 'X'.
      CALL METHOD gcl_error->raise_message
        EXPORTING
          type = 'E'.
    ENDIF.
  ENDMETHOD.                    "creat_excel


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZGW_DOI_CLASS->DOCUMENT_SAVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_PATH                         TYPE        STRING(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD document_save.
    IF i_path IS INITIAL.
      DATA: filename TYPE string,
            temp_path TYPE string,
            fullpath TYPE string,
            l_path(100) TYPE c.
      CALL METHOD cl_gui_frontend_services=>file_save_dialog(
        EXPORTING
*         WINDOW_TITLE      = ''
          initial_directory = '%HOMEPATH%\DESKTOP'
          default_file_name = 'EXPORT.XLS'
          file_filter       = '*.XLS;*.XLSX'
        CHANGING
          filename          = filename
          path              = temp_path
          fullpath          = fullpath ).
      i_path = fullpath.
    ENDIF.

    l_path = i_path.

    CALL METHOD gcl_document->save_as
      EXPORTING
        file_name = l_path
      IMPORTING
        error     = gcl_error.
    IF gcl_error->has_failed EQ 'X'.
      CALL METHOD gcl_error->raise_message
        EXPORTING
          type = 'I'.
    ENDIF.
  ENDMETHOD.                    "SAVE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZGW_DOI_CLASS->GET_RANGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_LEFT                         TYPE        I
* | [--->] I_TOP                          TYPE        I
* | [--->] I_ROWS                         TYPE        I(optional)
* | [--->] I_COLS                         TYPE        I(optional)
* | [<-()] I_RANGE                        TYPE        CHAR20
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_range.
    CALL METHOD gcl_spreadsheet->insert_range_dim
      EXPORTING
        name     = g_range
        no_flush = 'X'
        top      = i_top
        left     = i_left
        rows     = i_rows
        columns  = i_cols
      IMPORTING
        error    = gcl_error.
    APPEND gcl_error TO gcl_errors.

    i_range = g_range.
  ENDMETHOD.                    "get_range


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZGW_DOI_CLASS->INSERT_ROW_OR_COL
* +-------------------------------------------------------------------------------------------------+
* | [--->] ROW                            TYPE        I(optional)
* | [--->] COL                            TYPE        I(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD insert_row_or_col.

  ENDMETHOD.                    "INSERT


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZGW_DOI_CLASS->OPEN_FILE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_PATH                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD open_file.
    DATA: l_filename(100) TYPE c.
    l_filename = i_path.

    CALL METHOD gcl_document->open_document
      EXPORTING
*       document_title   = ' '
        document_url     = l_filename
        no_flush         = 'X'
*       open_inplace     = ' '
*       open_readonly    = ' '
*       protect_document = ' '
*       onsave_macro     = ' '
*       startup_macro    = ''
*       user_info        =
      IMPORTING
        error            = gcl_error.
    IF gcl_error->has_failed = 'X'.
      CALL METHOD gcl_error->raise_message
        EXPORTING
          type = 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    CALL METHOD gcl_document->get_spreadsheet_interface
      EXPORTING
        no_flush        = ' '
      IMPORTING
        error           = gcl_error
        sheet_interface = gcl_spreadsheet.
    IF gcl_error->has_failed = 'X'.
      CALL METHOD gcl_error->raise_message
        EXPORTING
          type = 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDMETHOD.                    "open_file


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZGW_DOI_CLASS->SET_RANGES_AUTOWRAP
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_RANGE                        TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_ranges_autowrap."自动换行
    me->change_rangename_range( i_range = i_range ).

    CALL METHOD gcl_spreadsheet->fit_widest
      EXPORTING
        name     = G_range
        no_flush = ''.
  ENDMETHOD.                    "CENTERCELL


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZGW_DOI_CLASS->SET_RANGES_BORDER
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_RANGE                        TYPE        CHAR20
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_ranges_border ."设置边框线
    me->change_rangename_range( i_range = i_range ).

    CALL METHOD gcl_spreadsheet->set_frame
      EXPORTING
        rangename = g_range
        typ       = '127'
        color     = 1
      IMPORTING
        error     = gcl_error.
    APPEND gcl_error TO gcl_errors.
*----------------------------------------------------------------------------------------*
*    typ
*    The control byte type contains the following bits.
*     If a bit is set, its corresponding line is drawn.
*        You can set the thickness of the line to one of four levels using bits 6 and 7.
*    Bit
*    Description
*    0
*    Sets the left margin
*    1
*    Sets the top margin
*    2
*    Sets the bottom margin
*    3
*    Sets the right margin
*    4
*    Horizontal line
*    5
*    Sets the left margin
*    6
*    Thickness
*    7
*    Thickness
*----------------------------------------------------------------------------------------*
  ENDMETHOD.                    "borderrange


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZGW_DOI_CLASS->SET_RANGES_COLOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_RANGE                        TYPE        CHAR20
* | [--->] I_FRONT_COLOR                  TYPE        I(optional)
* | [--->] I_BACK_COLOR                   TYPE        I(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_ranges_color. "设置字体颜色
    me->change_rangename_range( i_range = i_range ).

    CALL METHOD gcl_spreadsheet->set_color
      EXPORTING
        rangename = g_range
        front     = i_front_color
        back      = i_back_color
      IMPORTING
        error     = gcl_error.
  ENDMETHOD.                    "font


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZGW_DOI_CLASS->SET_RANGES_MERGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_RANGE                        TYPE        CHAR20
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_ranges_merge.
    me->change_rangename_range( i_range = i_range ).

    CALL METHOD gcl_spreadsheet->clear_range
      EXPORTING
        name  = g_range
      IMPORTING
        error = gcl_error.

    APPEND gcl_error TO gcl_errors.
  ENDMETHOD.                    "ranges_merge


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZGW_DOI_CLASS->SET_RANGES_STYLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_RANGE                        TYPE        CHAR20
* | [--->] I_FRONT                        TYPE        I(optional)
* | [--->] I_BACK                         TYPE        I(optional)
* | [--->] I_FONT                         TYPE        CHAR256(optional)
* | [--->] I_SIZE                         TYPE        I(optional)
* | [--->] I_BOLD                         TYPE        I(optional)
* | [--->] I_ITALIC                       TYPE        I(optional)
* | [--->] I_ALIGN                        TYPE        I(optional)
* | [--->] I_FRAMETYP                     TYPE        I(optional)
* | [--->] I_FRAMECOLOR                   TYPE        I(optional)
* | [--->] I_CURRENCY                     TYPE        CHAR3(optional)
* | [--->] I_NUMBER                       TYPE        I(optional)
* | [--->] I_DECIMALS                     TYPE        I(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_ranges_style."单元格类型
    DATA: lt_format_table TYPE soi_format_table,
          lw_format_item TYPE soi_format_item.

    me->change_rangename_range( i_range = i_range ).

    lw_format_item-name = g_range.
    lw_format_item-front = i_front.
    lw_format_item-back = i_back.
    lw_format_item-font = i_font.
    lw_format_item-size = i_size.
    lw_format_item-bold = i_bold.
    lw_format_item-italic = i_italic.
    lw_format_item-align = i_align.
    lw_format_item-frametyp = i_frametyp.
    lw_format_item-framecolor = i_framecolor.
    lw_format_item-currency = i_currency.
    lw_format_item-number = i_number.
    lw_format_item-decimals = i_decimals.
    APPEND lw_format_item TO lt_format_table.

    CALL METHOD gcl_spreadsheet->change_style
      EXPORTING
        format = lt_format_table
      IMPORTING
        error  = gcl_error.
    APPEND gcl_error TO gcl_errors.
  ENDMETHOD.                    "celltype


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZGW_DOI_CLASS->WRITE_CELL
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_ROW                          TYPE        I
* | [--->] I_COL                          TYPE        I
* | [--->] I_VAL                          TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD write_cell.
    DATA: columns_number TYPE i,
          rows_number TYPE i.
    DATA: rangeitem TYPE soi_range_item.
    DATA: ranges TYPE soi_range_list.
    DATA: excel_input TYPE soi_generic_table.
    DATA: excel_input_wa TYPE soi_generic_item.
    columns_number = 1.
    rows_number = 1.
    CALL METHOD gcl_spreadsheet->insert_range_dim
      EXPORTING
        name     = g_range
        no_flush = ''
        top      = i_row
        left     = i_col
        rows     = rows_number
        columns  = columns_number
      IMPORTING
        error    = gcl_error.
    APPEND gcl_error TO gcl_errors.


    REFRESH: ranges, excel_input.
    rangeitem-name = g_range.
    rangeitem-columns = 1.
    rangeitem-rows = 1.
    APPEND rangeitem TO ranges.

    excel_input_wa-column = 1.
    excel_input_wa-row = 1.
    excel_input_wa-value = i_val.
    APPEND excel_input_wa TO excel_input.

* set data
    CALL METHOD gcl_spreadsheet->set_ranges_data
      EXPORTING
        ranges   = ranges
        contents = excel_input
        no_flush = ''
      IMPORTING
        error    = gcl_error.
    APPEND gcl_error TO gcl_errors.

    CALL METHOD gcl_spreadsheet->fit_widest
      EXPORTING
        name     = space
        no_flush = ''.
  ENDMETHOD.                    "WRITE_CELL


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZGW_DOI_CLASS->WRITE_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_STARTROW                     TYPE        I
* | [--->] I_STARTCOL                     TYPE        I
* | [--->] IT_TAB                         TYPE        ANY TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD write_table.
    CONSTANTS l_tab TYPE c VALUE cl_abap_char_utilities=>horizontal_tab.
    DATA: lw_row_string(5000) TYPE c,
          lt_row_string LIKE TABLE OF lw_row_string,
          l_string(500) TYPE c.
    DATA: l_rc TYPE i,
          col TYPE i.
    FIELD-SYMBOLS: <wa> TYPE any,
                   <field> TYPE any.

    CHECK it_tab IS NOT INITIAL.

    LOOP AT it_tab ASSIGNING <wa>.
      col = 1.
      CLEAR lw_row_string.
      ASSIGN COMPONENT col OF STRUCTURE <wa> TO <field>.
      WHILE sy-subrc EQ 0.
        IF col = 1.
          lw_row_string = <field>.
        ELSE.
          l_string = <field>.
          CONCATENATE lw_row_string l_tab l_string INTO lw_row_string.
        ENDIF.
        ADD 1 TO col.
        ASSIGN COMPONENT col OF STRUCTURE <wa> TO <field>.
      ENDWHILE.

      APPEND lw_row_string TO lt_row_string.
    ENDLOOP.

    CALL METHOD gcl_spreadsheet->insert_range_dim
      EXPORTING
        name     = g_range
        no_flush = 'X'
        top      = i_startrow
        left     = i_startcol
        rows     = 1
        columns  = 1
      IMPORTING
        error    = gcl_error.
    APPEND gcl_error TO gcl_errors.

    cl_gui_frontend_services=>clipboard_export(
      IMPORTING
        data                 = lt_row_string
      CHANGING
        rc                   = l_rc
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        no_authority         = 4
        OTHERS               = 5
    ).
    IF sy-subrc <> 0.

    ENDIF.

    CALL METHOD gcl_spreadsheet->select_range
      EXPORTING
        rangename = g_range
*       no_flush  = ' '
      IMPORTING
        error     = gcl_error.
    APPEND gcl_error TO gcl_errors.

    CALL METHOD gcl_spreadsheet->set_format
      EXPORTING
        rangename = g_range
        typ       = 0
        currency  = ''
*      no_flush  = ' '
*      decimals  = -1
*    IMPORTING
*      error     =
*      retcode   =
      .


    CALL METHOD gcl_document->paste_clipboard
      IMPORTING
        error = gcl_error.
    APPEND gcl_error TO gcl_errors.
  ENDMETHOD.                    "write_table
ENDCLASS.