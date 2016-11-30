*----------------------------------------------------------------------*
*       CLASS CL_fix_source_list DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_fix_inforecord_sourcelist DEFINITION.
  "�޸�����Դ�嵥����

  PUBLIC SECTION.
    METHODS constructor IMPORTING it_tab TYPE STANDARD TABLE i_type TYPE c DEFAULT 'P'.


    TYPES: BEGIN OF st_matnr ,
      matnr TYPE mara-matnr,
      lifnr TYPE eina-lifnr,
      END OF st_matnr,
      table_matnr TYPE TABLE OF st_matnr.

    TYPES: BEGIN OF st_msg,
      matnr TYPE mara-matnr,
      msgtyp TYPE msgtyp,
      msg TYPE msg,
      END OF st_msg.

    TYPES:BEGIN OF st_marc,
    "��ʼ��Ļ
      matnr TYPE mara-matnr,    "���ϱ���                          ������ͼ1
      mtart TYPE mara-mtart,    "��������                          ������ͼ1
      werks TYPE marc-werks,    "����                            �����ƻ���ͼ
      beskz TYPE marc-beskz,    "�ɹ�����
      sobsl TYPE marc-sobsl,    "����ɹ�����
      "�ɹ���ͼ
      ekgrp TYPE marc-ekgrp,    "�ɹ���                            �ɹ���ͼ
      kordb TYPE marc-kordb,    "Դ�嵥                            �ɹ���ͼ

      plifz TYPE marc-plifz,    "�ɹ�����   �ƻ������ڽ���          MRP��ͼ2
      webaz TYPE marc-webaz,    "�ջ�����ʱ��                       �ɹ���ͼ
      bstrf TYPE marc-bstrf,     "����ֵ      ��С��װ              MRP��ͼ1
      END OF st_marc.

  PRIVATE SECTION.
    "��ȡ�ɹ���Ϣ
    METHODS fm_get_purch_info.
    "��ȡ�ɹ���Ϣ��¼
    METHODS fm_get_purch_info_record.
    "��ȡ�ɹ���Ϣ��¼��������
*    METHODS fm_get_purch_info_condition.
    "��ȡԴ�嵥
    METHODS fm_get_source_list
      IMPORTING i_matnr TYPE st_matnr.
    "�����ɹ���Ϣ��¼
    METHODS fm_create_info_record
      IMPORTING i_matnr TYPE st_matnr value(i_lohnb) TYPE c.
    "���Ĳɹ���Ϣ��¼,������������6
    METHODS fm_modify_info_record
      IMPORTING i_matnr TYPE st_matnr value(i_lohnb) TYPE c.
    "���Ĳɹ���Ϣ��¼,û�вɹ���
    METHODS fm_modify_info_record1
      IMPORTING i_matnr TYPE st_matnr value(i_lohnb) TYPE c.
    "������Դ�嵥
    METHODS fm_create_source_list
      IMPORTING i_matnr TYPE st_matnr value(i_count) TYPE i.
    "�����ɹ�����
    METHODS fm_create_purch_info
      IMPORTING value(i_matnr) TYPE st_matnr.
    "ȡ���ɹ���Ϣ��¼��ɾ�����
    METHODS fm_set_purch_info_undelete
      IMPORTING value(i_matnr) TYPE st_matnr value(i_lohnb) TYPE c.
    METHODS fm_set_purch_info_delete
      IMPORTING value(i_matnr) TYPE st_matnr value(i_lohnb) TYPE c.

    DATA: gw_matnr TYPE st_matnr,
          gt_matnr TYPE TABLE OF st_matnr,
          gw_msg TYPE st_msg,
          gt_msg TYPE TABLE OF st_msg,
          gw_marc TYPE st_marc,
          gt_marc TYPE SORTED TABLE OF st_marc WITH UNIQUE KEY primary_key COMPONENTS matnr.

    CONSTANTS con_werks TYPE werks VALUE '8001'.
    CONSTANTS con_ekorg TYPE ekorg VALUE '8000'.
ENDCLASS.                    "CL_fix_source_list DEFINITION

DEFINE bdc_dynpro.
  clear lw_bdcdata.
  lw_bdcdata-program = &1.
  lw_bdcdata-dynpro = &2.
  lw_bdcdata-dynbegin = 'X'.
  append lw_bdcdata to lt_bdcdata.
END-OF-DEFINITION.

DEFINE bdc_field.
  clear lw_bdcdata.
  lw_bdcdata-fnam = &1.
  lw_bdcdata-fval = &2.
  append lw_bdcdata to lt_bdcdata.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
*       CLASS CL_FIX_SOURCE_LIST IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_fix_inforecord_sourcelist IMPLEMENTATION.
  METHOD constructor.
    DATA: lt_fcat TYPE lvc_t_fcat,
          line TYPE i.
    FIELD-SYMBOLS <wa> TYPE any.
    DATA: l_lohnb TYPE c.

    LOOP AT it_tab ASSIGNING <wa>.
      MOVE-CORRESPONDING <wa> TO gw_matnr.
      IF gw_matnr-lifnr IS INITIAL.
        gw_msg-matnr = gw_matnr-matnr.
        gw_msg-msgtyp = 'E'.
        gw_msg-msg = '����û�й�Ӧ��,������Ӳɹ���Ϣ��¼��Դ�嵥'.
        APPEND gw_msg TO gt_msg.
      ELSE.
        APPEND gw_matnr TO gt_matnr.
      ENDIF.
    ENDLOOP.
    SORT gt_matnr.
    DELETE ADJACENT DUPLICATES FROM gt_matnr.

    IF i_type = 'P'."P�ǲɹ���Ϣ��¼�����޸Ļ�����
      CALL METHOD fm_get_purch_info.
      CALL METHOD fm_get_purch_info_record.
    ELSEIF i_type = 'S'."S��Դ�嵥��鼰����
      LOOP AT gt_matnr INTO gw_matnr.
        me->fm_get_source_list( i_matnr = gw_matnr ).
      ENDLOOP.
    ELSEIF i_type = 'D'."D��ɾ���ɹ���Ϣ��¼
      LOOP AT gt_matnr INTO gw_matnr.
        SELECT SINGLE sobsl INTO gw_marc-sobsl FROM marc WHERE matnr = gw_matnr-matnr.
        IF gw_marc-sobsl = '30'.
          l_lohnb = 'X'.
        ELSE.
          l_lohnb = ''.
        ENDIF.
        me->fm_set_purch_info_delete( i_matnr = gw_matnr i_lohnb = l_lohnb ).
      ENDLOOP.
    ENDIF.

    lt_fcat = zgw_yi_class=>get_fieldcat( it_tab = gt_msg ).
    line = lines( gt_msg ) + 1.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        it_fieldcat_lvc       = lt_fcat
        i_screen_start_column = 1
        i_screen_start_line   = 1
        i_screen_end_column   = 118
        i_screen_end_line     = line
      TABLES
        t_outtab              = gt_msg
      EXCEPTIONS
        program_error         = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.                    "CONSTRUCTOR


  METHOD fm_get_purch_info.
*====================================================================*
*COMMENT BY YI 17.10.2016 22:28:48
*�ж�������û�вɹ���ͼ,�ǲ����⹺��,�Ƿ����
*====================================================================*
    CHECK gt_matnr IS NOT INITIAL.

    SELECT a~matnr a~mtart c~werks c~beskz c~sobsl c~ekgrp c~kordb c~plifz c~webaz c~bstrf
        INTO TABLE gt_marc FROM mara AS a
        JOIN marc AS c ON a~matnr = c~matnr
        FOR ALL ENTRIES IN gt_matnr
        WHERE a~matnr = gt_matnr-matnr.

    LOOP AT gt_matnr INTO gw_matnr.
      READ TABLE gt_marc INTO gw_marc WITH KEY matnr = gw_matnr-matnr BINARY SEARCH.
      "�������û������,˵��������û���ṩ��������,�����Ҳ�������
      IF sy-subrc NE 0.
        gw_msg-matnr = gw_matnr-matnr.
        gw_msg-msgtyp = 'E'.
        gw_msg-msg = '���ϲ�����'.
        APPEND gw_msg TO gt_msg.
        DELETE gt_matnr.
        CONTINUE.
      ELSEIF sy-subrc EQ 0.
        "����������ʱ���ж��Ƿ�Ϊ���Ƽ�,��ûά���ɹ�����
        IF gw_marc-beskz = 'E'.
          gw_msg-matnr = gw_matnr-matnr.
          gw_msg-msgtyp = 'E'.
          gw_msg-msg = '���Ƽ�����Ҫά���ɹ���ͼ'.
          APPEND gw_msg TO gt_msg.
          DELETE gt_matnr.
          CONTINUE.
        ENDIF.

        IF gw_marc-ekgrp IS INITIAL OR gw_marc-kordb IS INITIAL OR gw_marc-plifz IS INITIAL.
          gw_msg-matnr = gw_matnr-matnr.
          gw_msg-msgtyp = 'E'.
          gw_msg-msg = 'û�вɹ���ͼ,��Ҫ��ά���ɹ���ͼ����'.
          APPEND gw_msg TO gt_msg.
          DELETE gt_matnr.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "FM_GET_PURCH_INFO

  METHOD fm_get_purch_info_record."��ȡ�ɹ���Ϣ��¼
    DATA: BEGIN OF lw_eina,
      matnr TYPE eina-matnr,"����
      infnr TYPE eina-infnr,"�ɹ���Ϣ��¼
      lifnr TYPE eina-lifnr,"��Ӧ�̴���
      loekz TYPE eina-loekz,"�ɹ���Ϣ: ���Ϊɾ����ͨ������
      esokz TYPE eine-esokz,"��Ϣ���  0 ��׼ 3 ��Э
      ekgrp TYPE eine-ekgrp,"�ɹ���
      netpr TYPE eine-netpr,"��Ч�ɹ���
      iloee TYPE eine-loekz,"�ɹ���Ϣ: ��֯���ݱ��Ϊɾ��
      END OF lw_eina,
      lt_eina LIKE SORTED TABLE OF lw_eina WITH NON-UNIQUE KEY matnr.
    DATA: l_lohnb TYPE c.
*====================================================================*
*COMMENT BY YI 17.10.2016 22:29:13
*�ж�������û�вɹ���Ϣ��¼,��Ϣ����Ƿ�ƥ��
*====================================================================*
    CHECK gt_matnr IS NOT INITIAL.

    SELECT a~matnr a~infnr a~lifnr a~loekz e~esokz e~ekgrp e~netpr e~loekz AS iloee
      INTO TABLE lt_eina FROM eina AS a JOIN eine AS e ON a~infnr = e~infnr
      FOR ALL ENTRIES IN gt_matnr
      WHERE a~matnr = gt_matnr-matnr.

    LOOP AT gt_matnr INTO gw_matnr.
      me->fm_get_source_list( i_matnr = gw_matnr )."����Դ�嵥

      READ TABLE gt_marc INTO gw_marc WITH KEY matnr = gw_matnr-matnr.
      IF gw_marc-sobsl = '30'.
        l_lohnb = 'X'.
        lw_eina-esokz = 3.
      ELSE.
        l_lohnb = ''.
        lw_eina-esokz = 0.
      ENDIF.

      READ TABLE lt_eina INTO lw_eina WITH KEY matnr = gw_matnr-matnr lifnr = gw_matnr-lifnr esokz = lw_eina-esokz.
      "���������ݴ����вɹ���Ϣ��¼
      IF sy-subrc EQ 0.
        IF lw_eina-loekz = 'X' OR lw_eina-iloee = 'X'.
          gw_msg-matnr = gw_matnr-matnr.
          gw_msg-msgtyp = 'W'.
          gw_msg-msg = '��Ȼ�вɹ���Ϣ��¼,�����ѱ��Ϊɾ��'.
          APPEND gw_msg TO gt_msg.

          me->fm_set_purch_info_undelete(
            EXPORTING
              i_matnr = gw_matnr
              i_lohnb = l_lohnb
          ).
        ENDIF.

        IF lw_eina-netpr = 0.
          gw_msg-matnr = gw_matnr-matnr.
          gw_msg-msgtyp = 'W'.
          gw_msg-msg = '��Ȼ�вɹ���Ϣ��¼,����ȱ�ټ۸�'.
          APPEND gw_msg TO gt_msg.
          DELETE gt_matnr.

          me->fm_modify_info_record1(
                EXPORTING
                  i_matnr = gw_matnr
                  i_lohnb = l_lohnb
              ).
          CONTINUE.
        ELSEIF lw_eina-netpr < '5.85' AND lw_eina-netpr > 0.
          gw_msg-matnr = gw_matnr-matnr.
          gw_msg-msgtyp = 'W'.
          gw_msg-msg = '��Ȼ�вɹ���Ϣ��¼,���Ǽ۸�̫С'."�ɹ�һ��ʱ�۸�Ҫ��С��5.85
          APPEND gw_msg TO gt_msg.
          DELETE gt_matnr.

          me->fm_modify_info_record(
                EXPORTING
                  i_matnr = gw_matnr
                  i_lohnb = l_lohnb
              ).
          CONTINUE.
        ENDIF.
        "�ɹ��鲻һ��ʱ֪ͨ�û�
        IF lw_eina-ekgrp NE gw_marc-ekgrp.
          gw_msg-matnr = gw_matnr-matnr.
          gw_msg-msgtyp = 'W'.
          gw_msg-msg = '�ɹ���Ϣ��¼�ɹ����������ݲ�һ��'.
          APPEND gw_msg TO gt_msg.
        ENDIF.
        gw_msg-matnr = gw_matnr-matnr.
        gw_msg-msgtyp = 'S'.
        gw_msg-msg = '�������в�������'.
        APPEND gw_msg TO gt_msg.
        DELETE gt_matnr.
        CONTINUE.
      ELSE."û�оͱ���
        gw_msg-matnr = gw_matnr-matnr.
        gw_msg-msgtyp = 'W'.
        gw_msg-msg = 'û�вɹ���Ϣ��¼,��Ҫά��'.
        APPEND gw_msg TO gt_msg.
        DELETE gt_matnr.

        me->fm_create_info_record(
              EXPORTING
                i_matnr = gw_matnr
                i_lohnb = l_lohnb
            ).
        CONTINUE.
      ENDIF.

      CLEAR: gw_marc,l_lohnb.
    ENDLOOP.
  ENDMETHOD.                    "fm_get_purch_info_record


  METHOD fm_get_source_list.
    DATA: BEGIN OF lw_eord,
      matnr TYPE eord-matnr,
      lifnr TYPE eord-lifnr,
      END OF lw_eord,
      lt_eord LIKE TABLE OF lw_eord.
    DATA: l_count TYPE i.

    SELECT matnr lifnr INTO TABLE lt_eord FROM eord
      WHERE matnr = i_matnr-matnr AND lifnr = i_matnr-lifnr.

    IF lt_eord IS INITIAL."û�л�Դ�嵥
      gw_msg-matnr = i_matnr-matnr.
      gw_msg-msgtyp = 'W'.
      gw_msg-msg = '����ȱ��' && i_matnr-lifnr && '�Ļ�Դ�嵥'.
      APPEND gw_msg TO gt_msg.
      "������ԭ���ж�������Դ�嵥
      SELECT COUNT(*) INTO l_count FROM eord
        WHERE matnr = i_matnr-matnr.

      ADD 1 TO l_count.

      me->fm_create_source_list( i_matnr = i_matnr i_count = l_count ).
    ELSE.
      gw_msg-matnr = i_matnr-matnr.
      gw_msg-msgtyp = 'S'.
      gw_msg-msg = '���ϻ�Դ�嵥����'.
      APPEND gw_msg TO gt_msg.
    ENDIF.
  ENDMETHOD.                    "FM_GET_SOURCE_LIST


  METHOD fm_create_info_record."�����ɹ���Ϣ��¼
    DATA: lw_bdcdata TYPE bdcdata,
          lt_bdcdata TYPE TABLE OF bdcdata,
          lw_msgtab TYPE bdcmsgcoll,
          lt_msgtab TYPE TABLE OF bdcmsgcoll.

    "ME11����Ļ
    bdc_dynpro:'SAPMM06I' '0100'.
    bdc_field:'BDC_OKCODE' '/00',
    'EINA-LIFNR' i_matnr-lifnr,
    'EINA-MATNR' i_matnr-matnr,
    'EINE-EKORG' con_ekorg,
    'EINE-WERKS' con_werks.
    IF i_lohnb = 'X'.
      bdc_field:'RM06I-LOHNB' 'X'.
    ELSE.
      bdc_field:'RM06I-NORMB' 'X'.
    ENDIF.
    "ME11��һ��Ļ  һ������
    bdc_dynpro:
    'SAPMM06I' '0101'.
    bdc_field:
    'BDC_OKCODE' '=EINE'.
    "ME11�ڶ���Ļ  �ɹ���֯����
    bdc_dynpro:
    'SAPMM06I' '0102'.
    bdc_field:
    'BDC_OKCODE' '=KO',
    'EINE-MWSKZ' 'J1'.
    "ME11������Ļ  ����
    bdc_dynpro:
    'SAPMV13A' '0201'.
    bdc_field:
    'BDC_OKCODE' '=SICH',
    'RV13A-DATAB' '20150401',
    'RV13A-DATBI' '99991231',
    "PB00
    'KONP-KBETR(01)' '0',
    'KONP-KPEIN(01)' '1170',
    "ZP00
    'KONP-KSCHL(02)' 'ZP00',
    'KONP-KBETR(02)' '6',
    'KONP-KPEIN(02)' '1170'.

    REFRESH lt_msgtab.

    CALL TRANSACTION 'ME11' USING lt_bdcdata MODE 'E' MESSAGES INTO lt_msgtab.

    LOOP AT lt_msgtab INTO lw_msgtab.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = lw_msgtab-msgid
          msgnr               = lw_msgtab-msgnr
          msgv1               = lw_msgtab-msgv1
          msgv2               = lw_msgtab-msgv2
          msgv3               = lw_msgtab-msgv3
          msgv4               = lw_msgtab-msgv4
        IMPORTING
          message_text_output = gw_msg-msg.
      gw_msg-msgtyp = lw_msgtab-msgtyp.
      gw_msg-matnr = i_matnr-matnr.
      APPEND gw_msg TO gt_msg.
    ENDLOOP.
  ENDMETHOD.                    "fm_create_purch_info_record

  METHOD fm_modify_info_record."���Ĳɹ���Ϣ��¼
    DATA: lw_bdcdata TYPE bdcdata,
      lt_bdcdata TYPE TABLE OF bdcdata,
      lw_msgtab TYPE bdcmsgcoll,
      lt_msgtab TYPE TABLE OF bdcmsgcoll.
    DATA: l_str TYPE string,
          l_row(2) TYPE n.
    "EORD �������ϵĻ�Դ�嵥
    "A017 ���������۸���������Ĺ���
    "KONP ���������۸�ĸ�������
    "����Դ�嵥�ı�
    DATA: BEGIN OF lw_a017,
      matnr TYPE a017-matnr,
      lifnr TYPE a017-lifnr,
      datbi TYPE a017-datbi,
      datab TYPE a017-datab,
      knumh TYPE a017-knumh,
      END OF lw_a017,
      lt_a017 LIKE TABLE OF lw_a017.
    DATA: l_esokz TYPE c.

    DATA: BEGIN OF lw_konp,
      knumh TYPE konp-knumh,"������¼��
      kschl TYPE konp-kschl,"��������
      kbetr TYPE konp-kbetr,"�۸� (��������ٷ��� ) �޵ȼ�����
      loevm_ko TYPE loevm_ko,"�������ɾ��ָʾ��
      zaehk_ind TYPE konp-zaehk_ind,"������Ŀ����
      END OF lw_konp,
      lt_konp LIKE TABLE OF lw_konp.

    CHECK i_matnr IS NOT INITIAL.

    IF i_lohnb = 'X'.
      l_esokz = '3'.
    ELSE.
      l_esokz = '0'.
    ENDIF.
    SELECT lifnr matnr datbi datab knumh
      INTO CORRESPONDING FIELDS OF TABLE lt_a017 FROM a017
      WHERE matnr = i_matnr-matnr
      AND lifnr = i_matnr-lifnr
      AND kschl = 'PB00'
      AND esokz = l_esokz.

    SORT lt_a017 BY datbi.
    LOOP AT lt_a017 INTO lw_a017 WHERE datbi > sy-datum AND datab <= sy-datum.
      l_row = sy-tabix.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_konp FROM konp
        WHERE knumh = lw_a017-knumh
        AND kschl = 'ZP00'
        AND loevm_ko NE 'X'.
    ENDLOOP.
    "ME12����Ļ
    bdc_dynpro:'SAPMM06I' '0100'.
    bdc_field:'BDC_OKCODE' '/00',
    'EINA-LIFNR' i_matnr-lifnr,
    'EINA-MATNR' i_matnr-matnr,
    'EINE-EKORG' con_ekorg,
    'EINE-WERKS' con_werks.
    IF i_lohnb = 'X'.
      bdc_field:'RM06I-LOHNB' 'X'.
    ELSE.
      bdc_field:'RM06I-NORMB' 'X'.
    ENDIF.
    "ME12��һ��Ļ  һ������
    bdc_dynpro:
    'SAPMM06I' '0101'.
    bdc_field:
    'BDC_OKCODE' '=KO'.
    "ME12�ڶ���Ļ ѡ����Ч��
    l_str =  'VAKE-DATAB(' && l_row && ')'."���ѡ�����
    bdc_dynpro:
    'SAPLV14A' '0102'.
    bdc_field:
    'BDC_CURSOR' l_str,
    'BDC_OKCODE' '=PICK'.
    "ME12������Ļ  ����
    READ TABLE lt_konp INTO lw_konp INDEX 1.
    l_row = lw_konp-zaehk_ind.
    l_str = 'KONP-KBETR(' && l_row && ')'.
    bdc_dynpro:
    'SAPMV13A' '0201'.
    bdc_field:
    'BDC_OKCODE' '=SICH',
    "ZP00
    l_str '6'.

    REFRESH lt_msgtab.

    CALL TRANSACTION 'ME12' USING lt_bdcdata MODE 'E' MESSAGES INTO lt_msgtab.

    LOOP AT lt_msgtab INTO lw_msgtab.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = lw_msgtab-msgid
          msgnr               = lw_msgtab-msgnr
          msgv1               = lw_msgtab-msgv1
          msgv2               = lw_msgtab-msgv2
          msgv3               = lw_msgtab-msgv3
          msgv4               = lw_msgtab-msgv4
        IMPORTING
          message_text_output = gw_msg-msg.
      gw_msg-msgtyp = lw_msgtab-msgtyp.
      gw_msg-matnr = i_matnr-matnr.
      APPEND gw_msg TO gt_msg.
    ENDLOOP.
  ENDMETHOD.                    "fm_MODIFY_purch_info_record


  METHOD fm_modify_info_record1."���Ĳɹ���Ϣ��¼
    DATA: lw_bdcdata TYPE bdcdata,
      lt_bdcdata TYPE TABLE OF bdcdata,
      lw_msgtab TYPE bdcmsgcoll,
      lt_msgtab TYPE TABLE OF bdcmsgcoll.

    "ME12����Ļ
    bdc_dynpro:'SAPMM06I' '0100'.
    bdc_field:'BDC_OKCODE' '/00',
    'EINA-LIFNR' i_matnr-lifnr,
    'EINA-MATNR' i_matnr-matnr,
    'EINE-EKORG' con_ekorg,
    'EINE-WERKS' con_werks.
    IF i_lohnb = 'X'.
      bdc_field:'RM06I-LOHNB' 'X'.
    ELSE.
      bdc_field:'RM06I-NORMB' 'X'.
    ENDIF.
    "ME12��һ��Ļ  һ������
    bdc_dynpro:
    'SAPMM06I' '0101'.
    bdc_field:
    'BDC_OKCODE' '=KO'.
    "ME12�ڶ���Ļ  ����
    bdc_dynpro:
    'SAPMV13A' '0201'.
    bdc_field:
    'BDC_OKCODE' '=SICH',
    'RV13A-DATAB' '20150401',
    'RV13A-DATBI' '99991231',
    "PB00
    'KONP-KBETR(01)' '0',
    'KONP-KPEIN(01)' '1170',
    "ZP00
    'KONP-KSCHL(02)' 'ZP00',
    'KONP-KBETR(02)' '6',
    'KONP-KPEIN(02)' '1170'.

    REFRESH lt_msgtab.

    CALL TRANSACTION 'ME12' USING lt_bdcdata MODE 'E' MESSAGES INTO lt_msgtab.

    LOOP AT lt_msgtab INTO lw_msgtab.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = lw_msgtab-msgid
          msgnr               = lw_msgtab-msgnr
          msgv1               = lw_msgtab-msgv1
          msgv2               = lw_msgtab-msgv2
          msgv3               = lw_msgtab-msgv3
          msgv4               = lw_msgtab-msgv4
        IMPORTING
          message_text_output = gw_msg-msg.
      gw_msg-msgtyp = lw_msgtab-msgtyp.
      gw_msg-matnr = i_matnr-matnr.
      APPEND gw_msg TO gt_msg.
    ENDLOOP.
  ENDMETHOD.                    "fm_modify_info_record2


  METHOD fm_create_source_list.
    DATA: lw_bdcdata TYPE bdcdata,
          lt_bdcdata TYPE TABLE OF bdcdata,
          lw_msgtab TYPE bdcmsgcoll,
          lt_msgtab TYPE TABLE OF bdcmsgcoll.

    DATA: l_row(2) TYPE n,
          l_str TYPE string.


    l_row = i_count.
    "ME01  ����Ļ
    bdc_dynpro: 'SAPLMEOR' '0200'.
    bdc_field:  'BDC_OKCODE' '/00',
                'EORD-MATNR' i_matnr-matnr,
                'EORD-WERKS' con_werks.
    "ME01  ��һ��Ļ
    bdc_dynpro: 'SAPLMEOR' '0205'.
    bdc_field:  'BDC_OKCODE'  '=BU'.
    l_str = 'EORD-VDATU(' && l_row && ')'."�ϳ��ֶ�
    bdc_field: l_str '20150401'.
    l_str = 'EORD-BDATU(' && l_row && ')'."�ϳ��ֶ�
    bdc_field: l_str '99991231'.
    l_str = 'EORD-LIFNR(' && l_row && ')'."�ϳ��ֶ�
    bdc_field: l_str i_matnr-lifnr.
    l_str = 'EORD-EKORG(' && l_row && ')'."�ϳ��ֶ�
    bdc_field: l_str con_ekorg.

    REFRESH lt_msgtab.

    CALL TRANSACTION 'ME01' USING lt_bdcdata MODE 'E' MESSAGES INTO lt_msgtab.

    LOOP AT lt_msgtab INTO lw_msgtab.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = lw_msgtab-msgid
          msgnr               = lw_msgtab-msgnr
          msgv1               = lw_msgtab-msgv1
          msgv2               = lw_msgtab-msgv2
          msgv3               = lw_msgtab-msgv3
          msgv4               = lw_msgtab-msgv4
        IMPORTING
          message_text_output = gw_msg-msg.
      gw_msg-msgtyp = lw_msgtab-msgtyp.
      gw_msg-matnr = i_matnr-matnr.
      APPEND gw_msg TO gt_msg.
    ENDLOOP.
  ENDMETHOD.                    "FM_CREATE_SOURCE_LIST


  METHOD fm_create_purch_info.
    DATA: lw_marc TYPE st_marc,
          lt_marc TYPE TABLE OF st_marc,
          l_lifnr TYPE lfa1-lifnr.
    DATA: lt_sval TYPE TABLE OF sval,
          lw_sval TYPE sval,
          returncode.
    DEFINE add_sval.
      lw_sval-tabname = &1.
      lw_sval-fieldname = &2.
      append lw_sval to lt_sval.
    END-OF-DEFINITION.
*================================================================================
*COMMENT BY YI 21.10.2016 13:41:36
*�����û���������
*================================================================================
    add_sval:
    'MARC' 'PLIFZ',
    'MARC' 'WEBAZ',
    'MARC' 'EKGRP',
    'LFA1' 'LIFNR'.

    lw_sval-tabname = 'MARA'.
    lw_sval-fieldname = 'MATNR'.
    lw_sval-value = i_matnr-matnr.
    lw_sval-field_attr = '02'."һ����,�������벻����
    INSERT lw_sval INTO lt_sval INDEX 1.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = '��������'
      IMPORTING
        returncode      = returncode
      TABLES
        fields          = lt_sval
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
*================================================================================
*COMMENT BY YI 21.10.2016 13:42:03
*�ж����������
*================================================================================
    CHECK returncode NE 'A'.
    CHECK lt_sval IS NOT INITIAL.
    LOOP AT lt_sval INTO lw_sval.
      IF lw_sval-value IS INITIAL.
        MESSAGE 'ֵ����Ϊ��' TYPE 'E'.
      ENDIF.
      CASE lw_sval-fieldname.
        WHEN 'PLIFZ'.lw_marc-plifz = lw_sval-value.
        WHEN 'WEBAZ'.lw_marc-webaz = lw_sval-value.
        WHEN 'EKGRP'.lw_marc-ekgrp = lw_sval-value.
        WHEN 'LIFNR'.l_lifnr = lw_sval-value.
      ENDCASE.
    ENDLOOP.
    lw_marc-matnr = i_matnr-matnr.
    lw_marc-kordb = 'X'.
    CONCATENATE 'Z00' lw_marc-matnr+9(1) INTO lw_marc-mtart.
    lw_marc-werks = con_werks.
    IF lw_marc-ekgrp+0(1) <> 'A'."����������������,��ת���ɴ���
      SELECT SINGLE ekgrp INTO lw_marc-ekgrp FROM t024 WHERE eknam = lw_marc-ekgrp.
    ENDIF.
    IF l_lifnr+4(1) <> '1'."�������������Ĺ�Ӧ������,��ת���ɴ���
      SELECT SINGLE lifnr INTO l_lifnr FROM lfa1 WHERE sortl = l_lifnr.
    ENDIF.
    "���ɹ���͹�Ӧ���Ƿ����
    SELECT SINGLE ekgrp INTO lw_marc-ekgrp FROM t024 WHERE ekgrp = lw_marc-ekgrp.
    IF sy-subrc NE 0.
      MESSAGE '�ɹ��鲻����' TYPE 'E'.
    ENDIF.
    SELECT SINGLE lifnr INTO l_lifnr FROM lfa1 WHERE lifnr = l_lifnr.
    IF sy-subrc NE 0.
      MESSAGE '��Ӧ�̲�����' TYPE 'E'.
    ENDIF.
*================================================================================
*COMMENT BY YI 21.10.2016 13:42:12
*������������ݵ�ϵͳ
*================================================================================
    DATA: lw_head   TYPE bapimathead,
          lw_return TYPE bapiret2,
          lw_bapi_marc   TYPE bapi_marc,
          lw_bapi_marcx  TYPE bapi_marcx.

    lw_bapi_marc-plant = con_werks.
    lw_bapi_marc-pur_group = lw_marc-ekgrp.   "�ɹ���
    lw_bapi_marc-sourcelist = lw_marc-kordb. "Դ�嵥
    lw_bapi_marc-gr_pr_time = lw_marc-webaz.   "�ջ�����ʱ��
    lw_bapi_marc-plnd_delry = lw_marc-plifz.   "�ɹ�����

    lw_bapi_marcx-plant = con_werks.
    lw_bapi_marcx-pur_group =  'X'.   "�ɹ���
    lw_bapi_marcx-sourcelist = 'X'. "Դ�嵥
    lw_bapi_marcx-gr_pr_time = 'X'."�ջ�����ʱ��
    lw_bapi_marcx-plnd_delry = 'X'.   "�ɹ�����

    "��ͼ
    lw_head-material   = lw_marc-matnr. "���ϱ���
    lw_head-matl_type  = lw_marc-mtart. "��������
    lw_head-ind_sector = 'M'.
    lw_head-purchase_view = 'X'."�ɹ���ͼ
    lw_head-mrp_view = 'X'."MRP��ͼ'
    "��������
    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING
        headdata   = lw_head
        plantdata  = lw_bapi_marc
        plantdatax = lw_bapi_marcx
      IMPORTING
        return     = lw_return.

    IF sy-subrc = 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

    gw_msg-matnr = lw_marc-matnr.
    gw_msg-msgtyp = lw_return-type.
    gw_msg-msg = lw_return-message.
    APPEND gw_msg TO gt_msg.
*================================================================================
*COMMENT BY YI 21.10.2016 13:42:29
*�����ɹ���Ϣ��¼&��Դ�嵥
*================================================================================
    DATA: lcl_fix_inforecord_sourcelist TYPE REF TO cl_fix_inforecord_sourcelist.
    DATA: lt_matnr TYPE TABLE OF st_matnr.
    i_matnr-lifnr = l_lifnr.
    APPEND i_matnr TO lt_matnr.

    CREATE OBJECT lcl_fix_inforecord_sourcelist
      EXPORTING
        it_tab = lt_matnr.
  ENDMETHOD.                    "fm_create_purch_info


  METHOD fm_set_purch_info_undelete.
    DATA: lw_bdcdata TYPE bdcdata,
          lt_bdcdata TYPE TABLE OF bdcdata,
          lw_msgtab TYPE bdcmsgcoll,
          lt_msgtab TYPE TABLE OF bdcmsgcoll.

    "ME15����Ļ
    bdc_dynpro:'SAPMM06I' '0100'.
    bdc_field:'BDC_OKCODE' '/00',
    'EINA-LIFNR' i_matnr-lifnr,
    'EINA-MATNR' i_matnr-matnr,
    'EINE-EKORG' con_ekorg,
    'EINE-WERKS' con_werks.
    IF i_lohnb = 'X'.
      bdc_field:'RM06I-LOHNB' 'X'.
    ELSE.
      bdc_field:'RM06I-NORMB' 'X'.
    ENDIF.
    "ME15������Ļ
    bdc_dynpro:'SAPMM06I' '0104'.
    bdc_field:'BDC_OKCODE' '=BU'.
    bdc_field:'EINA-LOEKZ' ''.
    bdc_field:'EINE-LOEKZ' ''.

    REFRESH lt_msgtab.

    CALL TRANSACTION 'ME15' USING lt_bdcdata MODE 'E' MESSAGES INTO lt_msgtab.

    LOOP AT lt_msgtab INTO lw_msgtab.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = lw_msgtab-msgid
          msgnr               = lw_msgtab-msgnr
          msgv1               = lw_msgtab-msgv1
          msgv2               = lw_msgtab-msgv2
          msgv3               = lw_msgtab-msgv3
          msgv4               = lw_msgtab-msgv4
        IMPORTING
          message_text_output = gw_msg-msg.
      gw_msg-msgtyp = lw_msgtab-msgtyp.
      gw_msg-matnr = i_matnr-matnr.
      APPEND gw_msg TO gt_msg.
    ENDLOOP.
  ENDMETHOD.                    "fm_set_purch_info_undelete


  METHOD fm_set_purch_info_delete.
    DATA: lw_bdcdata TYPE bdcdata,
          lt_bdcdata TYPE TABLE OF bdcdata,
          lw_msgtab TYPE bdcmsgcoll,
          lt_msgtab TYPE TABLE OF bdcmsgcoll.

    "ME15����Ļ
    bdc_dynpro:'SAPMM06I' '0100'.
    bdc_field:'BDC_OKCODE' '/00',
    'EINA-LIFNR' i_matnr-lifnr,
    'EINA-MATNR' i_matnr-matnr,
    'EINE-EKORG' con_ekorg,
    'EINE-WERKS' con_werks.
    IF i_lohnb = 'X'.
      bdc_field:'RM06I-LOHNB' 'X'.
    ELSE.
      bdc_field:'RM06I-NORMB' 'X'.
    ENDIF.
    "ME15������Ļ
    bdc_dynpro:'SAPMM06I' '0104'.
    bdc_field:'BDC_OKCODE' '=BU'.
    bdc_field:'EINA-LOEKZ' 'X'.
    bdc_field:'EINE-LOEKZ' 'X'.

    REFRESH lt_msgtab.

    CALL TRANSACTION 'ME15' USING lt_bdcdata MODE 'E' MESSAGES INTO lt_msgtab.

    LOOP AT lt_msgtab INTO lw_msgtab.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = lw_msgtab-msgid
          msgnr               = lw_msgtab-msgnr
          msgv1               = lw_msgtab-msgv1
          msgv2               = lw_msgtab-msgv2
          msgv3               = lw_msgtab-msgv3
          msgv4               = lw_msgtab-msgv4
        IMPORTING
          message_text_output = gw_msg-msg.
      gw_msg-msgtyp = lw_msgtab-msgtyp.
      gw_msg-matnr = i_matnr-matnr.
      APPEND gw_msg TO gt_msg.
    ENDLOOP.
  ENDMETHOD.                    "fm_set_purch_info_delete
ENDCLASS.                    "cl_fix_source_list IMPLEMENTATION