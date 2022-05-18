*&---------------------------------------------------------------------*
*& Include          ZECL_001_I03
*&---------------------------------------------------------------------*
CLASS lcl_mail DEFINITION.

  PUBLIC SECTION.

    DATA: mt_req TYPE TABLE OF zecl_req,
          ms_req LIKE LINE OF mt_req.

    DATA: mt_appr TYPE TABLE OF zecl_appr,
          ms_appr LIKE LINE OF mt_appr.

    DATA lv_a TYPE char3.


*    types : begin of ty_list. " ALV
*
*    types:     icon(6) type c,
*               message type zfi_t196-message.
*
*            .include type bkpf.
*
*    types :  end of ty_list.

    METHODS :
*      progress_indicator importing iv_text type any      ,
*      set_first_status                                   ,
      start_program                                      ,
*      check_fields                                       ,
      get_data RETURNING VALUE(rv_subrc) TYPE sy-subrc,
*      export_to_sql                                      ,
*      prepare_data                                       ,
*      insert_bkpf  importing is_bkpf type bkpf           ,
*      insert_bseg  importing is_bkpf type bkpf           ,
*      fill_log  importing is_bkpf type bkpf              ,
      prepare_alv                                        .
*      check_statu                                        ,
*      counter returning value(iv_count) type i           .

  PROTECTED SECTION.

    DATA : mo_alv     TYPE REF TO cl_salv_table,
           mo_detail  TYPE REF TO cl_salv_table,
*           mo_functions type ref to cl_salv_functions_list,
           mo_display TYPE REF TO cl_salv_display_settings,
           mo_columns   type ref to cl_salv_columns_table,
           mo_column    type ref to cl_salv_column_table,
*           mo_selection type ref to cl_salv_selections,
*           mo_layout    type ref to cl_salv_layout,
           mo_events  TYPE REF TO cl_salv_events_table.
*           mo_sorts     type ref to cl_salv_sorts,
*           mo_agg       type ref to cl_salv_aggregations,
*           mo_exp_msg   type ref to cx_salv_msg.

*    data : ms_key   type salv_s_layout_key,
*           ms_color type lvc_s_colo.

  PRIVATE SECTION.

    METHODS :
      create_alv,
      display_alv,
      set_pf_status,
*      set_top_of_page,
      set_alv_properties,
      set_column_text
        importing iv_fname type lvc_fname
                  iv_text  type any.
*      set_sort
*        importing value(iv_col)  type lvc_fname
*                  value(iv_seq)  type salv_de_sort_sequence
*                  value(iv_subt) type sap_bool.

    METHODS :
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,

      on_link_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row
                  column.

*      on_after_user_command for event after_salv_function of cl_salv_events
*        importing e_salv_function.

ENDCLASS.


CLASS lcl_mail IMPLEMENTATION.

*  method progress_indicator.
*    call function 'SAPGUI_PROGRESS_INDICATOR'
*      exporting
*        percentage = 75
*        text       = iv_text.
*  endmethod.


*  method set_first_status.
*    concatenate icon_xls 'Excel Template'
*       into p_button
*  separated by space.
*  endmethod.

*  method check_fields.
*    if s_bukrs is initial and s_belnr is initial
*                          and s_gjahr is initial
*                          and s_xrf2hd is initial.
*      message text-m01 type 'S' display like 'E'.
*      leave list-processing.
*    endif.
*  endmethod.

  METHOD get_data.

    SELECT * FROM zecl_req INTO CORRESPONDING FIELDS OF TABLE @mt_req.
*ZECL_REQ~request_no
*  ZECL_REQ~mail
*  ZECL_REQ~remark
*  ZECL_REQ~approval_status
*  ZECL_REQ~approval_log
*  ZECL_REQ~user_name

    SELECT * FROM zecl_appr INTO CORRESPONDING FIELDS OF TABLE mt_appr.

  ENDMETHOD.

  METHOD start_program.

    DATA lv_subrc TYPE sy-subrc.

    lv_subrc = me->get_data( ).

    IF lv_subrc <> 0.
*      MESSAGE 'No data' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ELSE.
      me->prepare_alv( ).
    ENDIF.

  ENDMETHOD.

*  method prepare_data.
*
*    loop at mt_bkpf into data(ls_bkpf).
*      select single * from zbc_t007 into @data(ls_log)
*                             where bukrs =  @ls_bkpf-bukrs
*                               and  belnr = @ls_bkpf-belnr
*                               and  gjahr = @ls_bkpf-gjahr.
*
*      if sy-subrc = 0.
*        "Referans no kontrolü yapacak log tablosunda varsa gönderim yapmayacak.
*
*      else.
*
*        me->insert_bkpf( ls_bkpf ).
*        me->insert_bseg( ls_bkpf ).
*        me->fill_log( ls_bkpf ).
*
*      endif.
*
*    endloop.
*
*  endmethod.

*  method fill_log.
*
*    data lt_t007 type standard table of zbc_t007.
*    data ls_t007 like line of lt_t007.
*    data ls_bkpf type bkpf.
*
*    ls_bkpf = is_bkpf.
*
*    ls_t007-bukrs = ls_bkpf-bukrs.
*    ls_t007-belnr = ls_bkpf-belnr.
*    ls_t007-gjahr = ls_bkpf-gjahr.
*    ls_t007-datum = sy-datum.
*    ls_t007-uzeit = sy-uzeit.
*    ls_t007-usnam = sy-uname.
*    modify zbc_t007 from ls_t007.
*    clear: zbc_t007, ls_t007.
*
*  endmethod.

*  method check_statu.
*
*    data ls_list like line of mt_list.
*
*    loop at mt_list into ls_list.
*
*      select single * from zbc_t007
*                       into @data(ls_t007)
*                       where bukrs = @ls_list-bukrs
*                         and belnr = @ls_list-belnr
*                         and gjahr = @ls_list-gjahr
*                         and datum = ( select max( datum ) from zbc_t007  where bukrs = @ls_list-bukrs
*                         and belnr = @ls_list-belnr
*                         and gjahr = @ls_list-gjahr )
*                         and uzeit = ( select max( uzeit ) from zbc_t007  where bukrs = @ls_list-bukrs
*                         and belnr = @ls_list-belnr
*                         and gjahr = @ls_list-gjahr ).
*
*      if sy-subrc = 0. "Successful
*        ls_list-icon = '@5B@'.
*      endif.
*
*      modify mt_list from ls_list.
*      clear ls_t007.
*    endloop.
*sort mt_list descending by icon.
*
*  endmethod.

  METHOD prepare_alv.
    me->create_alv( ).
    me->set_pf_status( ).
    me->set_alv_properties( ).
*    me->set_top_of_page( ).
    me->display_alv( ).
  ENDMETHOD.                    "prepare_alv

  METHOD create_alv.

    cl_salv_table=>factory(
*        EXPORTING
*          list_display   = if_salv_c_bool_sap=>false " ALV Displayed in List Mode
*          r_container    =                           " Abstract Container for GUI Controls
*          container_name =
      IMPORTING
        r_salv_table   = mo_alv                          " Basis Class Simple ALV Tables
      CHANGING
        t_table        = mt_req
    ).
*      CATCH cx_salv_msg. " ALV: General Error Class with Message

  ENDMETHOD.                    "create_alv

  METHOD display_alv.

    mo_alv->display( ).

  ENDMETHOD.                    "display_alv

  METHOD set_pf_status.
    mo_alv->set_screen_status(
      EXPORTING
        pfstatus      = 'ZGUI'                  " Screens, Current GUI Status
        report        = sy-repid                 " ABAP Program: Current Master Program
        set_functions = mo_alv->c_functions_all " ALV: Data Element for Constants
    ).
  ENDMETHOD.                    "set_pf_status

*  method set_top_of_page.
*
*    data : lo_grid_top    type ref to cl_salv_form_layout_grid,
*           lo_text        type ref to cl_salv_form_text,
*           lo_label       type ref to cl_salv_form_label,
*           lo_logo        type ref to cl_salv_form_layout_logo,
*           lv_count       type i,
*           lv_count2      type i,
*           lv_count3      type i,
*           lv_date        type sy-datum,
*           lo_grid_bottom type ref to cl_salv_form_layout_grid.

*    create object lo_grid_top.
*
*    lo_grid_top->create_header_information(
*                  row     = 1
*                  column  = 1
*                  text    = text-t01
*                  tooltip = text-t01 ).
*
*    lo_grid_top->add_row( ).
*
*    lv_count = me->counter( ).
*
*    lo_grid_bottom = lo_grid_top->create_grid(
*                   row    = 3
*                   column = 1 ).
*
*    lo_label = lo_grid_bottom->create_label(
*                   row     = 1
*                   column  = 1
*                   text    = text-t02
*                   tooltip = text-t02 ).
*
**    lv_date = |{ sy-datum date = environment }|.
**    WRITE sy-datum to lv_date.
*    lo_text = lo_grid_bottom->create_text(
*                   row     = 1
*                   column  = 2
*                   text    = lv_count
*                   tooltip = lv_count ).
*
*    lo_label = lo_grid_bottom->create_label(
*               row     = 2
*               column  = 1
*               text    = text-t03
*               tooltip = text-t03 ).
*
*    describe table mt_bseg lines lv_count2.
*
**    lv_date = |{ sy-datum date = environment }|.
**    WRITE sy-datum to lv_date.
*    lo_text = lo_grid_bottom->create_text(
*                   row     = 2
*                   column  = 2
*                   text    = lv_count2
*                   tooltip = lv_count2 ).
*
*    lo_label = lo_grid_bottom->create_label(
*               row     = 3
*               column  = 1
*               text    = text-t04
*               tooltip = text-t04 ).
*
*    lv_count3 = lv_count + lv_count2.
*
**    lv_date = |{ sy-datum date = environment }|.
**    WRITE sy-datum to lv_date.
*    lo_text = lo_grid_bottom->create_text(
*                   row     = 3
*                   column  = 2
*                   text    = lv_count3
*                   tooltip = lv_count3 ).
*
*    clear : lv_count, lv_count2, lv_count3.
*
*    lo_label->set_label_for( lo_text ).
*
*    create object lo_logo.
**    lo_logo->set_right_logo( 'BL_LOGO' ).
*    lo_logo->set_left_content( lo_grid_top ).
*
*    mo_alv->set_top_of_list( lo_logo ).
*
*  endmethod.                    "set_top_of_page

  METHOD set_alv_properties.

    mo_display = mo_alv->get_display_settings( ).

* Zebra sytle..
*    mo_display->set_striped_pattern( cl_salv_display_settings=>true ).
*
    mo_columns = mo_alv->get_columns( ).
* Set optimize..
*    mo_columns->set_optimize( abap_true ).
*
*    mo_layout = mo_alv->get_layout( ).
* Set variant..
*    ms_key-report = sy-repid.
*    mo_layout->set_key( ms_key ).
*    mo_layout->set_save_restriction( cl_salv_layout=>restrict_none ).
*    mo_layout->set_default( abap_true ).

* Set selection..
*    mo_selection = mo_alv->get_selections( ).
*    mo_selection->set_selection_mode( if_salv_c_selection_mode=>cell ).
*

    mo_events = mo_alv->get_event( ).
* Set ALV Events.
    SET HANDLER go_mail->on_user_command FOR mo_events.
    SET HANDLER go_mail->on_link_click FOR mo_events.

* Set Column Text..
    me->set_column_text( iv_fname = 'MANDT' iv_text = text-h01 ).
    me->set_column_text( iv_fname = 'REQUEST_NO' iv_text = text-h02 ).
    me->set_column_text( iv_fname = 'MAIL' iv_text = text-h03 ).
    me->set_column_text( iv_fname = 'REMARK' iv_text = text-h04 ).
    me->set_column_text( iv_fname = 'APPROVAL_STATUS' iv_text = text-h05 ).
    me->set_column_text( iv_fname = 'APPROVAL_LOG' iv_text = text-h06 ).

** Set Total.
*    mo_agg = mo_alv->get_aggregations( ).
*    TRY.
*        mo_agg->add_aggregation(
*                   columnname  = 'NETWR'
*                   aggregation = if_salv_c_aggregation=>total ).
*      CATCH cx_salv_data_error.
*      CATCH cx_salv_not_found.
*      CATCH cx_salv_existing.
*    ENDTRY.
*
*    mo_agg = mo_alv->get_aggregations( ).
*    TRY.
*        mo_agg->add_aggregation(
*                   columnname  = 'NETTR'
*                   aggregation = if_salv_c_aggregation=>total ).
*      CATCH cx_salv_data_error.
*      CATCH cx_salv_not_found.
*      CATCH cx_salv_existing.
*    ENDTRY.
*
*    mo_agg = mo_alv->get_aggregations( ).
*    TRY.
*        mo_agg->add_aggregation(
*                   columnname  = 'KSL'
*                   aggregation = if_salv_c_aggregation=>total ).
*      CATCH cx_salv_data_error.
*      CATCH cx_salv_not_found.
*      CATCH cx_salv_existing.
*    ENDTRY.


* Sort.
*    mo_sorts = mo_alv->get_sorts( ).
*      me->set_sort( iv_col = 'ICON' iv_seq = '1' iv_subt = abap_true ).
*      me->set_sort( iv_col = 'MESSAGE' iv_seq = '2' iv_subt = abap_true ).


* Hotspot.
*    try.
*        mo_column ?= mo_columns->get_column( 'MESSAGE' ).
*        mo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
*      catch cx_salv_not_found .
*    endtry.

*  Hide columns.
*    try.
*        if p_perio is initial.
*          mo_column ?= mo_columns->get_column( 'PERIO' ).
*          mo_column->set_technical( if_salv_c_bool_sap=>true ).
*        else.
*          mo_column ?= mo_columns->get_column( 'TPERIO' ).
*          mo_column->set_technical( if_salv_c_bool_sap=>true ).
*        endif.
*        mo_column ?= mo_columns->get_column( 'MTYPE' ).
*        mo_column->set_technical( if_salv_c_bool_sap=>true ).
*      catch cx_salv_not_found.
*    endtry.

* Icon
*    try.
*        mo_column ?= mo_columns->get_column( 'ICON' ).
*        mo_column->set_icon( 'X' ).
*      catch cx_salv_not_found .
*    endtry.
* Column Color.
*    TRY.
*        ms_color-col = '7'.
*        ms_color-int = '0'.
*        ms_color-inv = '0'.
*        mo_column ?= mo_columns->get_column( 'COMP_CODE' ).
*        mo_column->set_color( ms_color ).
*      CATCH cx_salv_not_found .
*    ENDTRY.

* Key.
*    TRY.
*        go_column ?= go_columns->get_column( 'TARIH' ).
*        go_column->set_key( mc_x ).
*      CATCH cx_salv_not_found .
*    ENDTRY.


  ENDMETHOD.                    "set_alv_properties

  method set_column_text.
    data : lv_textl type scrtext_l,
           lv_textm type scrtext_m,
           lv_texts type scrtext_s.

    lv_texts = lv_textm = lv_textl = iv_text.
*
    try.
        mo_column ?= mo_columns->get_column( iv_fname ).
        mo_column->set_long_text( lv_textl ).
        mo_column->set_medium_text( lv_textm ).
        mo_column->set_short_text( lv_texts ).
      catch cx_salv_not_found .
    endtry.
*    mo_columns = mo_alv->get_columns( ).
*    mo_column = mo_columns->get_column( columnname = 'APPROVAL_LOG' ).
*    mo_column->set_short_text( value = 'E-mail' ).
*    mo_column->set_medium_text( value = 'E-mail Addr' ).
*    mo_column->set_long_text( value = 'E-mail Address' ).
*                CATCH cx_salv_not_found. " ALV: General Error Class (Checked in Syntax Check)

  endmethod.                    "set_column_text

*  method set_sort.
*    try.
*        mo_sorts->add_sort( columnname = iv_col
*                            sequence   = iv_seq
*                            subtotal   = iv_subt ).
*      catch cx_salv_not_found .
*      catch cx_salv_existing .
*      catch cx_salv_data_error .
*    endtry.
*  endmethod.                    "set_sort

  METHOD on_user_command.
*    data :ps_selfield type slis_selfield.

    DATA(lo_selection) = mo_alv->get_selections( ).
*   data lt_rows type salv_t_row.
    data(lt_rows) = lo_selection->get_selected_rows( ).
*   data(lv_row) = row.
    DATA lv_row type i.
*   data ls_rows like line of lt_rows
*    loop at lt_rows into lv_row.
*    endloop.
    read table lt_rows into lv_row index 1.

    CASE e_salv_function.
      WHEN '&APPR'.

          READ TABLE mt_req INTO ms_req INDEX lv_row.
          lv_a = strlen( ms_req-approval_log ).
          READ TABLE mt_appr INTO ms_appr INDEX lv_a + 1.
*        IF ms_req-approval_status EQ '02'.
*          MESSAGE 'This email has already been approved.' TYPE 'S' DISPLAY LIKE 'I'.
*        ENDIF.
          if lv_a ge 3.
            message 'This email has already been approved.' type 'E' display like 'I'.
          endif.
          IF ms_appr-user_name EQ sy-uname.
            ms_req-approval_log = ms_req-approval_log && 'X'.
            "insert zecl_req from gs_req.
            MODIFY zecl_req FROM ms_req.
            COMMIT WORK.
            MESSAGE 'The mail has been approved.' TYPE 'I' DISPLAY LIKE 'I'.
            IF ms_appr-appr_per_no EQ 0.
              ms_req-approval_status = '02'.
              UPDATE zecl_req FROM ms_req.
              COMMIT WORK AND WAIT.
              MESSAGE 'The email has been definitively approved.' TYPE 'I' DISPLAY LIKE 'I'.
            ENDIF.

          ELSE.
            MESSAGE 'You are not authorized to approve email' TYPE 'E' DISPLAY LIKE 'I'.
          ENDIF.

        WHEN '&RJC'.
          READ TABLE mt_req INTO ms_req INDEX lv_row.
          lv_a = strlen( ms_req-approval_log ).
          READ TABLE mt_appr INTO ms_appr INDEX lv_a + 1 .
          if ms_req-approval_status ne 01.
            message 'This email has already been rejected.' type 'E' display like 'I'.
          endif.
          IF ms_appr-user_name EQ sy-uname.
*        gs_req-approval_status  = '03'.
            UPDATE zecl_req SET approval_status = '03' WHERE request_no EQ ms_req-request_no .
            COMMIT WORK.
            MESSAGE 'E-mail request rejected' TYPE 'I' DISPLAY LIKE 'I'.
          ELSEIF ms_appr-user_name NE sy-uname.
            MESSAGE 'You are not authorized to reject email' TYPE 'E' DISPLAY LIKE 'I'.
          ELSE.
            MESSAGE 'You are not authorized to reject email' TYPE 'E' DISPLAY LIKE 'I'.
          ENDIF.

        WHEN '&BCK'.
          LEAVE TO SCREEN 0.

      ENDCASE.

    ENDMETHOD.                    "on_user_command

    METHOD on_link_click.
*
*    data ls_list like line of mt_list.
*
*    data lr_columns type ref to cl_salv_columns_table.
      DATA lr_events    TYPE REF TO cl_salv_events_table.
*    data lr_column    type ref to cl_salv_column_table.
*
*
*    case column.
*      when 'MESSAGE'.
*
*        data(lv_row) = row.
*
*        read table mt_list index lv_row into ls_list.
*
*        check sy-subrc = 0.
*
*        select * from zbc_t007 into table mt_log
*                                    where bukrs = ls_list-bukrs
*                                      and belnr = ls_list-belnr
*                                      and gjahr = ls_list-gjahr.
*
*        sort mt_log descending by datum uzeit.
*
*        try.
*
**            mo_columns = mo_alv->get_columns( ).
*** Set optimize..
**            mo_columns->set_optimize( abap_true ).
*
*            cl_salv_table=>factory(
*            importing
*              r_salv_table = mo_detail
*              changing
*                t_table =  mt_log ).
*
*          catch cx_salv_msg.
*
*        endtry.
*
*        lr_columns = mo_detail->get_columns( ).
** Set optimize..
*        lr_columns->set_optimize( abap_true ).
*
      lr_events = mo_detail->get_event( ).
      SET HANDLER go_mail->on_link_click FOR lr_events.
*
** Hotspot.
*        try.
*            lr_column ?= lr_columns->get_column( 'BELNR' ).
*            lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
*          catch cx_salv_not_found .
*        endtry.
*
*
*        mo_detail->set_screen_popup( start_column = 1
*        end_column = 120
*        start_line = 1
*        end_line = 20 ).
*
*        mo_detail->display( ).
*
**Log tablosu hotspot özelliği için..
*      when 'BELNR'.
*
*        read table mt_log index row into data(ls_log).
*
*        if ls_log-belnr is not initial.
*
*          set parameter id 'BLN' field ls_log-belnr.
*
*          set parameter id 'BUK' field ls_log-bukrs.
*
*          set parameter id 'GJR' field ls_log-gjahr.
*
*          call transaction 'FB03' and skip first screen.
*
*        endif.
*
*        clear ls_log.
*
*    endcase.
*
    ENDMETHOD.                    "on_link_click


*  method on_after_user_command.
*
*    case e_salv_function.
*      when 'CREATE'.
*        data(lo_selection) = mo_alv->get_selections( ).
*        data(lt_rows) = lo_selection->get_selected_rows( ).
*
**        me->asset_batch( lt_rows ).
*
*        mo_alv->refresh( ).
*      when others.
*    endcase.
*
*  endmethod.                    "on_user_command

*  method counter.
*
*    describe table mt_list lines iv_count.
*
*  endmethod.

ENDCLASS.
