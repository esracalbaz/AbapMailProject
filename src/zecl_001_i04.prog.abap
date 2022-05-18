*&---------------------------------------------------------------------*
*& Include          ZECL_001_I04
*&---------------------------------------------------------------------*
initialization.
  create object go_mail.

*at selection-screen output.
*  go_report->set_first_status( ).

*at selection-screen on value-request for p_filen.
*  go_report->get_fname( ).

at selection-screen.

*  go_report->excel_temp( ).

start-of-selection.
*  go_mail->check_fields( ).
  go_mail->start_program( ).

end-of-selection.
*  go_report->prepare_alv( ).
*go_report->show_information( ).
