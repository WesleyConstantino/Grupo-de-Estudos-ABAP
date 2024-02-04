*&---------------------------------------------------------------------*
*& Report ZR_DESAFIO_ROBSON_JULIANDO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zr_desafio_robson_juliando.

*&---------------------------------------------------------------------*
*                              Tables                                  *
*&---------------------------------------------------------------------*
TABLES: ztbvenda.

*&---------------------------------------------------------------------*
*                             Includes                                 *
*&---------------------------------------------------------------------*
INCLUDE <icon>. "Include da tabela de ícones

*&---------------------------------------------------------------------*
*                                 TYPES                                *
*&---------------------------------------------------------------------*
TYPES:
*------* ty_out
      BEGIN OF ty_out,
         cod_da_venda      TYPE ztbvenda-cod_da_venda,
         produto           TYPE ztbvenda-produto,
         nome_do_cliente   TYPE ztbcliente-nome_do_cliente,
         rg                TYPE ztbvenda-rg,
         cpf               TYPE ztbvenda-cpf,
         endereco          TYPE ztbcliente-endereco,
         email             TYPE ztbcliente-email,
         telefone          TYPE ztbcliente-telefone,
         valor_da_venda    TYPE ztbvenda-valor_da_venda,
       END OF ty_out,

*<---- 21/01/2024 - Estudos - Wesley Constantino - Início
*------* ty_arquivo
      BEGIN OF ty_arquivo,
         linha(2000) TYPE c,
      END   OF ty_arquivo,
*<---- 21/01/2024 - Estudos - Wesley Constantino - Fim

*<---- 30/01/2024 - Estudos - Wesley Constantino - Início
*------* ty_log_cli
       BEGIN OF ty_log_cli_out,
         nome_do_cliente   TYPE ztbcliente-nome_do_cliente,
         rg                TYPE ztbcliente-rg,
         cpf               TYPE ztbcliente-cpf,
         endereco          TYPE ztbcliente-endereco,
         email             TYPE ztbcliente-email,
         telefone          TYPE ztbcliente-telefone,
         mensagem          TYPE c LENGTH 100,
         color             TYPE char4,
         id                TYPE icon-id,
       END OF ty_log_cli_out.
*<---- 30/01/2024 - Estudos - Wesley Constantino - Fim

*&---------------------------------------------------------------------*
*                        Tabelas Internas                              *
*&---------------------------------------------------------------------*
DATA: it_out          TYPE TABLE OF ty_out,
      it_ztbcliente   TYPE TABLE OF ztbcliente,
      it_log_cli      TYPE TABLE OF ztbcliente,
      it_ztbvenda     TYPE TABLE OF ztbvenda,
*<---- 21/01/2024 - Estudos - Wesley Constantino - Início
      it_arquivo      TYPE TABLE OF ty_arquivo,
*<---- 21/01/2024 - Estudos - Wesley Constantino - Fim
*<---- 30/01/2024 - Estudos - Wesley Constantino - Início
       it_log_cli_out TYPE TABLE OF ty_log_cli_out.
*<---- 30/01/2024 - Estudos - Wesley Constantino - Fim

*&---------------------------------------------------------------------*
*                           Workareas                                  *
*&---------------------------------------------------------------------*
DATA: wa_ztbvenda    TYPE ztbvenda,
      wa_ztbcliente  TYPE ztbcliente,
      wa_out         LIKE LINE OF it_out,
*<---- 21/01/2024 - Estudos - Wesley Constantino - Início
      wa_arquivo     TYPE ty_arquivo,
*<---- 21/01/2024 - Estudos - Wesley Constantino - Fim
*<---- 30/01/2024 - Estudos - Wesley Constantino - Início
      wa_log_cli_out TYPE ty_log_cli_out.
*<---- 30/01/2024 - Estudos - Wesley Constantino - Fim

*&---------------------------------------------------------------------*
*                            Variáveis                                 *
*&---------------------------------------------------------------------*
DATA: lv_okcode_100 TYPE sy-ucomm,
*<---- 21/01/2024 - Estudos - Wesley Constantino - Início
*------* VARIÁVEIS PARA O UPLOAD:
      it_files TYPE filetable,
      wa_files TYPE file_table,
      gv_rc    TYPE i.
*<---- 21/01/2024 - Estudos - Wesley Constantino - Fim

*&---------------------------------------------------------------------*
*                       Declaração de Tipos                            *
*&---------------------------------------------------------------------*
TYPE-POOLS: slis. "Preciso declarar essa estrutura para fucionar o ALV


*&---------------------------------------------------------------------*
*                        Estruturas do ALV                             *
*&---------------------------------------------------------------------*
DATA: lo_container_100 TYPE REF TO cl_gui_custom_container,
      lo_grid_100      TYPE REF TO cl_gui_alv_grid,
*<---- 30/01/2024 - Estudos - Wesley Constantino - Início
      lo_container_101 TYPE REF TO cl_gui_custom_container,
      lo_grid_101      TYPE REF TO cl_gui_alv_grid,
*<---- 30/01/2024 - Estudos - Wesley Constantino - Fim
      lt_fieldcat      TYPE lvc_t_fcat,
      ls_layout        TYPE lvc_s_layo,
      ls_variant       TYPE disvariant.

*&---------------------------------------------------------------------*
*                         Tela de seleção                              *
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-000.
*Radio Buttons
PARAMETERS: rb_cli RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND comando.
PARAMETERS: rb_cven RADIOBUTTON GROUP g1.
PARAMETERS: rb_rven RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
*PARAMETERS
"Cadastrar cliente -> rb_cli
PARAMETERS: p_nome TYPE ztbcliente-nome_do_cliente MODIF ID rb1, "OBLIGATORY,  "Modifico o ID dos campos que quero eventualmente ocultar com "MODIF ID".
            p_rg TYPE ztbcliente-rg MODIF ID rb1, "OBLIGATORY,
            p_cpf TYPE ztbcliente-cpf MODIF ID rb1, " OBLIGATORY,
            p_end TYPE ztbcliente-endereco MODIF ID rb1,
            p_email TYPE ztbcliente-email MODIF ID rb1,
            p_tel TYPE ztbcliente-telefone MODIF ID rb1.

*<---- 21/01/2024 - Estudos - Wesley Constantino - Início
PARAMETERS: p_file(1024) TYPE c  MODIF ID upl. "OBLIGATORY. "Caminho do Arquivo para Upload

"Tipos de carga do cadastro de cliente
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rb_unic RADIOBUTTON GROUP gr2 DEFAULT 'X' MODIF ID rb4 USER-COMMAND comando.
SELECTION-SCREEN COMMENT 5(11) text-004 FOR FIELD rb_unic.

PARAMETERS: rb_massa  RADIOBUTTON GROUP gr2 MODIF ID rb4.
SELECTION-SCREEN COMMENT 21(35) text-005 FOR FIELD rb_massa.

SELECTION-SCREEN END OF LINE.
*<---- 21/01/2024 - Estudos - Wesley Constantino - Fim

"Cadastrar venda -> rb_cven
PARAMETERS: p_rg2 TYPE ztbvenda-rg MODIF ID rb2 , "OBLIGATORY,
            p_cpf2 TYPE ztbvenda-cpf MODIF ID rb2 , "OBLIGATORY,
            p_dat_vd TYPE ztbvenda-data_da_venda MODIF ID rb2 , "OBLIGATORY,
            p_prod TYPE ztbvenda-produto MODIF ID rb2 , "OBLIGATORY,
            p_valor TYPE ztbvenda-valor_da_venda MODIF ID rb2. "OBLIGATORY.

*SELECT-OPTIONS
"Relatório de vendas
SELECT-OPTIONS: s_cod_vd FOR ztbvenda-cod_da_venda MODIF ID rb3,
                s_rg     FOR ztbvenda-rg MODIF ID rb3  NO INTERVALS,
                s_cpf    FOR ztbvenda-cpf MODIF ID rb3 NO INTERVALS,
                s_dat_vd FOR ztbvenda-data_da_venda MODIF ID rb3,
                s_prod   FOR ztbvenda-produto MODIF ID rb3 NO INTERVALS,
                s_val_vd FOR ztbvenda-valor_da_venda MODIF ID rb3.
SELECTION-SCREEN END OF BLOCK b1.

*"Evento para reconhecer os ciques do radiobutton e mudar as telas
AT SELECTION-SCREEN OUTPUT.
  PERFORM f_modifica_tela.

"Início da execusão
START-OF-SELECTION.

*<---- 04/02/2024 - Estudos - Wesley Constantino - Início
*  IF rb_unic EQ 'X'.
  IF rb_unic EQ 'X' AND rb_rven  EQ ' '.
*<---- 04/02/2024 - Estudos - Wesley Constantino - Fim

    IF p_nome IS NOT INITIAL AND p_rg IS NOT INITIAL AND p_cpf IS NOT INITIAL.
     PERFORM f_cadastra_cliente.
    ELSE.
     MESSAGE s208(00) WITH 'Preencha os dados obrigatórios!' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.

    IF rb_cven EQ 'X' AND p_dat_vd IS NOT INITIAL AND p_rg2 IS NOT INITIAL AND p_cpf2 IS NOT INITIAL AND p_prod IS NOT INITIAL AND p_valor IS NOT INITIAL.
     PERFORM  f_cadastra_venda.
    ELSE.
*<---- 23/01/2024 - Estudos - Wesley Constantino - Início
     IF rb_cven EQ 'X'.
*<---- 23/01/2024 - Estudos - Wesley Constantino - Fim
     MESSAGE s208(00) WITH 'Preencha os dados obrigatórios!' DISPLAY LIKE 'E'.
*<---- 23/01/2024 - Estudos - Wesley Constantino - Início
     ENDIF.
*<---- 23/01/2024 - Estudos - Wesley Constantino - Fim
    ENDIF.
*<---- 21/01/2024 - Estudos - Wesley Constantino - Início
*  ELSEIF rb_rven  EQ 'X'.
  IF rb_rven  EQ 'X'.
*<---- 21/01/2024 - Estudos - Wesley Constantino - Fim
    PERFORM f_relatorio_de_vendas.
  ENDIF.

*<---- 04/02/2024 - Estudos - Wesley Constantino - Início
*Evento caminho de upload
  AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  IF rb_massa EQ 'X'.
   PERFORM: f_exibe_popup_caminho_upload,
            f_gui_upload,
            f_split_upload.
  ENDIF.
*<---- 04/02/2024 - Estudos - Wesley Constantino - Início

*&---------------------------------------------------------------------*
*&      Form  f_cadastra_cliente
*&---------------------------------------------------------------------*
 FORM f_cadastra_cliente.

 DATA: it_ztbcliente TYPE TABLE OF ztbcliente.

   SELECT nome_do_cliente,
          rg,
          cpf
     INTO TABLE @it_ztbcliente
     FROM ztbcliente
     WHERE nome_do_cliente EQ @p_nome AND
           rg EQ  @p_rg AND
           cpf EQ @p_cpf.

    IF it_ztbcliente IS NOT INITIAL.
      MESSAGE s208(00) WITH 'Cliente já cadastrado.' DISPLAY LIKE 'E'.
    ELSE.
      PERFORM f_update_cliente.
    ENDIF.

 ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f_update_cliente
*&---------------------------------------------------------------------*
 FORM f_update_cliente.
 DATA: wa_ztbcliente TYPE ztbcliente.

*<---- 23/01/2024 - Estudos - Wesley Constantino - Início
"Para cadastro único de cliente
     IF rb_unic EQ 'X'.
*<---- 23/01/2024 - Estudos - Wesley Constantino - Fim
      wa_ztbcliente-cpf = p_cpf.
      wa_ztbcliente-email = p_email.
      wa_ztbcliente-endereco = p_end.
      wa_ztbcliente-nome_do_cliente = p_nome.
      wa_ztbcliente-rg = p_rg.
      wa_ztbcliente-telefone = p_tel.

      INSERT ztbcliente FROM wa_ztbcliente.
*<---- 23/01/2024 - Estudos - Wesley Constantino - Início
     ENDIF.
"Para cadastro de cliente em massa
     IF rb_massa EQ 'X'.
      INSERT ztbcliente FROM TABLE it_ztbcliente.
     ENDIF.
*<---- 23/01/2024 - Estudos - Wesley Constantino - Fim

      IF sy-subrc IS INITIAL.
        COMMIT WORK AND WAIT. "COMMIT WORK AND WAIT dá commit no banco de dados

        MESSAGE s208(00) WITH 'CLIENTE(S) CADASTRADO(S) COM SUCESSO!'.
      ELSE.
        ROLLBACK WORK. "ROLLBACK WORK desfaz tudo o que aconteceu na operação
        MESSAGE s208(00) WITH 'ERRO AO GRAVAR!'DISPLAY LIKE 'E'.
      ENDIF.
 ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f_cadastra_venda
*&---------------------------------------------------------------------*
 FORM f_cadastra_venda.
  DATA: it_ztbvenda TYPE TABLE OF ztbvenda.

   SELECT rg,
          cod_da_venda,
          cpf
     INTO TABLE @it_ztbvenda
     FROM ztbvenda
     WHERE rg  EQ @p_rg2 AND
           cpf EQ @p_cpf2.

     IF sy-subrc EQ '0'.
       PERFORM f_pop_up_cadastra_venda.
     ELSE.
       PERFORM f_update_venda.
     ENDIF.

 ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f_pop_up_cadastra_venda
*&---------------------------------------------------------------------*
FORM f_pop_up_cadastra_venda.
  DATA: lv_resposta TYPE c. "variável que receberá o parâmetro de saída do clique. Sim(001) ou Não(002).

  CALL FUNCTION 'POPUP_TO_CONFIRM'
  EXPORTING
   titlebar                    = 'Informação'  "Título
   text_question               = 'Venda já cadastrada, deseja modificar?'   "Texto da pergunta do pupup
   text_button_1               = 'Sim'(001)  "Texto do botão 1
*   ICON_BUTTON_1               = ' '   "Ícone do botão 1
   text_button_2               = 'Não'(002) "Texto do botão 2
*   ICON_BUTTON_2               = ' '  "Ícone do botão 2
   display_cancel_button       = 'X'
 IMPORTING
   answer                      = lv_resposta.   "Parâmetro de saída

*Lógica para validar a escolha do usuário:
    IF lv_resposta EQ '1'.
      PERFORM f_modifica_venda.

    ELSEIF lv_resposta EQ '2'.
       "Instrução (Caso deixe aqui sem intrução, o popup fecha automaticamente após o clique e volta para a tela de seleção)
    ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f_modifica_venda
*&---------------------------------------------------------------------*
FORM f_modifica_venda.

wa_ztbvenda-rg             = p_rg2.
wa_ztbvenda-cpf            = p_cpf2.
wa_ztbvenda-data_da_venda  = p_dat_vd.
wa_ztbvenda-produto        = p_prod.
wa_ztbvenda-valor_da_venda = p_valor.

SELECT cod_da_venda
  FROM ztbvenda
  INTO @DATA(lv_cod)
  WHERE rg EQ @p_rg2.
ENDSELECT.

wa_ztbvenda-cod_da_venda = lv_cod.

MODIFY ztbvenda FROM wa_ztbvenda.

      IF sy-subrc EQ '0'.
        COMMIT WORK AND WAIT.
        MESSAGE s208(00) WITH 'MODIFICADO COM SUCESSO!'.
      ELSE.
        ROLLBACK WORK.
        MESSAGE s208(00) WITH 'ERRO AO MODIFICAR!'DISPLAY LIKE 'E'.
      ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f_update_venda
*&---------------------------------------------------------------------*
FORM f_update_venda.

PERFORM f_gera_cod_automatico.

wa_ztbvenda-rg             = p_rg2.
wa_ztbvenda-cpf            = p_cpf2.
wa_ztbvenda-data_da_venda  = p_dat_vd.
wa_ztbvenda-produto        = p_prod.
wa_ztbvenda-valor_da_venda = p_valor.

      INSERT ztbvenda FROM wa_ztbvenda.

      IF sy-subrc IS INITIAL.
        COMMIT WORK AND WAIT. "COMMIT WORK AND WAIT dá commit no banco de dados

        MESSAGE s208(00) WITH 'SALVO COM SUCESSO!'.
      ELSE.
        ROLLBACK WORK. "ROLLBACK WORK desfaz tudo o que aconteceu na operação
        MESSAGE s208(00) WITH 'ERRO AO GRAVAR!'DISPLAY LIKE 'E'.
      ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f_relatorio_de_vendas
*&---------------------------------------------------------------------*
 FORM f_gera_cod_automatico.

     SELECT MAX( cod_da_venda )  "Seleciona o maior valor existente no campo ID
     FROM ztbvenda
     INTO @DATA(lv_cod_da_venda).

 lv_cod_da_venda = lv_cod_da_venda + 1.
 wa_ztbvenda-cod_da_venda = lv_cod_da_venda.

 ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f_relatorio_de_vendas
*&---------------------------------------------------------------------*
 FORM f_relatorio_de_vendas.

    SELECT *
      FROM ztbvenda
      INTO TABLE it_ztbvenda
      WHERE cod_da_venda    IN s_cod_vd AND
            produto         IN s_prod   AND
            rg              IN s_rg     AND
            cpf             IN s_cpf    AND
            data_da_venda   IN s_dat_vd AND
            valor_da_venda  IN s_val_vd.

      IF sy-subrc IS INITIAL.
"Dump proposital para não sair dados nos campos de cliente no ALV:
*      SELECT nome_do_cliente,
*             endereco,
*             email,
*             telefone,
*             rg,
*             cpf
*         INTO TABLE @it_ztbcliente
*         FROM ztbcliente
*         WHERE rg  IN @s_rg AND
*               cpf IN @s_cpf.

      SELECT *
         INTO TABLE @it_ztbcliente
         FROM ztbcliente
         FOR ALL ENTRIES IN @it_ztbvenda
         WHERE rg  EQ @it_ztbvenda-rg AND
               cpf EQ @it_ztbvenda-cpf.

      IF sy-subrc IS INITIAL.
        PERFORM f_monta_it_out.
      ENDIF.
      ENDIF.

  MESSAGE 'Nenhum registro encontrado' TYPE 'S' DISPLAY LIKE 'W'.

 ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f_monta_it_out
*&---------------------------------------------------------------------*
FORM f_monta_it_out.

  LOOP AT it_ztbvenda INTO wa_ztbvenda.

     wa_out-cod_da_venda   = wa_ztbvenda-cod_da_venda.
     wa_out-produto        = wa_ztbvenda-produto.
     wa_out-rg             = wa_ztbvenda-rg.
     wa_out-cpf            = wa_ztbvenda-cpf.
     wa_out-valor_da_venda = wa_ztbvenda-valor_da_venda.

   READ TABLE it_ztbcliente INTO wa_ztbcliente WITH KEY rg  = wa_ztbvenda-rg
                                                        cpf = wa_ztbvenda-cpf.
   IF sy-subrc IS INITIAL.

   wa_out-nome_do_cliente = wa_ztbcliente-nome_do_cliente.
   wa_out-endereco        = wa_ztbcliente-endereco.
   wa_out-email           = wa_ztbcliente-email.
   wa_out-telefone        = wa_ztbcliente-telefone.

   ENDIF.
   APPEND wa_out TO it_out.
   CLEAR: wa_out,
          wa_ztbvenda,
          wa_ztbvenda.

  ENDLOOP.

  IF lines( it_out ) > 0.
    CALL SCREEN 100.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  M_SHOW_GRID_100  OUTPUT
*&---------------------------------------------------------------------*
MODULE m_show_grid_100 OUTPUT.
  FREE: lt_fieldcat[].

  ls_layout-cwidth_opt = 'X'. "Ajustar largura das colunas (Layout otimizado).
  ls_layout-zebra      = 'X'. "Layout em Zebra.
  ls_variant-report    = sy-repid. "Variante (Não usá-la quando o tipo foi pop-up).

  PERFORM f_build_fieldcat USING:
          'COD_DA_VENDA'     'COD_DA_VENDA'     'ZTBVENDA'   'Código da venda'  ' '   CHANGING lt_fieldcat[],
          'PRODUTO'          'PRODUTO'          'ZTBVENDA'   'Produto'          ' '   CHANGING lt_fieldcat[],
          'NOME_DO_CLIENTE'  'NOME_DO_CLIENTE'  'ZTBCLIENTE' 'Nome do cliente'  ' '   CHANGING lt_fieldcat[],
          'RG'               'RG'               'ZTBVENDA'   'RG'               ' '   CHANGING lt_fieldcat[],
          'CPF'              'CPF'              'ZTBVENDA'   'CPF'              ' '   CHANGING lt_fieldcat[],
          'ENDERECO'         'ENDERECO'         'ZTBCLIENTE' 'Endereço'         ' '   CHANGING lt_fieldcat[],
          'EMAIL'            'EMAIL'            'ZTBCLIENTE' 'Email'            ' '   CHANGING lt_fieldcat[],
          'TELEFONE'         'TELEFONE'         'ZTBCLIENTE' 'Telefone'         ' '   CHANGING lt_fieldcat[],
          'VALOR_DA_VENDA'   'VALOR_DA_VENDA'   'ZTBVENDA'   'Valor da venda'   ' '   CHANGING lt_fieldcat[].

  IF lo_grid_100 IS INITIAL.
    "Instância o objeto do ALV
    lo_grid_100 = NEW cl_gui_alv_grid( i_parent = cl_gui_custom_container=>default_screen ).

    "Chama o ALV pela primeira vez
    lo_grid_100->set_table_for_first_display(
    EXPORTING
      is_variant  = ls_variant
      is_layout   = ls_layout
      i_save      = 'A'
    CHANGING
      it_fieldcatalog = lt_fieldcat[]
      it_outtab       = it_out[]
    ).

    "Define título do ALV
    lo_grid_100->set_gridtitle( 'Relatório de vendas' ).
  ELSE.
    lo_grid_100->refresh_table_display( ).
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  zf_build_fieldcat
*&---------------------------------------------------------------------*
FORM f_build_fieldcat USING VALUE(p_fieldname) TYPE c
                             VALUE(p_field)     TYPE c
                             VALUE(p_table)     TYPE c
                             VALUE(p_coltext)   TYPE c
                             VALUE(p_icon)      TYPE c
                          CHANGING t_fieldcat   TYPE lvc_t_fcat.

  DATA: ls_fieldcat LIKE LINE OF t_fieldcat[].

  "Nome do campo dado na tabela interna
  ls_fieldcat-fieldname = p_fieldname.

  "Nome do campo na tabela transparente
  ls_fieldcat-ref_field = p_field.

  "Tabela transparente
  ls_fieldcat-ref_table = p_table.

  "Descrição que daremos para o campo no ALV.
  ls_fieldcat-coltext   = p_coltext.

  "Ìcones
  ls_fieldcat-icon = p_icon.

  APPEND ls_fieldcat TO t_fieldcat[].

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MODIFICA_TELA
*&---------------------------------------------------------------------*
FORM f_modifica_tela .
  LOOP AT SCREEN.
*rb_cli
    IF rb_cli EQ 'X'.
      IF screen-group1 EQ 'RB1'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
      ENDIF.

      IF screen-group1 EQ 'RB2'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.

      IF screen-group1 EQ 'RB3'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.
*<---- 21/01/2024 - Estudos - Wesley Constantino - Início
      IF screen-group1 EQ 'RB4'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
      ENDIF.
*<---- 21/01/2024 - Estudos - Wesley Constantino - Fim
    ENDIF.

*rb_cven
    IF rb_cven EQ 'X'.
      IF screen-group1 EQ 'RB2'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
      ENDIF.

      IF screen-group1 EQ 'RB1'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.

      IF screen-group1 EQ 'RB3'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.
*<---- 21/01/2024 - Estudos - Wesley Constantino - Início
      IF screen-group1 EQ 'RB4'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.

     IF screen-group1 EQ 'UPL'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.
*<---- 21/01/2024 - Estudos - Wesley Constantino - Fim
    ENDIF.

*rb_rven
    IF rb_rven EQ 'X'.
      IF screen-group1 EQ 'RB3'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
      ENDIF.

      IF screen-group1 EQ 'RB1'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.

      IF screen-group1 EQ 'RB2'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.
*<---- 21/01/2024 - Estudos - Wesley Constantino - Início
      IF screen-group1 EQ 'RB4'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.

      IF screen-group1 EQ 'UPL'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.
*<---- 21/01/2024 - Estudos - Wesley Constantino - Fim
    ENDIF.

*<---- 21/01/2024 - Estudos - Wesley Constantino - Início
**rb_massa
      IF rb_massa EQ 'X'.

      IF screen-group1 EQ 'UPL'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
      ENDIF.

     IF screen-group1 EQ 'RB1'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.

      ENDIF.

*rb_unic
      IF rb_unic EQ 'X'.

      IF screen-group1 EQ 'UPL'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.

      ENDIF.
*<---- 21/01/2024 - Estudos - Wesley Constantino - Fim

    MODIFY SCREEN. "Preciso dar um MODIFY SCREEN para que funcione.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE lv_okcode_100.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0. "Volta para a tela chamadora
    WHEN 'EXIT'.
      LEAVE PROGRAM. "Sai do programa
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS100'. "Botões da tela 100
  SET TITLEBAR 'TITULE100'.  "Código do título da Tela 100
ENDMODULE.

*<---- 23/01/2024 - Estudos - Wesley Constantino - Início
*&---------------------------------------------------------------------*
*& Form f_exibe_popup_caminho_upload
*&---------------------------------------------------------------------*
FORM f_exibe_popup_caminho_upload .

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    CHANGING
      file_table              = it_files
      rc                      = gv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE it_files INTO wa_files INDEX 1.
    p_file = wa_files-filename.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_gui_upload
*&---------------------------------------------------------------------*
FORM f_gui_upload .

  DATA ls_file  TYPE string.

  ls_file = p_file.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = ls_file
      filetype                = 'ASC'
    TABLES
      data_tab                = it_arquivo
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

  IF sy-subrc <> 0.
    MESSAGE 'Erro no upload!' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_split_upload
*&---------------------------------------------------------------------*
FORM f_split_upload .

  DATA: ls_cod_da_venda TYPE c,
        ls_valor_da_venda TYPE c.

  IF it_arquivo[] IS NOT INITIAL.

    LOOP AT it_arquivo INTO wa_arquivo.
*--* Início *--* Ignora a primeira linha do arquivo CSV do upload.
      IF sy-tabix EQ '1'.
        CONTINUE.
      ENDIF.
*--* Fim *--*
     IF rb_cli EQ 'X'.
      SPLIT wa_arquivo-linha AT ';' INTO
                                   wa_ztbcliente-nome_do_cliente
                                   wa_ztbcliente-rg
                                   wa_ztbcliente-cpf
                                   wa_ztbcliente-endereco
                                   wa_ztbcliente-email
                                   wa_ztbcliente-telefone.

      APPEND wa_ztbcliente TO it_ztbcliente.

    ENDIF. "rb_cli

    IF rb_cven EQ 'X'.
      "Codar o split do cadastro de venda aqui
    ENDIF. "rb_cven

      CLEAR  wa_out.
    ENDLOOP.

      IF it_ztbcliente IS NOT INITIAL AND rb_cli EQ 'X'.
        PERFORM f_cad_cliente_massa.
      ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_cad_cliente_massa
*&---------------------------------------------------------------------*
FORM f_cad_cliente_massa .

"Verifica se existem clientes iguais na tabela transparente e no arquivo do upload
 SELECT *
   FROM ztbcliente
   INTO TABLE @it_log_cli
   FOR ALL ENTRIES IN @it_ztbcliente
   WHERE rg  EQ @it_ztbcliente-rg AND
         cpf EQ @it_ztbcliente-cpf.

   IF it_log_cli IS INITIAL.
    "Se não teverem clientes do arquivo de upload já cadastrados na tabela transparente.
    PERFORM f_update_cliente.
   ELSE.
    "Se teverem clientes do arquivo de upload já cadastrados na tabela transparente.
   LOOP AT it_log_cli INTO DATA(ls_log_cli).
   DELETE it_ztbcliente WHERE rg = ls_log_cli-rg AND
                              cpf = ls_log_cli-cpf.
   ENDLOOP.

   IF it_ztbcliente IS NOT INITIAL.
     PERFORM f_update_cliente.
   ELSE.
    MESSAGE 'Todos os clientes já estão cadastrados!' TYPE 'S' DISPLAY LIKE 'W'.
   ENDIF.
   ENDIF.

*<---- 30/01/2024 - Estudos - Wesley Constantino - Início
   "Log do status de todos os status dos clientes cadastrados e não cadastrados
   IF sy-subrc EQ 0.
    PERFORM f_popula_it_log_cli_out.
   ENDIF.
*<---- 30/01/2024 - Estudos - Wesley Constantino - Fim

ENDFORM.
*<---- 23/01/2024 - Estudos - Wesley Constantino - Fim

*<---- 30/01/2024 - Estudos - Wesley Constantino - Início
*&---------------------------------------------------------------------*
*& Form f_popula_it_log_cli_out
*&---------------------------------------------------------------------*
FORM f_popula_it_log_cli_out .

  LOOP AT it_log_cli INTO DATA(ls_log_cli).
    wa_log_cli_out-nome_do_cliente = ls_log_cli-nome_do_cliente.
    wa_log_cli_out-rg              = ls_log_cli-rg             .
    wa_log_cli_out-cpf             = ls_log_cli-cpf            .
    wa_log_cli_out-endereco        = ls_log_cli-endereco       .
    wa_log_cli_out-email           = ls_log_cli-email          .
    wa_log_cli_out-telefone        = ls_log_cli-telefone       .
    wa_log_cli_out-mensagem        = 'Cliente já cadastrado anteriormente!'.
    wa_log_cli_out-color           = 'C300'. "Código da cor amarela
    wa_log_cli_out-id              = icon_yellow_light. "Ícone amarelo

    APPEND wa_log_cli_out TO it_log_cli_out.
    CLEAR:  wa_log_cli_out,
            ls_log_cli.
  ENDLOOP.

CLEAR wa_ztbcliente.
  LOOP AT it_ztbcliente INTO wa_ztbcliente.
    wa_log_cli_out-nome_do_cliente = wa_ztbcliente-nome_do_cliente.
    wa_log_cli_out-rg              = wa_ztbcliente-rg             .
    wa_log_cli_out-cpf             = wa_ztbcliente-cpf            .
    wa_log_cli_out-endereco        = wa_ztbcliente-endereco       .
    wa_log_cli_out-email           = wa_ztbcliente-email          .
    wa_log_cli_out-telefone        = wa_ztbcliente-telefone       .
    wa_log_cli_out-mensagem        = 'Cliente cadastrado com sucesso!'.
    wa_log_cli_out-color           = 'C500'. "Código da cor verde
    wa_log_cli_out-id              = icon_green_light. "Ícone verde

  APPEND wa_log_cli_out TO it_log_cli_out.
  CLEAR:  wa_log_cli_out,
          wa_ztbcliente.
  ENDLOOP.

  IF lines( it_log_cli_out ) > 0.
    CALL SCREEN 101.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_0101 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
  SET PF-STATUS 'STATUS101'. "Botões da tela 101
  SET TITLEBAR 'TITULE101'.  "Código do título da Tela 101
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0101 INPUT.

  CASE lv_okcode_100.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0. "Volta para a tela chamadora
    WHEN 'EXIT'.
      LEAVE PROGRAM. "Sai do programa
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  M_SHOW_GRID_100  OUTPUT
*&---------------------------------------------------------------------*
MODULE m_show_grid_101 OUTPUT.
  FREE: lt_fieldcat[].

  ls_layout-cwidth_opt = 'X'. "Ajustar largura das colunas (Layout otimizado).
  ls_layout-zebra      = 'X'. "Layout em Zebra.
  ls_variant-report    = sy-repid. "Variante (Não usá-la quando o tipo foi pop-up).
  ls_layout-info_fname = 'COLOR'. "Cor das linhas

  PERFORM f_build_fieldcat USING:
          'ID'               'ID'               'ICON'          'Status'           'X'  CHANGING lt_fieldcat[],
          'NOME_DO_CLIENTE'  'NOME_DO_CLIENTE'  'ZTBCLIENTE'    'Nome do cliente'  ' '  CHANGING lt_fieldcat[],
          'RG'               'RG'               'ZTBCLIENTE'    'RG'               ' '  CHANGING lt_fieldcat[],
          'CPF'              'CPF'              'ZTBCLIENTE'    'CPF'              ' '  CHANGING lt_fieldcat[],
          'ENDERECO'         'ENDERECO'         'ZTBCLIENTE'    'Endereço'         ' '  CHANGING lt_fieldcat[],
          'EMAIL'            'EMAIL'            'ZTBCLIENTE'    'Email'            ' '  CHANGING lt_fieldcat[],
          'TELEFONE'         'TELEFONE'         'ZTBCLIENTE'    'Telefone'         ' '  CHANGING lt_fieldcat[],
          'MENSAGEM'         'MENSAGEM'         'IT_ZTBCLIENTE' 'Mensagem'         ' '  CHANGING lt_fieldcat[].

  IF lo_grid_101 IS INITIAL.
    "Instância o objeto do ALV
    lo_grid_101 = NEW cl_gui_alv_grid( i_parent = cl_gui_custom_container=>default_screen ).

    "Chama o ALV pela primeira vez
    lo_grid_101->set_table_for_first_display(
    EXPORTING
      is_variant  = ls_variant
      is_layout   = ls_layout
      i_save      = 'A'
    CHANGING
      it_fieldcatalog = lt_fieldcat[]
      it_outtab       = it_log_cli_out[]
    ).

    "Define título do ALV
    lo_grid_101->set_gridtitle( 'Log de mensagem dos registros de clientes' ).
  ELSE.
    lo_grid_101->refresh_table_display( ).
  ENDIF.

ENDMODULE.
*<---- 30/01/2024 - Estudos - Wesley Constantino - Fim
