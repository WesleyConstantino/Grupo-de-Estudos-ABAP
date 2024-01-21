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
*                                 TYPES                                *
*&---------------------------------------------------------------------*
TYPES:
*------* ty_OUT
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
       END OF ty_out.

*&---------------------------------------------------------------------*
*                        Tabelas Internas                              *
*&---------------------------------------------------------------------*
DATA: it_out        TYPE TABLE OF ty_out,
      it_ztbcliente TYPE TABLE OF ztbcliente,
      it_ztbvenda   TYPE TABLE OF ztbvenda.

*&---------------------------------------------------------------------*
*                           Workareas                                  *
*&---------------------------------------------------------------------*
DATA: wa_ztbvenda   TYPE ztbvenda,
      wa_ztbcliente TYPE ztbcliente,
      wa_out        LIKE LINE OF it_out.

*&---------------------------------------------------------------------*
*                            Variáveis                                 *
*&---------------------------------------------------------------------*
DATA: lv_okcode_100 TYPE sy-ucomm.

*&---------------------------------------------------------------------*
*                       Declaração de Tipos                            *
*&---------------------------------------------------------------------*
TYPE-POOLS: slis.

*&---------------------------------------------------------------------*
*                        Estruturas do ALV                             *
*&---------------------------------------------------------------------*
DATA: lo_container_100 TYPE REF TO cl_gui_custom_container,
      lo_grid_100      TYPE REF TO cl_gui_alv_grid,
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
"Tipos de carga do cadastro de cliente
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rb_unic RADIOBUTTON GROUP gr2 DEFAULT 'X' MODIF ID rb4.
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
  IF rb_cli EQ 'X'.

    IF p_nome IS NOT INITIAL AND p_rg IS NOT INITIAL AND p_cpf IS NOT INITIAL.
     PERFORM f_cadastra_cliente.
    ELSE.
     MESSAGE s208(00) WITH 'Preencha os dados obrigatórios!' DISPLAY LIKE 'E'.
    ENDIF.

  ELSEIF rb_cven EQ 'X'.

    IF p_dat_vd IS NOT INITIAL AND p_rg2 IS NOT INITIAL AND p_cpf2 IS NOT INITIAL AND p_prod IS NOT INITIAL AND p_valor IS NOT INITIAL.
     PERFORM  f_cadastra_venda.
    ELSE.
     MESSAGE s208(00) WITH 'Preencha os dados obrigatórios!' DISPLAY LIKE 'E'.
    ENDIF.

  ELSEIF rb_rven  EQ 'X'.
    PERFORM f_relatorio_de_vendas.
  ENDIF.

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

      wa_ztbcliente-cpf = p_cpf.
      wa_ztbcliente-email = p_email.
      wa_ztbcliente-endereco = p_end.
      wa_ztbcliente-nome_do_cliente = p_nome.
      wa_ztbcliente-rg = p_rg.
      wa_ztbcliente-telefone = p_tel.

      INSERT ztbcliente FROM wa_ztbcliente.

      IF sy-subrc IS INITIAL.
        COMMIT WORK AND WAIT. "COMMIT WORK AND WAIT dá commit no banco de dados

        MESSAGE s208(00) WITH 'CLIENTE CADASTRADO COM SUCESSO!'.
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
          'COD_DA_VENDA'     'COD_DA_VENDA'     'ZTBVENDA'   'Código da venda'   CHANGING lt_fieldcat[],
          'PRODUTO'          'PRODUTO'          'ZTBVENDA'   'Produto'           CHANGING lt_fieldcat[],
          'NOME_DO_CLIENTE'  'NOME_DO_CLIENTE'  'ZTBCLIENTE' 'Nome do cliente'   CHANGING lt_fieldcat[],
          'RG'               'RG'               'ZTBVENDA'   'RG'                CHANGING lt_fieldcat[],
          'CPF'              'CPF'              'ZTBVENDA'   'CPF'               CHANGING lt_fieldcat[],
          'ENDERECO'         'ENDERECO'         'ZTBCLIENTE' 'Endereço'          CHANGING lt_fieldcat[],
          'EMAIL'            'EMAIL'            'ZTBCLIENTE' 'Email'             CHANGING lt_fieldcat[],
          'TELEFONE'         'TELEFONE'         'ZTBCLIENTE' 'Telefone'          CHANGING lt_fieldcat[],
          'VALOR_DA_VENDA'   'VALOR_DA_VENDA'   'ZTBVENDA'   'Valor da venda'    CHANGING lt_fieldcat[].

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
*<---- 21/01/2024 - Estudos - Wesley Constantino - Fim
    ENDIF.

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
