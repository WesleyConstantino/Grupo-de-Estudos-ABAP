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
*                           Workareas                                  *
*&---------------------------------------------------------------------*
DATA: gv_ztbvenda TYPE ztbvenda.

*&---------------------------------------------------------------------*
*                         Tela de seleção                              *
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-000.
*Radio Buttons
PARAMETERS: rb_cli RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND comando.
PARAMETERS: rb_cven RADIOBUTTON GROUP g1.
PARAMETERS: rb_rven RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
*PARAMETERS
"Cadastrar cliente -> rb_cli
PARAMETERS: p_nome TYPE ZTBCLIENTE-nome_do_cliente MODIF ID rb1, "OBLIGATORY,  "Modifico o ID dos campos que quero eventualmente ocultar com "MODIF ID".
            p_rg TYPE ZTBCLIENTE-rg MODIF ID rb1, "OBLIGATORY,
            p_cpf TYPE ZTBCLIENTE-cpf MODIF ID rb1, " OBLIGATORY,
            p_end TYPE ZTBCLIENTE-endereco MODIF ID rb1,
            p_email TYPE ZTBCLIENTE-email MODIF ID rb1,
            p_tel TYPE ZTBCLIENTE-telefone MODIF ID rb1.

"Cadastrar venda -> rb_cven
PARAMETERS: p_rg2 TYPE ZTBVENDA-rg MODIF ID rb2 , "OBLIGATORY,
            p_cpf2 TYPE ZTBVENDA-cpf MODIF ID rb2 , "OBLIGATORY,
            p_dat_vd TYPE ZTBVENDA-data_da_venda MODIF ID rb2 , "OBLIGATORY,
            p_prod TYPE ZTBVENDA-produto MODIF ID rb2 , "OBLIGATORY,
            p_valor TYPE ZTBVENDA-valor_da_venda MODIF ID rb2. "OBLIGATORY.

*SELECT-OPTIONS
"Relatório de vendas
SELECT-OPTIONS: s_cod_vd FOR ZTBVENDA-cod_da_venda MODIF ID rb3,
                s_rg FOR ZTBVENDA-rg MODIF ID rb3,
                s_cpf FOR ZTBVENDA-cpf MODIF ID rb3,
                s_dat_vd FOR ZTBVENDA-data_da_venda MODIF ID rb3,
                s_prod FOR ZTBVENDA-produto MODIF ID rb3.
SELECTION-SCREEN END OF BLOCK b1.

*"Evento para reconhecer os ciques do radiobutton e mudar as telas
AT SELECTION-SCREEN OUTPUT.
  PERFORM f_modifica_tela.

"Início da execusão
START-OF-SELECTION.
  IF rb_cli EQ 'X'.
    PERFORM f_cadastra_cliente.
  ELSEIF rb_cven EQ 'X'.
    PERFORM  f_cadastra_venda.
  ELSEIF rb_rven  EQ 'X'.
    PERFORM f_relatorio_de_vendas.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  f_cadastra_cliente
*&---------------------------------------------------------------------*
 FORM f_cadastra_cliente.

 DATA: LT_ZTBCLIENTE TYPE TABLE OF ZTBCLIENTE.

   SELECT NOME_DO_CLIENTE,
          RG,
          CPF
     INTO TABLE @LT_ZTBCLIENTE
     FROM ZTBCLIENTE
     WHERE NOME_DO_CLIENTE EQ @p_nome AND
           RG EQ  @p_rg AND
           CPF EQ @p_cpf.

    IF LT_ZTBCLIENTE IS NOT INITIAL.
      MESSAGE s208(00) WITH 'Cliente já cadastrado.' DISPLAY LIKE 'E'.
    ELSE.
      PERFORM f_update_cliente.
    ENDIF.

 ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f_update_cliente
*&---------------------------------------------------------------------*
 FORM f_update_cliente.
 DATA: lv_ztbcliente TYPE ztbcliente.

      lv_ztbcliente-cpf = p_cpf.
      lv_ztbcliente-email = p_email.
      lv_ztbcliente-endereco = p_end.
      lv_ztbcliente-nome_do_cliente = p_nome.
      lv_ztbcliente-rg = p_rg.
      lv_ztbcliente-telefone = p_tel.

      INSERT ztbcliente FROM lv_ztbcliente.

      IF sy-subrc IS INITIAL.
        COMMIT WORK AND WAIT. "COMMIT WORK AND WAIT dá commit no banco de dados

        MESSAGE s208(00) WITH 'SALVO COM SUCESSO!'.
      ELSE.
        ROLLBACK WORK. "ROLLBACK WORK desfaz tudo o que aconteceu na operação
        MESSAGE s208(00) WITH 'ERRO AO GRAVAR!'DISPLAY LIKE 'E'.
      ENDIF.
 ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f_cadastra_venda
*&---------------------------------------------------------------------*
 FORM f_cadastra_venda.
  DATA: lt_ztbvenda TYPE TABLE OF ztbvenda.

   SELECT RG,
          COD_DA_VENDA,
          CPF
     INTO TABLE @lt_ztbvenda
     FROM ztbvenda
     WHERE RG  EQ @p_rg2 AND
           CPF EQ @p_cpf2.

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
  DATA: vl_resposta TYPE c. "variável que receberá o parâmetro de saída do clique. Sim(001) ou Não(002).

  CALL FUNCTION 'POPUP_TO_CONFIRM'
  EXPORTING
   TITLEBAR                    = 'Informação'  "Título
   text_question               = 'Venda já cadastrada, deseja modificar?'   "Texto da pergunta do pupup
   TEXT_BUTTON_1               = 'Sim'(001)  "Texto do botão 1
*   ICON_BUTTON_1               = ' '   "Ícone do botão 1
   TEXT_BUTTON_2               = 'Não'(002) "Texto do botão 2
*   ICON_BUTTON_2               = ' '  "Ícone do botão 2
   DISPLAY_CANCEL_BUTTON       = 'X'
 IMPORTING
   ANSWER                      = vl_resposta.   "Parâmetro de saída

*Lógica para validar a escolha do usuário:
    IF vl_resposta EQ '1'.
      PERFORM f_modifica_venda.

    ELSEIF vl_resposta EQ '2'.
       "Instrução (Caso deixe aqui sem intrução, o popup fecha automaticamente após o clique e volta para a tela de seleção)
    ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f_modifica_venda
*&---------------------------------------------------------------------*
FORM f_modifica_venda.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f_update_venda
*&---------------------------------------------------------------------*
FORM f_update_venda.

PERFORM f_gera_cod_automatico.

gv_ztbvenda-rg             = p_rg2.
gv_ztbvenda-cpf            = p_cpf2.
gv_ztbvenda-data_da_venda  = p_dat_vd.
gv_ztbvenda-produto        = p_prod.
gv_ztbvenda-valor_da_venda = p_valor.

      INSERT ztbvenda FROM gv_ztbvenda.

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
 gv_ztbvenda-cod_da_venda = lv_cod_da_venda.

 ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f_relatorio_de_vendas
*&---------------------------------------------------------------------*
 FORM f_relatorio_de_vendas.

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
    ENDIF.

    MODIFY SCREEN. "Preciso dar um MODIFY SCREEN para que funcione.
  ENDLOOP.
ENDFORM.
