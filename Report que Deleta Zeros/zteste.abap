REPORT zteste.

*&---------------------------------------------------------------------*
*                        Tabelas  Internas                             *
*&---------------------------------------------------------------------*
DATA: it_out     TYPE TABLE OF ztestudos_1.

*&---------------------------------------------------------------------*
*                            Workareas                                 *
*&---------------------------------------------------------------------*
DATA: wa_out     LIKE LINE OF it_out.

*&---------------------------------------------------------------------*
*                            Variaveis                                 *
*&---------------------------------------------------------------------*

INITIALIZATION.
PERFORM: zf_select.

*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBE_POPUP_CAMINHO_UPLOAD
*&---------------------------------------------------------------------*
FORM zf_select.

   SELECT *
     FROM ztestudos_1
     INTO TABLE it_out.

     LOOP AT it_out INTO wa_out.
      DATA: v_servidor TYPE ztestudos_1-servidor.

      MOVE wa_out-servidor TO v_servidor.
*Deleta os zeros que estão á esquerda:
       SHIFT v_servidor LEFT DELETING LEADING '0'.

*Deleta os zeros que estão á direita:
       SHIFT v_servidor RIGHT DELETING TRAILING'0'.

     ENDLOOP.
ENDFORM.
