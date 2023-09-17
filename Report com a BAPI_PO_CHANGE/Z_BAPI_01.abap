*&---------------------------------------------------------------------*
*& Report Z_BAPI_01
*&---------------------------------------------------------------------*
REPORT Z_BAPI_01.

TABLES EKPO.

*&---------------------------------------------------------------------*
*                             Types                                    *
*&---------------------------------------------------------------------*
TYPES:
  BEGIN OF TY_EKPO,
    EBELN TYPE EKPO-EBELN,
    EBELP TYPE EKPO-EBELP,
    MWSKZ TYPE EKPO-MWSKZ,
  END OF TY_EKPO.

*&---------------------------------------------------------------------*
*                             Tables                                   *
*&---------------------------------------------------------------------*
DATA: IT_EKPO TYPE TABLE OF TY_EKPO,
      WA_EKPO LIKE LINE OF IT_EKPO.

DATA : it_po_item TYPE TABLE OF bapimepoitem,
       wa_po_item TYPE bapimepoitem.

DATA : it_po_itemx TYPE TABLE OF bapimepoitemx,
       wa_po_itemx TYPE          bapimepoitemx.

DATA : lt_return TYPE TABLE OF bapiret2,
       ls_return TYPE          bapiret2.

*&---------------------------------------------------------------------*
*                         Tela de seleção                              *
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-000.
 SELECT-OPTIONS: P_EBELN FOR EKPO-EBELN.
SELECTION-SCREEN END OF BLOCK b0.

START-OF-SELECTION.
PERFORM: Z_SELECT,
         Z_BAPI_EXECULT.

*&---------------------------------------------------------------------*
*                          FORM Z_SELECT.                              *
*&---------------------------------------------------------------------*
FORM Z_SELECT.

SELECT EBELN
       EBELP
       MWSKZ
  FROM EKPO
  INTO TABLE IT_EKPO
  WHERE EBELN IN P_EBELN.

ENDFORM.

*&---------------------------------------------------------------------*
*                          Z_BAPI_EXECULT.                              *
*&---------------------------------------------------------------------*
FORM Z_BAPI_EXECULT.

LOOP AT it_ekpo INTO wa_ekpo.

    wa_po_item-po_item  = wa_ekpo-ebelp.
    wa_po_itemx-po_item  = wa_ekpo-ebelp.
    wa_po_itemx-po_itemx = 'X'.
    wa_po_item-tax_code  = 'A6'. "Campo que será atualizado na tabela EKPO.
    wa_po_itemx-tax_code = 'X'.
    APPEND wa_po_item TO it_po_item.
    APPEND wa_po_itemx TO it_po_itemx.

CALL FUNCTION 'BAPI_PO_CHANGE'
  EXPORTING
    purchaseorder               = wa_ekpo-ebeln "Número da ordem
 TABLES
   RETURN                       = lt_return
   POITEM                       = it_po_item
   POITEMX                      = it_po_itemx.

  READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
  IF sy-subrc EQ 0.
   CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        MESSAGE s208(00) WITH 'ERRO AO ATUALIZAR!'.
  ELSE.
   CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
   IF sy-subrc EQ 0.
     MESSAGE s208(00) WITH 'ATUALIZADO COM SUCESSO!'.
   ENDIF.
  ENDIF.

ENDLOOP.
ENDFORM.
