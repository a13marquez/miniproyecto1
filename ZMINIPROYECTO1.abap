*&---------------------------------------------------------------------*
*& Report  ZMINIPROYECTO1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZMINIPROYECTO1.
TYPES: BEGIN OF ty_vuelos_compania,
          id_compania         TYPE s_carr_id,
          compania            TYPE s_carrname,
          num_conexion        TYPE s_conn_id,
          pais_origen         TYPE land1,
          ciudad_origen       TYPE s_from_cit,
          aeropuerto_origen   TYPE s_fromairp,
          pais_destino        TYPE land1,
          ciudad_destino      TYPE s_to_city,
          aeropuerto_destino  TYPE s_toairp,
          distancia           TYPE s_distance,
          unidad_distancia    TYPE s_distid,
         END OF ty_vuelos_compania.

DATA: gt_vuelos_compania    TYPE STANDARD TABLE OF ty_vuelos_compania,
      gr_salv               TYPE REF TO CL_SALV_TABLE,
      ls_compania           TYPE s_carr_id,
      ls_conexion           TYPE s_conn_id.

FIELD-SYMBOLS: <g_vuelo> LIKE LINE OF gt_vuelos_compania.

CLASS lcl_manejador DEFINITION.
  PUBLIC SECTION.
    METHODS: manejar_doble_click FOR EVENT double_click
                                 OF cl_salv_events_table
                                  IMPORTING row
                                            column.
ENDCLASS.

CLASS lcl_manejador IMPLEMENTATION.
  METHOD manejar_doble_click.
    READ TABLE gt_vuelos_compania
        ASSIGNING <g_vuelo>
        INDEX row.
     CASE column.
     WHEN 'COMPANIA'.
      PERFORM mostrarInfoCompania
                      USING <g_vuelo>-ID_COMPANIA.
     WHEN 'NUM_CONEXION'.
       PERFORM mostrarInfoConexion
                      USING <g_vuelo>-NUM_CONEXION .
    ENDCASE.

  ENDMETHOD.
ENDCLASS.



DATA: gr_manejador_eventos  TYPE REF TO lcl_manejador.
"Clase para manejar el doble click


START-OF-SELECTION.

  PERFORM obtenerDatosVuelos
              CHANGING gt_vuelos_compania .

  PERFORM obtenerSalv
              USING gt_vuelos_compania gr_salv.

  PERFORM configurarAlv
              USING gr_salv.


  FORM obtenerDatosVuelos
           CHANGING lt_vuelos_compania LIKE gt_vuelos_compania.

    TYPES: BEGIN OF ty_compania,
            id_compania         TYPE s_carr_id,
            compania            TYPE s_carrname,
          END OF ty_compania,
          BEGIN OF ty_vuelos,
            id_compania         TYPE s_carr_id,
            num_conexion        TYPE s_conn_id,
            pais_origen         TYPE land1,
            ciudad_origen       TYPE s_from_cit,
            aeropuerto_origen   TYPE s_fromairp,
            pais_destino        TYPE land1,
            ciudad_destino      TYPE s_to_city,
            aeropuerto_destino  TYPE s_toairp,
            distancia           TYPE s_distance,
            unidad_distancia    TYPE s_distid,
          END OF ty_vuelos.

    DATA: lt_compania TYPE STANDARD TABLE OF ty_compania,
          lt_vuelos   TYPE STANDARD TABLE OF ty_vuelos.

    FIELD-SYMBOLS: <compania>       LIKE LINE OF lt_compania,
                   <vuelo>          LIKE LINE OF lt_vuelos,
                   <vuelo_compania> LIKE LINE OF lt_vuelos_compania.

    SELECT carrid carrname
      INTO TABLE lt_compania
      FROM scarr.

    SELECT carrid connid countryfr cityfrom airpfrom countryto cityto airpto distance distid
      INTO TABLE  lt_vuelos
      FROM spfli.

    LOOP AT lt_vuelos ASSIGNING <vuelo>.
      APPEND INITIAL LINE TO lt_vuelos_compania ASSIGNING <vuelo_compania>.
        <vuelo_compania>-ID_COMPANIA = <vuelo>-ID_COMPANIA.
        <vuelo_compania>-NUM_CONEXION = <vuelo>-NUM_CONEXION.
        <vuelo_compania>-PAIS_ORIGEN = <vuelo>-PAIS_ORIGEN.
        <vuelo_compania>-CIUDAD_ORIGEN = <vuelo>-CIUDAD_ORIGEN.
        <vuelo_compania>-AEROPUERTO_ORIGEN = <vuelo>-AEROPUERTO_ORIGEN.
        <vuelo_compania>-PAIS_DESTINO = <vuelo>-PAIS_DESTINO.
        <vuelo_compania>-CIUDAD_DESTINO = <vuelo>-CIUDAD_DESTINO.
        <vuelo_compania>-AEROPUERTO_DESTINO = <vuelo>-AEROPUERTO_DESTINO.
        <vuelo_compania>-DISTANCIA = <vuelo>-DISTANCIA.
        <vuelo_compania>-UNIDAD_DISTANCIA = <vuelo>-UNIDAD_DISTANCIA.
        LOOP AT lt_compania ASSIGNING <compania>
          WHERE ID_COMPANIA = <vuelo_compania>-ID_COMPANIA.
            <vuelo_compania>-COMPANIA = <COMPANIA>-COMPANIA.
        ENDLOOP.
    ENDLOOP.
 ENDFORM.
 FORM obtenerSalv
              USING gt_vuelos_compania lr_salv LIKE gr_salv.


  TRY .
    CL_SALV_TABLE=>FACTORY(
    exporting
      LIST_DISPLAY   = IF_SALV_C_BOOL_SAP=>FALSE    " ALV Displayed in List Mode=
    importing
      R_SALV_TABLE   =  gr_salv   " Basis Class Simple ALV Tables
    changing
      T_TABLE        =  gt_vuelos_compania
  ).
  CATCH CX_SALV_MSG.    " ALV: General Error Class with Message.

  ENDTRY.


 ENDFORM.
 FORM configurarAlv
              CHANGING lr_salv LIKE gr_salv.
   DATA: lr_columnas          TYPE REF TO cl_salv_columns_table,
         lr_columna           TYPE REF TO cl_salv_column_table,
         lr_color             TYPE lvc_s_colo,
         lr_sorts             TYPE REF TO cl_salv_sorts,
         lr_columna_sort      TYPE REF TO cl_salv_sort,
         lr_aggregations      TYPE REF TO cl_salv_aggregations,
         lr_columna_agregada  TYPE REF TO cl_salv_aggregation,
         lr_eventos           TYPE REF TO cl_salv_events_table.

  "Se añade la barra de tareas con todos los botones
   lr_salv->GET_FUNCTIONS( )->SET_ALL( ).
  "Se añade el título
   lr_salv->GET_DISPLAY_SETTINGS( )->SET_LIST_HEADER( 'Planificación de vuelos' ).


  "Se obtienen las columnas para los cambios que hay que realizar en ellas
   lr_columnas = lr_salv->GET_COLUMNS( ).
  "Se ajusta el ancho al contenido
   lr_columnas->SET_OPTIMIZE( ).

  "Se oculta la columna con el id de compañia
    TRY.
     lr_columna ?= lr_columnas->GET_COLUMN('ID_COMPANIA').
     lr_columna->SET_VISIBLE( value  = if_salv_c_bool_sap=>false ).
   CATCH cx_salv_not_found.
   ENDTRY.

  "Se añade el color rojo a la columna del numero de conexion
   TRY.
     lr_columna ?= lr_columnas->GET_COLUMN('NUM_CONEXION').
     lr_color-col = 6.
     lr_columna->SET_COLOR( lr_color ).
   CATCH cx_salv_not_found.
   ENDTRY.

   TRY.
     lr_columna ?= lr_columnas->GET_COLUMN('DISTANCIA').
     lr_columna->SET_QUANTITY_COLUMN( VALUE = 'UNIDAD_DISTANCIA' ).
   CATCH CX_SALV_DATA_ERROR.    " ALV: General Error Class (Checked During Syntax Check).
   CATCH cx_salv_not_found.
   ENDTRY.
   "Se agrega por unidad de distancia
   lr_aggregations = lr_salv->GET_AGGREGATIONS( ).
   TRY.
    CALL METHOD lr_aggregations->ADD_AGGREGATION
      exporting
        COLUMNNAME  =   'DISTANCIA'  " ALV Control: Field Name of Internal Table Field
        AGGREGATION = IF_SALV_C_AGGREGATION=>TOTAL    " Aggregation
      receiving
        VALUE       = lr_columna_agregada    " ALV: Aggregations
      .
      lr_aggregations->SET_AGGREGATION_BEFORE_ITEMS( ).
      CATCH CX_SALV_DATA_ERROR.    " ALV: General Error Class (Checked During Syntax Check)
      CATCH CX_SALV_NOT_FOUND.    " ALV: General Error Class (Checked During Syntax Check)
      CATCH CX_SALV_EXISTING.    " ALV: General Error Class (Checked During Syntax Check)
   ENDTRY.

  "Se ordenan las compañias y se crean subtotales
   lr_sorts = lr_salv->GET_SORTS( ).
   TRY.
     CALL METHOD lr_sorts->ADD_SORT
       exporting
         COLUMNNAME =  'COMPANIA'   " ALV Control: Field Name of Internal Table Field
       receiving
         VALUE      =  lr_columna_sort   " ALV Sort Settings
       .
     CALL METHOD lr_columna_sort->SET_SUBTOTAL
       exporting
         VALUE = IF_SALV_C_BOOL_SAP=>TRUE    " Boolean Variable (X=True, Space=False)
       .
     CATCH cx_salv_not_found.
     CATCH cx_salv_existing .
     CATCH cx_salv_data_error .
   ENDTRY.

   lr_eventos = gr_salv->GET_EVENT( ).
   CREATE OBJECT gr_manejador_eventos.
   SET HANDLER gr_manejador_eventos->manejar_doble_click FOR lr_eventos.

   GR_SALV->DISPLAY( ).
 ENDFORM.

 FORM mostrarInfoCompania
                        USING ls_compania.
   DATA: lr_columnas          TYPE REF TO cl_salv_columns_table,
         lr_columna           TYPE REF TO cl_salv_column_table,
         lr_sorts             TYPE REF TO cl_salv_sorts,
         lr_color             TYPE lvc_s_colo,
         lr_columna_sort      TYPE REF TO cl_salv_sort,
         lr_aggregations      TYPE REF TO cl_salv_aggregations,
         lr_columna_agregada  TYPE REF TO cl_salv_aggregation,
         lr_eventos           TYPE REF TO cl_salv_events_table.


  TYPES: BEGIN OF ty_compania_precio,
            id_compania TYPE s_carr_id,
            compania    TYPE s_carrname,
          END OF ty_compania_precio,
          BEGIN OF ty_vuelo_precio,
            id_compania TYPE s_carr_id,
            importe     TYPE s_price,
            moneda      TYPE s_currcode,
          END OF ty_vuelo_precio,
          BEGIN OF ty_vuelos_compania_precio,
            id_compania TYPE s_carr_id,
            compania    TYPE s_carrname,
            importe     TYPE s_price,
            moneda      TYPE s_currcode,
          END OF ty_vuelos_compania_precio.


  DATA: lt_compania_precio        TYPE STANDARD TABLE OF ty_compania_precio,
        lt_vuelos_precio          TYPE STANDARD TABLE OF ty_vuelo_precio,
        lt_vuelos_compania_precio TYPE STANDARD TABLE OF ty_vuelos_compania_precio.

  FIELD-SYMBOLS: <compania_precio>        LIKE LINE OF lt_compania_precio,
                 <vuelo_precio>           LIKE LINE OF lt_vuelos_precio,
                 <vuelo_compania_precio>  LIKE LINE OF lt_vuelos_compania_precio.


  SELECT carrid carrname
      INTO TABLE lt_compania_precio
      FROM scarr WHERE scarr~carrid = ls_compania.

  SELECT carrid price currency
      INTO TABLE  lt_vuelos_precio
      FROM sflight WHERE sflight~carrid = ls_compania.

  LOOP AT lt_vuelos_precio ASSIGNING <vuelo_precio>.
      APPEND INITIAL LINE TO lt_vuelos_compania_precio ASSIGNING <vuelo_compania_precio>.
        <vuelo_compania_precio>-ID_COMPANIA = <vuelo_precio>-ID_COMPANIA.
        <vuelo_compania_precio>-IMPORTE = <vuelo_precio>-IMPORTE.
        <vuelo_compania_precio>-MONEDA = <vuelo_precio>-MONEDA.
        LOOP AT lt_compania_precio ASSIGNING <compania_precio>
          WHERE ID_COMPANIA = <vuelo_compania_precio>-ID_COMPANIA.
            <vuelo_compania_precio>-COMPANIA = <compania_precio>-COMPANIA.
        ENDLOOP.
    ENDLOOP.

    TRY .
    CL_SALV_TABLE=>FACTORY(
    exporting
      LIST_DISPLAY   = IF_SALV_C_BOOL_SAP=>FALSE    " ALV Displayed in List Mode=
    importing
      R_SALV_TABLE   =  gr_salv   " Basis Class Simple ALV Tables
    changing
      T_TABLE        =  lt_vuelos_compania_precio
  ).
  CATCH CX_SALV_MSG.    " ALV: General Error Class with Message.

  ENDTRY.


  GR_SALV->DISPLAY( ).
 ENDFORM.
 FORM mostrarInfoConexion
                      USING ls_conexion.
   DATA: lr_columnas          TYPE REF TO cl_salv_columns_table,
         lr_columna           TYPE REF TO cl_salv_column_table,
         lr_columna1          TYPE REF TO cl_salv_column_table,
         lr_row               TYPE  i,
         lr_sorts             TYPE REF TO cl_salv_sorts,
         lr_color             TYPE lvc_s_scol,
         lr_columna_sort      TYPE REF TO cl_salv_sort,
         lr_aggregations      TYPE REF TO cl_salv_aggregations,
         lr_columna_agregada  TYPE REF TO cl_salv_aggregation,
         lr_eventos           TYPE REF TO cl_salv_events_table.


  TYPES: BEGIN OF ty_compania_asientos,
            id_compania TYPE s_carr_id,
            compania    TYPE s_carrname,
          END OF ty_compania_asientos,
          BEGIN OF ty_vuelo_asientos,
            id_compania       TYPE s_carr_id,
            turista_ocupado   TYPE s_seatsocc,
            turista_total     TYPE s_seatsmax,
            bussines_ocupado  TYPE s_socc_b,
            bussines_total    TYPE s_smax_b,
            primera_ocupado   TYPE s_socc_f,
            primera_total     TYPE s_smax_f,
          END OF ty_vuelo_asientos,
          BEGIN OF ty_vuelos_compania_asientos,
            id_compania         TYPE s_carr_id,
            compania            TYPE s_carrname,
            turista_ocupado     TYPE s_seatsocc,
            turista_porcentaje  TYPE i,
            bussines_ocupado    TYPE s_socc_b,
            bussines_porcentaje TYPE i,
            primera_ocupado     TYPE s_socc_f,
            primera_porcentaje  TYPE s_smax_f,
            color               TYPE lvc_t_scol,
          END OF ty_vuelos_compania_asientos.


  DATA: lt_compania_asientos        TYPE STANDARD TABLE OF ty_compania_asientos,
        lt_vuelos_asientos          TYPE STANDARD TABLE OF ty_vuelo_asientos,
        lt_vuelos_compania_asientos TYPE STANDARD TABLE OF ty_vuelos_compania_asientos.

  FIELD-SYMBOLS: <compania_asientos>        LIKE LINE OF lt_compania_asientos,
                 <vuelo_asientos>           LIKE LINE OF lt_vuelos_asientos,
                 <vuelo_compania_asientos>  LIKE LINE OF lt_vuelos_compania_asientos.


  SELECT carrid carrname
      INTO TABLE lt_compania_asientos
      FROM scarr.

  SELECT carrid seatsocc seatsmax seatsocc_b seatsmax_b seatsocc_f seatsocc_b
      INTO TABLE  lt_vuelos_asientos
      FROM sflight WHERE sflight~connid = ls_conexion.

  LOOP AT lt_vuelos_asientos ASSIGNING <vuelo_asientos>.
      APPEND INITIAL LINE TO lt_vuelos_compania_asientos ASSIGNING <vuelo_compania_asientos>.
        <vuelo_compania_asientos>-ID_COMPANIA = <vuelo_asientos>-ID_COMPANIA.
        <vuelo_compania_asientos>-TURISTA_OCUPADO = <vuelo_asientos>-TURISTA_OCUPADO.
        IF <vuelo_asientos>-TURISTA_OCUPADO = 0.
          <vuelo_compania_asientos>-TURISTA_PORCENTAJE = 0.
        ELSE.
           <vuelo_compania_asientos>-TURISTA_PORCENTAJE = <vuelo_asientos>-TURISTA_TOTAL / <vuelo_asientos>-TURISTA_OCUPADO.
        ENDIF.
        <vuelo_compania_asientos>-BUSSINES_OCUPADO = <vuelo_asientos>-BUSSINES_OCUPADO.
        IF <vuelo_asientos>-BUSSINES_OCUPADO = 0.
          <vuelo_compania_asientos>-BUSSINES_PORCENTAJE = 0.
        ELSE.
          <vuelo_compania_asientos>-BUSSINES_PORCENTAJE = <vuelo_asientos>-BUSSINES_TOTAL / <vuelo_asientos>-BUSSINES_OCUPADO.
        ENDIF.
        <vuelo_compania_asientos>-PRIMERA_OCUPADO = <vuelo_asientos>-PRIMERA_OCUPADO.
        IF <vuelo_asientos>-PRIMERA_OCUPADO = 0.
          <vuelo_compania_asientos>-PRIMERA_PORCENTAJE = 0.
        ELSE.
          <vuelo_compania_asientos>-PRIMERA_PORCENTAJE = <vuelo_asientos>-PRIMERA_TOTAL / <vuelo_asientos>-PRIMERA_OCUPADO.
        ENDIF.
        LOOP AT lt_compania_asientos ASSIGNING <compania_asientos>
          WHERE ID_COMPANIA = <vuelo_compania_asientos>-ID_COMPANIA.
            <vuelo_compania_asientos>-COMPANIA = <compania_asientos>-COMPANIA.
        ENDLOOP.
    ENDLOOP.
    "Se añade el color rojo a la columna del numero de conexion
    LOOP AT lt_vuelos_compania_asientos ASSIGNING <vuelo_compania_asientos>.

     IF <vuelo_compania_asientos>-TURISTA_PORCENTAJE > 2.
     	  lr_color-fname     = 'TURISTA_OCUPADO'.
        lr_color-color-col = 6.
        APPEND lr_color TO <vuelo_compania_asientos>-color.
     ELSE.
       lr_color-fname     = 'TURISTA_OCUPADO'.
        lr_color-color-col = 5.
        APPEND lr_color TO <vuelo_compania_asientos>-color.
     ENDIF.
    ENDLOOP.

    TRY .
    CL_SALV_TABLE=>FACTORY(
    exporting
      LIST_DISPLAY   = IF_SALV_C_BOOL_SAP=>FALSE    " ALV Displayed in List Mode=
    importing
      R_SALV_TABLE   =  gr_salv   " Basis Class Simple ALV Tables
    changing
      T_TABLE        =  lt_vuelos_compania_asientos
  ).
  CATCH CX_SALV_MSG.    " ALV: General Error Class with Message.

  ENDTRY.
  "Se añade la barra de tareas con todos los botones
   gr_salv->GET_FUNCTIONS( )->SET_ALL( ).
  "Se añade el título
   gr_salv->GET_DISPLAY_SETTINGS( )->SET_LIST_HEADER( 'Ocupación de asientos por conexión' ).


  "Se obtienen las columnas para los cambios que hay que realizar en ellas
   lr_columnas = gr_salv->GET_COLUMNS( ).
  "Se ajusta el ancho al contenido
   lr_columnas->SET_OPTIMIZE( ).

  "Se oculta la columna con el id de compañia
   TRY.
     lr_columna ?= lr_columnas->GET_COLUMN('ID_COMPANIA').
     lr_columna->SET_VISIBLE( value  = if_salv_c_bool_sap=>false ).
   CATCH cx_salv_not_found.
   ENDTRY.
   TRY.
     lr_columna ?= lr_columnas->GET_COLUMN('TURISTA_PORCENTAJE').
     lr_columna->SET_VISIBLE( value  = if_salv_c_bool_sap=>false ).
   CATCH cx_salv_not_found.
   ENDTRY.
   TRY.
     lr_columna ?= lr_columnas->GET_COLUMN('BUSSINES_PORCENTAJE').
     lr_columna->SET_VISIBLE( value  = if_salv_c_bool_sap=>false ).
   CATCH cx_salv_not_found.
   ENDTRY.
   TRY.
     lr_columna ?= lr_columnas->GET_COLUMN('PRIMERA_PORCENTAJE').
     lr_columna->SET_VISIBLE( value  = if_salv_c_bool_sap=>false ).
   CATCH cx_salv_not_found.
   ENDTRY.






  "Se ordenan las compañias y se crean subtotales
   lr_sorts = gr_salv->GET_SORTS( ).
   TRY.
     CALL METHOD lr_sorts->ADD_SORT
       exporting
         COLUMNNAME =  'COMPANIA'   " ALV Control: Field Name of Internal Table Field
       receiving
         VALUE      =  lr_columna_sort   " ALV Sort Settings
       .
     CALL METHOD lr_columna_sort->SET_SUBTOTAL
       exporting
         VALUE = IF_SALV_C_BOOL_SAP=>TRUE    " Boolean Variable (X=True, Space=False)
       .
     CATCH cx_salv_not_found.
     CATCH cx_salv_existing .
     CATCH cx_salv_data_error .
   ENDTRY.

   "Se agrega por unidad de distancia
   lr_aggregations = gr_salv->GET_AGGREGATIONS( ).


  GR_SALV->DISPLAY( ).

 ENDFORM.