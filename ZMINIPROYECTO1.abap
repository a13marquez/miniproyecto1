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
      gr_salv               TYPE REF TO CL_SALV_TABLE.

CLASS lcl_manejador DEFINITION.
  PUBLIC SECTION.
    METHODS: manejar_doble_click FOR EVENT double_click
                                 OF cl_salv_events_table
                                  IMPORTING row
                                            column.
ENDCLASS.

CLASS lcl_manejador IMPLEMENTATION.
  METHOD manejar_doble_click.
    DATA: ls_compania TYPE s_carr_id.
    READ TABLE gt_vuelos_compania
      INTO ls_compania
      INDEX row.
    IF column = 'COMPANIA'.
      PERFORM mostrarInfoCompania
                        USING ls_compania.
    ENDIF.

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

   lr_eventos = gr_salv->GET_EVENT( ).
   CREATE OBJECT gr_manejador_eventos.
   SET HANDLER gr_manejador_eventos->manejar_doble_click FOR lr_eventos.

   GR_SALV->DISPLAY( ).
 ENDFORM.
 
 PERFORM mostrarInfoCompania
                        USING ls_compania.
 ENDFORM.