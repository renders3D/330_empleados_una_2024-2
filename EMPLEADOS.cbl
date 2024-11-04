       *>  *************************************************************
       *>  UNIVERSIDAD NACIONAL ABIERTA                                *
       *>  TRABAJO PRÁCTICO                                            *
       *>  ASIGNATURA: PROCESAMIENTO DE DATOS                          *
       *>  CÓDIGO: 330                                                 *
       *>  LAPSO: 2024-2                                               *
       *> **************************************************************
       *> --------------------------------------------------------------
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. EMPLEADOS.
       AUTHOR. CARLOS LUIS NORIEGA MÉNDEZ.
       DATE-WRITTEN. [24-09-2024].
       DATE-COMPILED. [24-09-2024].
       *> --------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       *>  ESTRUCTURA DEL SISTEMA
       *>  ARCHIVO PRINCIPAL
           SELECT F-EMPLEADOS ASSIGN TO "EMPLEADOS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-EMPLEADOS.
       *>  ARCHIVO DE ÍNDICES
           SELECT F-INDICE ASSIGN TO "INDICE.TXT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ID-EMPLEADO
               FILE STATUS IS FS-INDICE.
       *>  ARCHIVO DE DESBORDAMIENTO
           SELECT F-DESBORDAMIENTO ASSIGN TO "DESBORDAMIENTO.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-DESBORDAMIENTO.
       *> --------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       *>  REGISTRO PARA ARCHIVO PRINCIPAL
       FD  EMPLEADOS.
       01  REG-EMPLEADO.
           02  CI-EMPLEADO         PIC 9(8).
           02  APELLIDO            PIC A(50).
           02  NOMBRE              PIC A(50).
           02  SEXO                PIC A(1).
           02  TELF-FIJO           PIC 9(11).
           02  TELF-CELULAR        PIC 9(11).
           02  EMAIL               PIC X(100).
           02  DIRECCION           PIC X(250).
           02  CONTACTO            PIC X(100).
           02  SALARIO.
               03  MONTO-SALARIO           PIC 9(8)V99.
               03  MONTO-PRESTAMO       PIC 9(8)V99.
               03  MONTO-SSO            PIC 9(8)V99.
               03  FECHA-INGRESO.
                   04 INGRESO-DIA          PIC 9(02)
                   04 INGRESO-MES          PIC 9(02)
                   04 INGRESO-ANNO         PIC 9(04)
       *>  REGISTRO PARA ARCHIVO DE ÍNDICES
       FD  INDICE.
       01  REG-INDICE.
           02  ID-EMPLEADO          PIC 9(8).
           02  POSICION-REG         PIC 9(5).
       *>  REGISTRO PARA ARCHIVO DE DESBORDAMIENTO
       FD  DESBORDAMIENTO.
       01  REG-DESBORDAMIENTO.
           02  CI-EMPLEADO         PIC 9(8).
           02  APELLIDO            PIC A(50).
           02  NOMBRE              PIC A(50).
           02  SEXO                PIC A(1).
           02  TELF-FIJO           PIC 9(11).
           02  TELF-CELULAR        PIC 9(11).
           02  EMAIL               PIC X(100).
           02  DIRECCION           PIC X(250).
           02  CONTACTO            PIC X(100).
           02  SALARIO.
               03  MONTO-SALARIO        PIC 9(8)V99.
               03  MONTO-PRESTAMO       PIC 9(8)V99.
               03  MONTO-SSO            PIC 9(8)V99.
               03  FECHA-INGRESO.
                   04 INGRESO-DIA          PIC 9(02)
                   04 INGRESO-MES          PIC 9(02)
                   04 INGRESO-ANNO         PIC 9(04)
       *> --------------------------------------------------------------
       WORKING-STORAGE SECTION. 
       *> ESTRUCTURA PARA EL REGISTRO DE EMPLEADO ----------------------
       01  WS-EMPLEADO
           02  WS-CI-EMPLEADO         PIC 9(8).
           02  WS-APELLIDO            PIC A(50).
           02  WS-NOMBRE              PIC A(50).
           02  WS-SEXO                PIC A(1).
           02  WS-TELF-FIJO           PIC 9(11).
           02  WS-TELF-CELULAR        PIC 9(11).
           02  WS-EMAIL               PIC X(100).
           02  WS-DIRECCION           PIC X(250).
           02  WS-CONTACTO            PIC X(100).
           02  WS-SALARIO.
               03  WS-MONTO-SALARIO        PIC 9(8)V99.
               03  WS-MONTO-PRESTAMO       PIC 9(8)V99.
               03  WS-MONTO-SSO            PIC 9(8)V99.
               03  WS-FECHA-INGRESO.
                   04  WS-INGRESO-DIA          PIC 9(02).
                   04  WS-INGRESO-MES          PIC 9(02).
                   04  WS-INGRESO-ANNO         PIC 9(04).
       *> ESTRUCTURA DEL REGISTRO DE ÍNDICES
       01  WS-INDICES.
           02  WS-ID-EMPLEADO          PIC 9(8).
           02  WS-POSICION-REG         PIC 9(5).
       *> VARIABLES PARA MANEJO DE ESTADOS -----------------------------
       77  FS-INDICE        PIC XX.
       01  CONTADOR-DESBORDAMIENTO PIC 9(5) VALUE ZERO.
       *> VARIABLES DE USO GENERAL -------------------------------------
       77  WS-FIN                     PIC 9(01) VALUE ZERO.
       77  WS-OPCION                  PIC 9(01).
       77  WS-CEDULA                  PIC 9(8).
       77  WS-POSICION-ACTUAL         PIC 9(10).
       77  WS-CONFIRMACION            PIC A(01).
       77  WS-EMPLEADO-EOF            PIC 9(01).
       *> MANEJO DE FECHA ----------------------------------------------
       77 WS-FECHA-SISTEMA            PIC 9(06).
       01 WS-FECHA-FORMATO.
           02 WS-FECHA-ANNO           PIC 9(02).
           02 WS-FECHA-MES            PIC 9(02).
           02 WS-FECHA-DIA            PIC 9(02).
       77 WS-ANNO-EXTENDIDO           PIC 9(04).
       *> --------------------------------------------------------------
       PROCEDURE DIVISION.
       *>  -------------------------------------------------------------
       *>  FUNCIÓN PRINCIPAL DEL SISTEMA
       *>  -------------------------------------------------------------
       MAIN-PROGRAM.
           MOVE ZERO TO WS-FIN
           PERFORM 000-MENU-PRINCIPAL UNTIL WS-FIN = 1.
           DISPLAY "TERMINANDO EL SISTEMA DE GESTIÓN DOCUMENTAL..."
           STOP RUN.
       *>  -------------------------------------------------------------
       *>  RUTINA PARA MOSTRAR Y GESTIONAR EL MENÚ PRINCIPAL
       *>  -------------------------------------------------------------
       000-MENU-PRINCIPAL.
           DISPLAY "+++++++++++++++++++++++++++++++++++++++++".
           DISPLAY "+++++ SISTEMA DE GESTIÓN DOCUMENTAL +++++".
           DISPLAY "+++++++++++++++++++++++++++++++++++++++++".

           ACCEPT WS-FECHA-SISTEMA FROM DATE.
           MOVE WS-FECHA-SISTEMA TO WS-FECHA-FORMATO.
           COMPUTE WS-ANNO-EXTENDIDO = WS-FECHA-ANNO + 2000.

           DISPLAY "BIENVENIDO | FECHA: "
                    WS-FECHA-DIA "/" WS-FECHA-MES
                     "/" WS-ANNO-EXTENDIDO.
           DISPLAY " ".
           DISPLAY "MENÚ DEL SISTEMA DE GESTIÓN DOCUMENTAL".
           DISPLAY "--------------------------------------".
           DISPLAY "1 - REGISTRAR NUEVO EMPLEADO.".
           DISPLAY "2 - EDITAR EMPLEADO.".
           DISPLAY "3 - ELIMINAR EMPLEADO.".
           DISPLAY "4 - CONSULTAR EMPLEADO.".
           DISPLAY "5 - INFORME DE EMPLEADOS.".
           DISPLAY "--------------------------------------".
           DISPLAY "9 - SALIR DEL SISTEMA.".
           DISPLAY " ".
           DISPLAY "OPCIÓN: " WITH NO ADVANCING.
           ACCEPT WS-OPCION.

           EVALUATE WS-OPCION
               WHEN 1     PERFORM 001-REGISTRAR
               WHEN 2     PERFORM 001-EDITAR
               WHEN 3     PERFORM 001-ELIMINAR
               WHEN 4     PERFORM 001-CONSULTAR
               WHEN 5     PERFORM 001-INFORME
               WHEN 9     MOVE 1 TO WS-FIN
               WHEN OTHER
                   DISPLAY " "
                   DISPLAY "OPCION NO DISPONIBLE."
           END-EVALUATE.
       *>  -------------------------------------------------------------
       *>  RUTINA PARA REGISTRAR NUEVO EMPLEADO
       *>  -------------------------------------------------------------
       001-REGISTRAR.
           DISPLAY " ".
           DISPLAY "REGISTRAR NUEVO EMPLEADO.".
           DISPLAY "---------------------------------------".
           DISPLAY " ".
           *> SOLICITUD DE LOS DATOS DEL EMPLEADO
           DISPLAY "C.I. EMPLEADO: " WITH NO ADVANCING.
           ACCEPT WS-CI-EMPLEADO.
           DISPLAY "APELLIDO: " WITH NO ADVANCING.
           ACCEPT WS-APELLIDO.
           DISPLAY "NOMBRE: " WITH NO ADVANCING.
           ACCEPT WS-NOMBRE.
           DISPLAY "NOMBRE: " WITH NO ADVANCING.
           ACCEPT WS-NOMBRE.
           DISPLAY "SEXO: " WITH NO ADVANCING.
           ACCEPT WS-SEXO.
           DISPLAY "TELÉFONO FIJO: " WITH NO ADVANCING.
           ACCEPT WS-TELF-FIJO.
           DISPLAY "TELÉFONO CELULAR: " WITH NO ADVANCING.
           ACCEPT WS-TELF-CELULAR.
           DISPLAY "E-MAIL: " WITH NO ADVANCING.
           ACCEPT WS-EMAIL.
           DISPLAY "DIRECCIÓN: " WITH NO ADVANCING.
           ACCEPT WS-DIRECCION.
           DISPLAY "CONTACTO: " WITH NO ADVANCING.
           ACCEPT WS-CONTACTO.
           DISPLAY "DIA DE INGRESO: " WITH NO ADVANCING.
           ACCEPT WS-INGRESO-DIA.
           DISPLAY "MES DE INGRESO: " WITH NO ADVANCING.
           ACCEPT WS-INGRESO-MES.
           DISPLAY "AÑO DE INGRESO: " WITH NO ADVANCING.
           ACCEPT WS-INGRESO-ANNO.
           DISPLAY "MONTO DEL SALARIO: " WITH NO ADVANCING.
           ACCEPT WS-MONTO-SALARIO.
           DISPLAY "MONTO DEL PRÉSTAMO: " WITH NO ADVANCING.
           ACCEPT WS-MONTO-PRESTAMO.
           DISPLAY "MONTO DEL SSO: " WITH NO ADVANCING.
           ACCEPT WS-MONTO-SSO.
           *> ESCRITURA DEL ARCHIVO
           PERFORM 000-ABRIR-EMPLEADOS
           WRITE REG-EMPLEADO FROM WS-EMPLEADO
               INVALID KEY
                   REWRITE REG-EMPLEADO FROM WS-EMPLEADO
                   END-REWRITE
           END-WRITE.
           *> MANEJO DE ÍNDICE
           PERFORM 000-ABRIR-INDICES
           IF FS-INDICE = "00"
               READ INDICE INVALID KEY
               MOVE WS-CI-EMPLEADO TO WS-ID-EMPLEADO
               MOVE POSICION-ACTUAL TO WS-POSICION-REG
               WRITE REG-INDICE FROM WS-INDICES
               END-WRITE.
           ELSE
               ADD 1 TO CONTADOR-DESBORDAMIENTO
               PERFORM 000-ABRIR-DESBORDAMIENTO
               WRITE REG-DESBORDAMIENTO FROM WS-EMPLEADO
               END-WRITE.
               PERFORM 000-CIERRE-DESBORDAMIENTO
           END-IF.
           *> CIERRE DE ARCHIVOS
           PERFORM 000-CIERRE-EMPLEADOS
           PERFORM 000-CIERRE-INDICES
           DISPLAY "EMPLEADO REGISTRADO EXITOSAMENTE... " 
               WITH NO ADVANCING.
           STOP "ENTER PARA CONTINUAR.".
       *>  -------------------------------------------------------------
       *>  RUTINA PARA EDITAR LA INFORMACIÓN DE UN EMPLEADO
       *>  -------------------------------------------------------------
       001-EDITAR.
           DISPLAY " ".
           DISPLAY "EDITAR INFORMACIÓN DE EMPLEADO.".
           DISPLAY "---------------------------------------".
           DISPLAY " ".
           DISPLAY "CÉDULA No: " WITH NO ADVANCING.
           ACCEPT WS-CEDULA.
           PERFORM 000-ABRIR-INDICES
           READ REG-INDICE
           IF FS-INDICE = "00"
               IF REG-INDICE.ID-EMPLEADO = WS-CEDULA
                   MOVE REG-INDICE.POSICION-REG TO WS-POSICION-ACTUAL
                   PERFORM 000-CIERRE-INDICES
                   PERFORM 000-ABRIR-EMPLEADOS
                   IF FS-EMPLEADOS = "00"
                       MOVE REG-EMPLEADO TO WS-EMPLEADO
                       *> LEER DATOS DE MODIFICACIÓN
                       DISPLAY "INGRESE NUEVA INFORMACIÓN."
                       DISPLAY "(DEJAR EN BLANCO PARA NO CAMBIAR):"
                       DISPLAY "NUEVO APELLIDO: "
                           WITH NO ADVANCING
                       ACCEPT WS-APELLIDO.
                       DISPLAY "NUEVO NOMBRE: "
                           WITH NO ADVANCING
                       ACCEPT WS-NOMBRE
                       DISPLAY "NUEVO SEXO: "
                           WITH NO ADVANCING
                       ACCEPT WS-SEXO
                       DISPLAY "NUEVO TELF. FIJO: "
                           WITH NO ADVANCING
                       ACCEPT WS-TELF-FIJO.
                       DISPLAY "NUEVO TELF. CELULAR: "
                           WITH NO ADVANCING
                       ACCEPT WS-TELF-CELULAR.
                       DISPLAY "NUEVO EMAIL: "
                           WITH NO ADVANCING
                       ACCEPT WS-EMAIL
                       DISPLAY "NUEVA DIRECCIÓN: "
                           WITH NO ADVANCING
                       ACCEPT WS-DIRECCION
                       DISPLAY "NUEVO CONTACTO: "
                           WITH NO ADVANCING
                       ACCEPT WS-CONTACTO.
                       DISPLAY "NUEVO SALARIO: "
                           WITH NO ADVANCING
                       ACCEPT WS-MONTO-SALARIO.
                       DISPLAY "NUEVO PRÉSTAMO: "
                           WITH NO ADVANCING
                       ACCEPT WS-MONTO-PRESTAMO.
                       DISPLAY "NUEVO SSO: "
                           WITH NO ADVANCING
                       ACCEPT WS-MONTO-SSO.
                       *> REESCRITURA DE ARCHIVO
                       REWRITE REG-EMPLEADO FROM WS-EMPLEADO
                       END-REWRITE
                       DISPLAY "INFORMACIÓN DE EMPLEADO ACTUALIZADA."
                   ELSE
                       DISPLAY "ERROR AL LEER EL ARCHIVO."
                   END-IF
               ELSE
                   DISPLAY "EMPLEADO NO ENCONTRADO."
               END-IF
           ELSE
               DISPLAY "ERROR AL LEER EL ARCHIVO DE ÍNDICES."
           END-IF
           PERFORM 000-CIERRE-EMPLEADOS
       *>  -------------------------------------------------------------
       *>  RUTINA PARA ELIMINAR EL REGISTRO DE UN EMPLEADO
       *>  -------------------------------------------------------------
       001-ELIMINAR.
           DISPLAY " ".
           DISPLAY "ELIMINAR REGISTRO DE EMPLEADO.".
           DISPLAY "---------------------------------------".
           DISPLAY " ".
           DISPLAY "CÉDULA No: " WITH NO ADVANCING.
           ACCEPT WS-CEDULA.
           PERFORM 000-ABRIR-INDICES
           READ REG-INDICE
           IF FS-INDICE = "00"
               IF REG-INDICE.ID-EMPLEADO = WS-CEDULA
                   MOVE REG-INDICE.POSICION-REG TO WS-POSICION-ACTUAL
                   PERFORM 000-CIERRE-INDICES
                   PERFORM 000-ABRIR-EMPLEADOS
                   IF FS-EMPLEADOS = "00"
                       DISPLAY "EMPLEADO ENCONTRADO:"
                       DISPLAY "C.I.: " REG-EMPLEADO.CI-EMPLEADO
                       DISPLAY "NOMBRE: " REG-EMPLEADO.NOMBRE
                       DISPLAY "APELLIDO: " REG-EMPLEADO.APELLIDO
                       DISPLAY "ELIMINAR REGISTRO (S/N)"
                       ACCEPT WS-CONFIRMACION
                       IF WS-CONFIRMACION = "S" OR WS-CONFIRMACION = "s"
                           DELETE REG-EMPLEADO
                           DISPLAY "EMPLEADO ELIMINADO."
                       ELSE
                           DISPLAY "OPERACIÓN CANCELADA."
                       END-IF
                   ELSE
                       DISPLAY "NO SE PUDO LEER LA INFORMACIÓN."
                   END-IF
               ELSE
                   DISPLAY "EMPLEADO NO ENCONTRADO."
               END-IF
           ELSE
               DISPLAY "ERROR AL LEER EL ÍNDICE."
           END-IF
           PERFORM 000-CIERRE-EMPLEADOS
       *>  -------------------------------------------------------------
       *>  RUTINA PARA CONSULTAR LA INFORMACIÓN DE UN EMPLEADO
       *>  -------------------------------------------------------------
       001-CONSULTAR.
           DISPLAY " ".
           DISPLAY "EDITAR INFORMACIÓN DE EMPLEADO.".
           DISPLAY "---------------------------------------".
           DISPLAY " ".
           DISPLAY "CÉDULA No: " WITH NO ADVANCING.
           ACCEPT WS-CEDULA.
           PERFORM 000-ABRIR-INDICES
           READ REG-INDICE
           IF FS-INDICE = "00"
               IF REG-INDICE.ID-EMPLEADO = WS-CEDULA
                   MOVE REG-INDICE.POSICION-REG TO WS-POSICION-ACTUAL
                   PERFORM 000-CIERRE-INDICES
                   PERFORM 000-ABRIR-EMPLEADOS
                   READ F-EMPLEADOS AT WS-POSICION-ACTUAL
                   IF FS-EMPLEADOS = "00"
                       DISPLAY "DATOS DEL EMPLEADO:"
                       DISPLAY "APELLIDO: " REG-EMPLEADO.APELLIDO
                       DISPLAY "NOMBRE: " REG-EMPLEADO.NOMBRE
                       DISPLAY "CÉDULA: " REG-EMPLEADO.CI-EMPLEADO
                       DISPLAY "DIRECCIÓN: " REG-EMPLEADO.DIRECCION
                       DISPLAY "TELF. FIJO: " REG-EMPLEADO.TELF-FIJO
                       DISPLAY "CELULAR: " REG-EMPLEADO.TELF-CELULAR
                       DISPLAY "CONTACTO: " REG-EMPLEADO.CONTACTO
                       DISPLAY "CORREO: " REG-EMPLEADO.EMAIL
                   ELSE
                       DISPLAY "ERROR AL LEER EL ARCHIVO."
                   END-IF
               ELSE
                   DISPLAY "EMPLEADO NO ENCONTRADO."
               END-IF
           ELSE
               DISPLAY "ERROR AL LEER EL ARCHIVO DE ÍNDICES."
           END-IF
           PERFORM 000-CIERRE-EMPLEADOS
       *>  -------------------------------------------------------------
       *>  RUTINA PARA GENERAR EL INFORME MENSUAL DE EMPLEADOS
       *>  -------------------------------------------------------------
       001-INFORME.
           MOVE ZERO TO WS-EMPLEADO-EOF
           DISPLAY "INFORME CON TODOS LOS EMPLEADOS:"
           PERFORM 000-ABRIR-EMPLEADOS
           PERFORM UNTIL WS-EMPLEADOS-EOF
               READ F-EMPLEADOS
               IF FS-STATUS-EMPLEADO = "00"
                   DISPLAY "DATOS DEL EMPLEADO:"
                   DISPLAY "APELLIDO: " REG-EMPLEADO.APELLIDO
                   DISPLAY "NOMBRE: " REG-EMPLEADO.NOMBRE
                   DISPLAY "CÉDULA: " REG-EMPLEADO.CI-EMPLEADO
                   DISPLAY "SALARIO: " REG-EMPLEADO.MONTO-SALARIO
                   DISPLAY "PRÉSTAMOS: " REG-EMPLEADO.MONTO-PRESTAMO
                   DISPLAY "SSO: " REG-EMPLEADO.MONTO-SSO
                   DISPLAY "----------------------------------------"
               ELSE
                   SET WS-EMPLEADO-EOF TO TRUE
               END-IF
           END-PERFORM
           PERFORM 000-CIERRE-EMPLEADOS
       *>  -------------------------------------------------------------
       *>  APERTURA O CREACIÓN DE NO EXISTIR El ARCHIVO EMPLEADOS
       *>  -------------------------------------------------------------
       000-ABRIR-EMPLEADOS.
           MOVE ZERO TO FS-EMPLEADOS.
           OPEN I-O F-EMPLEADOS.
           IF FS-EMPLEADOS = '10' OR FS-EMPLEADOS = '00' THEN
               EXIT
           ELSE
               IF FS-EMPLEADOS = '35' THEN
                   OPEN OUTPUT F-ARCHIVO-CARTA
                   IF FS-EMPLEADOS = '10' OR 
                   FS-EMPLEADOS = '00' THEN
                       EXIT
                   ELSE
                       DISPLAY "ERROR AL CREAR EL ARCHIVO, ERROR: "
                       FS-EMPLEADOS
                   END-IF
               ELSE
                   DISPLAY "ERROR AL ABRIR O CREAR EL ARCHIVO, ERROR: "
                       FS-EMPLEADOS
                   MOVE 1 TO WS-FIN
               END-IF
           END-IF.
       *>  -------------------------------------------------------------
       *>  CIERRE DEL ARCHIVO EMPLEADOS
       *>  -------------------------------------------------------------
       000-CIERRE-EMPLEADOS.
           CLOSE F-EMPLEADOS.
       *>  -------------------------------------------------------------
       *>  APERTURA O CREACIÓN DE NO EXISTIR El ARCHIVO DE ÍNDICE
       *>  -------------------------------------------------------------
       000-ABRIR-INDICES.
           MOVE ZERO TO FS-INDICE.
           OPEN I-O F-INDICE.
           IF FS-INDICE = '10' OR FS-INDICE = '00' THEN
               EXIT
           ELSE
               IF FS-INDICE = '35' THEN
                   OPEN OUTPUT F-ARCHIVO-CARTA
                   IF FS-INDICE = '10' OR 
                   FS-INDICE = '00' THEN
                       EXIT
                   ELSE
                       DISPLAY "ERROR AL CREAR EL ARCHIVO, ERROR: "
                       FS-INDICE
                   END-IF
               ELSE
                   DISPLAY 'ERROR AL ABRIR O CREAR EL ARCHIVO, ERROR: '
                       FS-INDICE
                   MOVE 1 TO WS-FIN
               END-IF
           END-IF.
       *>  -------------------------------------------------------------
       *>  CIERRE DEL ARCHIVO EMPLEADOS
       *>  -------------------------------------------------------------
       000-CIERRE-INDICES.
           CLOSE F-INDICE.
       *>  -------------------------------------------------------------
       *>  APERTURA O CREACIÓN DE NO EXISTIR El ARCHIVO DESBORDAMIENTO
       *>  -------------------------------------------------------------
       000-ABRIR-DESBORDAMIENTO.
           MOVE ZERO TO FS-DESBORDAMIENTO.
           OPEN I-O F-DESBORDAMIENTO.
           IF FS-DESBORDAMIENTO = '10' OR
               FS-DESBORDAMIENTO = '00' THEN
               EXIT
           ELSE
               IF FS-DESBORDAMIENTO = '35' THEN
                   OPEN OUTPUT F-ARCHIVO-CARTA
                   IF FS-DESBORDAMIENTO = '10' OR 
                   FS-DESBORDAMIENTO = '00' THEN
                       EXIT
                   ELSE
                       DISPLAY "ERROR AL CREAR EL ARCHIVO, ERROR: "
                       FS-DESBORDAMIENTO
                   END-IF
               ELSE
                   DISPLAY 'ERROR AL ABRIR O CREAR EL ARCHIVO, ERROR: '
                       FS-DESBORDAMIENTO
                   MOVE 1 TO WS-FIN
               END-IF
           END-IF.
       *>  -------------------------------------------------------------
       *>  CIERRE DEL ARCHIVO EMPLEADOS
       *>  -------------------------------------------------------------
       000-CIERRE-DESBORDAMIENTO.
           CLOSE F-DESBORDAMIENTO.
       *>  -------------------------------------------------------------
       END PROGRAM EMPLEADO.
       *>  -------------------------------------------------------------
