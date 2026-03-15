! PIA
! 08/05/24
! Version Final
!--------------------------------------------------------------------------------------------

program main
    
    !Implicit none, significa que todas las variables se deben declarar, ninguna esta implicita.
    implicit none

    !Declaracion de variables y funciones
    !integer :: n ! Almacena el número de intervalos para el método de Runge-Kutta (Índice del ciclo)
    integer :: p ! Controla el ejemplo preseleccionado que hará el código
    !real*16, dimension (99) :: dsolve ! Almacena datos relevantes para el método de Runge-Kutta, como condiciones iniciales, intervalos, etcétera
    !double precision, dimension(7) :: constantes ! Almacena las constantes/parametros propios de los ejemplos de sistemas

    !Imprimir nombre del programa
    write(*,*) ""
    write(*,*) "----------------------------------------------------------------------------"
    write(*,*) "PIA: Fisica Computacional"
    write(*,*) "----------------------------------------------------------------------------"
    write(*,*) ""

    ! ----------------------------------------------------------------------------------------

    !Crear una carpeta para guardar ahí todos los generados
    call folder()

    !Abrir unidades de texto, para almacenar los puntos
    open(20041,file="generated/data/posicion_longitudinal.txt") ! Posición z
    open(20042,file="generated/data/velocidad_longitudinal.txt") ! Velocidad z
    open(20043,file="generated/data/posicion_angular.txt") ! Posición Ángular
    open(20044,file="generated/data/velocidad_angular.txt") ! Velocidad Ángular
    open(20045,file="generated/data/fase.txt") ! Fase de posiciones
    open(20046,file="generated/data/energias.txt") ! Energías

    ! Nota
    ! Se entiende que en el código:

    ! La variable z es y
    ! La variable \theta es x

    ! La variale z' es v
    ! La variable \theta' es b

    ! Cambiar la variable p para obtener otro set de constantes y condiciones inciales. P va de 1 hasta 14

    p=14
    call sets(p)



    !Cerrar unidades

        close(20041)
        close(20042)
        close(20043)
        close(20044)
        close(20045)
        close(20046)

    !-----------------

    write(*,*) "Programa finalizado (!)"
    write(*,*) ""

end program main

!----------------------------------------------------------------------------------------------------------------------

subroutine sets(p)

    ! Declarar variables
    integer :: p ! Controla el ejemplo preseleccionado que hará el código
    integer :: n ! Almacena el número de intervalos para el método de Runge-Kutta (Índice del ciclo)
    real*16, dimension (99) :: dsolve ! Almacena datos relevantes para el método de Runge-Kutta, como condiciones iniciales, intervalos, etcétera
    double precision, dimension(7) :: constantes ! Almacena las constantes/parametros propios de los ejemplos de sistemas

    ! Seleccionar valores propios del ejemplo

        select case (p)

        case (1)

        ! Set de constantes

            ! Constante longitudinal
            constantes(1)=0

            ! Constante torcional
            constantes(2)=2.2
        
            ! Constante de acoplamiento
            constantes(3)=0.05
        
            ! Masa
            constantes(4)=0.5
        
            ! Inercia
            constantes(5)=0.15
        
            ! Frecuencias iguales
            constantes(1)=constantes(4)*constantes(2)/constantes(5)

        !----------------------------------------------------------
        
        ! Condiciones Iniciales
        
            ! Tiempo inicial
            dsolve(1)=0.0
        
            ! Valor de la función Y (posición z) evaluado en el tiempo inicial
            dsolve(2)=0.1
        
            ! Valor de la derivada Y (velocidad z) de la funcion en el tiempo inicial
            dsolve(3)=0
        
            ! Valor de la función X (posición theta) evaluado en el tiempo inicial
            dsolve(6)=4*atan(1.0)
        
            ! Valor de la derivada X (velocidad theta) de la funcion en el tiempo inicial
            dsolve(7)=0
        
            ! Paso (tamaño)
            dsolve(4)=0.01d0
        
            ! Tiempo final
            dsolve(5)=100.0
        
        !----------------------------------------------------------

        case (2)

        ! Set de constantes

            ! Constante longitudinal
            constantes(1)=0

            ! Constante torcional
            constantes(2)=7
        
            ! Constante de acoplamiento
            constantes(3)=1.2
        
            ! Masa
            constantes(4)=10
        
            ! Inercia
            constantes(5)=0.9
        
            ! Frecuencias iguales
            constantes(1)=constantes(4)*constantes(2)/constantes(5)

        !----------------------------------------------------------
        
        ! Condiciones Iniciales
        
            ! Tiempo inicial
            dsolve(1)=0.0
        
            ! Valor de la función Y (posición z) evaluado en el tiempo inicial
            dsolve(2)=0.01
        
            ! Valor de la derivada Y (velocidad z) de la funcion en el tiempo inicial
            dsolve(3)=0
        
            ! Valor de la función X (posición theta) evaluado en el tiempo inicial
            dsolve(6)=2*atan(1.0)
        
            ! Valor de la derivada X (velocidad theta) de la funcion en el tiempo inicial
            dsolve(7)=0
        
            ! Paso (tamaño)
            dsolve(4)=0.01d0
        
            ! Tiempo final
            dsolve(5)=100.0
        
        !----------------------------------------------------------

        case (3)

        ! Set de constantes

            ! Constante longitudinal
            constantes(1)=0

            ! Constante torcional
            constantes(2)=5.4
        
            ! Constante de acoplamiento
            constantes(3)=0.3
        
            ! Masa
            constantes(4)=0.4
        
            ! Inercia
            constantes(5)=0.2
        
            ! Frecuencias iguales
            constantes(1)=constantes(4)*constantes(2)/constantes(5)

        !----------------------------------------------------------
        
        ! Condiciones Iniciales
        
            ! Tiempo inicial
            dsolve(1)=0.0
        
            ! Valor de la función Y (posición z) evaluado en el tiempo inicial
            dsolve(2)=0.15
        
            ! Valor de la derivada Y (velocidad z) de la funcion en el tiempo inicial
            dsolve(3)=0
        
            ! Valor de la función X (posición theta) evaluado en el tiempo inicial
            dsolve(6)=3*atan(1.0)
        
            ! Valor de la derivada X (velocidad theta) de la funcion en el tiempo inicial
            dsolve(7)=0
        
            ! Paso (tamaño)
            dsolve(4)=0.01d0
        
            ! Tiempo final
            dsolve(5)=10.0
        
        !----------------------------------------------------------

        case (4)
        
        ! Set de constantes

            ! Constante longitudinal
            constantes(1)=0

            ! Constante torcional
            constantes(2)=0.4
        
            ! Constante de acoplamiento
            constantes(3)=0.03
        
            ! Masa
            constantes(4)=0.1
        
            ! Inercia
            constantes(5)=0.02
        
            ! Frecuencias iguales
            constantes(1)=constantes(4)*constantes(2)/constantes(5)

        !----------------------------------------------------------
        
        ! Condiciones Iniciales
        
            ! Tiempo inicial
            dsolve(1)=0.0
        
            ! Valor de la función Y (posición z) evaluado en el tiempo inicial
            dsolve(2)=0.1
        
            ! Valor de la derivada Y (velocidad z) de la funcion en el tiempo inicial
            dsolve(3)=0
        
            ! Valor de la función X (posición theta) evaluado en el tiempo inicial
            dsolve(6)=atan(1.0)
        
            ! Valor de la derivada X (velocidad theta) de la funcion en el tiempo inicial
            dsolve(7)=0
        
            ! Paso (tamaño)
            dsolve(4)=0.01d0
        
            ! Tiempo final
            dsolve(5)=100.0
        
        !----------------------------------------------------------

        case (5)

        ! Set de constantes

            ! Constante longitudinal
            constantes(1)=0

            ! Constante torcional
            constantes(2)=0.9
        
            ! Constante de acoplamiento
            constantes(3)=0.6
        
            ! Masa
            constantes(4)=1
        
            ! Inercia
            constantes(5)=0.1
        
            ! Frecuencias iguales
            constantes(1)=constantes(4)*constantes(2)/constantes(5)

        !----------------------------------------------------------
        
        ! Condiciones Iniciales
        
            ! Tiempo inicial
            dsolve(1)=0.0
        
            ! Valor de la función Y (posición z) evaluado en el tiempo inicial
            dsolve(2)=0.2
        
            ! Valor de la derivada Y (velocidad z) de la funcion en el tiempo inicial
            dsolve(3)=0
        
            ! Valor de la función X (posición theta) evaluado en el tiempo inicial
            dsolve(6)=0
        
            ! Valor de la derivada X (velocidad theta) de la funcion en el tiempo inicial
            dsolve(7)=0
        
            ! Paso (tamaño)
            dsolve(4)=0.01d0
        
            ! Tiempo final
            dsolve(5)=100.0
        
        !----------------------------------------------------------

        case (6)

        ! Set de constantes

            ! Constante longitudinal
            constantes(1)=0

            ! Constante torcional
            constantes(2)=2.3
        
            ! Constante de acoplamiento
            constantes(3)=1.6
        
            ! Masa
            constantes(4)=0.1
        
            ! Inercia
            constantes(5)=0.8
        
            ! Frecuencias iguales
            constantes(1)=constantes(4)*constantes(2)/constantes(5)

        !----------------------------------------------------------
        
        ! Condiciones Iniciales
        
            ! Tiempo inicial
            dsolve(1)=0.0
        
            ! Valor de la función Y (posición z) evaluado en el tiempo inicial
            dsolve(2)=0.08
        
            ! Valor de la derivada Y (velocidad z) de la funcion en el tiempo inicial
            dsolve(3)=0
        
            ! Valor de la función X (posición theta) evaluado en el tiempo inicial
            dsolve(6)=0
        
            ! Valor de la derivada X (velocidad theta) de la funcion en el tiempo inicial
            dsolve(7)=0
        
            ! Paso (tamaño)
            dsolve(4)=0.01d0
        
            ! Tiempo final
            dsolve(5)=100.0
        
        !----------------------------------------------------------

        case (7)

        ! Set de constantes

            ! Constante longitudinal
            constantes(1)=0

            ! Constante torcional
            constantes(2)=2.3
        
            ! Constante de acoplamiento
            constantes(3)=0.8
        
            ! Masa
            constantes(4)=0.1
        
            ! Inercia
            constantes(5)=0.8
        
            ! Frecuencias iguales
            constantes(1)=constantes(4)*constantes(2)/constantes(5)

        !----------------------------------------------------------
        
        ! Condiciones Iniciales
        
            ! Tiempo inicial
            dsolve(1)=0.0
        
            ! Valor de la función Y (posición z) evaluado en el tiempo inicial
            dsolve(2)=0.08
        
            ! Valor de la derivada Y (velocidad z) de la funcion en el tiempo inicial
            dsolve(3)=0
        
            ! Valor de la función X (posición theta) evaluado en el tiempo inicial
            dsolve(6)=0
        
            ! Valor de la derivada X (velocidad theta) de la funcion en el tiempo inicial
            dsolve(7)=0
        
            ! Paso (tamaño)
            dsolve(4)=0.01d0
        
            ! Tiempo final
            dsolve(5)=100.0
        
        !----------------------------------------------------------

        case (8)

        ! Set de constantes

            ! Constante longitudinal
            constantes(1)=0

            ! Constante torcional
            constantes(2)=2.3
        
            ! Constante de acoplamiento
            constantes(3)=0.2
        
            ! Masa
            constantes(4)=0.1
        
            ! Inercia
            constantes(5)=0.8
        
            ! Frecuencias iguales
            constantes(1)=constantes(4)*constantes(2)/constantes(5)

        !----------------------------------------------------------
        
        ! Condiciones Iniciales
        
            ! Tiempo inicial
            dsolve(1)=0.0
        
            ! Valor de la función Y (posición z) evaluado en el tiempo inicial
            dsolve(2)=0.08
        
            ! Valor de la derivada Y (velocidad z) de la funcion en el tiempo inicial
            dsolve(3)=0
        
            ! Valor de la función X (posición theta) evaluado en el tiempo inicial
            dsolve(6)=0
        
            ! Valor de la derivada X (velocidad theta) de la funcion en el tiempo inicial
            dsolve(7)=0
        
            ! Paso (tamaño)
            dsolve(4)=0.01d0
        
            ! Tiempo final
            dsolve(5)=100.0
        
        !----------------------------------------------------------

        case (9)

        ! Set de constantes

            ! Constante longitudinal
            constantes(1)=0

            ! Constante torcional
            constantes(2)=2.3
        
            ! Constante de acoplamiento
            constantes(3)=0.05
        
            ! Masa
            constantes(4)=0.1
        
            ! Inercia
            constantes(5)=0.8
        
            ! Frecuencias iguales
            constantes(1)=constantes(4)*constantes(2)/constantes(5)

        !----------------------------------------------------------
        
        ! Condiciones Iniciales
        
            ! Tiempo inicial
            dsolve(1)=0.0
        
            ! Valor de la función Y (posición z) evaluado en el tiempo inicial
            dsolve(2)=0.08
        
            ! Valor de la derivada Y (velocidad z) de la funcion en el tiempo inicial
            dsolve(3)=0
        
            ! Valor de la función X (posición theta) evaluado en el tiempo inicial
            dsolve(6)=0
        
            ! Valor de la derivada X (velocidad theta) de la funcion en el tiempo inicial
            dsolve(7)=0
        
            ! Paso (tamaño)
            dsolve(4)=0.01d0
        
            ! Tiempo final
            dsolve(5)=100.0
        
        !----------------------------------------------------------

        case (10)

        ! Set de constantes

            ! Constante longitudinal
            constantes(1)=0

            ! Constante torcional
            constantes(2)=2.3
        
            ! Constante de acoplamiento
            constantes(3)=0.01
        
            ! Masa
            constantes(4)=0.1
        
            ! Inercia
            constantes(5)=0.8
        
            ! Frecuencias iguales
            constantes(1)=constantes(4)*constantes(2)/constantes(5)

        !----------------------------------------------------------
        
        ! Condiciones Iniciales
        
            ! Tiempo inicial
            dsolve(1)=0.0
        
            ! Valor de la función Y (posición z) evaluado en el tiempo inicial
            dsolve(2)=0.08
        
            ! Valor de la derivada Y (velocidad z) de la funcion en el tiempo inicial
            dsolve(3)=0
        
            ! Valor de la función X (posición theta) evaluado en el tiempo inicial
            dsolve(6)=0
        
            ! Valor de la derivada X (velocidad theta) de la funcion en el tiempo inicial
            dsolve(7)=0
        
            ! Paso (tamaño)
            dsolve(4)=0.01d0
        
            ! Tiempo final
            dsolve(5)=100.0
        
        !----------------------------------------------------------

        case (11)

        ! Set de constantes

            ! Constante longitudinal
            constantes(1)=0

            ! Constante torcional
            constantes(2)=1.6
        
            ! Constante de acoplamiento
            constantes(3)=0.075
        
            ! Masa
            constantes(4)=0.07
        
            ! Inercia
            constantes(5)=0.5
        
            ! Frecuencias iguales
            constantes(1)=constantes(4)*constantes(2)/constantes(5)

        !----------------------------------------------------------
        
        ! Condiciones Iniciales
        
            ! Tiempo inicial
            dsolve(1)=0.0
        
            ! Valor de la función Y (posición z) evaluado en el tiempo inicial
            dsolve(2)=1.0
        
            ! Valor de la derivada Y (velocidad z) de la funcion en el tiempo inicial
            dsolve(3)=0
        
            ! Valor de la función X (posición theta) evaluado en el tiempo inicial
            dsolve(6)=0
        
            ! Valor de la derivada X (velocidad theta) de la funcion en el tiempo inicial
            dsolve(7)=0
        
            ! Paso (tamaño)
            dsolve(4)=0.01d0
        
            ! Tiempo final
            dsolve(5)=100.0
        
        !----------------------------------------------------------

        case (12)

        ! Set de constantes

            ! Constante longitudinal
            constantes(1)=0

            ! Constante torcional
            constantes(2)=1.6
        
            ! Constante de acoplamiento
            constantes(3)=0.075
        
            ! Masa
            constantes(4)=0.07
        
            ! Inercia
            constantes(5)=0.5
        
            ! Frecuencias iguales
            constantes(1)=constantes(4)*constantes(2)/constantes(5)

        !----------------------------------------------------------
        
        ! Condiciones Iniciales
        
            ! Tiempo inicial
            dsolve(1)=0.0
        
            ! Valor de la función Y (posición z) evaluado en el tiempo inicial
            dsolve(2)=1.0
        
            ! Valor de la derivada Y (velocidad z) de la funcion en el tiempo inicial
            dsolve(3)=0
        
            ! Valor de la función X (posición theta) evaluado en el tiempo inicial
            dsolve(6)=0.374165738
        
            ! Valor de la derivada X (velocidad theta) de la funcion en el tiempo inicial
            dsolve(7)=0
        
            ! Paso (tamaño)
            dsolve(4)=0.01d0
        
            ! Tiempo final
            dsolve(5)=100.0
        
        !----------------------------------------------------------

        case (13)

            ! Set de constantes
    
                ! Constante longitudinal
                constantes(1)=0
    
                ! Constante torcional
                constantes(2)=1.6
            
                ! Constante de acoplamiento
                constantes(3)=0.075
            
                ! Masa
                constantes(4)=0.07
            
                ! Inercia
                constantes(5)=0.5
            
                ! Frecuencias iguales
                constantes(1)=constantes(4)*constantes(2)/constantes(5)
    
            !----------------------------------------------------------
            
            ! Condiciones Iniciales
            
                ! Tiempo inicial
                dsolve(1)=0.0
            
                ! Valor de la función Y (posición z) evaluado en el tiempo inicial
                dsolve(2)=1.0
            
                ! Valor de la derivada Y (velocidad z) de la funcion en el tiempo inicial
                dsolve(3)=0
            
                ! Valor de la función X (posición theta) evaluado en el tiempo inicial
                dsolve(6)=0.374165738
            
                ! Valor de la derivada X (velocidad theta) de la funcion en el tiempo inicial
                dsolve(7)=0
            
                ! Paso (tamaño)
                dsolve(4)=0.001d0
            
                ! Tiempo final
                dsolve(5)=5.0
            
            !----------------------------------------------------------
    

        case (14)

            ! Set de constantes
    
                ! Constante longitudinal
                constantes(1)=0
    
                ! Constante torcional
                constantes(2)=7.44288e-4
            
                ! Constante de acoplamiento
                constantes(3)=9.27e-3
            
                ! Masa
                constantes(4)=0.4905
            
                ! Inercia
                constantes(5)=1.39e-4
            
                ! Frecuencias iguales
                constantes(1)=constantes(4)*constantes(2)/constantes(5)
    
            !----------------------------------------------------------
            
            ! Condiciones Iniciales
            
                ! Tiempo inicial
                dsolve(1)=0.0
            
                ! Valor de la función Y (posición z) evaluado en el tiempo inicial
                dsolve(2)=0
            
                ! Valor de la derivada Y (velocidad z) de la funcion en el tiempo inicial
                dsolve(3)=0
            
                ! Valor de la función X (posición theta) evaluado en el tiempo inicial
                dsolve(6)=2*4*atan(1.0)
            
                ! Valor de la derivada X (velocidad theta) de la funcion en el tiempo inicial
                dsolve(7)=0
            
                ! Paso (tamaño)
                dsolve(4)=0.01d0
            
                ! Tiempo final
                dsolve(5)=40.0
            
            !----------------------------------------------------------

        case default

            F=0

        end select
    !---------------------------------------------------------------------------


    ! Calculos necesarios

        ! Guardar los valores iniciales en la lista de constantes
        constantes(6)=real(dsolve(2),8)
        constantes(7)=real(dsolve(6),8)

        ! Calcular el numero de intervalos con base al paso y al valor de la función a la que quiero llegar
        n=int((dsolve(5)-dsolve(1))/dsolve(4))+1

        write(*,*)

        ! Aplicar Runge-Kutta
        call RK(dsolve,n,constantes)

        !---------------------------------------------------------------------------

        ! Gráficar en Gnuplot

            call graph_longitudinal(constantes) ! Resolución gráfica numérica para el apartado longitudinal
            call graph_angular(constantes) ! Resolución gráfica numérica para el apartado angular
            call graph_phase(constantes) ! Resolución gráfica numérica para el apartado de fase de posiciones
            call graph_analytic_longitudinal(constantes) ! Resolución gráfica analítica para el apartado longitudinal
            call graph_analytic_angular(constantes) ! Resolución gráfica analítica para el apartado ángular
            call graph_analytic_phase(constantes) ! Resolución gráfica analítica para el apartado de fase de posiciones
            call graph_energias(constantes) ! Resolución gráfica numérica para el apartado de energias

    !---------------------------------------------------------------------------

end subroutine

!Subrutina: Crear una carpeta, para organizar todas las gráficas, archivos de datos, y scripts de gnuplot
subroutine folder()

    ! Crear subcarpeta que almacena las gráficas
    open(20030,file="folder.bat") !Para crear una carpeta
    write(20030,*) "@echo off"
    write(20030,*) "cd /d %~dp0"
    write(20030,*) "mkdir generated\graficas"
    call system('folder.bat')

    ! Crear subcarpeta que almacena los scripts .gpl
    open(20031,file="folder.bat") !Para crear una carpeta
    write(20031,*) "@echo off"
    write(20031,*) "cd /d %~dp0"
    write(20031,*) "mkdir generated\gpl"
    call system('folder.bat')

    ! Crear subcarpeta que almacena los archivos de datos
    open(20032,file="folder.bat") !Para crear una carpeta
    write(20032,*) "@echo off"
    write(20032,*) "cd /d %~dp0"
    write(20032,*) "mkdir generated\data"
    call system('folder.bat')

    ! Cerrar unidades
    close(20032)
    close(20031)
    close(20030)

end subroutine

! Subrutinas generadoras de scripts de Gnuplot

    !Subrutina: Resolución gráfica analítica para el apartado ángular
    subroutine graph_analytic_angular(constantes)

        ! Importar funciones
        character(len=50), external :: strd_mod

        ! Declarar variables
        double precision, dimension(7) :: constantes

        !Configuraciones    
        open(20241,file="generated/gpl/analytic_angular.gpl") !Crear script para graficar

        !Configuración del gráfico
            write(20241,'(A)') "# Configuración del gráfico"
            write(20241,'(A)') "set terminal pngcairo size 2000,1200 enhanced font 'Verdana,22'"
            write(20241,'(A)') "set output 'generated/graficas/analytic_angular.png'"
            write(20241,'(A)')
        !----------------------------

        !Titulos y etiquetas
            write(20241,'(A)') "# Títulos y etiquetas"
            write(20241,'(A)') "set title '[Método Analítico] Ángular vs Tiempo'"
            write(20241,'(A)') "set xlabel 'Tiempo [s]'"
            write(20241,'(A)') "set ylabel 'Posición[rad] Velocidad[rad/s]'"
            write(20241,'(A)') "set key outside bottom right"
            write(20241,'(A)') 
        !---------------------------

        !Mostrar parámetros
            write(20241,'(A)') "set label 1 'Parámetros' at screen 0.88,0.9 left font 'Verdana,24' front"
            write(20241,'(A)') "set label 2 'k="//trim(adjustl(strd_mod(constantes(1))))//"'&
            & at screen 0.88,0.85 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 3 'δ="//trim(adjustl(strd_mod(constantes(2))))//"'&
            & at screen 0.88,0.8 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 4 'ε="//trim(adjustl(strd_mod(constantes(3))))//"'&
            & at screen 0.88,0.75 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 5 'm="//trim(adjustl(strd_mod(constantes(4))))//"'&
            & at screen 0.88,0.7 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 6 'I="//trim(adjustl(strd_mod(constantes(5))))//"'&
            & at screen 0.88,0.65 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 7 'z(0)="//trim(adjustl(strd_mod(constantes(6))))//"'&
            & at screen 0.88,0.6 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 8 'θ(0)="//trim(adjustl(strd_mod(constantes(7))))//"'&
            & at screen 0.88,0.55 left font 'Verdana,22' front"
            write(20241,'(A)') "show label"
            write(20241,'(A)') 
        !---------------------------

        !Configurar los límites de graficación
            write(20241,'(A)') "# Adecuar los limites"
            write(20241,'(A)') "set grid lw 4 lc rgb '#808080'"
            write(20241,'(A)') 
        !-------------------------------------

        !Graficar datos
        write(20241,'(A)') "# Graficar datos"

        !Funcion

            !Linea fantasma
            write(20241,'(A)') "plot 0 with lines lw 2 lt rgb '#FFFFFF00' notitle, \"

            ! Gráficar posición obtenida
            write(20241,'(A)') "     'generated/data/posicion_angular.txt' u 1:3 w lp&
            & ps 0.5 pt 7 lc rgb'#4A4954' title 'Posición', \"

            ! Gráficar velocidad obtenida
            write(20241,'(A)') "     'generated/data/velocidad_angular.txt' u 1:3 w lp&
            & ps 0.5 pt 7 lc rgb'#B0ADC7' title 'Velocidad', \"

            !Eje x
            write(20241,'(A)') "     0 with lines lw 2 lt rgb 'black' notitle behind"
        
        !-----------------
        
        close(20241) !Cerrar script
        
        !Correr el script generador de la grafica
        call system('start gnuplot -p generated/gpl/analytic_angular.gpl')

    end subroutine

    !Subrutina: Resolución gráfica analítica para el apartado longitudinal
    subroutine graph_analytic_longitudinal(constantes)

        ! Importar funciones
        character(len=50), external :: strd_mod

        ! Declarar variables
        double precision, dimension(7) :: constantes

        !Configuraciones    
        open(20241,file="generated/gpl/analytic_longitudinal.gpl") !Crear script para graficar

        !Configuración del gráfico
            write(20241,'(A)') "# Configuración del gráfico"
            write(20241,'(A)') "set terminal pngcairo size 2000,1200 enhanced font 'Verdana,22'"
            write(20241,'(A)') "set output 'generated/graficas/analytic_longitudinal.png'"
            write(20241,'(A)')
        !----------------------------

        !Titulos y etiquetas
            write(20241,'(A)') "# Títulos y etiquetas"
            write(20241,'(A)') "set title '[Método Analítico] Longitudinal vs Tiempo'"
            write(20241,'(A)') "set xlabel 'Tiempo [s]'"
            write(20241,'(A)') "set ylabel 'Posición[m] Velocidad[m/s]'"
            write(20241,'(A)') "set key outside bottom right"
            write(20241,'(A)') 
        !---------------------------

        !Mostrar parámetros
            write(20241,'(A)') "set label 1 'Parámetros' at screen 0.88,0.9 left font 'Verdana,24' front"
            write(20241,'(A)') "set label 2 'k="//trim(adjustl(strd_mod(constantes(1))))//"'&
            & at screen 0.88,0.85 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 3 'δ="//trim(adjustl(strd_mod(constantes(2))))//"'&
            & at screen 0.88,0.8 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 4 'ε="//trim(adjustl(strd_mod(constantes(3))))//"'&
            & at screen 0.88,0.75 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 5 'm="//trim(adjustl(strd_mod(constantes(4))))//"'&
            & at screen 0.88,0.7 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 6 'I="//trim(adjustl(strd_mod(constantes(5))))//"'&
            & at screen 0.88,0.65 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 7 'z(0)="//trim(adjustl(strd_mod(constantes(6))))//"'&
            & at screen 0.88,0.6 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 8 'θ(0)="//trim(adjustl(strd_mod(constantes(7))))//"'&
            & at screen 0.88,0.55 left font 'Verdana,22' front"
            write(20241,'(A)') "show label"
            write(20241,'(A)') 
        !---------------------------

        !Configurar los límites de graficación
            write(20241,'(A)') "# Adecuar los limites"
            write(20241,'(A)') "set grid lw 4 lc rgb '#808080'"
            write(20241,'(A)') 
        !-------------------------------------

        !Graficar datos
        write(20241,'(A)') "# Graficar datos"

        !Funcion

            !Linea fantasma
            write(20241,'(A)') "plot 0 with lines lw 2 lt rgb '#FFFFFF00' notitle, \"

            ! Gráficar posición obtenida
            write(20241,'(A)') "     'generated/data/posicion_longitudinal.txt' u 1:3 w lp&
            & ps 0.5 pt 7 lc rgb'#4A4954' title 'Posición', \"

            ! Gráficar velocidad obtenida
            write(20241,'(A)') "     'generated/data/velocidad_longitudinal.txt' u 1:3 w lp&
            & ps 0.5 pt 7 lc rgb'#B0ADC7' title 'Velocidad', \"

            !Eje x
            write(20241,'(A)') "     0 with lines lw 2 lt rgb 'black' notitle behind"
        
        !-----------------
        
        close(20241) !Cerrar script
        
        !Correr el script generador de la grafica
        call system('start gnuplot -p generated/gpl/analytic_longitudinal.gpl')

    end subroutine

    !Subrutina: Resolución gráfica numérica para el apartado longitudinal
    subroutine graph_longitudinal(constantes)

        ! Importar funciones
        character(len=50), external :: strd_mod

        ! Declarar variables
        double precision, dimension(7) :: constantes

        !Configuraciones    
        open(20241,file="generated/gpl/longitudinal.gpl") !Crear script para graficar

        !Configuración del gráfico
            write(20241,'(A)') "# Configuración del gráfico"
            write(20241,'(A)') "set terminal pngcairo size 2000,1200 enhanced font 'Verdana,22'"
            write(20241,'(A)') "set output 'generated/graficas/numerical_longitudinal.png'"
            write(20241,'(A)')
        !----------------------------

        !Titulos y etiquetas
            write(20241,'(A)') "# Títulos y etiquetas"
            write(20241,'(A)') "set title '[Método Numérico] Longitudinal vs Tiempo'"
            write(20241,'(A)') "set xlabel 'Tiempo [s]'"
            write(20241,'(A)') "set ylabel 'Posición[m] Velocidad[m/s]'"
            write(20241,'(A)') "set key outside bottom right"
            write(20241,'(A)') 
        !---------------------------

        !Mostrar parámetros
            write(20241,'(A)') "set label 1 'Parámetros' at screen 0.88,0.9 left font 'Verdana,24' front"
            write(20241,'(A)') "set label 2 'k="//trim(adjustl(strd_mod(constantes(1))))//"'&
            & at screen 0.88,0.85 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 3 'δ="//trim(adjustl(strd_mod(constantes(2))))//"'&
            & at screen 0.88,0.8 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 4 'ε="//trim(adjustl(strd_mod(constantes(3))))//"'&
            & at screen 0.88,0.75 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 5 'm="//trim(adjustl(strd_mod(constantes(4))))//"'&
            & at screen 0.88,0.7 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 6 'I="//trim(adjustl(strd_mod(constantes(5))))//"'&
            & at screen 0.88,0.65 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 7 'z(0)="//trim(adjustl(strd_mod(constantes(6))))//"'&
            & at screen 0.88,0.6 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 8 'θ(0)="//trim(adjustl(strd_mod(constantes(7))))//"'&
            & at screen 0.88,0.55 left font 'Verdana,22' front"
            write(20241,'(A)') "show label"
            write(20241,'(A)') 
        !---------------------------

        !Configurar los límites de graficación
            write(20241,'(A)') "# Adecuar los limites"
            write(20241,'(A)') "set grid lw 4 lc rgb '#808080'"
            write(20241,'(A)') 
        !-------------------------------------

        !Graficar datos
        write(20241,'(A)') "# Graficar datos"

        !Funcion

            !Linea fantasma
            write(20241,'(A)') "plot 0 with lines lw 2 lt rgb '#FFFFFF00' notitle, \"

            ! Gráficar posición obtenida
            write(20241,'(A)') "     'generated/data/posicion_longitudinal.txt' u 1:2 w lp&
            & ps 0.5 pt 7 lc rgb'#4778FF' title 'Posición', \"

            ! Gráficar velocidad obtenida
            write(20241,'(A)') "     'generated/data/velocidad_longitudinal.txt' u 1:2 w lp&
            & ps 0.5 pt 7 lc rgb'#A8BFFF' title 'Velocidad', \"

            !Eje x
            write(20241,'(A)') "     0 with lines lw 2 lt rgb 'black' notitle behind"
        
        !-----------------
        
        close(20241) !Cerrar script
        
        !Correr el script generador de la grafica
        call system('start gnuplot -p generated/gpl/longitudinal.gpl')

    end subroutine

    !Subrutina: Resolución gráfica numérica para el apartado ángular
    subroutine graph_angular(constantes)

        ! Importar funciones
        character(len=50), external :: strd_mod

        ! Declarar variables
        double precision, dimension(7) :: constantes

        !Configuraciones    
        open(20241,file="generated/gpl/angular.gpl") !Crear script para graficar

        !Configuración del gráfico
            write(20241,'(A)') "# Configuración del gráfico"
            write(20241,'(A)') "set terminal pngcairo size 2000,1200 enhanced font 'Verdana,22'"
            write(20241,'(A)') "set output 'generated/graficas/numerical_angular.png'"
            write(20241,'(A)')
        !----------------------------

        !Titulos y etiquetas
            write(20241,'(A)') "# Títulos y etiquetas"
            write(20241,'(A)') "set title '[Método Numérico] Angular vs Tiempo'"
            write(20241,'(A)') "set xlabel 'Tiempo [s]'"
            write(20241,'(A)') "set ylabel 'Posición[rad] Velocidad[rad/s]'"
            write(20241,'(A)') "set key outside bottom right"
            write(20241,'(A)') 
        !---------------------------

        !Mostrar parámetros
            write(20241,'(A)') "set label 1 'Parámetros' at screen 0.88,0.9 left font 'Verdana,24' front"
            write(20241,'(A)') "set label 2 'k="//trim(adjustl(strd_mod(constantes(1))))//"'&
            & at screen 0.88,0.85 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 3 'δ="//trim(adjustl(strd_mod(constantes(2))))//"'&
            & at screen 0.88,0.8 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 4 'ε="//trim(adjustl(strd_mod(constantes(3))))//"'&
            & at screen 0.88,0.75 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 5 'm="//trim(adjustl(strd_mod(constantes(4))))//"'&
            & at screen 0.88,0.7 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 6 'I="//trim(adjustl(strd_mod(constantes(5))))//"'&
            & at screen 0.88,0.65 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 7 'z(0)="//trim(adjustl(strd_mod(constantes(6))))//"'&
            & at screen 0.88,0.6 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 8 'θ(0)="//trim(adjustl(strd_mod(constantes(7))))//"'&
            & at screen 0.88,0.55 left font 'Verdana,22' front"
            write(20241,'(A)') "show label"
            write(20241,'(A)') 
        !---------------------------


        !Configurar los límites de graficación
            write(20241,'(A)') "# Adecuar los limites"
            write(20241,'(A)') "set grid lw 4 lc rgb '#808080'"
            write(20241,'(A)') 
        !-------------------------------------

        !Graficar datos
        write(20241,'(A)') "# Graficar datos"

        !Funcion

            !Linea fantasma
            write(20241,'(A)') "plot 0 with lines lw 2 lt rgb '#FFFFFF00' notitle, \"

            ! Gráficar posición obtenida
            write(20241,'(A)') "     'generated/data/posicion_angular.txt' u 1:2 w lp&
            & ps 0.5 pt 7 lc rgb'#7645FF' title 'Posición', \"

            ! Gráficar velocidad obtenida
            write(20241,'(A)') "     'generated/data/velocidad_angular.txt' u 1:2 w lp&
            & ps 0.5 pt 7 lc rgb'#BDA8FF' title 'Velocidad', \"

            !Eje x
            write(20241,'(A)') "     0 with lines lw 2 lt rgb 'black' notitle behind"
        
        !-----------------
        
        close(20241) !Cerrar script
        
        !Correr el script generador de la grafica
        call system('start gnuplot -p generated/gpl/angular.gpl')

    end subroutine

    !Subrutina: Resolución gráfica analítica para el apartado de fase de posiciones
    subroutine graph_analytic_phase(constantes)

        ! Importar funciones
        character(len=50), external :: strd_mod

        ! Declarar variables
        double precision, dimension(7) :: constantes

        !Configuraciones    
        open(20241,file="generated/gpl/analytic_fase.gpl") !Crear script para graficar

        !Configuración del gráfico
            write(20241,'(A)') "# Configuración del gráfico"
            write(20241,'(A)') "set terminal pngcairo size 2000,1200 enhanced font 'Verdana,22'"
            write(20241,'(A)') "set output 'generated/graficas/analytic_fase.png'"
            write(20241,'(A)')
        !----------------------------

        !Titulos y etiquetas
            write(20241,'(A)') "# Títulos y etiquetas"
            write(20241,'(A)') "set title '[Método Analítico] Longitudinal vs Angular'"
            write(20241,'(A)') "set xlabel 'Posición Angular [rad]'"
            write(20241,'(A)') "set ylabel 'Posición longitudinal [m]'"
            write(20241,'(A)') "set key outside bottom right"
            write(20241,'(A)') 
        !---------------------------

        !Mostrar parámetros
            write(20241,'(A)') "set label 1 'Parámetros' at screen 0.88,0.9 left font 'Verdana,24' front"
            write(20241,'(A)') "set label 2 'k="//trim(adjustl(strd_mod(constantes(1))))//"'&
            & at screen 0.88,0.85 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 3 'δ="//trim(adjustl(strd_mod(constantes(2))))//"'&
            & at screen 0.88,0.8 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 4 'ε="//trim(adjustl(strd_mod(constantes(3))))//"'&
            & at screen 0.88,0.75 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 5 'm="//trim(adjustl(strd_mod(constantes(4))))//"'&
            & at screen 0.88,0.7 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 6 'I="//trim(adjustl(strd_mod(constantes(5))))//"'&
            & at screen 0.88,0.65 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 7 'z(0)="//trim(adjustl(strd_mod(constantes(6))))//"'&
            & at screen 0.88,0.6 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 8 'θ(0)="//trim(adjustl(strd_mod(constantes(7))))//"'&
            & at screen 0.88,0.55 left font 'Verdana,22' front"
            write(20241,'(A)') "show label"
            write(20241,'(A)') 
        !---------------------------

        !Configurar los límites de graficación
            write(20241,'(A)') "# Adecuar los limites"
            write(20241,'(A)') "set grid lw 4 lc rgb '#808080'"
            write(20241,'(A)') 
        !-------------------------------------

        !Graficar datos
        write(20241,'(A)') "# Graficar datos"

        !Funcion

            !Linea fantasma
            write(20241,'(A)') "plot 0 with lines lw 2 lt rgb '#FFFFFF00' notitle, \"

            ! Gráficar fase obtenida
            write(20241,'(A)') "     'generated/data/fase.txt' u 3:4 w lp&
            & ps 0.5 pt 7 lc rgb'#B0ADC7' title 'Fase', \"

            !Eje x
            write(20241,'(A)') "     0 with lines lw 2 lt rgb 'black' notitle behind"
        
        !-----------------
        
        close(20241) !Cerrar script
        
        !Correr el script generador de la grafica
        call system('start gnuplot -p generated/gpl/analytic_fase.gpl')

    end subroutine

    !Subrutina: Resolución gráfica numérica para el apartado de fase de posiciones
    subroutine graph_phase(constantes)

        ! Importar funciones
        character(len=50), external :: strd_mod

        ! Declarar variables
        double precision, dimension(7) :: constantes

        !Configuraciones    
        open(20241,file="generated/gpl/fase.gpl") !Crear script para graficar

        !Configuración del gráfico
            write(20241,'(A)') "# Configuración del gráfico"
            write(20241,'(A)') "set terminal pngcairo size 2000,1200 enhanced font 'Verdana,22'"
            write(20241,'(A)') "set output 'generated/graficas/numerical_fase.png'"
            write(20241,'(A)')
        !----------------------------

        !Titulos y etiquetas
            write(20241,'(A)') "# Títulos y etiquetas"
            write(20241,'(A)') "set title '[Método Numérico] Longitudinal vs Angular'"
            write(20241,'(A)') "set xlabel 'Posición Angular [rad]'"
            write(20241,'(A)') "set ylabel 'Posición longitudinal [m]'"
            write(20241,'(A)') "set key outside bottom right"
            write(20241,'(A)') 
        !---------------------------

        !Mostrar parámetros
            write(20241,'(A)') "set label 1 'Parámetros' at screen 0.88,0.9 left font 'Verdana,24' front"
            write(20241,'(A)') "set label 2 'k="//trim(adjustl(strd_mod(constantes(1))))//"'&
            & at screen 0.88,0.85 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 3 'δ="//trim(adjustl(strd_mod(constantes(2))))//"'&
            & at screen 0.88,0.8 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 4 'ε="//trim(adjustl(strd_mod(constantes(3))))//"'&
            & at screen 0.88,0.75 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 5 'm="//trim(adjustl(strd_mod(constantes(4))))//"'&
            & at screen 0.88,0.7 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 6 'I="//trim(adjustl(strd_mod(constantes(5))))//"'&
            & at screen 0.88,0.65 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 7 'z(0)="//trim(adjustl(strd_mod(constantes(6))))//"'&
            & at screen 0.88,0.6 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 8 'θ(0)="//trim(adjustl(strd_mod(constantes(7))))//"'&
            & at screen 0.88,0.55 left font 'Verdana,22' front"
            write(20241,'(A)') "show label"
            write(20241,'(A)') 
        !---------------------------

        !Configurar los límites de graficación
            write(20241,'(A)') "# Adecuar los limites"
            write(20241,'(A)') "set grid lw 4 lc rgb '#808080'"
            write(20241,'(A)') 
        !-------------------------------------

        !Graficar datos
        write(20241,'(A)') "# Graficar datos"

        !Funcion

            !Linea fantasma
            write(20241,'(A)') "plot 0 with lines lw 2 lt rgb '#FFFFFF00' notitle, \"

            ! Gráficar fase obtenida
            write(20241,'(A)') "     'generated/data/fase.txt' u 1:2 w lp&
            & ps 0.5 pt 7 lc rgb'#C7329A' title 'Fase', \"

            !Eje x
            write(20241,'(A)') "     0 with lines lw 2 lt rgb 'black' notitle behind"
        
        !-----------------
        
        close(20241) !Cerrar script
        
        !Correr el script generador de la grafica
        call system('start gnuplot -p generated/gpl/fase.gpl')

    end subroutine

    !Subrutina: Resolución gráfica numérica para el apartado de energías
    subroutine graph_energias(constantes)

        ! Importar funciones
        character(len=50), external :: strd_mod

        ! Declarar variables
        double precision, dimension(7) :: constantes

        !Configuraciones    
        open(20241,file="generated/gpl/energias.gpl") !Crear script para graficar

        !Configuración del gráfico
            write(20241,'(A)') "# Configuración del gráfico"
            write(20241,'(A)') "set terminal pngcairo size 2000,1200 enhanced font 'Verdana,22'"
            write(20241,'(A)') "set output 'generated/graficas/numerical_energias.png'"
            write(20241,'(A)')
        !----------------------------

        !Titulos y etiquetas
            write(20241,'(A)') "# Títulos y etiquetas"
            write(20241,'(A)') "set title '[Método Numérico] Energia vs Tiempo'"
            write(20241,'(A)') "set xlabel 'Tiempo [s]'"
            write(20241,'(A)') "set ylabel 'Energía[J]'"
            write(20241,'(A)') "set key outside bottom right"
            write(20241,'(A)') 
        !---------------------------

        !Mostrar parámetros
            write(20241,'(A)') "set label 1 'Parámetros' at screen 0.88,0.9 left font 'Verdana,24' front"
            write(20241,'(A)') "set label 2 'k="//trim(adjustl(strd_mod(constantes(1))))//"'&
            & at screen 0.88,0.85 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 3 'δ="//trim(adjustl(strd_mod(constantes(2))))//"'&
            & at screen 0.88,0.8 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 4 'ε="//trim(adjustl(strd_mod(constantes(3))))//"'&
            & at screen 0.88,0.75 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 5 'm="//trim(adjustl(strd_mod(constantes(4))))//"'&
            & at screen 0.88,0.7 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 6 'I="//trim(adjustl(strd_mod(constantes(5))))//"'&
            & at screen 0.88,0.65 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 7 'z(0)="//trim(adjustl(strd_mod(constantes(6))))//"'&
            & at screen 0.88,0.6 left font 'Verdana,22' front"
            write(20241,'(A)') "set label 8 'θ(0)="//trim(adjustl(strd_mod(constantes(7))))//"'&
            & at screen 0.88,0.55 left font 'Verdana,22' front"
            write(20241,'(A)') "show label"
            write(20241,'(A)') 
        !---------------------------

        !Configurar los límites de graficación
            write(20241,'(A)') "# Adecuar los limites"
            write(20241,'(A)') "set grid lw 4 lc rgb '#808080'"
            write(20241,'(A)') 
        !-------------------------------------

        !Graficar datos
        write(20241,'(A)') "# Graficar datos"

        !Funcion

            !Linea fantasma
            write(20241,'(A)') "plot 0 with lines lw 2 lt rgb '#FFFFFF00' notitle, \"

            ! Gráficar contribución longitudinal
            write(20241,'(A)') "     'generated/data/energias.txt' u 1:2 w lp&
            & ps 0.5 pt 7 lc rgb'#4778FF' title 'Longitudinal', \"

            ! Gráficar contribución angular
            write(20241,'(A)') "     'generated/data/energias.txt' u 1:3 w lp&
            & ps 0.5 pt 7 lc rgb'#7645FF' title 'Angular', \"

            ! Gráficar contribución de acoplamiento
            write(20241,'(A)') "     'generated/data/energias.txt' u 1:4 w lp&
            & ps 0.5 pt 7 lc rgb'#4E59BA' title 'Acoplamiento', \"

            ! Gráficar energía total
            write(20241,'(A)') "     'generated/data/energias.txt' u 1:5 w lp&
            & ps 0.5 pt 7 lc rgb'#1CD9FF' title 'Energía Total', \"

            !Eje x
            write(20241,'(A)') "     0 with lines lw 2 lt rgb 'black' notitle behind"
        
        !-----------------
        
        close(20241) !Cerrar script
        
        !Correr el script generador de la grafica
        call system('start gnuplot -p generated/gpl/energias.gpl')

    end subroutine

!----------------------------------------------

!Función strd_mod. Convierte un double en caracter formateado
character(len=50) function strd_mod(x)

    ! Declarar variables
    double precision :: x

    ! Convertir double en caractér
    write(strd_mod,'(F0.4)') x

    ! Si el número solo contiene parte decimal, entonces agregar un cero antes de la cadena de caracteres
    if (x < 1.0) then
        write(strd_mod, '(A)') '0'//trim(strd_mod)
    end if

end

! Funciones auxiliares que requiere el método de Runge-Kutta

    !Función auxiliar Rungen-Kutta: Es la función que usa el método de Rungen-Kutta para el cambio de orden longitudinal
    real*16 function d1y(t,y,v)
        real*16 :: v,y,t

        d1y=v
        
    end

    !Función auxiliar Rungen-Kutta: Es la función que usa el método de Rungen-Kutta para la ecuación diferencial longitudinal
    real*16 function d2y(t,y,v,x,b,constantes)
        real*16 :: y,t,v,g,m,c,x,b,k,e
        double precision, dimension(5) :: constantes

        ! Constante longitudinal
        k=constantes(1)

        ! Masa
        m=constantes(4)

        ! Constante de acoplamiento
        e=constantes(3)

        !d2y=2-v
        d2y= -k/m * y - 0.5*e/m * x

    end

    !Función auxiliar Rungen-Kutta: Es la función que usa el método de Rungen-Kutta para el cambio de orden angular
    real*16 function d1x(t,x,b)
        real*16 :: b,x,t

        d1x=b
        
    end

    !Función auxiliar Rungen-Kutta: Es la función que usa el método de Rungen-Kutta para la ecuación diferencial longitudinal
    real*16 function d2x(t,y,v,x,b,constantes)
        real*16 :: x,t,b,g,m,c,v,y,s,e,w
        double precision, dimension(5) :: constantes

        ! Constante torcional
        s=constantes(2)

        ! Constante de acoplamiento
        e=constantes(3)

        ! Inercia
        w=constantes(5)

        d2x= -s/w * x -0.5*e/w * y

    end

!----------------------------------------------

!Función Rungen-Kutta: Soluciona una ecuación diferencial por el método de Rungen-Kutta de cuarto orden
subroutine RK(dsolve,n,constantes)
    double precision, dimension(5) :: constantes ! Se importan las constantes propias del sistema
    real*16, external :: d1y, d2y, d1x, d2x ! Se importan las funciones auxiliares del método de Runge-Kutta
    real*16, dimension (99) :: dsolve ! Importa los datos relevantes para el método de Runge-Kutta, como condiciones iniciales, intervalos, etcétera
    real*16 :: t,k1y,k1v,k2y,k2v,k3y,k3v,k4y,k4v,yf,vf,y0,v0,y_o,v_o,x_o,b_o
    real*16 :: x0,b0,xf,bf,j1x,j1b,j2x,j2b,j3x,j3b,j4x,j4b
    real*16 :: s,w,e,m,k ! Estos son los parametros del sistema, connstantes, masas, inercia, frecuencias, etcétera
    real*16 :: y_a,v_a,x_a,b_a,w_bar,B,D,w1,w2 
    real*16 :: e_kz, e_pz, e_ka, e_pa, e_ac ! Estas son las variables que almacenan los valores de la energía

    ! Heredar y almacenar valores iniciales
    y_o=dsolve(2)
    v_o=dsolve(3)

    x_o=dsolve(6)
    b_o=dsolve(7)

    y0=dsolve(2)
    v0=dsolve(3)

    x0=dsolve(6)
    b0=dsolve(7)

    ! Obtener resultados analíticos

        ! Se heredan los parámetros del sistema en variables más visuales
        
            ! Constante longitudinal
            k=constantes(1)

            ! Constante torcional
            s=constantes(2)

            ! Constante de acoplamiento
            e=constantes(3)

            ! Masa
            m=constantes(4)

            ! Inercia
            w=constantes(5)

        !--------------------------------------------------------------

        ! Obtener las frecuencias de la solución analítica

            ! Frecuencia 1
            w1= sqrt(s/w + 0.5*sqrt(e**2/(w*m)))

            !obtener frecuencia del mov
            !write(*,*) w1

            ! Frecuencia 2
            w2= sqrt(s/w - 0.5*sqrt(e**2/(w*m)))

        !-------------------------------------------------

        ! Frecuencia propia del movimiento ángular (que para nuestro manejo, es la misma que la frecuencia propia del movimiento longitudinal)

            w_bar = sqrt(s/w)

            !obetener frecuencia propia
            !write(*,*) w_bar

            ! Obtener cte de acoplamiento
            !write(*,*) 2*(w1**2-w_bar**2)*sqrt(m*w)

        !----------------------------------------------

        ! Obtener los coeficientes de la solución analítica

            ! Obtener coeficiente B
            B= x_o*(w2**2-w_bar**2) - y_o*e/(2*w)
            B=B/(w2**2-w1**2)

            ! Obtener coeficiente D
            D= y_o*e/(2*w) - x_o*(w1**2-w_bar**2)
            D=D/(w2**2-w1**2)

        !-------------------------------------------------

    !-------------------------------------

    ! Aplicación del método Runge-Kutta de 4 orden para sistemas de ecuaciones diferenciales de segundo orden
    do i=1,n
        t=dsolve(1)+dsolve(4)*real(i)

        ! Primera K y J
            k1y=d1y(t,dsolve(2),dsolve(3))
            k1v=d2y(t,dsolve(2),dsolve(3),dsolve(6),dsolve(7),constantes)

            j1x=d1x(t,dsolve(6),dsolve(7))
            j1b=d2x(t,dsolve(2),dsolve(3),dsolve(6),dsolve(7),constantes)

        ! Segunda K y J
            k2y=d1y(t+dsolve(4)/2.,dsolve(2)+dsolve(4)*k1y/2.,dsolve(3)+dsolve(4)*k1v/2.)
            k2v=d2y(t+dsolve(4)/2.,dsolve(2)+dsolve(4)*k1y/2.,dsolve(3)+dsolve(4)*k1v/2.,&
            & dsolve(6)+dsolve(4)*j1x/2.,dsolve(7)+dsolve(4)*j1b/2.,constantes)

            j2x=d1x(t+dsolve(4)/2.,dsolve(6)+dsolve(4)*j1x/2.,dsolve(7)+dsolve(4)*j1b/2.)
            j2b=d2x(t+dsolve(4)/2.,dsolve(2)+dsolve(4)*k1y/2.,dsolve(3)+dsolve(4)*k1v/2.,&
            & dsolve(6)+dsolve(4)*j1x/2.,dsolve(7)+dsolve(4)*j1b/2.,constantes)

        ! Tercera K y J
            k3y=d1y(t+dsolve(4)/2.,dsolve(2)+dsolve(4)*k2y/2.,dsolve(3)+dsolve(4)*k2v/2.)
            k3v=d2y(t+dsolve(4)/2.,dsolve(2)+dsolve(4)*k2y/2.,dsolve(3)+dsolve(4)*k2v/2., &
            & dsolve(6)+dsolve(4)*j2x/2.,dsolve(7)+dsolve(4)*j2b/2.,constantes)

            j3x=d1x(t+dsolve(4)/2.,dsolve(6)+dsolve(4)*j2x/2.,dsolve(7)+dsolve(4)*j2b/2.)
            j3b=d2x(t+dsolve(4)/2.,dsolve(2)+dsolve(4)*k2y/2.,dsolve(3)+dsolve(4)*k2v/2., &
            & dsolve(6)+dsolve(4)*j2x/2.,dsolve(7)+dsolve(4)*j2b/2.,constantes)

        ! Cuarta K y J
            k4y=d1y(t+dsolve(4),dsolve(2)+dsolve(4)*k3y,dsolve(3)+dsolve(4)*k3v)
            k4v=d2y(t+dsolve(4),dsolve(2)+dsolve(4)*k3y,dsolve(3)+dsolve(4)*k3v,&
            &dsolve(6)+dsolve(4)*j3x,dsolve(7)+dsolve(4)*j3b,constantes)

            j4x=d1x(t+dsolve(4),dsolve(6)+dsolve(4)*j3x,dsolve(7)+dsolve(4)*j3b)
            j4b=d2x(t+dsolve(4),dsolve(2)+dsolve(4)*k3y,dsolve(3)+dsolve(4)*k3v,&
            &dsolve(6)+dsolve(4)*j3x,dsolve(7)+dsolve(4)*j3b,constantes)

        ! Obtener valores de las funciones

            ! Obtención del valor de la posición longitudinal
            yf=y0+(1./6.)*dsolve(4)*(k1y+2.*k2y+2.*k3y+k4y)

            ! Obtención del valor de la velocidad longitudinal
            vf=v0+(1./6.)*dsolve(4)*(k1v+2.*k2v+2.*k3v+k4v)

            ! Obtención del valor de la posición angular
            xf=x0+(1./6.)*dsolve(4)*(j1x+2.*j2x+2.*j3x+j4x)

            ! Obtención del valor de la velocidad angular
            bf=b0+(1./6.)*dsolve(4)*(j1b+2.*j2b+2.*j3b+j4b)

        ! Heredar nuevos valores obtenidos para ciclar
            dsolve(2)=yf
            dsolve(3)=vf

            dsolve(6)=xf
            dsolve(7)=bf

            y0=yf
            v0=vf
            x0=xf
            b0=bf

            ! Obtener posiciones y velocidades analíticamente

                ! Obtener posición longitudinal análiticamente
                y_a = 2*w*(B*(w1**2-w_bar**2)*cos(w1*t)+D*(w2**2-w_bar**2)*cos(w2*t))/e

                ! Obtener velocidad longitudinal analíticamente
                v_a= 2*w*(B*(w1**2-w_bar**2)*(-w1)*sin(w1*t)+D*(w2**2-w_bar**2)*(-w2)*sin(w2*t))/e

                ! Obtener posición ángular analíticamente
                x_a= B*cos(w1*t)+D*cos(w2*t)

                ! Obtener velocidad ángular analíticamente
                b_a= -w1*B*sin(w1*t)-w2*D*sin(w2*t)

            !----------------------------------------------------------------------------------

            ! Obtener Energía cinética longitudinal numéricamente
            e_kz=0.5*m*dsolve(3)**2

            ! Obtener Energía potencial longitudinal numéricamente
            e_pz=0.5*k*dsolve(2)**2

            ! Obtener Energía cinética ángular numéricamente
            e_ka=0.5*w*dsolve(7)**2

            ! Obtener Energía potencial ángular numéricamente
            e_pa=0.5*s*dsolve(6)**2

            ! Obtener Energía de acoplamiento numéricamente
            e_ac=0.5*e*dsolve(2)*dsolve(6)

        ! Guardar puntos en archivos de datos
            write(20041,*) t,dsolve(2),y_a ! Posición longitudinal
            write(20042,*) t,dsolve(3),v_a ! Velocidad longitudinal

            write(20043,*) t,dsolve(6),x_a ! Posición ángular
            write(20044,*) t,dsolve(7),b_a ! Velocidad ángular

            write(20045,*) dsolve(6),dsolve(2),x_a,y_a  ! Fase (longitudinal vs ángular)

            write(20046,*) t,e_kz+e_pz,e_ka+e_pa,e_ac,e_kz+e_pz+e_ka+e_pa+e_ac ! Energías

    end do

    write(*,*)

end