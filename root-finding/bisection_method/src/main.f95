! Start BISECTION METHOD program
PROGRAM BISECTION
    Implicit none
    
! Este programa usa el método de bisección para 
! encontrar una raíz de una función

    ! Declare variables
     REAL A,B,DX,DL,F,x0
     INTEGER ISTEP

    ! Start graph script
     open(20043,file="out/graph_script.gpl")

    ! Start data points
     open(20044,file="out/result.txt")

    ! Ask for boundaries
     WRITE (*,*)'Dame los extremos del intervalo:'
     READ(*,*) A,B

!Guardar en un archivo los datos de los extremos iniciales
     write(20044,*) A,F(A)
     write(20044,*) " "
     write(20044,*) B,F(B)
     write(20044,*) " "

     DL = 1.0E-06

!    Tamaño del intervalo
    DX = B - A

    ISTEP = 0

! Imprimimos el valor de la funcion evaluada
! en los extremos del intervalo
    WRITE (*,*) 'F(A):', F(A)
    WRITE (*,*) 'F(B):', F(B)

! Ciclo DO
    DO    100  WHILE (ABS(DX).GT.DL)

! XO es el punto c que esta a la mitad del intervalo [A,B]
      X0 = (A+B)/2.0
!
! Originalmente F(A)*F(B) es negativo porque se da cambio 
! de signo de la función
! Si en el punto c se cumple ese cambio de signo
! entonces F(A)*F(C) implica que B=C

      IF ((F(A)*F(X0)).LT.0.0) THEN ! Se tiene cambio de signo
        B  = X0
        DX = B - A
      ELSE
        A  = X0
        DX = B - A
      END IF
      WRITE (6,1000)x0,F(x0)
      ISTEP = ISTEP + 1
100 END DO
    
    WRITE (*,*) 'Iteracion,Raiz,Tolerancia' 
    WRITE (6,999) ISTEP,X0,DX

999 FORMAT (I4,2F16.8)
1000 FORMAT (2F16.8)

!Guardar en un archivo los datos de los extremos iniciales la raiz encontrada
write(20044,*) x0,F(x0)

! Graficar función
write(20043,*) "set title 'Funcion y una raiz'"
write(20043,*) "set terminal png"
write(20043,*) "set output 'out/grafica.png'"
write(20043,*) "set xlabel 'x'"
write(20043,*) "set ylabel 'y'"
write(20043,*) "set xtics"
write(20043,*) "set ytics"
write(20043,*) "set grid"
write(20043,*)"set xrange [-10:10]"
write(20043,*)"set yrange [-5:10]"
write(20043,*) "set yzeroaxis lw 3 lc rgb 'black'"
write(20043,*) "unset key"
write(20043,*) "plot x**6-4*x**4+x**2+6 lw 3, 'out/result.txt' u 1:2 w lp ps 2 pt 7 lc rgb 'black'"
write(20043,*) "replot 0 lw 5"

! Correr el script generador de la grafica
call system('start gnuplot -p out/graph_script.gpl')

!Cerrar unidades
close(20043)
close(20044)

    END
      
    FUNCTION F(X)
    REAL F,X
      F = (x**6)-(4*x**4)+(x**2)+6

    RETURN
    END

