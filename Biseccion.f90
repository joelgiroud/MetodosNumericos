program biseccion
    !Programa de búsqueda de raíces de un polinomio con el método de bisección
    implicit none !

    ! Definimos las variables que se usarán
    ! lim_sup y lim_inf son las cotas
    ! punto_medio es evidentemente el punto medio

    real cota_sup, cota_inf, x, punto_medio, criterio       ! Valores primitivos que se van a estar manejando
    real funcion, ult_aprox, eval_sup, eval_inf, error             ! Valores iterados que se van a estar manejando
    integer contador                                        !Contador de operaciones
    contador=0                              
    error=100
    
    write(*,*) "Calculadora del método de bisección."
    
    do while ((funcion(cota_inf)*funcion(cota_sup).GE.0).OR.(cota_inf.GE.cota_sup))
        ! Pedimos los límites separados por comas
        write(*,*) "Ingresa los límites inferiores y superiores separados por commas. Ejemplo: 1,2"
        read(*,*) cota_inf, cota_sup
    end do !Repetir hasta obtener valores aceptables
    
    write (*,*) "¿Cuál es el porcentaje del criterio de aceptación en el error del resultado (mayor criterio mayor error)?"
    read (*,*) criterio
    write (*,*) "Criterio de aceptación = ", criterio
    
    write (*,*) "Inicia proceso de bisección sobre la función. Para conocer la función ir al código fuente..."
    ult_aprox = cota_inf

    
    do while (error.GE.criterio)                            !Iteramos hasta que el error actual sea diferente al criterio dado
        punto_medio = (cota_inf + cota_sup) / 2.0           ! Obtenemos la aproximación de la raiz
        contador = contador + 1
        eval_sup = funcion(punto_medio)                     ! Evaluamos límites en la función
        eval_inf = funcion(cota_inf)                        ! Comenzamos desde el intervalo del límite inferior y la aproximación

        ! EVALUAMOS LAS CONDICIONES
        if((eval_sup*eval_inf).LT.0.0) then                 ! Si la multiplicación es menor a cero, entonces la raíz está entre ese intervalo
            cota_sup = punto_medio                          ! Por lo tanto, nuestra aproximación será el nuevo límite superior
        else if((eval_sup*eval_inf).GT.0.0) then            ! Si la multiplicación es mayor a cero, entonces la raíz está en el otro intervalo
            cota_inf = punto_medio                          ! Por lo tanto, el límite menor será nuestra aproximación
        else                                                ! Si no se cumple ninguna condición, es porque encontramos la raíz
            exit
        end if

        
        if(punto_medio.NE.0.0) then                                 ! Si la aproximación obtenida no es cero
            error = abs((ult_aprox - punto_medio)*100/punto_medio)  ! Calculamos el error relativo
        end if
        ult_aprox = punto_medio                             ! Guardamos el valor obtenido para la siguiente iteración
    END DO
    
    write(*,*) "El número de iteraciones realizadas fue: ", contador
    write(*,*) "||||    La aproximación a la raíz es: ", punto_medio, "||||"
    
stop 'Fin del programa'
end program biseccion

function funcion(x)
    implicit none
    real funcion, x
    funcion= x**2-5 !MODIFICAR valores de esta función a conveniencia
    return
end function




10%	2.06250000
05%	2.15625000
02%	2.22656250
01%	2.23828125
 2.23606801 
