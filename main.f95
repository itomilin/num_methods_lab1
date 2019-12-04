program main
    !implicit none

    integer, parameter :: N_SIZE = 6
    !integer            :: i = 1

    real, parameter    :: vector_x(N_SIZE) = (/ 1.0, 1.2, 1.4, 1.6, 1.7, 1.9 /), &
                          vector_y(N_SIZE) = (/ 1.100, 1.1212, 1.1427, 1.1647, 1.1759, 1.1985 /)

    real               :: vector_b(N_SIZE), &
                          vector_c(N_SIZE), &
                          vector_d(N_SIZE)

    real               :: a, &
                          b, &
                          c, &
                          f_a, &
                          f_c, &
                          f_b

    a = minval(vector_x)
    b = maxval(vector_x)
    k = 0

    100 format('[', i2, ']', F15.10)
    101 format(A, F10.7, A, F10.7)
    call Spline(N_SIZE, vector_x, vector_y, vector_b, vector_c, vector_d)

    print *, "Разность между левой и правой частью при найденном корне."
    do
        k = k + 1
        c = (a + b) / 2

        f_a = SEVAL(N_SIZE, a, vector_x, vector_y, vector_b, vector_c, vector_d)
        f_a = f_a - a


        f_b = SEVAL(N_SIZE, b, vector_x, vector_y, vector_b, vector_c, vector_d)
        f_b = f_b - b

        f_c = SEVAL(N_SIZE, c, vector_x, vector_y, vector_b, vector_c, vector_d)
        !print *, c
        print 101, "Значение корня f(x) ", f_c, "      f(x) - x", f_c - c
        f_c = f_c - c

        if ( f_a * f_c < 0.0 ) then
            b = c
        end if

        if ( f_b * f_c < 0.0 ) then
            a = c
        end if

        !if ( f_c == f_b ) then
        !    b = c
        !else
        !    a = c
        !end if
!ззначение корня и невязка
        !print *, a, b, c, f_a, f_b
        !print 100, k, b - a

        !print 100, b - a
        if ( (b - a) < 0.00001 ) then
            exit
        end if

    end do

    write (*, *) "Количество итераций: ", k
    write (*, *) "Результат: ", "a = ", a, "b = ", b

    !print 100

    !print 101, (vector_b(I), vector_c(I), vector_d(I), i = 1, N_SIZE)

    !STOP

    !100 format(14X, 'B', 15X, 'C', 15X, 'D')
    !101 format(5X, 3E16.7)


    !s = SEVAL(N_SIZE, u, vector_x, vector_y, vector_b, vector_c, vector_d)

    !print 102, u, s


    !102 format(14X, 'U = ', F3.1, 5X, 'S = ', F10.7)

end
