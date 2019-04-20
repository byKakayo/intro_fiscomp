PROGRAM exer03
  IMPLICIT NONE
  INTEGER :: x, sinal
  INTEGER(8) :: fat, i
  REAL(4) :: x1, aprox
  REAL(8) :: x2
  !REAL(8), parameter :: pi=4.0d0*atan(1.0d0)

  DO x=1, 4
    x1 = x*0.1
    aprox = 0
    PRINT *, "LOOP 1"
    i = 1
    DO WHILE (i < 3)
      sinal = (-1)**(i+1)
      aprox = aprox + sinal*((x1**i)/fat(i))
      i = i+1
      PRINT *, i
    END DO
    PRINT *, sin(x1), aprox
  END DO
END PROGRAM exer03

INTEGER(8) FUNCTION fat(x)
  IMPLICIT NONE
  INTEGER(8) :: x
  fat = 1
  DO WHILE (x /= 0)
    fat = fat*x
    x = x - 1
  END DO
END FUNCTION fat
