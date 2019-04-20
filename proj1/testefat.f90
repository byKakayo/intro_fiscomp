PROGRAM testefat
  IMPLICIT NONE
  INTEGER(8) fat, x
  x = 3
  PRINT *, fat(x)
END PROGRAM testefat

INTEGER(8) FUNCTION fat(x)
  IMPLICIT NONE
  INTEGER(8) :: x
  fat = 1
  DO WHILE (x /= 0)
    fat = fat*x
    x = x - 1
  END DO
END FUNCTION fat
