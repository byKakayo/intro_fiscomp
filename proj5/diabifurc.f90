PROGRAM diag_bifurc
  REAL(8) :: r, x, dr
  INTEGER(8) :: i, j, m, n

  OPEN(10, file="bifurcacao.dat")

  dr = 1d-3
  r = 1d0
  m = 1000
  n = 500

  DO WHILE (r.LT.4)
    DO i = 0, m+n
      CALL RANDOM_NUMBER(x)
      DO j = 0, n
        x = r*x*(1-x)
      ENDDO
      x = r*x*(1-x)
      WRITE(10, *) r, x
    ENDDO
    r = r + dr
  ENDDO

  CLOSE(10)

END PROGRAM diag_bifurc
