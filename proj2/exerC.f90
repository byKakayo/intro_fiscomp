PROGRAM exerC
  IMPLICIT NONE
  INTEGER :: n, i

  !Arquivo de saida
  OPEN(UNIT=2,FILE="tabC_out.dat")

  READ(*,*)n

  !Derivada: 6*x**2 - 8*x -1

  !Escrevedo descrição no arquivo tabC_out
  WRITE(2, *)"iter ", &
  & " dir1 ", &
  & " dir2 ", &
  & " dir3 ", &
  & " NR1 ", &
  & " NR2 ", &
  & " NR3 ", &
  & " sec1 ", &
  & " sec2 ", &
  & " sec3"

  DO i = 1, n

  END DO

END PROGRAM exerC
