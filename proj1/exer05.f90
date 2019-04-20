PROGRAM exer05
  IMPLICIT NONE
  INTEGER :: N, i !Define como inteiro as variáveis N e i
  REAL(8) :: precisao, erro, lamnew, lam !Define como real precisao, erro, lamnew e lam (lambda)
  REAL(8), DIMENSION(:,:), ALLOCATABLE :: M
  REAL(8), DIMENSION(:), ALLOCATABLE :: x, y, u

  !Recebe o valor da precisao
  READ(*,*)precisao
  !Recebe o valor de N, ordem da matrix
  READ(*,*)N

  !Aloca a matriz M de ordem N
  ALLOCATE(M(N,N))
  !Aloca os vetores de ordem N
  ALLOCATE(x(N), y(N), u(N))

  !Define aleatoriamente o vetor x
  CALL RANDOM_NUMBER(x)

  !Popula a matriz M
  DO i = 1, N
    READ(*,*)M(i,:)
  END DO

  !erro inicial igual 2x a precisao
  erro = 2d0*precisao
  y = x/sqrt(sum(x*x))
  u = matmul(y,M)
  lam = sum(u*y)

  !Cálcula autovalor
  DO WHILE (erro > precisao)
    x = matmul(y,M)
    y = x/sqrt(sum(x*x))
    u = matmul(y,M)
    lamnew = sum(u*y)
    !erro igual a diferença do autovalor anterior com o autovalor calculado
    erro = abs(lamnew-lam)
    !atualiza o autovalor
    lam = lamnew
  END DO

  !escreve no terminal o autovalor encontrado
  print *, lam

END PROGRAM exer05
