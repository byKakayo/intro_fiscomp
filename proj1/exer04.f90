PROGRAM exer04
  IMPLICIT NONE
  !Variáveis de controle
  INTEGER(8) :: i, N, M, j
  !Define dinamicamente o vetor v
  REAL(8), DIMENSION(:), ALLOCATABLE :: v
  REAL(8) :: aux

  !Arquivo de entrada
  OPEN(UNIT=1,FILE="ord_in.dat")
  !Arquivo de saida
  OPEN(UNIT=2,FILE="ord_out.dat")
  !Recebe o valor N
  READ(*,*)N
  !Recebe o valor M
  READ(*,*)M
  !Aloca N posições no vetor v
  ALLOCATE(V(N))

  DO i = 1, N !LER N NÚMEROS DO ARQUIVO ord_in.dat
    READ(1, *)v(i)
  END DO

  !percorre o vetor v, que tem tamanho N, e escala os maiores valores para o final do vetor v
  DO i=N-1,1,-1
      DO j=1,i
          IF (v(j) >= v(j+1)) THEN
              aux = v(j)
              v(j) = v(j+1)
              v(j+1) = aux
          END IF
      END DO
  END DO

  !Escreve no arquivo de saida ord_out.dat os M maiores números
  DO i = N-M+1, N
    WRITE(2, *)v(i)
  END DO
  !Fechar os arquivos
  CLOSE(1)
  CLOSE(2)
END PROGRAM exer04
