PROGRAM exer01
  IMPLICIT NONE
  REAL(4) :: a
  REAL(8) :: b
  REAL(16) :: c
  INTEGER :: i

  !Definindo o formato dos reais e o valor como 1
  a = 1.0e0
  b = 1.0d0
  c = 1.0_16

  i = 0 !Zerando o contador dos bit da mantissa
  DO WHILE ((1+a) /= 1) !Enquanto a não converge p/ 0
    i = i+1 !Soma 1 no contador
    a = a/2
  END DO
  PRINT *, i-1, a*2

  i = 0 !Zerando o contador dos bit da mantissa
  DO WHILE ((1+b) /= 1)
    i = i+1 !Soma 1 no contador
    b = b/2
  END DO
  PRINT *, i-1, b*2

  i = 0 !Zerando o contador dos bit da mantissa
  DO WHILE ((1+c) /= 1)
    i = i+1 !Soma 1 no contador
    c = c/2
  END DO
  PRINT *, i-1, c*2
END PROGRAM exer01
