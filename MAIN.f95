PROGRAM MAIN


Double precision,allocatable::A(:,:),B(:,:),C(:,:)
INTEGER::MT1,MT2,NT3,MT3,NT2,I,N1,N2,NT1,M1,M2,MC,NC
CHARACTER::ASW

100 PRINT*,"�ó���Ϊ�����Զ���������ɽ��B*A�����⣬������A�ĳ����"!����
PRINT*,"��"
READ*,N1
PRINT*,"��"
READ*,M1
ALLOCATE(A(N1,M1))
!����A����
NT1=1
MT1=1
DO MT1=1,M1
  DO NT1=1,N1
    PRINT*,"������a",MT1,"",NT1
    READ(*,*)A(NT1,MT1)
    
  END DO
  NT1=1

END DO 
NT1=1
MT1=1
PRINT*,"����A�����У�"
DO WHILE(MT1<=M1)
  DO WHILE(NT1<=N1)
   PRINT*,"a",MT1,"",NT1,"=",A(MT1,NT1)
   NT1=NT1+1
  END DO
  NT1=1
  MT1=MT1+1
END DO
PRINT*,"������B�ĳ����"
PRINT*,"��"
READ*,N2
PRINT*,"��"
READ*,M2
ALLOCATE(B(N2,M2))
NT2=1
MT2=1
DO WHILE(MT2<=M2)
  DO WHILE(NT2<=N2)
    PRINT*,"������b",MT2,"",NT2
    READ*,B(MT2,NT2)
    NT2=NT2+1
  END DO
  NT2=1
  MT2=MT2+1
END DO 
NT2=1
MT2=1
PRINT*,"����B�����У�"
DO WHILE(MT2<=M2)
  DO WHILE(NT2<=N2)
   PRINT*,"b",MT2,"",NT2,"=",B(MT2,NT2)
   NT2=NT2+1
  END DO
  NT2=1
  MT2=MT2+1
END DO
NC=N2
MC=M1
NT3=1
MT3=1
MT1=1
MT2=1
NT1=1
NT2=1
I=1
ALLOCATE(C(NC,MC))
DO WHILE(MT3<=MC)
  DO WHILE(NT3<=NC)
    IF(NC<MC) THEN
      DO WHILE(I<=NC)
        C(NT3,MT3)=A(NT3,I)*B(I,NT3)+C(NT3,MT3)
        I=I+1
      END DO
      I=1
    END IF
    IF (NC>=MC) THEN
      DO WHILE(I<=MC)
        C(NT3,MT3)=A(NT3,I)*B(I,NT3)+C(NT3,MT3)
        I=I+1
      END DO
      I=1
    END IF
    NT3=NT3+1
  END DO
  NT3=1
  MT3=MT3+1
END DO
NT3=1
MT3=1
PRINT*,"����C�����У�"
DO WHILE(MT3<=MC)
  DO WHILE(NT3<=NC)
   PRINT*,"c",MT3,"",NT3,"=",C(MT3,NT3)
   NT3=NT3+1
  END DO
  NT3=1
  MT3=MT3+1
END DO

DEALLOCATE(A,B,C)
PRINT *,"�����(Y/N"
300 READ(*,*),ASW
 IF(ASW=="Y")THEN
  GOTO 100
  ELSE IF(ASW=="N")THEN
  GOTO 200
  ELSE
  PRINT*,"�ֵܣ�������Y����N�е�һ��������һ����Y����֮����N��лл��"
  GOTO 300
  END IF
200 PRINT*,"��������pzqking������ллʹ��"
READ(*,*)
END PROGRAM MAIN