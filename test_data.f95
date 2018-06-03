program test_data
	implicit none
	double precision :: x(2), lambda, function_value, T_1, T_2, spacing, l_log, u_log, gap
	double precision, allocatable ::  rho(:), width(:)
	integer :: i, j, n, n_points
	
	read*, n! no of layers
	allocate(rho(n), width(n-1))
	
	

	do i=1, n
	rho(1) = 100
	rho(2) = 1000
	rho(3) = 100
!	rho(4) = 100	
!	rho(5) = 500
	
	width(1) = 100
	width(2) = 50
!	width(3) = 50
!	width(4) = 200
	end do


OPEN (FILE="test_data_lambda.csv",UNIT=42,ACTION="WRITE")
OPEN (FILE="test_fdata_transform.csv",UNIT=72,ACTION="WRITE")


! do j = 1, 10	
! 	call random_number(x)
! 	 x = -6 + 12 * x	
! 	WRITE (42,*) x(1), x(2)
! 	function_value = 0
! 	do i=1,2
! 		function_value = FUNCTION_VALUE + 0.01 * ( (X(I)+0.5)**4 - 30*X(I)**2 -20*X(I))	
! 	end do		
! 			WRITE (72,*) 	function_value
! end do




!lambda = 1
!do while (lambda > 0.00005) 

spacing = 1
lambda = 1/spacing


n_points = 100
l_log = log(spacing)
u_log = log(40000.0) ! upper bound of spacing
gap = ( u_log - l_log )/n_points

	
	
	
do  j =1, 100

	T_2 = rho(n)
	do i = n-1,1,-1
		T_1 = (T_2 + rho(i) * tanh(lambda*width(i)))/(1 + T_2 * tanh(lambda*width(i))/rho(i))	
		T_2 = T_1
	end do
	
	!WRITE (42,'(*(G0.4,:,","))') 1/lambda, t_1
	WRITE (42,*) lambda
	WRITE (72,*) T_1
	
	
	spacing = exp(l_log + gap*j)
	print*, "gap", gap, "spacing",spacing
	lambda = 1/spacing
	
end do	



end program test_data