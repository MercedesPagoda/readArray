subroutine readIntArray(Line, Inx, Nx, IntArray)
!********************************************************************************
!*                                                                              *
!*  Read the Integer Array                                                      *
!*                                                                              *
!********************************************************************************
    implicit none
    integer,intent(in)::	    Nx
    integer,intent(inout)::     Inx
    integer,intent(out)::       IntArray(Nx)           
    character(*),intent(inout)::Line
    integer::                   NumWord,    NumValue,   IntValue(Nx),   i,  j
    real(8)::                   DblValue(Nx)   
    character(132)::            Words(Nx)
!================================================================================
!	Line		:	Line string given (Max assumed 132 as default in Fortran)
!	Inx		    :	Index of the IntArray	
!   IntArray	:	Array to be inputed
!	NumWord     :   Number of words in the Line
!	Words       :	Array of the words comprised of the line
!	NumValue    :   Number of values
!	IntValue(Nx):   Integer   value extracted from the Line
!	DblValue(Nx):   Double    value extracted from the Line
!================================================================================
!   Extract IntValue and DblValue from the line string given
!================================================================================
   	call strtoimd(Line, Nx, Words, NumValue, IntValue, DblValue)
!================================================================================
!   Input DbleArray given
!================================================================================
	do i=1,NumValue
	do j=1,IntValue(i)
		Inx = Inx + 1
		IntArray(Inx) = int(DblValue(i))
	enddo
	enddo	
!================================================================================
end subroutine
subroutine readDblArray(Line, Inx, Nx, DblArray)
!********************************************************************************
!*  Read the Double Array                                                       *
!********************************************************************************
    implicit none
    integer,intent(in)::        Nx
    integer,intent(inout)::     Inx
    real*8,intent(out)::        DblArray(Nx)
    character(*),intent(inout)::Line
    integer::                   NumWord,    NumValue,   IntValue(Nx),   i,  j
    real(8)::                   DblValue(Nx)   
    character(132)::            Words(Nx)
!================================================================================
!   Line	    :   Line string given (Max assumed 132 as default in Fortran)
!   Inx		    :   Index of the DblArray	
!   Nx          :   Number of array of the words comprised of the line
!   DblArray	:   Array to be inputed
!   NumWord     :   Number of words in the Line
!   Words       :   Array of the words comprised of the line
!   NumValue    :   Number of values
!   IntValue(Nx):   Integer   value extracted from the Line
!   DblValue(Nx):   Double    value extracted from the Line
!================================================================================
!   Extract IntValue and DblValue from the line string given
!================================================================================
    call strtoimd(Line, Nx, Words, NumValue, IntValue, DblValue)
!================================================================================
!   Input DbleArray given
!================================================================================
    do i=1,NumValue
    do j=1,IntValue(i)
	    Inx = Inx + 1
	    DblArray(Inx) = DblValue(i)
    enddo
    enddo
!================================================================================
end subroutine