subroutine strtoimd(Line, Nx, Words, NumValue, IntValue, DblValue)
!********************************************************************************
!*                                                                              *
!*  Convert the String to Integer and Double                                	*
!*                                                                              *
!********************************************************************************
    implicit none
    integer,intent(in)::        Nx 
    integer,intent(out)::       NumValue,    IntValue(Nx)
    real*8,intent(out)::        DblValue(Nx)  
    character(*),intent(inout)::Line
    integer::                   NumWord,NumChar(Nx),length, cbegin, cend,       &
                                count,  star,   endPtr,     i,      j,      k,  &
                                atoi
    real*8::                    atof
    character(*)::              Words(Nx)
    logical::                   digit
!================================================================================
!   Function	:	Convert string to float including the expression like
!   			:	"15*100.0", which is read as double (100.0) by int (15)
!	Line		:	Each line in the input data
!	NumWord		:	Number of words in the Line
!   NumChar     :   Number of characters in each word      
!	Words		:	Array of the words comprised of the line
!	NumValue	:	Number of values extracted from the Line
!	IntValue	:	Integer value extracted from the Line
!	DblValue	:	Double  value extracted from the Line
!   endPtr      :   Pointer of the remaining word after converting the word
!   length      :   Length of Line    
!  	cbegin      :   Location of the first character of each word
!	cend        :   Location of the last  character of each word
!	count       :   Number of count	
!================================================================================
!	Extract words from Line given
!================================================================================    
    length = len_trim(Line)        !Calculate length of the string without space

    !Global loop to find the "void" character
	cbegin = 1
	cend   = 1
	count  = 0
    do while (cbegin <=length)	

		if ( Line(cbegin:cbegin) == '0' .or. Line(cbegin:cbegin) == '1' .or. &
             Line(cbegin:cbegin) == '2' .or. Line(cbegin:cbegin) == '3' .or. &
             Line(cbegin:cbegin) == '4' .or. Line(cbegin:cbegin) == '5' .or. &
             Line(cbegin:cbegin) == '6' .or. Line(cbegin:cbegin) == '7' .or. &
             Line(cbegin:cbegin) == '8' .or. Line(cbegin:cbegin) == '9' .or. &
             Line(cbegin:cbegin) == '-' .or. Line(cbegin:cbegin) == '.' ) then
        
            count = count + 1
			do j=cbegin+1,length+1   !Local loop to find the next "void" character
				if (Line(j:j) == ' ' .or. Line(j:j) == '\0' .or. Line(j:j) == '\n') then
					cend = j - 1
					exit
                endif
                cend = j
            enddo

			NumChar(count) = cend - cbegin + 1
            do k=1,NumChar(count)
                Words(count)(k:k) = Line(cbegin+k-1:cbegin+k-1)
            enddo
            cbegin = cend
        endif
        cbegin = cbegin + 1   
        
    enddo
    NumWord = count
    
    !print *, "Numword= ",NumWord, ", Word= ",Words(NumWord)
!================================================================================
!	Convert words to digit
!================================================================================
	count = 0   !Count number of values

	do i=1,NumWord
        
		digit = .true. 
		do j=1,NumChar(i)
            !Check if Words is digit or letter
			if (.not.( Words(i)(j:j) == '0' .or. Words(i)(j:j) == '1' .or. &
                       Words(i)(j:j) == '2' .or. Words(i)(j:j) == '3' .or. &
                       Words(i)(j:j) == '4' .or. Words(i)(j:j) == '5' .or. &
                       Words(i)(j:j) == '6' .or. Words(i)(j:j) == '7' .or. &
                       Words(i)(j:j) == '8' .or. Words(i)(j:j) == '9' .or. &
                       Words(i)(j:j) == '-' .or. Words(i)(j:j) == '.' .or. &
                       Words(i)(j:j) == '*' ) )     digit = .false.
        enddo

		!Convert the string to float
		if (digit) then !Words contains digit only

			!Check if string contains '*' and location number
			star = index( Words(i)(1:NumChar(i)), '*' )

            !Convert the string to int and double
			if (star > 0) then
				count = count + 1
				IntValue(count) = atoi( Words(i)(:star-1))
				DblValue(count) = atof( Words(i)(star+1:NumChar(i)))
			else 
				count = count + 1
				IntValue(count) = 1
				DblValue(count) = atof( Words(i)(1:NumChar(i)))
            endif
            
        endif
		NumValue = count

        !print *,"Words = ",Words(i),", NumValue = ",NumValue,", IntValue = ",IntValue(i),", DblValue = ",DblValue(i),", ChrValue = ",ChrValue(i)
        
    enddo
!================================================================================
end subroutine