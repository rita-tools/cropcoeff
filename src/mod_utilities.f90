! /***************************************************************************
!  cropcoeff
!  A executable/library to use to produce inputs for the IdrAgra model
! -------------------
!         begin                : 2022-01-01
!         copyright            : (C) 2022 by Enrico A. Chiaradia
!         email                    : enrico.chiaradia@unimi.it
!  ***************************************************************************/

! /***************************************************************************
!  *                                                                           *
!  *   This program is free software; you can redistribute it and/or modify  *
!  *   it under the terms of the GNU General Public License as published by  *
!  *   the Free Software Foundation; either version 2 of the License, or       *
!  *   (at your option) any later version.                                   *
!  *                                                                           *
!  ***************************************************************************/
!
!  /***************************************************************************
! credits:
! Part of this work derives from unsigned codes. If have rights about some parts,
! please contact me at enrico.chiaradia@unimi.it
!   ***************************************************************************/

module mod_utilities
    implicit none

    !-----------------------------------!     
    !     KINDS to define PRECISION     !     
    !-----------------------------------!     
    integer, parameter :: sp = kind(1.0)
    integer, parameter :: dp = kind(1.0D0)
    real(dp), parameter :: pi = 3.1415926535897932384626433832795
    integer, parameter :: maxlength = 250 ! maximum first try allocation
    integer :: max_plen = 250 ! maximum for printing, it will updated after crop list loading
    Real(dp),parameter :: nodatar = -9999.0
    Integer,parameter :: nodatai = -9999
    character(len=10),parameter :: realFormat =  '(*(f10.3))'!'(*(e15.5))' ! scientific notation
    character(len=10),parameter :: intFormat =  '(*(I10.0))'
  
    contains
        
        subroutine splitString(inString,  sep, firstPart, secondPart)
            implicit none
            character(len=*), intent(in) :: inString
            character(Len=*), intent(in) :: sep
            character(len=*), intent(out):: firstPart, secondPart
            character(len=maxlength) :: string
            integer :: i, l

            string = TRIM(instring)
            i = scan(string, sep)  ! find the first equal symbol
            l = len(sep)
            if (i>0) then ! split row
                firstPart = string(1:i-1) 
                secondPart = string(i+l:) 
            else ! it could be a table row
                firstPart = string 
                secondPart = ''
            end if
            
        END SUBROUTINE splitString

        subroutine resizeArray(array, num)
            REAL(dp),dimension(:),intent(inout), pointer :: array
            integer,intent(in) :: num
            REAL(dp),dimension(num) :: dummy
            integer :: i
            !print *,'** in resizeArray **',num
            do i=1,num
                dummy(i) = array(i)
            end do 

            deallocate(array)
            allocate(array(num))
            
            array = dummy
        end subroutine resizeArray

        FUNCTION replaceText (s,text,rep)  RESULT(outs)
            ! from https://fortranwiki.org/fortran/show/String_Functions
            CHARACTER(*)        :: s,text,rep
            CHARACTER(LEN(s)+100) :: outs     ! provide outs with extra 100 char len
            INTEGER             :: i, nt, nr

            outs = s ; nt = LEN_TRIM(text) ; nr = LEN_TRIM(rep)
            DO
               i = INDEX(outs,text(:nt)) ; IF (i == 0) EXIT
               outs = outs(:i-1) // rep(:nr) // outs(i+nt:)
            END DO
        END FUNCTION replaceText
        
        FUNCTION removeRepChar (word,repCh)  RESULT(outs)
            CHARACTER(*) , intent(in) :: word
            character , intent(in) :: repCh
            character :: preCh
            CHARACTER(LEN(word)) :: outs     ! at least the same length
            INTEGER             :: i,new_len
            
            new_len = 0
            
            ! init preCh with the following character of repCh
            preCh = char(ichar(repCh)+1)
            
            do i=1,len(word)
                if (.not. ((word(i:i) == preCh) .and. (word(i:i) == repCh))) then
                    new_len = new_len+1
                    outs(new_len:new_len)=word(i:i)
                    preCh = word(i:i)
                end if
            end do
            
            outs = outs(1:new_len)
            
        END FUNCTION removeRepChar

        subroutine lower_case(word)
            ! convert a word to lower case
            character (len=*) , intent(inout) :: word
            integer :: i,ic,nlen
            nlen = len(word)
            if (nlen == 0) stop "Zero characters' string"
            do i=1,nlen
                ic = ichar(word(i:i))
                if (ic >= 65 .and. ic <= 90) word(i:i) = char(ic+32)
            end do
        end subroutine lower_case

        subroutine replaceChar(word, findCh,repCh)
            ! replace findCh character with repCh character
            character (len=*) , intent(inout) :: word
            character , intent(in) :: findCh,repCh
            integer :: i,ic,nlen
            nlen = len(word)
            do i=1,nlen
                if (word(i:i) == findCh) word(i:i) = repCh
            end do
        end subroutine replaceChar

        function strToInt(str) result(a)
            integer :: a
            character(*) :: str
            !print *, 'test str: ',str
            read(str,*) a
        end function strToInt
            
        function strToReal(str) result(a)
            Real(dp) :: a
            character(*) :: str
            read(str,*) a
        end function strToReal

        function strToBool(str) result(a)
            integer :: dummy
            Logical :: a
            character(*) :: str
            dummy = strToInt(str)
            !read(str,*) a
            if (dummy<=0) then
                a = .false.
            else
                a = .true.
            end if
        end function strToBool
        
        function intToStr(intValue) result(a)
            integer :: intValue
            character(len =maxlength) :: a
            write(a,*) intValue
        end function

        function realToStr(rValue) result(a)
            real(dp) :: rValue
            character(len =maxlength) :: a
            write(a,realFormat) rValue
        end function
        
        function boolToStr(bValue) result(a)
            logical :: bValue
            character(len =maxlength) :: a
            if (bValue .eqv. .true.) then
                a = '1'
            else
                a = '0'
            end if
        end function
        
        function addReal(a, aRealVal) result(nOfVal)
            Real(dp), intent(inout), dimension(:), allocatable :: a
            Real(dp), intent(in) :: aRealVal
            Real(dp), dimension(:), allocatable :: newA
            integer :: i,nOfVal
            nOfVal = 0
            if (ALLOCATED(a) .eqv. .true.) then
                nOfVal = ubound(a,1)
                !print*, 'following allocate', nOfCropSeq
                ! copy the list
                allocate(newA(nOfVal))
                do i=1, nOfVal
                    newA(i) = a(i)
                end do
                ! clear original
                deallocate(a)
                allocate(a(nOfVal+1))
                do i=1, nOfVal
                    a(i) = newA(i)
                end do
                
                a(nOfVal+1) = aRealVal
            else
                !print*, 'first allocation', nOfCropSeq
                allocate(a(1))
                a(1) = aRealVal
            end if
            nOfVal = nOfVal + 1
        end function
        
        function addInt(a, aIntVal) result(nOfVal)
            integer, intent(inout), dimension(:), allocatable :: a
            integer, intent(in) :: aIntVal
            integer, dimension(:), allocatable :: newA
            integer :: i,nOfVal
            nOfVal = 0
            if (ALLOCATED(a) .eqv. .true.) then
                nOfVal = ubound(a,1)
                !print*, 'following allocate', nOfCropSeq
                ! copy the list
                allocate(newA(nOfVal))
                do i=1, nOfVal
                    newA(i) = a(i)
                end do
                ! clear original
                deallocate(a)
                allocate(a(nOfVal+1))
                do i=1, nOfVal
                    a(i) = newA(i)
                end do
                
                a(nOfVal+1) = aIntVal
            else
                !print*, 'first allocation', nOfCropSeq
                allocate(a(1))
                a(1) = aIntVal
            end if
            nOfVal = nOfVal + 1
        end function

        subroutine addTails(origArray,newArray,window)
            REAL(dp), dimension(:), intent(in) :: origArray
            REAL(dp), dimension(:), ALLOCATABLE, intent(INout) :: newArray
            Integer, intent(inout) :: window
            INTEGER :: origLen, newLen
            
            ! init new array
            origLen = ubound(origArray,1)
            if (origLen<window) then
                window = origLen
            end if
            
            newLen = window+origLen+window
            allocate(newArray(newLen))
            
            ! copy values
            newArray(1:window) = origArray(origLen-window+1:origLen) ! add the last part
            newArray(newLen-window+1:newLen) = origArray(1:window)
            newArray(window+1:newLen-window) = origArray(:)
            
        end subroutine

        subroutine removeTails(origArray,newArray,window)
            REAL(dp), dimension(:), intent(in) :: origArray
            REAL(dp), dimension(:), ALLOCATABLE, intent(INout) :: newArray
            integer, intent(in) :: window
            INTEGER :: origLen, newLen
            
            ! init new array
            origLen = ubound(origArray,1)
            
            newLen = origLen-window-window
            allocate(newArray(newLen))
            
            ! copy values between window
            newArray(1:newLen) = origArray(origLen+window:origLen-window+1)
        end subroutine

        pure function cumsum(a) result (r)
            real(dp), intent(in) :: a(:)
            real(dp) :: r(size(a))
            integer :: i
            r(:) = [(sum(a(1:i)),i=1,size(a))]
        end function cumsum

        pure function round(a,digit) result (r)
            real(dp), intent(in) :: a(:)
            integer, intent(in) :: digit
            real(dp) :: r(size(a))
            real(dp) :: k
            
            k = 10**digit
            r(:) = FLOAT (INT(a * k + 0.5)) / k
        end function round


        pure function movMean(a, n) result(aMean)
            ! n = number of values before and after the target
            real(dp), intent(in) :: a(:)
            integer, intent(in) :: n
            real(dp) ::  aMean(size(a))
            real(dp), allocatable :: aTemp(:), tailCount(:), csum(:), diff(:), aMeanTemp(:)
            integer :: newLen, origLen, after, before, i
            
            ! add tails
            origLen = size(a, dim=1)
            
            newLen = n+origLen+n
            !print*, 'newLen: ', newLen, ' n: ', n
            
            allocate(aTemp(newLen), tailCount(newLen), diff(newLen),aMeanTemp(newLen))
            
            aTemp = 0.
            aTemp(1+n:newLen-n) = a
            
            tailCount = 0.
            tailCount(1+n:newLen-n) = 1.
            
            aMeanTemp = 0.
            
            do i=1+n,newLen-n
                aMeanTemp(i) = sum(aTemp(i-n:i+n))/sum(tailCount(i-n:i+n))
            end do
            
            aMean = aMeanTemp(1+n:newLen-n)
            
        end function

        pure function mean(a) result (r)
            real(dp), intent(in) :: a(:)
            real(dp) :: r
            
            r = sum(a)/(max(1,size(a)))
        end function mean

        pure function fillMissingK(y) result (r)
            real(dp), intent(in) :: y(:)
            real(dp) :: r(size(y))
            integer, allocatable :: idx(:)
            integer :: i, j, nElem, eIdx, sIdx
            integer :: n
            n = size(y)
            r = y
            
            ! fill nan with constant value
            idx = pack([(i,i=1,n)],y /= nodatar,[(0,i=1,n)])
            do i=1,size(idx)-1
                sIdx =idx(i)
                if (sIdx == 0) then
                    exit
                end if
                eIdx = idx(i+1)
                ! note that eIdx element will be overwritten the next cycle
                r(sIdx:eIdx-1) = y(sIdx)
                !r(sIdx:eIdx) = y(sIdx)
            end do
            
        end function

        pure function fillMissingL(y,x) result (r)
            real(dp), intent(in) :: y(:)
            real(dp), optional, intent(in) :: x(:)
            real(dp) :: r(size(y))
            integer, allocatable :: idx(:)
            real(dp), allocatable :: xi(:)
            real(dp) :: m,q
            integer :: i, j, nElem, eIdx, sIdx
            integer :: n
            n = size(y)
            r = y
            
            allocate(xi(n))
            if(present(x)) then
                xi = x(1:n)
            else
                xi=1.
                xi = cumsum(xi)
            end if
            
            ! fill nan with linear interpolation
            idx = pack([(i,i=1,n)],y /= nodatar,[(0,i=1,n)])
            do i=1,size(idx)-1
                sIdx =idx(i)
                if (sIdx == 0) then
                    exit
                end if
                eIdx = idx(i+1)
                if (eIdx == 0) then
                    exit
                end if
                nElem = eIdx-sIdx
                if (xi(eIdx)/=xi(sIdx)) then
                    m = (y(eIdx)-y(sIdx))/(xi(eIdx)-xi(sIdx))
                else
                    m = 0.
                end if
                
                q = y(sIdx)-m*xi(sIdx)
                
                if (nElem > 1) then
                    do j=1,nElem-1
                        r(sIdx+j) = m*xi(sIdx+j)+q
                    end do
                end if    
                
            end do
            
        end function
        
        pure function getValue(x,y,xTarget)result(yTarget)
            real(dp), intent(in) :: x(:), y(:)
            real(dp), intent(in) :: xTarget
            real(dp) :: yTarget
            
            real(dp) :: m,q
            integer :: i, eIdx, sIdx
            
            yTarget = nodatar
            do i=1,size(x)-1
                sIdx = i
                eIdx = i+1
                if ((xTarget>x(sIdx)).and.(xTarget<x(eIdx))) then
                    m = (y(eIdx)-y(sIdx))/(x(eIdx)-x(sIdx))
                    q = y(sIdx)-m*x(sIdx)
                    yTarget = m*xTarget+q
                end if
            end do
            
        end function
        
        subroutine SeekUN( ErrorFlag, free_unit)

            !Function "SeekUN" seeks for the first unit free to be used for
            !read/write operations.
            !Out: free_unit -> number of the first unit free (from 11 to 99)
            !     ErrorFlag -> to check if any error occous...

            implicit none
            integer :: ErrorFlag, free_unit

            logical :: OP !To check if unit is used

            ErrorFlag=0 !If no Error occurs, remains 0
            !Look for a free unit
            do free_unit=11,99
               inquire( UNIT=free_unit, OPENED=OP)
               if( free_unit .eq. 99 ) ErrorFlag=1 ! No unit avaibles
               if(.not. OP) exit ! Not opened unit found=>
               ! Exit and returns the value of the founded free unit 
            enddo

        end subroutine

        function getFlatArea(a) result(r)
            real(dp), intent(in) :: a(:)
            real(dp) :: r(size(a))
            real(dp), dimension(size(a)) :: delta
            real(dp), dimension(size(a)) :: iszero,absdiff
            integer :: i,n
            
            n = size(a)
        
            delta(1) = 1.
            delta(2:n) = a(2:n) - a(1:n-1) ! assign to the end
            delta = round(delta, 6)
            
            ! Create an array that is 1 where a is 0, and pad each end with an extra 0.
            iszero = 0.
            where (delta == 0)
                iszero = 1.
            end where
            
            !~ print*,'sum iszero', sum(iszero)
            
            absdiff = 0.
            absdiff(1:n-1) = abs(iszero(2:n)-iszero(1:n-1)) ! assign to the end
            
            !~ print*,'sum absdiff', sum(absdiff)
            
            
            ! Runs start and end where absdiff is 1.
            r = 0.
            r = pack([(i,i=1,n)],absdiff==1.,[(0,i=1,n)])
            !~ print*,'i         a     delta    iszero   absdiff         r'
            !~ do i=1,size(r)
                !~ write(*,'(I3,f10.3,f10.3,f10.3,f10.3,f10.3)') i,a(i),delta(i),iszero(i),absdiff(i),r(i)
            !~ end do
        end function

        function getDescArea(a) result(r)
            real(dp), intent(in) :: a(:)
            real(dp) :: r(size(a))
            real(dp), dimension(size(a)) :: delta
            real(dp), dimension(size(a)) :: iszero,absdiff
            integer :: i,n
            
            n = size(a)
        
            delta(1) = 0
            delta(2:n) = a(2:n) - a(1:n-1) ! assign to the end
            delta = round(delta, 6)
            
            ! Create an array that is 1 where a is 0, and pad each end with an extra 0.
            iszero = 0.
            where (delta < 0)
                iszero = 1.
            end where
            
            !~ print*,'sum iszero', sum(iszero)
            
            absdiff = 0.
            absdiff(1:n-1) = abs(iszero(2:n)-iszero(1:n-1)) ! assign to the first
            
            !~ print*,'sum absdiff', sum(absdiff)
            
            ! Runs start and end where absdiff is 1.
            r = 0.
            r = pack([(i,i=1,n)],absdiff==1.,[(0,i=1,n)])
            !~ print*,'  i         a       delta      iszero     absdiff         r'
            !~ do i=1,size(r)
                !~ write(*,'(I3,f10.3,f10.3,f10.3,f10.3,f10.3)') i,a(i),delta(i),iszero(i),absdiff(i),r(i)
            !~ end do
        end function

        function getAscArea(a) result(r)
            real(dp), intent(in) :: a(:)
            real(dp) :: r(size(a))
            real(dp), dimension(size(a)) :: delta
            real(dp), dimension(size(a)) :: iszero,absdiff
            integer :: i,n
            
            n = size(a)
        
            delta(1) = a(1)
            delta(2:n) = a(2:n) - a(1:n-1) ! assign to the end
            delta = round(delta, 6)
            
            ! Create an array that is 1 where a is 0, and pad each end with an extra 0.
            iszero = 0.
            where (delta > 0)
                iszero = 1.
            end where
            
            absdiff = 0.
            absdiff(1:n-1) = abs(iszero(2:n)-iszero(1:n-1)) ! assign to the first
            
            ! Runs start and end where absdiff is 1.
            r = pack([(i,i=1,n)],absdiff==1.,[(0,i=1,n)])
        end function

        pure recursive function replace_str(string,search,substitute) result(modifiedString)
            ! https://stackoverflow.com/questions/58938347/how-do-i-replace-a-character-in-the-string-with-another-charater-in-fortran
            implicit none
            character(len=*), intent(in)  :: string, search, substitute
            character(len=:), allocatable :: modifiedString
            integer                       :: i, stringLen, searchLen
            stringLen = len(string)
            searchLen = len(search)
            if (stringLen==0 .or. searchLen==0) then
                modifiedString = ""
                return
            elseif (stringLen<searchLen) then
                modifiedString = string
                return
            end if
            i = 1
            do
                if (string(i:i+searchLen-1)==search) then
                    modifiedString = string(1:i-1) // substitute // replace_str(string(i+searchLen:stringLen),search,substitute)
                    exit
                end if
                if (i+searchLen>stringLen) then
                    modifiedString = string
                    exit
                end if
                i = i + 1
                cycle
            end do
        end function replace_str

end module

!~ program testMovMean
    !~ use mod_utilities
    !~ implicit none
    
    !~ ! real(dp), dimension(8) :: a = (/1.,1.,1.,1.,1.,1.,1.,1./)
    !~ real(dp), dimension(8) :: a = (/1.,2.,3.,4.,5.,6.,7.,8./)
    !~ real(dp), dimension(:), allocatable :: b
    !~ integer :: i
    
    !~ b = movMean(a,1)
    
    !~ do i=1, size(a)
        !~ print*, i, a(i), b(i)
    !~ end do
    
!~ end program


!~ program testRepeatArray
    !~ use mod_utilities
    !~ implicit none
    
    !~ ! real(dp), dimension(8) :: a = (/1.,1.,1.,1.,1.,1.,1.,1./)
    !~ real(dp), dimension(3) :: a = (/1.,2.,3./)
    !~ real(dp), dimension(:), allocatable :: b
    !~ integer :: i
    !~ integer :: n = 3
    
    !~ b = repeatArray(a,n,.false.)
    
    !~ do i=1, size(b)
        !~ print*, i, b(i)
    !~ end do
    
    !~ print*
    
    !~ b = repeatArray(a,n,.true.)
    
    !~ do i=1, size(b)
        !~ print*, i, b(i)
    !~ end do
    
!~ end program

!~ program testSplitString
    !~ use mod_utilities
    !~ implicit none
    
    !~ character(len=maxlength) :: inString = 'par1 = value1'
    !~ character(len=maxlength) :: sep = '='
    !~ character(len=maxlength) :: firstPart
    !~ character(len=maxlength) :: secondPart    
    
    !~ CALL splitString(inString,  sep, firstPart, secondPart)
    
    !~ print*, 'inString: ', trim(adjustl(inString)), len(inString)
    !~ print*, 'sep: ',trim(adjustl(sep)),len(sep)
    !~ print*, 'firstPart: ',trim(adjustl(firstPart)),len(firstPart)
    !~ print*, 'secondPart: ',trim(adjustl(secondPart)),len(secondPart)
    
!~ end program

!~ program testFillMissing
    !~ use mod_utilities
    !~ implicit none
    
    !~ !real(dp), dimension(8) :: a = (/1.,2.,3.,-9999.,-9999.,-9999.,10.,11./)
    !~ real(dp), dimension(8) :: a = (/-9999.,-9999.,-9999.,-9999.,-9999.,-9999.,-9999.,-9999./)
    !~ real(dp), dimension(8) :: c = (/1.,2.,3.,4.,8.,9.,10.,11./)
    !~ real(dp), dimension(:), allocatable :: b
    !~ integer :: i
    
    !~ b = fillMissing(a,c)
    
    !~ print*, 'i    a    c    b'
    !~ do i=1,size(b)
        !~ print*,i,a(i),c(i),b(i)
    !~ end do
    
!~ end program

!~ program testStrToInt
    !~ use mod_utilities
    !~ implicit none
    
    !~ !real(dp), dimension(8) :: a = (/1.,2.,3.,-9999.,-9999.,-9999.,10.,11./)
    !~ character(len=10) :: test = '1'
    !~ integer :: i
    !~ real(dp), dimension(8) :: a = (/1.,2.,3.,-9999.,-9999.,-9999.,10.,11./)
    !~ integer,dimension(:), allocatable :: b
    
    !~ i = strToInt(test)
    
    !~ print*, 'i    = ',i
    
    !~ b = pack([(i,i=1,8)],int(a))
    !~ print '(d10)', b
    
!~ end program

!~ program testRemoveRepChar
    !~ use mod_utilities
    !~ implicit none
    
    !~ character(len=maxlength) :: inString = 'par1   =   value1'
    !~ character :: repCh = ' '
    !~ character(len=maxlength) :: outString
    
    !~ outString = removeRepChar(inString, repCh)
    
    !~ print*, 'inString: ', trim(adjustl(inString))
    !~ print*, 'repCh: ',trim(adjustl(repCh))
    !~ print*, 'outString: ',trim(adjustl(outString))
!~ end program