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

module mod_cropseq
    use mod_crop
    Use mod_utilities
    implicit none
    
    type CropSeq
        character(LEN=maxlength) :: cropSeqName ! Crop sequence denomination
        integer :: cropSeqId ! unique id identification
        type(Crop), dimension(:), allocatable :: cropList
        integer :: nOfCrops = 0
    end type
    
    contains
    
        subroutine addCrop(aCropSeq, aCrop)
            type(CropSeq), intent(inout) :: aCropSeq
            type(Crop), intent(in) :: aCrop
            type(Crop), dimension(:), allocatable :: newCropList
            integer :: i
            
            !if (aCropSeq%nOfCrops>0) then
            if (ALLOCATED(aCropSeq%cropList) .eqv. .true.) then
                aCropSeq%nOfCrops = size(aCropSeq%cropList, DIM=1)
                ! copy the list
                allocate(newCropList(aCropSeq%nOfCrops))
                do i=1, aCropSeq%nOfCrops
                    newCropList(i) = aCropSeq%cropList(i)
                end do
                ! clear original
                deallocate(aCropSeq%cropList)
                allocate(aCropSeq%cropList(aCropSeq%nOfCrops+1))
                do i=1, aCropSeq%nOfCrops
                    aCropSeq%cropList(i) = newCropList(i)
                end do
                
                !aCropSeq%cropList(1:nOfCrops) = newCropList
                aCropSeq%cropList(aCropSeq%nOfCrops+1) = aCrop
            else
                allocate(aCropSeq%cropList(1))
                aCropSeq%cropList(1) = aCrop
            end if
            aCropSeq%nOfCrops = aCropSeq%nOfCrops + 1
        end subroutine
        
        
        function addCropSeq(aCropSeqList, aCropSeq) result(nOfCropSeq)
            type(CropSeq), intent(inout), dimension(:), allocatable :: aCropSeqList
            Type(CropSeq), intent(in) :: aCropSeq
            type(CropSeq), dimension(:), allocatable :: newCropSeqList
            integer :: i,nOfCropSeq
            nOfCropSeq = 0
            if (ALLOCATED(aCropSeqList) .eqv. .true.) then
                nOfCropSeq = ubound(aCropSeqList,1)
                !print*, 'following allocate', nOfCropSeq
                ! copy the list
                allocate(newCropSeqList(nOfCropSeq))
                do i=1, nOfCropSeq
                    newCropSeqList(i) = aCropSeqList(i)
                end do
                ! clear original
                deallocate(aCropSeqList)
                allocate(aCropSeqList(nOfCropSeq+1))
                do i=1, nOfCropSeq
                    aCropSeqList(i) = newCropSeqList(i)
                end do
                
                !aCropSeq%cropList(1:nOfCrops) = newCropList
                aCropSeqList(nOfCropSeq+1) = aCropSeq
            else
                !print*, 'first allocation', nOfCropSeq
                allocate(aCropSeqList(1))
                aCropSeqList(1) = aCropSeq
            end if
            nOfCropSeq = nOfCropSeq + 1
        end function
        
        subroutine clearCropList(aCropSeq)
            type(CropSeq), intent(inout) :: aCropSeq
            deallocate(aCropSeq%cropList)
            aCropSeq%nOfCrops = 0
        end subroutine
        
        
end module
