module mod_truncation_warnings

    use mod_utilities, only: maxlength, intToStr

    implicit none
    private

    integer, parameter, public :: warning_early_harvest = 1
    integer, parameter, public :: warning_not_sown = 2

    public :: resetCropTruncationWarnings
    public :: recordCropTruncationWarning
    public :: printCropTruncationWarningSummary

    abstract interface
        subroutine warning_print_interface(text)
            character(len=*) :: text
        end subroutine warning_print_interface
    end interface

    type crop_truncation_summary
        integer :: land_use_id = 0
        integer :: previous_crop_id = 0
        integer :: new_crop_id = 0
        integer :: warning_kind = 0
        integer :: occurrences = 0
        integer :: min_days_removed = huge(0)
        integer :: max_days_removed = 0
        character(len=maxlength) :: previous_crop_name = ''
        character(len=maxlength) :: new_crop_name = ''
        integer, dimension(:), allocatable :: weather_station_ids
        integer, dimension(:), allocatable :: calendar_years
    end type crop_truncation_summary

    type(crop_truncation_summary), dimension(:), allocatable, save :: warning_summaries
    logical, save :: print_detailed_warnings = .false.

    contains

    subroutine resetCropTruncationWarnings(printDetails)
        logical, intent(in) :: printDetails

        if (allocated(warning_summaries)) deallocate(warning_summaries)
        print_detailed_warnings = printDetails
    end subroutine resetCropTruncationWarnings

    subroutine recordCropTruncationWarning(landUseId, previousCropId, newCropId, warningKind, previousCropName, newCropName, &
                                         & segmentStart, segmentEnd, sowingDay, weatherStationId, calendarYear, printFun     )
        integer, intent(in) :: landUseId, previousCropId, newCropId, warningKind
        integer, intent(in) :: segmentStart, segmentEnd, sowingDay
        integer, intent(in) :: weatherStationId, calendarYear
        character(len=*), intent(in) :: previousCropName, newCropName
        procedure(warning_print_interface) :: printFun
        character(len=4*maxlength) :: msg, details
        integer :: daysRemoved

        daysRemoved = segmentEnd-segmentStart+1
        if (warningKind == warning_not_sown) then
            msg = 'WARNING: '//trim(previousCropName)//' was not sown to make space for '//trim(newCropName)
            details = trim(countLabel(daysRemoved,'scheduled day removed','scheduled days removed'))// &
                      ': days '//trim(adjustl(intToStr(segmentStart)))//' to '// &
                      trim(adjustl(intToStr(segmentEnd)))//'; new crop sown on day '// &
                      trim(adjustl(intToStr(sowingDay)))
        else
            msg = 'WARNING: '//trim(previousCropName)//' was harvested early to make space for '//trim(newCropName)
            if (daysRemoved == 1) then
                details = '1 day cut: day '//trim(adjustl(intToStr(segmentStart)))// &
                          '; new crop sown on day '//trim(adjustl(intToStr(sowingDay)))
            else
                details = trim(adjustl(intToStr(daysRemoved)))//' days cut: days '// &
                          trim(adjustl(intToStr(segmentStart)))//' to '// &
                          trim(adjustl(intToStr(segmentEnd)))//'; new crop sown on day '// &
                          trim(adjustl(intToStr(sowingDay)))
            end if
        end if

        call addToSummary(landUseId, previousCropId, newCropId, warningKind, previousCropName, &
                          newCropName, daysRemoved, weatherStationId, calendarYear)

        ! %PS% Detailed occurrences are printed in debug logs and with -verbose.
        call printFun(trim(msg))
        call printFun('  '//trim(details))
        if (print_detailed_warnings) then
            print *, trim(msg)
            print *, '  '//trim(details)
        end if
    end subroutine recordCropTruncationWarning

    subroutine printCropTruncationWarningSummary()
        integer, dimension(:), allocatable :: summaryOrder
        integer :: i, j, tempIndex, summaryIndex, nYears, nStations
        character(len=4*maxlength) :: msg, details, removedRange

        if (.not. allocated(warning_summaries)) return
        if (size(warning_summaries) == 0) return

        allocate(summaryOrder(size(warning_summaries)))
        do i=1,size(summaryOrder)
            summaryOrder(i) = i
        end do
        do i=1,size(summaryOrder)-1
            do j=1,size(summaryOrder)-i
                if (summaryComesAfter(warning_summaries(summaryOrder(j)), warning_summaries(summaryOrder(j+1)))) then
                    tempIndex = summaryOrder(j)
                    summaryOrder(j) = summaryOrder(j+1)
                    summaryOrder(j+1) = tempIndex
                end if
            end do
        end do

        print *
        print *, 'Crop truncation warning summary'
        do i=1,size(summaryOrder)
            summaryIndex = summaryOrder(i)
            nYears = size(warning_summaries(summaryIndex)%calendar_years)
            nStations = size(warning_summaries(summaryIndex)%weather_station_ids)

            if (warning_summaries(summaryIndex)%warning_kind == warning_not_sown) then
                msg = 'Warning: land-use '//trim(adjustl(intToStr(warning_summaries(summaryIndex)%land_use_id)))// &
                      ': '//trim(warning_summaries(summaryIndex)%previous_crop_name)// &
                      ' was not sown because of '//trim(warning_summaries(summaryIndex)%new_crop_name)
                removedRange = daysRemovedLabel(warning_summaries(summaryIndex),.true.)
            else
                msg = 'Warning: land-use '//trim(adjustl(intToStr(warning_summaries(summaryIndex)%land_use_id)))// &
                      ': '//trim(warning_summaries(summaryIndex)%previous_crop_name)// &
                      ' was harvested early because of '//trim(warning_summaries(summaryIndex)%new_crop_name)
                removedRange = daysRemovedLabel(warning_summaries(summaryIndex),.false.)
            end if

            details = trim(countLabel(warning_summaries(summaryIndex)%occurrences,'time','times'))// &
                      ' across '//trim(countLabel(nYears,'year','years'))// &
                      ' and '//trim(countLabel(nStations,'weather station','weather stations'))//'; '// &
                      trim(removedRange)
            print *, trim(msg)
            print *, "   "//trim(details)
        end do
    end subroutine printCropTruncationWarningSummary

    subroutine addToSummary(landUseId, previousCropId, newCropId, warningKind, previousCropName, &
                           & newCropName, daysRemoved, weatherStationId, calendarYear)
        integer, intent(in) :: landUseId, previousCropId, newCropId, warningKind
        integer, intent(in) :: daysRemoved, weatherStationId, calendarYear
        character(len=*), intent(in) :: previousCropName, newCropName
        type(crop_truncation_summary), dimension(:), allocatable :: expandedSummaries
        integer :: i, summaryIndex, nSummaries

        summaryIndex = 0
        if (allocated(warning_summaries)) then
            do i=1,size(warning_summaries)
                if (warning_summaries(i)%land_use_id == landUseId .and. &
                    warning_summaries(i)%previous_crop_id == previousCropId .and. &
                    warning_summaries(i)%new_crop_id == newCropId .and. &
                    warning_summaries(i)%warning_kind == warningKind) then
                    summaryIndex = i
                    exit
                end if
            end do
        end if

        if (summaryIndex == 0) then
            if (allocated(warning_summaries)) then
                nSummaries = size(warning_summaries)
                allocate(expandedSummaries(nSummaries+1))
                expandedSummaries(1:nSummaries) = warning_summaries
                call move_alloc(expandedSummaries,warning_summaries)
            else
                allocate(warning_summaries(1))
            end if

            summaryIndex = size(warning_summaries)
            warning_summaries(summaryIndex)%land_use_id = landUseId
            warning_summaries(summaryIndex)%previous_crop_id = previousCropId
            warning_summaries(summaryIndex)%new_crop_id = newCropId
            warning_summaries(summaryIndex)%warning_kind = warningKind
            warning_summaries(summaryIndex)%previous_crop_name = previousCropName
            warning_summaries(summaryIndex)%new_crop_name = newCropName
        end if

        warning_summaries(summaryIndex)%occurrences = warning_summaries(summaryIndex)%occurrences + 1
        warning_summaries(summaryIndex)%min_days_removed = &
            min(warning_summaries(summaryIndex)%min_days_removed,daysRemoved)
        warning_summaries(summaryIndex)%max_days_removed = &
            max(warning_summaries(summaryIndex)%max_days_removed,daysRemoved)
        call addUniqueInteger(warning_summaries(summaryIndex)%weather_station_ids,weatherStationId)
        call addUniqueInteger(warning_summaries(summaryIndex)%calendar_years,calendarYear)
    end subroutine addToSummary

    subroutine addUniqueInteger(values,newValue)
        integer, dimension(:), allocatable, intent(inout) :: values
        integer, intent(in) :: newValue
        integer, dimension(:), allocatable :: expandedValues
        integer :: nValues

        if (.not. allocated(values)) then
            allocate(values(1))
            values(1) = newValue
            return
        end if
        if (any(values == newValue)) return

        nValues = size(values)
        allocate(expandedValues(nValues+1))
        expandedValues(1:nValues) = values
        expandedValues(nValues+1) = newValue
        call move_alloc(expandedValues,values)
    end subroutine addUniqueInteger

    function countLabel(value,singularLabel,pluralLabel) result(text)
        integer, intent(in) :: value
        character(len=*), intent(in) :: singularLabel, pluralLabel
        character(len=2*maxlength) :: text

        if (value == 1) then
            text = trim(adjustl(intToStr(value)))//' '//singularLabel
        else
            text = trim(adjustl(intToStr(value)))//' '//pluralLabel
        end if
    end function countLabel

    function daysRemovedLabel(summary,scheduledDays) result(text)
        type(crop_truncation_summary), intent(in) :: summary
        logical, intent(in) :: scheduledDays
        character(len=2*maxlength) :: text

        if (summary%min_days_removed == summary%max_days_removed) then
            if (scheduledDays) then
                text = countLabel(summary%min_days_removed,'scheduled day removed','scheduled days removed')
            else
                text = countLabel(summary%min_days_removed,'day cut','days cut')
            end if
        else if (scheduledDays) then
            text = trim(adjustl(intToStr(summary%min_days_removed)))//'-'// &
                   trim(adjustl(intToStr(summary%max_days_removed)))//' scheduled days removed'
        else
            text = trim(adjustl(intToStr(summary%min_days_removed)))//'-'// &
                   trim(adjustl(intToStr(summary%max_days_removed)))//' days cut'
        end if
    end function daysRemovedLabel

    logical function summaryComesAfter(leftSummary,rightSummary)
        type(crop_truncation_summary), intent(in) :: leftSummary, rightSummary

        if (leftSummary%land_use_id /= rightSummary%land_use_id) then
            summaryComesAfter = leftSummary%land_use_id > rightSummary%land_use_id
        else if (leftSummary%previous_crop_id /= rightSummary%previous_crop_id) then
            summaryComesAfter = leftSummary%previous_crop_id > rightSummary%previous_crop_id
        else if (leftSummary%new_crop_id /= rightSummary%new_crop_id) then
            summaryComesAfter = leftSummary%new_crop_id > rightSummary%new_crop_id
        else
            summaryComesAfter = leftSummary%warning_kind > rightSummary%warning_kind
        end if
    end function summaryComesAfter

end module mod_truncation_warnings
