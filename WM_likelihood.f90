!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                  
!   ICM Wetland Morphology Raster Likelihood Counts
!                                                  
!                                                  
!   Fortran code to count the number of times each raster pixel is not equal to a specific value.
!   The value to count instances of non-equaliy is passed into the program as a runtime variable.
!   The list of rasters (with full path) to be examined is passed in as a filepath.
!   The raster types should be the binary rasters saved by ICM-Morph.
!   
!   Command line arguments passed into this executable:
!
!   1 = ras_list_pth    : full path to text file that has the fullpaths to each binary raster that will be read in as part of this analysis
!   2 = ras_out_pth     : full path to binary output where for first raster that both ras1 and ras2 will be compared to (ras012 = ras2 - ras1 - ras0)
!   3 = ras_val_str     : value (in integer) that will be used to count the number of instances of (e.g., if we want to count the instances of land, ras_value = 1)
!   4 = nras_str        : number of raster pixels of dataset, must match size of binary arrays
!   5 = noData_str      : value of NoData in the input raster datasets
!                                                  
!   Questions: eric.white@la.gov                   
!   last update: 03/21/2023
!                                                     
!   project site: https://github.com/CPRA-MP      
!   documentation: http://coastal.la.gov/our-plan  
!                                                  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module params
    character*100 :: ras_list_pth                       ! input text string - file path to text file that contains filepaths to each raster to be used in analysis
    character*100 :: ras_out_pth                        ! input text string - file path to output binary raster saved by this program
    character*20 :: ras_val_str                         ! input text string - exclusion value to be used for analysis - this value will be used to set what value to exclude from likelihood calculations (e.g., if set to 2, program will count likelihood of pixel NOT being equal to 2)
    character*20 :: noData_str                          ! input text string - no data value of raster (e.g., -9999)
    character*20 :: nras_str                            ! input text string - number of pixels in raster
    character*100 :: ras_file_pth                       ! temp text string  - file path to raster being read for analysis
    integer :: ras_val                                  ! integer version of ras_val_str
    integer :: nras                                     ! integer version of nras_str
    integer :: noData                                   ! integer version of noData_str
    integer :: io                                       ! flag used for looping through raster file list
    integer :: ras_cnt                                  ! number of rasters being analyzed
    integer :: n,m                                      ! iterators
    integer,dimension(:),allocatable :: count_ras       ! array that has the count, at each pixel, of the number of times each pixel was not equal to ras_val
    integer,dimension(:),allocatable :: input_ras       ! array used to store each individual raster read into program
end module params
    
program main
    use params
    implicit none
    
    ! read in input variables passed into executable
    call GET_COMMAND_ARGUMENT(1,ras_list_pth)
    call GET_COMMAND_ARGUMENT(2,ras_out_pth)
    call GET_COMMAND_ARGUMENT(3,ras_val_str)
    call GET_COMMAND_ARGUMENT(4,nras_str)
    call GET_COMMAND_ARGUMENT(5,noData_str)
    
    ! convert text variables read in into integers
    read(ras_val_str,*) ras_val
    read(nras_str,*) nras
    read(noData_str,*) noData
    
    ! allocate and intialize arrays
    allocate(count_ras(nras))
    count_ras = 0
    
    allocate(input_ras(nras))
    input_ras = noData
    
       
    ! read in list of rasters to be included in this analysis
    write(*,'(a)') 'Reading in list of rasters to analyze.'
    open(unit=1, file=trim(adjustL(ras_list_pth)),status='old',action='read')
    ras_cnt = 0
    do
        read(1,*,*,iostat=io)
        if (io /= 0) exit
        ras_cnt = ras_cnt + 1
    end do
    rewind(1)
    write(*,'(A,I0,A)') 'Found ',ras_cnt,' rasters to compare.'
    
    ! loop over list of rasters and process each one at a time
    do n = 1,ras_cnt
        input_ras = noData
        read(1,*) ras_file_path
        
        ! read in raster binary file
        write(*,'(A,A)') ' - reading in ', trim(adjustL(ras_file_path))
        open(unit=2,file=trim(adjustL(ras_file_path)) )
        read(2) input_ras
        close(2)
        
        ! loop over landwater raster and add any non-water pixels to the overall count_ras
        do n = 1,nras
            val = nras(n)
            if (val /= ras_val) then
                count_ras(n) = count_ras(n) + 1
            end if
        end do
    end do
        
    close(1)
    
    ! write output file
    write(*,'(a)') 'Writing output file to ',trim(adjustL(ras_out_path))
    open(unit=3, file=trim(adjustL(ras_out_pth)),form='unformatted')
    write(3) count_ras
    close(3)
end program
