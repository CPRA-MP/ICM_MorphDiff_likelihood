!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                  
!   ICM Wetland Morphology Raster Likelihood Coutns
!                                                  
!                                                  
!   Fortran code to count the number of times each raster pixel is equal to a specific value.
!   The value to count instances of is passed into the program as a runtime variable.
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
!   last update: 03/16/2023
!                                                     
!   project site: https://github.com/CPRA-MP      
!   documentation: http://coastal.la.gov/our-plan  
!                                                  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module params
    character*100 :: ras_list_pth
    character*100 :: ras_out_pth
    character*20 :: ras_val_str
    character*20 :: noData_str
    character*20 :: nras_str
    integer,parameter :: sp=selected_real_kind(p=6) 
    integer :: ras_val
    integer :: nras
    integer :: noData
    integer :: io
    integer :: ras_cnt
    integer :: n
    integer,dimension(:),allocatable :: count_ras
    integer,dimension(:),allocatable :: input_ras
    
    
end module params
    
program main
    use params
    implicit none
    call GET_COMMAND_ARGUMENT(1,ras_list_pth)
    call GET_COMMAND_ARGUMENT(2,ras_out_pth)
    call GET_COMMAND_ARGUMENT(3,ras_val_str)
    call GET_COMMAND_ARGUMENT(4,nras_str)
    call GET_COMMAND_ARGUMENT(5,noData_str)
    
    read(ras_val_str,*) ras_val
    read(nras_str,*) nras
    read(noData_str,*) noData
    
    allocate(count_ras(nras))
    allocate(input_ras(nras))
    
    count_ras = 0
    input_ras = noData
    
    write(*,'(a)') 'Reading in list of rasters to analyze.'
    
    open(unit=1, file=trim(adjustL(ras_list_pth)),status='old',action='read')
    ras_cnt = 0
    do
        read(1,*,*,iostat=io)
        if (io /= 0) exit
        ras_cnt = ras_cnt + 1
    end do
    
    write(*,'(A,I0,A)') 'Found ',ras_cnt,' rasters to compare.'
    
    rewind(2)
    do n = 1,ras_cnt
            
    
    
    
    close(1)
    
    
    
end program
