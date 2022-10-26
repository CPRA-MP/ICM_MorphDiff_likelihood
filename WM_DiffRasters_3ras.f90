!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                  
!   ICM Wetland Morphology Raster Differencing
!                                                  
!                                                  
!   Fortran code to compare two binary rasters saved by ICM-Morph 
!   
!   Command line arguments passed into this executable:
!
!   1 = ras2_bin_pth    : full path to binary raster file for output of interest
!   2 = ras1_bin_pth    : full path to binary raster file for raster of same year that ras2 will be compared to
!   3 = ras0_bin_pth    : full path to binary output for first raster that both ras1 and ras2 will be compared to (ras012 = ras2 - ras1 - ras0)
!   3 = ras012_bin_pth  : full path to binary output file that saves the comparison (ras012 = ras2 - ras1 - ras0)
!   4 = difftype        : type of difference, either categorical land/water comparison or pure subtraction('dlw' is the only option supported by diff_3ras)
!   5 = nras_str        : number of raster pixels of dataset, must match size of binary arrays

!                                                  
!   Questions: eric.white@la.gov                   
!   last update: 10/26/2022
!                                                     
!   project site: https://github.com/CPRA-MP      
!   documentation: http://coastal.la.gov/our-plan  
!                                                  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module params
    character*100 :: ras2_bin_pth
    character*100 :: ras1_bin_pth
    character*100 :: ras0_bin_pth
    character*100 :: ras012_bin_pth
    character*3 :: difftype
    character*20 :: noData_str
    character*20 :: nras_str
    integer,parameter :: sp=selected_real_kind(p=6) 
    integer :: nras
end module params
    
program main
    use params
    implicit none
    call GET_COMMAND_ARGUMENT(1,ras2_bin_pth)
    call GET_COMMAND_ARGUMENT(2,ras1_bin_pth)
    call GET_COMMAND_ARGUMENT(3,ras0_bin_pth)
    call GET_COMMAND_ARGUMENT(4,ras012_bin_pth)
    call GET_COMMAND_ARGUMENT(5,difftype)
    call GET_COMMAND_ARGUMENT(6,nras_str)
    call GET_COMMAND_ARGUMENT(7,noData_str)
    
    read(nras_str,*) nras
    if (difftype == 'dlw') then
        call DiffLW_3ras
    !else if (difftype == 'dz') then
    !    call DiffZ
    else
        write(*,'(a)') 'Improper differencing type entered. Must only be dlw when comparing three different rasters.'
    end if
end program
