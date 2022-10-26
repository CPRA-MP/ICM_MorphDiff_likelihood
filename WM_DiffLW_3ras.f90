subroutine DiffLW_3ras
    ! lndtyp 1 = vegetated wetland
    ! lndtyp 2 = water
    ! lndtyp 3 = unvegetated wetland/new subaerial unvegetated mudflat (e.g., bare ground)
    ! lndtyp 4 = developed land/upland/etc. that are not modeled in ICM-LAVegMod
    ! lndtyp 5 = flotant marsh
    
    use params
    implicit none

    
    integer,dimension(:),allocatable :: ras2
    integer,dimension(:),allocatable :: ras1
    integer,dimension(:),allocatable :: ras0
    integer,dimension(:),allocatable :: ras012
    integer :: noData
    integer :: i
    

    read(noData_str,*) noData
    
    allocate(ras2(nras))
    allocate(ras1(nras))
    allocate(ras0(nras))
    allocate(ras012(nras))
    
    ras2    = 0
    ras1    = 0
    ras0    = 0
    ras012  = 0
    
    write(*,'(a,a)') 'comparing:  ', trim(adjustL(ras1_bin_pth))
    write(*,'(a,a)') '       to:  ', trim(adjustL(ras0_bin_pth))
    
    open(unit=100, file = trim(adjustL(ras2_bin_pth)),form='unformatted')
    read(100) ras2
    close(100)

    open(unit=101, file = trim(adjustL(ras1_bin_pth)),form='unformatted')
    read(101) ras1
    close(101)
    
    open(unit=102, file = trim(adjustL(ras0_bin_pth)),form='unformatted')
    read(102) ras0
    close(102)
    

    do i=1,nras
        if (ras2(i) == noData) then
            ras012(i) = noData
        elseif (ras1(i) == noData) then
            ras012(i) = noData
        elseif (ras0(i) == noData) then
            ras012(i) = noData
        else
            ras012(i) = 100*ras0(i) + 10*ras1(i) + ras2(i)
        end if
    end do

    open(unit=200, file = trim(adjustL(ras012_bin_pth)) , form='unformatted' )
    write(*,'(A,A)') ' saved to: ',trim(adjustL(ras012_bin_pth))    
    write(200) ras012
    close(200)
    
    
    return
end


    !  ras012 = 111     Initial:  vegetated wetland     FWOA:  vegetated wetland    FWA:  vegetated wetland     ! no difference in landtype
    !  ras012 = 112     Initial:  vegetated wetland     FWOA:  vegetated wetland    FWA:  water                 ! induced loss
    !  ras012 = 113     Initial:  vegetated wetland     FWOA:  vegetated wetland    FWA:  bare wetland          ! induced vegetation loss
    !  ras012 = 121     Initial:  vegetated wetland     FWOA:  water                FWA:  vegetated wetland     ! land maintained
    !  ras012 = 122     Initial:  vegetated wetland     FWOA:  water                FWA:  water                 ! loss under both
    !  ras012 = 123     Initial:  vegetated wetland     FWOA:  water                FWA:  bare wetland          
    !  ras012 = 131     Initial:  vegetated wetland     FWOA:  bare wetland         FWA:  vegetated wetland     
    !  ras012 = 132     Initial:  vegetated wetland     FWOA:  bare wetland         FWA:  water                 
    !  ras012 = 133     Initial:  vegetated wetland     FWOA:  bare wetland         FWA:  bare wetland          
    !  ras012 = 211     Initial:  water                 FWOA:  vegetated wetland    FWA:  vegetated wetland     
    !  ras012 = 212     Initial:  water                 FWOA:  vegetated wetland    FWA:  water                 
    !  ras012 = 213     Initial:  water                 FWOA:  vegetated wetland    FWA:  bare wetland          
    !  ras012 = 221     Initial:  water                 FWOA:  water                FWA:  vegetated wetland     ! land gain
    !  ras012 = 222     Initial:  water                 FWOA:  water                FWA:  water                 ! no difference in landtype
    !  ras012 = 223     Initial:  water                 FWOA:  water                FWA:  bare wetland          
    !  ras012 = 231     Initial:  water                 FWOA:  bare wetland         FWA:  vegetated wetland     
    !  ras012 = 232     Initial:  water                 FWOA:  bare wetland         FWA:  water                 
    !  ras012 = 233     Initial:  water                 FWOA:  bare wetland         FWA:  bare wetland          
    !  ras012 = 311     Initial:  bare wetland          FWOA:  vegetated wetland    FWA:  vegetated wetland     
    !  ras012 = 312     Initial:  bare wetland          FWOA:  vegetated wetland    FWA:  water                 
    !  ras012 = 313     Initial:  bare wetland          FWOA:  vegetated wetland    FWA:  bare wetland          
    !  ras012 = 321     Initial:  bare wetland          FWOA:  water                FWA:  vegetated wetland     
    !  ras012 = 322     Initial:  bare wetland          FWOA:  water                FWA:  water                 
    !  ras012 = 323     Initial:  bare wetland          FWOA:  water                FWA:  bare wetland          
    !  ras012 = 331     Initial:  bare wetland          FWOA:  bare wetland         FWA:  vegetated wetland     
    !  ras012 = 332     Initial:  bare wetland          FWOA:  bare wetland         FWA:  water                 
    !  ras012 = 333     Initial:  bare wetland          FWOA:  bare wetland         FWA:  bare wetland          ! no difference in landtype
    !  ras012 = 444     Initial:  developed/upland      FWOA:  developed/upland     FWA:  developed/upland      ! no difference in landtype
    !  ras012 = 511     Initial:  flotant               FWOA:  vegetated wetland    FWA:  vegetated wetland     
    !  ras012 = 512     Initial:  flotant               FWOA:  vegetated wetland    FWA:  water                 
    !  ras012 = 513     Initial:  flotant               FWOA:  vegetated wetland    FWA:  bare wetland          
    !  ras012 = 515     Initial:  flotant               FWOA:  vegetated wetland    FWA:  flotant               
    !  ras012 = 521     Initial:  flotant               FWOA:  water                FWA:  vegetated wetland     
    !  ras012 = 522     Initial:  flotant               FWOA:  water                FWA:  water                 
    !  ras012 = 523     Initial:  flotant               FWOA:  water                FWA:  bare wetland          
    !  ras012 = 525     Initial:  flotant               FWOA:  water                FWA:  flotant               
    !  ras012 = 531     Initial:  flotant               FWOA:  bare wetland         FWA:  vegetated wetland     
    !  ras012 = 532     Initial:  flotant               FWOA:  bare wetland         FWA:  water                 
    !  ras012 = 533     Initial:  flotant               FWOA:  bare wetland         FWA:  bare wetland          
    !  ras012 = 535     Initial:  flotant               FWOA:  bare wetland         FWA:  flotant               
    !  ras012 = 551     Initial:  flotant               FWOA:  flotant              FWA:  vegetated wetland     
    !  ras012 = 552     Initial:  flotant               FWOA:  flotant              FWA:  water                 
    !  ras012 = 553     Initial:  flotant               FWOA:  flotant              FWA:  bare wetland          
    !  ras012 = 555     Initial:  flotant               FWOA:  flotant              FWA:  flotant               ! no difference in landtype
    
    ! ras012 combinations that should never occur
    !  ras012 = 114     Initial:  vegetated wetland     FWOA:  vegetated wetland    FWA:  developed/upland      ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 115     Initial:  vegetated wetland     FWOA:  vegetated wetland    FWA:  flotant               ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 124     Initial:  vegetated wetland     FWOA:  water                FWA:  developed/upland      ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 125     Initial:  vegetated wetland     FWOA:  water                FWA:  flotant               ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 134     Initial:  vegetated wetland     FWOA:  bare wetland         FWA:  developed/upland      ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 135     Initial:  vegetated wetland     FWOA:  bare wetland         FWA:  flotant               ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 141     Initial:  vegetated wetland     FWOA:  developed/upland     FWA:  vegetated wetland     ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 142     Initial:  vegetated wetland     FWOA:  developed/upland     FWA:  water                 ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 143     Initial:  vegetated wetland     FWOA:  developed/upland     FWA:  bare wetland          ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 144     Initial:  vegetated wetland     FWOA:  developed/upland     FWA:  developed/upland      ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 145     Initial:  vegetated wetland     FWOA:  developed/upland     FWA:  flotant               ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 151     Initial:  vegetated wetland     FWOA:  flotant              FWA:  vegetated wetland     ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 152     Initial:  vegetated wetland     FWOA:  flotant              FWA:  water                 ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 153     Initial:  vegetated wetland     FWOA:  flotant              FWA:  bare wetland          ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 154     Initial:  vegetated wetland     FWOA:  flotant              FWA:  developed/upland      ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 155     Initial:  vegetated wetland     FWOA:  flotant              FWA:  flotant               ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 214     Initial:  water                 FWOA:  vegetated wetland    FWA:  developed/upland      ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 215     Initial:  water                 FWOA:  vegetated wetland    FWA:  flotant               ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 224     Initial:  water                 FWOA:  water                FWA:  developed/upland      ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 225     Initial:  water                 FWOA:  water                FWA:  flotant               ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 234     Initial:  water                 FWOA:  bare wetland         FWA:  developed/upland      ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 235     Initial:  water                 FWOA:  bare wetland         FWA:  flotant               ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 241     Initial:  water                 FWOA:  developed/upland     FWA:  vegetated wetland     ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 242     Initial:  water                 FWOA:  developed/upland     FWA:  water                 ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 243     Initial:  water                 FWOA:  developed/upland     FWA:  bare wetland          ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 244     Initial:  water                 FWOA:  developed/upland     FWA:  developed/upland      ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 245     Initial:  water                 FWOA:  developed/upland     FWA:  flotant               ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 251     Initial:  water                 FWOA:  flotant              FWA:  vegetated wetland     ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 252     Initial:  water                 FWOA:  flotant              FWA:  water                 ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 253     Initial:  water                 FWOA:  flotant              FWA:  bare wetland          ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 254     Initial:  water                 FWOA:  flotant              FWA:  developed/upland      ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 255     Initial:  water                 FWOA:  flotant              FWA:  flotant               ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 314     Initial:  bare wetland          FWOA:  vegetated wetland    FWA:  developed/upland      ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 315     Initial:  bare wetland          FWOA:  vegetated wetland    FWA:  flotant               ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 324     Initial:  bare wetland          FWOA:  water                FWA:  developed/upland      ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 325     Initial:  bare wetland          FWOA:  water                FWA:  flotant               ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 334     Initial:  bare wetland          FWOA:  bare wetland         FWA:  developed/upland      ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 335     Initial:  bare wetland          FWOA:  bare wetland         FWA:  flotant               ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 341     Initial:  bare wetland          FWOA:  developed/upland     FWA:  vegetated wetland     ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 342     Initial:  bare wetland          FWOA:  developed/upland     FWA:  water                 ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 343     Initial:  bare wetland          FWOA:  developed/upland     FWA:  bare wetland          ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 344     Initial:  bare wetland          FWOA:  developed/upland     FWA:  developed/upland      ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 345     Initial:  bare wetland          FWOA:  developed/upland     FWA:  flotant               ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 351     Initial:  bare wetland          FWOA:  flotant              FWA:  vegetated wetland     ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 352     Initial:  bare wetland          FWOA:  flotant              FWA:  water                 ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 353     Initial:  bare wetland          FWOA:  flotant              FWA:  bare wetland          ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 354     Initial:  bare wetland          FWOA:  flotant              FWA:  developed/upland      ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 355     Initial:  bare wetland          FWOA:  flotant              FWA:  flotant               ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 411     Initial:  developed/upland      FWOA:  vegetated wetland    FWA:  vegetated wetland     ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 412     Initial:  developed/upland      FWOA:  vegetated wetland    FWA:  water                 ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 413     Initial:  developed/upland      FWOA:  vegetated wetland    FWA:  bare wetland          ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 414     Initial:  developed/upland      FWOA:  vegetated wetland    FWA:  developed/upland      ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 415     Initial:  developed/upland      FWOA:  vegetated wetland    FWA:  flotant               ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 421     Initial:  developed/upland      FWOA:  water                FWA:  vegetated wetland     ! this combination should not appear since this process is not currently in ICM-Morph
    !  ras012 = 422     Initial:  developed/upland      FWOA:  water                FWA:  water                 ! this combination should not appear since this process is not currently in ICM-Morph


