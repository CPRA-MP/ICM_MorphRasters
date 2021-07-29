!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                  
!   ICM Wetland Morphology Raster Converter
!                                                  
!                                                  
!   Fortran code to convert to inary rasters saved by ICM-Morph from ascii files
!   XYZ ascii rasters can be converted to any format using GDAL_translate
!   
!   Command line arguments passed into this executable:
!
!   1 = xyz_asc_pth : full path to XYZ file that will be passed into this program
!   2 = x_bin_pth   : full path to binary output file from ICM-Morph that saved array of X-coordinates
!   3 = y_bin_pth   : full path to binary output file from ICM-Morph that saved array of Y-coordinates
!   4 = z_bin_pth   : full path to binary output file from ICM-Morph of raster values (Z) that will be saved by this program
!   5 = dtype       : data type of Z values must be either 'int' or 'flt'
!   6 = nras_str    : number of raster pixels of dataset, must match size of binary arrays
!                                                  
!   Questions: eric.white@la.gov                   
!   last update: 7/29/2021
!                                                     
!   project site: https://github.com/CPRA-MP      
!   documentation: http://coastal.la.gov/our-plan  
!                                                  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
program main
    
    implicit none

    character*100 :: xyz_asc_pth
    character*100 :: x_bin_pth
    character*100 :: y_bin_pth
    character*100 :: z_bin_pth
    character*3 :: dtype
    character*20 :: noData_str
    character*20 :: nras_str
    
    integer,parameter :: sp=selected_real_kind(p=6) 
    integer,dimension(:),allocatable :: pct_i
    integer,dimension(:),allocatable :: x
    integer,dimension(:),allocatable :: y
    real(sp),dimension(:),allocatable :: z
    integer :: nras
    real(sp) :: rasval
    integer :: i
    integer :: binsize
    integer :: pct
    
    call GET_COMMAND_ARGUMENT(1,xyz_asc_pth)
    call GET_COMMAND_ARGUMENT(2,x_bin_pth)
    call GET_COMMAND_ARGUMENT(3,y_bin_pth)
    call GET_COMMAND_ARGUMENT(4,z_bin_pth)
    call GET_COMMAND_ARGUMENT(5,dtype)
    call GET_COMMAND_ARGUMENT(6,nras_str)
    
    read(nras_str,*) nras
    
    allocate(x(nras))
    allocate(y(nras))
    allocate(z(nras))
    
    allocate(pct_i(10))
    
    binsize = nras/10
    do i=1,10
        pct_i(i) = binsize*i
    end do
    
    write(*,'(a,a)') 'x file:', trim(adjustL(x_bin_pth))
    write(*,'(a,a)') 'y file:', trim(adjustL(y_bin_pth))
    write(*,'(a,a)') 'z file:', trim(adjustL(z_bin_pth))
    
    
    open(unit=200, file = trim(adjustL(xyz_asc_pth)))
    write(*,'(A,A)') 'reading input ',trim(adjustL(xyz_asc_pth))

    pct = 0
    write(*,'(I3,A)') pct,'%... '

    do i=1,nras
        read(200,*) x(i),y(i),z(i)  
            
        if ( ANY(pct_i == i) ) then
            pct = pct + 10
            write(*,'(I3,A)') pct,'%... '
        end if
    end do
    close(200)
    
    open(unit=102, file = trim(adjustL(z_bin_pth)),form='unformatted')
    write(102) z
    close(102)

end program
