!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                  
!   ICM Wetland Morphology Raster Converter
!                                                  
!                                                  
!   Fortran code to convert from binary rasters saved by ICM-Morph to XYZ ascii files
!   XYZ ascii rasters can be converted to any format using GDAL_translate
!   
!   Command line arguments passed into this executable:
!
!   1 = xyz_asc_pth : full path to XYZ file that will be saved by this program
!   3 = x_bin_pth   : full path to binary output file from ICM-Morph that saved array of X-coordinates
!   4 = y_bin_pth   : full path to binary output file from ICM-Morph that saved array of Y-coordinates
!   2 = z_bin_pth   : full path to binary output file from ICM-Morph that saved array of raster values (Z) to be mapped
!   5 = dtype       : data type of Z values must be either 'int' or 'flt'
!   6 = nras_str    : number of raster pixels of dataset, must match size of binary arrays
!                                                  
!   Questions: eric.white@la.gov                   
!   last update: 8/16/2020                          
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
    integer,dimension(:),allocatable :: x
    integer,dimension(:),allocatable :: y
    real(sp),dimension(:),allocatable :: z
    integer :: nras
    real(sp) :: rasval
    integer :: i
    
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
    
    write(*,*) 'x file:', trim(adjustL(x_bin_pth))
    write(*,*) 'y file:', trim(adjustL(y_bin_pth))
    write(*,*) 'z file:', trim(adjustL(z_bin_pth))
    
    open(unit=100, file = trim(adjustL(x_bin_pth)),form='unformatted')
    read(100) x
    close(100)

    open(unit=101, file = trim(adjustL(y_bin_pth)),form='unformatted')
    read(101) y
    close(101)
    
    open(unit=102, file = trim(adjustL(z_bin_pth)),form='unformatted')
    read(102) z
    close(102)
    
    open(unit=200, file = trim(adjustL(xyz_asc_pth)))
    if(dtype == 'int') then
        do i=1,nras
            write(200,2000) x(i),y(i),z(i)
        end do
    else if (dtype =='flt') then
        do i=1,nras
            write(200,2001) x(i),y(i),z(i)
        end do
    end if
    close(200)
    
2000 format(I0,',',I0,',',I0)
2001 format(I0,',',I0,',',F0.4)
    
    
end program
