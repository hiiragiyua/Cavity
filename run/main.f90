! Fractional step cavity
! central difference, Euler, SOR
module parameters
  implicit none
  real(8), parameter :: Uwall =0.1d0
  real(8), parameter :: Kvis = 1.d-6
  real(8), parameter :: Dens = 1.d3

  real(8), parameter :: Lx = 0.1d0
  real(8), parameter :: Ly = Lx
  integer, parameter :: Nx=41
  integer, parameter :: Ny=43

  real(8), parameter :: accel = 1.925d0
  real(8), parameter :: err_tol = 1.0d-9
  real(8), parameter :: tiny = 1.0d-20

  real(8), parameter :: dx = Lx/dble(Nx-1)
  real(8), parameter :: dy = Lx/dble(Ny-1)

  real(8), parameter :: ndt = 0.1d0
  real(8), parameter :: dt = ndt*dx/Uwall
  real(8), parameter :: endT=50
  integer, parameter :: Nt = int(endT*Lx/Uwall/dt)

  character*256, parameter :: file_dir='./pool/'
  integer, parameter :: interval_w = 100

end module parameters

module global_fwd
  implicit none
  real(8),dimension(:),allocatable :: x,y
  real(8),dimension(:,:),allocatable :: u,v,p
  real(8),dimension(:,:),allocatable :: uaux,vaux,dive

end module global_fwd

program main

  use parameters, only: Nx,Ny,Nt,interval_w
  use global_fwd

  implicit none

  integer iter, ifile
  real(8) t1,t0

  allocate (x(0:Nx+1),y(0:Ny+1))

  allocate (u(0:Nx+1,0:Ny  ))
  allocate (v(0:Nx,  0:Ny+1))
  allocate (p(0:Nx,  0:Ny  ))

  allocate (uaux(0:Nx+1,0:Ny  ))
  allocate (vaux(0:Nx,  0:Ny+1))
  allocate (dive(0:Nx,  0:Ny  ))

  ifile=0
  call init(u,v,p)
  call write_file_bin(ifile); ifile=ifile+1

  do iter=1,Nt
     call cpu_time(t0)
     call calcAuxVel(uaux,vaux,dive,u,v)

     call calcP(p,dive)

     call correctVel(u,v,uaux,vaux,p)

     call cpu_time(t1)
     if(mod(iter,100).eq.0) then
        write(*,*) 'iter=',iter, 'cpu time=',t1-t0
     end if
     if(mod(iter,interval_w).eq.0) then
        call write_file_bin(ifile); ifile=ifile+1
     end if
  end do

  deallocate(dive)
  deallocate(p)
  deallocate(vaux)
  deallocate(uaux)
  deallocate(v)
  deallocate(u)

  deallocate(x,y)

end program main

subroutine init(u,v,p)
  use parameters, only:Nx,Ny
  implicit none
  real(8), intent(inout) :: u(0:Nx+1,0:Ny)
  real(8), intent(inout) :: v(0:Nx  ,0:Ny+1)
  real(8), intent(inout) :: p(0:Nx  ,0:Ny)

  u=0.d0
  v=0.d0
  call set_bc_vel(u,v)
  p=0.d0

end subroutine init

subroutine set_bc_vel(u,v)

  use parameters, only:Nx,Ny,Uwall
  implicit none

  real(8), intent(inout) :: u(0:Nx+1,0:Ny)
  real(8), intent(inout) :: v(0:Nx  ,0:Ny+1)

  integer i,j,ic,jc

  ! left and right walls
  do jc=0,Ny-1
     u(1,jc) =0.d0
     u(Nx,jc)=0.d0
     u(0,jc) = -u(2,jc) ! left imaginary cell
     u(Nx+1,jc) = -u(Nx-1,jc) ! right imaginary cell
  end do

  ! bottom and top walls
  do i=0,Nx+1
     u(i,0) = -u(i,0+1)  ! bottom wall (uc=0)
     u(i,Ny) = -u(i,Ny-1)+2.d0*Uwall ! moving wall (uc=Uwall)
  end do

  do j=0,Ny+1
     v(0,j) = -v(1,j)
     v(Nx,j)= -v(Nx-1,j)
  end do
  do ic=0,Nx
     v(ic,1)  =0.d0
     v(ic,Ny) =0.d0
     v(ic,0)  = -v(ic,2)
     v(ic,Ny+1) = -v(ic,Ny-1)
  end do

end subroutine set_bc_vel

subroutine set_bc_pressure(p)

  use parameters, only:Nx,Ny
  implicit none

  real(8),intent(inout) :: p(0:Nx, 0:Ny)
  integer ic,jc


  !p(1,1) = 0.d0 !fix a point

  do ic=1,Nx-1
     p(ic,0) = p(ic,1)
     p(ic,Ny)= p(ic,Ny-1)
  end do

  do jc=1,Ny-1
     p(0, jc) = p(1,   jc)
     p(Nx,jc) = p(Nx-1,jc)
  end do


end subroutine set_bc_pressure


subroutine calcAuxVel(uaux,vaux,dive,u,v)

  use parameters,only: Nx,Ny,dx,dy,dt,Uwall,Dens,Kvis
  implicit none

  real(8),intent(inout) :: uaux(0:Nx+1,0:Ny)
  real(8),intent(inout) :: vaux(0:Nx  ,0:Ny+1)
  real(8),intent(inout) :: dive(0:Nx  ,0:Ny)
  real(8),intent(in) :: u(0:Nx+1,0:Ny)
  real(8),intent(in) :: v(0:Nx  ,0:Ny+1)

  integer i,j
  integer ic,jc
  real(8) conv, visc

  do jc=1,Ny-1
     do i=1,Nx
        visc = (u(i-1,jc  )-2d0*u(i,jc)+u(i+1,jc  ))/(dx*dx) &
              +(u(i  ,jc-1)-2d0*u(i,jc)+u(i  ,jc+1))/(dy*dy)

        conv = (+( ( u(i-1,jc)+u(i  ,jc))/2d0 &
                  *(-u(i-1,jc)+u(i  ,jc))/dx )&
                +( ( u(i,  jc)+u(i+1,jc))/2d0 &
                  *(-u(i,  jc)+u(i+1,jc))/dx )&
               )/2d0 &
              +(+( ( v(i-1,jc  )+v(i  ,jc  ))/2d0 &
                  *(-u(i  ,jc-1)+u(i  ,jc  ))/dy )&
                +( ( v(i-1,jc+1)+v(i  ,jc+1))/2d0 &
                  *(-u(i  ,jc  )+u(i  ,jc+1))/dy )&
               )/2d0
        uaux(i,jc) = u(i,jc) + dt*(-conv + Kvis*visc)
     end do
  end do

  do j=1,Ny
     do ic=1,Nx-1
        visc = (v(ic-1,j  )-2d0*v(ic,j)+v(ic+1,j  ))/(dx*dx) &
              +(v(ic  ,j-1)-2d0*v(ic,j)+v(ic  ,j+1))/(dy*dy)

        conv = (+( ( u(ic  ,j-1)+u(ic  ,j))/2d0 &
                  *(-v(ic-1,j  )+v(ic  ,j))/dx )&
                +( ( u(ic+1,j-1)+u(ic+1,j))/2d0 &
                  *(-v(ic  ,j  )+v(ic+1,j))/dx )&
               )/2d0 &
              +(+( ( v(ic,j-1)+v(ic  ,j  ))/2d0 &
                  *(-v(ic,j-1)+v(ic  ,j  ))/dy )&
                +( ( v(ic,j  )+v(ic  ,j+1))/2d0 &
                  *(-v(ic,j  )+v(ic  ,j+1))/dy )&
               )/2d0
        vaux(ic,j) = v(ic,j) + dt*(-conv + Kvis*visc)
     end do
  end do

  call set_bc_vel(uaux,vaux)

  ! div. of p
  do jc=1,Ny-1
     do ic=1,Nx-1
        dive(ic,jc) = ( (-uaux(ic,jc) + uaux(ic+1,jc  ))/dx &
                       +(-vaux(ic,jc) + vaux(ic,  jc+1))/dy &
                      )/dt*Dens
     end do
  end do

end subroutine calcAuxVel

subroutine calcP(p,dive)

  use parameters,only: Nx,Ny,dx,dy,dt,Uwall,Dens,err_tol,accel,tiny
  implicit none

  real(8),intent(inout) :: p(0:Nx,0:Ny)
  !real(8),intent(in) :: dive(1:Nx-1,1:Ny-1)
  real(8),intent(in) :: dive(0:Nx,0:Ny)

  integer i,j
  integer ic,jc
  integer itr_SOR
  real(8) :: err_n,err_d,err_r,d_pres

  itr_SOR = 0
  err_r = 1d0
  do while( err_r > err_tol)
     itr_SOR = itr_SOR +1
     err_r =0d0
     err_n =0d0
     err_d =0d0

     do jc=1,Ny-1
        do ic=1,Nx-1
           d_pres = (  dy*dy*(p(ic-1,jc  ) + p(ic+1,jc  )) &
                     + dx*dx*(p(ic  ,jc-1) + p(ic  ,jc+1)) &
                     - (dx*dx*dy*dy * dive(ic,jc)) )/((dx*dx+dy*dy)*2d0) - p(ic,jc)
           p(ic,jc) = p(ic,jc) + accel*d_pres
           err_n = err_n + d_pres*d_pres
           err_d = err_d + p(ic,jc)*p(ic,jc)
        end do
     end do
     call set_bc_pressure(p)

     if(err_d <= tiny) err_d = 1d0
     err_r = dsqrt(err_n/err_d)

  end do

  !write(*,*) 'calcP: itr (SOR)=',itr_SOR,' error=',err_r

end subroutine calcP

subroutine correctVel(u,v,uaux,vaux,p)
  use parameters,only: Nx,Ny,dx,dy,dt,Uwall,Dens
  implicit none
  real(8),intent(inout) :: u(0:Nx+1,0:Ny)
  real(8),intent(inout) :: v(0:Nx  ,0:Ny+1)
  real(8),intent(in) :: uaux(0:Nx+1,0:Ny)
  real(8),intent(in) :: vaux(0:Nx  ,0:Ny+1)
  real(8),intent(in) :: p(0:Nx  ,0:Ny)

  integer i,j
  integer ic,jc

  do jc=1,Ny-1
     do i=1,Nx
        u(i,jc) = uaux(i,jc) - dt*(-p(i-1,jc) + p(i,jc))/dx/Dens
     enddo
  enddo
  do j=1,Ny
     do ic=1,Nx-1
        v(ic,j) = vaux(ic,j) - dt*(-p(ic,j-1) + p(ic,j))/dy/Dens
     end do
  end do

  ! b.c.
  call set_bc_vel(u,v)

end subroutine correctVel

subroutine write_file_bin(ifile)

  use parameters
  use global_fwd

  implicit none

  character*4 cext
  character*256 filename,fileheader
  integer,intent(in) :: ifile

  integer i,j,iout,ifw

  write(cext,'(i4.4)') ifile
  fileheader=trim(file_dir)//'fields_'//trim(cext)//'.txt'
  filename=trim(file_dir)//'fields_'//trim(cext)//'.fwd'
  !call write_file_txt(filename)
  ! write a parameter file
  iout=34
  open(iout,file=fileheader,form="formatted")
  ! header
  write(iout,*) '# ', trim(filename)
  write(iout,*) '# Lx,Ly,Nx,Ny: ', Lx,Ly,Nx,Ny
  write(iout,*) '# accel, err_tol: , ',accel, err_tol
  write(iout,*) '# Uwall, Kvis, Dens: ', Uwall, Kvis, Dens
  write(iout,*) '# ndt,endT: ', ndt, endT
  write(iout,*) '# interval_w:', interval_w
  write(iout,*) '# '
  close(iout)
  ! write fortran binary
  ifw=35
  open(ifw,file=filename,form="unformatted")
  ! u
  write(ifw) u
  ! v
  write(ifw) v
  ! p
  write(ifw) p
  !
  close(ifw)

end subroutine write_file_bin
