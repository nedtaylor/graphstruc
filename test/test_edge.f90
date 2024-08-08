program test_edge
  !! Test program for edge types
  use graphstruc, only: edge_type

  implicit none

  class(edge_type), allocatable :: edge

  logical :: success = .true.


  !-----------------------------------------------------------------------------
  ! test undirected edge
  !-----------------------------------------------------------------------------
  edge = edge_type( &
       index = [1, 2], &
       weight = 2.0, &
       feature = [ 1.0, 2.0, 3.0 ], &
       directed = .false. &
  )

  if(edge%index(1) .ne. 1)then
     write(0,*) 'Edge initialisation failed for index(1)'
     success = .false.
  end if
  if(edge%index(2) .ne. 2)then
     write(0,*) 'Edge initialisation failed for index(2)'
     success = .false.
  end if
  if(abs(edge%weight - 2.0).gt.1.E-6)then
     write(0,*) 'Edge initialisation failed for weight'
     success = .false.
  end if
  if(size(edge%feature,dim=1).ne.3)then
     write(0,*) 'Edge initialisation failed for feature'
     success = .false.
  end if
  if(any( &
       abs(edge%feature - [1.0, 2.0, 3.0]) .gt. 1.E-6))then
     write(0,*) 'Edge initialisation failed for feature'
     success = .false.
  end if  


  !-----------------------------------------------------------------------------
  ! test directed edge
  !-----------------------------------------------------------------------------
  edge = edge_type( &
       index = [1, 2], &
       directed = .true. &
  )

  if(edge%index(2) .ne. -2)then
    write(0,*) 'Edge initialisation failed directed for index(2)'
    success = .false.
 end if
 if(abs(edge%weight - 1.0).gt.1.E-6)then
    write(0,*) 'Edge initialisation failed for weight'
    success = .false.
 end if
 if(allocated(edge%feature))then
    write(0,*) 'Edge initialisation failed for feature'
    success = .false.
 end if


  !-----------------------------------------------------------------------------
  ! check for any failed tests
  !-----------------------------------------------------------------------------
  write(*,*) "----------------------------------------"
  if(success)then
     write(*,*) 'test_edge passed all tests'
  else
     write(0,*) 'test_edge failed one or more tests'
     stop 1
  end if

end program test_edge