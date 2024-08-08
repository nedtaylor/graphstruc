program test_vertex
  !! Test program for vertex types
  use graphstruc, only: vertex_type

  implicit none

  type(vertex_type) :: vertex

  logical :: success = .true.


  !-----------------------------------------------------------------------------
  ! test vertex
  !-----------------------------------------------------------------------------
  if(vertex%degree .ne. 0)then
     write(0,*) 'Vertex initialisation failed for degree'
     success = .false.
  end if
  if(allocated(vertex%feature))then
     write(0,*) 'Vertex initialisation failed for feature'
     success = .false.
  end if

  vertex%feature = [ 1.0, 2.0, 3.0 ]
  if(any( &
       abs(vertex%feature - [1.0, 2.0, 3.0]) .gt. 1.E-6))then
     write(0,*) 'Vertex initialisation failed for feature'
     success = .false.
  end if


  !-----------------------------------------------------------------------------
  ! check for any failed tests
  !-----------------------------------------------------------------------------
  write(*,*) "----------------------------------------"
  if(success)then
     write(*,*) 'test_vertex passed all tests'
  else
     write(0,*) 'test_vertex failed one or more tests'
     stop 1
  end if

end program test_vertex