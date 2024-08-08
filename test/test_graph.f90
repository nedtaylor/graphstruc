program test_graph
  !! Test program for graph types
  use graphstruc, only: graph_type, vertex_type, edge_type

  implicit none

  class(graph_type), allocatable :: graph
  type(vertex_type), dimension(:), allocatable :: vertex
  type(edge_type), dimension(:), allocatable :: edge

  logical :: success = .true.


  !-----------------------------------------------------------------------------
  ! test graph
  !-----------------------------------------------------------------------------
  graph = graph_type( &
       vertices = [ vertex_type(), vertex_type() ], &
       edges = [ edge_type( &
            index = [1, 2], weight = 2.0, &
            feature = [ 1.0, 2.0, 3.0 ], &
            directed = .false. &
       ) ], &
       directed = .false. &
  )
  if(graph%num_vertices .ne. 2)then
     write(0,*) 'Graph initialisation failed for num_vertices'
     success = .false.
  end if
  if(graph%num_edges .ne. 1)then
     write(0,*) 'Graph initialisation failed for num_edges'
     success = .false.
  end if
  if(graph%directed)then
     write(0,*) 'Graph initialisation failed for directed'
     success = .false.
  end if
  if(size(graph%vertex,dim=1).ne.2)then
     write(0,*) 'Graph initialisation failed for vertex'
     success = .false.
  end if
  if(size(graph%edge,dim=1).ne.1)then
     write(0,*) 'Graph initialisation failed for edge'
     success = .false.
  end if
  if(graph%vertex(1)%degree .ne. 1 .or. graph%vertex(2)%degree .ne. 1)then
     write(0,*) 'Graph initialisation failed for vertex degree'
     success = .false.
  end if
  if(graph%adjacency(1,2) .ne. 1)then
     write(0,*) 'Graph initialisation failed for adjacency'
     success = .false.
  end if

  graph%directed = .true.
  graph%edge(1)%index(2) = -2
  call graph%generate_adjacency()
  if(graph%adjacency(1,2) .ne. 1)then
     write(0,*) 'Graph update failed for directed adjacency'
     success = .false.
  end if
  if(graph%adjacency(2,1) .ne. 0)then
    write(0,*) 'Graph update failed for directed adjacency'
    success = .false.
 end if


  !-----------------------------------------------------------------------------
  ! check for any failed tests
  !-----------------------------------------------------------------------------
  write(*,*) "----------------------------------------"
  if(success)then
     write(*,*) 'test_graph passed all tests'
  else
     write(0,*) 'test_graph failed one or more tests'
     stop 1
  end if

end program test_graph