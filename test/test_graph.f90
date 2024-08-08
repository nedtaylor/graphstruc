program test_graph
  !! Test program for graph types
  use graphstruc, only: graph_type, vertex_type, edge_type

  implicit none

  class(graph_type), allocatable :: graph

  logical :: success = .true.


  !-----------------------------------------------------------------------------
  ! test graph
  !-----------------------------------------------------------------------------
  graph = graph_type( &
       vertex = [ &
            vertex_type(feature=[1.0,1.0,1.0]), &
            vertex_type(feature=[1.0,12.0,1.0]) &
       ], &
       edge = [ edge_type( &
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
  ! test add_vertex
  !-----------------------------------------------------------------------------
  call graph%add_vertex(vertex = vertex_type( &
       feature = [ 1.0, 2.0, 3.0 ] &
  ))
  if(graph%num_vertices .ne. 3)then
     write(0,*) 'Graph add_vertex failed for num_vertices'
     success = .false.
  end if
  if(size(graph%vertex,dim=1).ne.3)then
     write(0,*) 'Graph add_vertex failed for vertex'
     write(0,*) size(graph%vertex,dim=1)
     success = .false.
  end if
  if(size(graph%vertex(3)%feature,dim=1).ne.3)then
     write(0,*) 'Graph add_vertex failed for vertex feature'
     success = .false.
  end if
  call graph%add_vertex(feature=[ 3.0, 2.0, 12.0 ])
  if(graph%num_vertices .ne. 4)then
     write(0,*) 'Graph add_vertex failed for num_vertices'
     success = .false.
  end if
  if(size(graph%vertex,dim=1).ne.4)then
     write(0,*) 'Graph add_vertex failed for vertex'
     write(0,*) size(graph%vertex,dim=1)
     success = .false.
  end if

  if(size(graph%vertex(4)%feature,dim=1).ne.3)then
     write(0,*) 'Graph add_vertex failed for vertex features'
     success = .false.
  end if


  !-----------------------------------------------------------------------------
  ! test add_edge
  !-----------------------------------------------------------------------------
  call graph%add_edge(edge = edge_type( &
       index = [1, 3], weight = 2.0, &
       feature = [ 1.0, 2.0, 3.0 ], &
       directed = .false. &
  ))
  if(graph%num_edges .ne. 2)then
     write(0,*) 'Graph add_edge failed for num_edges'
     success = .false.
  end if
  if(size(graph%edge,dim=1).ne.2)then
     write(0,*) 'Graph add_edge failed for edge'
     success = .false.
  end if


  !-----------------------------------------------------------------------------
  ! test set_edges
  !-----------------------------------------------------------------------------
  call graph%set_edges(1, [ -3, 4 ])
  if(graph%num_edges .ne. 4)then
     write(0,*) 'Graph set_edges failed for num_edges'
     success = .false.
  end if
  if(size(graph%edge,dim=1).ne.4)then
     write(0,*) 'Graph set_edges failed for edge'
     success = .false.
  end if
  if(graph%vertex(1)%degree .ne. 4)then
     write(0,*) 'Graph set_edges failed for vertex degree of vertex 1'
     write(0,*) graph%vertex(1)%degree
     success = .false.
  end if
  if(graph%vertex(3)%degree .ne. 1)then
     write(0,*) 'Graph set_edges failed for vertex degree of vertex 3'
     write(0,*) graph%vertex(3)%degree
     success = .false.
  end if
  if(graph%vertex(4)%degree .ne. 1)then
     write(0,*) 'Graph set_edges failed for vertex degree of vertex 4'
     write(0,*) graph%vertex(4)%degree
     success = .false.
  end if
  if(graph%adjacency(1,3) .ne. 3)then
     write(0,*) 'Graph set_edges failed for adjacency'
     success = .false.
  end if


  !-----------------------------------------------------------------------------
  ! test remove_vertices and remove_edges
  ! remove_edges is called internally by remove_vertices
  !-----------------------------------------------------------------------------
  call graph%remove_vertices([4,2])
  if(graph%num_vertices .ne. 2)then
     write(0,*) 'Graph remove_vertices failed for num_vertices'
     success = .false.
  end if
  if(size(graph%vertex,dim=1) .ne. 2)then
     write(0,*) 'Graph remove_vertices failed for vertex'
     success = .false.
  end if
  if(graph%num_edges .ne. 2)then
     write(0,*) 'Graph remove_vertices failed for num_edges'
     write(0,*) graph%num_edges
     success = .false.
  end if
  if(graph%vertex(1)%degree .ne. 2)then
     write(0,*) 'Graph remove_vertices failed for vertex degree of vertex 1'
     write(0,*) graph%vertex(1)%degree
     success = .false.
  end if


  !-----------------------------------------------------------------------------
  ! test set_num_vertices
  !-----------------------------------------------------------------------------
  call graph%set_num_vertices(2, num_vertex_features = 3)
  if(graph%num_edges .ne. 2)then
     write(0,*) 'Graph set_num_vertices failed for num_edges'
     success = .false.
  end if
  if(size(graph%edge,dim=1).ne.2)then
     write(0,*) 'Graph set_num_edges failed for edge'
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