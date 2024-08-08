program graphstruc_example
  !! Example program for graphstruc
  use graphstruc, only: graph_type
  implicit none

  type(graph_type) :: graph
  integer :: i

  graph%directed = .true.
  call graph%add_vertex( feature = [ 1.0, 1.0 ] )
  call graph%add_vertex( feature = [ 1.0, 12.0 ] )
  call graph%add_edge( &
       index = [1, -2], weight = 2.0, &
       feature = [ 1.0, 2.0, 3.0 ], directed = .true. &
  )
  write(*,'("Number of vertices:    ",I0)') graph%num_vertices
  write(*,'("Number of edges:       ",I0)') graph%num_edges
  write(*,*) 'Directed:', graph%directed
  write(*,'("Vertex 1 degree:       ",I0)') graph%vertex(1)%degree
  write(*,'("Vertex 2 degree:       ",I0)') graph%vertex(2)%degree

  write(*,*) 'Adjacency matrix:'
  do i = 1, graph%num_vertices
     write(*,*) graph%adjacency(i,:)
  end do
  write(*,*)

  write(*,*) "-------------------"
  write(*,*) "Adding edge"
  call graph%remove_edges([1])
  call graph%add_edge( &
       index = [1, 2], weight = 2.0, &
       feature = [ 1.0, 2.0, 3.0 ] &
  )
  write(*,*) 'Directed:', graph%directed
  write(*,*) 'Adjacency matrix:'
  do i = 1, graph%num_vertices
     write(*,*) graph%adjacency(i,:)
  end do

  write(*,*) "-------------------"
  write(*,*) "Resetting graph"
  call graph%set_num_vertices(4, num_vertex_features = 2)
  call graph%set_edges(2, [1, 4])
  write(*,'("Number of vertices:    ",I0)') graph%num_vertices
  write(*,'("Number of edges:       ",I0)') graph%num_edges
  
  write(*,*) "-------------------"
  write(*,*) "Removing vertex 1"
  call graph%remove_vertices([1, 3])
  write(*,'("Number of vertices:    ",I0)') graph%num_vertices
  write(*,'("Number of edges:       ",I0)') graph%num_edges

  write(*,*)
  write(*,*) "-------------------"
  write(*,*) "Adding vertex"
  call graph%add_vertex( feature = [ 1.0, 2.0 ] )
  write(*,'("Number of vertices:    ",I0)') graph%num_vertices
  write(*,'("Number of edges:       ",I0)') graph%num_edges

end program graphstruc_example