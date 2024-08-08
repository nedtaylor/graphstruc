submodule(graphstruc) graphstruc_submodule
  !! This submodule contains the implementation of procedures outlined in the
  !! graphstruc module.
  implicit none  


contains
  
  module function edge_type_init(index, weight, feature, directed) &
       result(output)
    !! Initialise an edge.
    implicit none

    ! Arguments
    integer, dimension(2), intent(in) :: index
    !! Vertex indices of the edge.
    real(real32), intent(in), optional :: weight
    !! Weight of the edge.
    real(real32), dimension(:), intent(in), optional :: feature
    !! Feature vector of the edge.
    logical, intent(in), optional :: directed
    !! Boolean whether the edge is directed. Default is False.
    type(edge_type) :: output
    output%index = index
    if(present(directed))then
       if(directed) output%index(2) = -abs(index(2))
    end if
    if(present(weight)) output%weight = weight
    if(present(feature)) output%feature = feature
  end function edge_type_init


  module function graph_type_init(vertex, edge, name, directed) &
       result(output)
    !! Interface for initialising a graph.
    implicit none

    ! Arguments
    type(vertex_type), dimension(:), intent(in) :: vertex
    !! Vertices in the graph.
    type(edge_type), dimension(:), intent(in) :: edge
    !! Edges in the graph.
    character(len=128), intent(in), optional :: name
    !! Name of the graph.
    logical, intent(in), optional :: directed
    !! Boolean whether the graph is directed. Default is False.
    type(graph_type) :: output
    !! Initialised graph.

    output%num_vertices = size(vertex, dim=1)
    output%num_edges = size(edge, dim=1)
    output%directed = .false.
    if(present(directed)) output%directed = directed
    if(present(name)) output%name = name
    allocate(output%vertex(output%num_vertices))
    allocate(output%edge(output%num_edges))
    output%vertex = vertex
    output%edge = edge
    call output%generate_adjacency()
    call output%calculate_degree()
  end function graph_type_init


  module subroutine add_vertex(this, vertex, feature)
    !! Add a vertex to the graph.
    implicit none

    ! Arguments
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.
    type(vertex_type), intent(in), optional :: vertex
    !! Vertex to be added.
    real(real32), dimension(:), intent(in), optional :: feature
    !! Feature vector of the vertex.

    ! Local variables
    type(vertex_type) :: vertex_
    !! Initialised vertex.


    if(present(vertex).and.present(feature))then
       write(0,*) 'ERROR: Both vertex and feature are present where only one &
            &should be defined'
       stop "Exiting..."
    elseif(.not.present(vertex).and..not.present(feature))then
       write(0,*) 'ERROR: Neither vertex nor feature are present'
        stop "Exiting..."
    end if
    if(present(vertex)) vertex_ = vertex
    if(present(feature)) vertex_%feature = feature
    this%num_vertices = this%num_vertices + 1
    this%vertex = [this%vertex, vertex_]
    call this%generate_adjacency()
  end subroutine add_vertex


  module subroutine add_edge(this, edge, index, weight, feature, directed)
    !! Add an edge to the graph.
    implicit none

    ! Arguments
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.
    type(edge_type), intent(in), optional :: edge
    !! Edge to be added.
    integer, dimension(2), intent(in), optional :: index
    !! Vertex indices of the edge.
    real(real32), intent(in), optional :: weight
    !! Weight of the edge.
    real(real32), dimension(:), intent(in), optional :: feature
    !! Feature vector of the edge.
    logical, intent(in), optional :: directed
    !! Boolean whether the edge is directed. Default is False.

    ! Local variables
    class(edge_type), allocatable :: edge_
    !! Initialised edge.
    real(real32) :: weight_
    !! Weight of the edge.
    logical :: directed_ = .false.

    if(present(edge).and.( &
         present(index)   .or. &
         present(weight)  .or. &
         present(directed).or. &
         present(feature) &
    ))then
       write(0,*) 'ERROR: Both edge and parameters are present where only one &
            &should be defined'
       stop "Exiting..."
    elseif(.not.present(edge)  .and. &
         .not.present(index)   .and. &
         .not.present(weight)  .and. &
         .not.present(directed).and. &
         .not.present(feature) &
    )then
       write(0,*) 'ERROR: Neither edge nor parameters are present'
        stop "Exiting..."
    end if

    if(present(edge))then
       edge_ = edge
    else
       if(.not.present(index))then
          write(0,*) 'ERROR: Index is not present'
          stop "Exiting..."
       else
          if(present(weight)) weight_ = weight
          if(present(directed))then
             directed_ = directed
          else
             if(any(index .lt. 0)) directed_ = .true.
          end if
          if(present(feature)) then
             edge_ = edge_type_init(index, weight_, feature, directed_)
          else
             edge_ = edge_type_init(index, weight_, directed=directed_)
          end if
       end if
      end if

    this%num_edges = this%num_edges + 1
    this%edge = [this%edge, edge_]
    call this%generate_adjacency()

    this%vertex(edge_%index(1))%degree = this%vertex(edge_%index(1))%degree + 1
    if(.not.directed_) &
       this%vertex(abs(edge_%index(2)))%degree = &
            this%vertex(abs(edge_%index(2)))%degree + 1

  end subroutine add_edge


  module subroutine set_edges(this, vertex_index, connected_indices)
    !! Add edge connections between vertices of the graph.
    implicit none
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.
    integer, intent(in) :: vertex_index
    !! Index of the vertex.
    integer, dimension(:), intent(in) :: connected_indices
    !! Indices of the connected vertices.

    ! Local variables
    integer :: i
    !! Loop index.
    logical :: directed
    !! Boolean whether the edge is directed.


    do i = 1, size(connected_indices, dim=1)
      directed = .false.
      if(connected_indices(i).lt.0) directed = .true.
      call this%add_edge( &
           index=[vertex_index, connected_indices(i)], &
           directed=directed &
      )
    end do
  end subroutine set_edges


  module subroutine calculate_degree(this)
    !! Calculate the degree of the vertices in the graph.
    implicit none
    
    ! Arguments
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.

    ! Local variables
    integer :: i, j
    !! Loop indices.

    this%vertex(:)%degree = 0
    do i = 1, this%num_vertices
      do j = 1, this%num_vertices, 1
        if(this%adjacency(i,j) .gt. 0) &
             this%vertex(i)%degree = this%vertex(i)%degree + 1
      end do
    end do
  end subroutine calculate_degree


  module subroutine generate_adjacency(this)
    !! Generate the adjacency matrix of the graph.
    implicit none

    ! Arguments
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.
    integer :: i, j, k
    !! Loop indices.

    if(allocated(this%adjacency)) deallocate(this%adjacency)
    allocate(this%adjacency(this%num_vertices, this%num_vertices))
    this%adjacency = 0
    do k = 1, this%num_edges
      i = this%edge(k)%index(1)
      j = this%edge(k)%index(2)
      if(this%directed.and.j.lt.0) then
        this%adjacency(i,abs(j)) = k
      else
        this%adjacency(i,j) = k
        this%adjacency(j,i) = k
      end if
    end do
  end subroutine generate_adjacency
  

end submodule graphstruc_submodule