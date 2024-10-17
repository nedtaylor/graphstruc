submodule(graphstruc_types) graphstruc_types_submodule
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
    type(vertex_type), dimension(:), intent(in), optional :: vertex
    !! Vertices in the graph.
    type(edge_type), dimension(:), intent(in), optional :: edge
    !! Edges in the graph.
    character(len=128), intent(in), optional :: name
    !! Name of the graph.
    logical, intent(in), optional :: directed
    !! Boolean whether the graph is directed. Default is False.
    type(graph_type) :: output
    !! Initialised graph.

    ! Local variables
    integer :: i
    !! Loop index.

    output%directed = .false.
    if(present(directed)) output%directed = directed
    if(present(name)) output%name = name
    if(present(vertex))then
       output%num_vertices = size(vertex, dim=1)
       output%num_vertex_features = size(vertex(1)%feature, dim=1)
       do i = 1, output%num_vertices
          if(size(vertex(i)%feature, dim=1) .ne. output%num_vertex_features)then
             write(0,*) 'ERROR: Number of vertex features do not match'
             stop "Exiting..."
          end if
       end do
       allocate(output%vertex(output%num_vertices))
       output%vertex = vertex
       if(present(edge))then
          output%num_edges = size(edge, dim=1)
          output%num_edge_features = size(edge(1)%feature, dim=1)
          do i = 1, output%num_edges
             if(size(edge(i)%feature, dim=1) .ne. output%num_edge_features)then
                write(0,*) 'ERROR: Number of edge indices do not match'
                stop "Exiting..."
             end if
          end do
       end if
       allocate(output%edge(output%num_edges))
       output%edge = edge
       call output%generate_adjacency()
       call output%calculate_degree()
    elseif(present(edge))then
       write(0,*) 'ERROR: Edges are present without vertices'
       stop "Exiting..."
    else
       output%num_vertices = 0
       output%num_vertex_features = 0
       output%num_edges = 0
       output%num_edge_features = 0
    end if
  end function graph_type_init


  module subroutine add_vertex(this, vertex, feature, id)
    !! Add a vertex to the graph.
    implicit none

    ! Arguments
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.
    type(vertex_type), intent(in), optional :: vertex
    !! Vertex to be added.
    real(real32), dimension(:), intent(in), optional :: feature
    !! Feature vector of the vertex.
    integer, intent(in), optional :: id
    !! Identifier of the vertex.

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

    if(.not.allocated(vertex_%feature))then
       allocate( &
            vertex_%feature(this%num_vertex_features), &
            source = 0.0_real32&
       )
    else if(this%num_vertex_features.eq.0)then
       this%num_vertex_features = size(vertex_%feature, dim=1)
    else if(size(vertex_%feature, dim=1).ne.this%num_vertex_features)then
       write(0,*) 'ERROR: Number of vertex features do not match'
       stop "Exiting..."
    end if

    if(present(id)) vertex_%id = id


    this%num_vertices = this%num_vertices + 1
    if(.not.allocated(this%vertex)) allocate(this%vertex(0))
    this%vertex = [this%vertex, vertex_]
    call this%generate_adjacency()
  end subroutine add_vertex


  module subroutine add_edge(this, edge, index, weight, feature, directed, id)
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
    integer, intent(in), optional :: id
    !! Identifier of the edge.

    ! Local variables
    class(edge_type), allocatable :: edge_
    !! Initialised edge.
    real(real32) :: weight_
    !! Weight of the edge.
    logical :: directed_


    directed_ = .false.
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
          if(directed_.and.(directed_.neqv.this%directed))then
             write(0,*) 'ERROR: Edge direction does not match graph direction'
             stop "Exiting..."
          end if
          if(present(feature)) then
             edge_ = edge_type_init(index, weight_, feature, directed_)
          else
             edge_ = edge_type_init(index, weight_, directed=directed_)
          end if
       end if
    end if

    if(.not.allocated(edge_%feature))then
       allocate( &
            edge_%feature(this%num_edge_features), &
            source = 0.0_real32&
       )
    else if(this%num_edge_features.eq.0)then
       this%num_edge_features = size(edge_%feature, dim=1)
    else if(size(edge_%feature, dim=1).ne.this%num_edge_features)then
       write(0,*) 'ERROR: Number of edge features do not match'
       stop "Exiting..."
    end if

    if(present(id)) edge_%id = id

    this%num_edges = this%num_edges + 1
    if(.not.allocated(this%edge)) allocate(this%edge(0))
    this%edge = [this%edge, edge_]
    call this%generate_adjacency()

    this%vertex(edge_%index(1))%degree = this%vertex(edge_%index(1))%degree + 1
    if(.not.directed_) &
       this%vertex(abs(edge_%index(2)))%degree = &
            this%vertex(abs(edge_%index(2)))%degree + 1

  end subroutine add_edge


  module subroutine set_num_vertices(this, num_vertices, num_vertex_features)
    !! Set the number of vertices of the graph.
    !!
    !! This will deallocate the existing vertices and edges
    !! and set the number of vertices.
    !! New vertices will be allocated but not initialised.
    implicit none

    ! Arguments
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.
    integer, intent(in) :: num_vertices
    !! Number of vertices in the graph.
    integer, intent(in), optional :: num_vertex_features

    if(allocated(this%vertex)) deallocate(this%vertex)
    if(allocated(this%edge)) deallocate(this%edge)
    this%num_vertices = num_vertices
    this%num_edges = 0
    if(present(num_vertex_features)) &
         this%num_vertex_features = num_vertex_features
    allocate(this%vertex(num_vertices))
  end subroutine set_num_vertices


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


  module subroutine remove_vertices(this, indices)
    !! Remove vertices from the graph.
    implicit none

    ! Arguments
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.
    integer, dimension(:), intent(in) :: indices
    !! Indices of the vertices to be removed.

    ! Local variables
    integer :: i, j, k
    !! Loop indices.
    integer, dimension(size(indices, dim=1)) :: vertex_indices
    !! Indices of the vertices to be removed.
    integer, dimension(:), allocatable :: edge_indices
    !! Indices of the edges to be removed.

    allocate(edge_indices(0))
    do i = 1, size(indices, dim=1)
      do j = 1, this%num_edges
        if( &
             any(this%edge(j)%index .eq. indices(i)) .or. &
             any(this%edge(j)%index .eq. -indices(i)) &
        ) &
             edge_indices = [edge_indices, j]
      end do
    end do
    if(size(edge_indices, dim=1) .gt. 0) &
         call this%remove_edges(edge_indices, update_adjacency=.false.)

    vertex_indices = indices
    do i = 1, size(indices, dim=1)
       k = maxval(vertex_indices, dim = 1)
       this%vertex = [ &
           this%vertex(1:k-1:1), &
           this%vertex(k+1:this%num_vertices:1) &
       ]
       this%num_vertices = this%num_vertices - 1
       vertex_indices(maxloc(vertex_indices, dim = 1)) = 0
       do j = 1, this%num_edges
          if(this%edge(j)%index(1).gt.k)then
             this%edge(j)%index(1) = this%edge(j)%index(1) - 1
          elseif(this%edge(j)%index(1).lt.-k)then
             this%edge(j)%index(1) = this%edge(j)%index(1) + 1
          end if
          if(this%edge(j)%index(2).gt.k)then
             this%edge(j)%index(2) = this%edge(j)%index(2) - 1
          elseif(this%edge(j)%index(2).lt.-k)then
             this%edge(j)%index(2) = this%edge(j)%index(2) + 1
          end if
       end do
    end do
    call this%generate_adjacency()
  end subroutine remove_vertices


  module subroutine remove_edges(this, indices, update_adjacency)
    !! Remove edges from the graph.
    implicit none

    ! Arguments
    class(graph_type), intent(inout) :: this
    !! Parent. Instance of the graph structure.
    integer, dimension(:), intent(in) :: indices
    !! Indices of the edges to be removed.
    logical, intent(in), optional :: update_adjacency
    !! Boolean whether to update the adjacency matrix. Default is True.

    ! Local variables
    integer :: i, k
    !! Loop indices.
    logical :: update_adjacency_ = .true.
    !! Boolean whether to update the adjacency matrix.
    integer, dimension(size(indices, dim=1)) :: edge_indices
    !! Indices of the vertices to be removed.

    if(present(update_adjacency)) update_adjacency_ = update_adjacency

    edge_indices = indices
    do i = 1, size(indices, dim=1)
       k = maxval(edge_indices, dim = 1)
       this%vertex(this%edge(k)%index(1))%degree = &
            this%vertex(this%edge(k)%index(1))%degree - 1
       if(.not.this%directed)then
          this%vertex(abs(this%edge(k)%index(2)))%degree = &
               this%vertex(abs(this%edge(k)%index(2)))%degree - 1
       else if(this%edge(k)%index(2).gt.0)then
          this%vertex(this%edge(k)%index(2))%degree = &
               this%vertex(this%edge(k)%index(2))%degree - 1
       end if
       this%edge = [ &
            this%edge(1:k-1:1), &
            this%edge(k+1:this%num_edges:1) &
       ]
       this%num_edges = this%num_edges - 1
       edge_indices(maxloc(edge_indices, dim = 1)) = 0
      end do
    if(update_adjacency_) call this%generate_adjacency()
  end subroutine remove_edges


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
          this%adjacency(i,abs(j)) = k
          this%adjacency(abs(j),i) = k
       end if
    end do
  end subroutine generate_adjacency
  

end submodule graphstruc_types_submodule