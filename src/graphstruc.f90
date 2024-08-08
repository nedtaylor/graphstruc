module graphstruc
  !! This is the top-level module for the graphstruc Fortran library.
  use graphstruc_types, only: vertex_type, edge_type, graph_type
  implicit none

  private

  public :: vertex_type, edge_type, graph_type

end module graphstruc