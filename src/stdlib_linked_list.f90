module stdlib_linked_list 
  use stdlib_child_list 
  implicit none

  private :: Parent_Node
  public  :: linked_list
  integer, private, parameter :: MAX_SIZE = 10000
  integer, private, parameter :: SPLIT_POINT = INT(0.9*MAX_SIZE)
  type Parent_Node
    type(Parent_Node), pointer :: next => null()
    type(Parent_Node), pointer :: prev => null()
    type(child_list) , allocatable :: child 
    contains 
    procedure :: size=>child_length
    procedure :: split=> split_into_two_nodes
    procedure :: initialise => initialise_parent_node
    procedure, private :: destroy => parent_node_destroyed
  end type Parent_Node

  type linked_list
    integer, private           :: num_parent_nodes
    integer, private           :: total_nodes
    type(Parent_Node), pointer, private :: head => null()
    type(Parent_Node), pointer, private :: tail => null()
    contains
    procedure :: append => append_at_child_tail
    procedure :: append_new_child => append_in_new_child
    procedure :: remove => remove_node_at_index_parent
    procedure :: destroy => destroy_whole_linked_list
    procedure :: get => get_element_at_index_in_parent 
    procedure :: insert => insert_in_parent_at_index
    procedure :: size => get_total_nodes
    procedure :: replace => replace_in_parent_at_index
    procedure :: reverse => reverse_linked_list
  end type linked_list

  contains

  pure function get_total_nodes ( this_linked_list ) result ( length ) 
    class(linked_list), intent(in) :: this_linked_list
    integer                        :: length
    length = this_linked_list%total_nodes
  end function get_total_nodes

  pure function child_length( this_parent_node ) result( size ) 
    class(Parent_Node), intent(in) :: this_parent_node
    integer :: size 
    size = this_parent_node%child%size()
  end function child_length

  subroutine append_at_child_tail( this_linked_list, item ) 
    ! initialisation of list to be used and item
    class(linked_list), intent(inout) :: this_linked_list
    class(*), intent(in) :: item
    integer :: temp
    real :: r

    ! Finding if its a first node or the list already have a node
    if( this_linked_list%num_parent_nodes == 0 ) then
      call this_linked_list%append_new_child(item)
    else
      if( this_linked_list%tail%child%size() > SPLIT_POINT ) then
        temp = MAX_SIZE-this_linked_list%tail%child%size()
        call random_number(r);
        if( r*( MAX_SIZE-SPLIT_POINT ) >= temp ) then
          call this_linked_list%tail%split();
          this_linked_list%num_parent_nodes = this_linked_list%num_parent_nodes + 1;
          if( associated(this_linked_list%tail%next) ) this_linked_list%tail => this_linked_list%tail%next
        end if
      end if
      call this_linked_list%tail%child%push(item)
    end if
    this_linked_list%total_nodes = this_linked_list%total_nodes + 1
  end subroutine append_at_child_tail

  pure subroutine initialise_parent_node( this_parent_node, item )
    class(Parent_Node), intent(inout) :: this_parent_node
    type(child_list), intent(in) :: item 
    ! allocating item to the new node
    allocate(this_parent_node%child, source=item)
  end subroutine initialise_parent_node

  pure subroutine append_in_new_child( this_linked_list, item ) 
    ! initialisation of list to be used and item
    class(linked_list), intent(inout) :: this_linked_list
    class(*), intent(in)  :: item
    type(child_list)            :: new_child 

    call new_child%push(item)
    if( this_linked_list%num_parent_nodes == 0 )then
      call this_linked_list%head%initialise(new_child)
      this_linked_list%tail => this_linked_list%head
    else
      call this_linked_list%tail%next%initialise(new_child)
      this_linked_list%tail%next%prev => this_linked_list%tail
      this_linked_list%tail => this_linked_list%tail%next
      this_linked_list%tail%child%head%prev => this_linked_list%tail%prev%child%tail
      this_linked_list%tail%prev%child%tail%next => this_linked_list%tail%child%head
    end if
    this_linked_list%num_parent_nodes = this_linked_list%num_parent_nodes + 1
  end subroutine append_in_new_child

  pure subroutine destroy_whole_linked_list( this_linked_list )
    !Entrada:
    class(linked_list), intent(inout) :: this_linked_list
    !Local:
    type(Parent_Node), pointer:: current_node
    do while ( this_linked_list%num_parent_nodes > 0 )
      current_node => this_linked_list%head
      if ( associated(current_node%next) ) then
        nullify(current_node%next%prev)
        this_linked_list%head => current_node%next
      end if
      call current_node%child%destroy()
      call current_node%destroy()
      deallocate(current_node)
      this_linked_list%num_parent_nodes = this_linked_list%num_parent_nodes - 1
    end do
      
  end subroutine destroy_whole_linked_list

  ! Delete a node from the list and frees the memory in the item.
  pure subroutine parent_node_destroyed( this_linked_list )
    !initialising:
    class(parent_node), intent(inout) :: this_linked_list

    !Deallocate it's item
    if ( allocated(this_linked_list%child) ) deallocate(this_linked_list%child)
    !Nullify it's pointers
    nullify(this_linked_list%next)
    nullify(this_linked_list%prev)
  end subroutine parent_node_destroyed

  function get_element_at_index_in_parent( this_linked_list, node_index ) result ( return_item )
    class(linked_list), intent(inout) :: this_linked_list
    integer, intent(in):: node_index
    class(*), pointer :: return_item
    type(Parent_Node), pointer:: current_node
    integer:: count
    !iterating through the list to reach the nth node
    if( this_linked_list%total_nodes == 0 ) return
    count = node_index
    if( count <= 0 ) count = 1;
    if( count >= this_linked_list%total_nodes ) count = this_linked_list%total_nodes;
    current_node => this_linked_list%head
    do while ( associated(current_node) )
      if( count <= current_node%child%size() ) then
        return_item => current_node%child%get(count)
        return
      else
        count = count - current_node%child%size()
        current_node => current_node%next
      end if
    end do
    nullify(current_node)
    nullify(return_item)
  end function get_element_at_index_in_parent

  pure subroutine remove_node_at_index_parent( this_linked_list, node_index )
        
    class(linked_list), intent(inout) :: this_linked_list
    integer, intent(in):: node_index

    type(Parent_Node), pointer:: current_node
    integer:: count

    count = node_index
    current_node => this_linked_list%head
    if( node_index <= 0 ) return;
    if( node_index > this_linked_list%total_nodes ) return;
    
    do while( count > current_node%child%size() )
      count=count-current_node%child%size()
      current_node => current_node%next
    end do
    call current_node%child%remove(count);
    if ( current_node%child%size() == 0 ) then
      if ( associated(current_node%prev) .and. associated(current_node%next) ) then
        !List Node is in mid
        current_node%prev%child%tail%next => current_node%next%child%head
        current_node%next%child%head%prev => current_node%prev%child%tail
        current_node%next%prev => current_node%prev
        current_node%prev%next => current_node%next
  
      else if ( associated(current_node%prev) ) then
        !List tail
        nullify(current_node%prev%child%tail%next)
        nullify(current_node%prev%next)
        this_linked_list%tail => current_node%prev
  
      else if ( associated(current_node%next) ) then
        !List head
        nullify(current_node%next%child%head%prev)
        nullify(current_node%next%prev)
        this_linked_list%head => current_node%next
      end if
      !Destroy node content and Free it's memory
      call current_node%destroy()  
      deallocate(current_node)
      !Reduce the count by 1
      this_linked_list%num_parent_nodes = this_linked_list%num_parent_nodes - 1
    end if
    this_linked_list%total_nodes = this_linked_list%total_nodes-1
  end subroutine remove_node_at_index_parent

  subroutine insert_in_parent_at_index( this_linked_list, item, node_index ) 
    class(linked_list), intent(inout) :: this_linked_list
    integer, intent(in):: node_index
    class(*), intent(in)       :: item
    type(Parent_Node), pointer:: current_node 
    real :: r
    integer :: count, temp
    count = node_index
    current_node => this_linked_list%head
    if( this_linked_list%total_nodes == 0 ) then
      call this_linked_list%append(item);
      return
    end if
    if( count <= 0 ) count = 1;
    if( count > this_linked_list%total_nodes ) count = this_linked_list%total_nodes+1;
    do while( count > current_node%child%size() )
      count = count - current_node%child%size()
      current_node => current_node%next
    end do
    
    if( current_node%child%size() > (MAX_SIZE-1000) ) then
      temp = MAX_SIZE-current_node%child%size()
      call random_number(r);
        if( r*1000 >= temp ) then
        call current_node%split();
        this_linked_list%num_parent_nodes = this_linked_list%num_parent_nodes + 1;
        if( associated(this_linked_list%tail%next) ) this_linked_list%tail => this_linked_list%tail%next
      end if
    end if
    do while( count > current_node%child%size() )
      count= count - current_node%child%size()
      current_node => current_node%next
    end do
    call current_node%child%insert(item,count);
    this_linked_list%total_nodes = this_linked_list%total_nodes + 1
  end subroutine insert_in_parent_at_index

  pure subroutine split_into_two_nodes( this_parent_node )
    class(Parent_Node), intent(inout) :: this_parent_node;
    type(Parent_Node), pointer        :: next_parent_node;
    type(node), pointer               :: old_child_tail;
    type(child_list)                  :: new_child_list
    integer :: node_child_size
    integer :: count
    node_child_size = this_parent_node%child%size()/2;
    count = 1
    old_child_tail => this_parent_node%child%head
    do while( count < node_child_size)
      count = count+1
      old_child_tail => old_child_tail%next
    end do
    new_child_list%head => old_child_tail%next
    new_child_list%tail => this_parent_node%child%tail
    this_parent_node%child%tail => old_child_tail
    call new_child_list%set_size(this_parent_node%child%size()-node_child_size)
    call this_parent_node%child%set_size(node_child_size)

    if( associated(this_parent_node%next) ) then
      next_parent_node => this_parent_node%next
      call this_parent_node%next%initialise(new_child_list)
      this_parent_node%next%next => next_parent_node
      this_parent_node%next%prev => next_parent_node%prev
      next_parent_node%prev => this_parent_node%next
    else
      call this_parent_node%next%initialise(new_child_list)
      next_parent_node = this_parent_node
      next_parent_node%next%prev => next_parent_node 
    end if
  end subroutine split_into_two_nodes

  pure subroutine replace_in_parent_at_index( this_linked_list, item, node_index )

    class(linked_list), intent(inout) :: this_linked_list 
    integer, intent(in)        :: node_index
    class(*), intent(in)       :: item
    type(Parent_Node), pointer        :: current_node
    integer :: count
    
    count = node_index;
    if( count < 1 .or. count > this_linked_list%total_nodes) return;
    current_node => this_linked_list%head;
    do while( count > current_node%child%size() )
      count = count-current_node%child%size();
      current_node => current_node%next;
    end do
    call current_node%child%replace(item, count)
  end subroutine replace_in_parent_at_index

  pure subroutine reverse_linked_list ( this_linked_list )
      class(linked_list), intent(inout) :: this_linked_list 
      type(parent_node), pointer        :: temp_node
      type(node), pointer               :: temp_child_node
      type(parent_node), pointer        :: curr_node
      type(node), pointer               :: curr_child_node

      nullify(temp_child_node) 
      curr_child_node => this_linked_list%head%child%head
      do while ( associated(curr_child_node) )
        temp_child_node => curr_child_node%prev;
        curr_child_node%prev => curr_child_node%next;
        curr_child_node%next => temp_child_node;            
        curr_child_node => curr_child_node%prev;
      end do

      nullify(temp_node) 
      curr_node => this_linked_list%head
      do while ( associated(curr_node) )
        temp_child_node => curr_node%child%head
        curr_node%child%head => curr_node%child%tail
        curr_node%child%tail => temp_child_node 
        temp_node => curr_node%prev;
        curr_node%prev => curr_node%next;
        curr_node%next => temp_node;            
        curr_node => curr_node%prev;
      end do

      temp_node=> this_linked_list%head
      this_linked_list%head => this_linked_list%tail
      this_linked_list%tail => temp_node
      
    end subroutine reverse_linked_list

end module stdlib_linked_list