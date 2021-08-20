!> Implementation of a Child list type to hold various types of data.
!>
!> The child list module provides a heterogeneous generic linked list 
!> that acts as a basic building block for the linked list module


module stdlib_child_list
    implicit none

    ! making Node and child_list struct globally available
    public:: Node, child_list

    !> Defining Node
    !>
    !> The purpose of this node is to hold an item
    !> and links to previous and next Node.
    type Node
        type(Node), pointer :: next => null()
        type(Node), pointer :: prev => null()
        class(*), allocatable :: item
        contains
        procedure :: clear => node_destroyed
        procedure, private :: clear_all => all_nodes_destroyed
    end type Node

    !> Defining Child List
    !>
    !> This linked list is single-dimentional chain of Nodes.
    !> It is a doubly-linked heterogeneous generic list .
    type child_list
        integer, private :: num_nodes = 0
        type(Node), pointer :: head => null()
        type(Node), pointer :: tail => null()
        contains
        procedure:: push => push_at_tail
        procedure:: insert => insert_at_index
        procedure:: pop => pop_node_at_tail
        procedure:: remove => remove_node_at_index
        procedure:: get => get_node_at_index
        procedure:: size => get_length
        procedure:: set_size => set_length
        procedure:: replace => replace_at_index
        procedure:: reverse => reverse_child_list
        procedure:: clear => destroy_whole_child_list
    end type child_list

    contains

    !> Creates a Node that contains 'new_item' as its child
    !>
    !> Returns the new parent node   
    pure function initialise_node( new_item ) result( new_node )
        type(node) :: new_node
        class(*), intent(in), optional :: new_item
        
        ! allocating new_item to the new node's item
        allocate(new_node%item, source=new_item)
    end function initialise_node
    
    !> Delete a node and frees the memory in the item.
    pure subroutine node_destroyed( this_node )
        class(node), intent(inout) :: this_node

        !Deallocate it's item
        if (allocated(this_node%item)) deallocate(this_node%item)

        !Nullify it's pointers
        nullify(this_node%next)
        nullify(this_node%prev)
    end subroutine node_destroyed


    pure subroutine all_nodes_destroyed( this_node )
        !Entrada:
        class(node), intent(inout) :: this_node
        !Local:
        type(node), pointer :: current_node
        type(node), pointer :: next_node
        !Deallocate it's item
        current_node = this_node
        next_node => current_node%next
        do
            deallocate(current_node)
            if (.not. associated(next_node)) exit
            current_node => next_node
            next_node => current_node%next
        end do
    end subroutine all_nodes_destroyed


    !> Insert 'item' at the tail of the input child list
    pure subroutine push_at_tail( this_child_list, item )

        class(child_list), intent(inout) :: this_child_list
        class(*), intent(in) :: item

        ! Finding if its a first node or the child_list already have a node
        if (associated(this_child_list%tail)) then
            allocate(this_child_list%tail%next, source=initialise_node(item))
            this_child_list%tail%next%prev => this_child_list%tail
            this_child_list%tail => this_child_list%tail%next
        else
            allocate(this_child_list%head, source=initialise_node(item))
            this_child_list%tail => this_child_list%head
        end if

        this_child_list%num_nodes = this_child_list%num_nodes + 1
    end subroutine push_at_tail


    !> Insert 'item' at the given 'node_index' of the input child list
    pure subroutine insert_at_index( this_child_list, item ,node_index )
        class(child_list), intent(inout) :: this_child_list
        integer, intent(in)        :: node_index
        class(*), intent(in)       :: item
        type(node), pointer        :: current_node
        type(node), pointer        :: next_node

        integer :: index 

        ! This index will be used for iteraing
        index  = node_index-1;

        ! will insert after tail when the input is more than size of the child list
        if(index >=this_child_list%num_nodes) then
            call this_child_list%push(item)
            return
        else if(index <=0) then
            ! will insert after tail when the input is more than size of the child list
            current_node => this_child_list%head
            allocate(this_child_list%head,source = initialise_node(item))
            this_child_list%head%next => current_node
            current_node%prev   => this_child_list%head
        else
            current_node => this_child_list%head
            do while(index >1)
            index  = index -1;
            current_node => current_node%next;
            end do
            next_node => current_node%next
            allocate(current_node%next,source = initialise_node(item))
            current_node%next%prev => current_node
            current_node%next%next => next_node
            current_node => current_node%next
            current_node%next%prev => current_node
        end if
        this_child_list%num_nodes = this_child_list%num_nodes + 1;
    end subroutine insert_at_index


    !> Removing the last node from the input child list
    pure subroutine pop_node_at_tail( this_child_list )

        class(child_list), intent(inout) :: this_child_list

        type(node), pointer:: current_node

        ! return if the size of the child list is 0
        if(this_child_list%num_nodes == 0) return;


        ! poping the last node of the child list
        current_node => this_child_list%tail
        if (associated(current_node%prev).and.associated(current_node%next)) then
            !child_list Node is in mid
            current_node%next%prev => current_node%prev
            current_node%prev%next => current_node%next

        else if (associated(current_node%prev)) then
            !child_list tail
            nullify(current_node%prev%next)
            this_child_list%tail => current_node%prev

        else if (associated(current_node%next)) then
            !child_list head
            nullify(current_node%next%prev)
            this_child_list%head => current_node%next
        else
            nullify(this_child_list%head)
            nullify(this_child_list%tail)
        end if

        !Destroy node content and Free it's memory
        call current_node%clear()
        deallocate(current_node)

        !Reduce the count by 1
        this_child_list%num_nodes = this_child_list%num_nodes - 1
    end subroutine pop_node_at_tail

    !> Removing the node at the given 'node_index' from the input child list
    pure subroutine remove_node_at_index( this_child_list, node_index )

        class(child_list), intent(inout) :: this_child_list
        integer, intent(in):: node_index
        type(node), pointer:: current_node
        
        ! This index will be reference for child list
        integer:: index

        !iterating through the child_list to reach the nth node
        current_node => this_child_list%head
        
        ! return if the given node index is not in range of 1 to size of linked list
        if(node_index<=0) return;
        if(node_index>this_child_list%num_nodes) return;
        index = 1
        do while ( associated(current_node) )
            if (index==node_index) then
            if (associated(current_node%prev).and.associated(current_node%next)) then
                !child_list Node is in mid
                current_node%next%prev => current_node%prev
                current_node%prev%next => current_node%next

            else if (associated(current_node%prev)) then
                !child_list tail
                nullify(current_node%prev%next)
                this_child_list%tail => current_node%prev

            else if (associated(current_node%next)) then
                !child_list head
                nullify(current_node%next%prev)
                this_child_list%head => current_node%next
            else
                !only node in list
                nullify(this_child_list%head)
                nullify(this_child_list%tail)  
            end if

            !Destroy node content and Free it's memory
            call current_node%clear()
            deallocate(current_node)

            !Reduce the index by 1
            this_child_list%num_nodes = this_child_list%num_nodes - 1
            return
            end if
            current_node => current_node%next
            index = index+1
        end do
    end subroutine remove_node_at_index


    !> Returns the pointer to the item stored at 'node_index' in the input child list
    !> 
    !> Returns a pointer
    function get_node_at_index( this_child_list, node_index ) result (return_item)

        class(child_list), intent(inout) :: this_child_list
        integer, intent(in):: node_index
        class(*), pointer :: return_item
        type(node), pointer:: current_node
        integer:: index
        
        !iterating through the child_list to reach the nth node
        current_node => this_child_list%head
        index = 1
        do while ( associated(current_node) )
        
            if (index == node_index) then
                ! Return the pointer to item stored at specified index
                return_item => current_node%item
                nullify(current_node)
                return
            end if
            current_node => current_node%next
            index = index+1
        
        end do
        nullify(current_node)
        nullify(return_item)
    
    end function get_node_at_index

    !> Returns the total number of nodes in the input child list
    !> 
    !> Returns an integer
    pure function get_length ( this_child_list ) result ( length )
        class(child_list), intent(in)  :: this_child_list
        integer                     :: length
        
        length = this_child_list%num_nodes
    
    end function get_length

    
    !> Changes the size of the input child list to 'length'
    pure subroutine set_length ( this_child_list, length )
        class(child_list), intent(inout)  :: this_child_list
        integer, intent(in)      :: length
    
        this_child_list%num_nodes = length
    
    end subroutine set_length

    
    
    !> Replaces the item stored in node at 'node_index' of the input child list with 'new_item'
    pure subroutine replace_at_index( this_child_list, item ,node_index )

        class(child_list), intent(inout) :: this_child_list
        integer, intent(in)        :: node_index
        class(*), intent(in)       :: item
        type(node), pointer        :: current_node
        integer :: index


        ! This index will be reference for child list
        index = node_index;

        ! return if the given node index is not in range of 1 to size of child list
        if(index<1 .or. index>this_child_list%num_nodes) return;


        ! Iterating through parent nodes while size of the child list is smaller than index
        current_node => this_child_list%head;
        do while(index>1)
            index = index-1;
            current_node => current_node%next;
        end do
        current_node%item = item

    end subroutine replace_at_index

    !> Reverses the input child list
    pure subroutine reverse_child_list (this_child_list)
        class(child_list), intent(inout) :: this_child_list
        type(node), pointer        :: temp_node
        type(node), pointer        :: curr_node

        nullify(temp_node)

        ! Swapping head of the child node with tail of the child node
        curr_node => this_child_list%head
        do while (associated(curr_node))
            temp_node => curr_node%prev;
            curr_node%prev => curr_node%next;
            curr_node%next => temp_node;
            curr_node => curr_node%prev;
        end do

        temp_node=> this_child_list%head
        this_child_list%head => this_child_list%tail
        this_child_list%tail => temp_node

    end subroutine reverse_child_list

    !> Destroy the whole given linked list
    !> Free  the allocated memory
    !> Nullify all the variables 
    pure subroutine destroy_whole_child_list( this_child_list )
        !Entrada:
        class(child_list), intent(inout) :: this_child_list
        !Local:
        type(node), pointer:: current_node

        do while (this_child_list%num_nodes>0)
            current_node => this_child_list%head
            if (associated(current_node%next)) then
            nullify(current_node%next%prev)
            this_child_list%head => current_node%next
            end if
            call current_node%clear()
            deallocate(current_node)
            this_child_list%num_nodes = this_child_list%num_nodes - 1
        end do

    end subroutine destroy_whole_child_list
end module stdlib_child_list
