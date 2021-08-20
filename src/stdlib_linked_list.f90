!> Implementation of a linked list type to hold various types of data.
!>
!> This module provides a heterogeneous generic linked list.
!>

module stdlib_linked_list
    use stdlib_child_list
    implicit none

    ! making Parent_Node and linked_list struct globally available
    public :: Parent_Node
    public  :: linked_list

    ! Maximum size of the child linked list
    integer, private, parameter :: MAX_SIZE = 10000

    ! The number of child list's nodes after which splitting of the parent node begins
    integer, private, parameter :: SPLIT_POINT = INT(0.9*MAX_SIZE)

    !> Defining Parent Node
    !>
    !> The purpose of this node is to hold a child list
    !> and links to previous and next Parent Node.
    type Parent_Node
        type(Parent_Node), pointer :: next => null()
        type(Parent_Node), pointer :: prev => null()
        type(child_list) , allocatable :: child
        contains
        procedure :: size => child_length
        procedure :: split => split_into_two_nodes
        procedure, private :: destroy => parent_node_destroyed
    end type Parent_Node

    !> Defining Linked List
    !>
    !> This linked list is single-dimentional chain of Parent Nodes.
    !> It is a doubly-linked heterogeneous generic list .
    type linked_list
        integer, private           :: num_parent_nodes
        integer, private           :: total_nodes
        type(Parent_Node), pointer :: head => null()
        type(Parent_Node), pointer :: tail => null()
        contains
        procedure :: push => append_at_child_tail
        procedure :: insert => insert_in_parent_at_index
        procedure :: pop => pop_node_at_tail_parent
        procedure :: remove => remove_node_at_index_parent
        procedure :: get => get_element_at_index_in_parent
        procedure :: number_of_parent_nodes => get_number_of_parent_nodes
        procedure :: set_number_of_parent_nodes => set_number_of_parent_nodes
        procedure :: size => get_total_nodes
        procedure :: set_size => set_size_of_list
        procedure :: replace => replace_in_parent_at_index
        procedure :: reverse => reverse_linked_list
        procedure :: clear => clear_whole_linked_list
        procedure :: concat => concat_at_end_of_list
        procedure :: absorb => absorb_another_list
        procedure :: slice => slice_a_part_of_list
        procedure :: splice => splice_a_part_of_list
    end type linked_list

    contains

    !> Creates a Parent Node that contains 'item' as its child
    !>
    !> Returns the new parent node                 
    pure function initialise_parent_node( item ) result( new_node )
        type(Parent_Node) :: new_node
        type(child_list), intent(in) :: item

        ! allocating item to the new node's child
        allocate(new_node%child, source=item)

    end function initialise_parent_node


    !> Returns the number of nodes stored in the input parent node's child list
    pure function child_length( this_parent_node ) result( size )
        class(Parent_Node), intent(in) :: this_parent_node
        integer :: size

        size = this_parent_node%child%size()

    end function child_length

    !> Splits the input parent node into two half and
    !> connects them with next and prev references
    pure subroutine split_into_two_nodes( this_parent_node )

        !
        class(Parent_Node), intent(inout) :: this_parent_node;
        type(Parent_Node), pointer        :: next_parent_node;
        type(node), pointer               :: old_child_tail;
        type(child_list)                  :: new_child_list
        integer :: node_child_size
        integer :: i
        

        node_child_size = this_parent_node%child%size()/2;

        ! Iterating to the mid point of the list to find tail for old child
        i = 1
        old_child_tail => this_parent_node%child%head
        do while( i < node_child_size)
            i = i+1
            old_child_tail => old_child_tail%next
        end do

        ! Associating new child's head and tail
        new_child_list%head => old_child_tail%next
        new_child_list%tail => this_parent_node%child%tail

        ! Associating old child's tail
        this_parent_node%child%tail => old_child_tail

        ! Change the size of the linked lists
        call new_child_list%set_size(this_parent_node%child%size()-node_child_size)
        call this_parent_node%child%set_size(node_child_size)

        ! Fitting in the new parent node with proper next and prev references
        if( associated(this_parent_node%next) ) then
            next_parent_node => this_parent_node%next
            allocate(this_parent_node%next, source=initialise_parent_node(new_child_list))
            this_parent_node%next%next => next_parent_node
            this_parent_node%next%prev => next_parent_node%prev
            next_parent_node%prev => this_parent_node%next
        else
            allocate(this_parent_node%next, source=initialise_parent_node(new_child_list))
            next_parent_node = this_parent_node
            next_parent_node%next%prev => next_parent_node
        end if

    end subroutine split_into_two_nodes


    !> Delete a node and frees the memory in the item.
    pure subroutine parent_node_destroyed( this_linked_list )
        class(parent_node), intent(inout) :: this_linked_list
        
        !Deallocate it's child
        if ( allocated(this_linked_list%child) ) deallocate(this_linked_list%child)
        
        !Nullify it's pointers
        nullify(this_linked_list%next)
        nullify(this_linked_list%prev)

    end subroutine parent_node_destroyed


    !> Insert 'item' at the tail of the input linked list
    subroutine append_at_child_tail( this_linked_list, item )

        class(linked_list), intent(inout) :: this_linked_list
        class(*), intent(in) :: item
        integer :: temp
        real :: r
        type(child_list) :: new_child

        ! Finding if its a first node or the list already have a node
        if( this_linked_list%num_parent_nodes == 0 ) then
            ! Linked List is empty. Associating head and tail of the input linked list
            call new_child%push(item)
            allocate(this_linked_list%head, source=initialise_parent_node(new_child))
            this_linked_list%tail => this_linked_list%head
            this_linked_list%num_parent_nodes = this_linked_list%num_parent_nodes + 1
        else
            ! Checking if the tail node of linked list is needed to break into two parent nodes.
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


    !> Insert 'item' at the given 'node_index' of the input parent list
    subroutine insert_in_parent_at_index( this_linked_list, item, node_index )
        class(linked_list), intent(inout) :: this_linked_list
        integer, intent(in):: node_index
        class(*), intent(in)       :: item
        type(Parent_Node), pointer:: current_node
        real :: r
        integer :: index, temp

        ! This index will be reference for child list
        index = node_index
        current_node => this_linked_list%head
        if( this_linked_list%total_nodes == 0 ) then
            call this_linked_list%push(item);
            return
        end if

        ! will insert before head when the input index is less than 1
        if( index <= 0 ) index = 1;

        ! will insert after tail when the input is more than size of the linked list
        if( index > this_linked_list%total_nodes ) index = this_linked_list%total_nodes+1;
        
        ! Iterating through parent nodes while size of the child list is smaller than index
        do while( index > current_node%child%size()+1 )
            index = index - current_node%child%size()
            current_node => current_node%next
        end do

        ! Checking if the current node is needed to split into two parent nodes.
        if( current_node%child%size() > (MAX_SIZE-1000) ) then
            temp = MAX_SIZE-current_node%child%size()
            call random_number(r);
            if( r*1000 >= temp ) then
                call current_node%split();
                this_linked_list%num_parent_nodes = this_linked_list%num_parent_nodes + 1;
                if( associated(this_linked_list%tail%next) ) this_linked_list%tail => this_linked_list%tail%next
            end if
        end if

        do while( index > current_node%child%size()+1 )
            index = index - current_node%child%size()
            current_node => current_node%next
        end do
        
        ! Insert 'item' in the child list at index
        call current_node%child%insert(item,index);
        this_linked_list%total_nodes = this_linked_list%total_nodes + 1

    end subroutine insert_in_parent_at_index


    !> Removing the last node from the input linked list
    pure subroutine pop_node_at_tail_parent( this_linked_list )

        class(linked_list), intent(inout) :: this_linked_list
        type(Parent_Node), pointer :: current_node

        ! return if the size of the linked list is 0
        if( this_linked_list%total_nodes == 0 ) return;
        
        ! pop the last node of the child list of the tail parent node
        current_node => this_linked_list%tail
        call current_node%child%pop()

        ! if child list of tail parent node is empty, remove the tail parent node
        if ( current_node%child%size() == 0 ) then
            if ( associated(current_node%prev) .and. associated(current_node%next) ) then
                !Parent Node is in mid
                current_node%prev%child%tail%next => current_node%next%child%head
                current_node%next%child%head%prev => current_node%prev%child%tail
                current_node%next%prev => current_node%prev
                current_node%prev%next => current_node%next

            else if ( associated(current_node%prev) ) then
                !Parent Node is tail
                nullify(current_node%prev%child%tail%next)
                nullify(current_node%prev%next)
                this_linked_list%tail => current_node%prev

            else if ( associated(current_node%next) ) then
                !Parent Node is head
                nullify(current_node%next%child%head%prev)
                nullify(current_node%next%prev)
                this_linked_list%head => current_node%next

            else
                !Parent Node is the Last Node
                nullify(this_linked_list%head)
                nullify(this_linked_list%tail)
            end if

            !Destroy Paret Node's content and Free it's memory
            call current_node%destroy()
            deallocate(current_node)

            !Reduce the number of parent nodes by 1
            this_linked_list%num_parent_nodes = this_linked_list%num_parent_nodes - 1
        end if

        this_linked_list%total_nodes = this_linked_list%total_nodes-1

    end subroutine pop_node_at_tail_parent


    !> Removing the node at the given 'node_index' from the input linked list
    pure subroutine remove_node_at_index_parent( this_linked_list, node_index )

        class(linked_list), intent(inout) :: this_linked_list
        integer, intent(in):: node_index

        type(Parent_Node), pointer:: current_node
        integer:: index

        ! This index will be reference for child list
        index = node_index
        current_node => this_linked_list%head

        ! return if the given node index is not in range of 1 to size of linked list
        if( node_index <= 0 ) return;
        if( node_index > this_linked_list%total_nodes ) return;


        ! Iterating through parent nodes while size of the child list is smaller index
        do while( index > current_node%child%size() )
            index=index-current_node%child%size()
            current_node => current_node%next
        end do
        call current_node%child%remove(index);

        ! if child list of current parent node is empty, remove the current parent node
        if ( current_node%child%size() == 0 ) then
            if ( associated(current_node%prev) .and. associated(current_node%next) ) then
                !Parent Node is in mid
                current_node%prev%child%tail%next => current_node%next%child%head
                current_node%next%child%head%prev => current_node%prev%child%tail
                current_node%next%prev => current_node%prev
                current_node%prev%next => current_node%next

            else if ( associated(current_node%prev) ) then
                !Parent Node is tail
                nullify(current_node%prev%child%tail%next)
                nullify(current_node%prev%next)
            this_linked_list%tail => current_node%prev

            else if ( associated(current_node%next) ) then
                !Parent Node is head
                nullify(current_node%next%child%head%prev)
                nullify(current_node%next%prev)
            this_linked_list%head => current_node%next
            
            else
                !Parent Node is the Last Node
                nullify(this_linked_list%head)
                nullify(this_linked_list%tail)
            end if
            
            !Destroy Paret Node's content and Free it's memory
            call current_node%destroy()
            deallocate(current_node)

            !Reduce the number of parent nodes by 1
            this_linked_list%num_parent_nodes = this_linked_list%num_parent_nodes - 1
        end if

        this_linked_list%total_nodes = this_linked_list%total_nodes-1

    end subroutine remove_node_at_index_parent


    !> Returns the pointer to the item stored at 'node_index' in the input linked list
    !> 
    !> Returns a pointer 
    function get_element_at_index_in_parent( this_linked_list, node_index ) result ( return_item )
        class(linked_list), intent(inout) :: this_linked_list
        integer, intent(in):: node_index
        class(*), pointer :: return_item
        type(Parent_Node), pointer:: current_node
        integer:: index

        nullify(return_item)
        
        ! return if the input linked list is empty
        if( this_linked_list%total_nodes == 0 ) return
        
        ! This index will be reference for child list
        index = node_index

        ! Handling out of range index cases
        if( index <= 0 ) index = 1;
        if( index >= this_linked_list%total_nodes ) index = this_linked_list%total_nodes;
        
        ! Iterating through parent nodes while size of the child list is smaller index
        current_node => this_linked_list%head
        do while ( associated(current_node) )

            if( index <= current_node%child%size() ) then
                ! Return the pointer to item stored at specified index
                return_item => current_node%child%get(index)
                return
            else
                index = index - current_node%child%size()
                current_node => current_node%next
            end if
        end do
        nullify(current_node)

    end function get_element_at_index_in_parent

    
    !> Returns the number of parent nodes in the input linked list
    !> 
    !> Returns an integer
    pure function get_number_of_parent_nodes ( this_linked_list ) result ( length )
        class(linked_list), intent(in) :: this_linked_list
        integer                        :: length
        
        length = this_linked_list%num_parent_nodes

    end function get_number_of_parent_nodes


    !> Returns the total number of nodes in the input linked list
    !> 
    !> Returns an integer
    pure function get_total_nodes ( this_linked_list ) result ( length )
        class(linked_list), intent(in) :: this_linked_list
        integer                        :: length

        length = this_linked_list%total_nodes
    
    end function get_total_nodes

    
    !> Changes the size of the input linked list to 'length'
    pure subroutine set_size_of_list (this_linked_list, length)
        class(linked_list), intent(inout) :: this_linked_list
        integer, intent(in)                       :: length
    
        this_linked_list%total_nodes = length
    
    end subroutine set_size_of_list


    !> Changes the number of parent nodes of the input linked list to 'length'
    pure subroutine set_number_of_parent_nodes (this_linked_list, length)
        class(linked_list), intent(inout) :: this_linked_list
        integer, intent(in)                       :: length
        
        this_linked_list%num_parent_nodes = length
    
    end subroutine set_number_of_parent_nodes

    !> Replaces the item stored in node at 'node_index' of the input linked list with 'new_item'
    pure subroutine replace_in_parent_at_index( this_linked_list, new_item, node_index )

        class(linked_list), intent(inout) :: this_linked_list
        integer, intent(in)        :: node_index
        class(*), intent(in)       :: new_item
        type(Parent_Node), pointer        :: current_node
        integer :: index

        ! This index will be reference for child list
        index = node_index;
        
        ! return if the given node index is not in range of 1 to size of linked list
        if( index < 1 .or. index > this_linked_list%total_nodes) return;
        
        ! Iterating through parent nodes while size of the child list is smaller than index
        current_node => this_linked_list%head;
        do while( index > current_node%child%size() )
            index = index-current_node%child%size();
            current_node => current_node%next;
        end do

        call current_node%child%replace(new_item, index)

    end subroutine replace_in_parent_at_index


    !> Reverses the input linked list
    pure subroutine reverse_linked_list ( this_linked_list )
        class(linked_list), intent(inout) :: this_linked_list
        type(parent_node), pointer        :: temp_parent_node
        type(node), pointer               :: temp_child_node
        type(parent_node), pointer        :: curr_parent_node
        type(node), pointer               :: curr_child_node

        ! return if the linked list is empty 
        if( this_linked_list%total_nodes == 0 ) return;
        
        nullify(temp_child_node)
        
        ! Reversing all the child lists 
        curr_child_node => this_linked_list%head%child%head
        do while ( associated(curr_child_node) )
            temp_child_node => curr_child_node%prev;
            curr_child_node%prev => curr_child_node%next;
            curr_child_node%next => temp_child_node;
            curr_child_node => curr_child_node%prev;
        end do

        ! Reversing all the Parent nodes and
        ! Swapping head of the child node with tail of the child node
        nullify(temp_parent_node)
        curr_parent_node => this_linked_list%head
        do while ( associated(curr_parent_node) )

            ! Swapping head with tail (child list)
            temp_child_node => curr_parent_node%child%head
            curr_parent_node%child%head => curr_parent_node%child%tail
            curr_parent_node%child%tail => temp_child_node

            ! Reversing Connections of Parent Nodes
            temp_parent_node => curr_parent_node%prev;
            curr_parent_node%prev => curr_parent_node%next;
            curr_parent_node%next => temp_parent_node;

            curr_parent_node => curr_parent_node%prev;
        end do

        ! Swapping the head of the linked list with tail of the linked list 
        temp_parent_node=> this_linked_list%head
        this_linked_list%head => this_linked_list%tail
        this_linked_list%tail => temp_parent_node

    end subroutine reverse_linked_list

    
    !> Destroy the whole given linked list
    !> Free all the allocated memory
    !> Nullify all the variables 
    pure subroutine clear_whole_linked_list( this_linked_list )
        class(linked_list), intent(inout) :: this_linked_list
        type(Parent_Node), pointer:: current_node

        !> Iterating through the parent nodes to destroy them 
        do while ( this_linked_list%num_parent_nodes > 0 )
            
            current_node => this_linked_list%head
            if ( associated(current_node%next) ) then
                nullify(current_node%next%prev)
                this_linked_list%head => current_node%next
            end if
            
            !destroy the whole child list
            call current_node%child%clear()

            ! Destroy the current node
            call current_node%destroy()
            deallocate(current_node)

            !Decrement the number of parent nodes
            this_linked_list%num_parent_nodes = this_linked_list%num_parent_nodes - 1
        end do

    end subroutine clear_whole_linked_list


    !> Concat one input linked list (list_to_concat) 
    !> at the end of other input linked list (this_linked_list)
    !> 
    !> Creates a deep copy of the list_to_concat and 
    !> appends it at the end of this_linked_list
    subroutine concat_at_end_of_list( this_linked_list, list_to_concat )
        class(linked_list), intent(inout) :: this_linked_list
        type(linked_list), intent(inout) :: list_to_concat
        type(node), pointer :: current_node
        
        ! Return if list to append is empty
        if(list_to_concat%size() == 0) return;
        
        ! Push every item from list_of _concat to this_linked_list
        current_node => list_to_concat%head%child%head
        do while(associated(current_node))
            call this_linked_list%push(current_node%item)
            current_node => current_node%next
        end do

    end subroutine concat_at_end_of_list

    !> Absorb one input linked list (list_to_concat) 
    !> at the end of other input linked list (this_linked_list)
    !> 
    !> Creates a shallow copy of the list_to_concat and 
    !> appends it at the end of this_linked_list
    subroutine absorb_another_list( this_linked_list, list_to_absorb )
        class(linked_list), intent(inout) :: this_linked_list
        type(linked_list), intent(inout) :: list_to_absorb
        integer :: total

        ! Return if list to append is empty
        if(list_to_absorb%size() == 0) return

        ! if this_linked_list is empty
        if(this_linked_list%size() == 0) then
            this_linked_list%head => list_to_absorb%head;
            this_linked_list%tail => list_to_absorb%tail;
        else
            this_linked_list%tail%next => list_to_absorb%head 
            list_to_absorb%head%prev => this_linked_list%tail
            this_linked_list%tail%child%tail%next => list_to_absorb%head%child%head
            list_to_absorb%head%child%head%prev => this_linked_list%tail%child%tail 
            this_linked_list%tail => list_to_absorb%tail;
        end if

        nullify(list_to_absorb%head)
        nullify(list_to_absorb%tail)

        ! Change the size of the linked lists
        call this_linked_list%set_size(this_linked_list%size() + list_to_absorb%size())
        total = this_linked_list%number_of_parent_nodes() + list_to_absorb%number_of_parent_nodes();
        
        call this_linked_list%set_number_of_parent_nodes(total)
        call list_to_absorb%set_size(0)
        call list_to_absorb%set_number_of_parent_nodes(0)

    end subroutine absorb_another_list


    !> Returns a linked list that is a slice part of the input linked list
    !> Starting from index start till end
    !> Returns a linked list

    function slice_a_part_of_list( this_linked_list, start, end ) result ( return_list )
        class(linked_list), intent(inout) :: this_linked_list
        type(linked_list) :: return_list
        type(node), pointer :: current_node
        integer, intent(inout) :: start
        integer, intent(inout) :: end
        integer :: i = 1

        ! return if the index is out-side range of 1 to size of linked list
        if(this_linked_list%size() == 0) return;
        if(start>end) return;
        start = max(start,1)
        start = min(start,this_linked_list%size())
        end = max(end,1)
        end = min(end,this_linked_list%size())


        !iterating to find start
        current_node => this_linked_list%head%child%head
        do while(i < start)
            current_node => current_node%next
            i = i+1
        end do
        
        !iterating to find end
        do while(associated(current_node) .and. (i <= end))
            call return_list%push(current_node%item)
            current_node => current_node%next
            i = i+1
        end do

    end function slice_a_part_of_list



    subroutine splice_a_part_of_list (this_linked_list, start, end)
        class(linked_list), intent(inout) :: this_linked_list
        type(parent_node), pointer :: start_parent_node
        type(parent_node), pointer :: end_parent_node
        type(node), pointer :: current_node 
        type(node), pointer :: next_node
        type(node), pointer :: prev_node
        integer, intent(inout) :: start
        integer, intent(inout) :: end
        integer :: ptr
        integer :: count
        integer :: nodes_in_start_parent_node
        integer :: nodes_in_end_parent_node
        class(*), pointer :: data

        !nullify every pointer
        nullify(start_parent_node)
        nullify(end_parent_node)
        nullify(current_node)
        nullify(next_node)
        nullify(prev_node)


        ! return if the input linked list is empty
        if(this_linked_list%size() == 0) return;

        ! return if input start is nore than input end 
        if(start>end) return;

        ! handling the out of range index
        start = max(start,1)
        start = min(start,this_linked_list%size())
        end = max(end,1)
        end = min(end,this_linked_list%size())

        ! destroy the whole llist
        if(end == this_linked_list%size() .and. start == 1) then
            call this_linked_list%clear()
            return
        end if
        count = 0
        
        !iterating through the linked list to find the end parent node
        end_parent_node => this_linked_list%head;
        ptr = 0
        do while(associated(end_parent_node))
            if(ptr+end_parent_node%child%size() > end) exit;
            ptr = ptr + end_parent_node%child%size()
            end_parent_node => end_parent_node%next
            count = count+1;
        end do
        nodes_in_end_parent_node = ptr


        !iterating through the linked list to find the end parent node
        if(start /= 1) then 
            start_parent_node => this_linked_list%head;
            ptr = 1
            do while(associated(start_parent_node))
                if(ptr+start_parent_node%child%size() >= start) exit;
                ptr = ptr + start_parent_node%child%size()
                start_parent_node => start_parent_node%next
                count = count-1
            end do
            nodes_in_start_parent_node = ptr-1;
        end if

        ! iterating to the find the start_node 
        ptr = 1
        current_node => this_linked_list%head%child%head
        do while(ptr < start)
            current_node => current_node%next
            ptr = ptr+1
        end do
        prev_node => current_node%prev
        if(associated(prev_node)) then
        end if
        
        ! iterating to find the last node to splice 
        do while(associated(current_node) .and. (ptr <= end))
            next_node => current_node%next
            if (associated(current_node%prev).and.associated(current_node%next)) then
                current_node%next%prev => current_node%prev
                current_node%prev%next => current_node%next
            else if (associated(current_node%prev)) then
                nullify(current_node%prev%next)
            else if (associated(current_node%next)) then
                nullify(current_node%next%prev)
            end if
            call current_node%clear()
            deallocate(current_node)
            current_node => next_node
            ptr = ptr+1
        end do

        ! Connecting the parent nodes
        if(count == 0) then
            if(associated(start_parent_node)) call start_parent_node%child%set_size(start_parent_node%child%size() - (end-start+1))
        else
            if(associated(start_parent_node)) then
                if(start-nodes_in_start_parent_node-1>0) call start_parent_node%child%set_size(start-nodes_in_start_parent_node-1)
                start_parent_node%next => end_parent_node
                start_parent_node%child%tail => prev_node
            end if
            if(associated(end_parent_node)) then
                call end_parent_node%child%set_size(end_parent_node%child%size() + nodes_in_end_parent_node - end)
                end_parent_node%prev => start_parent_node
                end_parent_node%child%head => current_node
            end if
        end if
        
        ! setting up new linked list tail if needed
        if(end == this_linked_list%size()) then
            count = count+1 
            this_linked_list%tail => start_parent_node
        end if

        ! setting up new linked list head if needed
        if(start == 1) then 
            count = count + 1
            this_linked_list%head => end_parent_node
        end if

        ! Changing size of the linked list corrospondingly 
        call this_linked_list%set_size( this_linked_list%size() - (end - start + 1) )
        if(count>1) call this_linked_list%set_number_of_parent_nodes(this_linked_list%number_of_parent_nodes() - count + 1)
    
    end subroutine splice_a_part_of_list

end module stdlib_linked_list
