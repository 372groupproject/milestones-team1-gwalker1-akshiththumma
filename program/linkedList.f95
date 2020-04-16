!begining of module
module linkedList
        implicit none

	! define the type node for the linked list
        type node
		!contains a number and a next pointer
                integer :: val
                type(node), pointer :: next
        end type node
! this keyword lets us describe what functions/subroutines are in the module
contains

!this function takes the head of a list and a new node to be added
!we move to the end of the list and add the node
subroutine insert(head, newnode)
        implicit none
	!the new node must be specified as a target so we can point at it
        type(node), target :: newnode
        type(node) :: head
	
	!this is the variable we'll use to move through the list
	!it is specified as a pointer so we can point at the nodes in the list
        type(node),pointer :: curr
	
	!if the list is empty. that is, if head points to nothing.
	!the associated function returns true if a pointer has a target
	!false otherwise
        if(.not. associated(head%next)) then
                head%next => newnode
                nullify(newnode%next)
                return
        end if
	
	!point curr at the first item
        curr => head%next

	!move through the list until the end
        do while(associated(curr%next))
        !       print *, curr%val
                curr => curr%next
        end do

	!add the new node to the end and make sure it is not pointing to anything
	!using 'nullify' function
        curr%next=>newnode
        nullify(newnode%next)

end subroutine insert

subroutine remove(head, valToRemove)
        implicit none
        type(node) :: head
        type(node), pointer :: curr
        integer :: valToRemove
        type(node) :: searched
        type(node), pointer :: prev

        searched = search(head, valToRemove)

        if (searched%val .eq. -1) then
                print *, "Can't remove non-existent item"
                return
        end if

        if (head%next%val .eq. valToRemove .and. .not. associated(head%next%next)) then
                nullify(head%next)
                return
        end if

        curr=>head%next%next
        prev=>head%next

        do while(associated(curr))
                if (head%next%val .eq. valToRemove) then
                        head%next=>head%next%next
                        return
                else if (curr%val .eq. valToRemove) then
                        prev%next=>curr%next
                        !curr=>prev
                        return
                else
                        prev=>prev%next
                        curr=>curr%next
                end if
        end do
end subroutine remove

type(node) function search(head, searchVal)
        implicit none
        type(node) :: head
        type(node), pointer :: curr
        integer :: searchVal

        if (head%next%val .eq. searchVal) then
                search = head%next
                return
        end if

        curr => head%next
        do while(associated(curr))
                if (curr%val .eq. searchVal) then
                        search = curr
                        return
                else
                        curr => curr%next
                end if
        end do
        print *, "value not found. returning dummy node w/ value -1"
        search = createNode(-1)
        return

end function search

integer function sumList(head)
        type(node) :: head
        integer :: sum = 0
        type(node), pointer :: curr

        curr=>head%next
        do while(associated(curr))
                sum = sum + curr%val
                curr=>curr%next
        end do
        sumList = sum
        return
end function sumList

type(node) function createNode(value)
        implicit none
        integer, intent(in) :: value
        type(node), target :: newNode
        newNode%val = value
        !print *, "New node val = ", newNode%val
        createNode = newNode
end function createNode

subroutine printList(head)
        implicit none
        type(node) :: head
        type(node) :: curr

        curr = head
        do while(associated(curr%next))
                print *, curr%next%val
                curr = curr%next

        end do
end subroutine printList
end module linkedList
