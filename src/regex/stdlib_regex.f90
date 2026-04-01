module stdlib_regex
  use stdlib_kinds, only: dp, sp, int32
  implicit none
  private

  public :: regex_type
  public :: regcomp
  public :: regmatch

  ! Opcodes for NFA states
  integer, parameter :: OP_CHAR  = 1
  integer, parameter :: OP_ANY   = 2
  integer, parameter :: OP_CLASS = 3
  integer, parameter :: OP_START = 4
  integer, parameter :: OP_END   = 5
  integer, parameter :: OP_SPLIT = 6
  integer, parameter :: OP_MATCH = 7
  integer, parameter :: OP_JMP   = 8

  ! Tags for tokens
  integer, parameter :: TOK_CHAR   = 1
  integer, parameter :: TOK_ANY    = 2
  integer, parameter :: TOK_CLASS  = 3
  integer, parameter :: TOK_START  = 4
  integer, parameter :: TOK_END    = 5
  integer, parameter :: TOK_STAR   = 6
  integer, parameter :: TOK_PLUS   = 7
  integer, parameter :: TOK_QUEST  = 8
  integer, parameter :: TOK_LPAREN = 9
  integer, parameter :: TOK_RPAREN = 10
  integer, parameter :: TOK_ALT    = 11
  integer, parameter :: TOK_CONCAT = 12

  type :: state_type
    integer :: op
    character(len=1) :: c
    logical :: bmap(0:127)
    logical :: invert
    integer :: out1
    integer :: out2
  end type state_type

  type :: token_type
    integer :: tag
    character(len=1) :: c
    logical :: bmap(0:127)
    logical :: invert
  end type token_type

  type :: regex_type
    type(state_type), allocatable :: states(:)
    integer :: start_state
    integer :: n_states
  end type regex_type

  type :: out_node
    integer :: s
    integer :: o
    integer :: next
  end type out_node

  type :: out_list_type
    integer :: head
    integer :: tail
  end type out_list_type

  type :: frag_type
    integer :: start
    type(out_list_type) :: out_list
  end type frag_type

  type :: thread
    integer :: state
    integer :: start_pos
  end type thread

  type(out_node), allocatable :: out_pool(:)

contains

  logical function is_term_ender(tag)
    integer, intent(in) :: tag
    is_term_ender = (tag == TOK_CHAR .or. tag == TOK_ANY .or. &
                     tag == TOK_CLASS .or. tag == TOK_STAR .or. &
                     tag == TOK_PLUS .or. tag == TOK_QUEST .or. &
                     tag == TOK_RPAREN .or. tag == TOK_END .or. &
                     tag == TOK_START)
  end function is_term_ender

  logical function is_term_starter(tag)
    integer, intent(in) :: tag
    is_term_starter = (tag == TOK_CHAR .or. tag == TOK_ANY .or. &
                       tag == TOK_CLASS .or. tag == TOK_LPAREN .or. &
                       tag == TOK_START .or. tag == TOK_END)
  end function is_term_starter

  integer function prec(tag)
    integer, intent(in) :: tag
    if (tag == TOK_STAR .or. tag == TOK_PLUS .or. tag == TOK_QUEST) then
      prec = 3
    else if (tag == TOK_CONCAT) then
      prec = 2
    else if (tag == TOK_ALT) then
      prec = 1
    else
      prec = 0
    end if
  end function prec

  subroutine tokenize(pattern, tokens, num_tokens, stat)
    character(len=*), intent(in) :: pattern
    type(token_type), allocatable, intent(out) :: tokens(:)
    integer, intent(out) :: num_tokens
    integer, intent(out) :: stat

    type(token_type), allocatable :: tmp_tokens(:)
    type(token_type) :: t
    integer :: i, k, len_p
    character(len=1) :: c, c1, c2

    len_p = len(pattern)
    allocate(tmp_tokens(len_p * 4 + 1))
    num_tokens = 0
    stat = 0
    i = 1

    do while (i <= len_p)
      c = pattern(i:i)
      t%tag = TOK_CHAR
      t%c = ' '
      t%bmap = .false.
      t%invert = .false.

      if (c == '\') then
        if (i < len_p) then
          i = i + 1
          c = pattern(i:i)
        end if
        t%tag = TOK_CHAR
        t%c = c
        if (c == 'd') then
          t%tag = TOK_CLASS
          do k = iachar('0'), iachar('9'); t%bmap(k) = .true.; end do
        else if (c == 's') then
          t%tag = TOK_CLASS
          t%bmap(iachar(' ')) = .true.
          t%bmap(iachar(char(9))) = .true.
          t%bmap(iachar(char(10))) = .true.
          t%bmap(iachar(char(13))) = .true.
        else if (c == 'w') then
          t%tag = TOK_CLASS
          do k = iachar('a'), iachar('z'); t%bmap(k) = .true.; end do
          do k = iachar('A'), iachar('Z'); t%bmap(k) = .true.; end do
          do k = iachar('0'), iachar('9'); t%bmap(k) = .true.; end do
          t%bmap(iachar('_')) = .true.
        end if
      else if (c == '.') then
        t%tag = TOK_ANY
      else if (c == '*') then
        t%tag = TOK_STAR
      else if (c == '+') then
        t%tag = TOK_PLUS
      else if (c == '?') then
        t%tag = TOK_QUEST
      else if (c == '|') then
        t%tag = TOK_ALT
      else if (c == '(') then
        t%tag = TOK_LPAREN
      else if (c == ')') then
        t%tag = TOK_RPAREN
      else if (c == '^') then
        t%tag = TOK_START
      else if (c == '$') then
        t%tag = TOK_END
      else if (c == '[') then
        t%tag = TOK_CLASS
        i = i + 1
        if (i <= len_p .and. pattern(i:i) == '^') then
          t%invert = .true.
          i = i + 1
        end if
        do while (i <= len_p .and. pattern(i:i) /= ']')
          if (pattern(i:i) == '\') then
            i = i + 1
            if (i > len_p) exit
          end if
          c1 = pattern(i:i)
          if (i + 2 <= len_p .and. pattern(i+1:i+1) == '-') then
            if (pattern(i+2:i+2) /= ']') then
              c2 = pattern(i+2:i+2)
              do k = iachar(c1), iachar(c2)
                if (k >= 0 .and. k <= 127) t%bmap(k) = .true.
              end do
              i = i + 3
              cycle
            end if
          end if
          k = iachar(c1)
          if (k >= 0 .and. k <= 127) t%bmap(k) = .true.
          i = i + 1
        end do
        if (i > len_p) stat = 1 ! missing ]
      else
        t%tag = TOK_CHAR
        t%c = c
      end if
      
      num_tokens = num_tokens + 1
      tmp_tokens(num_tokens) = t
      i = i + 1
    end do

    allocate(tokens(num_tokens * 2 + 1))
    ! Inject concats
    k = 0
    do i = 1, num_tokens
      if (i > 1) then
        if (is_term_ender(tmp_tokens(i-1)%tag) .and. is_term_starter(tmp_tokens(i)%tag)) then
          k = k + 1
          tokens(k)%tag = TOK_CONCAT
          tokens(k)%c = ' '
          tokens(k)%invert = .false.
          tokens(k)%bmap = .false.
        end if
      end if
      k = k + 1
      tokens(k) = tmp_tokens(i)
    end do
    num_tokens = k

  end subroutine tokenize

  subroutine parse_to_postfix(tokens, num_tokens, postfix, num_postfix, stat)
    type(token_type), intent(in) :: tokens(:)
    integer, intent(in) :: num_tokens
    type(token_type), allocatable, intent(out) :: postfix(:)
    integer, intent(out) :: num_postfix
    integer, intent(out) :: stat

    type(token_type), allocatable :: stack(:)
    integer :: top, i, tag
    
    allocate(postfix(num_tokens + 1))
    allocate(stack(num_tokens + 1))
    num_postfix = 0
    top = 0
    stat = 0

    do i = 1, num_tokens
      tag = tokens(i)%tag
      if (tag == TOK_CHAR .or. tag == TOK_ANY .or. tag == TOK_CLASS .or. &
          tag == TOK_START .or. tag == TOK_END) then
        num_postfix = num_postfix + 1
        postfix(num_postfix) = tokens(i)
      else if (tag == TOK_STAR .or. tag == TOK_PLUS .or. tag == TOK_QUEST) then
        num_postfix = num_postfix + 1
        postfix(num_postfix) = tokens(i)
      else if (tag == TOK_LPAREN) then
        top = top + 1
        stack(top) = tokens(i)
      else if (tag == TOK_RPAREN) then
        do while (top > 0)
          if (stack(top)%tag == TOK_LPAREN) exit
          num_postfix = num_postfix + 1
          postfix(num_postfix) = stack(top)
          top = top - 1
        end do
        if (top == 0) then
          stat = 1 ! mismatched parens
          return
        end if
        top = top - 1
      else if (tag == TOK_CONCAT .or. tag == TOK_ALT) then
        do while (top > 0)
          if (stack(top)%tag == TOK_LPAREN) exit
          if (prec(stack(top)%tag) < prec(tag)) exit
          num_postfix = num_postfix + 1
          postfix(num_postfix) = stack(top)
          top = top - 1
        end do
        top = top + 1
        stack(top) = tokens(i)
      end if
    end do

    do while (top > 0)
      if (stack(top)%tag == TOK_LPAREN) then
        stat = 1
        return
      end if
      num_postfix = num_postfix + 1
      postfix(num_postfix) = stack(top)
      top = top - 1
    end do
  end subroutine parse_to_postfix

  integer function new_out(s, o, pool, p_size)
    integer, intent(in) :: s, o
    type(out_node), intent(inout) :: pool(:)
    integer, intent(inout) :: p_size
    p_size = p_size + 1
    pool(p_size)%s = s
    pool(p_size)%o = o
    pool(p_size)%next = 0
    new_out = p_size
  end function new_out

  subroutine merge_lists(l1, l2, res, pool)
    type(out_list_type), intent(in) :: l1, l2
    type(out_list_type), intent(out) :: res
    type(out_node), intent(inout) :: pool(:)
    if (l1%head == 0) then
      res = l2
    else if (l2%head == 0) then
      res = l1
    else
      pool(l1%tail)%next = l2%head
      res%head = l1%head
      res%tail = l2%tail
    end if
  end subroutine merge_lists

  subroutine do_patch(states, list, target, pool)
    type(state_type), intent(inout) :: states(:)
    type(out_list_type), intent(in) :: list
    integer, intent(in) :: target
    type(out_node), intent(in) :: pool(:)
    integer :: curr
    curr = list%head
    do while (curr /= 0)
      if (pool(curr)%o == 1) then
        states(pool(curr)%s)%out1 = target
      else
        states(pool(curr)%s)%out2 = target
      end if
      curr = pool(curr)%next
    end do
  end subroutine do_patch

  subroutine build_nfa(postfix, num_postfix, states, n_states, start_state, stat)
    type(token_type), intent(in) :: postfix(:)
    integer, intent(in) :: num_postfix
    type(state_type), allocatable, intent(out) :: states(:)
    integer, intent(out) :: n_states
    integer, intent(out) :: start_state
    integer, intent(out) :: stat

    type(frag_type), allocatable :: stack(:)
    integer :: top, i, tag, out_idx
    type(frag_type) :: f1, f2
    type(out_list_type) :: t_list, empty_list
    type(out_node), allocatable :: local_pool(:)
    integer :: p_size
    
    empty_list%head = 0
    empty_list%tail = 0

    allocate(states((num_postfix+1) * 2))
    allocate(stack((num_postfix+1) * 2))
    allocate(local_pool((num_postfix+1) * 4))
    p_size = 0
    n_states = 0
    top = 0
    stat = 0

    ! Empty pattern matches immediately
    if (num_postfix == 0) then
       n_states = n_states + 1
       states(n_states)%op = OP_MATCH
       states(n_states)%out1 = 0
       states(n_states)%out2 = 0
       start_state = 1
       return
    end if

    do i = 1, num_postfix
      tag = postfix(i)%tag
      
      select case(tag)
      case (TOK_CHAR, TOK_ANY, TOK_CLASS, TOK_START, TOK_END)
        n_states = n_states + 1
        if (tag == TOK_CHAR) states(n_states)%op = OP_CHAR
        if (tag == TOK_ANY) states(n_states)%op = OP_ANY
        if (tag == TOK_CLASS) states(n_states)%op = OP_CLASS
        if (tag == TOK_START) states(n_states)%op = OP_START
        if (tag == TOK_END) states(n_states)%op = OP_END

        states(n_states)%c = postfix(i)%c
        states(n_states)%bmap = postfix(i)%bmap
        states(n_states)%invert = postfix(i)%invert
        states(n_states)%out1 = 0
        states(n_states)%out2 = 0
        
        top = top + 1
        stack(top)%start = n_states
        out_idx = new_out(n_states, 1, local_pool, p_size)
        stack(top)%out_list%head = out_idx
        stack(top)%out_list%tail = out_idx
        
      case (TOK_CONCAT)
        if (top < 2) then; stat = 1; return; end if
        f2 = stack(top); top = top - 1
        f1 = stack(top)
        
        call do_patch(states, f1%out_list, f2%start, local_pool)
        stack(top)%start = f1%start
        stack(top)%out_list = f2%out_list

      case (TOK_ALT)
        if (top < 2) then; stat = 1; return; end if
        f2 = stack(top); top = top - 1
        f1 = stack(top)
        
        n_states = n_states + 1
        states(n_states)%op = OP_SPLIT
        states(n_states)%out1 = f1%start
        states(n_states)%out2 = f2%start
        
        stack(top)%start = n_states
        call merge_lists(f1%out_list, f2%out_list, stack(top)%out_list, local_pool)
        
      case (TOK_QUEST)
        if (top < 1) then; stat = 1; return; end if
        f1 = stack(top)
        
        n_states = n_states + 1
        states(n_states)%op = OP_SPLIT
        states(n_states)%out1 = f1%start
        states(n_states)%out2 = 0
        
        out_idx = new_out(n_states, 2, local_pool, p_size)
        t_list%head = out_idx
        t_list%tail = out_idx
        call merge_lists(t_list, f1%out_list, stack(top)%out_list, local_pool)
        stack(top)%start = n_states
        
      case (TOK_STAR)
        if (top < 1) then; stat = 1; return; end if
        f1 = stack(top)
        
        n_states = n_states + 1
        states(n_states)%op = OP_SPLIT
        states(n_states)%out1 = f1%start
        states(n_states)%out2 = 0
        
        call do_patch(states, f1%out_list, n_states, local_pool)
        
        out_idx = new_out(n_states, 2, local_pool, p_size)
        stack(top)%out_list%head = out_idx
        stack(top)%out_list%tail = out_idx
        stack(top)%start = n_states

      case (TOK_PLUS)
        if (top < 1) then; stat = 1; return; end if
        f1 = stack(top)
        
        n_states = n_states + 1
        states(n_states)%op = OP_SPLIT
        states(n_states)%out1 = f1%start
        states(n_states)%out2 = 0
        
        call do_patch(states, f1%out_list, n_states, local_pool)
        
        out_idx = new_out(n_states, 2, local_pool, p_size)
        stack(top)%out_list%head = out_idx
        stack(top)%out_list%tail = out_idx
        stack(top)%start = f1%start
        
      end select
    end do
    
    if (top /= 1) then; stat = 1; return; end if
    f1 = stack(1)
    
    n_states = n_states + 1
    states(n_states)%op = OP_MATCH
    states(n_states)%out1 = 0
    states(n_states)%out2 = 0
    
    call do_patch(states, f1%out_list, n_states, local_pool)
    start_state = f1%start
    ! print *, "DEBUG build_nfa success. n_states=", n_states


  end subroutine build_nfa

  subroutine regcomp(re, pattern, status)
    type(regex_type), intent(out) :: re
    character(len=*), intent(in) :: pattern
    integer, intent(out), optional :: status
    
    type(token_type), allocatable :: tokens(:)
    type(token_type), allocatable :: postfix(:)
    integer :: n_tok, n_post, stat
    
    call tokenize(pattern, tokens, n_tok, stat)
    if (stat /= 0) then
       if (present(status)) status = stat
       return
    end if
    
    call parse_to_postfix(tokens, n_tok, postfix, n_post, stat)
    if (stat /= 0) then
       if (present(status)) status = stat
       return
    end if
    
    call build_nfa(postfix, n_post, re%states, re%n_states, re%start_state, stat)
    if (present(status)) status = stat
  end subroutine regcomp

  recursive subroutine add_thread(list, count, state_idx, start_pos, step_index, states, str_len, visited)
    type(thread), intent(inout) :: list(:)
    integer, intent(inout) :: count
    integer, intent(in) :: state_idx, start_pos, step_index
    type(state_type), intent(in) :: states(:)
    integer, intent(in) :: str_len
    integer, intent(inout) :: visited(:)
    integer :: op

    if (state_idx == 0) return
    if (visited(state_idx) == step_index) return
    visited(state_idx) = step_index
    
    op = states(state_idx)%op
    if (op == OP_SPLIT) then
      call add_thread(list, count, states(state_idx)%out1, start_pos, step_index, states, str_len, visited)
      call add_thread(list, count, states(state_idx)%out2, start_pos, step_index, states, str_len, visited)
    else if (op == OP_JMP) then
      call add_thread(list, count, states(state_idx)%out1, start_pos, step_index, states, str_len, visited)
    else if (op == OP_START) then
      if (step_index == 0) then
        call add_thread(list, count, states(state_idx)%out1, start_pos, step_index, states, str_len, visited)
      end if
    else if (op == OP_END) then
      if (step_index == str_len) then
        call add_thread(list, count, states(state_idx)%out1, start_pos, step_index, states, str_len, visited)
      end if
    else
      count = count + 1
      list(count)%state = state_idx
      list(count)%start_pos = start_pos
    end if
  end subroutine add_thread

  subroutine regmatch(re, string, is_match, match_start, match_end)
    type(regex_type), intent(in) :: re
    character(len=*), intent(in) :: string
    logical, intent(out) :: is_match
    integer, intent(out), optional :: match_start
    integer, intent(out), optional :: match_end
    
    type(thread), allocatable :: clist(:), nlist(:)
    integer :: c_cnt, n_cnt, i, j, step_index, str_len
    integer :: c_code, op
    integer :: b_start, b_end
    logical :: match_char
    type(thread) :: t
    integer, allocatable :: visited(:)
    
    str_len = len(string)
    allocate(clist(re%n_states * 2))
    allocate(nlist(re%n_states * 2))
    allocate(visited(re%n_states))
    
    b_start = -1
    b_end = -1
    is_match = .false.
    
    ! Empty matches at the very beginning
    visited = -1
    c_cnt = 0
    call add_thread(clist, c_cnt, re%start_state, 1, 0, re%states, str_len, visited)
    
    do j = 1, c_cnt
      if (re%states(clist(j)%state)%op == OP_MATCH) then
        b_start = 1
        b_end = 0
      end if
    end do
    
    do i = 1, str_len
      step_index = i
      n_cnt = 0
      visited = -1
      
      ! Always see if a new match can start here if we don't have one yet
      if (b_start == -1) then
        call add_thread(nlist, n_cnt, re%start_state, i, step_index, re%states, str_len, visited)
      end if
      
      do j = 1, c_cnt
        t = clist(j)
        if (t%state == 0) cycle
        op = re%states(t%state)%op
        match_char = .false.
        
        if (op == OP_CHAR) then
          if (re%states(t%state)%c == string(i:i)) match_char = .true.
        else if (op == OP_ANY) then
          match_char = .true.
        else if (op == OP_CLASS) then
          c_code = iachar(string(i:i))
          if (c_code >= 0 .and. c_code <= 127) then
            if (re%states(t%state)%bmap(c_code)) match_char = .true.
          end if
          if (re%states(t%state)%invert) match_char = .not. match_char
        end if
        
        if (match_char) then
          call add_thread(nlist, n_cnt, re%states(t%state)%out1, t%start_pos, step_index, re%states, str_len, visited)
        end if
      end do
      
      do j = 1, n_cnt
         if (re%states(nlist(j)%state)%op == OP_MATCH) then
           if (b_start == -1 .or. nlist(j)%start_pos < b_start) then
              b_start = nlist(j)%start_pos
              b_end = i
           else if (nlist(j)%start_pos == b_start .and. i > b_end) then
              b_end = i
           end if
         end if
      end do
      
      clist = nlist
      c_cnt = n_cnt
    end do
    
    if (b_start /= -1) then
      is_match = .true.
      if (present(match_start)) match_start = b_start
      if (present(match_end)) match_end = b_end
    end if
  end subroutine regmatch

end module stdlib_regex
