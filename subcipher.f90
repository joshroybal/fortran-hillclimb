module subcipher
    implicit none
    contains
        ! map alphabetic characters to 1..26, otherwise to zero
        function charcode(c) result(n)
            ! dummy argument
            character, intent(in) :: c
            ! function result location
            integer :: n
            ! local data
            integer :: asc
            ! processing
            asc=iachar(c)
            if (asc .ge. 97 .and. asc .le. 122) then
                n=asc-96
            else if (asc .ge. 65 .and. asc .le. 90) then
                n=asc-64
            else
                n=0
            end if
        end function

        ! swap subroutine
        subroutine exchange(i, j)
            ! dummy arguments
            integer, intent(inout) :: i, j
            ! local data
            integer :: k
            ! processing
            k=i
            i=j
            j=k
        end subroutine

        ! shuffle subprogram
        subroutine shuffle(n, a)
            ! dummy arguments
            integer, intent(in) :: n
            integer, dimension(n), intent(inout) :: a
            ! local data
            integer :: i, idx
            real :: r
            ! processing
            do i=1,n-1
                call random_number(r)
                idx=i+int((n-i)*r)+1
                call exchange(a(i), a(idx))
            end do
        end subroutine

        ! output characters as text
        subroutine teleprinter(n, a)
            ! dummy arguments
            integer, intent(in) :: n
            integer, dimension(n), intent(in) :: a
            ! local data
            integer :: i
            ! processing
            print '(72a1)', (achar(64+a(i)),i=1,n)
            !print '(5(1x,5a1))', (achar(64+a(i)),i=1,n)
        end subroutine

        ! encrypt plaintext using substitution cipher
        function substitution_encrypt(n, plaintext, key) result(ciphertext)
            ! dummy arguments
            integer, intent(in) :: n
            integer, intent(in), dimension(n) :: plaintext
            integer, intent(in), dimension(26) :: key
            ! function result location
            integer, dimension(n) :: ciphertext
            ! local data
            integer :: i
            ! processing
            do i=1,n
                ciphertext(i) = key(plaintext(i))
            end do
        end function

        ! decrypt ciphertext using substitution cipher
        function substitution_decrypt(n, ciphertext, key) result(plaintext)
            ! dummy arguments
            integer, intent(in) :: n
            integer, intent(in), dimension(n) :: ciphertext
            integer, intent(in), dimension(26) :: key
            ! function result location
            integer, dimension(n) :: plaintext
            ! local data
            integer :: i
            integer, dimension(26) :: invkey
            ! processing
            do i = 1, 26
                invkey(key(i)) = i
            end do
            do i = 1, n
                plaintext(i) = invkey(ciphertext(i))
            end do
        end function
end module
