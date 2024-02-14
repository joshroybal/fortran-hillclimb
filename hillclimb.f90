module hillclimb
    use subcipher
    implicit none
    ! parameters
    integer, parameter :: C = 26
    ! from frequency table
    character (len=C), parameter :: ordstr = 'ETOANIRSHDLCFUMPYWGBVKXJQZ'
    contains
        ! function returns letter counts of text
        function ltrcnt(n, a) result(cnt)
            ! dummy arguments
            integer, intent(in) :: n
            integer, intent(in), dimension(n) :: a
            ! function result location
            integer, dimension(C) :: cnt
            ! local data
            integer :: i
            ! processing
            cnt = 0
            do i = 1, n
                cnt(a(i)) = cnt(a(i)) + 1
            end do
        end function

        ! function returns letter frequencies of text
        function ltrfrq(n, a) result(frq)
            ! dummy arguments
            integer, intent(in) :: n
            integer, intent(in), dimension(n) :: a
            ! function result location
            real, dimension(C) :: frq
            ! processing
            frq = ltrcnt(n, a) / real(n)
        end function

        ! function returns rank array of letters in text
        function ltrrnk(n, a) result(rnk)
            ! dummy arguments
            integer, intent(in) :: n
            integer, intent(in), dimension(n) :: a
            ! function result location
            integer, dimension(C) :: rnk
            ! local data
            integer :: i, j, maxidx
            integer, dimension(C) :: cnt
            ! processing
            cnt = ltrcnt(n, a)
            do i = 1 , C
                rnk(i) = i
            end do
            do i = 1, C - 1
                maxidx = i
                do j = i + 1, C
                    if (cnt(rnk(j)) .gt. cnt(rnk(maxidx))) maxidx = j
                end do
                call exchange(rnk(i), rnk(maxidx))
            end do
        end function

        ! monographic index of coincidence real location
        function mgrioc(n, a) result(ioc)
            ! dummy arguments
            integer, intent(in) :: n
            integer, intent(in), dimension(n) :: a
            ! function result location
            real :: ioc
            ! local data
            integer :: i
            real :: acc
            integer, dimension(C) :: x
            ! processing
            ! count letters
            x = ltrcnt(n, a)
            ! compute monographic index of coincidence
            acc = 0.0
            do i = 1, C
                acc = acc + real(x(i))*(x(i)-1.0)
            end do
            acc = acc / (real(n)*(n-1) / C)
            ioc = acc
        end function

        ! function returns english text correlation as real location
        function mgrcor(n, a) result(cor)
            ! dummy arguments
            integer, intent(in) :: n
            integer, intent(in), dimension(n) :: a
            ! function result location
            real :: cor
            ! local data
            integer :: i
            real :: acc
            real, dimension(C) :: frq, eng = (/.0781,.0128,.0293,.0411,&
            .1305,.0288,.0139,.0565,.0677,.0023,.0042,.0360,.0262,.0728,&
            .0821,.0215,.0014,.0664,.0646,.0902,.0277,.0100,.0149,.0030,&
            .0151,.0009/)
            ! processing
            frq = ltrfrq(n, a)
            acc = 0
            do i = 1, C
                acc = acc + frq(i) * eng(i)
            end do
            cor = acc
        end function

        ! load english digraph frequencies into two dimensional memory banks
        function dgreng() result(a)
            ! function result location
            real, dimension(C,C) :: a
            ! local data
            integer :: j
            ! processing
            open (7,file='digraph.dat',status='old',action='read')
            do j = 1, C
                read (7,*) a(:C,j)
            end do
            close (7)
        end function

        ! function returns matrix containing digram counts
        function dgrcnt(n, a) result(cnt)
            ! dummy arguments
            integer, intent(in) :: n
            integer, intent(in), dimension(n) :: a
            ! function result location
            integer, dimension(C,C) :: cnt
            ! local data
            integer :: i
            ! processing
            cnt = 0
            do i = 1, n - 1
                cnt(a(i),a(i+1)) = cnt(a(i),a(i+1)) + 1
            end do
        end function

        ! function returns matrix containing digram frequencies
        function dgrfrq(n, a) result(frq)
            ! dummy arguments
            integer, intent(in) :: n
            integer, intent(in), dimension(n) :: a
            ! result location
            real, dimension(C,C) :: frq
            ! processing
            frq = dgrcnt(n, a) / real(n - 1)
        end function

        ! function returns digraphical index of coincidence
        function dgrioc(n, a) result(ioc)
            ! dummy arguments
            integer, intent(in) :: n
            integer, intent(in), dimension(n) :: a
            ! result location
            real :: ioc
            ! local data
            integer :: i, j
            integer, dimension(C,C) :: cnt
            real :: acc
            ! processing
            cnt = dgrcnt(n, a)
            acc = 0.0
            do j = 1, C
                do i = 1, C
                    acc = acc + real(cnt(i,j) - 1) * (cnt(i,j) - 2)
                end do
            end do
            acc = acc / (real(n - 1) * (n - 2) / 676.0)
            ioc = acc
        end function

        ! function returns english digraphical correlation
        function dgrcor(n, a) result(cor)
            ! dummy arguments
            integer, intent(in) :: n
            integer, intent(in), dimension(n) :: a
            ! function result location
            real :: cor
            ! local data
            integer :: i, j
            real :: acc
            real, dimension(C,C) :: eng, frq
            ! processing
            eng = dgreng()
            frq = dgrfrq(n, a)
            acc = 0.0
            do j = 1, C
                do i = 1, C
                    acc = acc + real(eng(i,j)) * real(frq(i,j))
                end do
            end do
            cor = 676.0 * acc
        end function

        subroutine climb(n, a, str, hi, hikey)
            ! dummy arguments
            integer, intent(in) :: n
            integer, intent(in), dimension(n) :: a
            character (len=C), intent(in) :: str
            real, intent(out) :: hi
            integer, intent(out), dimension(C) :: hikey
            ! local data
            integer :: i, j
            real :: cor, prv
            integer, dimension(C) :: ord, rnk, key
            integer, dimension(n) :: aloc
            ! processing
            do i = 1, C
                ord(i) = iachar(str(i:i)) - 64
            end do
            rnk = ltrrnk(n, a)
            do i = 1, C
                key(ord(i)) = rnk(i)
            end do

            aloc = substitution_decrypt(n, a, key)
            hi = dgrcor(n, aloc)
            hikey = key
            prv = 0

            do while (abs(hi-prv) .gt. .001)
                prv = hi
                do i = 1, C - 1
                    do j = i + 1, C
                        call exchange(key(ord(i)), key(ord(j)))
                        aloc = substitution_decrypt(n, a, key)
                        cor = dgrcor(n, aloc)
                        if (cor .gt. hi) then
                            hi = cor
                            hikey = key
                        else
                            key = hikey
                        end if
                    end do
                    key = hikey
                end do
            end do
        end subroutine

        ! climb various hills
        subroutine hills(n, ciphertext, plaintext)
            ! dummy arguments
            integer, intent(in) :: n
            integer, intent(in), dimension(n) :: ciphertext
            integer, intent(out), dimension(n) :: plaintext
            ! local data
            integer :: i, maxidx
            real :: hi
            real, dimension(3) :: cor
            integer, dimension(C,3) :: key
            ! processing
            call climb(n, ciphertext, 'ETOANIRSHDLCFUMPYWGBVKXJQZ', cor(1), key(:C,1))
            call climb(n, ciphertext, 'ETAONISRHLDCUPFMWYBGVKQXJZ', cor(2), key(:C,2))
            call climb(n, ciphertext, 'ETAOINSHRDLUCWMFYGPBVKXJQZ', cor(3), key(:C,3))
            hi = 0
            do i = 1, 3
                if (cor(i) .gt. hi) then
                    hi = cor(i)
                    maxidx = i
                end if
            end do
            plaintext = substitution_decrypt(n, ciphertext, key(:C,maxidx))
            write (*,'(a,26a1)') 'key = ', (char(64+key(:C,maxidx)))
        end subroutine
end module
