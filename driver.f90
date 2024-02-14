program driver
    use subcipher
    use hillclimb
    implicit none
    ! data
    integer, parameter :: SIZ = 10000
    integer :: i, k, n, ll, eof
    integer, dimension(C) :: key
    integer, dimension(SIZ) :: plaintext, ciphertext
    character (len=256) :: line
    ! processing
    n = 0
    open (7,file='plaintext',status='old',action='read')
    read (7,'(a)',iostat=eof) line
    do while (eof .eq. 0 .and. n .lt. SIZ)
        ll = len(trim(line))
        do i = 1, ll
            k = charcode(line(i:i))
            if (k .ne. 0) then
                n = n + 1 
                plaintext(n) = k
                if (n .eq. siz) exit
            end if
        end do
        read (7,'(a)',iostat=eof) line
    end do
    close (7)
    call teleprinter(min(n,360), plaintext)
    write (*,*) 'monographic index of coincidence =', mgrioc(n, plaintext)
    write (*,*) 'monographic correlation =', mgrcor(n, plaintext)
    write (*,*) 'digraphic index of coincidence =', dgrioc(n, plaintext)
    write (*,*) 'digraphic correlation =', dgrcor(n, plaintext)
    do i = 1, C
        key(i) = i
    end do
    call shuffle(C, key)
    write (*,'(a,26a1)') 'key = ', (char(64+key(i)),i=1,C)
    ciphertext = substitution_encrypt(n, plaintext, key)
    call teleprinter(min(n,360), ciphertext)
    write (*,*) 'monographic index of coincidence =', mgrioc(n, ciphertext)
    write (*,*) 'monographic correlation =', mgrcor(n, ciphertext)
    write (*,*) 'digraphic index of coincidence =', dgrioc(n, ciphertext)
    write (*,*) 'digraphic correlation =', dgrcor(n, ciphertext)
    call hills(n, ciphertext, plaintext)
    call teleprinter(min(n,360), plaintext)
    write (*,*) 'monographic index of coincidence =', mgrioc(n, plaintext)
    write (*,*) 'monographic correlation =', mgrcor(n, plaintext)
    write (*,*) 'digraphic index of coincidence =', dgrioc(n, plaintext)
    write (*,*) 'digraphic correlation =', dgrcor(n, plaintext)
    write (*,'(1x,a,1x,i0)') 'sample size =', n
end program
