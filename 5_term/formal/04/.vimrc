function! OutSyntax()
	syntax match Blank /_/
	syntax match Sep   /->/
	syntax match Move  /<\|>/
	syntax match Stay  /\^/
	syntax match Space / /
	syntax match Hash /[#$%][0-9]\+/

	highlight Blank ctermfg=darkyellow
	highlight Sep   ctermfg=magenta
	highlight Move  ctermfg=green
	highlight Stay  ctermfg=cyan
	highlight Space ctermfg=black
	highlight Hash  ctermfg=red

	set tabstop=8
	set shiftwidth=8

	set list
	set listchars=space:·,tab:\ \ 
	hi SpecialKey ctermfg=black
endfunction

autocmd BufRead,BufNewFile *.out,*.mt call OutSyntax()
