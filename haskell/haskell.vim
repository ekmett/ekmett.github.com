" Haskell Cuteness for Vim.
" Inspired by emacs-haskell-cuteness.
" Based on unilatex.vim by Jos van den Oever <oever@fenk.wau.nl>
" Version: 0.1
" Last Changed: 7 April 2009
" Maintainer: Andrey Popp <andrey.popp@braintrace.ru>
" Modified by Edward Kmett
" Save as ~/.vim/ftplugin/haskell/haskell.vim

imap <buffer> \ λ
imap <buffer> <- ←
imap <buffer> -> →
imap <buffer> <= ≲
imap <buffer> >= ≳
imap <buffer> == ≡
imap <buffer> /= ≠
imap <buffer> => ⇒
imap <buffer> >> »
imap <buffer> .<space> ∙<space>
imap <buffer> forall<space> ∀
imap <buffer> alpha α
imap <buffer> beta β
imap <buffer> gamma γ
imap <buffer> delta δ
imap <buffer> epsilon ε

" imap <buffer> && ∧
" imap <buffer> \|\| ∨
" imap <buffer> `union` ∩
" imap <buffer> `intersection` ∪
" imap <buffer> <*> ⊗


if exists("s:loaded_unihaskell")
    finish
endif
let s:loaded_unihaskell = 1

augroup HaskellC
    autocmd BufReadPost *.hs cal s:HaskellSrcToUTF8()
    autocmd BufWritePre *.hs cal s:UTF8ToHaskellSrc()
    autocmd BufWritePost *.hs cal s:HaskellSrcToUTF8()
augroup END

" function to convert ''fancy haskell source'' to haskell source
function s:UTF8ToHaskellSrc()
    let s:line = line(".")
    let s:column = col(".")

    silent %s/λ/\\/eg
    silent %s/←/<-/eg
    silent %s/→/->/eg
    silent %s/≲/<=/eg
    silent %s/≳/>=/eg
    silent %s/≡/==/eg
    silent %s/≠/\/=/eg
    silent %s/⇒/=>/eg
    silent %s/»/>>/eg
    silent %s/∙ /. /eg
    silent %s/∀/forall /eg
    silent %s/α/alpha/eg
    silent %s/β/beta/eg
    silent %s/γ/gamma/eg
    silent %s/δ/delta/eg
    silent %s/ε/epsilon/eg
    " silent %s/∧/&&/eg
    " silent %s/∨/\|\|/eg
    " silent %s/⊗/<*>/eg
    " silent %s/∩/`union`/eg
    " silent %s/∪/`intersection`/eg

    let &l:fileencoding = s:oldencoding
    call cursor(s:line,s:column)
endfunction

" function to convert haskell source to ''fancy haskell source''
function s:HaskellSrcToUTF8()
    let s:line = line(".")
    let s:column = col(".")

    let s:oldencoding = &l:fileencoding
    set fileencoding=utf-8

    silent %s/[^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>?@\^|~.]\@<=\\\([^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>\?@\^|~.]\)/λ\1/eg
    silent %s/[^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>?@\^|~.]\@<=->\([^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>\?@\^|~.]\)/→\1/eg
    silent %s/[^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>?@\^|~.]\@<=<-\([^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>\?@\^|~.]\)/←\1/eg
    silent %s/[^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>?@\^|~.]\@<=<=\([^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>\?@\^|~.]\)/≲\1/eg
    silent %s/[^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>?@\^|~.]\@<=>=\([^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>\?@\^|~.]\)/≳\1/eg
    silent %s/[^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>?@\^|~.]\@<===\([^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>\?@\^|~.]\)/≡\1/eg
    silent %s/[^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>?@\^|~.]\@<=\/=\([^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>\?@\^|~.]\)/≠\1/eg
    silent %s/[^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>?@\^|~.]\@<==>\([^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>\?@\^|~.]\)/⇒\1/eg
    silent %s/[^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>?@\^|~.]\@<=>>\([^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>\?@\^|~.]\)/»\1/eg
    silent %s/forall /∀/eg
    silent %s/ \@<=\. /∙ /eg
    silent %s/alpha/α/eg
    silent %s/beta/β/eg
    silent %s/gamma/γ/eg
    silent %s/delta/δ/eg
    silent %s/epsilon/ε/eg
    " silent %s/\V&&/∧/eg
    " silent %s/\V||/∨/eg
    " silent %s/<\*>/⊗/eg
    " silent %s/\`union\`/∩/eg
    " silent %s/\`intersection\`/∪/eg

    let &l:fileencoding = s:oldencoding
    call cursor(s:line,s:column)
endfunction

do HaskellC BufRead

" syn match hsVarSym /[λ←→≲≳≡≠⇒»∙∀]/
