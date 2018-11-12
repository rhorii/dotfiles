# pwd
set fish_prompt_pwd_dir_length 0

# git
set __fish_git_prompt_showupstream 'informative'
set __fish_git_prompt_showcolorhints 1
set __fish_git_prompt_show_informative_status 1
set __fish_git_prompt_char_cleanstate ''
set __fish_git_prompt_char_dirtystate '!'
set __fish_git_prompt_char_invalidstate '#'
set __fish_git_prompt_char_stagedstate '+'
set __fish_git_prompt_char_stateseparator ' '
set __fish_git_prompt_char_untrackedfiles '?'
set __fish_git_prompt_char_upstream_ahead '>'
set __fish_git_prompt_char_upstream_behind '<'

function fish_prompt --description 'Write out the prompt'
    set -l last_status $status

    echo

    # user
    set_color $fish_color_user
    printf '%s' $USER

    # hostname
    set_color $fish_color_normal
    printf ' at '
    set_color $fish_color_host
    printf '%s' (prompt_hostname)

    # pwd
    set_color $fish_color_normal
    printf ' in '
    set_color $fish_color_cwd
    printf '%s' (prompt_pwd)

    # git
    set_color $fish_color_normal
    printf ' on %s' (__fish_git_prompt '%s')

    echo

    # symbol
    if test $last_status -eq 0
        set_color $fish_color_normal
    else
        set_color $fish_color_error
    end
    switch $USER
        case root toor
            printf '# '
        case '*'
            printf '$ '
    end

    set_color $fish_color_normal
end
