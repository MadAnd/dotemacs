#!/usr/bin/env bash

# Install Spacemacs configuration into separate (from .emacs.d) directory

# This is free and unencumbered software released into the public domain.
#
# Anyone is free to copy, modify, publish, use, compile, sell, or
# distribute this software, either in source code form or as a compiled
# binary, for any purpose, commercial or non-commercial, and by any
# means.
#
# In jurisdictions that recognize copyright laws, the author or authors
# of this software dedicate any and all copyright interest in the
# software to the public domain. We make this dedication for the benefit
# of the public at large and to the detriment of our heirs and
# successors. We intend this dedication to be an overt act of
# relinquishment in perpetuity of all present and future rights to this
# software under copyright law.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
# OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
# ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.
#
# For more information, please refer to <http://unlicense.org>

set -e

# Configuration variables:
DOTSPACEMACS=~/.spacemacs
GIT_REPO="https://github.com/MadAnd/dotemacs.git"
INSTALL_DIR=~/.emacs
SPACEMACS_REPO="https://github.com/MadAnd/spacemacs.git"
SPACEMACS_DIR=~/.emacs.d

########################################################################

# USAGE
#   find_executable executable-name
# DESCRIPTION
#   Find an executable, given as a first argument, in PATH
#   and assign the found path to a global variable.
# EXAMPLES
#  `find_executable git` - will set the variable GIT_CMD to e.g. '/usr/bin/git'
#  `find_executable bash` - will set the variable BASH_CMD, etc.
find_executable()
{
    if (( $# != 1 )); then
        echo 'USAGE: find_executable executable-name' >&2
        return 1
    fi

    local cmd_executable=$(which $1 2>/dev/null)

    if [ -z "$cmd_executable" ]; then
        echo "$1 not found in PATH" >&2
        return 2
    fi

    local cmd_var_name=${1^^}_CMD
    declare -g $cmd_var_name="$cmd_executable"
}

# USAGE
#   backup_place path
# DESCRIPTION
#   Rename the target file or diretory by adding the ".bak" suffix
#   or delete it, if it is a symlink.
# EXAMPLES
#   `backup_place ~/.config` - will rename it to `~/.config.bak`
backup_place()
{
    if (( $# != 1 )); then
        echo 'USAGE: backup_place path' >&2
        return 1
    fi

    if [[ -h "$1" ]]; then
        # if symlink - just remove it
        rm "$1"
    elif [[ -e "$1" ]]; then
        # if file or dir - move to the same path with ".bak" suffix
        mv -f "$1" "$1".bak
    fi
}

# Generate GIT_CMD
find_executable git

# Backup things we are going to replace
backup_place "$DOTSPACEMACS" 
backup_place "$INSTALL_DIR" 
backup_place "$SPACEMACS_DIR" 

# Install the config
$GIT_CMD clone "$GIT_REPO" "$INSTALL_DIR"
ln -sf "$INSTALL_DIR/dotspacemacs" "$DOTSPACEMACS"

# Install spacemacs
$GIT_CMD clone --recursive --branch develop "$SPACEMACS_REPO" "$SPACEMACS_DIR"

exit 0
