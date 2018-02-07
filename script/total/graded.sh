#!/bin/bash

# This script gathers graded student submission and produces both a
# CSV file (for upload to grades.cs.umd.edu) and directory of
# distribution-ready copies of students' graded files.

usage() {
    cat <<EOF
Usage: $0 [directory with student submissions]
The assignment-specific variables ASSIGN, SUBPARTS, SUBTOTALS must be
configured to proper values inside this script.
EOF
}

###
# Input verification

# Exit if not given a submissions directory as input.
if [[ $# -ne 1 || ! -d $1 ]]
then >&2 echo "You must provide the graded submissions directory."
     usage
     exit 1
else # $1 is a directory, which should contain all graded student
     # submissions (directories of the form 'studentid__N').
     for d in $1/*
     do if grep -Ev "([a-Z]|[0-9])+__[0-9]+" &>/dev/null <<<"$(basename "$d")"
        then >&2 echo "Non-submission found in given directory: '$d'"
             usage
             exit 2
        fi
     done
     SUBS="$1"
fi

# The following variables define which files are graded in each
# student submission:
#
# ASSIGN    is the grades.cs.umd.edu 'short assignment' name;
# SUBPARTS  is an array of literal file names or regexes for
#           each graded file;
# SUBTOTALS are the total amount of points for each corresponding SUBPART.
#
# If some files points are worth less than others, use the DIVISORS
# array to divide each resp. subparts graded points by some
# integer. An empty DIVISORS array is the same as all 1s.  See
# ASSIGN=A4 for an example use of DIVISORS.

ASSIGN=
SUBPARTS=()
SUBTOTALS=()
DIVISORS=()

# ASSIGN='A4'
# SUBPARTS=('(calendar|a[ssign]*3).*.rkt' '(editor|83).rkt' '(index|87).*.rkt' 'chips.rkt')
# SUBTOTALS=(100 110 110 100)
# DIVISORS=(2 2 2 1)

# ASSIGN='A5'
# SUBPARTS=('.*nvad.*\.rkt' '.*ist.*\.rkt')
# SUBTOTALS=(100 94)

# ASSIGN='A6'
# SUBPARTS=('.*nvad.*\.rkt' '.*ist.*\.rkt')
# SUBTOTALS=(140 60)

ASSIGN='A7'
SUBPARTS=('^abs\.rkt$' 'invader.*\.rkt')
SUBTOTALS=(55 180)

# ASSIGN='A8'
# SUBPARTS=('ft.rkt' 'ml.rkt')
# SUBTOTALS=(80 140)

# ASSIGN='A9'
# SUBPARTS=('^.*.rkt$')
# SUBTOTALS=(30)

# ASSIGN='A10'
# SUBPARTS=('tml.rkt')
# SUBTOTALS=(100)

# ASSIGN='A11'
# SUBPARTS=('.*.rkt')
# SUBTOTALS=(45)

# Calculate totals for sanity checks later.
NPARTS=${#SUBPARTS[@]}
TOTAL=0
for ((i=0;i<$NPARTS;i++))
do divisor=${DIVISORS[$i]}
   TOTAL=$(($TOTAL + (${SUBTOTALS[$i]} / ${divisor:-1})))
done
if [[ $NPARTS -eq 0 || $TOTAL -eq 0 ]]
then >&2 echo "No assignment subparts configured!"
     >&2 echo "You need to edit the required variables in the script."
     usage
     exit 1
fi

#
###

###
# Output configuration

# The current term (used in URL generation)
CURRTERM=fall2017
# The class name (used in URL generation)
CLASSNM=cmsc131A
URLBASE="https://cs.umd.edu/class/${CURRTERM}/${CLASSNM}"

# The generated CSV for upload to grades.cs.umd.edu.
CSVFILE="${ASSIGN,,}-grades.csv"

# The generated directory of distribution-ready student files (with
# generated names). This directory shares the assignment short name,
# and must be uploaded to the following www-public class directory
# (with privileges 0771, so students can't list the directory).
DISTRDIR="${ASSIGN}"

#
###

###
# Utilities

# For more information at runtime, set positive VERBOSITY.
VERBOSITY=0
function errcho {
    if [[ VERBOSITY -gt 0 ]]
    then >&2 echo $*
    fi
}

# text_of_wxme : [racket-file-path] -> [racket-file-path]
# If the given racket file is in the GRacket editor format, create
# a file of the form "$(basename "$1")${NO_WXME_SUFFIX}.rkt" with images,
# comment boxes, etc. removed. Returns either the converted file's 
# path or the given path if no need for conversion.
# OUTPUT: The name of the non-GRacket formatted file via STDOUT.
NO_WXME_SUFFIX='-no-wxme'
function text_of_wxme {
    if [[ -s $1 && $(head -n 1 "$1") =~ .*wxme.* ]]
    then local textf="$(sed "s/.rkt/${NO_WXME_SUFFIX}.rkt/" <<<"$1")"
         errcho "WXME file: $1"
         errcho "$(head -n 1 "$1")"
         racket -e "(require wxme)
(call-with-input-file \"$1\"
  (λ (inp) 
    (call-with-output-file \"$textf\"
      (λ (outp)
        (when (is-wxme-stream? inp)
          (copy-port (wxme-port->text-port inp) outp))) 
      #:exists 'truncate)))"
         echo "$textf"
    else echo "$1"
    fi
}

#
###

###
# `grep'ing through graded files

# graded_comments : [path] -> [grader-comments]
# Get all grader comments from the given file.
# Output: all grader comments via STDOUT
function graded_comments {
    grep -o ";;>.*" "$1"
}

# graded_grade : [path] -> [grade]
# What is the assigned grade in the given file?
# Output: the assigned grade via STDOUT, if found, otherwise nothing.
function graded_grade {
    grep -oP ';;> \K[0-9]+(?=/[0-9]+.*)' "$1"
}

# graded_students : [path] -> [student-ids]
# Who are the listed student authors of the given file?
# Output: the student names via STDOUT, if found, otherwise nothing.
function graded_students {
    graded_comments "$1" | head -n 1 | \
        grep -oP ';;> \K([[:alnum:]]| )+(?=[Nn]/[Aa])*'
}

# graded_hash : [path] -> [hash-of-grader-comments]
# Create a hash from the grader comments of the given file.
# Assumes that grader comments are unique. This is verified
# during `graded_hash_file' generation.
# Output: the hash via STDOUT.
function graded_hash {
    graded_comments "$1" | md5sum | awk '{ print $1 }'
}

#
###

###
# Core functionality

# graded_hash_file : [path] -> [hash-file-path]
# Create a distribution-ready file with a generated name. If any
# duplicate file names exist in the distribution directory, print
# duplicates and exit (can only be caused by duplicate grader
# comments).
# Output: the distribution-ready file path
function graded_hash_file {
    local ghash="$(graded_hash "$1")"
    [[ ! -d $DISTRDIR ]] && mkdir "${DISTRDIR}"
    local hashf="${DISTRDIR}/${ghash}.rkt"
    if [[ -f $hashf ]]
    then if [[ "$(cat "$1" | md5sum)" == "$(cat "$hashf" | md5sum)"  ]]
         then echo "$hashf"
         else errcho "Duplicate grader comments found (for $1 and $hashf)"
              errcho $(graded_comments "$1")
              exit 2
         fi
    else cp "$1" "$hashf" && echo "$hashf"
    fi
}

# interactive_grade : [file] [max-grade] -> [grade]
# Interactively searches for a grade in the given file $1, with a
# maximum grade of $2.
# Output: The found or entered grade, via STDOUT.
function interactive_grade {
    errcho "  Looking for grade in '$1'"
    local grade=$(graded_grade "$1")
    [[ -n $grade ]] && errcho "  found grade: $grade"
    case $grade in
        ''|*[!0-9]*) # Grade is not a valid number!
            local resp
            # Offer to open file in DrRacket to manually look for the grade.
            read -e -p "
  Error: No grade found in $1, look for it manually [Y|n]? " resp
            case "$resp" in
                n|N) errcho "Not looking!" ;;
                *) errcho "Looking!" ; drracket "$1" ;;
            esac
            pushd $(dirname "$1") &>/dev/null
            >&2 echo -n "  Available files: "
            ls >&2
            read -e -p "  Manually enter grade [0/$2] or new file name: " resp
            popd &>/dev/null
            # If a file is given, attempt to grade that file.
            # If a numeric grade is manually given, return that.
            case "$resp" in
                ''|*[!0-9]*) [[ -f $resp ]] && interactive_grade "$resp" $2 || echo 0 ;;
                *) echo "$resp" ;;
            esac ;;
        *) echo "$grade" ;;
    esac
}

# auto_file : [submission-dir] [regex-pattern] -> [path]
# Attempts to automatically choose the correct file for the current
# sub-part inside the given directory $1 based on the given regex
# pattern $2. If there are zero or too many matching files, outputs
# nothing.
# Output: If 1 matching file, the relative file path on STDOUT.
function auto_file {
    pushd "$1" &>/dev/null
    local auto_files=()
    # This search will have to be deeper for java source directories.
    for f in *.rkt
    do if [[ ${f,,} == ${2,,} || ${f,,} =~ ${2,,} ]]
       then auto_files+=("$f")
       fi
    done
    if [[ 0 -eq ${#auto_files[@]} ]]  # 0 matches
    then errcho "  No matching ($2) files in $1"
    elif [[ 1 -eq ${#auto_files[@]} && -f ${auto_files[0]} ]]  # 1 match
    then errcho "  Matching ($2) file: '${auto_files[0]}'"
         echo "${auto_files[0]}"
    else [[ 1 -lt ${#auto_files[@]} ]] # 2+ matches
         errcho "  Too many matching files: '${auto_files[@]}'"
    fi
    popd &>/dev/null
}

# interactive_file : [submission-dir] [regex-pattern] -> [path]
# Interactively choose a file for the current subpart in the given
# submission directory $1 that should have matched the given regex
# pattern $2 (but didn't).
# Output: The relative file path on STDOUT.
function interactive_file {
    local resp
    pushd "$1" &>/dev/null
    >&2 echo -n "
  Available files: "
    ls >&2
    read -e -p "  Enter file for $2 [skip if none]: " resp
    if [[ -z $resp ]]
    then errcho "  Missing ${part_name} for submission $1"
    else if [[ -f "$resp" ]]
         then echo "$resp"
         else errcho "  Not a ${part_name} file: $resp"
         fi
    fi
    popd &>/dev/null
}

# grade_parts : [submission-dir] -> [nothing]
# Grade each file of a student submission given a relative path to the
# submission's directory. Produces no output to be parsed, only
# logging and information messages.
function grade_parts {
    local students=()
    local urls=()
    local sub="$1"
    local grade=0
    for ((i=0;i<$NPARTS;i++))
    {
        local part_pattern="${SUBPARTS[$i]}"
        local sub_total="${SUBTOTALS[$i]}"
        local divisor="${DIVISORS[$i]}"
        # Select the proper file for this part of the assignment
        local file_name="$(auto_file "$sub" "$part_pattern")"
        if [[ ! -f "${sub}/${file_name}" ]]
        then file_name="$(interactive_file "${sub}" "${part_pattern}")"
        fi
        local file_path="${sub}/${file_name}"
        if [[ -f $file_path ]]
        then echo -n "."
             # Handle DrRacket wxme encoded files
             local txt_path="$(text_of_wxme "$file_path")"
             # Get the grade
             local subgrade=$(if [[ -z $txt_path ]]
                              then echo 0
                              else interactive_grade "$txt_path" $sub_total
                              fi)
             echo -n "."
             # look for student user ids for this submission
             if [[ 0 -eq ${#students[@]} ]]
             then students=($(graded_students "$txt_path"))
             fi
             echo -n "."
             # create hash file and URL
             local hashf="$(graded_hash_file "$txt_path")"
             echo -n "."
             urls+=("${URLBASE}/${hashf}")
             grade="$((($subgrade / ${divisor:-1}) + $grade))"
        else errcho "  No valid file found for $part_pattern in $sub"
        fi
    }

    # last chance if no student ids found
    if [[ 0 -eq ${#students[@]} ]]
    then errcho "  No students found in '$sub', handle manually"
    fi

    echo -n "."

    # assigned grade sanity checks: 0 < $grade <= $TOTAL
    if [[ 0 -eq $grade ]]
    then errcho "  Actually a 0 for $sub (students: ${students[@]})...?"
    elif [[ $TOTAL -lt $grade ]]
    then errcho "  Grade $grade is greater than maximum $TOTAL for $sub (students: ${students[@]})..."
    fi

    # write CSV lines for each student
    for student in "${students[@]}"
    do echo "${student},${ASSIGN},${grade},${urls[@]}" >> "${CSVFILE}"
    done
    echo "  wrote ${#students[@]} grade(s) for ${students[@]}"
}

#
###


###
# Main

# Delete any existing grades CSV, hashed files, or textified temporaries.
rm -rf "${CSVFILE}" "${DISTRDIR}" ${SUBS}/*/*${NO_WXME_SUFFIX}.rkt

# Cue up all student submissions to total.
set -- ${SUBS}/*
NSUBS=$#

# Grade each submission.
while [[ $# -gt 0 ]]
do printf "[%02d/%02d] %s" $(($NSUBS - $# + 1)) $NSUBS "$1"
   grade_parts "$1"
   shift
done
