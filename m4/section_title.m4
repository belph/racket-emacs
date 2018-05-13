dnl Small macro to print out a nice, pretty section title.

AC_DEFUN([COLOR_INIT],[
  AM_CONDITIONAL([COLOR_SUPPORTED], [test -t AS_MESSAGE_FD && test $(tput colors) -ge 8])
])

AC_DEFUN([SET_COLOR],[
  AM_COND_IF([COLOR_SUPPORTED],[
    m4_case([$1],
      [red],     [ echo "$(tput setaf 1)" >&AS_MESSAGE_FD ],
      [green],   [ echo "$(tput setaf 2)" >&AS_MESSAGE_FD ],
      [yellow],  [ echo "$(tput setaf 3)" >&AS_MESSAGE_FD ],
      [blue],    [ echo "$(tput setaf 4)" >&AS_MESSAGE_FD ],
      [magenta], [ echo "$(tput setaf 5)" >&AS_MESSAGE_FD ],
      [cyan],    [ echo "$(tput setaf 6)" >&AS_MESSAGE_FD ],
      [white],   [ echo "$(tput setaf 7)" >&AS_MESSAGE_FD ],
      [ echo "$(tput sgr0)" >&AS_MESSAGE_FD ]
    )
  ],[
    echo ""
  ])
])

AC_DEFUN([SECTION_TITLE],
[
  SET_COLOR([$1])
  echo '   $2   ' | sed -e's/./-/g' >&AS_MESSAGE_FD
  echo '   $2' >&AS_MESSAGE_FD
  echo -n '   $2   ' | sed -e's/./-/g' >&AS_MESSAGE_FD
  SET_COLOR()
])
