      *******************************************************************
      ** virtual printer subprogram
      *******************************************************************
       identification division.
      **************************************
       program-id. virtual-printer.
      **
       environment division.
      ***************************************
      **
       input-output section.
      **-*-*-*-*-*-*-*-*-*-*-*-*-*
       file-control.
           select fprinter assign to "./printer.dat"
           organization line sequential
       access sequential.
      **
       data division.
      **************************************
       file section.
      **-*-*-*-*-*-*-*-*-*-*-*-*-*
       fd fprinter.
       01 enreg-printer pic x(80).
      **
       working-storage section.
      **-*-*-*-*-*-*-*-*-*-*-*-*-*
       linkage section.
      **-*-*-*-*-*-*-*-*-*-*-*-*-*
       01 received-param.
           02 pa-reset         pic x       .
           02 pa-buffer        pic x(80)   .
           02 pa-when          pic x(6)    .
           02 pa-what          pic x(5)    .
           02 pa-howmany       pic 99      .
       procedure division using received-param.
      **************************************
       main-printer.
           if(pa-reset = "o")
               open output fprinter
           else
               open extend fprinter
               if(pa-when = "after")
                   if(pa-what = "page")
                       move '>------------------------------------------'
      -'------------------------------------<' to enreg-printer
                       write enreg-printer
                   else
                       subtract 1 from pa-howmany
                       perform pa-howmany times
                           move spaces to enreg-printer
                           write enreg-printer
                       end-perform
                    end-if
                end-if
                write enreg-printer from pa-buffer
                if(pa-when = "before")
                   if(pa-what = "page")
                       move '>------------------------------------------'
      -'------------------------------------<' to enreg-printer
                       write enreg-printer
                   else
                       subtract 1 from pa-howmany
                       perform pa-howmany times
                           move spaces to enreg-printer
                           write enreg-printer
                       end-perform
                   end-if
               end-if
           end-if
           close fprinter
           move "n"        to pa-reset
           move spaces     to pa-buffer
           move "after"    to pa-when
           move "lines"    to pa-what
           move 1          to pa-howmany
           exit program.
       end program virtual-printer.
