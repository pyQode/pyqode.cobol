*>******************************************************************************
*>  This file is part of cobjapi.
*>
*>  cobjapi.cob is free software: you can redistribute it and/or
*>  modify it under the terms of the GNU Lesser General Public License as
*>  published by the Free Software Foundation, either version 3 of the License,
*>  or (at your option) any later version.
*>
*>  cobjapi.cob is distributed in the hope that it will be useful,
*>  but WITHOUT ANY WARRANTY; without even the implied warranty of
*>  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*>  See the GNU Lesser General Public License for more details.
*>
*>  You should have received a copy of the GNU Lesser General Public License
*>  along with cobjapi.cob.
*>  If not, see <http://www.gnu.org/licenses/>.
*>******************************************************************************

*>******************************************************************************
*> Program:      cobjapi.cob
*>
*> Purpose:      GnuCOBOL wrapper functions for JAPI
*>
*> Author:       Laszlo Erdos - https://www.facebook.com/wortfee
*>
*> Date-Written: 2014.12.24
*>
*> Tectonics:    cobc -c -free cobjapi.cob
*>
*> Usage:        Use these functions in GnuCOBOL GUI programs.
*>
*>******************************************************************************
*> Date       Name / Change description
*> ========== ==================================================================
*> 2003.02.26 This comment is only for History. The latest Version (V1.0.6) of
*>            JAPI was released on 02/26/2003. Homepage: http://www.japi.de
*>------------------------------------------------------------------------------
*> 2014.12.24 Laszlo Erdos:
*>            - GnuCOBOL support for JAPI added.
*>            - japi4c.c converted into cobjapi.cob.
*>******************************************************************************

*>------------------------------------------------------------------------------
*> int    j_start( )
*> { return( japi_start());  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-START.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION
    RETURNING          LNK-RET.
 MAIN-J-START SECTION.

    CALL STATIC "japi_start"
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-START-EX.
    EXIT.
 END FUNCTION J-START.


*>------------------------------------------------------------------------------
*> int    j_connect( char* arg0)
*> { return( japi_connect(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-CONNECT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY REFERENCE LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-CONNECT SECTION.

    CALL STATIC "japi_connect"
         USING BY CONTENT CONCATENATE(TRIM(LNK-ARG-0), X"00")
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-CONNECT-EX.
    EXIT.
 END FUNCTION J-CONNECT.


*>------------------------------------------------------------------------------
*> void j_setport( int arg0)
*> { japi_setport(arg0);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETPORT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-SETPORT SECTION.

    CALL STATIC "japi_setport"
         USING BY VALUE LNK-ARG-0
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETPORT-EX.
    EXIT.
 END FUNCTION J-SETPORT.


*>------------------------------------------------------------------------------
*> void j_setdebug( int arg0)
*> { japi_setdebug(arg0);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETDEBUG.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-SETDEBUG SECTION.

    CALL STATIC "japi_setdebug"
         USING BY VALUE LNK-ARG-0
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETDEBUG-EX.
    EXIT.
 END FUNCTION J-SETDEBUG.


*>------------------------------------------------------------------------------
*> int  j_frame( char* arg0)
*> { return( japi_frame(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-FRAME.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY REFERENCE LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-FRAME SECTION.

    CALL STATIC "japi_frame"
         USING BY CONTENT CONCATENATE(TRIM(LNK-ARG-0), X"00")
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-FRAME-EX.
    EXIT.
 END FUNCTION J-FRAME.


*>------------------------------------------------------------------------------
*> int  j_button( int arg0, char* arg1)
*> { return( japi_button(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-BUTTON.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-BUTTON SECTION.

    CALL STATIC "japi_button"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-BUTTON-EX.
    EXIT.
 END FUNCTION J-BUTTON.


*>------------------------------------------------------------------------------
*> int  j_graphicbutton( int arg0, char* arg1)
*> { return( japi_graphicbutton(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GRAPHICBUTTON.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-GRAPHICBUTTON SECTION.

    CALL STATIC "japi_graphicbutton"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GRAPHICBUTTON-EX.
    EXIT.
 END FUNCTION J-GRAPHICBUTTON.


*>------------------------------------------------------------------------------
*> int  j_checkbox( int arg0, char* arg1)
*> { return( japi_checkbox(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-CHECKBOX.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-CHECKBOX SECTION.

    CALL STATIC "japi_checkbox"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-CHECKBOX-EX.
    EXIT.
 END FUNCTION J-CHECKBOX.


*>------------------------------------------------------------------------------
*> int  j_label( int arg0, char* arg1)
*> { return( japi_label(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-LABEL.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-LABEL SECTION.

    CALL STATIC "japi_label"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-LABEL-EX.
    EXIT.
 END FUNCTION J-LABEL.


*>------------------------------------------------------------------------------
*> int  j_graphiclabel( int arg0, char* arg1)
*> { return( japi_graphiclabel(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GRAPHICLABEL.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-GRAPHICLABEL SECTION.

    CALL STATIC "japi_graphiclabel"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GRAPHICLABEL-EX.
    EXIT.
 END FUNCTION J-GRAPHICLABEL.


*>------------------------------------------------------------------------------
*> int  j_canvas( int arg0, int arg1, int arg2)
*> { return( japi_canvas(arg0, arg1, arg2));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-CANVAS.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                    RETURNING          LNK-RET.

 MAIN-J-CANVAS SECTION.

    CALL STATIC "japi_canvas"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-CANVAS-EX.
    EXIT.
 END FUNCTION J-CANVAS.


*>------------------------------------------------------------------------------
*> int  j_panel( int arg0)
*> { return( japi_panel(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-PANEL.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-PANEL SECTION.

    CALL STATIC "japi_panel"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-PANEL-EX.
    EXIT.
 END FUNCTION J-PANEL.


*>------------------------------------------------------------------------------
*> int  j_borderpanel( int arg0, int arg1)
*> { return( japi_borderpanel(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-BORDERPANEL.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-BORDERPANEL SECTION.

    CALL STATIC "japi_borderpanel"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-BORDERPANEL-EX.
    EXIT.
 END FUNCTION J-BORDERPANEL.


*>------------------------------------------------------------------------------
*> int  j_radiogroup( int arg0)
*> { return( japi_radiogroup(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-RADIOGROUP.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-RADIOGROUP SECTION.

    CALL STATIC "japi_radiogroup"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-RADIOGROUP-EX.
    EXIT.
 END FUNCTION J-RADIOGROUP.


*>------------------------------------------------------------------------------
*> int  j_radiobutton( int arg0, char* arg1)
*> { return( japi_radiobutton(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-RADIOBUTTON.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-RADIOBUTTON SECTION.

    CALL STATIC "japi_radiobutton"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-RADIOBUTTON-EX.
    EXIT.
 END FUNCTION J-RADIOBUTTON.


*>------------------------------------------------------------------------------
*> int  j_list( int arg0, int arg1)
*> { return( japi_list(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-LIST.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-LIST SECTION.

    CALL STATIC "japi_list"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-LIST-EX.
    EXIT.
 END FUNCTION J-LIST.


*>------------------------------------------------------------------------------
*> int  j_choice( int arg0)
*> { return( japi_choice(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-CHOICE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-CHOICE SECTION.

    CALL STATIC "japi_choice"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-CHOICE-EX.
    EXIT.
 END FUNCTION J-CHOICE.


*>------------------------------------------------------------------------------
*> int  j_dialog( int arg0, char* arg1)
*> { return( japi_dialog(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-DIALOG.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-DIALOG SECTION.

    CALL STATIC "japi_dialog"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-DIALOG-EX.
    EXIT.
 END FUNCTION J-DIALOG.


*>------------------------------------------------------------------------------
*> int  j_window( int arg0)
*> { return( japi_window(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-WINDOW.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-WINDOW SECTION.

    CALL STATIC "japi_window"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-WINDOW-EX.
    EXIT.
 END FUNCTION J-WINDOW.


*>------------------------------------------------------------------------------
*> int  j_popupmenu( int arg0, char* arg1)
*> { return( japi_popupmenu(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-POPUPMENU.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-POPUPMENU SECTION.

    CALL STATIC "japi_popupmenu"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-POPUPMENU-EX.
    EXIT.
 END FUNCTION J-POPUPMENU.


*>------------------------------------------------------------------------------
*> int  j_scrollpane( int arg0)
*> { return( japi_scrollpane(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SCROLLPANE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-SCROLLPANE SECTION.

    CALL STATIC "japi_scrollpane"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-SCROLLPANE-EX.
    EXIT.
 END FUNCTION J-SCROLLPANE.


*>------------------------------------------------------------------------------
*> int  j_hscrollbar( int arg0)
*> { return( japi_hscrollbar(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-HSCROLLBAR.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-HSCROLLBAR SECTION.

    CALL STATIC "japi_hscrollbar"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-HSCROLLBAR-EX.
    EXIT.
 END FUNCTION J-HSCROLLBAR.


*>------------------------------------------------------------------------------
*> int  j_vscrollbar( int arg0)
*> { return( japi_vscrollbar(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-VSCROLLBAR.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-VSCROLLBAR SECTION.

    CALL STATIC "japi_vscrollbar"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-VSCROLLBAR-EX.
    EXIT.
 END FUNCTION J-VSCROLLBAR.


*>------------------------------------------------------------------------------
*> int  j_line( int arg0, int arg1, int arg2, int arg3)
*> { return( japi_line(arg0, arg1, arg2, arg3));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-LINE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
    01 LNK-ARG-0                          BINARY-INT.
    01 LNK-ARG-1                          BINARY-INT.
    01 LNK-ARG-2                          BINARY-INT.
    01 LNK-ARG-3                          BINARY-INT.
        05 Truc.
    01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                    RETURNING          LNK-RET.

 MAIN-J-LINE SECTION.

    CALL STATIC "japi_line"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
               BY VALUE LNK-ARG-3
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-LINE-EX.
    EXIT.
 END FUNCTION J-LINE.


*>------------------------------------------------------------------------------
*> int  j_printer( int arg0)
*> { return( japi_printer(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-PRINTER.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-PRINTER SECTION.

    CALL STATIC "japi_printer"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-PRINTER-EX.
    EXIT.
 END FUNCTION J-PRINTER.


*>------------------------------------------------------------------------------
*> int  j_image( int arg0, int arg1)
*> { return( japi_image(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-IMAGE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-IMAGE SECTION.

    CALL STATIC "japi_image"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-IMAGE-EX.
    EXIT.
 END FUNCTION J-IMAGE.


*>------------------------------------------------------------------------------
*> char*  j_filedialog( int arg0, char* arg1, char* arg2, char* arg3)
*> { return( japi_filedialog(arg0, arg1, arg2, arg3));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-FILEDIALOG.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 WS-IND                             BINARY-INT.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-ARG-2                          PIC X ANY LENGTH.
 01 LNK-ARG-3                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                          BY REFERENCE LNK-ARG-2
                          BY REFERENCE LNK-ARG-3
                    RETURNING          LNK-RET.

 MAIN-J-FILEDIALOG SECTION.

*>  init output text field
    MOVE ALL X"00" TO LNK-ARG-3

    CALL STATIC "japi_filedialog"
         USING BY VALUE     LNK-ARG-0
               BY CONTENT   CONCATENATE(TRIM(LNK-ARG-1), X"00")
               BY CONTENT   CONCATENATE(TRIM(LNK-ARG-2), X"00")
               BY REFERENCE LNK-ARG-3
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

*>  replace all chars with spaces after EOL
    PERFORM VARYING WS-IND FROM 1 BY 1
      UNTIL WS-IND > LENGTH(LNK-ARG-3)
       IF LNK-ARG-3(WS-IND:1) = X"00"
       THEN
          MOVE SPACES TO LNK-ARG-3(WS-IND:)
          EXIT PERFORM
       END-IF
    END-PERFORM

    GOBACK

    .
 MAIN-J-FILEDIALOG-EX.
    EXIT.
 END FUNCTION J-FILEDIALOG.


*>------------------------------------------------------------------------------
*> char*  j_fileselect( int arg0, char* arg1, char* arg2, char* arg3)
*> { return( japi_fileselect(arg0, arg1, arg2, arg3));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-FILESELECT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 WS-IND                             BINARY-INT.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-ARG-2                          PIC X ANY LENGTH.
 01 LNK-ARG-3                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                          BY REFERENCE LNK-ARG-2
                          BY REFERENCE LNK-ARG-3
                    RETURNING          LNK-RET.

 MAIN-J-FILESELECT SECTION.

*>  init output text field
    MOVE ALL X"00" TO LNK-ARG-3

    CALL STATIC "japi_fileselect"
         USING BY VALUE     LNK-ARG-0
               BY CONTENT   CONCATENATE(TRIM(LNK-ARG-1), X"00")
               BY CONTENT   CONCATENATE(TRIM(LNK-ARG-2), X"00")
               BY REFERENCE LNK-ARG-3
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

*>  replace all chars with spaces after EOL
    PERFORM VARYING WS-IND FROM 1 BY 1
      UNTIL WS-IND > LENGTH(LNK-ARG-3)
       IF LNK-ARG-3(WS-IND:1) = X"00"
       THEN
          MOVE SPACES TO LNK-ARG-3(WS-IND:)
          EXIT PERFORM
       END-IF
    END-PERFORM

    GOBACK

    .
 MAIN-J-FILESELECT-EX.
    EXIT.
 END FUNCTION J-FILESELECT.


*>------------------------------------------------------------------------------
*> int  j_messagebox( int arg0, char* arg1, char* arg2)
*> { return( japi_messagebox(arg0, arg1, arg2));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-MESSAGEBOX.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-ARG-2                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                          BY REFERENCE LNK-ARG-2
                    RETURNING          LNK-RET.

 MAIN-J-MESSAGEBOX SECTION.

    CALL STATIC "japi_messagebox"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-2), X"00")
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-MESSAGEBOX-EX.
    EXIT.
 END FUNCTION J-MESSAGEBOX.


*>------------------------------------------------------------------------------
*> int  j_alertbox( int arg0, char* arg1, char* arg2, char* arg3)
*> { return( japi_alertbox(arg0, arg1, arg2, arg3));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-ALERTBOX.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-ARG-2                          PIC X ANY LENGTH.
 01 LNK-ARG-3                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                          BY REFERENCE LNK-ARG-2
                          BY REFERENCE LNK-ARG-3
                    RETURNING          LNK-RET.

 MAIN-J-ALERTBOX SECTION.

    CALL STATIC "japi_alertbox"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-2), X"00")
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-3), X"00")
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-ALERTBOX-EX.
    EXIT.
 END FUNCTION J-ALERTBOX.


*>------------------------------------------------------------------------------
*> int  j_choicebox2( int arg0, char* arg1, char* arg2, char* arg3, char* arg4)
*> { return( japi_choicebox2(arg0, arg1, arg2, arg3, arg4));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-CHOICEBOX2.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-ARG-2                          PIC X ANY LENGTH.
 01 LNK-ARG-3                          PIC X ANY LENGTH.
 01 LNK-ARG-4                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                          BY REFERENCE LNK-ARG-2
                          BY REFERENCE LNK-ARG-3
                          BY REFERENCE LNK-ARG-4
                    RETURNING          LNK-RET.

 MAIN-J-CHOICEBOX2 SECTION.

    CALL STATIC "japi_choicebox2"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-2), X"00")
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-3), X"00")
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-4), X"00")
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-CHOICEBOX2-EX.
    EXIT.
 END FUNCTION J-CHOICEBOX2.


*>------------------------------------------------------------------------------
*> int  j_choicebox3( int arg0, char* arg1, char* arg2, char* arg3, char* arg4, char* arg5)
*> { return( japi_choicebox3(arg0, arg1, arg2, arg3, arg4, arg5));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-CHOICEBOX3.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-ARG-2                          PIC X ANY LENGTH.
 01 LNK-ARG-3                          PIC X ANY LENGTH.
 01 LNK-ARG-4                          PIC X ANY LENGTH.
 01 LNK-ARG-5                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                          BY REFERENCE LNK-ARG-2
                          BY REFERENCE LNK-ARG-3
                          BY REFERENCE LNK-ARG-4
                          BY REFERENCE LNK-ARG-5
                    RETURNING          LNK-RET.

 MAIN-J-CHOICEBOX3 SECTION.

    CALL STATIC "japi_choicebox3"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-2), X"00")
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-3), X"00")
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-4), X"00")
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-5), X"00")
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-CHOICEBOX3-EX.
    EXIT.
 END FUNCTION J-CHOICEBOX3.


*>------------------------------------------------------------------------------
*> int  j_progressbar( int arg0, int arg1)
*> { return( japi_progressbar(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-PROGRESSBAR.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-PROGRESSBAR SECTION.

    CALL STATIC "japi_progressbar"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-PROGRESSBAR-EX.
    EXIT.
 END FUNCTION J-PROGRESSBAR.


*>------------------------------------------------------------------------------
*> int  j_led( int arg0, int arg1, int arg2)
*> { return( japi_led(arg0, arg1, arg2));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-LED.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                    RETURNING          LNK-RET.

 MAIN-J-LED SECTION.

    CALL STATIC "japi_led"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-LED-EX.
    EXIT.
 END FUNCTION J-LED.


*>------------------------------------------------------------------------------
*> int  j_sevensegment( int arg0, int arg1)
*> { return( japi_sevensegment(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SEVENSEGMENT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SEVENSEGMENT SECTION.

    CALL STATIC "japi_sevensegment"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-SEVENSEGMENT-EX.
    EXIT.
 END FUNCTION J-SEVENSEGMENT.


*>------------------------------------------------------------------------------
*> int  j_meter( int arg0, char* arg1)
*> { return( japi_meter(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-METER.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-METER SECTION.

    CALL STATIC "japi_meter"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-METER-EX.
    EXIT.
 END FUNCTION J-METER.


*>------------------------------------------------------------------------------
*> void j_additem( int arg0, char* arg1)
*> { japi_additem(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-ADDITEM.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-ADDITEM SECTION.

    CALL STATIC "japi_additem"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-ADDITEM-EX.
    EXIT.
 END FUNCTION J-ADDITEM.


*>------------------------------------------------------------------------------
*> int  j_textfield( int arg0, int arg1)
*> { return( japi_textfield(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-TEXTFIELD.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-TEXTFIELD SECTION.

    CALL STATIC "japi_textfield"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-TEXTFIELD-EX.
    EXIT.
 END FUNCTION J-TEXTFIELD.


*>------------------------------------------------------------------------------
*> int  j_textarea( int arg0, int arg1, int arg2)
*> { return( japi_textarea(arg0, arg1, arg2));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-TEXTAREA.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                    RETURNING          LNK-RET.

 MAIN-J-TEXTAREA SECTION.

    CALL STATIC "japi_textarea"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-TEXTAREA-EX.
    EXIT.
 END FUNCTION J-TEXTAREA.


*>------------------------------------------------------------------------------
*> int  j_menubar( int arg0)
*> { return( japi_menubar(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-MENUBAR.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-MENUBAR SECTION.

    CALL STATIC "japi_menubar"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-MENUBAR-EX.
    EXIT.
 END FUNCTION J-MENUBAR.


*>------------------------------------------------------------------------------
*> int  j_menu( int arg0, char* arg1)
*> { return( japi_menu(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-MENU.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-MENU SECTION.

    CALL STATIC "japi_menu"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-MENU-EX.
    EXIT.
 END FUNCTION J-MENU.


*>------------------------------------------------------------------------------
*> int  j_helpmenu( int arg0, char* arg1)
*> { return( japi_helpmenu(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-HELPMENU.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-HELPMENU SECTION.

    CALL STATIC "japi_helpmenu"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-HELPMENU-EX.
    EXIT.
 END FUNCTION J-HELPMENU.


*>------------------------------------------------------------------------------
*> int  j_menuitem( int arg0, char* arg1)
*> { return( japi_menuitem(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-MENUITEM.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-MENUITEM SECTION.

    CALL STATIC "japi_menuitem"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-MENUITEM-EX.
    EXIT.
 END FUNCTION J-MENUITEM.


*>------------------------------------------------------------------------------
*> int  j_checkmenuitem( int arg0, char* arg1)
*> { return( japi_checkmenuitem(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-CHECKMENUITEM.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-CHECKMENUITEM SECTION.

    CALL STATIC "japi_checkmenuitem"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-CHECKMENUITEM-EX.
    EXIT.
 END FUNCTION J-CHECKMENUITEM.


*>------------------------------------------------------------------------------
*> void j_pack( int arg0)
*> { japi_pack(arg0);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-PACK.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-PACK SECTION.

    CALL STATIC "japi_pack"
         USING BY VALUE LNK-ARG-0
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-PACK-EX.
    EXIT.
 END FUNCTION J-PACK.


*>------------------------------------------------------------------------------
*> void j_print( int arg0)
*> { japi_print(arg0);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-PRINT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-PRINT SECTION.

    CALL STATIC "japi_print"
         USING BY VALUE LNK-ARG-0
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-PRINT-EX.
    EXIT.
 END FUNCTION J-PRINT.


*>------------------------------------------------------------------------------
*> void j_playsoundfile( char* arg0)
*> { japi_playsoundfile(arg0);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-PLAYSOUNDFILE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY REFERENCE LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-PLAYSOUNDFILE SECTION.

    CALL STATIC "japi_playsoundfile"
         USING BY CONTENT CONCATENATE(TRIM(LNK-ARG-0), X"00")
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-PLAYSOUNDFILE-EX.
    EXIT.
 END FUNCTION J-PLAYSOUNDFILE.


*>------------------------------------------------------------------------------
*> void j_play( int arg0)
*> { japi_play(arg0);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-PLAY.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-PLAY SECTION.

    CALL STATIC "japi_play"
         USING BY VALUE LNK-ARG-0
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-PLAY-EX.
    EXIT.
 END FUNCTION J-PLAY.


*>------------------------------------------------------------------------------
*> int  j_sound( char* arg0)
*> { return( japi_sound(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SOUND.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY REFERENCE LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-SOUND SECTION.

    CALL STATIC "japi_sound"
         USING BY CONTENT CONCATENATE(TRIM(LNK-ARG-0), X"00")
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-SOUND-EX.
    EXIT.
 END FUNCTION J-SOUND.


*>------------------------------------------------------------------------------
*> void j_setfont( int arg0, int arg1, int arg2, int arg3)
*> { japi_setfont(arg0, arg1, arg2, arg3);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETFONT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                    RETURNING          LNK-RET.

 MAIN-J-SETFONT SECTION.

    CALL STATIC "japi_setfont"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
               BY VALUE LNK-ARG-3
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETFONT-EX.
    EXIT.
 END FUNCTION J-SETFONT.


*>------------------------------------------------------------------------------
*> void j_setfontname( int arg0, int arg1)
*> { japi_setfontname(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETFONTNAME.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETFONTNAME SECTION.

    CALL STATIC "japi_setfontname"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETFONTNAME-EX.
    EXIT.
 END FUNCTION J-SETFONTNAME.


*>------------------------------------------------------------------------------
*> void j_setfontsize( int arg0, int arg1)
*> { japi_setfontsize(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETFONTSIZE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETFONTSIZE SECTION.

    CALL STATIC "japi_setfontsize"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETFONTSIZE-EX.
    EXIT.
 END FUNCTION J-SETFONTSIZE.


*>------------------------------------------------------------------------------
*> void j_setfontstyle( int arg0, int arg1)
*> { japi_setfontstyle(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETFONTSTYLE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETFONTSTYLE SECTION.

    CALL STATIC "japi_setfontstyle"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETFONTSTYLE-EX.
    EXIT.
 END FUNCTION J-SETFONTSTYLE.


*>------------------------------------------------------------------------------
*> void j_seperator( int arg0)
*> { japi_seperator(arg0);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SEPERATOR.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-SEPERATOR SECTION.

    CALL STATIC "japi_seperator"
         USING BY VALUE LNK-ARG-0
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SEPERATOR-EX.
    EXIT.
 END FUNCTION J-SEPERATOR.


*>------------------------------------------------------------------------------
*> void j_disable( int arg0)
*> { japi_disable(arg0);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-DISABLE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-DISABLE SECTION.

    CALL STATIC "japi_disable"
         USING BY VALUE LNK-ARG-0
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-DISABLE-EX.
    EXIT.
 END FUNCTION J-DISABLE.


*>------------------------------------------------------------------------------
*> void j_enable( int arg0)
*> { japi_enable(arg0);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-ENABLE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-ENABLE SECTION.

    CALL STATIC "japi_enable"
         USING BY VALUE LNK-ARG-0
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-ENABLE-EX.
    EXIT.
 END FUNCTION J-ENABLE.


*>------------------------------------------------------------------------------
*> int    j_getstate( int arg0)
*> { return( japi_getstate(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETSTATE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETSTATE SECTION.

    CALL STATIC "japi_getstate"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETSTATE-EX.
    EXIT.
 END FUNCTION J-GETSTATE.


*>------------------------------------------------------------------------------
*> int  j_getrows( int arg0)
*> { return( japi_getrows(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETROWS.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETROWS SECTION.

    CALL STATIC "japi_getrows"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETROWS-EX.
    EXIT.
 END FUNCTION J-GETROWS.


*>------------------------------------------------------------------------------
*> int  j_getcolumns( int arg0)
*> { return( japi_getcolumns(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETCOLUMNS.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETCOLUMNS SECTION.

    CALL STATIC "japi_getcolumns"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETCOLUMNS-EX.
    EXIT.
 END FUNCTION J-GETCOLUMNS.


*>------------------------------------------------------------------------------
*> int  j_getselect( int arg0)
*> { return( japi_getselect(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETSELECT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETSELECT SECTION.

    CALL STATIC "japi_getselect"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETSELECT-EX.
    EXIT.
 END FUNCTION J-GETSELECT.


*>------------------------------------------------------------------------------
*> int    j_isselect( int arg0, int arg1)
*> { return( japi_isselect(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-ISSELECT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-ISSELECT SECTION.

    CALL STATIC "japi_isselect"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-ISSELECT-EX.
    EXIT.
 END FUNCTION J-ISSELECT.


*>------------------------------------------------------------------------------
*> int    j_isvisible( int arg0)
*> { return( japi_isvisible(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-ISVISIBLE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-ISVISIBLE SECTION.

    CALL STATIC "japi_isvisible"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-ISVISIBLE-EX.
    EXIT.
 END FUNCTION J-ISVISIBLE.


*>------------------------------------------------------------------------------
*> int    j_isparent( int arg0, int arg1)
*> { return( japi_isparent(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-ISPARENT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-ISPARENT SECTION.

    CALL STATIC "japi_isparent"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-ISPARENT-EX.
    EXIT.
 END FUNCTION J-ISPARENT.


*>------------------------------------------------------------------------------
*> int    j_isresizable( int arg0)
*> { return( japi_isresizable(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-ISRESIZABLE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-ISRESIZABLE SECTION.

    CALL STATIC "japi_isresizable"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-ISRESIZABLE-EX.
    EXIT.
 END FUNCTION J-ISRESIZABLE.


*>------------------------------------------------------------------------------
*> void j_select( int arg0, int arg1)
*> { japi_select(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SELECT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SELECT SECTION.

    CALL STATIC "japi_select"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SELECT-EX.
    EXIT.
 END FUNCTION J-SELECT.


*>------------------------------------------------------------------------------
*> void j_deselect( int arg0, int arg1)
*> { japi_deselect(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-DESELECT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-DESELECT SECTION.

    CALL STATIC "japi_deselect"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-DESELECT-EX.
    EXIT.
 END FUNCTION J-DESELECT.


*>------------------------------------------------------------------------------
*> void j_multiplemode( int arg0, int arg1)
*> { japi_multiplemode(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-MULTIPLEMODE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-MULTIPLEMODE SECTION.

    CALL STATIC "japi_multiplemode"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-MULTIPLEMODE-EX.
    EXIT.
 END FUNCTION J-MULTIPLEMODE.


*>------------------------------------------------------------------------------
*> void j_insert( int arg0, int arg1, char* arg2)
*> { japi_insert(arg0, arg1, arg2);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-INSERT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY REFERENCE LNK-ARG-2
                    RETURNING          LNK-RET.

 MAIN-J-INSERT SECTION.

    CALL STATIC "japi_insert"
         USING BY VALUE   LNK-ARG-0
               BY VALUE   LNK-ARG-1
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-2), X"00")
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-INSERT-EX.
    EXIT.
 END FUNCTION J-INSERT.


*>------------------------------------------------------------------------------
*> void j_remove( int arg0, int arg1)
*> { japi_remove(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-REMOVE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-REMOVE SECTION.

    CALL STATIC "japi_remove"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-REMOVE-EX.
    EXIT.
 END FUNCTION J-REMOVE.


*>------------------------------------------------------------------------------
*> void j_removeitem( int arg0, char* arg1)
*> { japi_removeitem(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-REMOVEITEM.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-REMOVEITEM SECTION.

    CALL STATIC "japi_removeitem"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-REMOVEITEM-EX.
    EXIT.
 END FUNCTION J-REMOVEITEM.


*>------------------------------------------------------------------------------
*> void j_removeall( int arg0)
*> { japi_removeall(arg0);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-REMOVEALL.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-REMOVEALL SECTION.

    CALL STATIC "japi_removeall"
         USING BY VALUE LNK-ARG-0
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-REMOVEALL-EX.
    EXIT.
 END FUNCTION J-REMOVEALL.


*>------------------------------------------------------------------------------
*> void j_setstate( int arg0, int arg1)
*> { japi_setstate(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETSTATE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETSTATE SECTION.

    CALL STATIC "japi_setstate"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETSTATE-EX.
    EXIT.
 END FUNCTION J-SETSTATE.


*>------------------------------------------------------------------------------
*> void j_setrows( int arg0, int arg1)
*> { japi_setrows(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETROWS.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETROWS SECTION.

    CALL STATIC "japi_setrows"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETROWS-EX.
    EXIT.
 END FUNCTION J-SETROWS.


*>------------------------------------------------------------------------------
*> void j_setcolumns( int arg0, int arg1)
*> { japi_setcolumns(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETCOLUMNS.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETCOLUMNS SECTION.

    CALL STATIC "japi_setcolumns"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETCOLUMNS-EX.
    EXIT.
 END FUNCTION J-SETCOLUMNS.


*>------------------------------------------------------------------------------
*> void j_seticon( int arg0, int arg1)
*> { japi_seticon(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETICON.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETICON SECTION.

    CALL STATIC "japi_seticon"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETICON-EX.
    EXIT.
 END FUNCTION J-SETICON.


*>------------------------------------------------------------------------------
*> void j_setimage( int arg0, int arg1)
*> { japi_setimage(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETIMAGE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETIMAGE SECTION.

    CALL STATIC "japi_setimage"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETIMAGE-EX.
    EXIT.
 END FUNCTION J-SETIMAGE.


*>------------------------------------------------------------------------------
*> void j_setvalue( int arg0, int arg1)
*> { japi_setvalue(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETVALUE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETVALUE SECTION.

    CALL STATIC "japi_setvalue"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETVALUE-EX.
    EXIT.
 END FUNCTION J-SETVALUE.


*>------------------------------------------------------------------------------
*> void j_setradiogroup( int arg0, int arg1)
*> { japi_setradiogroup(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETRADIOGROUP.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETRADIOGROUP SECTION.

    CALL STATIC "japi_setradiogroup"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETRADIOGROUP-EX.
    EXIT.
 END FUNCTION J-SETRADIOGROUP.


*>------------------------------------------------------------------------------
*> void j_setunitinc( int arg0, int arg1)
*> { japi_setunitinc(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETUNITINC.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETUNITINC SECTION.

    CALL STATIC "japi_setunitinc"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETUNITINC-EX.
    EXIT.
 END FUNCTION J-SETUNITINC.


*>------------------------------------------------------------------------------
*> void j_setblockinc( int arg0, int arg1)
*> { japi_setblockinc(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETBLOCKINC.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETBLOCKINC SECTION.

    CALL STATIC "japi_setblockinc"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETBLOCKINC-EX.
    EXIT.
 END FUNCTION J-SETBLOCKINC.


*>------------------------------------------------------------------------------
*> void j_setmin( int arg0, int arg1)
*> { japi_setmin(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETMIN.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETMIN SECTION.

    CALL STATIC "japi_setmin"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETMIN-EX.
    EXIT.
 END FUNCTION J-SETMIN.


*>------------------------------------------------------------------------------
*> void j_setmax( int arg0, int arg1)
*> { japi_setmax(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETMAX.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETMAX SECTION.

    CALL STATIC "japi_setmax"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETMAX-EX.
    EXIT.
 END FUNCTION J-SETMAX.


*>------------------------------------------------------------------------------
*> void j_setdanger( int arg0, int arg1)
*> { japi_setdanger(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETDANGER.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETDANGER SECTION.

    CALL STATIC "japi_setdanger"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETDANGER-EX.
    EXIT.
 END FUNCTION J-SETDANGER.


*>------------------------------------------------------------------------------
*> void j_setslidesize( int arg0, int arg1)
*> { japi_setslidesize(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETSLIDESIZE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETSLIDESIZE SECTION.

    CALL STATIC "japi_setslidesize"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETSLIDESIZE-EX.
    EXIT.
 END FUNCTION J-SETSLIDESIZE.


*>------------------------------------------------------------------------------
*> void j_setcursor( int arg0, int arg1)
*> { japi_setcursor(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETCURSOR.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETCURSOR SECTION.

    CALL STATIC "japi_setcursor"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETCURSOR-EX.
    EXIT.
 END FUNCTION J-SETCURSOR.


*>------------------------------------------------------------------------------
*> void j_setresizable( int arg0, int arg1)
*> { japi_setresizable(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETRESIZABLE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETRESIZABLE SECTION.

    CALL STATIC "japi_setresizable"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETRESIZABLE-EX.
    EXIT.
 END FUNCTION J-SETRESIZABLE.


*>------------------------------------------------------------------------------
*> int  j_getlength( int arg0)
*> { return( japi_getlength(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETLENGTH.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETLENGTH SECTION.

    CALL STATIC "japi_getlength"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETLENGTH-EX.
    EXIT.
 END FUNCTION J-GETLENGTH.


*>------------------------------------------------------------------------------
*> int  j_getvalue( int arg0)
*> { return( japi_getvalue(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETVALUE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETVALUE SECTION.

    CALL STATIC "japi_getvalue"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETVALUE-EX.
    EXIT.
 END FUNCTION J-GETVALUE.


*>------------------------------------------------------------------------------
*> int  j_getdanger( int arg0)
*> { return( japi_getdanger(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETDANGER.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETDANGER SECTION.

    CALL STATIC "japi_getdanger"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETDANGER-EX.
    EXIT.
 END FUNCTION J-GETDANGER.


*>------------------------------------------------------------------------------
*> int  j_getscreenheight( )
*> { return( japi_getscreenheight());  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETSCREENHEIGHT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION
                    RETURNING          LNK-RET.

 MAIN-J-GETSCREENHEIGHT SECTION.

    CALL STATIC "japi_getscreenheight"
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETSCREENHEIGHT-EX.
    EXIT.
 END FUNCTION J-GETSCREENHEIGHT.


*>------------------------------------------------------------------------------
*> int  j_getscreenwidth( )
*> { return( japi_getscreenwidth());  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETSCREENWIDTH.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION
                    RETURNING          LNK-RET.

 MAIN-J-GETSCREENWIDTH SECTION.

    CALL STATIC "japi_getscreenwidth"
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETSCREENWIDTH-EX.
    EXIT.
 END FUNCTION J-GETSCREENWIDTH.


*>------------------------------------------------------------------------------
*> int  j_getheight( int arg0)
*> { return( japi_getheight(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETHEIGHT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETHEIGHT SECTION.

    CALL STATIC "japi_getheight"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETHEIGHT-EX.
    EXIT.
 END FUNCTION J-GETHEIGHT.


*>------------------------------------------------------------------------------
*> int  j_getwidth( int arg0)
*> { return( japi_getwidth(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETWIDTH.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETWIDTH SECTION.

    CALL STATIC "japi_getwidth"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETWIDTH-EX.
    EXIT.
 END FUNCTION J-GETWIDTH.


*>------------------------------------------------------------------------------
*> int  j_getinsets( int arg0, int arg1)
*> { return( japi_getinsets(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETINSETS.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-GETINSETS SECTION.

    CALL STATIC "japi_getinsets"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETINSETS-EX.
    EXIT.
 END FUNCTION J-GETINSETS.


*>------------------------------------------------------------------------------
*> int  j_getlayoutid( int arg0)
*> { return( japi_getlayoutid(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETLAYOUTID.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETLAYOUTID SECTION.

    CALL STATIC "japi_getlayoutid"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETLAYOUTID-EX.
    EXIT.
 END FUNCTION J-GETLAYOUTID.


*>------------------------------------------------------------------------------
*> int  j_getinheight( int arg0)
*> { return( japi_getinheight(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETINHEIGHT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETINHEIGHT SECTION.

    CALL STATIC "japi_getinheight"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETINHEIGHT-EX.
    EXIT.
 END FUNCTION J-GETINHEIGHT.


*>------------------------------------------------------------------------------
*> int  j_getinwidth( int arg0)
*> { return( japi_getinwidth(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETINWIDTH.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETINWIDTH SECTION.

    CALL STATIC "japi_getinwidth"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETINWIDTH-EX.
    EXIT.
 END FUNCTION J-GETINWIDTH.


*>------------------------------------------------------------------------------
*> char*  j_gettext( int arg0, char* arg1)
*> { return( japi_gettext(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETTEXT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 WS-IND                             BINARY-INT.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-GETTEXT SECTION.

*>  init output text field
    MOVE ALL X"00" TO LNK-ARG-1

    CALL STATIC "japi_gettext"
         USING BY VALUE     LNK-ARG-0
               BY REFERENCE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

*>  replace all chars with spaces after EOL
    PERFORM VARYING WS-IND FROM 1 BY 1
      UNTIL WS-IND > LENGTH(LNK-ARG-1)
       IF LNK-ARG-1(WS-IND:1) = X"00"
       THEN
          MOVE SPACES TO LNK-ARG-1(WS-IND:)
          EXIT PERFORM
       END-IF
    END-PERFORM

    GOBACK

    .
 MAIN-J-GETTEXT-EX.
    EXIT.
 END FUNCTION J-GETTEXT.


*>------------------------------------------------------------------------------
*> char*  j_getitem( int arg0, int arg1, char* arg2)
*> { return( japi_getitem(arg0, arg1, arg2));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETITEM.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 WS-IND                             BINARY-INT.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY REFERENCE LNK-ARG-2
                    RETURNING          LNK-RET.

 MAIN-J-GETITEM SECTION.

*>  init output text field
    MOVE ALL X"00" TO LNK-ARG-2

    CALL STATIC "japi_getitem"
         USING BY VALUE     LNK-ARG-0
               BY VALUE     LNK-ARG-1
               BY REFERENCE LNK-ARG-2
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

*>  replace all chars with spaces after EOL
    PERFORM VARYING WS-IND FROM 1 BY 1
      UNTIL WS-IND > LENGTH(LNK-ARG-2)
       IF LNK-ARG-2(WS-IND:1) = X"00"
       THEN
          MOVE SPACES TO LNK-ARG-2(WS-IND:)
          EXIT PERFORM
       END-IF
    END-PERFORM

    GOBACK

    .
 MAIN-J-GETITEM-EX.
    EXIT.
 END FUNCTION J-GETITEM.


*>------------------------------------------------------------------------------
*> int  j_getitemcount( int arg0)
*> { return( japi_getitemcount(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETITEMCOUNT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETITEMCOUNT SECTION.

    CALL STATIC "japi_getitemcount"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETITEMCOUNT-EX.
    EXIT.
 END FUNCTION J-GETITEMCOUNT.


*>------------------------------------------------------------------------------
*> void j_delete( int arg0, int arg1, int arg2)
*> { japi_delete(arg0, arg1, arg2);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-DELETE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                    RETURNING          LNK-RET.

 MAIN-J-DELETE SECTION.

    CALL STATIC "japi_delete"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-DELETE-EX.
    EXIT.
 END FUNCTION J-DELETE.


*>------------------------------------------------------------------------------
*> void j_replacetext( int arg0, char* arg1, int arg2, int arg3)
*> { japi_replacetext(arg0, arg1, arg2, arg3);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-REPLACETEXT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                    RETURNING          LNK-RET.

 MAIN-J-REPLACETEXT SECTION.

    CALL STATIC "japi_replacetext"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
               BY VALUE   LNK-ARG-2
               BY VALUE   LNK-ARG-3
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-REPLACETEXT-EX.
    EXIT.
 END FUNCTION J-REPLACETEXT.


*>------------------------------------------------------------------------------
*> void j_appendtext( int arg0, char* arg1)
*> { japi_appendtext(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-APPENDTEXT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-APPENDTEXT SECTION.

    CALL STATIC "japi_appendtext"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-APPENDTEXT-EX.
    EXIT.
 END FUNCTION J-APPENDTEXT.


*>------------------------------------------------------------------------------
*> void j_inserttext( int arg0, char* arg1, int arg2)
*> { japi_inserttext(arg0, arg1, arg2);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-INSERTTEXT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                    RETURNING          LNK-RET.

 MAIN-J-INSERTTEXT SECTION.

    CALL STATIC "japi_inserttext"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
               BY VALUE   LNK-ARG-2
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-INSERTTEXT-EX.
    EXIT.
 END FUNCTION J-INSERTTEXT.


*>------------------------------------------------------------------------------
*> void j_settext( int arg0, char* arg1)
*> { japi_settext(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETTEXT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETTEXT SECTION.

    CALL STATIC "japi_settext"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETTEXT-EX.
    EXIT.
 END FUNCTION J-SETTEXT.


*>------------------------------------------------------------------------------
*> void j_selectall( int arg0)
*> { japi_selectall(arg0);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SELECTALL.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-SELECTALL SECTION.

    CALL STATIC "japi_selectall"
         USING BY VALUE LNK-ARG-0
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SELECTALL-EX.
    EXIT.
 END FUNCTION J-SELECTALL.


*>------------------------------------------------------------------------------
*> void j_selecttext( int arg0, int arg1, int arg2)
*> { japi_selecttext(arg0, arg1, arg2);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SELECTTEXT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                    RETURNING          LNK-RET.

 MAIN-J-SELECTTEXT SECTION.

    CALL STATIC "japi_selecttext"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SELECTTEXT-EX.
    EXIT.
 END FUNCTION J-SELECTTEXT.


*>------------------------------------------------------------------------------
*> int  j_getselstart( int arg0)
*> { return( japi_getselstart(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETSELSTART.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETSELSTART SECTION.

    CALL STATIC "japi_getselstart"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETSELSTART-EX.
    EXIT.
 END FUNCTION J-GETSELSTART.


*>------------------------------------------------------------------------------
*> int  j_getselend( int arg0)
*> { return( japi_getselend(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETSELEND.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETSELEND SECTION.

    CALL STATIC "japi_getselend"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETSELEND-EX.
    EXIT.
 END FUNCTION J-GETSELEND.


*>------------------------------------------------------------------------------
*> char*  j_getseltext( int arg0, char* arg1)
*> { return( japi_getseltext(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETSELTEXT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 WS-IND                             BINARY-INT.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-GETSELTEXT SECTION.

*>  init output text field
    MOVE ALL X"00" TO LNK-ARG-1

    CALL STATIC "japi_getseltext"
         USING BY VALUE     LNK-ARG-0
               BY REFERENCE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

*>  replace all chars with spaces after EOL
    PERFORM VARYING WS-IND FROM 1 BY 1
      UNTIL WS-IND > LENGTH(LNK-ARG-1)
       IF LNK-ARG-1(WS-IND:1) = X"00"
       THEN
          MOVE SPACES TO LNK-ARG-1(WS-IND:)
          EXIT PERFORM
       END-IF
    END-PERFORM

    GOBACK

    .
 MAIN-J-GETSELTEXT-EX.
    EXIT.
 END FUNCTION J-GETSELTEXT.


*>------------------------------------------------------------------------------
*> int  j_getcurpos( int arg0)
*> { return( japi_getcurpos(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETCURPOS.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETCURPOS SECTION.

    CALL STATIC "japi_getcurpos"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETCURPOS-EX.
    EXIT.
 END FUNCTION J-GETCURPOS.


*>------------------------------------------------------------------------------
*> void j_setcurpos( int arg0, int arg1)
*> { japi_setcurpos(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETCURPOS.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETCURPOS SECTION.

    CALL STATIC "japi_setcurpos"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETCURPOS-EX.
    EXIT.
 END FUNCTION J-SETCURPOS.


*>------------------------------------------------------------------------------
*> void j_setechochar( int arg0, char arg1)
*> { japi_setechochar(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETECHOCHAR.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-CHAR.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETECHOCHAR SECTION.

    CALL STATIC "japi_setechochar"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETECHOCHAR-EX.
    EXIT.
 END FUNCTION J-SETECHOCHAR.


*>------------------------------------------------------------------------------
*> void j_seteditable( int arg0, int arg1)
*> { japi_seteditable(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETEDITABLE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-CHAR.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETEDITABLE SECTION.

    CALL STATIC "japi_seteditable"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETEDITABLE-EX.
    EXIT.
 END FUNCTION J-SETEDITABLE.


*>------------------------------------------------------------------------------
*> void j_setshortcut( int arg0, char arg1)
*> { japi_setshortcut(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETSHORTCUT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-CHAR.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETSHORTCUT SECTION.

    CALL STATIC "japi_setshortcut"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETSHORTCUT-EX.
    EXIT.
 END FUNCTION J-SETSHORTCUT.


*>------------------------------------------------------------------------------
*> void j_quit( )
*> { japi_quit();  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-QUIT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION
                    RETURNING          LNK-RET.

 MAIN-J-QUIT SECTION.

    CALL STATIC "japi_quit"
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-QUIT-EX.
    EXIT.
 END FUNCTION J-QUIT.


*>------------------------------------------------------------------------------
*> void j_kill( )
*> { japi_kill();  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-KILL.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION
                    RETURNING          LNK-RET.

 MAIN-J-KILL SECTION.

    CALL STATIC "japi_kill"
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-KILL-EX.
    EXIT.
 END FUNCTION J-KILL.


*>------------------------------------------------------------------------------
*> void j_setsize( int arg0, int arg1, int arg2)
*> { japi_setsize(arg0, arg1, arg2);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETSIZE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                    RETURNING          LNK-RET.

 MAIN-J-SETSIZE SECTION.

    CALL STATIC "japi_setsize"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETSIZE-EX.
    EXIT.
 END FUNCTION J-SETSIZE.


*>------------------------------------------------------------------------------
*> int  j_getaction( )
*> { return( japi_getaction());  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETACTION.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION
                    RETURNING          LNK-RET.

 MAIN-J-GETACTION SECTION.

    CALL STATIC "japi_getaction"
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETACTION-EX.
    EXIT.
 END FUNCTION J-GETACTION.


*>------------------------------------------------------------------------------
*> int  j_nextaction( )
*> { return( japi_nextaction());  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-NEXTACTION.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION
                    RETURNING          LNK-RET.

 MAIN-J-NEXTACTION SECTION.

    CALL STATIC "japi_nextaction"
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-NEXTACTION-EX.
    EXIT.
 END FUNCTION J-NEXTACTION.


*>------------------------------------------------------------------------------
*> void j_show( int arg0)
*> { japi_show(arg0);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SHOW.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-SHOW SECTION.

    CALL STATIC "japi_show"
         USING BY VALUE LNK-ARG-0
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SHOW-EX.
    EXIT.
 END FUNCTION J-SHOW.


*>------------------------------------------------------------------------------
*> void j_showpopup( int arg0, int arg1, int arg2)
*> { japi_showpopup(arg0, arg1, arg2);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SHOWPOPUP.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                    RETURNING          LNK-RET.

 MAIN-J-SHOWPOPUP SECTION.

    CALL STATIC "japi_showpopup"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SHOWPOPUP-EX.
    EXIT.
 END FUNCTION J-SHOWPOPUP.


*>------------------------------------------------------------------------------
*> void j_add( int arg0, int arg1)
*> { japi_add(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-ADD.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-ADD SECTION.

    CALL STATIC "japi_add"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-ADD-EX.
    EXIT.
 END FUNCTION J-ADD.


*>------------------------------------------------------------------------------
*> void j_release( int arg0)
*> { japi_release(arg0);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-RELEASE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-RELEASE SECTION.

    CALL STATIC "japi_release"
         USING BY VALUE LNK-ARG-0
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-RELEASE-EX.
    EXIT.
 END FUNCTION J-RELEASE.


*>------------------------------------------------------------------------------
*> void j_releaseall( int arg0)
*> { japi_releaseall(arg0);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-RELEASEALL.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-RELEASEALL SECTION.

    CALL STATIC "japi_releaseall"
         USING BY VALUE LNK-ARG-0
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-RELEASEALL-EX.
    EXIT.
 END FUNCTION J-RELEASEALL.


*>------------------------------------------------------------------------------
*> void j_hide( int arg0)
*> { japi_hide(arg0);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-HIDE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-HIDE SECTION.

    CALL STATIC "japi_hide"
         USING BY VALUE LNK-ARG-0
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-HIDE-EX.
    EXIT.
 END FUNCTION J-HIDE.


*>------------------------------------------------------------------------------
*> void j_dispose( int arg0)
*> { japi_dispose(arg0);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-DISPOSE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-DISPOSE SECTION.

    CALL STATIC "japi_dispose"
         USING BY VALUE LNK-ARG-0
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-DISPOSE-EX.
    EXIT.
 END FUNCTION J-DISPOSE.


*>------------------------------------------------------------------------------
*> void j_setpos( int arg0, int arg1, int arg2)
*> { japi_setpos(arg0, arg1, arg2);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETPOS.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                    RETURNING          LNK-RET.

 MAIN-J-SETPOS SECTION.

    CALL STATIC "japi_setpos"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETPOS-EX.
    EXIT.
 END FUNCTION J-SETPOS.


*>------------------------------------------------------------------------------
*> int  j_getviewportheight( int arg0)
*> { return( japi_getviewportheight(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETVIEWPORTHEIGHT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETVIEWPORTHEIGHT SECTION.

    CALL STATIC "japi_getviewportheight"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETVIEWPORTHEIGHT-EX.
    EXIT.
 END FUNCTION J-GETVIEWPORTHEIGHT.


*>------------------------------------------------------------------------------
*> int  j_getviewportwidth( int arg0)
*> { return( japi_getviewportwidth(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETVIEWPORTWIDTH.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETVIEWPORTWIDTH SECTION.

    CALL STATIC "japi_getviewportwidth"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETVIEWPORTWIDTH-EX.
    EXIT.
 END FUNCTION J-GETVIEWPORTWIDTH.


*>------------------------------------------------------------------------------
*> int  j_getxpos( int arg0)
*> { return( japi_getxpos(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETXPOS.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETXPOS SECTION.

    CALL STATIC "japi_getxpos"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETXPOS-EX.
    EXIT.
 END FUNCTION J-GETXPOS.


*>------------------------------------------------------------------------------
*> int  j_getypos( int arg0)
*> { return( japi_getypos(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETYPOS.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETYPOS SECTION.

    CALL STATIC "japi_getypos"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETYPOS-EX.
    EXIT.
 END FUNCTION J-GETYPOS.


*>------------------------------------------------------------------------------
*> void j_getpos( int arg0, int* arg1, int* arg2)
*> { japi_getpos(arg0, arg1, arg2);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETPOS.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                          BY REFERENCE LNK-ARG-2
                    RETURNING          LNK-RET.

 MAIN-J-GETPOS SECTION.

    CALL STATIC "japi_getpos"
         USING BY VALUE     LNK-ARG-0
               BY REFERENCE LNK-ARG-1
               BY REFERENCE LNK-ARG-2
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-GETPOS-EX.
    EXIT.
 END FUNCTION J-GETPOS.


*>------------------------------------------------------------------------------
*> int  j_getparentid( int arg0)
*> { return( japi_getparentid(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETPARENTID.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETPARENTID SECTION.

    CALL STATIC "japi_getparentid"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETPARENTID-EX.
    EXIT.
 END FUNCTION J-GETPARENTID.


*>------------------------------------------------------------------------------
*> void j_setfocus( int arg0)
*> { japi_setfocus(arg0);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETFOCUS.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-SETFOCUS SECTION.

    CALL STATIC "japi_setfocus"
         USING BY VALUE LNK-ARG-0
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETFOCUS-EX.
    EXIT.
 END FUNCTION J-SETFOCUS.


*>------------------------------------------------------------------------------
*> int    j_hasfocus( int arg0)
*> { return( japi_hasfocus(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-HASFOCUS.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-HASFOCUS SECTION.

    CALL STATIC "japi_hasfocus"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-HASFOCUS-EX.
    EXIT.
 END FUNCTION J-HASFOCUS.


*>------------------------------------------------------------------------------
*> int  j_getstringwidth( int arg0, char* arg1)
*> { return( japi_getstringwidth(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETSTRINGWIDTH.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-GETSTRINGWIDTH SECTION.

    CALL STATIC "japi_getstringwidth"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETSTRINGWIDTH-EX.
    EXIT.
 END FUNCTION J-GETSTRINGWIDTH.


*>------------------------------------------------------------------------------
*> int  j_getfontheight( int arg0)
*> { return( japi_getfontheight(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETFONTHEIGHT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETFONTHEIGHT SECTION.

    CALL STATIC "japi_getfontheight"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETFONTHEIGHT-EX.
    EXIT.
 END FUNCTION J-GETFONTHEIGHT.


*>------------------------------------------------------------------------------
*> int  j_getfontascent( int arg0)
*> { return( japi_getfontascent(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETFONTASCENT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETFONTASCENT SECTION.

    CALL STATIC "japi_getfontascent"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETFONTASCENT-EX.
    EXIT.
 END FUNCTION J-GETFONTASCENT.


*>------------------------------------------------------------------------------
*> int  j_keylistener( int arg0)
*> { return( japi_keylistener(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-KEYLISTENER.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-KEYLISTENER SECTION.

    CALL STATIC "japi_keylistener"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-KEYLISTENER-EX.
    EXIT.
 END FUNCTION J-KEYLISTENER.


*>------------------------------------------------------------------------------
*> int  j_getkeycode( int arg0)
*> { return( japi_getkeycode(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETKEYCODE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETKEYCODE SECTION.

    CALL STATIC "japi_getkeycode"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETKEYCODE-EX.
    EXIT.
 END FUNCTION J-GETKEYCODE.


*>------------------------------------------------------------------------------
*> int  j_getkeychar( int arg0)
*> { return( japi_getkeychar(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETKEYCHAR.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETKEYCHAR SECTION.

    CALL STATIC "japi_getkeychar"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETKEYCHAR-EX.
    EXIT.
 END FUNCTION J-GETKEYCHAR.


*>------------------------------------------------------------------------------
*> int  j_mouselistener( int arg0, int arg1)
*> { return( japi_mouselistener(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-MOUSELISTENER.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-MOUSELISTENER SECTION.

    CALL STATIC "japi_mouselistener"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-MOUSELISTENER-EX.
    EXIT.
 END FUNCTION J-MOUSELISTENER.


*>------------------------------------------------------------------------------
*> int  j_getmousex( int arg0)
*> { return( japi_getmousex(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETMOUSEX.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETMOUSEX SECTION.

    CALL STATIC "japi_getmousex"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETMOUSEX-EX.
    EXIT.
 END FUNCTION J-GETMOUSEX.


*>------------------------------------------------------------------------------
*> int  j_getmousey( int arg0)
*> { return( japi_getmousey(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETMOUSEY.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETMOUSEY SECTION.

    CALL STATIC "japi_getmousey"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETMOUSEY-EX.
    EXIT.
 END FUNCTION J-GETMOUSEY.


*>------------------------------------------------------------------------------
*> void j_getmousepos( int arg0, int* arg1, int* arg2)
*> { japi_getmousepos(arg0, arg1, arg2);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETMOUSEPOS.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.

 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                          BY REFERENCE LNK-ARG-2
                    RETURNING          LNK-RET.

 MAIN-J-GETMOUSEPOS SECTION.

    CALL STATIC "japi_getmousepos"
         USING BY VALUE     LNK-ARG-0
               BY REFERENCE LNK-ARG-1
               BY REFERENCE LNK-ARG-2
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-GETMOUSEPOS-EX.
    EXIT.
 END FUNCTION J-GETMOUSEPOS.


*>------------------------------------------------------------------------------
*> int  j_getmousebutton( int arg0)
*> { return( japi_getmousebutton(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETMOUSEBUTTON.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETMOUSEBUTTON SECTION.

    CALL STATIC "japi_getmousebutton"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETMOUSEBUTTON-EX.
    EXIT.
 END FUNCTION J-GETMOUSEBUTTON.


*>------------------------------------------------------------------------------
*> int  j_focuslistener( int arg0)
*> { return( japi_focuslistener(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-FOCUSLISTENER.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-FOCUSLISTENER SECTION.

    CALL STATIC "japi_focuslistener"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-FOCUSLISTENER-EX.
    EXIT.
 END FUNCTION J-FOCUSLISTENER.


*>------------------------------------------------------------------------------
*> int  j_componentlistener( int arg0, int arg1)
*> { return( japi_componentlistener(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-COMPONENTLISTENER.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-COMPONENTLISTENER SECTION.

    CALL STATIC "japi_componentlistener"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-COMPONENTLISTENER-EX.
    EXIT.
 END FUNCTION J-COMPONENTLISTENER.


*>------------------------------------------------------------------------------
*> int  j_windowlistener( int arg0, int arg1)
*> { return( japi_windowlistener(arg0, arg1));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-WINDOWLISTENER.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-WINDOWLISTENER SECTION.

    CALL STATIC "japi_windowlistener"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-WINDOWLISTENER-EX.
    EXIT.
 END FUNCTION J-WINDOWLISTENER.


*>------------------------------------------------------------------------------
*> void j_setflowlayout( int arg0, int arg1)
*> { japi_setflowlayout(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETFLOWLAYOUT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETFLOWLAYOUT SECTION.

    CALL STATIC "japi_setflowlayout"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETFLOWLAYOUT-EX.
    EXIT.
 END FUNCTION J-SETFLOWLAYOUT.


*>------------------------------------------------------------------------------
*> void j_setborderlayout( int arg0)
*> { japi_setborderlayout(arg0);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETBORDERLAYOUT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-SETBORDERLAYOUT SECTION.

    CALL STATIC "japi_setborderlayout"
         USING BY VALUE LNK-ARG-0
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETBORDERLAYOUT-EX.
    EXIT.
 END FUNCTION J-SETBORDERLAYOUT.


*>------------------------------------------------------------------------------
*> void j_setgridlayout( int arg0, int arg1, int arg2)
*> { japi_setgridlayout(arg0, arg1, arg2);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETGRIDLAYOUT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                    RETURNING          LNK-RET.

 MAIN-J-SETGRIDLAYOUT SECTION.

    CALL STATIC "japi_setgridlayout"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETGRIDLAYOUT-EX.
    EXIT.
 END FUNCTION J-SETGRIDLAYOUT.


*>------------------------------------------------------------------------------
*> void j_setfixlayout( int arg0)
*> { japi_setfixlayout(arg0);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETFIXLAYOUT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-SETFIXLAYOUT SECTION.

    CALL STATIC "japi_setfixlayout"
         USING BY VALUE LNK-ARG-0
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETFIXLAYOUT-EX.
    EXIT.
 END FUNCTION J-SETFIXLAYOUT.


*>------------------------------------------------------------------------------
*> void j_setnolayout( int arg0)
*> { japi_setnolayout(arg0);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETNOLAYOUT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-SETNOLAYOUT SECTION.

    CALL STATIC "japi_setnolayout"
         USING BY VALUE LNK-ARG-0
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETNOLAYOUT-EX.
    EXIT.
 END FUNCTION J-SETNOLAYOUT.


*>------------------------------------------------------------------------------
*> void j_setborderpos( int arg0, int arg1)
*> { japi_setborderpos(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETBORDERPOS.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETBORDERPOS SECTION.

    CALL STATIC "japi_setborderpos"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETBORDERPOS-EX.
    EXIT.
 END FUNCTION J-SETBORDERPOS.


*>------------------------------------------------------------------------------
*> void j_sethgap( int arg0, int arg1)
*> { japi_sethgap(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETHGAP.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETHGAP SECTION.

    CALL STATIC "japi_sethgap"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETHGAP-EX.
    EXIT.
 END FUNCTION J-SETHGAP.


*>------------------------------------------------------------------------------
*> void j_setvgap( int arg0, int arg1)
*> { japi_setvgap(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETVGAP.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETVGAP SECTION.

    CALL STATIC "japi_setvgap"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETVGAP-EX.
    EXIT.
 END FUNCTION J-SETVGAP.


*>------------------------------------------------------------------------------
*> void j_setinsets( int arg0, int arg1, int arg2, int arg3, int arg4)
*> { japi_setinsets(arg0, arg1, arg2, arg3, arg4);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETINSETS.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-ARG-4                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                          BY VALUE     LNK-ARG-4
                    RETURNING          LNK-RET.

 MAIN-J-SETINSETS SECTION.

    CALL STATIC "japi_setinsets"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
               BY VALUE LNK-ARG-3
               BY VALUE LNK-ARG-4
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETINSETS-EX.
    EXIT.
 END FUNCTION J-SETINSETS.


*>------------------------------------------------------------------------------
*> void j_setalign( int arg0, int arg1)
*> { japi_setalign(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETALIGN.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETALIGN SECTION.

    CALL STATIC "japi_setalign"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETALIGN-EX.
    EXIT.
 END FUNCTION J-SETALIGN.


*>------------------------------------------------------------------------------
*> void j_setflowfill( int arg0, int arg1)
*> { japi_setflowfill(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETFLOWFILL.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETFLOWFILL SECTION.

    CALL STATIC "japi_setflowfill"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETFLOWFILL-EX.
    EXIT.
 END FUNCTION J-SETFLOWFILL.


*>------------------------------------------------------------------------------
*> void j_translate( int arg0, int arg1, int arg2)
*> { japi_translate(arg0, arg1, arg2);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-TRANSLATE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                    RETURNING          LNK-RET.

 MAIN-J-TRANSLATE SECTION.

    CALL STATIC "japi_translate"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-TRANSLATE-EX.
    EXIT.
 END FUNCTION J-TRANSLATE.


*>------------------------------------------------------------------------------
*> void j_cliprect( int arg0, int arg1, int arg2, int arg3, int arg4)
*> { japi_cliprect(arg0, arg1, arg2, arg3, arg4);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-CLIPRECT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-ARG-4                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                          BY VALUE     LNK-ARG-4
                    RETURNING          LNK-RET.

 MAIN-J-CLIPRECT SECTION.

    CALL STATIC "japi_cliprect"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
               BY VALUE LNK-ARG-3
               BY VALUE LNK-ARG-4
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-CLIPRECT-EX.
    EXIT.
 END FUNCTION J-CLIPRECT.


*>------------------------------------------------------------------------------
*> void j_drawrect( int arg0, int arg1, int arg2, int arg3, int arg4)
*> { japi_drawrect(arg0, arg1, arg2, arg3, arg4);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-DRAWRECT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-ARG-4                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                          BY VALUE     LNK-ARG-4
                    RETURNING          LNK-RET.

 MAIN-J-DRAWRECT SECTION.

    CALL STATIC "japi_drawrect"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
               BY VALUE LNK-ARG-3
               BY VALUE LNK-ARG-4
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-DRAWRECT-EX.
    EXIT.
 END FUNCTION J-DRAWRECT.


*>------------------------------------------------------------------------------
*> void j_fillrect( int arg0, int arg1, int arg2, int arg3, int arg4)
*> { japi_fillrect(arg0, arg1, arg2, arg3, arg4);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-FILLRECT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-ARG-4                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                          BY VALUE     LNK-ARG-4
                    RETURNING          LNK-RET.

 MAIN-J-FILLRECT SECTION.

    CALL STATIC "japi_fillrect"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
               BY VALUE LNK-ARG-3
               BY VALUE LNK-ARG-4
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-FILLRECT-EX.
    EXIT.
 END FUNCTION J-FILLRECT.


*>------------------------------------------------------------------------------
*> void j_drawroundrect( int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6)
*> { japi_drawroundrect(arg0, arg1, arg2, arg3, arg4, arg5, arg6);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-DRAWROUNDRECT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-ARG-4                          BINARY-INT.
 01 LNK-ARG-5                          BINARY-INT.
 01 LNK-ARG-6                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                          BY VALUE     LNK-ARG-4
                          BY VALUE     LNK-ARG-5
                          BY VALUE     LNK-ARG-6
                    RETURNING          LNK-RET.

 MAIN-J-DRAWROUNDRECT SECTION.

    CALL STATIC "japi_drawroundrect"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
               BY VALUE LNK-ARG-3
               BY VALUE LNK-ARG-4
               BY VALUE LNK-ARG-5
               BY VALUE LNK-ARG-6
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-DRAWROUNDRECT-EX.
    EXIT.
 END FUNCTION J-DRAWROUNDRECT.


*>------------------------------------------------------------------------------
*> void j_fillroundrect( int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6)
*> { japi_fillroundrect(arg0, arg1, arg2, arg3, arg4, arg5, arg6);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-FILLROUNDRECT.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-ARG-4                          BINARY-INT.
 01 LNK-ARG-5                          BINARY-INT.
 01 LNK-ARG-6                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                          BY VALUE     LNK-ARG-4
                          BY VALUE     LNK-ARG-5
                          BY VALUE     LNK-ARG-6
                    RETURNING          LNK-RET.

 MAIN-J-FILLROUNDRECT SECTION.

    CALL STATIC "japi_fillroundrect"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
               BY VALUE LNK-ARG-3
               BY VALUE LNK-ARG-4
               BY VALUE LNK-ARG-5
               BY VALUE LNK-ARG-6
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-FILLROUNDRECT-EX.
    EXIT.
 END FUNCTION J-FILLROUNDRECT.


*>------------------------------------------------------------------------------
*> void j_drawoval( int arg0, int arg1, int arg2, int arg3, int arg4)
*> { japi_drawoval(arg0, arg1, arg2, arg3, arg4);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-DRAWOVAL.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-ARG-4                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                          BY VALUE     LNK-ARG-4
                    RETURNING          LNK-RET.

 MAIN-J-DRAWOVAL SECTION.

    CALL STATIC "japi_drawoval"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
               BY VALUE LNK-ARG-3
               BY VALUE LNK-ARG-4
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-DRAWOVAL-EX.
    EXIT.
 END FUNCTION J-DRAWOVAL.


*>------------------------------------------------------------------------------
*> void j_filloval( int arg0, int arg1, int arg2, int arg3, int arg4)
*> { japi_filloval(arg0, arg1, arg2, arg3, arg4);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-FILLOVAL.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-ARG-4                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                          BY VALUE     LNK-ARG-4
                    RETURNING          LNK-RET.

 MAIN-J-FILLOVAL SECTION.

    CALL STATIC "japi_filloval"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
               BY VALUE LNK-ARG-3
               BY VALUE LNK-ARG-4
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-FILLOVAL-EX.
    EXIT.
 END FUNCTION J-FILLOVAL.


*>------------------------------------------------------------------------------
*> void j_drawcircle( int arg0, int arg1, int arg2, int arg3)
*> { japi_drawcircle(arg0, arg1, arg2, arg3);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-DRAWCIRCLE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                    RETURNING          LNK-RET.

 MAIN-J-DRAWCIRCLE SECTION.

    CALL STATIC "japi_drawcircle"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
               BY VALUE LNK-ARG-3
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-DRAWCIRCLE-EX.
    EXIT.
 END FUNCTION J-DRAWCIRCLE.


*>------------------------------------------------------------------------------
*> void j_fillcircle( int arg0, int arg1, int arg2, int arg3)
*> { japi_fillcircle(arg0, arg1, arg2, arg3);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-FILLCIRCLE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                    RETURNING          LNK-RET.

 MAIN-J-FILLCIRCLE SECTION.

    CALL STATIC "japi_fillcircle"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
               BY VALUE LNK-ARG-3
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-FILLCIRCLE-EX.
    EXIT.
 END FUNCTION J-FILLCIRCLE.


*>------------------------------------------------------------------------------
*> void j_drawarc( int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6)
*> { japi_drawarc(arg0, arg1, arg2, arg3, arg4, arg5, arg6);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-DRAWARC.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-ARG-4                          BINARY-INT.
 01 LNK-ARG-5                          BINARY-INT.
 01 LNK-ARG-6                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                          BY VALUE     LNK-ARG-4
                          BY VALUE     LNK-ARG-5
                          BY VALUE     LNK-ARG-6
                    RETURNING          LNK-RET.

 MAIN-J-DRAWARC SECTION.

    CALL STATIC "japi_drawarc"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
               BY VALUE LNK-ARG-3
               BY VALUE LNK-ARG-4
               BY VALUE LNK-ARG-5
               BY VALUE LNK-ARG-6
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-DRAWARC-EX.
    EXIT.
 END FUNCTION J-DRAWARC.


*>------------------------------------------------------------------------------
*> void j_fillarc( int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6)
*> { japi_fillarc(arg0, arg1, arg2, arg3, arg4, arg5, arg6);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-FILLARC.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-ARG-4                          BINARY-INT.
 01 LNK-ARG-5                          BINARY-INT.
 01 LNK-ARG-6                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                          BY VALUE     LNK-ARG-4
                          BY VALUE     LNK-ARG-5
                          BY VALUE     LNK-ARG-6
                    RETURNING          LNK-RET.

 MAIN-J-FILLARC SECTION.

    CALL STATIC "japi_fillarc"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
               BY VALUE LNK-ARG-3
               BY VALUE LNK-ARG-4
               BY VALUE LNK-ARG-5
               BY VALUE LNK-ARG-6
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-FILLARC-EX.
    EXIT.
 END FUNCTION J-FILLARC.


*>------------------------------------------------------------------------------
*> void j_drawline( int arg0, int arg1, int arg2, int arg3, int arg4)
*> { japi_drawline(arg0, arg1, arg2, arg3, arg4);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-DRAWLINE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-ARG-4                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                          BY VALUE     LNK-ARG-4
                    RETURNING          LNK-RET.

 MAIN-J-DRAWLINE SECTION.

    CALL STATIC "japi_drawline"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
               BY VALUE LNK-ARG-3
               BY VALUE LNK-ARG-4
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-DRAWLINE-EX.
    EXIT.
 END FUNCTION J-DRAWLINE.


*>------------------------------------------------------------------------------
*> void j_drawpolyline( int arg0, int arg1, int* arg2, int* arg3)
*> { japi_drawpolyline(arg0, arg1, arg2, arg3);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-DRAWPOLYLINE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY REFERENCE LNK-ARG-2
                          BY REFERENCE LNK-ARG-3
                    RETURNING          LNK-RET.

 MAIN-J-DRAWPOLYLINE SECTION.

    CALL STATIC "japi_drawpolyline"
         USING BY VALUE     LNK-ARG-0
               BY VALUE     LNK-ARG-1
               BY REFERENCE LNK-ARG-2
               BY REFERENCE LNK-ARG-3
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-DRAWPOLYLINE-EX.
    EXIT.
 END FUNCTION J-DRAWPOLYLINE.


*>------------------------------------------------------------------------------
*> void j_drawpolygon( int arg0, int arg1, int* arg2, int* arg3)
*> { japi_drawpolygon(arg0, arg1, arg2, arg3);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-DRAWPOLYGON.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY REFERENCE LNK-ARG-2
                          BY REFERENCE LNK-ARG-3
                    RETURNING          LNK-RET.

 MAIN-J-DRAWPOLYGON SECTION.

    CALL STATIC "japi_drawpolygon"
         USING BY VALUE     LNK-ARG-0
               BY VALUE     LNK-ARG-1
               BY REFERENCE LNK-ARG-2
               BY REFERENCE LNK-ARG-3
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-DRAWPOLYGON-EX.
    EXIT.
 END FUNCTION J-DRAWPOLYGON.


*>------------------------------------------------------------------------------
*> void j_fillpolygon( int arg0, int arg1, int* arg2, int* arg3)
*> { japi_fillpolygon(arg0, arg1, arg2, arg3);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-FILLPOLYGON.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY REFERENCE LNK-ARG-2
                          BY REFERENCE LNK-ARG-3
                    RETURNING          LNK-RET.

 MAIN-J-FILLPOLYGON SECTION.

    CALL STATIC "japi_fillpolygon"
         USING BY VALUE     LNK-ARG-0
               BY VALUE     LNK-ARG-1
               BY REFERENCE LNK-ARG-2
               BY REFERENCE LNK-ARG-3
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-FILLPOLYGON-EX.
    EXIT.
 END FUNCTION J-FILLPOLYGON.


*>------------------------------------------------------------------------------
*> void j_drawpixel( int arg0, int arg1, int arg2)
*> { japi_drawpixel(arg0, arg1, arg2);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-DRAWPIXEL.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                    RETURNING          LNK-RET.

 MAIN-J-DRAWPIXEL SECTION.

    CALL STATIC "japi_drawpixel"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-DRAWPIXEL-EX.
    EXIT.
 END FUNCTION J-DRAWPIXEL.


*>------------------------------------------------------------------------------
*> void j_drawstring( int arg0, int arg1, int arg2, char* arg3)
*> { japi_drawstring(arg0, arg1, arg2, arg3);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-DRAWSTRING.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY REFERENCE LNK-ARG-3
                    RETURNING          LNK-RET.

 MAIN-J-DRAWSTRING SECTION.

    CALL STATIC "japi_drawstring"
         USING BY VALUE   LNK-ARG-0
               BY VALUE   LNK-ARG-1
               BY VALUE   LNK-ARG-2
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-3), X"00")
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-DRAWSTRING-EX.
    EXIT.
 END FUNCTION J-DRAWSTRING.


*>------------------------------------------------------------------------------
*> void j_setxor( int arg0, int arg1)
*> { japi_setxor(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETXOR.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETXOR SECTION.

    CALL STATIC "japi_setxor"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETXOR-EX.
    EXIT.
 END FUNCTION J-SETXOR.


*>------------------------------------------------------------------------------
*> int  j_getimage( int arg0)
*> { return( japi_getimage(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETIMAGE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-GETIMAGE SECTION.

    CALL STATIC "japi_getimage"
         USING BY VALUE LNK-ARG-0
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETIMAGE-EX.
    EXIT.
 END FUNCTION J-GETIMAGE.


*>------------------------------------------------------------------------------
*> void j_getimagesource( int arg0, int arg1, int arg2, int arg3, int arg4, int* arg5, int* arg6, int* arg7)
*> { japi_getimagesource(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETIMAGESOURCE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-ARG-4                          BINARY-INT.
 01 LNK-ARG-5                          BINARY-INT.
 01 LNK-ARG-6                          BINARY-INT.
 01 LNK-ARG-7                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                          BY VALUE     LNK-ARG-4
                          BY REFERENCE LNK-ARG-5
                          BY REFERENCE LNK-ARG-6
                          BY REFERENCE LNK-ARG-7
                    RETURNING          LNK-RET.

 MAIN-J-GETIMAGESOURCE SECTION.

    CALL STATIC "japi_getimagesource"
         USING BY VALUE     LNK-ARG-0
               BY VALUE     LNK-ARG-1
               BY VALUE     LNK-ARG-2
               BY VALUE     LNK-ARG-3
               BY VALUE     LNK-ARG-4
               BY REFERENCE LNK-ARG-5
               BY REFERENCE LNK-ARG-6
               BY REFERENCE LNK-ARG-7
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-GETIMAGESOURCE-EX.
    EXIT.
 END FUNCTION J-GETIMAGESOURCE.


*>------------------------------------------------------------------------------
*> void j_drawimagesource( int arg0, int arg1, int arg2, int arg3, int arg4, int* arg5, int* arg6, int* arg7)
*> { japi_drawimagesource(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-DRAWIMAGESOURCE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-ARG-4                          BINARY-INT.
 01 LNK-ARG-5                          BINARY-INT.
 01 LNK-ARG-6                          BINARY-INT.
 01 LNK-ARG-7                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                          BY VALUE     LNK-ARG-4
                          BY REFERENCE LNK-ARG-5
                          BY REFERENCE LNK-ARG-6
                          BY REFERENCE LNK-ARG-7
                    RETURNING          LNK-RET.

 MAIN-J-DRAWIMAGESOURCE SECTION.

    CALL STATIC "japi_drawimagesource"
         USING BY VALUE     LNK-ARG-0
               BY VALUE     LNK-ARG-1
               BY VALUE     LNK-ARG-2
               BY VALUE     LNK-ARG-3
               BY VALUE     LNK-ARG-4
               BY REFERENCE LNK-ARG-5
               BY REFERENCE LNK-ARG-6
               BY REFERENCE LNK-ARG-7
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-DRAWIMAGESOURCE-EX.
    EXIT.
 END FUNCTION J-DRAWIMAGESOURCE.


*>------------------------------------------------------------------------------
*> int  j_getscaledimage( int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6)
*> { return( japi_getscaledimage(arg0, arg1, arg2, arg3, arg4, arg5, arg6));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-GETSCALEDIMAGE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-ARG-4                          BINARY-INT.
 01 LNK-ARG-5                          BINARY-INT.
 01 LNK-ARG-6                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                          BY VALUE     LNK-ARG-4
                          BY VALUE     LNK-ARG-5
                          BY VALUE     LNK-ARG-6
                    RETURNING          LNK-RET.

 MAIN-J-GETSCALEDIMAGE SECTION.

    CALL STATIC "japi_getscaledimage"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
               BY VALUE LNK-ARG-3
               BY VALUE LNK-ARG-4
               BY VALUE LNK-ARG-5
               BY VALUE LNK-ARG-6
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-GETSCALEDIMAGE-EX.
    EXIT.
 END FUNCTION J-GETSCALEDIMAGE.


*>------------------------------------------------------------------------------
*> void j_drawimage( int arg0, int arg1, int arg2, int arg3)
*> { japi_drawimage(arg0, arg1, arg2, arg3);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-DRAWIMAGE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                    RETURNING          LNK-RET.

 MAIN-J-DRAWIMAGE SECTION.

    CALL STATIC "japi_drawimage"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
               BY VALUE LNK-ARG-3
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-DRAWIMAGE-EX.
    EXIT.
 END FUNCTION J-DRAWIMAGE.


*>------------------------------------------------------------------------------
*> void j_drawscaledimage( int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8, int arg9)
*> { japi_drawscaledimage(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-DRAWSCALEDIMAGE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-ARG-4                          BINARY-INT.
 01 LNK-ARG-5                          BINARY-INT.
 01 LNK-ARG-6                          BINARY-INT.
 01 LNK-ARG-7                          BINARY-INT.
 01 LNK-ARG-8                          BINARY-INT.
 01 LNK-ARG-9                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                          BY VALUE     LNK-ARG-4
                          BY VALUE     LNK-ARG-5
                          BY VALUE     LNK-ARG-6
                          BY VALUE     LNK-ARG-7
                          BY VALUE     LNK-ARG-8
                          BY VALUE     LNK-ARG-9
                    RETURNING          LNK-RET.

 MAIN-J-DRAWSCALEDIMAGE SECTION.

    CALL STATIC "japi_drawscaledimage"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
               BY VALUE LNK-ARG-3
               BY VALUE LNK-ARG-4
               BY VALUE LNK-ARG-5
               BY VALUE LNK-ARG-6
               BY VALUE LNK-ARG-7
               BY VALUE LNK-ARG-8
               BY VALUE LNK-ARG-9
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-DRAWSCALEDIMAGE-EX.
    EXIT.
 END FUNCTION J-DRAWSCALEDIMAGE.


*>------------------------------------------------------------------------------
*> void j_setcolor( int arg0, int arg1, int arg2, int arg3)
*> { japi_setcolor(arg0, arg1, arg2, arg3);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETCOLOR.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                    RETURNING          LNK-RET.

 MAIN-J-SETCOLOR SECTION.

    CALL STATIC "japi_setcolor"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
               BY VALUE LNK-ARG-3
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETCOLOR-EX.
    EXIT.
 END FUNCTION J-SETCOLOR.


*>------------------------------------------------------------------------------
*> void j_setcolorbg( int arg0, int arg1, int arg2, int arg3)
*> { japi_setcolorbg(arg0, arg1, arg2, arg3);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETCOLORBG.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-ARG-3                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                          BY VALUE     LNK-ARG-3
                    RETURNING          LNK-RET.

 MAIN-J-SETCOLORBG SECTION.

    CALL STATIC "japi_setcolorbg"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
               BY VALUE LNK-ARG-2
               BY VALUE LNK-ARG-3
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETCOLORBG-EX.
    EXIT.
 END FUNCTION J-SETCOLORBG.


*>------------------------------------------------------------------------------
*> void j_setnamedcolor( int arg0, int arg1)
*> { japi_setnamedcolor(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETNAMEDCOLOR.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETNAMEDCOLOR SECTION.

    CALL STATIC "japi_setnamedcolor"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETNAMEDCOLOR-EX.
    EXIT.
 END FUNCTION J-SETNAMEDCOLOR.


*>------------------------------------------------------------------------------
*> void j_setnamedcolorbg( int arg0, int arg1)
*> { japi_setnamedcolorbg(arg0, arg1);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SETNAMEDCOLORBG.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY VALUE     LNK-ARG-1
                    RETURNING          LNK-RET.

 MAIN-J-SETNAMEDCOLORBG SECTION.

    CALL STATIC "japi_setnamedcolorbg"
         USING BY VALUE LNK-ARG-0
               BY VALUE LNK-ARG-1
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SETNAMEDCOLORBG-EX.
    EXIT.
 END FUNCTION J-SETNAMEDCOLORBG.


*>------------------------------------------------------------------------------
*> int  j_loadimage( char* arg0)
*> { return( japi_loadimage(arg0));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-LOADIMAGE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          PIC X ANY LENGTH.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY REFERENCE LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-LOADIMAGE SECTION.

    CALL STATIC "japi_loadimage"
         USING BY CONTENT CONCATENATE(TRIM(LNK-ARG-0), X"00")
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-LOADIMAGE-EX.
    EXIT.
 END FUNCTION J-LOADIMAGE.


*>------------------------------------------------------------------------------
*> int    j_saveimage( int arg0, char* arg1, int arg2)
*> { return( japi_saveimage(arg0, arg1, arg2));  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SAVEIMAGE.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-ARG-1                          PIC X ANY LENGTH.
 01 LNK-ARG-2                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                          BY REFERENCE LNK-ARG-1
                          BY VALUE     LNK-ARG-2
                    RETURNING          LNK-RET.

 MAIN-J-SAVEIMAGE SECTION.

    CALL STATIC "japi_saveimage"
         USING BY VALUE   LNK-ARG-0
               BY CONTENT CONCATENATE(TRIM(LNK-ARG-1), X"00")
               BY VALUE   LNK-ARG-2
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-SAVEIMAGE-EX.
    EXIT.
 END FUNCTION J-SAVEIMAGE.


*>------------------------------------------------------------------------------
*> void j_sync( )
*> { japi_sync();  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SYNC.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION
                    RETURNING          LNK-RET.

 MAIN-J-SYNC SECTION.

    CALL STATIC "japi_sync"
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SYNC-EX.
    EXIT.
 END FUNCTION J-SYNC.


*>------------------------------------------------------------------------------
*> void j_beep( )
*> { japi_beep();  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-BEEP.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION
                    RETURNING          LNK-RET.

 MAIN-J-BEEP SECTION.

    CALL STATIC "japi_beep"
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-BEEP-EX.
    EXIT.
 END FUNCTION J-BEEP.


*>------------------------------------------------------------------------------
*> int  j_random( )
*> { return( japi_random());  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-RANDOM.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION
                    RETURNING          LNK-RET.

 MAIN-J-RANDOM SECTION.

    CALL STATIC "japi_random"
         RETURNING LNK-RET
    END-CALL

    GOBACK

    .
 MAIN-J-RANDOM-EX.
    EXIT.
 END FUNCTION J-RANDOM.


*>------------------------------------------------------------------------------
*> void j_sleep( int arg0)
*> { japi_sleep(arg0);  }
*>------------------------------------------------------------------------------
 IDENTIFICATION DIVISION.
 FUNCTION-ID. J-SLEEP.
 AUTHOR.      Laszlo Erdos.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
    FUNCTION ALL INTRINSIC.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 LINKAGE SECTION.
 01 LNK-ARG-0                          BINARY-INT.
 01 LNK-RET                            BINARY-INT.

 PROCEDURE DIVISION USING BY VALUE     LNK-ARG-0
                    RETURNING          LNK-RET.

 MAIN-J-SLEEP SECTION.

    CALL STATIC "japi_sleep"
         USING BY VALUE LNK-ARG-0
         RETURNING OMITTED
    END-CALL

    MOVE ZEROES TO LNK-RET

    GOBACK

    .
 MAIN-J-SLEEP-EX.
    EXIT.
 END FUNCTION J-SLEEP.
