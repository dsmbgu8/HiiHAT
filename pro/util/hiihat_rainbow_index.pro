;+  
; Takes an index between 0 and 1 and maps it to a rainbow spectra that
; returns an appropriate 24 bit integer representation in RGB space.
; Ensure you have loadct,0 active (grey scale) for sanity.
;
; :Author: David Ray Thompson
;
; :Categories: util
;
; :Params:
;  index, in, required, type=float
;   value in [0,1] to map to RGB
;
; :Keywords:
;  black: in, optional, type=boolean
;   Add black to the below_red region of the matrix
;  white: in, optional, type=boolean
;   Add white to the above_purple region of the matrix
;
; :Returns:
;   24 bit integer representation in RGB space.
; :Copyright:
;  Copyright 2009, by the California Institute of Technology. ALL RIGHTS
;  RESERVED. United States Government Sponsorship acknowledged. Any commercial
;  use must be negotiated with the Office of Technology Transfer at the
;  California Institute of Technology.
; 
;  This software may be subject to U.S. export control laws and regulations.  By
;  accepting this document, the user agrees to comply with all applicable U.S.
;  export laws and regulations.  User has the responsibility to obtain export
;  licenses, or other export authority as may be required before exporting such
;  information to foreign countries or providing access to foreign persons.
;-
function hiihat_rainbow_index, index, BLACK = black, WHITE = white

    ;This is a Matlab-style color array with RGB between 0 and 1
    ;Will need to adapt this to IDL standards in a moment
    colormap=[ $
               [1.0,0.0,  0],$ ;red
               [1.0,0.5,  0],$ ;orange
               [1.0,1.0,  0],$ ;yellow
               [0.5,1.0,  0],$ ;puce
               [0.0,1.0,  0],$ ;green
               [0.0,0.75,0.75],$ ;cyan
               [0.0,0.0,  1],$ ;blue
               [0.5,0.0,  1],$ ;violet
               [1.0,0.0,  1] $ ;purple
             ]
    
    ;Add white to the above_purple region of the matrix
    if keyword_set(white) then begin
        colormap = [[colormap],[[1.0,1.0,1.0]]]
    end
    
    ;Add black to the below_red region of the matrix
    if keyword_set(black) then begin
        colormap = [[[0.0,0.0,0.0]],[colormap]]
    end

    ;;Now we have to figure out where the
    ;;index specifies we are in terms of
    ;;this new colormap object

    ;How many colors do we have anyway?
    temp = size(colormap)
    n_colors = temp[2]

    ;Generates index's position relative to the number of colors present
    expanded_index=(n_colors-1)*index

    leftcolor  = min  ([n_colors-1, floor(expanded_index)])
    rightcolor = min  ([n_colors-1, leftcolor+1.0        ])

    rightweight=expanded_index  - leftcolor
    leftweight =1.0             - rightweight

;    print,expanded_index,rightweight,leftweight,leftcolor,rightcolor

    ;calculate the resulting rgb using 0-1 notation
    r=colormap[0,leftcolor]*leftweight+colormap[0,rightcolor]*rightweight
    g=colormap[1,leftcolor]*leftweight+colormap[1,rightcolor]*rightweight
    b=colormap[2,leftcolor]*leftweight+colormap[2,rightcolor]*rightweight

    ;map the rgb to 0-255 notation
    r=long(r*255.0)
    g=long(g*255.0)
    b=long(b*255.0)

;    print, index, expanded_index, n_colors-1, 'rgb', r, g, b, floor(r+g*256L+b*65536L)make


    ;Return the 24 bit integer version
    return,floor(r+g*256L+b*65536L)

end  
