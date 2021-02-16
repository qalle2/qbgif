DECLARE SUB lzwdecode (imgaddr%, palbits%, imgwidth%)
DECLARE SUB setpalette (palbits%)
DEFINT A-Z

' GIF decoder. Uses SCREEN 13.

CLS
OPEN "image\wolf4.gif" FOR BINARY ACCESS READ AS #1

' Header
c$ = SPACE$(6)
GET #1, 1, c$
IF LEFT$(c$, 3) <> "GIF" THEN PRINT "Not a GIF file.": END

' Logical Screen Descriptor (get GCT bit depth from packed fields byte)
c$ = SPACE$(1)
GET #1, 11, c$
IF ASC(c$) AND &H80 = 0 THEN PRINT "No GCT.": END
palbits = (ASC(c$) AND 7) + 1

' Image Descriptor

imgaddr = 14 + 2 ^ palbits * 3
c$ = SPACE$(1)
GET #1, imgaddr, c$
IF c$ <> "," THEN PRINT "No Image Descriptor after GCT.": END

c$ = SPACE$(2)
GET #1, imgaddr + 5, c$
imgwidth = CVI(c$)
IF imgwidth = 0 OR imgwidth > 320 THEN PRINT "Invalid width.": END

c$ = SPACE$(2)
GET #1, imgaddr + 7, c$
imgheight = CVI(c$)
IF imgwidth = 0 OR imgheight > 200 THEN PRINT "Invalid height.": END

c$ = SPACE$(1)
GET #1, imgaddr + 9, c$
IF ASC(c$) AND &H80 THEN PRINT "LCT not supported.": END
IF ASC(c$) AND &H40 THEN PRINT "Interlace not supported.": END

c$ = SPACE$(1)
GET #1, imgaddr + 10, c$
lzwbits = ASC(c$)
IF lzwbits < 2 OR lzwbits > 8 THEN PRINT "Invalid LZW bit depth.": END

SCREEN 13
PRINT "Width            :"; imgwidth
PRINT "Height           :"; imgheight
PRINT "Palette bit depth:"; palbits
PRINT "LZW     bit depth:"; lzwbits
PRINT
PRINT "Esc to quit, any other key to draw"
k$ = INPUT$(1)
IF k$ = CHR$(27) THEN END

CLS
CALL setpalette(palbits)
CALL lzwdecode(imgaddr + 11, lzwbits, imgwidth)
CLOSE

k$ = INPUT$(1)

SUB lzwdecode (imgaddr, palbits, imgwidth)
' Decode and draw LZW-encoded image data.
' TODO: better error handling (EOF etc.)
' imgaddr: LZW data start, palbits: bit depth, imgwidth: width

' read LZW data from subblocks
p = imgaddr  ' byte position in file
lzw$ = ""  ' LZW data
DO
    ' subblock size
    c$ = SPACE$(1)
    GET #1, p, c$
    p = p + 1
    ' data
    sbsize = ASC(c$)
    IF sbsize = 0 THEN EXIT DO
    c$ = SPACE$(sbsize)
    GET #1, p, c$
    lzw$ = lzw$ + c$
    p = p + sbsize
LOOP

' Trailer must follow
GET #1, p, c$
IF c$ <> ";" THEN PRINT "Missing trailer.": END

' powers of two
DIM pow2(16)
FOR i = 0 TO 12
    pow2(i) = 2 ^ i
NEXT

clearcode = pow2(palbits)    ' LZW clear code
endcode = pow2(palbits) + 1  ' LZW end code

p = 1                  ' byte index being read in LZW data
b = 0                  ' number of bits read from byte
codelen = palbits + 1  ' current length of LZW codes (3...12)
prevcode = -1          ' nonnegative=dict entry, -1 = not dict entry

DIM dictrefs(4095)   ' reference to another entry
DIM dictbytes(4095)  ' final byte
dictsize = 0

' dict entry is reconstructed here before drawing (you may need to increase)
DIM entrypixels(999)

DO
    ' read next code from remaining data
    ' combine 1...3 bytes in reverse order, e.g. 0xab 0xcd -> 0xcdab
    codelng& = ASC(MID$(lzw$, p, 1))
    IF b + codelen > 8 THEN
        codelng& = codelng& OR ASC(MID$(lzw$, p + 1, 1)) * &H100&
        IF b + codelen > 16 THEN
            codelng& = codelng& OR ASC(MID$(lzw$, p + 2, 1)) * &H10000
        END IF
    END IF
    codelng& = codelng& \ pow2(b)           ' delete bits from end
    code = codelng& AND (pow2(codelen) - 1) ' delete bits from start

    ' advance in data
    b = b + codelen
    p = p + b \ 8
    b = b AND 7

    IF code = clearcode THEN
        ' reset dictionary and code length
        dictsize = pow2(palbits) + 2
        FOR i = 0 TO dictsize - 1
            dictrefs(i) = -1
            dictbytes(i) = i AND &HFF  ' doesn't matter for clear/end code
        NEXT
        codelen = palbits + 1
        prevcode = -1
    ELSEIF code = endcode THEN
        EXIT DO
    ELSE
        ' dictionary entry

        IF prevcode <> -1 THEN
            ' add new entry (prev entry + 1st byte of curr/prev entry)
            IF code < dictsize THEN
                suffixcode = code
            ELSE
                suffixcode = prevcode
            END IF
            WHILE suffixcode <> -1
                suffixbyte = dictbytes(suffixcode)
                suffixcode = dictrefs(suffixcode)
            WEND
            dictrefs(dictsize) = prevcode
            dictbytes(dictsize) = suffixbyte
            dictsize = dictsize + 1
            prevcode = -1
        END IF

        IF code < dictsize < 4096 THEN
            ' prepare to add a dictionary entry
            prevcode = code
        END IF

        IF dictsize = pow2(codelen) AND codelen < 12 THEN
            codelen = codelen + 1
        END IF

        ' get pixels of entry by traversing dictionary
        entrylen = 0
        WHILE code <> -1
            entrypixels(entrylen) = dictbytes(code)
            code = dictrefs(code)
            entrylen = entrylen + 1
        WEND
        ' draw entry in reverse
        FOR i = entrylen - 1 TO 0 STEP -1
            PSET (pixx, pixy), entrypixels(i)
            pixx = pixx + 1
            IF pixx = imgwidth THEN pixx = 0: pixy = pixy + 1
        NEXT
    END IF

    IF INKEY$ = CHR$(27) THEN EXIT DO
LOOP

END SUB

SUB setpalette (palbits)

c$ = SPACE$(3)
p = 14  ' position in file (start of Global Color Table)

' Note: each palette entry in GIF is red, green, blue.
' We append a zero byte and use CVL() to get the following bits:
'     BBBBBBbb GGGGGGgg RRRRRRrr
'     (B/G/R = important bits, b/g/r = bits to discard)
' Important bits in 18-bit VGA color number:
'     00BBBBBB 00GGGGGG 00RRRRRR

FOR i = 0 TO 2 ^ palbits - 1
    GET #1, p, c$
    PALETTE i, CVL(c$ + CHR$(0)) \ 4 AND &H3F3F3F
    p = p + 3
NEXT

END SUB

