DECLARE SUB lzwdecode (lzwaddr%, palbits%, imgwidth%)
DECLARE SUB setpalette (palbits%)
DECLARE FUNCTION getfileinfo& ()
DEFINT A-Z

' GIF decoder. Uses SCREEN 13.

' blank/blazko/doom/doom16/keen/wolf/wolf16
CONST file = "gif\keen.gif"

' make sure file exists (otherwise OPEN...BINARY would create it)
ON ERROR GOTO notfound
OPEN file FOR INPUT AS #1: CLOSE
ON ERROR GOTO 0

OPEN file FOR BINARY ACCESS READ AS #1

' validate Header, Logical Screen Descriptor, Image Descriptor;
' get (palbits * &H10000 + lzwbits * &H1000 + imgwidth)
temp& = getfileinfo&
palbits = temp& \ &H10000
lzwbits = (temp& \ &H1000) AND &HF
imgwidth = temp& AND &HFFF

PRINT
PRINT "Press Esc to quit, any other key to draw."
PRINT "(During or after drawing, press Esc to quit.)"
k$ = INPUT$(1): IF k$ = CHR$(27) THEN END

SCREEN 13
CALL setpalette(palbits)
starttime! = TIMER
CALL lzwdecode(14 + 2 ^ palbits * 3 + 11, lzwbits, imgwidth)
CLOSE

decodetime = TIMER - starttime!
DO: k$ = INPUT$(1): LOOP UNTIL k$ = CHR$(27)

SCREEN 0
WIDTH 80
PRINT "Decoded in"; decodetime; "seconds."
END

notfound:
PRINT "File not found. (Edit the 'file' constant at the beginning.)"
END

FUNCTION getfileinfo&
' Validate Header, Logical Screen Descriptor and Image Descriptor.
' Exit on error. Otherwise return:
'     palette_bit_depth (1...8)   * &H10000
'     + lzw_bit_depth   (2...8)   * &H1000
'     + image_width     (1...320)

' Header (6 bytes) and Logical Screen Descriptor (7 bytes)
headlsd$ = SPACE$(13)
GET #1, 1, headlsd$
IF LEFT$(headlsd$, 3) <> "GIF" THEN PRINT "Not a GIF file.": END
ver$ = MID$(headlsd$, 4, 3)
IF ver$ <> "87a" AND ver$ <> "89a" THEN PRINT "Warning: unknown GIF version"
' get Global Color Table bit depth (1...8) from packed fields byte
packedfields = ASC(MID$(headlsd$, 11, 1))
IF packedfields AND &H80 = 0 THEN
    PRINT "Files without Global Color Table are not supported.": END
END IF
palbits = (packedfields AND 7) + 1

' Image Descriptor (10 bytes) and palette bit depth in LZW encoding (1 byte)
imgaddr = 14 + 2 ^ palbits * 3
imgdesc$ = SPACE$(11)
GET #1, imgaddr, imgdesc$
IF LEFT$(imgdesc$, 1) <> "," THEN
    PRINT "Images without an Image Descriptor immediately after ";
    PRINT "Global Color Table are not supported.": END
END IF
imgwidth = CVI(MID$(imgdesc$, 6, 2))
imgheight = CVI(MID$(imgdesc$, 8, 2))
packedfields = ASC(MID$(imgdesc$, 10, 1))
lzwbits = ASC(MID$(imgdesc$, 11, 1))
IF lzwbits < 2 OR lzwbits > 11 THEN PRINT "Invalid LZW bit depth.": END

IF EOF(1) THEN PRINT "Unexpected EOF.": END

CLS
PRINT "Width            :"; imgwidth
PRINT "Height           :"; imgheight
PRINT "Palette bit depth:"; palbits
PRINT "LZW     bit depth:"; lzwbits

IF imgwidth < 1 OR imgwidth > 320 THEN PRINT "Unsupported width.": END
IF imgheight < 1 OR imgheight > 200 THEN PRINT "Unsupported height.": END
IF lzwbits > 8 THEN PRINT "Unsupported LZW bit depth.": END
IF packedfields AND &H80 THEN PRINT "Local Color Table not supported.": END
IF packedfields AND &H40 THEN PRINT "Interlace not supported.": END

getfileinfo& = palbits * &H10000 + lzwbits * &H1000& + imgwidth

END FUNCTION

SUB lzwdecode (lzwaddr, palbits, imgwidth)
' Decode and draw LZW-encoded image data.
' lzwaddr: LZW data start, palbits: bit depth, imgwidth: width

DIM pow2(12)          ' powers of two (faster than using the "^" operator)
DIM dictrefs(4095)    ' LZW dictionary - reference to another entry
DIM dictbytes(4095)   ' LZW dictionary - final byte
DIM entrypixels(356)  ' reconstructed LZW dictionary entry

' constants
FOR i = 0 TO 12: pow2(i) = 2 ^ i: NEXT
clearcode = pow2(palbits)    ' LZW code: reset dictionary
endcode = pow2(palbits) + 1  ' LZW code: stop decoding

filepos& = lzwaddr           ' byte position in file
lzw$ = ""                    ' ca. one subblock of LZW data
p = 1                        ' byte index in lzw$
b = 0                        ' number of bits read from byte (0...7)
codelen = palbits + 1        ' current length of LZW codes (3...12)
maxdictsize = pow2(codelen)  ' max LZW dict size for current codelen
andmask = pow2(codelen) - 1  ' LZW code AND mask for current codelen
prevcode = -1                ' nonnegative=dict entry, -1=not dict entry
dictsize = 0                 ' number of entries in LZW dictionary

DO
    IF p > LEN(lzw$) - 2 THEN
        ' possibly not enough data left for next LZW code
        ' discard used LZW data
        lzw$ = MID$(lzw$, p)
        p = 1
        ' read subblocks until there's enough data
        ' TODO: check for unexpected EOF or end of LZW subblocks
        WHILE LEN(lzw$) < 3
            ' get subblock size
            c$ = SPACE$(1)
            GET #1, filepos&, c$
            filepos& = filepos& + 1
            sbsize = ASC(c$)
            ' read subblock
            c$ = SPACE$(sbsize)
            GET #1, filepos&, c$
            lzw$ = lzw$ + c$
            filepos& = filepos& + sbsize
        WEND
    END IF

    ' read next code from remaining data
    ' combine 1...3 bytes in reverse order, e.g. 0xab 0xcd -> 0xcdab
    codelng& = ASC(MID$(lzw$, p, 1))
    IF b + codelen > 8 THEN
        codelng& = codelng& OR ASC(MID$(lzw$, p + 1, 1)) * &H100&
        IF b + codelen > 16 THEN
            codelng& = codelng& OR ASC(MID$(lzw$, p + 2, 1)) * &H10000
        END IF
    END IF
    ' delete unnecessary bits from start and end
    code = codelng& \ pow2(b) AND andmask

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
        maxdictsize = pow2(codelen)
        andmask = pow2(codelen) - 1
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

        IF dictsize = maxdictsize AND codelen < 12 THEN
            codelen = codelen + 1
            maxdictsize = pow2(codelen)
            andmask = pow2(codelen) - 1
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
            ' this PSET takes almost all the time in this sub
            PSET (pixx, pixy), entrypixels(i)
            pixx = pixx + 1
            IF pixx = imgwidth THEN pixx = 0: pixy = pixy + 1
        NEXT
    END IF

    IF INKEY$ = CHR$(27) THEN SCREEN 0: WIDTH 80: END
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

IF EOF(1) THEN SCREEN 0: WIDTH 80: PRINT "Unexpected EOF.": END

END SUB

