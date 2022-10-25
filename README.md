# qbgif
GIF decoder in Microsoft QBasic. Based on my [Python GIF decoder/encoder](https://github.com/qalle2/pygif).

Limitations:
* size: 320&times;200 pixels or less
* only the first image in the file will be decoded
* the image must use the Global Color Table (not a Local Color Table)
* there must be no extra fields before the first image

Some of the included GIF files are from *Commander Keen 4: Secret of the Oracle*, *Doom* and *Wolfenstein 3D* by id Software.
