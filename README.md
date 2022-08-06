# Twitch-Code
This repository holds all of the code I have made or shown on stream at twitch.tv/nondairyneutrino

## How to use the code

All of the code that is meant to be worked with is in the _Wolfram Language Package_ format, as noted by the ".wl" file extension.  These files are in this format for 2 main reasons

1. While there just a few extra steps on your end to use the code, this method provides the most consistent user experience to anyone that uses these files.

2. The code will be easily readable even when viewed in a plain text editor like the one in GitHub, Atom, or even Notepad.  This way those without access to Mathematica are able to at least have their hands on something.  This also lends itself very nicely to being manipulated by Git.

Since the code is in this _package_ format, once you have the code (either by copy+paste or through Git) **you need to load the package into your _Mathematica_ session**.  The shortest way to do this is by evaluating

```Mathematica  
<< [LOCATOIN OF PACKAGE]
```

in you _Mathematica_ notebook.  Once this is done, the functions that are defined (along with there informative descriptions which can be found with the `?` function!) in the package are now available to be used in the notebook like any other function.
