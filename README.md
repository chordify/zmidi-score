### Segmenting MidiFiles by Repeating Patterns

## Executable
The executable is called `midiseg` and has the following options:

```
usage: midiseg [options]
  -m,--mode <mode>          The operation mode (print|segment|reppat)
  -v,--view <view>          The view/feature 
                            (ioi|contour|pc|pitch|interval|ioirat)
  [-r,--rep <rep>]          Repeition kind (max|supermax). Default: max
  [-p,--overlap <overlap>]  Remove overlapping repetitions (no|yes) Default: no
  [-l,--length <length>]    Minimal repetition length Default: 0
  [-o,--out <filepath>]     Output directory for the profile files
  [-f,--file <filepath>]    Input file midi file to analyse
  [-d,--dir <filepath>]     Base directory path to test or train
```

 