
_tldr: Option 3 below will convert all of the example projects to run on Mac Mseries_

---
# Background

For M-series Macs (M1, M2, etc. aka Apple Silicon or arm64 or aarch64) the included examples won't work without doing a few small things.

These **one-time** changes will probably also need to be done to any other example projects you get off the internet or any projects you created in the past. 

### What needs to be done in plain english

In the project file (`<`Project Name`>`.lpi) there are two options that need to be set:
```
   -framework IOKit
```
 and
```
   -WM11.0
```

In the main .lpr or .pas file in the project folder add this to the uses list:
```
CocoaAll,
```

# How to make these changes

There are three options:
1. Manually change one project/example at a time **(most informative)**
2. Commands to change one project/example at a time
3. Commands to change all of the examples at once **(easiest)**

### Option 1 - Manually change one project/example at a time
 Edit the `<`Project Name`>`.lpi file of a project with a text editor
 
1. Find this near the bottom of the file 
```
      </Linking>
  </CompilerOptions> 
```
 and replace it with 
```
      <Options>
        <PassLinkerOptions Value="True"/>
        <LinkerOptions Value="&apos;-framework IOKit&apos;"/>
      </Options>
    </Linking>
    <Other>
      <CustomOptions Value="&apos;-WM11.0&apos;"/>
    </Other>
  </CompilerOptions>
```

2. Edit the main `.lpr` or `.pas` file of the project with a text editor find the uses section and add ` CocoaAll` to that section.  OR you can load the project in Lazarus and change it in the `.lpr` or `.pas` file from there.  hint: you will probably only need to change the `.pas` file if there is no `.lpr` file.

### Option 2 - Use a command to change one project/example at a time

Run these commands in the folder of the project you wish to change:

```
sed -i '' '/<\/Linking>/,/<\/CompilerOptions>/c\
     <Options>\
        <PassLinkerOptions Value="True"/>\
        <LinkerOptions Value="&apos;-framework IOKit&apos;"/>\
      </Options>\
    </Linking>\
    <Other>\
      <CustomOptions Value="&apos;-WM11.0&apos;"/>\
    </Other>\
  </CompilerOptions>' *.lpi
sed -i '' '/^uses/ s/^uses/& CocoaAll,/' *.lpr
sed -i '' '/^uses/ s/^uses/& CocoaAll,/' *.pas

```

### Option 3 - Use a command to change all of the examples at once

Run these commands from the _root folder of the repo_ to make these changes to every project in the examples folder.
```
find examples -type f -name "*.lpi" -print0 | xargs -0 sed -i '' '/<\/Linking>/,/<\/CompilerOptions>/c\
     <Options>\
        <PassLinkerOptions Value="True"/>\
        <LinkerOptions Value="&apos;-framework IOKit&apos;"/>\
      </Options>\
    </Linking>\
    <Other>\
      <CustomOptions Value="&apos;-WM11.0&apos;"/>\
    </Other>\
  </CompilerOptions>'
find examples -type f -name "*.lpr" -print0 | xargs -0 sed -i '' '/^uses/ s/^\(uses\)/\1 CocoaAll,/'
find examples -type f -name "*.pas" -print0 | xargs -0 sed -i '' '/^uses/ s/^\(uses\)/\1 CocoaAll,/'
```
##### Note about running this twice
If you run this twice then you will probably have to edit the .lpr files manually to remove the extra `CocoAll,` which is easy and not a big deal.  The compiler will tell you about it if you run it twice and don't notice.


# FAQ
## Q: What if this doesn't work? 
After doing one of the options above you should be able to run the examples by choosing one and loading that projects `.lpi` file into Lazarus.  At that point select the menu option: `Run->Run Without Debugging` and the example should fire right up.   You should be able to tweak the examples to your hearts content and it should all work.

This should work for all of the included example projects but if a few of them don't work then a little manual investigation into that project should solve it.
1. The main pascal file should have `CocoaAll` in its uses section.   Usually its a `.lpr` file but sometimes it might be  `.pas` file.
2. The .lpi file should have the `-WM11` and the `-framework IOKit` options added near the end
3. I [made a video about this process](https://youtu.be/h2-GrChtwMY?si=QbW_rwmVjI7JGDvP&t=341) since it could help to see me do it.
4. If you need to redownload the examples [from the github](https://github.com/GuvaCode/Ray4Laz) I don't blame you, I had to do that several times to make this :)
5. You can use Option 2 above to try again on just one project. 
## Q: What about the `macosx_version_min` error?
This is not a problem wth Ray4Laz.  This is a small problem with the build of the Free Pascal Compiler on Mac Mseries and can *safely be ignored*.  It shows up as an error but I have seen no ill effects and everything I have done has worked in spite of this error showing up in every build.
#### What can you do to fix the macosx_version_min_error?
This is a small problem related to the fpc compiler that you can either *safely ignore* or you can fix it in your own local build by changing just one character in a file if you want to. [I made a video about fixing this](https://www.youtube.com/watch?v=0otHOdNNuaE) which in a nutshell involves editing `lazarus/fpcsrc/compiler/systems/t_darwin.pas` and in that file changing `macosx_version_min` to `macos_version_min` and recompiling fpc.  Yes It is literally just a one character change.  That should fix it until you update fpc from source in which case you will have to do this small fix again.
## Q: Do any of the examples not work on Mac Mseries?
1. `examples/shaders/shaders_custom_uniform` compiles fine but doesn't seem to run.  Can you figure out why?
	Clues: Once you have compiled it if you go into the ray4laz/binary folder you can run the compiled version and see what error you get.  I got `ERROR: 0:46: Use of undeclared identifier 'color'` among others.
	
