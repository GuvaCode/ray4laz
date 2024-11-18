**ray4laz is a header translation of the [raylib game development Library](https://www.raylib.com/) for the [Lazarus project](https://www.lazarus-ide.org/).**

---

<img align="left" src="binary/resources/raylogo.png" width="256px">

[raylib](https://github.com/raysan5/raylib) is a simple and easy-to-use library to enjoy videogames programming.

raylib is highly inspired by Borland BGI graphics lib and by XNA framework and it's specially well suited for prototyping, tooling, graphical applications, embedded systems and education.

<br>
 NOTE for ADVENTURERS: raylib is a programming library to enjoy videogames programming; no fancy interface, no visual helpers, no auto-debugging... just coding in the most pure spartan-programmers way.


---

<br>

features
--------
  - **NO external dependencies**, all required libraries are [bundled into raylib](https://github.com/raysan5/raylib/tree/master/src/external)
  - Multiple platforms supported: **Windows, Linux, MacOS, RPI, Android, HTML5... and more!**
  - Written in plain C code (C99) using PascalCase/camelCase notation
  - Hardware accelerated with OpenGL (**1.1, 2.1, 3.3, 4.3 or ES 2.0**)
  - **Unique OpenGL abstraction layer** (usable as standalone module): [rlgl](https://github.com/GuvaCode/Ray4Laz/blob/main/source/rlgl.pas)
  - Multiple **Fonts** formats supported (TTF, Image fonts, AngelCode fonts)
  - Multiple texture formats supported, including **compressed formats** (DXT, ETC, ASTC)
  - **Full 3D support**, including 3D Shapes, Models, Billboards, Heightmaps and more! 
  - Flexible Materials system, supporting classic maps and **PBR maps**
  - **Animated 3D models** supported (skeletal bones animation) (IQM, M3D, glTF)
  - Shaders support, including model and **postprocessing** shaders.
  - **Powerful math module** for Vector, Matrix and Quaternion operations: [raymath](https://github.com/GuvaCode/Ray4Laz/blob/main/source/raymath.pas)
  - Audio loading and playing with streaming support (WAV, QOA, OGG, MP3, FLAC, XM, MOD)
  - **VR stereo rendering** support with configurable HMD device parameters
  - Huge examples collection with [+120 code examples](https://github.com/GuvaCode/Ray4Laz/tree/main/examples)!
  - Bindings to [+60 programming languages](https://github.com/raysan5/raylib/blob/master/BINDINGS.md)!
  - **Free and open source**.


supported headers
--------

Header     | Supported          |
---------  | ------------------ |
raylib.h   | :heavy_check_mark: |
raymath.h  | :heavy_check_mark: |
rlgl.h     | :heavy_check_mark: |
raygui.h   | :heavy_check_mark: |

tested platforms
--------

OS         | Supported          |
---------  | ------------------ |
MacOS*     | :heavy_check_mark: |
Windows    | :heavy_check_mark: |
Linux      | :heavy_check_mark: |
Haiku      | :heavy_check_mark: |

\* to compile examples for the Apple m-series, see the [m-series readme](README_mac_mseries.md) or watch this [video](https://www.youtube.com/watch?v=h2-GrChtwMY)


build and installation raylib
--------

raylib binary releases for Windows, Linux and macOS are available at the [Github Releases page](https://github.com/raysan5/raylib/releases),and also in the ray4laz/libs folder.

building raylib on multiple platforms
--------

[raylib Wiki](https://github.com/raysan5/raylib/wiki#development-platforms) contains detailed instructions on building and usage on multiple platforms.

 - [Working on Windows](https://github.com/raysan5/raylib/wiki/Working-on-Windows)
 - [Working on macOS](https://github.com/raysan5/raylib/wiki/Working-on-macOS)
 - [Working on GNU Linux](https://github.com/raysan5/raylib/wiki/Working-on-GNU-Linux)
 - [Working on FreeBSD](https://github.com/raysan5/raylib/wiki/Working-on-FreeBSD)
 - [Working on Raspberry Pi](https://github.com/raysan5/raylib/wiki/Working-on-Raspberry-Pi)
 - [Working for Android](https://github.com/raysan5/raylib/wiki/Working-for-Android)
 - [Working for Web (HTML5)](https://github.com/raysan5/raylib/wiki/Working-for-Web-(HTML5))
 - [Working anywhere with CMake](https://github.com/raysan5/raylib/wiki/Working-with-CMake)
 - [CMake Build Options](https://github.com/raysan5/raylib/wiki/CMake-Build-Options)


installation in lazarus ide. 
--------

select the package menu, open the network package manager. 

![](https://raw.githubusercontent.com/GuvaCode/GuvaCode/main/ray4laz_img/1_openpkg.png)


select ray4laz package and install. 

![](https://raw.githubusercontent.com/GuvaCode/GuvaCode/main/ray4laz_img/3_opm.png)

create a new project (Ray Simple Project). 

![](https://raw.githubusercontent.com/GuvaCode/GuvaCode/main/ray4laz_img/2_newproject.png)

enjoy !!!

![](https://raw.githubusercontent.com/GuvaCode/GuvaCode/main/ray4laz_img/4_enjoy.png)



manual installation and use without lazarus ide
--------

clone this repository, then use 'fpc-wrapper.sh' or 'fpc-wrapper.bat' as your compiler. this script automatically feeds fpc with the necessary arguments.


usage examples from other developers
--------

- [Raylib Platformer Game Created with Lazarus and Raster Master](https://www.youtube.com/watch?v=DhdHi7fPkhk)
- [Sonic The Hedgehog physics in Object Pascal (Raylib, Ray4laz)](https://www.youtube.com/watch?v=3PAmUILrFGw&t=101s)


other examples are rewritten from other programming languages
--------
[Ray4LazExample](https://github.com/GuvaCode/Ray4LazExample)


contributors
--------

<a href="https://github.com/GuvaCode/Ray4Laz/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=GuvaCode/Ray4Laz" />
</a>

