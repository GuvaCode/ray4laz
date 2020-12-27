
**Ray4Laz is a header translation of the [Raylib Game Development Library](https://www.raylib.com/) for the [Lazarus project](https://www.lazarus-ide.org/).**

**[raylib](https://github.com/raysan5/raylib) is a simple and easy-to-use library to enjoy videogames programming.**

raylib is highly inspired by Borland BGI graphics lib and by XNA framework and it's specially well suited for prototyping, tooling, graphical applications, embedded systems and education.

*NOTE for ADVENTURERS: raylib is a programming library to enjoy videogames programming; no fancy interface, no visual helpers, no auto-debugging... just coding in the most pure spartan-programmers way.*




**Bindings**
Header     | Supported          |
---------  | ------------------ |
raylib.h   | :heavy_check_mark: |
raymath.h  | :heavy_check_mark: |

**Platforms**
OS         | Supported          |
---------  | ------------------ |
Mac        | ❓ no tested|
Windows    | ❓ no tested|
Linux      | :heavy_check_mark: |



**Build**

You can execute `make` on GNU+Linux, macOS and Windows.

**Shared or Static Library**

You will need to source [raylib](https://github.com/raysan5/raylib/) for your platform.

- [Working on GNU Linux](https://github.com/raysan5/raylib/wiki/Working-on-GNU-Linux)
- [Working on macOS](https://github.com/raysan5/raylib/wiki/Working-on-macOS)
- [Working on Windows](https://github.com/raysan5/raylib/wiki/Working-on-Windows)

**Compiled and install package**

<div class="Box Box--condensed mt-3">
      <div>
          <div class="d-flex flex-justify-between flex-items-center py-1 py-md-2 Box-body px-2">
            <a href="/raysan5/raylib/releases/download/3.5.0/raylib-3.5.0_android_api29_arm64.tar.gz" rel="nofollow" class="d-flex flex-items-center min-width-0">
              <svg class="octicon octicon-package flex-shrink-0 text-gray" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M8.878.392a1.75 1.75 0 00-1.756 0l-5.25 3.045A1.75 1.75 0 001 4.951v6.098c0 .624.332 1.2.872 1.514l5.25 3.045a1.75 1.75 0 001.756 0l5.25-3.045c.54-.313.872-.89.872-1.514V4.951c0-.624-.332-1.2-.872-1.514L8.878.392zM7.875 1.69a.25.25 0 01.25 0l4.63 2.685L8 7.133 3.245 4.375l4.63-2.685zM2.5 5.677v5.372c0 .09.047.171.125.216l4.625 2.683V8.432L2.5 5.677zm6.25 8.271l4.625-2.683a.25.25 0 00.125-.216V5.677L8.75 8.432v5.516z"></path></svg>
              <span class="pl-2 flex-auto min-width-0 text-bold">raylib-3.5.0_android_api29_arm64.tar.gz</span>
            </a>
            <small class="pl-2 text-gray flex-shrink-0">1 MB</small>
          </div>
          <div class="d-flex flex-justify-between flex-items-center py-1 py-md-2 Box-body px-2">
            <a href="/raysan5/raylib/releases/download/3.5.0/raylib-3.5.0_android_api29_x86_64.tar.gz" rel="nofollow" class="d-flex flex-items-center min-width-0">
              <svg class="octicon octicon-package flex-shrink-0 text-gray" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M8.878.392a1.75 1.75 0 00-1.756 0l-5.25 3.045A1.75 1.75 0 001 4.951v6.098c0 .624.332 1.2.872 1.514l5.25 3.045a1.75 1.75 0 001.756 0l5.25-3.045c.54-.313.872-.89.872-1.514V4.951c0-.624-.332-1.2-.872-1.514L8.878.392zM7.875 1.69a.25.25 0 01.25 0l4.63 2.685L8 7.133 3.245 4.375l4.63-2.685zM2.5 5.677v5.372c0 .09.047.171.125.216l4.625 2.683V8.432L2.5 5.677zm6.25 8.271l4.625-2.683a.25.25 0 00.125-.216V5.677L8.75 8.432v5.516z"></path></svg>
              <span class="pl-2 flex-auto min-width-0 text-bold">raylib-3.5.0_android_api29_x86_64.tar.gz</span>
            </a>
            <small class="pl-2 text-gray flex-shrink-0">1.13 MB</small>
          </div>
          <div class="d-flex flex-justify-between flex-items-center py-1 py-md-2 Box-body px-2">
            <a href="/raysan5/raylib/releases/download/3.5.0/raylib-3.5.0_linux_amd64.tar.gz" rel="nofollow" class="d-flex flex-items-center min-width-0">
              <svg class="octicon octicon-package flex-shrink-0 text-gray" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M8.878.392a1.75 1.75 0 00-1.756 0l-5.25 3.045A1.75 1.75 0 001 4.951v6.098c0 .624.332 1.2.872 1.514l5.25 3.045a1.75 1.75 0 001.756 0l5.25-3.045c.54-.313.872-.89.872-1.514V4.951c0-.624-.332-1.2-.872-1.514L8.878.392zM7.875 1.69a.25.25 0 01.25 0l4.63 2.685L8 7.133 3.245 4.375l4.63-2.685zM2.5 5.677v5.372c0 .09.047.171.125.216l4.625 2.683V8.432L2.5 5.677zm6.25 8.271l4.625-2.683a.25.25 0 00.125-.216V5.677L8.75 8.432v5.516z"></path></svg>
              <span class="pl-2 flex-auto min-width-0 text-bold">raylib-3.5.0_linux_amd64.tar.gz</span>
            </a>
            <small class="pl-2 text-gray flex-shrink-0">1.16 MB</small>
          </div>
          <div class="d-flex flex-justify-between flex-items-center py-1 py-md-2 Box-body px-2">
            <a href="/raysan5/raylib/releases/download/3.5.0/raylib-3.5.0_linux_i386.tar.gz" rel="nofollow" class="d-flex flex-items-center min-width-0">
              <svg class="octicon octicon-package flex-shrink-0 text-gray" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M8.878.392a1.75 1.75 0 00-1.756 0l-5.25 3.045A1.75 1.75 0 001 4.951v6.098c0 .624.332 1.2.872 1.514l5.25 3.045a1.75 1.75 0 001.756 0l5.25-3.045c.54-.313.872-.89.872-1.514V4.951c0-.624-.332-1.2-.872-1.514L8.878.392zM7.875 1.69a.25.25 0 01.25 0l4.63 2.685L8 7.133 3.245 4.375l4.63-2.685zM2.5 5.677v5.372c0 .09.047.171.125.216l4.625 2.683V8.432L2.5 5.677zm6.25 8.271l4.625-2.683a.25.25 0 00.125-.216V5.677L8.75 8.432v5.516z"></path></svg>
              <span class="pl-2 flex-auto min-width-0 text-bold">raylib-3.5.0_linux_i386.tar.gz</span>
            </a>
            <small class="pl-2 text-gray flex-shrink-0">599 KB</small>
          </div>
          <div class="d-flex flex-justify-between flex-items-center py-1 py-md-2 Box-body px-2">
            <a href="/raysan5/raylib/releases/download/3.5.0/raylib-3.5.0_macos.tar.gz" rel="nofollow" class="d-flex flex-items-center min-width-0">
              <svg class="octicon octicon-package flex-shrink-0 text-gray" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M8.878.392a1.75 1.75 0 00-1.756 0l-5.25 3.045A1.75 1.75 0 001 4.951v6.098c0 .624.332 1.2.872 1.514l5.25 3.045a1.75 1.75 0 001.756 0l5.25-3.045c.54-.313.872-.89.872-1.514V4.951c0-.624-.332-1.2-.872-1.514L8.878.392zM7.875 1.69a.25.25 0 01.25 0l4.63 2.685L8 7.133 3.245 4.375l4.63-2.685zM2.5 5.677v5.372c0 .09.047.171.125.216l4.625 2.683V8.432L2.5 5.677zm6.25 8.271l4.625-2.683a.25.25 0 00.125-.216V5.677L8.75 8.432v5.516z"></path></svg>
              <span class="pl-2 flex-auto min-width-0 text-bold">raylib-3.5.0_macos.tar.gz</span>
            </a>
            <small class="pl-2 text-gray flex-shrink-0">1.13 MB</small>
          </div>
          <div class="d-flex flex-justify-between flex-items-center py-1 py-md-2 Box-body px-2">
            <a href="/raysan5/raylib/releases/download/3.5.0/raylib-3.5.0_webassembly.zip" rel="nofollow" class="d-flex flex-items-center min-width-0">
              <svg class="octicon octicon-package flex-shrink-0 text-gray" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M8.878.392a1.75 1.75 0 00-1.756 0l-5.25 3.045A1.75 1.75 0 001 4.951v6.098c0 .624.332 1.2.872 1.514l5.25 3.045a1.75 1.75 0 001.756 0l5.25-3.045c.54-.313.872-.89.872-1.514V4.951c0-.624-.332-1.2-.872-1.514L8.878.392zM7.875 1.69a.25.25 0 01.25 0l4.63 2.685L8 7.133 3.245 4.375l4.63-2.685zM2.5 5.677v5.372c0 .09.047.171.125.216l4.625 2.683V8.432L2.5 5.677zm6.25 8.271l4.625-2.683a.25.25 0 00.125-.216V5.677L8.75 8.432v5.516z"></path></svg>
              <span class="pl-2 flex-auto min-width-0 text-bold">raylib-3.5.0_webassembly.zip</span>
            </a>
            <small class="pl-2 text-gray flex-shrink-0">353 KB</small>
          </div>
          <div class="d-flex flex-justify-between flex-items-center py-1 py-md-2 Box-body px-2">
            <a href="/raysan5/raylib/releases/download/3.5.0/raylib-3.5.0_win32_mingw-w64.zip" rel="nofollow" class="d-flex flex-items-center min-width-0">
              <svg class="octicon octicon-package flex-shrink-0 text-gray" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M8.878.392a1.75 1.75 0 00-1.756 0l-5.25 3.045A1.75 1.75 0 001 4.951v6.098c0 .624.332 1.2.872 1.514l5.25 3.045a1.75 1.75 0 001.756 0l5.25-3.045c.54-.313.872-.89.872-1.514V4.951c0-.624-.332-1.2-.872-1.514L8.878.392zM7.875 1.69a.25.25 0 01.25 0l4.63 2.685L8 7.133 3.245 4.375l4.63-2.685zM2.5 5.677v5.372c0 .09.047.171.125.216l4.625 2.683V8.432L2.5 5.677zm6.25 8.271l4.625-2.683a.25.25 0 00.125-.216V5.677L8.75 8.432v5.516z"></path></svg>
              <span class="pl-2 flex-auto min-width-0 text-bold">raylib-3.5.0_win32_mingw-w64.zip</span>
            </a>
            <small class="pl-2 text-gray flex-shrink-0">1.16 MB</small>
          </div>
          <div class="d-flex flex-justify-between flex-items-center py-1 py-md-2 Box-body px-2">
            <a href="/raysan5/raylib/releases/download/3.5.0/raylib-3.5.0_win32_msvc16.zip" rel="nofollow" class="d-flex flex-items-center min-width-0">
              <svg class="octicon octicon-package flex-shrink-0 text-gray" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M8.878.392a1.75 1.75 0 00-1.756 0l-5.25 3.045A1.75 1.75 0 001 4.951v6.098c0 .624.332 1.2.872 1.514l5.25 3.045a1.75 1.75 0 001.756 0l5.25-3.045c.54-.313.872-.89.872-1.514V4.951c0-.624-.332-1.2-.872-1.514L8.878.392zM7.875 1.69a.25.25 0 01.25 0l4.63 2.685L8 7.133 3.245 4.375l4.63-2.685zM2.5 5.677v5.372c0 .09.047.171.125.216l4.625 2.683V8.432L2.5 5.677zm6.25 8.271l4.625-2.683a.25.25 0 00.125-.216V5.677L8.75 8.432v5.516z"></path></svg>
              <span class="pl-2 flex-auto min-width-0 text-bold">raylib-3.5.0_win32_msvc16.zip</span>
            </a>
            <small class="pl-2 text-gray flex-shrink-0">1.91 MB</small>
          </div>
          <div class="d-flex flex-justify-between flex-items-center py-1 py-md-2 Box-body px-2">
            <a href="/raysan5/raylib/releases/download/3.5.0/raylib-3.5.0_win64_mingw-w64.zip" rel="nofollow" class="d-flex flex-items-center min-width-0">
              <svg class="octicon octicon-package flex-shrink-0 text-gray" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M8.878.392a1.75 1.75 0 00-1.756 0l-5.25 3.045A1.75 1.75 0 001 4.951v6.098c0 .624.332 1.2.872 1.514l5.25 3.045a1.75 1.75 0 001.756 0l5.25-3.045c.54-.313.872-.89.872-1.514V4.951c0-.624-.332-1.2-.872-1.514L8.878.392zM7.875 1.69a.25.25 0 01.25 0l4.63 2.685L8 7.133 3.245 4.375l4.63-2.685zM2.5 5.677v5.372c0 .09.047.171.125.216l4.625 2.683V8.432L2.5 5.677zm6.25 8.271l4.625-2.683a.25.25 0 00.125-.216V5.677L8.75 8.432v5.516z"></path></svg>
              <span class="pl-2 flex-auto min-width-0 text-bold">raylib-3.5.0_win64_mingw-w64.zip</span>
            </a>
            <small class="pl-2 text-gray flex-shrink-0">1.05 MB</small>
          </div>
          <div class="d-flex flex-justify-between flex-items-center py-1 py-md-2 Box-body px-2">
            <a href="/raysan5/raylib/releases/download/3.5.0/raylib-3.5.0_win64_msvc16.zip" rel="nofollow" class="d-flex flex-items-center min-width-0">
              <svg class="octicon octicon-package flex-shrink-0 text-gray" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M8.878.392a1.75 1.75 0 00-1.756 0l-5.25 3.045A1.75 1.75 0 001 4.951v6.098c0 .624.332 1.2.872 1.514l5.25 3.045a1.75 1.75 0 001.756 0l5.25-3.045c.54-.313.872-.89.872-1.514V4.951c0-.624-.332-1.2-.872-1.514L8.878.392zM7.875 1.69a.25.25 0 01.25 0l4.63 2.685L8 7.133 3.245 4.375l4.63-2.685zM2.5 5.677v5.372c0 .09.047.171.125.216l4.625 2.683V8.432L2.5 5.677zm6.25 8.271l4.625-2.683a.25.25 0 00.125-.216V5.677L8.75 8.432v5.516z"></path></svg>
              <span class="pl-2 flex-auto min-width-0 text-bold">raylib-3.5.0_win64_msvc16.zip</span>
            </a>
            <small class="pl-2 text-gray flex-shrink-0">1.97 MB</small>
          </div>
          <div class="d-flex flex-justify-between flex-items-center py-1 py-md-2 Box-body px-2">
            <a href="/raysan5/raylib/releases/download/3.5.0/raylib_installer_v350.mingw.exe" rel="nofollow" class="d-flex flex-items-center min-width-0">
              <svg class="octicon octicon-package flex-shrink-0 text-gray" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M8.878.392a1.75 1.75 0 00-1.756 0l-5.25 3.045A1.75 1.75 0 001 4.951v6.098c0 .624.332 1.2.872 1.514l5.25 3.045a1.75 1.75 0 001.756 0l5.25-3.045c.54-.313.872-.89.872-1.514V4.951c0-.624-.332-1.2-.872-1.514L8.878.392zM7.875 1.69a.25.25 0 01.25 0l4.63 2.685L8 7.133 3.245 4.375l4.63-2.685zM2.5 5.677v5.372c0 .09.047.171.125.216l4.625 2.683V8.432L2.5 5.677zm6.25 8.271l4.625-2.683a.25.25 0 00.125-.216V5.677L8.75 8.432v5.516z"></path></svg>
              <span class="pl-2 flex-auto min-width-0 text-bold">raylib_installer_v350.mingw.exe</span>
            </a>
            <small class="pl-2 text-gray flex-shrink-0">103 MB</small>
          </div>
          <div class="d-flex flex-justify-between flex-items-center py-1 py-md-2 Box-body px-2">
            <a href="/raysan5/raylib/releases/download/3.5.0/raylib_installer_v350.tcc.exe" rel="nofollow" class="d-flex flex-items-center min-width-0">
              <svg class="octicon octicon-package flex-shrink-0 text-gray" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M8.878.392a1.75 1.75 0 00-1.756 0l-5.25 3.045A1.75 1.75 0 001 4.951v6.098c0 .624.332 1.2.872 1.514l5.25 3.045a1.75 1.75 0 001.756 0l5.25-3.045c.54-.313.872-.89.872-1.514V4.951c0-.624-.332-1.2-.872-1.514L8.878.392zM7.875 1.69a.25.25 0 01.25 0l4.63 2.685L8 7.133 3.245 4.375l4.63-2.685zM2.5 5.677v5.372c0 .09.047.171.125.216l4.625 2.683V8.432L2.5 5.677zm6.25 8.271l4.625-2.683a.25.25 0 00.125-.216V5.677L8.75 8.432v5.516z"></path></svg>
              <span class="pl-2 flex-auto min-width-0 text-bold">raylib_installer_v350.tcc.exe</span>
            </a>
            <small class="pl-2 text-gray flex-shrink-0">39.2 MB</small>
          </div>


          <div class="d-block py-1 py-md-2 Box-body px-2">
            <a href="/raysan5/raylib/archive/3.5.0.zip" rel="nofollow" class="d-flex flex-items-center">
              <svg class="octicon octicon-file-zip flex-shrink-0 text-gray" width="16" height="16" viewBox="0 0 16 16" version="1.1" aria-hidden="true"><path fill-rule="evenodd" d="M3.5 1.75a.25.25 0 01.25-.25h3a.75.75 0 000 1.5h.5a.75.75 0 000-1.5h2.086a.25.25 0 01.177.073l2.914 2.914a.25.25 0 01.073.177v8.586a.25.25 0 01-.25.25h-.5a.75.75 0 000 1.5h.5A1.75 1.75 0 0014 13.25V4.664c0-.464-.184-.909-.513-1.237L10.573.513A1.75 1.75 0 009.336 0H3.75A1.75 1.75 0 002 1.75v11.5c0 .649.353 1.214.874 1.515a.75.75 0 10.752-1.298.25.25 0 01-.126-.217V1.75zM8.75 3a.75.75 0 000 1.5h.5a.75.75 0 000-1.5h-.5zM6 5.25a.75.75 0 01.75-.75h.5a.75.75 0 010 1.5h-.5A.75.75 0 016 5.25zm2 1.5A.75.75 0 018.75 6h.5a.75.75 0 010 1.5h-.5A.75.75 0 018 6.75zm-1.25.75a.75.75 0 000 1.5h.5a.75.75 0 000-1.5h-.5zM8 9.75A.75.75 0 018.75 9h.5a.75.75 0 010 1.5h-.5A.75.75 0 018 9.75zm-.75.75a1.75 1.75 0 00-1.75 1.75v3c0 .414.336.75.75.75h2.5a.75.75 0 00.75-.75v-3a1.75 1.75 0 00-1.75-1.75h-.5zM7 12.25a.25.25 0 01.25-.25h.5a.25.25 0 01.25.25v2.25H7v-2.25z"></path></svg>
              <span class="px-1 text-bold">Source code</span> (zip)
            </a>
          </div>
          <div class="d-block py-1 py-md-2 Box-body px-2">
            <a href="/raysan5/raylib/archive/3.5.0.tar.gz" rel="nofollow" class="d-flex flex-items-center">
              <svg class="octicon octicon-file-zip flex-shrink-0 text-gray" width="16" height="16" viewBox="0 0 16 16" version="1.1" aria-hidden="true"><path fill-rule="evenodd" d="M3.5 1.75a.25.25 0 01.25-.25h3a.75.75 0 000 1.5h.5a.75.75 0 000-1.5h2.086a.25.25 0 01.177.073l2.914 2.914a.25.25 0 01.073.177v8.586a.25.25 0 01-.25.25h-.5a.75.75 0 000 1.5h.5A1.75 1.75 0 0014 13.25V4.664c0-.464-.184-.909-.513-1.237L10.573.513A1.75 1.75 0 009.336 0H3.75A1.75 1.75 0 002 1.75v11.5c0 .649.353 1.214.874 1.515a.75.75 0 10.752-1.298.25.25 0 01-.126-.217V1.75zM8.75 3a.75.75 0 000 1.5h.5a.75.75 0 000-1.5h-.5zM6 5.25a.75.75 0 01.75-.75h.5a.75.75 0 010 1.5h-.5A.75.75 0 016 5.25zm2 1.5A.75.75 0 018.75 6h.5a.75.75 0 010 1.5h-.5A.75.75 0 018 6.75zm-1.25.75a.75.75 0 000 1.5h.5a.75.75 0 000-1.5h-.5zM8 9.75A.75.75 0 018.75 9h.5a.75.75 0 010 1.5h-.5A.75.75 0 018 9.75zm-.75.75a1.75 1.75 0 00-1.75 1.75v3c0 .414.336.75.75.75h2.5a.75.75 0 00.75-.75v-3a1.75 1.75 0 00-1.75-1.75h-.5zM7 12.25a.25.25 0 01.25-.25h.5a.25.25 0 01.25.25v2.25H7v-2.25z"></path></svg>
              <span class="px-1 text-bold">Source code</span> (tar.gz)
            </a>
          </div>
      </div>
    </div>

