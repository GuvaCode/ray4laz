unit raymedia;
(*
  Copyright (c) 2025 Claudio Z. (@cloudofoz)
  https://github.com/cloudofoz/raylib-media
  pascal header translation 2024 gunko vadim
*)

{$mode objfpc}{$H+}
{$I raylib.inc}

interface

uses
  raylib;

{$IFNDEF RAY_STATIC}
const
  rMediaName =
    {$IFDEF WINDOWS} 'libraymedia.dll'; {$IFEND}
    {$IFDEF LINUX} 'libraymedia.so'; {$IFEND}
{$ENDIF}

type
  PMediaContext = ^TMediaContext;
  TMediaContext = pointer; //record end;

  PMediaStream = ^TMediaStream;
  TMediaStream = record
    videoTexture: TTexture;      // Current video frame texture (if available)
    audioStream: TAudioStream;   // Audio stream for playback (if available)
    ctx: PMediaContext;  //????         // Internal use only
  end;

  TMediaProperties = record
    durationSec: double;   // Media duration in seconds
    avgFPS: single;        // Average video FPS
    hasVideo: boolean;     // True if video is present
    hasAudio: boolean;     // True if audio is present
  end;

type
  TReadFn = function(userData: Pointer; buffer: PByte; bufferSize: Integer): Integer; cdecl;
  TSeekFn = function(userData: Pointer; offset: Int64; whence: Integer): Int64; cdecl;

  PMediaStreamReader = ^TMediaStreamReader;
  TMediaStreamReader = record
    readFn: TReadFn;
    seekFn: TSeekFn;
    userData: Pointer;
  end;

  //Flags for LoadMediaEx() to customize media loading.
  PMediaLoadFlag = ^TMediaLoadFlag;
  TMediaLoadFlag = Integer;
  const
    MEDIA_LOAD_AV           = TMediaLoadFlag(0);      // Load audio and video (default)
    MEDIA_LOAD_NO_AUDIO     = TMediaLoadFlag(1 shl 1); // Do not load audio
    MEDIA_LOAD_NO_VIDEO     = TMediaLoadFlag(1 shl 2); // Do not load video
    MEDIA_FLAG_LOOP         = TMediaLoadFlag(1 shl 3); // Loop playback
    MEDIA_FLAG_NO_AUTOPLAY  = TMediaLoadFlag(1 shl 4); // Load without starting playback

type
  PMediaState = ^TMediaState;
  TMediaState = Integer;
  const
    MEDIA_STATE_INVALID     = TMediaState(-1);     // Not loaded or initialized
    MEDIA_STATE_STOPPED     = TMediaState(0);      // Stopped
    MEDIA_STATE_PAUSED      = TMediaState(1);      // Paused
    MEDIA_STATE_PLAYING     = TMediaState(2);      // Playing (call UpdateMedia() each frame)

type
  PMediaConfigFlag = ^TMediaConfigFlag;
  TMediaConfigFlag = Integer;
  const
    MEDIA_VIDEO_QUEUE           = TMediaConfigFlag(0);  // Video packet queue capacity
    MEDIA_AUDIO_QUEUE           = TMediaConfigFlag(1);  // Audio packet queue capacity
    MEDIA_AUDIO_DECODED_BUFFER  = TMediaConfigFlag(2);  // Maximum decoded audio buffer size
    MEDIA_AUDIO_STREAM_BUFFER   = TMediaConfigFlag(3);  // Audio stream buffer size
    MEDIA_AUDIO_FORMAT          = TMediaConfigFlag(4);  // Output audio format (refer to MediaAudioFormat)
    MEDIA_AUDIO_CHANNELS        = TMediaConfigFlag(5);  // Number of audio channels
    MEDIA_VIDEO_MAX_DELAY       = TMediaConfigFlag(6);  // Maximum delay (ms) before discarding a video packet
    MEDIA_AUDIO_MAX_DELAY       = TMediaConfigFlag(7);  // Maximum delay (ms) before discarding an audio packet
    MEDIA_AUDIO_UPDATE          = TMediaConfigFlag(8);  // Max bytes uploaded to AudioStream per frame

type
  PMediaAudioFormat = ^TMediaAudioFormat;
  TMediaAudioFormat = Integer;
  const
    AUDIO_FMT_U8  = TMediaAudioFormat(0);  // Unsigned 8-bit
    AUDIO_FMT_S16 = TMediaAudioFormat(1);  // Signed 16-bit (default)
    AUDIO_FMT_S32 = TMediaAudioFormat(2);  // Signed 32-bit
    AUDIO_FMT_FLT = TMediaAudioFormat(3);  // Float
    AUDIO_FMT_DBL = TMediaAudioFormat(4);  // Double

  type
    TMediaStreamIOResult = (
      MEDIA_IO_EOF     = -541478725, // End of the stream reached (matches AVERROR_EOF)
      MEDIA_IO_INVALID = -22         // Invalid call or operation (matches AVERROR(EINVAL))
    );


(*
 * Load a MediaStream from a file.
 * @param fileName Path to the movie file
 * @return MediaStream on success; empty structure on failure
 *)
function LoadMedia(const fileName: PChar): TMediaStream; cdecl; external {$IFNDEF RAY_STATIC}rMediaName{$ENDIF} name 'LoadMedia';

(*
 * Load a MediaStream from a file with flags.
 * @param fileName Path to the movie file
 * @param flags Combination of MediaLoadFlag values
 * @return MediaStream on success; empty structure on failure
 *)
function LoadMediaEx(const fileName: PChar; flags: Integer): TMediaStream; cdecl; external {$IFNDEF RAY_STATIC}rMediaName{$ENDIF} name 'LoadMediaEx';

(*
 * Load a MediaStream from a custom stream with flags.
 * @param streamReader A valid MediaStreamReader with callback functions and context
 * @param flags Combination of MediaLoadFlag values
 * @return MediaStream on success; empty structure on failure
 *)
function LoadMediaFromStream(streamReader: TMediaStreamReader; flags: integer): TMediaStream; cdecl; external {$IFNDEF RAY_STATIC}rMediaName{$ENDIF} name 'LoadMediaFromStream';

(*
 * Check if a MediaStream is valid (loaded and initialized).
 * @param media MediaStream structure
 * @return true if media is valid; false otherwise
 *)
function IsMediaValid(media: TMediaStream): Boolean; cdecl; external {$IFNDEF RAY_STATIC}rMediaName{$ENDIF} name 'IsMediaValid';

(*
 * Retrieve properties of the loaded media.
 * @param media A valid MediaStream
 * @return Filled MediaProperties structure on success; empty structure on failure
 *)
function GetMediaProperties(media: TMediaStream): TMediaProperties; cdecl; external {$IFNDEF RAY_STATIC}rMediaName{$ENDIF} name 'GetMediaProperties';

(*
 * Update a MediaStream.
 * @param media Pointer to a valid MediaStream
 * @return true on success; false otherwise
 *)
function UpdateMedia(media: PMediaStream): Boolean; cdecl; external {$IFNDEF RAY_STATIC}rMediaName{$ENDIF} name 'UpdateMedia';

(*
  * Update a MediaStream with a specified deltaTime.
  * @param media Pointer to a valid MediaStream
  * @param deltaTime Time in seconds since the last update
  * @return true on success; false otherwise
  *)
function UpdateMediaEx(media: PMediaStream; deltaTime: Double): Boolean; cdecl; external {$IFNDEF RAY_STATIC}rMediaName{$ENDIF} name 'UpdateMediaEx';

(*
 * Get the state of a MediaStream.
 * @param media A valid MediaStream
 * @return Current state; MEDIA_STATE_INVALID on failure
 *)
 function SetMediaState(media: TMediaStream; newState: Integer): Integer; cdecl; external {$IFNDEF RAY_STATIC}rMediaName{$ENDIF} name 'SetMediaState';

(*
 * Set the state of a MediaStream (play, pause, or stop).
 * @param media A valid MediaStream
 * @param newState Desired state
 * @return The new state on success; MEDIA_STATE_INVALID on failure
 *)
function GetMediaState(media: TMediaStream): Integer; cdecl; external {$IFNDEF RAY_STATIC}rMediaName{$ENDIF} name 'GetMediaState';

(*
  * Get the playback position of a MediaStream in seconds.
  * @param media A valid MediaStream
  * @return Playback position in seconds; negative on failure
  *)
function GetMediaPosition(media: TMediaStream): Double; cdecl; external {$IFNDEF RAY_STATIC}rMediaName{$ENDIF} name 'GetMediaPosition';

(*
 * Set the playback position of a MediaStream.
 * @param media A valid MediaStream
 * @param timeSec Desired position in seconds
 * @return true on success; false otherwise
 *)
function SetMediaPosition(media: TMediaStream; timeSec: Double): Boolean; cdecl; external {$IFNDEF RAY_STATIC}rMediaName{$ENDIF} name 'SetMediaPosition';

(*
 * Enable or disable loop playback for a MediaStream.
 * @param media A valid MediaStream
 * @param loopPlay true to enable looping; false to disable
 * @return true on success; false otherwise
 *)
function SetMediaLooping(media: TMediaStream; loopPlay: Boolean): Boolean; cdecl; external {$IFNDEF RAY_STATIC}rMediaName{$ENDIF} name 'SetMediaLooping';

(*
 * Set a global configuration property.
 * @param flag One of MediaConfigFlag values
 * @param value New property value
 * @return 0 on success; -1 on failure
 *)
function SetMediaFlag(flag: Integer; value: Integer): Integer; cdecl; external {$IFNDEF RAY_STATIC}rMediaName{$ENDIF} name 'SetMediaFlag';

(*
 * Get a global configuration property.
 * @param flag One of MediaConfigFlag values
 * @return Property value; negative on failure
 *)
function GetMediaFlag(flag: Integer): Integer; cdecl; external {$IFNDEF RAY_STATIC}rMediaName{$ENDIF} name 'GetMediaFlag';

(*
 * Unload a MediaStream and free its associated memory.
 * @param media Pointer to a valid MediaStream
 *)
procedure UnloadMedia(media: PMediaStream); cdecl; external {$IFNDEF RAY_STATIC}rMediaName{$ENDIF} name 'UnloadMedia';

implementation

{$IFDEF linux}
{$IFDEF RAY_STATIC}
 {$linklib c}
 {$linklib m}
 {$linklib dl}
 {$linklib pthread}
 {$linklib libraymedia.a}
 {$linklib libavcodec}
 {$linklib libavformat}
 {$linklib libavutil}
 {$linklib libswresample}
 {$linklib libswscale}
{$endif}
{$endif}
end.


