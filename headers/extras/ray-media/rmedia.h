/***************************************************************************************************
*
*   LICENSE: zlib
*
*   Copyright (c) 2024 Claudio Z. (@cloudofoz)
*
*   This software is provided "as-is," without any express or implied warranty. In no event
*   will the authors be held liable for any damages arising from the use of this software.
*
*   Permission is granted to anyone to use this software for any purpose, including commercial
*   applications, and to alter and redistribute it freely, subject to the following restrictions:
*
*     1. The origin of this software must not be misrepresented; you must not claim that you
*     wrote the original software. If you use this software in a product, an acknowledgment
*     in the product documentation would be appreciated but is not required.
*
*     2. Altered source versions must be plainly marked as such and must not be misrepresented
*     as being the original software.
*
*     3. This notice may not be removed or altered from any source distribution.
*
***************************************************************************************************/

#ifndef RAYMEDIA_H
#define RAYMEDIA_H

//--------------------------------------------------------------------------------------------------
// Includes
//--------------------------------------------------------------------------------------------------

#include <raylib.h>
#include <stdint.h>

//--------------------------------------------------------------------------------------------------
// Structures Definition
//--------------------------------------------------------------------------------------------------

typedef struct MediaContext MediaContext;    // Context holding implementation data

/**
 * Stores video and/or audio data from a movie file.
 * Usage:
 *      1. Initialize with LoadMedia() / LoadMediaEx()
 *      2. Call UpdateMedia() each frame
 *      3. Access videoTexture and audioStream for playback
 *      4. Free with UnloadMedia()
 */
typedef struct MediaStream
{
    Texture       videoTexture;      // Current video frame texture (if available)
    AudioStream   audioStream;       // Audio stream for playback (if available)
    MediaContext* ctx;               // Internal use only
} MediaStream;

/**
 * Holds MediaStream properties.
 * Use GetMediaProperties() to retrieve details.
 */
typedef struct MediaProperties
{
    double durationSec;              // Media duration in seconds
    float  avgFPS;                   // Average video FPS
    bool   hasVideo;                 // True if video is present
    bool   hasAudio;                 // True if audio is present
} MediaProperties;

/**
 * Holds the data needed to implement a custom stream reader.
 * Used to define custom read and seek behaviors for media input streams.
 */
typedef struct MediaStreamReader
{
    int     (*readFn) (void* userData, uint8_t* buffer, int bufferSize);    // Custom read function pointer
    int64_t (*seekFn) (void* userData, int64_t offset, int whence);         // Custom seek function pointer (optional, can be NULL)
    void* userData; // Pointer to user-defined context, passed to the callback functions
} MediaStreamReader;


//--------------------------------------------------------------------------------------------------
// Enumerators Definition
//--------------------------------------------------------------------------------------------------

/**
 * Flags for LoadMediaEx() to customize media loading.
 */
typedef enum
{
    MEDIA_LOAD_AV           = 0,      // Load audio and video (default)
    MEDIA_LOAD_NO_AUDIO     = 1 << 1, // Do not load audio
    MEDIA_LOAD_NO_VIDEO     = 1 << 2, // Do not load video
    MEDIA_FLAG_LOOP         = 1 << 3, // Loop playback
    MEDIA_FLAG_NO_AUTOPLAY  = 1 << 4  // Load without starting playback
} MediaLoadFlag;

/**
 * Possible states of a MediaStream.
 * Use SetMediaState() to change the MediaStream's state.
 */
typedef enum
{
    MEDIA_STATE_INVALID     = -1,     // Not loaded or initialized
    MEDIA_STATE_STOPPED,              // Stopped
    MEDIA_STATE_PAUSED,               // Paused
    MEDIA_STATE_PLAYING               // Playing (call UpdateMedia() each frame)
} MediaState;

/**
 * Global properties flags for media configuration.
 * Use SetMediaFlag() and GetMediaFlag() to configure.
 * @note: Affects MediaStreams loaded after setting.
 */
typedef enum
{
    MEDIA_IO_BUFFER,                  // Size of the buffer used for media IO operations (only applies to custom read callbacks)
    MEDIA_VIDEO_QUEUE,                // Video packet queue capacity
    MEDIA_AUDIO_QUEUE,                // Audio packet queue capacity
    MEDIA_AUDIO_DECODED_BUFFER,       // Maximum decoded audio buffer size
    MEDIA_AUDIO_STREAM_BUFFER,        // Audio stream buffer size
    MEDIA_AUDIO_FORMAT,               // Output audio format (refer to MediaAudioFormat)
    MEDIA_AUDIO_CHANNELS,             // Number of audio channels
    MEDIA_VIDEO_MAX_DELAY,            // Maximum delay (ms) before discarding a video packet
    MEDIA_AUDIO_MAX_DELAY,            // Maximum delay (ms) before discarding an audio packet
    MEDIA_AUDIO_UPDATE                // Max bytes uploaded to AudioStream per frame
} MediaConfigFlag;

/**
 * Supported audio formats for AudioStream output.
 * Configured using SetMediaFlag(MEDIA_AUDIO_FORMAT, AUDIO_FMT_*).
 * @note Increasing output format quality may require adjustments
 * to audio buffer capacities via SetMediaFlag.
 */
typedef enum
{
    AUDIO_FMT_U8 = 0,                 // Unsigned 8-bit
    AUDIO_FMT_S16 = 1,                // Signed 16-bit (default)
    AUDIO_FMT_S32 = 2,                // Signed 32-bit
    AUDIO_FMT_FLT = 3,                // Float
    AUDIO_FMT_DBL = 4                 // Double
} MediaAudioFormat;

/**
 * Status values for MediaStreamReader callback functions.
 * These values indicate the outcome of custom IO operations.
 */
typedef enum
{
    MEDIA_IO_EOF     = -541478725, // End of the stream reached (matches AVERROR_EOF)
    MEDIA_IO_INVALID = -22         // Invalid call or operation (matches AVERROR(EINVAL))
} MediaStreamIOResult;


//--------------------------------------------------------------------------------------------------
// MediaStream API
//--------------------------------------------------------------------------------------------------

#if defined(__cplusplus)
extern "C" {
#endif

    //----------------------------------------------------------------------------------------------

    /**
     * Load a MediaStream from a file.
     * @param fileName Path to the movie file
     * @return MediaStream on success; empty structure on failure
     */
    RLAPI MediaStream LoadMedia(const char* fileName);

    /**
     * Load a MediaStream from a file with flags.
     * @param fileName Path to the movie file
     * @param flags Combination of MediaLoadFlag values
     * @return MediaStream on success; empty structure on failure
     */
    RLAPI MediaStream LoadMediaEx(const char* fileName, int flags);

    /**
     * Load a MediaStream from a custom stream with flags.
     * @param streamReader A valid MediaStreamReader with callback functions and context
     * @param flags Combination of MediaLoadFlag values
     * @return MediaStream on success; empty structure on failure
     */
    RLAPI MediaStream LoadMediaFromStream(MediaStreamReader streamReader, int flags);

    /**
     * Check if a MediaStream is valid (loaded and initialized).
     * @param media MediaStream structure
     * @return true if media is valid; false otherwise
     */
    RLAPI bool IsMediaValid(MediaStream media);

    /**
     * Retrieve properties of the loaded media.
     * @param media A valid MediaStream
     * @return Filled MediaProperties structure on success; empty structure on failure
     */
    RLAPI MediaProperties GetMediaProperties(MediaStream media);

    /**
     * Update a MediaStream.
     * @param media Pointer to a valid MediaStream
     * @return true on success; false otherwise
     */
    RLAPI bool UpdateMedia(MediaStream* media);

    /**
     * Update a MediaStream with a specified deltaTime.
     * @param media Pointer to a valid MediaStream
     * @param deltaTime Time in seconds since the last update
     * @return true on success; false otherwise
     */
    RLAPI bool UpdateMediaEx(MediaStream* media, double deltaTime);

    /**
     * Get the state of a MediaStream.
     * @param media A valid MediaStream
     * @return Current state; MEDIA_STATE_INVALID on failure
     */
    RLAPI int GetMediaState(MediaStream media);

    /**
     * Set the state of a MediaStream (play, pause, or stop).
     * @param media A valid MediaStream
     * @param newState Desired state
     * @return The new state on success; MEDIA_STATE_INVALID on failure
     */
    RLAPI int SetMediaState(MediaStream media, int newState);

    /**
     * Get the playback position of a MediaStream in seconds.
     * @param media A valid MediaStream
     * @return Playback position in seconds; negative on failure
     */
    RLAPI double GetMediaPosition(MediaStream media);

    /**
     * Set the playback position of a MediaStream.
     * @param media A valid MediaStream
     * @param timeSec Desired position in seconds
     * @return true on success; false otherwise
     */
    RLAPI bool SetMediaPosition(MediaStream media, double timeSec);

    /**
     * Enable or disable loop playback for a MediaStream.
     * @param media A valid MediaStream
     * @param loopPlay true to enable looping; false to disable
     * @return true on success; false otherwise
     */
    RLAPI bool SetMediaLooping(MediaStream media, bool loopPlay);

    /**
     * Set a global configuration property.
     * @param flag One of MediaConfigFlag values
     * @param value New property value
     * @return 0 on success; -1 on failure
     */
    RLAPI int SetMediaFlag(int flag, int value);

    /**
     * Get a global configuration property.
     * @param flag One of MediaConfigFlag values
     * @return Property value; negative on failure
     */
    RLAPI int GetMediaFlag(int flag);

    /**
     * Unload a MediaStream and free its associated memory.
     * @param media Pointer to a valid MediaStream
     */
    RLAPI void UnloadMedia(MediaStream* media);

    //----------------------------------------------------------------------------------------------

#if defined(__cplusplus)
}
#endif

//--------------------------------------------------------------------------------------------------

#endif  // RAYMEDIA_H