 /***************************************************************************************************
 *
 *   LICENSE: zlib
 *
 *   Copyright (c) 2024 Claudio Z. (@cloudofoz)
 *
 *   This software is provided "as-is", without any express or implied warranty. In no event
 *   will the authors be held liable for any damages arising from the use of this software.
 *
 *   Permission is granted to anyone to use this software for any purpose, including commercial
 *   applications, and to alter it and redistribute it freely, subject to the following restrictions:
 *
 *     1. The origin of this software must not be misrepresented; you must not claim that you
 *     wrote the original software. If you use this software in a product, an acknowledgment
 *     in the product documentation would be appreciated but is not required.
 *
 *     2. Altered source versions must be plainly marked as such, and must not be misrepresented
 *     as being the original software.
 *
 *     3. This notice may not be removed or altered from any source distribution.
 *
 ***************************************************************************************************/

//---------------------------------------------------------------------------------------------------
// Includes
//---------------------------------------------------------------------------------------------------

#include <assert.h>

#include "raymedia.h"

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libavutil/imgutils.h>
#include <libswresample/swresample.h>
#include <libswscale/swscale.h>

//---------------------------------------------------------------------------------------------------
// Defines and Macros
//---------------------------------------------------------------------------------------------------

#define CLAMP(x, low, high) ((x) < (low) ? (low) : (x) > (high) ? (high) : (x))

#ifndef MIN
#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#endif

#ifndef MAX
#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#endif

#if defined(RAYLIB_VERSION_MAJOR) && defined(RAYLIB_VERSION_MINOR)
// Compatibility check for Raylib versions older than 5.5
#if (RAYLIB_VERSION_MAJOR < 5) || (RAYLIB_VERSION_MAJOR == 5 && RAYLIB_VERSION_MINOR < 5)
	#define IsImageValid IsImageReady
	#define IsTextureValid IsTextureReady
	#define IsAudioStreamValid IsAudioStreamReady
#endif
#else
	#error "RAYLIB_VERSION_MAJOR and RAYLIB_VERSION_MINOR must be defined"
#endif

//---------------------------------------------------------------------------------------------------
// Enumerators Definition
//---------------------------------------------------------------------------------------------------

// Supported stream types
enum {
	STREAM_AUDIO	= 0,									// Index of the audio stream
	STREAM_VIDEO	= 1,									// Index of the video stream

	STREAM_COUNT											// Number of streams
};

// Internal return codes 
enum
{	
	// Error codes, starting from MEDIA_ERR_BASE ----------------------------------------------------

	MEDIA_ERR_BASE = -11000,

	MEDIA_ERR_OVERFLOW = MEDIA_ERR_BASE - 1,				// Buffer overflow
	MEDIA_ERR_DECODE_AUDIO,									// Error decoding audio
	MEDIA_ERR_UNKNOWN_STREAM,								// Unknown or unsupported stream
	MEDIA_ERR_GRAB_PACKET,									// Error grabbing packet
	MEDIA_ERR_DUPLICATE_STREAM,								// Duplicate stream type detected
	MEDIA_ERR_CODEC_ALLOC_FAILED,							// Codec context allocation failure
	MEDIA_ERR_CTX_PARAMS_FAILED,							// Failed to copy codec parameters
	MEDIA_ERR_CODEC_OPEN_FAILED,							// Codec initialization failure


	// Success and EOF return codes -----------------------------------------------------------------

	MEDIA_RET_SUCCEED = 0,									// Operation successful
	MEDIA_EOF = 1											// End of file (stream) reached
};


//---------------------------------------------------------------------------------------------------
// Types and Structures Definition
//---------------------------------------------------------------------------------------------------

// Circular buffer logic
typedef struct BufferState
{
	int readPos;					// Read position index
	int writePos;					// Write position index
	int capacity;					// Capacity of the circular buffer
} BufferState;

// Queue structure specialized in handling pending AVPackets.
// - Two queues are used: one for video packets and one for audio packets.
// - Set queue capacities for a specific MediaStream before calling LoadMedia(), 
//   or rely on default values if not set.
// - To customize capacities, use SetMediaFlag(MEDIA_VIDEO_QUEUE, [videoQueueCapacity]) 
//   and SetMediaFlag(MEDIA_AUDIO_QUEUE, [audioQueueCapacity]).
typedef struct PacketQueue
{
	AVPacket** packets;             // Pointer to the circular buffer queue for AVPackets
	BufferState state;              // Current state of the circular buffer
} PacketQueue;

// Buffer structure
// - A single buffer is used to hold decoded audio before feeding it to the AudioStream.
// - Set the buffer capacity for a specific MediaStream before calling LoadMedia(), 
//   or rely on the default value if not set.
// - To customize capacity, use SetMediaFlag(MEDIA_AUDIO_DECODED_BUFFER, [decodedBufferCapacity]).
typedef struct Buffer
{
	uint8_t* data;                  // Pointer to the circular buffer
	BufferState state;              // Current state of the circular buffer
} Buffer;

// Library configuration structure 
// - To set a property, use SetMediaFlag([MediaLoadFlag], [value]).
// - To get a property, use GetMediaFlag([MediaLoadFlag]).
// - Changes to these settings will take effect only on MediaStreams loaded after the change.
typedef struct MediaConfig
{
	int ioBufferSize;                       // Size of the buffer for custom IO operations (in bytes)

	int videoQueueSize;						// Maximum number of pending video packets
	int audioQueueSize;						// Maximum number of pending audio packets
	int audioDecodedBufferSize;				// Size in bytes of the buffer holding decoded audio for the AudioStream
	enum AVSampleFormat audioOutputFmt;		// Output format for the audio stream
	int audioOutputChannels;				// Number of output channels for the audio stream
	double maxAllowedDelay[STREAM_COUNT];	// Maximum allowed delay (in seconds) before an Audio or Video packet is discarded
											// Late packets below this threshold are speed-up

	int audioMaxUpdateSize;					// Maximum number of bytes to be uploaded to the AudioStream in a frame
	int audioStreamBufferSize;				// Size of the AudioStream buffer
} MediaConfig;

// Audio/Video stream context data
typedef struct StreamDataContext
{
	AVCodecContext* codecCtx;       // Pointer to the codec context for this stream
	PacketQueue pendingPackets;     // Queue of pending packets, enqueued if they cannot be used immediately
	int streamIdx;                  // Index of this stream within the AVFormatContext structure
	int64_t startPts;               // Starting presentation timestamp (PTS) of the stream; AV_NOPTS_VALUE initially
} StreamDataContext;

// Structure to hold implementation-specific data for a media instance.
// This structure is presented as an opaque pointer in a MediaStream.
typedef struct MediaContext
{
	AVFormatContext* formatContext;             // Format I/O context
	StreamDataContext streams[STREAM_COUNT];    // Data associated with video/audio streams

	// Video stream-related fields
	struct SwsContext* swsContext;              // Video resampling and scaling context
	Image videoOutputImage;                     // Image buffer holding the decoded video frame, uploaded to [MediaStream].videoTexture

	// Audio stream-related fields
	struct SwrContext* swrContext;              // Audio resampling context
	Buffer audioOutputBuffer;                   // Buffer with decoded audio, used to fill the AudioStream when needed
	int audioOutputFmt;                         // Output audio format for this stream; must be an interleaved format
	int audioMaxUpdateSize;						// Maximum number of bytes to be uploaded to the AudioStream in a frame

	// libav* library-related fields
	AVPacket* avPacket;                         // AVPacket used before dispatching to the correct stream context
	AVFrame* avFrame;                           // AVFrame used for each packet during processing

	// MediaStream-related fields
	MediaState state;                           // Current state of the media. Use SetMediaState()/GetMediaState() to modify.
	double timePos;                             // Current playback position in seconds
	bool loopPlay;                              // Indicates if the media plays in a loop. Use SetMediaLooping() to set.
} MediaContext;


//---------------------------------------------------------------------------------------------------
// Global Variables Definition
//---------------------------------------------------------------------------------------------------

// Default global settings in raylib style (see documentation for MediaConfig, SetMediaFlag(), and GetMediaFlag()).
static MediaConfig MEDIA = {
	.ioBufferSize   = 4 * 1024,
	.videoQueueSize = 50,
	.audioQueueSize = 50,
	.audioDecodedBufferSize = 16 * 1024, // TODO: Fine-tune these values.
	.audioMaxUpdateSize     = 4  * 1024, // 
	.audioStreamBufferSize  = 1  * 1024, //
	.audioOutputChannels = 2,
	.audioOutputFmt = AV_SAMPLE_FMT_S16,
	.maxAllowedDelay = {0.04, 1.0}    //!IMPORTANT: Assuming here STREAM_AUDIO = 0, STREAM_VIDEO = 1
};


//---------------------------------------------------------------------------------------------------
// Functions Declaration - Circular buffer logic
//---------------------------------------------------------------------------------------------------

// State checking functions

int IsBufferFull(const BufferState* state);        // Check if the circular buffer is full (returns 1 if full, 0 otherwise)
int IsBufferEmpty(const BufferState* state);       // Check if the circular buffer is empty (returns 1 if empty, 0 otherwise)

// Space calculation functions

int GetBufferWritableSpace(const BufferState* state);         // Calculate total writable space in the buffer, in bytes (not necessarily contiguous)
int GetBufferWritableSegmentSize(const BufferState* state);   // Calculate the largest contiguous writable segment, in bytes
int GetBufferReadableSpace(const BufferState* state);         // Calculate total readable space in the buffer, in bytes (not necessarily contiguous)
int GetBufferReadableSegmentSize(const BufferState* state);   // Calculate the largest contiguous readable segment, in bytes

// Position advancing functions

void AdvanceWritePos(BufferState* state);          // Advance the write position by one byte in the circular buffer
void AdvanceWritePosN(BufferState* state, int n);  // Advance the write position by N bytes in the circular buffer
void AdvanceReadPos(BufferState* state);           // Advance the read position by one byte in the circular buffer
void AdvanceReadPosN(BufferState* state, int n);   // Advance the read position by N bytes in the circular buffer


//---------------------------------------------------------------------------------------------------
// Functions Declaration - Buffer management
//---------------------------------------------------------------------------------------------------

Buffer LoadBuffer(int capacity);                   // Load a circular buffer with the specified capacity.
void UnloadBuffer(Buffer* buffer);                 // Free memory associated with the buffer.
bool IsBufferReady(const Buffer* buffer);          // Check if the buffer is properly loaded.
void ClearBuffer(Buffer* buffer);                  // Reset the circular buffer without freeing memory, allowing for reuse.

int  WriteBuffer(Buffer* buffer, const uint8_t* srcData, int srcSize);	// Write srcData to the buffer. Returns a negative value on error; 
																		// otherwise, returns the actual size written (may be less than srcSize 
																		// if not enough writable space is available).

int  ReadBuffer(Buffer* buffer, uint8_t* dstData, int dstSize);			// Read data from the buffer into dstData. Returns a negative value on 
																		// error; otherwise, returns the actual size read (may be less than dstSize 
																		// if not enough readable space is available).


//---------------------------------------------------------------------------------------------------
// Functions Declaration - PacketQueue management
//---------------------------------------------------------------------------------------------------

PacketQueue LoadQueue(int capacity);					// Load a packet queue with the specified capacity.
void UnloadQueue(PacketQueue* queue);					// Free memory associated with the queue.
bool IsQueueReady(const PacketQueue* queue);			// Check if the queue is properly loaded.
void ClearQueue(PacketQueue* queue);					// Reset the queue without freeing memory, allowing for reuse.
bool IsQueueFull(const PacketQueue* queue);				// Check if the queue is full (no more writable space).
bool IsQueueEmpty(const PacketQueue* queue);			// Check if the queue is empty (no more readable space).

bool EnqueuePacket(PacketQueue* queue, AVPacket* src);	// Enqueue a packet. Automatically handles reference management.
														// Returns true on success, false if the queue is full.

bool DequeuePacket(PacketQueue* queue, AVPacket* dst);	// Dequeue a packet. Automatically handles reference management.
														// Returns true on success, false if the queue is empty.

AVPacket* PeekPacket(const PacketQueue* queue);			// Returns a pointer to the first available packet, or NULL if the queue is empty.
														// Does not modify the queue or advance the read position.


//---------------------------------------------------------------------------------------------------
// Functions Definition - AV management - FFmpeg (libav*)
//---------------------------------------------------------------------------------------------------

void AVPrintError(int errCode);							 // Prints a libav error code as a string.

// Grabs a packet of the specified type (STREAM_VIDEO, STREAM_AUDIO).
// First, searches the pending packets queue; if empty, it grabs and enqueues packets
// until finding one of the desired type.
int AVGrabPacket(MediaContext* ctx, int streamType, AVPacket* dst);

// Returns a ptr to a packet of the desired type keeping the reference inside the queue
int AVPeekPacket(MediaContext* ctx, int streamType, AVPacket** ptr);

// Decodes a packet grabbed with AVGrabPacket, then calls AVProcessVideoFrame or AVProcessAudioFrame 
// based on the packet type. Optionally discards the packet without processing it.
int AVDecodePacket(const MediaStream* media, int streamType, const AVPacket* packet, bool discardPacket);

// Processes a specific audio frame to provide usable data for [MediaStream].audioStream.
int AVProcessAudioFrame(const MediaStream* media);

// Processes a specific video frame to provide usable data for [MediaStream].videoTexture.
int AVProcessVideoFrame(const MediaStream* media);

// Helper for seeking to the first video keyframe in the media. Called by AVSeek after codec are flushed.
// It's used to avoid visual codec artifacts while seeking in the media.
bool AVSeekVideoKeyframe(const MediaStream* media);

// Helper for seeking to a specific position in the media (targetTimestamp in libav time units).
bool AVSeek(MediaStream* media, int64_t targetTimestamp);

// Helper for relative position seeking in the media (factor is the relative position 
// between 0.0 and 1.0) [not used].
bool AVSeekRelative(MediaStream* media, double factor);

// Helper function to load codec context data for a specific stream.
bool AVLoadCodecContext(StreamDataContext* streamCtx, const AVCodec* codec, const AVCodecParameters* params);

// Helper function to free memory associated with codec context data for a specific stream.
void AVUnloadCodecContext(StreamDataContext* streamCtx);


//---------------------------------------------------------------------------------------------------
// Functions Declaration - Media Context loading and unloading
//---------------------------------------------------------------------------------------------------

// Load a MediaContext from a file or a custom MediaStreamReader.
// - fileName: Name of the media file to load. Ignored if a valid MediaStreamReader is provided.
// - streamReader: A MediaStreamReader containing custom IO callbacks. Takes precedence over fileName if valid.
// - flags: Combination of MediaLoadFlag values to configure loading behavior.
// Returns: Pointer to the allocated MediaContext on success, or NULL on failure.
MediaContext* LoadMediaContext(const char* fileName, MediaStreamReader streamReader, int flags);

void UnloadMediaContext(MediaContext* ctx);


//---------------------------------------------------------------------------------------------------
// Functions Declaration - Helpers
//---------------------------------------------------------------------------------------------------

// Loads a MediaStream from an already initialized MediaContext.
// This internal function is shared by exposed API functions (LoadMedia, LoadMediaEx, LoadMediaFromStream),
// which differ only in how the MediaContext is initialized.
// - ctx: Pointer to an initialized MediaContext structure.
// - flags: Combination of MediaLoadFlag values to configure the loading process.
// Returns: A valid MediaStream on success; an empty MediaStream on failure.
MediaStream LoadMediaFromContext(MediaContext* ctx, int flags);

void NotifyEndOfStream(const MediaStream* media);         // Handles the end-of-stream event for the specified media.

void UpdateState(const MediaStream* media, int newState); // Helper function to update the state of the media to the specified new state.

bool HasStream(const MediaContext* ctx, int streamType);  // Checks if the media has an available VIDEO_STREAM or AUDIO_STREAM.


//---------------------------------------------------------------------------------------------------
// Functions Definition - MediaConfigFlags settings
//---------------------------------------------------------------------------------------------------

int SetMediaFlag(int flag, int value)
{
	int ret = 0;

	switch (flag)
	{
	case MEDIA_IO_BUFFER:
		MEDIA.ioBufferSize = MAX(value, 1);
		break;

	case MEDIA_VIDEO_QUEUE:
		MEDIA.videoQueueSize = MAX(value, 1);
		break;

	case MEDIA_AUDIO_QUEUE:
		MEDIA.audioQueueSize = MAX(value, 1);
		break;

	case MEDIA_AUDIO_DECODED_BUFFER:
		MEDIA.audioDecodedBufferSize = MAX(value, 1024);
		break;

	case MEDIA_AUDIO_STREAM_BUFFER:
		MEDIA.audioStreamBufferSize = MAX(value, 1024);
		break;

	case MEDIA_AUDIO_FORMAT:
		// A planar format (not interleaved) would require a different implementation
		if(!av_sample_fmt_is_planar(value)) 
		{
			MEDIA.audioOutputFmt = (enum AVSampleFormat)CLAMP(value, AV_SAMPLE_FMT_U8, AV_SAMPLE_FMT_DBL);
		}
		else 
		{
			TraceLog(LOG_WARNING, "MEDIA: Non-interleaved audio format (%i) is not supported.", value);
		}		
		break;

	case MEDIA_AUDIO_CHANNELS:
		MEDIA.audioOutputChannels = MAX(1, value);
		break;

	case MEDIA_VIDEO_MAX_DELAY:
		MEDIA.maxAllowedDelay[STREAM_VIDEO] = MAX(0, value) / 1000.0;
		break;

	case MEDIA_AUDIO_MAX_DELAY:
		MEDIA.maxAllowedDelay[STREAM_AUDIO] = MAX(0, value) / 1000.0;
		break;

	case MEDIA_AUDIO_UPDATE:
		MEDIA.audioMaxUpdateSize = MAX(value, 1024);
		break;	

	default:
		ret = -1; // Flag not recognized
		break;
	}

	return ret;
}

int GetMediaFlag(int flag)
{
	int ret = -1; 

	switch (flag)
	{
	case MEDIA_IO_BUFFER:
		ret = MEDIA.ioBufferSize;
		break;

	case MEDIA_VIDEO_QUEUE:
		ret = MEDIA.videoQueueSize;
		break;

	case MEDIA_AUDIO_QUEUE:
		ret = MEDIA.audioQueueSize;
		break;

	case MEDIA_AUDIO_DECODED_BUFFER:
		ret = MEDIA.audioDecodedBufferSize;
		break;

	case MEDIA_AUDIO_STREAM_BUFFER:
		ret = MEDIA.audioStreamBufferSize;
		break;

	case MEDIA_AUDIO_FORMAT:
		ret = MEDIA.audioOutputFmt;
		break;

	case MEDIA_AUDIO_CHANNELS:
		ret = MEDIA.audioOutputChannels;
		break;

	case MEDIA_VIDEO_MAX_DELAY:
		ret = (int)(MEDIA.maxAllowedDelay[STREAM_VIDEO] * 1000.0);
		break;

	case MEDIA_AUDIO_MAX_DELAY:
		ret = (int)(MEDIA.maxAllowedDelay[STREAM_AUDIO] * 1000.0);
		break;

	case MEDIA_AUDIO_UPDATE:
		ret = MEDIA.audioMaxUpdateSize;
		break;

	default:
		break;
	}

	return ret;
}

 //---------------------------------------------------------------------------------------------------
// Functions Definition - Media properties handling
//---------------------------------------------------------------------------------------------------

MediaProperties GetMediaProperties(MediaStream media)
{
	MediaProperties props = (MediaProperties){ 0 };

	if(IsMediaValid(media))
	{
		const AVFormatContext* fmtCtx = media.ctx->formatContext;
		assert(fmtCtx);

		const StreamDataContext* videoCtx = &media.ctx->streams[STREAM_VIDEO];
		
		props.durationSec = (double)fmtCtx->duration / AV_TIME_BASE; // av_rescale_q(fmtCtx->duration, AV_TIME_BASE_Q, (AVRational) { 1, 1000 });
		props.hasVideo	  = HasStream(media.ctx, STREAM_VIDEO); 
		props.hasAudio    = HasStream(media.ctx, STREAM_AUDIO);
		props.avgFPS      = props.hasVideo ? (float)av_q2d(fmtCtx->streams[videoCtx->streamIdx]->avg_frame_rate) : 0;
	}
	else
	{
		TraceLog(LOG_WARNING, "MEDIA: Trying to retrieve properties of an invalid media.");
	}

	return props;
}


//---------------------------------------------------------------------------------------------------
// Functions Definition - Media play management
//---------------------------------------------------------------------------------------------------


bool SetMediaPosition(MediaStream media, double timeSec)
{
	if(!IsMediaValid(media))
	{
		TraceLog(LOG_WARNING, "MEDIA: Trying to set the position of an invalid media.");
		return false;
	}

	timeSec = MAX(0, timeSec);

	return AVSeek(&media, (int64_t)(timeSec * AV_TIME_BASE));
}

double GetMediaPosition(MediaStream media)
{
	double pos = -1.0;

	if (IsMediaValid(media))
	{
		pos = media.ctx->timePos;
	}
	else
	{
		TraceLog(LOG_WARNING, "MEDIA: Trying to retrieve the position of an invalid media.");
	}

	return pos;
}

bool SetMediaLooping(MediaStream media, bool loopPlay)
{
	int ret = false;

	if (IsMediaValid(media))
	{
		media.ctx->loopPlay = loopPlay;
		ret = true;
	}
	else
	{
		TraceLog(LOG_WARNING, "MEDIA: Trying to set the looping mode of an invalid media.");
	}

	return ret;
}


//---------------------------------------------------------------------------------------------------
// Functions Definition - Media Context loading and unloading
//---------------------------------------------------------------------------------------------------

MediaContext* LoadMediaContext(const char* fileName, MediaStreamReader streamReader, int flags)
{
	MediaContext* ctx = (MediaContext*) RL_MALLOC(sizeof(MediaContext));

	*ctx = (MediaContext){ 0 };

	ctx->state = MEDIA_STATE_INVALID;

	ctx->formatContext = avformat_alloc_context();

	if (!ctx->formatContext)
	{
		TraceLog(LOG_ERROR, "MEDIA: Can't allocate AVFormatContext");
		UnloadMediaContext(ctx);
		return NULL;
	}

	// If a custom read function is provided, set up the AVIOContext for custom IO
	if (streamReader.readFn)
	{
		// Allocate buffer for AVIOContext
		unsigned char* ioBuffer = av_malloc(MEDIA.ioBufferSize);
		if (!ioBuffer)
		{
			TraceLog(LOG_ERROR, "MEDIA: Can't allocate AVIOContext buffer");
			UnloadMediaContext(ctx); // Free resources and exit on error
			return NULL;
		}

		// Allocate and initialize the AVIOContext for custom IO
		AVIOContext* avIOContext = avio_alloc_context(
			ioBuffer,                        // Buffer for IO operations
			MEDIA.ioBufferSize,              // Size of the buffer in bytes
			0,                               // Write flag (0 for read-only operations)
			streamReader.userData,           // Opaque pointer to custom stream context
			streamReader.readFn,             // Custom read function
			NULL,                            // Custom write function (NULL for read-only)
			streamReader.seekFn              // Custom seek function (NULL if not supported)
		);

		if (!avIOContext)
		{
			TraceLog(LOG_ERROR, "MEDIA: Can't allocate AVIOContext");
			av_freep(&ioBuffer);         // Free the allocated buffer on failure
			UnloadMediaContext(ctx);        // Free resources and exit on error
			return NULL;
		}

		// Assign the custom AVIOContext to the format context
		ctx->formatContext->pb = avIOContext;

		// Set the custom IO flag for the format context
		ctx->formatContext->flags |= AVFMT_FLAG_CUSTOM_IO;
	}

	int ret = avformat_open_input(&ctx->formatContext, fileName, NULL, NULL);

	if ( ret < 0) {
		AVPrintError(ret);
		UnloadMediaContext(ctx);
		return NULL;
	}

	ret = avformat_find_stream_info(ctx->formatContext, NULL);

	if (ret < 0) {
		AVPrintError(ret);
		UnloadMediaContext(ctx);
		return NULL;
	}

	for (int i = 0; i < (int)ctx->formatContext->nb_streams; i++)
	{
		const AVCodecParameters* localCodecParameters = ctx->formatContext->streams[i]->codecpar;

		const AVCodec* localCodec = avcodec_find_decoder(localCodecParameters->codec_id);

		if (localCodec == NULL) 
		{
			TraceLog(LOG_WARNING, "MEDIA: Unsupported codec.");

			continue;
		}

		if (localCodecParameters->codec_type == AVMEDIA_TYPE_VIDEO &&
			(flags & MEDIA_LOAD_NO_VIDEO) == 0)
		{
			StreamDataContext* videoCtx = &ctx->streams[STREAM_VIDEO];

			ret = AVLoadCodecContext(videoCtx, localCodec, localCodecParameters);

			if(ret == MEDIA_RET_SUCCEED)
			{
				//-------------------------------------------------------------

				videoCtx->streamIdx = -1;

				//-------------------------------------------------------------

				const AVCodecContext* codecCtx = videoCtx->codecCtx;

				//-------------------------------------------------------------

				videoCtx->pendingPackets = LoadQueue(MEDIA.videoQueueSize);

				if(!IsQueueReady(&videoCtx->pendingPackets))
				{
					TraceLog(LOG_ERROR, "MEDIA: Cannot initialize the video packet queue.");

					AVUnloadCodecContext(videoCtx);

					continue;
				}				

				//-------------------------------------------------------------

				// Video resampling and scaling context
				ctx->swsContext = sws_getContext(
					codecCtx->width, codecCtx->height, codecCtx->pix_fmt,  // Input format
					codecCtx->width, codecCtx->height, AV_PIX_FMT_RGB24,   // Output format
					SWS_BILINEAR, NULL, NULL, NULL);

				if(!ctx->swsContext)
				{
					TraceLog(LOG_ERROR, "MEDIA: Cannot initialize the SWS context.");

					AVUnloadCodecContext(videoCtx);

					continue;
				}

				//-------------------------------------------------------------

				ctx->videoOutputImage.data = RL_MALLOC(av_image_get_buffer_size(AV_PIX_FMT_RGB24, codecCtx->width, codecCtx->height, 1));

				if(!ctx->videoOutputImage.data)
				{
					TraceLog(LOG_ERROR, "MEDIA: Cannot allocate memory for holding the decoded frame.");

					AVUnloadCodecContext(videoCtx);

					continue;
				}

				ctx->videoOutputImage.width   = codecCtx->width;
				ctx->videoOutputImage.height  = codecCtx->height;
				ctx->videoOutputImage.mipmaps = 1;
				ctx->videoOutputImage.format  = PIXELFORMAT_UNCOMPRESSED_R8G8B8;

				ImageClearBackground(&ctx->videoOutputImage, BLANK),

				//-------------------------------------------------------------

				videoCtx->streamIdx = i;

				//-------------------------------------------------------------
			}
			else
			{
				TraceLog(LOG_WARNING, "MEDIA: Cannot initialize the video codec. (Error code: %i)", ret);				
			}

			TraceLog(LOG_DEBUG, "Media Debug: '%s' - Video Codec: resolution %d x %d", fileName, localCodecParameters->width, localCodecParameters->height);
		}
		else if (localCodecParameters->codec_type == AVMEDIA_TYPE_AUDIO &&
			(flags & MEDIA_LOAD_NO_AUDIO) == 0) 
		{

			if(!IsAudioDeviceReady())
			{
				TraceLog(LOG_WARNING, "MEDIA: '%s' - Audio Codec: raylib audio device is not initialized. Audio will be skipped.");
				continue;
			}

			StreamDataContext* audioCtx = &ctx->streams[STREAM_AUDIO];

			ret = AVLoadCodecContext(audioCtx, localCodec, localCodecParameters);

			if (ret == MEDIA_RET_SUCCEED)
			{
				//-------------------------------------------------------------

				const AVCodecContext* codecCtx = audioCtx->codecCtx;

				//-------------------------------------------------------------

				audioCtx->pendingPackets = LoadQueue(MEDIA.audioQueueSize);

				if (!IsQueueReady(&audioCtx->pendingPackets))
				{
					TraceLog(LOG_ERROR, "MEDIA: Cannot initialize the audio packet queue.");

					AVUnloadCodecContext(audioCtx);

					continue;
				}

				//-------------------------------------------------------------

				ctx->audioOutputBuffer = LoadBuffer(MEDIA.audioDecodedBufferSize);

				if (!IsBufferReady(&ctx->audioOutputBuffer))
				{
					TraceLog(LOG_ERROR, "MEDIA: Cannot allocate memory for holding the decoded audio.");

					AVUnloadCodecContext(audioCtx);

					continue;
				}

				//-------------------------------------------------------------

				ctx->audioOutputFmt = MEDIA.audioOutputFmt;
				ctx->audioMaxUpdateSize = MEDIA.audioMaxUpdateSize;

				//-------------------------------------------------------------

				// Audio resampling
				ctx->swrContext = swr_alloc();

				if (!ctx->swrContext)
				{
					TraceLog(LOG_ERROR, "MEDIA: Cannot initialize the SWR context.");

					AVUnloadCodecContext(audioCtx);

					continue;
				}

				// Prepare output channel layout
				AVChannelLayout out_ch_layout;
				av_channel_layout_default(&out_ch_layout, MEDIA.audioOutputChannels);

				// Set options for SwrContext
				ret = swr_alloc_set_opts2(&ctx->swrContext,
					&out_ch_layout,
					ctx->audioOutputFmt,   // Output sample format
					codecCtx->sample_rate, // Output sample rate
					&codecCtx->ch_layout,  // Input channel layout
					codecCtx->sample_fmt,  // Input sample format
					codecCtx->sample_rate, // Input sample rate
					0, NULL);

				// Clean up the output channel layout
				av_channel_layout_uninit(&out_ch_layout);

				if(ret < 0)
				{
					AVPrintError(ret);

					swr_free(&ctx->swrContext);

					AVUnloadCodecContext(audioCtx);

					continue;
				}

				ret = swr_init(ctx->swrContext);

				// Initialize the SwrContext
				if (ret < 0) 
				{
					AVPrintError(ret);

					swr_free(&ctx->swrContext);

					AVUnloadCodecContext(audioCtx);

					continue;
				}

				//-------------------------------------------------------------

				audioCtx->streamIdx = i;

				//-------------------------------------------------------------
			}
			else
			{
				TraceLog(LOG_WARNING, "MEDIA: Cannot initialize the video codec. (Error code: %i)", ret);
			}

			TraceLog(LOG_DEBUG, "Media Debug: '%s' - Audio Codec: %d channels, sample rate %d", fileName, localCodecParameters->ch_layout.nb_channels, localCodecParameters->sample_rate);
		}

		TraceLog(LOG_DEBUG, "Media Debug: '%s' - Audio Codec: %s ID %d bit_rate %lld", fileName, localCodec->name, localCodec->id, localCodecParameters->bit_rate);
	}
	
	if (HasStream(ctx, STREAM_VIDEO) || HasStream(ctx, STREAM_AUDIO))
	{
		ctx->avFrame  = av_frame_alloc();

		if(!ctx->avFrame)
		{
			TraceLog(LOG_ERROR, "MEDIA: Failed to allocate memory for AVFrame");
			UnloadMediaContext(ctx);
			return NULL;
		}

		ctx->avPacket = av_packet_alloc();

		if (!ctx->avPacket)
		{
			TraceLog(LOG_ERROR, "MEDIA: Failed to allocate memory for AVPacket");
			UnloadMediaContext(ctx);
			return NULL;
		}

		ctx->state = MEDIA_STATE_STOPPED;
	}
	
	return ctx;
}

void UnloadMediaContext(MediaContext* ctx)
{
	assert(ctx);

	ctx->state = MEDIA_STATE_INVALID;

	for(int i = 0; i < STREAM_COUNT; ++i)
	{
		const StreamDataContext* streamCtx = &ctx->streams[i];
		if(streamCtx->codecCtx != NULL)
		{
			AVUnloadCodecContext(&ctx->streams[i]);
		}
		if(IsQueueReady(&ctx->streams[i].pendingPackets))
		{
			UnloadQueue(&ctx->streams[i].pendingPackets);
		}		
	}

	if(ctx->swsContext)
	{
		sws_freeContext(ctx->swsContext);
		ctx->swsContext = NULL;
	}

	if (IsImageValid(ctx->videoOutputImage))
	{
		UnloadImage(ctx->videoOutputImage);
		ctx->videoOutputImage = (Image){ 0 };
	}

	if(ctx->swrContext)
	{
		swr_free(&ctx->swrContext);
	}

	if(IsBufferReady(&ctx->audioOutputBuffer))
	{
		UnloadBuffer(&ctx->audioOutputBuffer);
	}

	if(ctx->formatContext)
	{
		//AVIOContext
	    if (ctx->formatContext->pb)
	    {
		    av_freep(&ctx->formatContext->pb->buffer);
		    avio_context_free(&ctx->formatContext->pb);
	    }

		avformat_close_input(&ctx->formatContext);
	}

	if(ctx->avPacket)
	{
		av_packet_free(&ctx->avPacket);
	}

	if (ctx->avFrame)
	{
		av_frame_free(&ctx->avFrame);
	}

	RL_FREE(ctx);
}


//---------------------------------------------------------------------------------------------------
// Functions Definition - MediaStream loading and unloading 
//---------------------------------------------------------------------------------------------------

MediaStream LoadMediaFromContext(MediaContext *ctx, int flags)
{
	MediaStream ret = (MediaStream){ 0 };

	ret.ctx = ctx;

	bool isLoaded = true;

	if (!ret.ctx)
	{
		isLoaded = false;
	}

	if (isLoaded && ret.ctx->streams[STREAM_VIDEO].codecCtx)
	{
		ret.videoTexture = LoadTextureFromImage(ret.ctx->videoOutputImage);

		if (IsTextureValid(ret.videoTexture))
		{
			SetTextureFilter(ret.videoTexture, TEXTURE_FILTER_BILINEAR);
		}
		else
		{
			isLoaded = false;
		}
	}

	if (isLoaded && ret.ctx->streams[STREAM_AUDIO].codecCtx)
	{
		const int sampleRate = ret.ctx->streams[STREAM_AUDIO].codecCtx->sample_rate;
		const int sampleSize = 8 * av_get_bytes_per_sample(MEDIA.audioOutputFmt);
		const int channels = MEDIA.audioOutputChannels;

		SetAudioStreamBufferSizeDefault(MEDIA.audioStreamBufferSize);

		ret.audioStream = LoadAudioStream(sampleRate, sampleSize, channels);

		// Revert to default buffer size
		SetAudioStreamBufferSizeDefault(0);

		if (!IsAudioStreamValid(ret.audioStream))
		{
			isLoaded = false;
		}
	}

	if (isLoaded)
	{
		if ((flags & MEDIA_FLAG_LOOP) != 0)
		{
			ret.ctx->loopPlay = true;
		}
		if ((flags & MEDIA_FLAG_NO_AUTOPLAY) == 0)
		{
			SetMediaState(ret, MEDIA_STATE_PLAYING);
		}
	}
	else
	{
		TraceLog(LOG_ERROR, "MEDIA: Failed to load the media");
		UnloadMedia(&ret);
		ret = (MediaStream){ 0 };
	}

	return ret;
}

 MediaStream LoadMedia(const char* fileName)
 {
	 return LoadMediaEx(fileName, MEDIA_LOAD_AV);
 }

 MediaStream LoadMediaEx(const char* fileName, int flags)
 {
	 MediaContext* ctx = LoadMediaContext(fileName, (MediaStreamReader) { 0 }, flags);
	 return LoadMediaFromContext(ctx, flags);
 }

 MediaStream LoadMediaFromStream(MediaStreamReader streamReader, int flags)
 {
	 if (!streamReader.readFn)
	 {
		 TraceLog(LOG_ERROR, "MEDIA: A valid read function is required to load media from a stream");
		 return (MediaStream) { 0 }; 
	 }

	 MediaContext* ctx = LoadMediaContext(NULL, streamReader, flags); 
	 return LoadMediaFromContext(ctx, flags); 
 }

 bool IsMediaValid(MediaStream media)
{
	return media.ctx != NULL && media.ctx->state != MEDIA_STATE_INVALID;
}

void UnloadMedia(MediaStream* media)
{
	assert(media);

	if (IsAudioStreamValid(media->audioStream))
	{
		UnloadAudioStream(media->audioStream);
		media->audioStream = (AudioStream){ 0 };
	}

	if(IsTextureValid(media->videoTexture))
{
		UnloadTexture(media->videoTexture);
		media->videoTexture = (Texture2D){ 0 };
}

	if(media->ctx)
{
		UnloadMediaContext(media->ctx);
		media->ctx = NULL;
	}
}


//---------------------------------------------------------------------------------------------------
// Functions Definition - Media State handling
//---------------------------------------------------------------------------------------------------

int GetMediaState(MediaStream media)
{
	return media.ctx ? media.ctx->state : MEDIA_STATE_INVALID;
}

int SetMediaState(MediaStream media, int newState)
{
	int curState = GetMediaState(media);

	if (curState == MEDIA_STATE_INVALID) 
	{
		TraceLog(LOG_WARNING, "MEDIA: Trying to change the state of an invalid media");
		return MEDIA_STATE_INVALID;
	}

	switch (newState)
	{

	case MEDIA_STATE_PLAYING:

		if (curState == MEDIA_STATE_STOPPED) 
		{
			UpdateState(&media, MEDIA_STATE_PLAYING);
			if(IsAudioStreamValid(media.audioStream))
			{
				PlayAudioStream(media.audioStream);
			}
		}
		else if (curState == MEDIA_STATE_PAUSED) 
		{
			UpdateState(&media, MEDIA_STATE_PLAYING);
			if (IsAudioStreamValid(media.audioStream)) 
			{
				ResumeAudioStream(media.audioStream);
			}
		}
		break;

	case MEDIA_STATE_PAUSED:

		if (curState == MEDIA_STATE_PLAYING) 
		{
			UpdateState(&media, MEDIA_STATE_PAUSED);
			if (IsAudioStreamValid(media.audioStream)) 
			{
				PauseAudioStream(media.audioStream);
			}
		}
		break;

	case MEDIA_STATE_STOPPED:

		if (curState != MEDIA_STATE_STOPPED) 
		{
			UpdateState(&media, MEDIA_STATE_STOPPED);
			if (!AVSeek(&media, 0)) 
			{
				TraceLog(LOG_WARNING, "MEDIA: Could not reset the stream.");
			}
		}
		break;

	default:

		TraceLog(LOG_WARNING, "MEDIA: Invalid state transition requested");
		return curState; // No change if newState is invalid
	}

	return media.ctx->state;
}

//---------------------------------------------------------------------------------------------------
// Functions Definition - Media Update logic
//---------------------------------------------------------------------------------------------------

bool UpdateMedia(MediaStream* media)
{
	return UpdateMediaEx(media, GetFrameTime());
}

bool UpdateMediaEx(MediaStream* media, double deltaTime)
{
	assert(media);

	if (!media->ctx)
	{
		TraceLog(LOG_WARNING, "MEDIA: Trying to update a invalid media.");
		return false;
	}

	MediaContext* ctx = media->ctx;

	if (media->ctx->state != MEDIA_STATE_PLAYING)
	{
		return true;
	}

	ctx->timePos += deltaTime;

	int ret = MEDIA_RET_SUCCEED;

	for (int i = 0; i < STREAM_COUNT; ++i)
	{
		StreamDataContext* streamCtx = &ctx->streams[i];

		if (!streamCtx->codecCtx)
		{
			continue;
		}

		bool discardPacketAndContinue = true;

		while(discardPacketAndContinue)
		{

			AVPacket* avPacket;

			// Peek at a packet from the queue without modifying the queue or changing packet references.
			ret = AVPeekPacket(ctx, i, &avPacket);

			if (ret == MEDIA_EOF)
			{
				NotifyEndOfStream(media);
				return true;
			}

			if (ret != MEDIA_RET_SUCCEED)
			{
				TraceLog(LOG_WARNING, "MEDIA: Failed grabbing packet from stream #i. (Error code: %i)", i, ret);
				break;
			}

			if (streamCtx->startPts == AV_NOPTS_VALUE)
			{
				streamCtx->startPts = avPacket->pts;
			}

			const AVRational* timebase = &ctx->formatContext->streams[streamCtx->streamIdx]->time_base;
			const double nextFrameTime = (double)(avPacket->pts - streamCtx->startPts) * timebase->num / timebase->den;

			// It's not yet time to use the packet
			// Since we have just "peeked" the packet no reference handling is needed
			if (ctx->timePos < nextFrameTime)
			{
				break;
			}
		   
			const double delaySec = ctx->timePos - nextFrameTime;
			
			discardPacketAndContinue = delaySec > MEDIA.maxAllowedDelay[i];

			ret = AVDecodePacket(media, i, avPacket, discardPacketAndContinue);

			if (ret < 0)
			{
				TraceLog(LOG_ERROR, "MEDIA: Decoding packet (stream type: %i, error code: %i)", i, ret);
			}

			// Advance the read position in the circular buffer queue and un-reference the packet.
			// This is equivalent to DequeueBuffer but avoids transferring ownership of the packet reference.
			AdvanceReadPos(&streamCtx->pendingPackets.state);
			av_packet_unref(avPacket);
		}		
	}

	if (HasStream(ctx, STREAM_AUDIO) && IsAudioStreamProcessed(media->audioStream))
	{
		const int readableSegmentBytes = GetBufferReadableSegmentSize(&ctx->audioOutputBuffer.state);

		const int updateSize = MIN(readableSegmentBytes, ctx->audioMaxUpdateSize);

		const int bytesPerSample = (int)((media->audioStream.sampleSize / 8) * media->audioStream.channels);

		const int frameCount = updateSize / bytesPerSample;

		UpdateAudioStream(media->audioStream, &ctx->audioOutputBuffer.data[ctx->audioOutputBuffer.state.readPos], frameCount);

		AdvanceReadPosN(&ctx->audioOutputBuffer.state, updateSize);
	}

	return ret == MEDIA_RET_SUCCEED;
}


//---------------------------------------------------------------------------------------------------
// Functions Declaration - Circular buffer logic
//---------------------------------------------------------------------------------------------------


int IsBufferFull(const BufferState* state)
{
	return (state->writePos + 1) % state->capacity == state->readPos;
}

int IsBufferEmpty(const BufferState* state)
{
	return state->writePos == state->readPos;
}

int GetBufferWritableSpace(const BufferState* state)
{
	if(state->readPos > state->writePos)
	{
		return state->readPos - state->writePos - 1;
	}

	return state->capacity - state->writePos + state->readPos;
}

int GetBufferWritableSegmentSize(const BufferState* state)
{
	if(state->readPos > state->writePos)
	{
		return state->readPos - state->writePos - 1;
	}

	return state->capacity - state->writePos;	
}

int GetBufferReadableSpace(const BufferState* state)
{
	if (state->readPos > state->writePos)
	{
		return state->capacity - state->readPos + state->writePos;
	}

	return state->writePos - state->readPos;
}

int GetBufferReadableSegmentSize(const BufferState* state)
{
	if(state->readPos > state->writePos)
	{
		return state->capacity - state->readPos;
	}

	return state->writePos - state->readPos;
}

void AdvanceWritePos(BufferState* state)
{
	AdvanceWritePosN(state, 1);
}

void AdvanceWritePosN(BufferState* state, int n)
{
	state->writePos = (state->writePos + n) % state->capacity;
}

void AdvanceReadPos(BufferState* state)
{
	AdvanceReadPosN(state, 1);
}

void AdvanceReadPosN(BufferState* state, int n)
{
	state->readPos = (state->readPos + n) % state->capacity;
}

//---------------------------------------------------------------------------------------------------
// Functions Definition - Buffer management
//---------------------------------------------------------------------------------------------------

Buffer LoadBuffer(int capacity)
{
	assert(capacity > 0); 

	Buffer ret = (Buffer){ 0 };

	ret.data = RL_MALLOC(capacity);

	if (ret.data)
	{
		ret.state.capacity = capacity;
	}
	else
	{
		TraceLog(LOG_ERROR, "MEDIA: Failed to allocate a buffer with capacity of %i bytes.", capacity);
	}

	return ret;
}

void UnloadBuffer(Buffer* buffer)
{
	assert(buffer);

	if(buffer->data)
	{
		RL_FREE(buffer->data);
		*buffer = (Buffer){ 0 };
	}
	else
	{
		TraceLog(LOG_WARNING, "MEDIA: Trying to unload an NUL buffer.");
	}
}

bool IsBufferReady(const Buffer* buffer)
{
	assert(buffer);

	return buffer->data != NULL;
}

void ClearBuffer(Buffer* buffer)
{
	assert(buffer);

	if(buffer->data)
	{
		buffer->state.readPos = 0;
		buffer->state.writePos = 0;
	}
	else
	{
		TraceLog(LOG_WARNING, "MEDIA: Trying to clear a buffer with no data.");
	}
}

int WriteBuffer(Buffer* buffer, const uint8_t* srcData, int srcSize)
{
	assert(buffer);
	assert(srcSize>=0);

	if(!srcData)
	{
		TraceLog(LOG_ERROR, "MEDIA: Trying to write from a NULL source.");
		return -1;
	}

	int sizeToWrite = srcSize;

	while(sizeToWrite > 0)
	{
		const int segmentToWrite = MIN(sizeToWrite, GetBufferWritableSegmentSize(&buffer->state));

		if(segmentToWrite <= 0)
		{
			break;
		}

		memcpy(&buffer->data[buffer->state.writePos], srcData, segmentToWrite);

		AdvanceWritePosN(&buffer->state, segmentToWrite);

		srcData += segmentToWrite;
		sizeToWrite -= segmentToWrite;
	}

	if(sizeToWrite > 0)
	{
		TraceLog(LOG_WARNING, "MEDIA: Could not write %i bytes to the buffer.", sizeToWrite);
		if (IsBufferFull(&buffer->state))
		{
			if (IsBufferFull(&buffer->state))
			{
				TraceLog(LOG_WARNING,
					"The buffer was filled while writing. Consider increasing its capacity. "
					"Capacity: %i bytes, Remaining unwritten: %i bytes.",
					buffer->state.capacity, sizeToWrite);
			}
		}
	}

	return srcSize - sizeToWrite;
}

int ReadBuffer(Buffer* buffer, uint8_t* dstData, int dstSize)
{
	assert(buffer);
	assert(dstSize >= 0);

	if (!dstData)
	{
		TraceLog(LOG_ERROR, "MEDIA: Trying to read into a NULL destination.");
		return -1;
	}

	int sizeToRead = dstSize;

	while (sizeToRead > 0)
	{
		const int segmentToRead = MIN(sizeToRead, GetBufferReadableSegmentSize(&buffer->state));

		if (segmentToRead <= 0)
		{
			break;
		}

		memcpy(dstData, &buffer->data[buffer->state.readPos], segmentToRead);

		AdvanceReadPosN(&buffer->state, segmentToRead);

		dstData += segmentToRead;
		sizeToRead -= segmentToRead;
	}

	if (sizeToRead > 0)
	{
		TraceLog(LOG_WARNING, "MEDIA: Could not read %i bytes from the buffer.", sizeToRead);
		if(IsBufferEmpty(&buffer->state))
		{
			TraceLog(LOG_WARNING,
				"Readable data was exhausted while reading. Consider increasing buffer capacity."
				"Capacity: %i bytes, Remaining to read: %i bytes.",
				buffer->state.capacity, sizeToRead);
		}
	}

	return dstSize - sizeToRead;
}


//---------------------------------------------------------------------------------------------------
// Functions Declaration - PacketQueue management
//---------------------------------------------------------------------------------------------------

PacketQueue LoadQueue(int capacity)
{
	assert(capacity > 0);

	PacketQueue ret = (PacketQueue){ 0 };

	const int sizeToAllocate = (int)sizeof(AVPacket*) * capacity;

	ret.packets = RL_MALLOC(sizeToAllocate);

	if (ret.packets)
	{
		ret.state.capacity = capacity;

		memset((void*)ret.packets, 0, sizeToAllocate);

		for (int i = 0; i < capacity; ++i)
		{
			ret.packets[i] = av_packet_alloc();
			if(!ret.packets[i])
			{
				TraceLog(LOG_ERROR,"MEDIA: Failed to allocate packet at index %i, the queue will be unloaded.", i);

				UnloadQueue(&ret);

				return (PacketQueue) { 0 };
			}
		}
	}
	else
	{
		TraceLog(LOG_ERROR, "MEDIA: Failed to allocate a packet queue with capacity of %i packets.", capacity);
	}

	return ret;
}

bool IsQueueReady(const PacketQueue* queue)
{
	assert(queue);

	return queue->packets != NULL;
}

void UnloadQueue(PacketQueue* queue)
{
	assert(queue);

	if (queue->packets)
	{

		while(!IsBufferEmpty(&queue->state))
		{
			av_packet_unref(queue->packets[queue->state.readPos]);

			AdvanceReadPos(&queue->state);			
		}

		for (int i = 0; i < queue->state.capacity; ++i)
		{
			av_packet_free(&queue->packets[i]);
		}

		RL_FREE((void*)queue->packets);

		*queue = (PacketQueue){ 0 };
	}
	else
	{
		TraceLog(LOG_WARNING, "MEDIA: Trying to unload a NULL queue.");
	}
}

void ClearQueue(PacketQueue* queue)
{
	assert(queue);

	if (queue->packets)
	{
		while (!IsBufferEmpty(&queue->state))
		{
			av_packet_unref(queue->packets[queue->state.readPos]);

			AdvanceReadPos(&queue->state);
		}

		queue->state.readPos = 0;

		queue->state.writePos = 0;
	}
	else
	{
		TraceLog(LOG_WARNING, "MEDIA: Trying to clear a packet queue with no allocated packets.");
	}
}

bool IsQueueEmpty(const PacketQueue* queue)
{
	assert(queue);

	return IsBufferEmpty(&queue->state);
}

bool IsQueueFull(const PacketQueue* queue)
{
	assert(queue);

	return IsBufferFull(&queue->state);
}

bool EnqueuePacket(PacketQueue* queue, AVPacket* src)
{
	assert(queue);

	if (!src)
	{
		TraceLog(LOG_ERROR, "MEDIA: Trying to enqueue a NULL packet.");
		return false;
	}

	if(!queue->packets)
	{
		TraceLog(LOG_ERROR, "MEDIA: Trying to enqueue packet to a queue with no allocated packets.");
		return false;
	}

	if(IsQueueFull(queue))
	{
		TraceLog(LOG_DEBUG, "MEDIA: Queue full, dropping the packet.");

		av_packet_unref(src);

		return false;
	}

	av_packet_move_ref(queue->packets[queue->state.writePos], src);

	AdvanceWritePos(&queue->state);

	return true;
}

bool DequeuePacket(PacketQueue* queue, AVPacket* dst)
{
	assert(queue);

	if (!dst)
	{
		TraceLog(LOG_ERROR, "MEDIA: Trying to dequeue a NULL packet.");
		return false;
	}

	if (!queue->packets)
	{
		TraceLog(LOG_ERROR, "MEDIA: Trying to dequeue packet from a queue with no allocated packets.");
		return false;
	}

	if (IsQueueEmpty(queue))
	{
		TraceLog(LOG_WARNING, "MEDIA: Queue empty, cannot dequeue a packet.");

		return false;
	}

	av_packet_move_ref(dst, queue->packets[queue->state.readPos]);

	AdvanceReadPos(&queue->state);

	return true;
}

AVPacket* PeekPacket(const PacketQueue* queue)
{
	assert(queue);

	if (!queue->packets)
	{
		TraceLog(LOG_ERROR, "MEDIA: Trying to peek a packet from a queue with no allocated packets.");
		return NULL;
	}

	return !IsQueueEmpty(queue) ? queue->packets[queue->state.readPos] : NULL;
}


//---------------------------------------------------------------------------------------------------
// Functions Definition - AV management - FFmpeg(libav)
//---------------------------------------------------------------------------------------------------

int AVGrabPacket(MediaContext* ctx, int streamType, AVPacket* dst)
{
	assert(ctx);

	if (!dst)
	{
		TraceLog(LOG_ERROR, "MEDIA: packet destination is NULL.");
		return MEDIA_ERR_GRAB_PACKET;
	}

	if(!ctx->streams[streamType].codecCtx)
	{
		TraceLog(LOG_WARNING, "MEDIA: Trying to grab a packet of an unavailable type: %i", streamType);
		return MEDIA_ERR_GRAB_PACKET;
	}

	// If there are no pending AVPackets of the desired type to dequeue, start grabbing and enqueueing new packets 
	// until a packet of the desired type is available.
	while(IsQueueEmpty(&ctx->streams[streamType].pendingPackets))
	{

		const int ret = av_read_frame(ctx->formatContext, dst);

		if (ret < 0)
		{

			if (ret == AVERROR_EOF) 
			{
				return MEDIA_EOF;
			}
			else 
			{
				AVPrintError(ret);
				TraceLog(LOG_ERROR, "MEDIA: Error reading packet for stream type %d", streamType);
			}

			return MEDIA_ERR_GRAB_PACKET;
		}

		
		if(dst->stream_index == ctx->streams[STREAM_VIDEO].streamIdx)			// The grabbed packet is a video packet
		{
			// If we requested a video packet, return success as we have what we need
			if (streamType == STREAM_VIDEO)
			{
				return MEDIA_RET_SUCCEED;
			}

			// Otherwise, enqueue this video packet since a different packet type was requested
			EnqueuePacket(&ctx->streams[STREAM_VIDEO].pendingPackets, dst);
		}
		else if (dst->stream_index == ctx->streams[STREAM_AUDIO].streamIdx)		// The grabbed packet is an audio packet
		{
			// If we requested an audio packet, return success as we have what we need
			if (streamType == STREAM_AUDIO)
			{
				return MEDIA_RET_SUCCEED;
			}

			// Otherwise, enqueue this audio packet since a different packet type was requested
			EnqueuePacket(&ctx->streams[STREAM_AUDIO].pendingPackets, dst);
		}
		else // Unhandled packet
		{
			av_packet_unref(dst);
		}

	}

	// A previously enqueued packet of the desired type is available to return
	return DequeuePacket(&ctx->streams[streamType].pendingPackets, dst) ? MEDIA_RET_SUCCEED : MEDIA_ERR_GRAB_PACKET;
}

int AVPeekPacket(MediaContext* ctx, int streamType, AVPacket** ptr)
{
	assert(ctx);
	assert(ctx->streams[streamType].codecCtx);

	PacketQueue* queue = &ctx->streams[streamType].pendingPackets;

	if (IsQueueEmpty(queue))
	{
		int ret = AVGrabPacket(ctx, streamType, ctx->avPacket);
		if (ret != MEDIA_RET_SUCCEED)
		{
			return ret;
		}
		ret = EnqueuePacket(queue, ctx->avPacket);
		assert(ret);
	}

	*ptr = PeekPacket(queue);

	return MEDIA_RET_SUCCEED;
}

bool AVSeekRelative(MediaStream* media, double factor)
{
	assert(media->ctx);

	factor = CLAMP(factor, 0.0, 1.0);

	const int64_t targetTimestamp = (int64_t)((double)media->ctx->formatContext->duration * factor);

	return AVSeek(media, targetTimestamp);
}

bool AVSeekVideoKeyframe(const MediaStream* media)
{
	assert(media);

	if (!HasStream(media->ctx, STREAM_VIDEO))
		return true;

	MediaContext* ctx = media->ctx;
	StreamDataContext* streamCtx = &ctx->streams[STREAM_VIDEO];

	while (true)
	{
		AVPacket* vPacket = NULL;
		int ret = AVPeekPacket(ctx, STREAM_VIDEO, &vPacket);

		if (ret == MEDIA_EOF)
		{
			NotifyEndOfStream(media);
			break;
		}

		if (ret != MEDIA_RET_SUCCEED)
		{
			TraceLog(LOG_WARNING, "MEDIA: Failed grabbing packet from video stream. (Error code: %i)", ret);
			return false;
		}

		// A video keyframe has just been found, we are done.
		if (vPacket->flags & AV_PKT_FLAG_KEY)
		{
			// Set the media time to match this keyframe
			ctx->timePos = (double)(vPacket->pts - streamCtx->startPts) *
				av_q2d(ctx->formatContext->streams[streamCtx->streamIdx]->time_base);
			break;
		}

		// Discard non-keyframe packets (Same of DequeuePacket, but it avoids redundant reference moving)
		AdvanceReadPos(&streamCtx->pendingPackets.state);
		av_packet_unref(vPacket);
	}

	return true;
}


bool AVSeek(MediaStream* media, int64_t targetTimestamp)
{
	MediaContext* ctx = media->ctx;

	assert(ctx);

	const int ret = avformat_seek_file(ctx->formatContext, -1, INT64_MIN, targetTimestamp, INT64_MAX, AVSEEK_FLAG_BACKWARD);

	if (ret < 0) 
	{
		AVPrintError(ret);
		return false;
	}

	// Update the time to the target TS
	ctx->timePos = (double)targetTimestamp / AV_TIME_BASE;

	for(int i = 0; i < STREAM_COUNT; ++i)
	{
		AVCodecContext* codecCtx = ctx->streams[i].codecCtx;

		if(codecCtx)
		{
			avcodec_flush_buffers(codecCtx);

			ClearQueue(&ctx->streams[i].pendingPackets);
		}		
	}

	// If the media has a video stream then seek the first video keyframe to avoid image output artifacts
	if(!AVSeekVideoKeyframe(media))
	{
		return false;
	}

	if (IsAudioStreamValid(media->audioStream))
	{
		StopAudioStream(media->audioStream);

		ClearBuffer(&ctx->audioOutputBuffer);

		switch (GetMediaState(*media))
		{
		case MEDIA_STATE_PLAYING:

			UpdateMediaEx(media, 0.0); // grab the first packets with deltaTime = 0.0

			PlayAudioStream(media->audioStream);

			break;

		case MEDIA_STATE_PAUSED:

			UpdateMediaEx(media, 0.0); // grab the first packets with deltaTime = 0.0

			PlayAudioStream(media->audioStream);

			PauseAudioStream(media->audioStream);

			break;

		case MEDIA_STATE_STOPPED:

		default:

			// Leave the audio stream in a stopped state
			break;
		}
	}	

	return true;

}

void AVPrintError(int errCode)
{
	char errBuffer[AV_ERROR_MAX_STRING_SIZE] = { 0 }; 
	if (av_strerror(errCode, errBuffer, sizeof(errBuffer)) == 0) 
	{
		TraceLog(LOG_ERROR, "FFMPEG: %s", errBuffer);
	}
	else
	{
		TraceLog(LOG_ERROR, "FFMPEG: Unknown error code %d", errCode);
	}
}

int AVDecodePacket(const MediaStream* media, int streamType, const AVPacket* packet, bool discardPacket)
{
	const StreamDataContext* streamCtx = &media->ctx->streams[streamType];

	// Supply raw packet data as input to a decoder
	int ret = avcodec_send_packet(streamCtx->codecCtx, packet);					

	if (ret < 0) 
	{
		AVPrintError(ret);

		return ret;
	}

	while (ret >= 0)
	{
		ret = avcodec_receive_frame(streamCtx->codecCtx, media->ctx->avFrame);

		if (ret == AVERROR(EAGAIN) || ret == AVERROR_EOF) 
		{
			ret = MEDIA_RET_SUCCEED;
			break;
		}

		if (ret < 0) 
		{
			AVPrintError(ret);
			break;
		}

		if (ret >= 0 && !discardPacket) {

			switch(streamType)
			{
			case STREAM_VIDEO:
				ret = AVProcessVideoFrame(media);
				break;
			case STREAM_AUDIO:
				ret = AVProcessAudioFrame(media);
				break;
			default:
				TraceLog(LOG_WARNING, "MEDIA: Unsupported stream type.");
				ret = MEDIA_ERR_UNKNOWN_STREAM;
				break;
			}
		}

	}

	av_frame_unref(media->ctx->avFrame);

	return ret;
}

int  AVProcessVideoFrame(const MediaStream* media)
{
	const MediaContext* ctx = media->ctx;
	const AVCodecContext* codec = ctx->streams[STREAM_VIDEO].codecCtx;
	const int rgbLineSize = codec->width * 3;

	// Convert the frame to RGB
	sws_scale(ctx->swsContext, (const uint8_t* const*)ctx->avFrame->data, ctx->avFrame->linesize, 0, codec->height, (uint8_t* const*) &ctx->videoOutputImage.data, &rgbLineSize);

	// Update texture with the decoded image data
	UpdateTexture(media->videoTexture, ctx->videoOutputImage.data);

	return 0;
}

int  AVProcessAudioFrame(const MediaStream* media)
{
	MediaContext* ctx = media->ctx;

	int ret = 0;

	// Initialize input data and sample count for conversion. After the first call, inData and inSamples 
	// will be set to NULL and 0 respectively to signal swr_convert to finish processing remaining data.
	const uint8_t* const* inData = (const uint8_t* const*)ctx->avFrame->data;
	int inSamples = ctx->avFrame->nb_samples;

	// Initialize the number of samples left to convert. The loop will continue until all samples are converted.
	int leftSamples = inSamples;

	do
	{
		if(IsBufferFull(&ctx->audioOutputBuffer.state))
		{
			TraceLog(LOG_WARNING, "MEDIA: Not enough space for decoding in the audio buffer.");
			ret = MEDIA_ERR_OVERFLOW;
			break;
		}
		
		const int writableSegmentSizeBytes = GetBufferWritableSegmentSize(&ctx->audioOutputBuffer.state);

		// Determine the number of bytes per audio frame, based on sample size and channel count.
		const int bytesPerFrame = (int)((media->audioStream.sampleSize / 8) * media->audioStream.channels);

		// Calculate the writable segment size in terms of audio samples.
		const int writableSegmentSizeSamples = writableSegmentSizeBytes / bytesPerFrame;

		uint8_t* outputBuffer = &ctx->audioOutputBuffer.data[ctx->audioOutputBuffer.state.writePos];

		// Convert and store the incoming audio samples into the output buffer.
		// This will fill up to the writable segment size in samples, using the provided input data.
		const int convertedSamples = swr_convert(ctx->swrContext,
		                                         &outputBuffer,
		                                         writableSegmentSizeSamples,
		                                         inData,
			 									 inSamples);

		if(convertedSamples < 0)
		{
			AVPrintError(convertedSamples);
			ret = MEDIA_ERR_DECODE_AUDIO;
			break; 
		}

		// Calculate the size in bytes of the converted samples just written to the buffer.
		const int convertedSamplesBytes = av_samples_get_buffer_size(
			NULL,
			(int)media->audioStream.channels,
			convertedSamples,
			ctx->audioOutputFmt,
			1
		);

		AdvanceWritePosN(&ctx->audioOutputBuffer.state, convertedSamplesBytes);

		leftSamples -= convertedSamples;

		// Setting inData to NULL and inSamples to 0 instructs subsequent swr_convert calls
		// to process any remaining buffered data
		inData = NULL;
		inSamples = 0;

	} while (leftSamples > 0);

	return ret;
}


bool AVLoadCodecContext(StreamDataContext* streamCtx, const AVCodec* codec, const AVCodecParameters* params)
{
	if (streamCtx->codecCtx)
	{
		TraceLog(LOG_WARNING, "MEDIA: Multiple stream of the same type (%i), only the first will be used.", params->codec_type);

		return MEDIA_ERR_DUPLICATE_STREAM;
	}

	streamCtx->startPts = AV_NOPTS_VALUE;

	streamCtx->codecCtx = avcodec_alloc_context3(codec);

	if (!streamCtx->codecCtx)
	{
		TraceLog(LOG_WARNING, "MEDIA: Failed to allocate memory for a codec (type %i)", params->codec_type);

		return MEDIA_ERR_CODEC_ALLOC_FAILED;
	}

	int ret = avcodec_parameters_to_context(streamCtx->codecCtx, params);

	if (ret < 0)
	{
		AVPrintError(ret);

		return MEDIA_ERR_CTX_PARAMS_FAILED;
	}

	// Initialize the AVCodecContext to use the given AVCodec.
	ret = avcodec_open2(streamCtx->codecCtx, codec, NULL);

	if (ret < 0)
	{
		AVPrintError(ret);

		return MEDIA_ERR_CODEC_OPEN_FAILED;
	}

	return MEDIA_RET_SUCCEED;
}

void AVUnloadCodecContext(StreamDataContext* streamCtx)
{
	assert(streamCtx->codecCtx);

	avcodec_free_context(&streamCtx->codecCtx);
}


//---------------------------------------------------------------------------------------------------
// Functions Definition - Helpers
//---------------------------------------------------------------------------------------------------

void NotifyEndOfStream(const MediaStream* media)
{
	SetMediaState(*media, MEDIA_STATE_STOPPED);

	if(media->ctx->loopPlay)
	{
		SetMediaState(*media, MEDIA_STATE_PLAYING);
	}
}

void UpdateState(const MediaStream* media, int newState)
{
	media->ctx->state = newState;
}

bool HasStream(const MediaContext* ctx, int streamType)
{
	return ctx->streams[streamType].codecCtx != NULL;
}
