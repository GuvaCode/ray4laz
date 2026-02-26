(*******************************************************************************************
*
*   raylib [audio] example - fft spectrum visualizer
*
*   Example complexity rating: [★★★☆] 3/4
*
*   Example originally created with raylib 6.0
*
*   Inspired by Inigo Quilez's https://www.shadertoy.com/
*   Resources/specification: https://gist.github.com/soulthreads/2efe50da4be1fb5f7ab60ff14ca434b8
*
*   Example created by created by IANN (@meisei4) reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2025 IANN (@meisei4)
*   Pascal translation (c) 2025 Vadim Gunko (@GuvaCode)
*
********************************************************************************************)

program audio_fft_spectrum_visualizer;

uses
  Math, SysUtils, Classes,
  raylib, raymath;

{$POINTERMATH ON}

const
  MONO = 1;
  SAMPLE_RATE = 44100;
  SAMPLE_RATE_F = 44100.0;
  FFT_WINDOW_SIZE = 1024;
  BUFFER_SIZE = 512;
  PER_SAMPLE_BIT_DEPTH = 16;
  AUDIO_STREAM_RING_BUFFER_SIZE = FFT_WINDOW_SIZE * 2;
  EFFECTIVE_SAMPLE_RATE = SAMPLE_RATE_F * 0.5;
  WINDOW_TIME = FFT_WINDOW_SIZE / EFFECTIVE_SAMPLE_RATE;
  FFT_HISTORICAL_SMOOTHING_DUR = 2.0;
  MIN_DECIBELS = -100.0;
  MAX_DECIBELS = -30.0;
  INVERSE_DECIBEL_RANGE = 1.0 / (MAX_DECIBELS - MIN_DECIBELS);
  DB_TO_LINEAR_SCALE = 20.0 / 2.302585092994046;
  SMOOTHING_TIME_CONSTANT = 0.8;
  TEXTURE_HEIGHT = 1;
  FFT_ROW = 0;
  UNUSED_CHANNEL = 0.0;

type
  PFFTComplex = ^TFFTComplex;
  TFFTComplex = record
    real: Single;
    imaginary: Single;
  end;
  PFFTComplexes = array of PFFTComplex;

  TFFTBuffer = array[0..BUFFER_SIZE-1] of Single;
  PFFTBuffer = {%H-}^TFFTBuffer;

  PFFTData = ^TFFTData;
  TFFTData = record
    spectrum: PFFTComplex;
    workBuffer: PFFTComplex;
    prevMagnitudes: PSingle;
    fftHistory: array of TFFTBuffer;
    fftHistoryLen: Integer;
    historyPos: Integer;
    lastFftTime: Double;
    tapbackPos: Single;
  end;

procedure CooleyTukeyFFTSlow(spectrum: PFFTComplex; n: Integer);
var
  i, j, bit, len: Integer;
  angle: Single;
  twiddleUnit, twiddleCurrent, even, odd, twiddledOdd: TFFTComplex;
  temp: TFFTComplex;
  twiddleRealNext: Single;
begin
  j := 0;
  for i := 1 to n - 2 do
  begin
    bit := n shr 1;
    while j >= bit do
    begin
      j := j - bit;
      bit := bit shr 1;
    end;
    j := j + bit;
    if i < j then
    begin
      temp := spectrum[i];
      spectrum[i] := spectrum[j];
      spectrum[j] := temp;
    end;
  end;

  len := 2;
  while len <= n do
  begin
    angle := -2.0 * PI / len;
    twiddleUnit.real := Cos(angle);
    twiddleUnit.imaginary := Sin(angle);

    i := 0;
    while i < n do
    begin
      twiddleCurrent.real := 1.0;
      twiddleCurrent.imaginary := 0.0;

      for j := 0 to (len div 2) - 1 do
      begin
        even := spectrum[i + j];
        odd := spectrum[i + j + len div 2];

        twiddledOdd.real := odd.real * twiddleCurrent.real - odd.imaginary * twiddleCurrent.imaginary;
        twiddledOdd.imaginary := odd.real * twiddleCurrent.imaginary + odd.imaginary * twiddleCurrent.real;

        spectrum[i + j].real := even.real + twiddledOdd.real;
        spectrum[i + j].imaginary := even.imaginary + twiddledOdd.imaginary;
        spectrum[i + j + len div 2].real := even.real - twiddledOdd.real;
        spectrum[i + j + len div 2].imaginary := even.imaginary - twiddledOdd.imaginary;

        twiddleRealNext := twiddleCurrent.real * twiddleUnit.real - twiddleCurrent.imaginary * twiddleUnit.imaginary;
        twiddleCurrent.imaginary := twiddleCurrent.real * twiddleUnit.imaginary + twiddleCurrent.imaginary * twiddleUnit.real;
        twiddleCurrent.real := twiddleRealNext;
      end;

      i := i + len;
    end;

    len := len shl 1;
  end;
end;

procedure CaptureFrame(fftData: PFFTData; audioSamples: PSingle);
var
  i, bin: Integer;
  x, blackmanWeight, re, im, linearMagnitude, smoothedMagnitude, db, normalized: Single;
  smoothedSpectrum: array[0..BUFFER_SIZE-1] of Single;
begin
  for i := 0 to FFT_WINDOW_SIZE - 1 do
  begin
    x := (2.0 * PI * i) / (FFT_WINDOW_SIZE - 1.0);
    blackmanWeight := 0.42 - 0.5 * Cos(x) + 0.08 * Cos(2.0 * x);
    fftData^.workBuffer[i].real := audioSamples[i] * blackmanWeight;
    fftData^.workBuffer[i].imaginary := 0.0;
  end;

  CooleyTukeyFFTSlow(fftData^.workBuffer, FFT_WINDOW_SIZE);
  Move(fftData^.workBuffer^, fftData^.spectrum^, SizeOf(TFFTComplex) * FFT_WINDOW_SIZE);

  for bin := 0 to BUFFER_SIZE - 1 do
  begin
    re := fftData^.workBuffer[bin].real;
    im := fftData^.workBuffer[bin].imaginary;
    linearMagnitude := Sqrt(re * re + im * im) / FFT_WINDOW_SIZE;

    smoothedMagnitude := SMOOTHING_TIME_CONSTANT * fftData^.prevMagnitudes[bin] +
                        (1.0 - SMOOTHING_TIME_CONSTANT) * linearMagnitude;
    fftData^.prevMagnitudes[bin] := smoothedMagnitude;

    db := Ln(Max(smoothedMagnitude, 1e-40)) * DB_TO_LINEAR_SCALE;
    normalized := (db - MIN_DECIBELS) * INVERSE_DECIBEL_RANGE;
    smoothedSpectrum[bin] := Clamp(normalized, 0.0, 1.0);
  end;

  fftData^.lastFftTime := GetTime();
  Move(smoothedSpectrum, fftData^.fftHistory[fftData^.historyPos], SizeOf(smoothedSpectrum));
  fftData^.historyPos := (fftData^.historyPos + 1) mod fftData^.fftHistoryLen;
end;

procedure RenderFrame(fftData: PFFTData; fftImage: PImage);
var
  framesSinceTapback: Double;
  historyPosition, bin: Integer;
  amplitude: PSingle;
  colorVec: TVector4;
begin
  framesSinceTapback := Floor(fftData^.tapbackPos / WINDOW_TIME);
  framesSinceTapback := Clamp(framesSinceTapback, 0.0, fftData^.fftHistoryLen - 1);

  historyPosition := (fftData^.historyPos - 1 - Trunc(framesSinceTapback)) mod fftData^.fftHistoryLen;
  if historyPosition < 0 then
    historyPosition := historyPosition + fftData^.fftHistoryLen;

  amplitude := @fftData^.fftHistory[historyPosition][0];

  colorVec.x := UNUSED_CHANNEL;
  colorVec.y := UNUSED_CHANNEL;
  colorVec.z := UNUSED_CHANNEL;
  colorVec.w := UNUSED_CHANNEL;

  for bin := 0 to BUFFER_SIZE - 1 do
  begin
    colorVec.x := amplitude[bin];
    ImageDrawPixel(fftImage, bin, FFT_ROW, ColorFromNormalized(colorVec));
  end;
end;

var
  screenWidth, screenHeight: Integer;
  fftImage: TImage;
  fftTexture: TTexture2D;
  bufferA: TRenderTexture2D;
  iResolution: TVector2;
  shader: TShader;
  iResolutionLocation, iChannel0Location: Integer;
  wav: TWave;
  audioStream: TAudioStream;
  fftHistoryLen: Integer;
  fft: TFFTData;
  wavCursor: NativeInt;
  wavPCM16: PSmallInt;
  chunkSamples: array[0..AUDIO_STREAM_RING_BUFFER_SIZE-1] of SmallInt;
  audioSamples: array[0..FFT_WINDOW_SIZE-1] of Single;
  i: Integer;
  left, right: Integer;
begin
  screenWidth := 800;
  screenHeight := 450;

  InitWindow(screenWidth, screenHeight, 'raylib [audio] example - fft spectrum visualizer');

  fftImage := GenImageColor(BUFFER_SIZE, TEXTURE_HEIGHT, WHITE);
  fftTexture := LoadTextureFromImage(fftImage);
  bufferA := LoadRenderTexture(screenWidth, screenHeight);
  iResolution.x := screenWidth;
  iResolution.y := screenHeight;

  shader := LoadShader(nil, 'resources/fft.glsl');
  iResolutionLocation := GetShaderLocation(shader, 'iResolution');
  iChannel0Location := GetShaderLocation(shader, 'iChannel0');
  SetShaderValue(shader, iResolutionLocation, @iResolution, SHADER_UNIFORM_VEC2);
  SetShaderValueTexture(shader, iChannel0Location, fftTexture);

  InitAudioDevice();
  SetAudioStreamBufferSizeDefault(AUDIO_STREAM_RING_BUFFER_SIZE);

  wav := LoadWave('resources/country.mp3');
  WaveFormat(@wav, SAMPLE_RATE, PER_SAMPLE_BIT_DEPTH, MONO);

  audioStream := LoadAudioStream(SAMPLE_RATE, PER_SAMPLE_BIT_DEPTH, MONO);
  PlayAudioStream(audioStream);

  fftHistoryLen := Ceil(FFT_HISTORICAL_SMOOTHING_DUR / WINDOW_TIME) + 1;

  fft.spectrum := GetMemory(SizeOf(TFFTComplex) * FFT_WINDOW_SIZE);
  fft.workBuffer := GetMemory(SizeOf(TFFTComplex) * FFT_WINDOW_SIZE);
  fft.prevMagnitudes := GetMemory(SizeOf(Single) * BUFFER_SIZE);
  FillChar(fft.prevMagnitudes^, SizeOf(Single) * BUFFER_SIZE, 0);

  SetLength(fft.fftHistory, fftHistoryLen);
  FillChar(fft.fftHistory[0], SizeOf(TFFTBuffer) * fftHistoryLen, 0);

  fft.fftHistoryLen := fftHistoryLen;
  fft.historyPos := 0;
  fft.lastFftTime := 0.0;
  fft.tapbackPos := 0.01;

  wavCursor := 0;
  wavPCM16 := wav.data;

  if wavPCM16 = nil then
  begin
    raise Exception.Create('wav is empty');
    Exit;
  end;

  FillChar(chunkSamples{%H-}, SizeOf(chunkSamples), 0);
  FillChar(audioSamples{%H-}, SizeOf(audioSamples), 0);

  SetTargetFPS(60);

  while not WindowShouldClose() do
  begin
    while IsAudioStreamProcessed(audioStream) do
    begin
      for i := 0 to AUDIO_STREAM_RING_BUFFER_SIZE - 1 do
      begin
        if wav.channels = 2 then
        begin
          left := wavPCM16[wavCursor * 2];
          right := wavPCM16[wavCursor * 2 + 1];
        end
        else
        begin
          left := wavPCM16[wavCursor];
          right := left;
        end;

        chunkSamples[i] := SmallInt((left + right) div 2);

        Inc(wavCursor);
        if wavCursor >= wav.frameCount then
          wavCursor := 0;
      end;

      UpdateAudioStream(audioStream, @chunkSamples, AUDIO_STREAM_RING_BUFFER_SIZE);

      for i := 0 to FFT_WINDOW_SIZE - 1 do
        audioSamples[i] := (chunkSamples[i * 2] + chunkSamples[i * 2 + 1]) * 0.5 / 32767.0;
    end;

    CaptureFrame(@fft, @audioSamples);
    RenderFrame(@fft, @fftImage);
    UpdateTexture(fftTexture, fftImage.data);

    BeginDrawing();
      ClearBackground(BLACK);
      BeginShaderMode(shader);
        SetShaderValueTexture(shader, iChannel0Location, fftTexture);
        DrawTextureRec(bufferA.texture,
          RectangleCreate(0, 0, screenWidth, -screenHeight),
          Vector2Create(0, 0),
          WHITE);
      EndShaderMode();
    EndDrawing();
  end;

  UnloadShader(shader);
  UnloadRenderTexture(bufferA);
  UnloadTexture(fftTexture);
  UnloadImage(fftImage);
  UnloadAudioStream(audioStream);
  UnloadWave(wav);
  CloseAudioDevice();

  FreeMem(fft.spectrum);
  FreeMem(fft.workBuffer);
  FreeMem(fft.prevMagnitudes);
  SetLength(fft.fftHistory, 0);

  CloseWindow();
end.
