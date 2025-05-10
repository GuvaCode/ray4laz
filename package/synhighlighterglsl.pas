unit SynHighlighterGLSL;

//{$I SynEdit.inc}

interface

uses
  SysUtils, Classes, Graphics, SynEditHighlighter, SynHighlighterAny;

type
  TSynGLSLSyn = class(TSynAnySyn)
  private
    procedure InitGLSL;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetLanguageName: string; override;
  end;

implementation

const
  GLSLKeywords: array[0..95] of string = (
    'attribute', 'const', 'uniform', 'varying', 'buffer', 'shared', 'coherent',
    'volatile', 'restrict', 'readonly', 'writeonly', 'atomic_uint',
    'layout', 'centroid', 'flat', 'smooth', 'noperspective', 'patch', 'sample',
    'break', 'continue', 'do', 'for', 'while', 'switch', 'case', 'default',
    'if', 'else', 'subroutine', 'in', 'out', 'inout',
    'float', 'double', 'int', 'void', 'bool', 'true', 'false',
    'invariant', 'discard', 'return',
    'mat2', 'mat3', 'mat4', 'mat2x2', 'mat2x3', 'mat2x4',
    'mat3x2', 'mat3x3', 'mat3x4', 'mat4x2', 'mat4x3', 'mat4x4',
    'vec2', 'vec3', 'vec4', 'ivec2', 'ivec3', 'ivec4', 'bvec2', 'bvec3', 'bvec4',
    'uint', 'uvec2', 'uvec3', 'uvec4',
    'lowp', 'mediump', 'highp', 'precision',
    'sampler1D', 'sampler2D', 'sampler3D', 'samplerCube',
    'sampler1DShadow', 'sampler2DShadow', 'samplerCubeShadow',
    'sampler1DArray', 'sampler2DArray',
    'sampler1DArrayShadow', 'sampler2DArrayShadow',
    'isampler1D', 'isampler2D', 'isampler3D', 'isamplerCube',
    'isampler1DArray', 'isampler2DArray',
    'usampler1D', 'usampler2D', 'usampler3D', 'usamplerCube',
    'usampler1DArray', 'usampler2DArray',
    'struct'
  );

  GLSLConstants: array[0..5] of string = (
    'gl_Position', 'gl_PointSize', 'gl_ClipDistance', 'gl_FragCoord',
    'gl_FrontFacing', 'gl_FragDepth'
  );

constructor TSynGLSLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitGLSL;
end;

procedure TSynGLSLSyn.InitGLSL;
var
  i: Integer;
begin
  // Setup GLSL specific highlighting
  fDefaultFilter := 'GLSL Files (*.fs;*.vs;*.glsl,*.vert,*.frag,*.geom,*.tesc,*.tese,*.comp)|*.fs;*.vs;*.glsl;*.vert;*.frag;*.geom;*.tesc;*.tese;*.comp';

  // Add GLSL keywords
  for i := Low(GLSLKeywords) to High(GLSLKeywords) do
    KeyWords.Add(GLSLKeywords[i]);

  // Add GLSL built-in constants
  for i := Low(GLSLConstants) to High(GLSLConstants) do
    Constants.Add(GLSLConstants[i]);

  // Setup comment styles for GLSL (C-style comments)
  Comments := [csCStyle];

  // Setup string delimiter (double quotes are standard in GLSL)
  StringDelim := sdDoubleQuote;

  // Disable features not needed for GLSL
  Markup := False;
  Entity := False;
  DollarVariables := False;
  ActiveDot := False;

  // Customize colors
  CommentAttri.Foreground := clGreen;
  CommentAttri.Style := [fsItalic];

  KeyAttri.Foreground := clNavy;
  KeyAttri.Style := [fsBold];

  NumberAttri.Foreground := clBlue;

  StringAttri.Foreground := clPurple;

  SymbolAttri.Foreground := clRed;

  // Rebuild method tables with new settings
 // self.MakeMethodTables;
end;

class function TSynGLSLSyn.GetLanguageName: string;
begin
  Result := 'GLSL';
end;

initialization
  RegisterPlaceableHighlighter(TSynGLSLSyn);
end.
