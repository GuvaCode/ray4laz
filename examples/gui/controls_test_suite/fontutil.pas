unit fontUtil;

{$mode ObjFPC}{$H+}

interface

uses
  raylib, SysUtils;

function LoadUnicodeFont(FileName: String; FontSize: Integer;
  TextureFilter: TTextureFilter = TEXTURE_FILTER_POINT): TFont;

implementation

function LoadUnicodeFont(FileName: String; FontSize: Integer;
  TextureFilter: TTextureFilter): TFont;
var
  cp: array of Integer;  // Array to store Unicode codepoints
  count: Integer;        // Counter for codepoints

  // Helper procedure to add a range of Unicode codepoints
  procedure AddRange(start, stop: Integer);
  begin
    while start <= stop do
    begin
      // Dynamically expand array if needed
      if count >= Length(cp) then
        SetLength(cp, Length(cp) + 1024);
      // Add current codepoint and increment
      cp[count] := start;
      Inc(count);
      Inc(start);
    end;
  end;

begin
  // Initialize codepoint array with initial capacity
  SetLength(cp, 65536);
  count := 0;

  // --------------------------------------------------
  // 1. BASIC ASCII CHARACTERS
  // --------------------------------------------------
  AddRange(32, 126);  // Basic Latin (letters, digits, punctuation)

  // --------------------------------------------------
  // 2. EUROPEAN LANGUAGES (LATIN SCRIPT)
  // --------------------------------------------------
  AddRange($C0, $17F);  // Latin-1 Supplement + Latin Extended-A
  AddRange($180, $24F); // Latin Extended-B
  AddRange($1E00, $1EFF); // Latin Extended Additional
  AddRange($2C60, $2C7F); // Latin Extended-C

  // --------------------------------------------------
  // 3. GREEK AND COPTIC
  // --------------------------------------------------
  AddRange($370, $3FF); // Greek and Coptic
  AddRange($1F00, $1FFF); // Greek Extended

  // --------------------------------------------------
  // 4. CYRILLIC SCRIPTS
  // --------------------------------------------------
  AddRange($400, $4FF); // Basic Cyrillic
  AddRange($500, $52F); // Cyrillic Supplement
  AddRange($2DE0, $2DFF); // Cyrillic Extended-A
  AddRange($A640, $A69F); // Cyrillic Extended-B

  // --------------------------------------------------
  // 5. CJK LANGUAGES (CHINESE, JAPANESE, KOREAN)
  // --------------------------------------------------
  AddRange($4E00, $9FFF); // CJK Unified Ideographs
  AddRange($3400, $4DBF); // CJK Extension A
  AddRange($3000, $303F); // CJK Symbols and Punctuation
  AddRange($3040, $309F); // Hiragana (Japanese)
  AddRange($30A0, $30FF); // Katakana (Japanese)
  AddRange($31F0, $31FF); // Katakana Phonetic Extensions
  AddRange($FF00, $FFEF); // Halfwidth and Fullwidth Forms
  AddRange($AC00, $D7AF); // Hangul Syllables (Korean)
  AddRange($1100, $11FF); // Hangul Jamo

  // --------------------------------------------------
  // 6. SOUTHEAST ASIAN LANGUAGES
  // --------------------------------------------------
  AddRange($0E00, $0E7F); // Thai
  AddRange($0E80, $0EFF); // Lao
  AddRange($1780, $17FF); // Khmer
  AddRange($1000, $109F); // Myanmar
  AddRange($1980, $19DF); // New Tai Lue

  // --------------------------------------------------
  // 7. INDIAN SUBCONTINENT LANGUAGES
  // --------------------------------------------------
  AddRange($900, $97F);  // Devanagari (Hindi, Sanskrit)
  AddRange($980, $9FF);  // Bengali
  AddRange($A00, $A7F);  // Gurmukhi (Punjabi)
  AddRange($A80, $AFF);  // Gujarati
  AddRange($B00, $B7F);  // Oriya
  AddRange($B80, $BFF);  // Tamil
  AddRange($C00, $C7F);  // Telugu
  AddRange($C80, $CFF);  // Kannada
  AddRange($D00, $D7F);  // Malayalam
  AddRange($D80, $DFF);  // Sinhala

  // --------------------------------------------------
  // 8. MIDDLE EASTERN LANGUAGES
  // --------------------------------------------------
  AddRange($600, $6FF);  // Arabic
  AddRange($750, $77F);  // Arabic Supplement
  AddRange($8A0, $8FF);  // Arabic Extended-A
  AddRange($FB50, $FDFF); // Arabic Presentation Forms-A
  AddRange($5D0, $5EA);  // Hebrew
  AddRange($591, $5C7);  // Hebrew Extended
  AddRange($7C0, $7FF);  // N'Ko
  AddRange($640, $6FF);  // Syriac

  // --------------------------------------------------
  // 9. AFRICAN LANGUAGES
  // --------------------------------------------------
  AddRange($2C80, $2CFF); // Coptic
  AddRange($2D30, $2D7F); // Tifinagh
  AddRange($A6A0, $A6FF); // Bamum
  AddRange($AB00, $AB2F); // Ethiopic Extended

  // --------------------------------------------------
  // 10. SPECIAL CHARACTERS AND SYMBOLS
  // --------------------------------------------------
  AddRange($300, $36F);  // Combining Diacritical Marks
  AddRange($1DC0, $1DFF); // Combining Diacritical Marks Supplement
  AddRange($2000, $206F); // General Punctuation
  AddRange($20A0, $20CF); // Currency Symbols
  AddRange($2100, $214F); // Letterlike Symbols
  AddRange($2190, $21FF); // Arrows
  AddRange($2200, $22FF); // Mathematical Operators

  // Trim the array to actual used size
  SetLength(cp, count);

  // Attempt to load the specified font file with all collected codepoints
  if FileExists(FileName) then
    Result := LoadFontEx(PChar(FileName), FontSize, @cp[0], Length(cp));

  // Fallback to default font if loading fails
  if Result.texture.id = 0 then
    Result := GetFontDefault();

  // Apply requested texture filter (defaults to point filtering)
  SetTextureFilter(Result.texture, TextureFilter);
end;

end.
