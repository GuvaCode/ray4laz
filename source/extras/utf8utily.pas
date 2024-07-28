unit UTF8Utily;

{$mode objfpc}{$H+}
{$WARN 5066 off : Symbol "$1" is deprecated: "$2"}
{$WARN 5093 off : function result variable of a managed type does not seem to be initialized}
interface

uses
  Classes, sysutils, lazutf8;

type

// Tools for UTF8 string

// Because an utf8 character may have from 1 to 4 bytes length, we put it in a string
TArrayOfUTF8Char = array of string;  // container for each code point of an utf8 string ( utf8 character may have 1, 2, 3 or 4 bytes )


{ TS8 }     // <= permet d'accéder à une UTF8 comme avant : machaine[i]...
            // mais pas optimisé pour de très grandes chaines de caractères

TS8 = class     // TStringUTF8
private
  FArrayChar: TArrayOfUTF8Char ;
  function GetCharacter(i: integer): string;
  function GetCharCode(i: integer): DWord;
  function GetCharCount: integer;
  function GetHexString: String;
  procedure SetCharacter(i: integer; AValue: string);
  procedure NotifyIndexError( Index: integer);
  procedure SetCharCode(i: integer; AValue: DWord);
public
  constructor Create;
  constructor Create( aUTF8String: string );
  procedure InitWith( aUTF8String: string ); // initialize with a string
  function All: string; // give the entire utf8 string
  property Char[i: integer]: string read GetCharacter write SetCharacter; default;  // 1 based (as for type string)
  property CharCode[i: integer]: DWord read GetCharCode write SetCharCode;
  property CharCount: integer read GetCharCount;
  property HexString: String read GetHexString;
end;


operator = (const s1: TS8; const s2: string ): boolean; inline;
operator > (const s1: TS8; const s2: string ): boolean; inline;
operator >= (const s1: TS8; const s2: string ): boolean; inline;
operator < (const s1: TS8; const s2: string ): boolean; inline;
operator <= (const s1: TS8; const s2: string ): boolean; inline;
operator <> (const s1: TS8; const s2: string ): boolean; inline;
operator + (const s1: TS8; const s2: string ): string ; inline;






// Separates all characters of 'anUTF8String' in an array of utf8 characters.
function UTF8ToCharArray( anUTF8String: string ): TArrayOfUTF8Char;
// Rebuilds an utf8 string from an array of utf8 characters.
function ArrayCharToUTF8( anArray: TArrayOfUTF8Char ): string;

// from utf8 character, it return its 32 bits codepoint
function UTF8CharToCode(const anUTF8Char: string ): DWord;
// from 32 bits codepoint, return an utf8 character
function CodeToUTF8Char( aCode:DWord ): string;

implementation

procedure ErrorUTF8(const aMsg: string );
begin
 raise exception.create(aMsg) at
     get_caller_addr(ExceptAddr),
     get_caller_frame(ExceptFrames);
end;

function UTF8CharToCode(const anUTF8Char: string): DWord;
var p: PByte;
begin
 p := PByte(PChar( anUTF8Char ));
 if Length(anUTF8Char)=0
   then Result :=$00000000
   else if (p^ and $F0) = $F0
          then Result := (p^ shl 24) or ((p+1)^ shl 16) or ((p+2)^ shl 8) or (p+3)^
          else if (p^ and $E0) = $E0
                 then Result := (p^ shl 16) or ((p+1)^ shl 8) or (p+2)^
                 else if (p^ and $C0) = $C0
                        then Result := (p^ shl 8) or (p+1)^
                        else Result := p^;
end;

function CodeToUTF8Char(aCode: DWord): string;
var b1, b2,b3,b4: byte;
begin
 b1 := (aCode and $FF000000) shr 24 ;
 b2 := (aCode and $00FF0000) shr 16 ;
 b3 := (aCode and $0000FF00) shr 8 ;
 b4 := aCode and $000000FF;
 Result := '';
 if b1<>0
   then Result += chr(b1);
 if (b1<>0) or (b2<>0)
   then Result += chr(b2);
 if (b1<>0) or (b2<>0) or (b3<>0)
   then Result += chr(b3);
 if (b1<>0) or (b2<>0) or (b3<>0) or (b4<>0)
   then Result += chr(b4);
end;

function UTF8ToCharArray(anUTF8String: string): TArrayOfUTF8Char;
var
  p: PChar;
  CharLen: integer;
  i: integer;
begin
 SetLength( Result, 0 );
 if Length( anUTF8String )=0 then exit;
 p:=PChar(anUTF8String);
 repeat
   i :=  Length( Result );
   SetLength( Result, i+1 );
   Result[i] :='';
   CharLen := UTF8CharacterLength(p);
   if CharLen >= 1 then Result[i] += P[0] ; //FirstByte := P[0];
   if CharLen >= 2 then Result[i] += P[1] ; //SecondByte := P[1];
   if CharLen >= 3 then Result[i] += P[2] ; //ThirdByte := P[2];
   if CharLen = 4 then Result[i] += P[3] ; //FourthByte := P[3];
   inc(p,CharLen);
 until (CharLen=0) or (p^ = #0);
end;


function ArrayCharToUTF8(anArray: TArrayOfUTF8Char): string;
var i: integer;
begin
 Result := '';
 for i:=0 to Length( anArray )-1 do
   Result += anArray[i] ;
end;



{ TS8 }

function TS8.GetCharacter(i: integer): string;
begin
 if (i<1) or (i>Length( FArrayChar )) then NotifyIndexError( i );
 Result := FArrayChar[i-1];
end;

function TS8.GetCharCode(i: integer): DWord;
begin
 if (i<1) or (i>Length( FArrayChar )) then NotifyIndexError( i );
 Result := UTF8CharToCode( FArrayChar[i-1] );
end;

function TS8.GetCharCount: integer;
begin
 Result := Length( FArrayChar );
end;

function TS8.GetHexString: String;
const LX = '\x';
var   i: Integer;
begin
 Result := ''; i := 1;
 while i < CharCount do
 begin
   if LeftStr(intToHex(CharCode[i],2),2) = RightStr(intToHex(CharCode[i],2),2) then
   Result := Result + LX + LeftStr(intToHex(CharCode[i],1),2)
   else
   Result := Result + LX + LeftStr(intToHex(CharCode[i],1),2) + LX
   + RightStr(intToHex(CharCode[i],1),2);
   inc(i);
 end;
end;

procedure TS8.SetCharacter(i: integer; AValue: string);
begin
 if (i<1) or (i>Length( FArrayChar ))
   then NotifyIndexError( i );
  FArrayChar[i-1] := AValue ;
end;

procedure TS8.NotifyIndexError( Index: integer);
begin
 ErrorUTF8('Index error, ['+inttostr( Index )+'] is out of bound ! (1 based)');
end;

procedure TS8.SetCharCode(i: integer; AValue: DWord);
begin
 if (i<1) or (i>Length( FArrayChar )) then NotifyIndexError( i );
 FArrayChar[i-1] := CodeToUTF8Char( AValue );
end;

constructor TS8.Create;
begin
  inherited Create;
  InitWith( '' );
end;

constructor TS8.Create(aUTF8String: string);
begin
  inherited Create;
  InitWith( aUtf8String );
end;

procedure TS8.InitWith(aUTF8String: string);
begin
  SetLength( FArrayChar, 0 );
  FArrayChar := UTF8ToCharArray( aUTF8String );
end;

function TS8.All: string;
var i: integer;
begin
 Result :='';
 for i:=0 to Length( FArrayChar )-1 do
  Result += FArrayChar[i];
end;




operator = (const s1: TS8; const s2: string ): boolean;
begin
 Result := ( s1.All = s2 );
end;

operator>(const s1: TS8; const s2: string): boolean;
begin
  Result := UTF8CompareStr( s1.All, s2 ) > 0;
end;

operator>=(const s1: TS8; const s2: string): boolean;
begin
 Result := UTF8CompareStr( s1.All, s2 ) >= 0;
end;

operator<(const s1: TS8; const s2: string): boolean;
begin
 Result := UTF8CompareStr( s1.All, s2 ) < 0;
end;

operator<=(const s1: TS8; const s2: string): boolean;
begin
 Result := UTF8CompareStr( s1.All, s2 ) <= 0;
end;

operator<>(const s1: TS8; const s2: string): boolean;
begin
 Result := s1.All <> s2;
end;

operator + (const s1: TS8; const s2: string ): string;
begin
 Result :=  S1.All + s2 ;
end;

end.

