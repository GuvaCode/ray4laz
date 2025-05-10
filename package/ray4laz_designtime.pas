{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ray4laz_designtime;

{$warn 5023 off : no warning about unused units}
interface

uses
  ray4laz_simplePrj, ray4laz_misc, ray4laz_customAppPrj, uShaderTool, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ray4laz_simplePrj', @ray4laz_simplePrj.Register);
  RegisterUnit('ray4laz_misc', @ray4laz_misc.Register);
  RegisterUnit('ray4laz_customAppPrj', @ray4laz_customAppPrj.Register);
end;

initialization
  RegisterPackage('ray4laz_designtime', @Register);
end.
