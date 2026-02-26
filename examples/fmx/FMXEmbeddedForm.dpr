program FMXEmbeddedForm;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMXEF.Main in 'FMXEF.Main.pas' {Form2},
  FMX.Game in 'FMX.Game.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
